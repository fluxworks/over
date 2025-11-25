//! Implementation of Unicode Standard Annex #31 for 
//! determining which `char` values are valid in programming language identifiers.
#![feature
(
    
)]

#![allow
(
    bare_trait_objects,
    deprecated,
    invalid_value,
    mismatched_lifetime_syntaxes,
    non_camel_case_types,
    non_fmt_panics,
    non_snake_case,
    non_upper_case_globals,
    unreachable_code,
    unused_attributes,
    unused_imports,
    unused_variables,
)]
/*
pub mod _
{
    pub use std::_::{ * };
}

pub mod __
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
}
*/
#[macro_use] extern crate nix;
#[macro_use] extern crate regex as re;

#[macro_use] pub mod macros
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    macro_rules! println_stderr {
        ($fmt:expr) => (
            match writeln!(&mut ::io::stderr(), $fmt) {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
        ($fmt:expr, $($arg:tt)*) => (
            match writeln!(&mut ::io::stderr(), $fmt, $($arg)*) {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
    }
}

pub mod collections
{
    pub use std::collections::{ * };
}

pub mod env
{
    pub use std::env::{ * };
}

pub mod fmt
{
    pub use std::fmt::{ * };
}

pub mod get
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
        

    pub fn get_user_name() -> String {
        match env::var("USER") {
            Ok(x) => {
                return x;
            }
            Err(e) => {
                /* log!("cicada: env USER error: {}", e); */
            }
        }
        let cmd_result = now::run("whoami");
        cmd_result.stdout.trim().to_string()
    }

    pub fn get_user_home() -> String {
        match env::var("HOME") {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: env HOME error: {}", e);
                String::new()
            }
        }
    }

    pub fn get_config_dir() -> String {
        if let Ok(x) = env::var("XDG_CONFIG_HOME") {
            format!("{}/cicada", x)
        } else {
            let home = get_user_home();
            format!("{}/.config/cicada", home)
        }
    }

    pub fn get_user_completer_dir() -> String {
        let dir_config = get_config_dir();
        format!("{}/completers", dir_config)
    }

}

pub mod io
{
    pub use std::io::{ * };
}

pub mod is
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    // pub fn is_signal_handler_enabled() -> bool
    pub fn signal_handler_enabled() -> bool
    {
        env::var("CICADA_ENABLE_SIG_HANDLER").is_ok_and(|x| x == "1")
    }

    //pub fn is_signal_handler_enabled() -> bool
    pub fn is_env(line: &str) -> bool {
        regex::contains(line, r"^[a-zA-Z_][a-zA-Z0-9_]*=.*$")
    }
}

pub mod now
{
    /*!
    */
    use ::
    {
        collections::{ HashMap },
        io::{ self, Read, Write },
        *,
    };
    /*
    use crate::core;
    use crate::parsers;
    use crate::shell::{self, Shell};
    use crate::types::{drain_env_tokens, CommandLine, CommandResult, Tokens};
    */
    /// Entry point for non-ttys (e.g. Cmd-N on MacVim)
    pub fn run_procs_for_non_tty(sh: &mut Shell) {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        match handle.read_to_string(&mut buffer) {
            Ok(_) => {
                log!("run non tty command: {}", &buffer);
                run_command_line(sh, &buffer, false, false);
            }
            Err(e) => {
                println!("cicada: stdin.read_to_string() failed: {:?}", e);
            }
        }
    }

    pub fn run_command_line(
        sh: &mut Shell,
        line: &str,
        tty: bool,
        capture: bool,
    ) -> Vec<CommandResult> {
        let mut cr_list = Vec::new();
        let mut status = 0;
        let mut sep = String::new();
        for token in parsers::parser_line::line_to_cmds(line) {
            if token == ";" || token == "&&" || token == "||" {
                sep = token.clone();
                continue;
            }
            if sep == "&&" && status != 0 {
                break;
            }
            if sep == "||" && status == 0 {
                break;
            }
            let cmd = token.clone();
            let cr = run_proc(sh, &cmd, tty, capture);
            status = cr.status;
            sh.previous_status = status;
            cr_list.push(cr);
        }
        cr_list
    }

    fn line_to_tokens(sh: &mut Shell, line: &str) -> (Tokens, HashMap<String, String>) {
        let linfo = parsers::parser_line::parse_line(line);
        let mut tokens = linfo.tokens;
        shell::do_expansion(sh, &mut tokens);
        let envs = drain_env_tokens(&mut tokens);
        (tokens, envs)
    }

    fn set_shell_vars(sh: &mut Shell, envs: &HashMap<String, String>) {
        for (name, value) in envs.iter() {
            sh.set_env(name, value);
        }
    }

    /// Run simple command or pipeline without using `&&`, `||`, `;`.
    /// example 1: `ls`
    /// example 2: `ls | wc`
    fn run_proc(sh: &mut Shell, line: &str, tty: bool, capture: bool) -> CommandResult {
        let log_cmd = !sh.cmd.starts_with(' ');
        match CommandLine::from_line(line, sh) {
            Ok(cl) => {
                if cl.is_empty() {
                    // for commands with only envs, e.g.
                    // $ FOO=1 BAR=2
                    // we need to define these **Shell Variables**.
                    if !cl.envs.is_empty() {
                        set_shell_vars(sh, &cl.envs);
                    }
                    return CommandResult::new();
                }

                let (term_given, cr) = core::run_pipeline(sh, &cl, tty, capture, log_cmd);
                if term_given {
                    unsafe {
                        let gid = libc::getpgid(0);
                        shell::give_terminal_to(gid);
                    }
                }

                cr
            }
            Err(e) => {
                println_stderr!("cicada: {}", e);
                CommandResult::from_status(0, 1)
            }
        }
    }

    fn run_with_shell(sh: &mut Shell, line: &str) -> CommandResult {
        let (tokens, envs) = line_to_tokens(sh, line);
        if tokens.is_empty() {
            set_shell_vars(sh, &envs);
            return CommandResult::new();
        }

        match CommandLine::from_line(line, sh) {
            Ok(c) => {
                let (term_given, cr) = core::run_pipeline(sh, &c, false, true, false);
                if term_given {
                    unsafe {
                        let gid = libc::getpgid(0);
                        shell::give_terminal_to(gid);
                    }
                }

                cr
            }
            Err(e) => {
                println_stderr!("cicada: {}", e);
                CommandResult::from_status(0, 1)
            }
        }
    }

    pub fn run(line: &str) -> CommandResult {
        let mut sh = Shell::new();
        run_with_shell(&mut sh, line)
    }

}

pub mod parses
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    pub mod line
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        */
    }
}

pub mod regex
{
    /*!
    Regular Expressions.*/
    pub use re::{ * };
    use ::
    {
        *,
    };
    /*
    */
    // pub fn find_first_group( a:&str, b:&str ) -> Option<String>
    pub fn first( a:&str, b:&str ) -> Option<String>
    {
        let re = match Regex::new( a )
        {
            Ok( x ) => x,
            Err( _ ) => return None,
        };

        match re.captures( b )
        {
            Some( c ) =>
            {
                if let Some( x ) = c.get(1) { return Some( x.as_str().to_owned() ); }
            }

            None => { return None; }
        }

        None
    }

    pub fn contains(a: &str, b: &str) -> bool
    {
        let re = match Regex::new( b )
        {
            Ok( x ) => x,
            Err( e ) =>
            {
                println!("Regex new error: {:?}", e);
                return false;
            }
        };

        re.is_match( a )
    }

    pub fn replace_all( a:&str, b: &str, c: &str) -> String
    {
        let re = regex::Regex::new( b ).unwrap();
        let result = re.replace_all( a, c );
        result.to_string()
    }
}

pub mod shell
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use errno::errno;
    use std::collections::{HashMap, HashSet};
    use std::env;
    use std::io::Write;
    use std::mem;

    use regex::Regex;
    use uuid::Uuid;

    use crate::core;
    use crate::libs;
    use crate::parsers;
    use crate::tools;
    use crate::types::{self, CommandLine};
    */
    #[derive(Debug, Clone)]
    pub struct Shell
    {
        pub jobs: HashMap<i32, types::Job>,
        pub aliases: HashMap<String, String>,
        pub envs: HashMap<String, String>,
        pub funcs: HashMap<String, String>,
        pub cmd: String,
        pub current_dir: String,
        pub previous_dir: String,
        pub previous_cmd: String,
        pub previous_status: i32,
        pub is_login: bool,
        pub exit_on_error: bool,
        pub has_terminal: bool,
        pub session_id: String,
    }

    impl Shell
    {
        pub fn new() -> Shell {
            let uuid = Uuid::new_v4().as_hyphenated().to_string();
            let current_dir = tools::get_current_dir();
            let has_terminal = proc_has_terminal();
            let (session_id, _) = uuid.split_at(13);
            Shell {
                jobs: HashMap::new(),
                aliases: HashMap::new(),
                envs: HashMap::new(),
                funcs: HashMap::new(),
                cmd: String::new(),
                current_dir: current_dir.clone(),
                previous_dir: String::new(),
                previous_cmd: String::new(),
                previous_status: 0,
                is_login: false,
                exit_on_error: false,
                has_terminal,
                session_id: session_id.to_string(),
            }
        }

        pub fn insert_job(&mut self, gid: i32, pid: i32, cmd: &str, status: &str, bg: bool) {
            let mut i = 1;
            loop {
                let mut indexed_job_missing = false;
                if let Some(x) = self.jobs.get_mut(&i) {
                    if x.gid == gid {
                        x.pids.push(pid);
                        x.cmd = format!("{} | {}", x.cmd, cmd);
                        return;
                    }
                } else {
                    indexed_job_missing = true;
                }

                if indexed_job_missing {
                    self.jobs.insert(
                        i,
                        types::Job {
                            cmd: cmd.to_string(),
                            id: i,
                            gid,
                            pids: vec![pid],
                            pids_stopped: HashSet::new(),
                            status: status.to_string(),
                            is_bg: bg,
                        },
                    );
                    return;
                }
                i += 1;
            }
        }

        pub fn get_job_by_id(&self, job_id: i32) -> Option<&types::Job> {
            self.jobs.get(&job_id)
        }

        pub fn mark_job_member_continued(&mut self, pid: i32, gid: i32) -> Option<&types::Job> {
            if self.jobs.is_empty() {
                return None;
            }
            let mut i = 1;
            let mut idx_found = 0;
            loop {
                if let Some(job) = self.jobs.get_mut(&i) {
                    if job.gid == gid {
                        job.pids_stopped.remove(&pid);
                        idx_found = i;
                        break;
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }

            self.jobs.get(&idx_found)
        }

        pub fn mark_job_member_stopped(&mut self, pid: i32, gid: i32) -> Option<&types::Job> {
            if self.jobs.is_empty() {
                return None;
            }
            let mut i = 1;
            let mut idx_found = 0;
            loop {
                if let Some(job) = self.jobs.get_mut(&i) {
                    if job.gid == gid {
                        job.pids_stopped.insert(pid);
                        idx_found = i;
                        break;
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }

            self.jobs.get(&idx_found)
        }

        pub fn get_job_by_gid(&self, gid: i32) -> Option<&types::Job> {
            if self.jobs.is_empty() {
                return None;
            }

            let mut i = 1;
            loop {
                if let Some(x) = self.jobs.get(&i) {
                    if x.gid == gid {
                        return Some(x);
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }
            None
        }

        pub fn mark_job_as_running(&mut self, gid: i32, bg: bool) {
            if self.jobs.is_empty() {
                return;
            }

            let mut i = 1;
            loop {
                if let Some(job) = self.jobs.get_mut(&i) {
                    if job.gid == gid {
                        job.status = "Running".to_string();
                        job.pids_stopped.clear();
                        job.is_bg = bg;
                        return;
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }
        }

        pub fn mark_job_as_stopped(&mut self, gid: i32) {
            if self.jobs.is_empty() {
                return;
            }

            let mut i = 1;
            loop {
                if let Some(x) = self.jobs.get_mut(&i) {
                    if x.gid == gid {
                        x.status = "Stopped".to_string();
                        x.is_bg = true;
                        return;
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }
        }

        pub fn remove_pid_from_job(&mut self, gid: i32, pid: i32) -> Option<types::Job> {
            if self.jobs.is_empty() {
                return None;
            }

            let mut empty_pids = false;
            let mut i = 1;
            loop {
                if let Some(x) = self.jobs.get_mut(&i) {
                    if x.gid == gid {
                        if let Ok(i_pid) = x.pids.binary_search(&pid) {
                            x.pids.remove(i_pid);
                        }
                        empty_pids = x.pids.is_empty();
                        break;
                    }
                }

                i += 1;
                if i >= 65535 {
                    break;
                }
            }

            if empty_pids {
                return self.jobs.remove(&i);
            }
            None
        }

        /// Update existing *ENV Variable* if such name exists in ENVs,
        /// otherwise, we define a local *Shell Variable*, which would not
        /// be exported into child processes.
        pub fn set_env(&mut self, name: &str, value: &str) {
            if env::var(name).is_ok() {
                env::set_var(name, value);
            } else {
                self.envs.insert(name.to_string(), value.to_string());
            }
        }

        /// get *Shell Variable*, or *ENV Variable*.
        pub fn get_env(&self, name: &str) -> Option<String> {
            match self.envs.get(name) {
                Some(x) => Some(x.to_string()),
                None => env::var(name).ok(),
            }
        }

        /// Remove environment variable, function from the environment of
        /// the currently running process
        pub fn remove_env(&mut self, name: &str) -> bool {
            // function names can contain the `-` char.
            let ptn_env = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_-]*$").unwrap();
            if !ptn_env.is_match(name) {
                return false;
            }

            env::remove_var(name);
            self.envs.remove(name);
            self.remove_func(name);
            true
        }

        pub fn remove_path(&mut self, path: &str) {
            if let Ok(paths) = env::var("PATH") {
                let mut paths_new: Vec<&str> = paths.split(":").collect();
                paths_new.retain(|&x| x != path);
                env::set_var("PATH", paths_new.join(":").as_str());
            }
        }

        fn remove_func(&mut self, name: &str) {
            self.funcs.remove(name);
        }

        pub fn set_func(&mut self, name: &str, value: &str) {
            self.funcs.insert(name.to_string(), value.to_string());
        }

        pub fn get_func(&self, name: &str) -> Option<String> {
            self.funcs.get(name).map(|x| x.to_string())
        }

        pub fn get_alias_list(&self) -> Vec<(String, String)> {
            let mut result = Vec::new();
            for (name, value) in &self.aliases {
                result.push((name.clone(), value.clone()));
            }
            result
        }

        pub fn add_alias(&mut self, name: &str, value: &str) {
            self.aliases.insert(name.to_string(), value.to_string());
        }

        pub fn is_alias(&self, name: &str) -> bool {
            self.aliases.contains_key(name)
        }

        pub fn remove_alias(&mut self, name: &str) -> bool {
            let opt = self.aliases.remove(name);
            opt.is_some()
        }

        pub fn get_alias_content(&self, name: &str) -> Option<String> {
            let result = match self.aliases.get(name) {
                Some(x) => x.to_string(),
                None => String::new(),
            };
            if result.is_empty() {
                None
            } else {
                Some(result)
            }
        }
    }

    pub unsafe fn give_terminal_to(gid: i32) -> bool
    {
        let mut mask: libc::sigset_t = mem::zeroed();
        let mut old_mask: libc::sigset_t = mem::zeroed();

        libc::sigemptyset(&mut mask);
        libc::sigaddset(&mut mask, libc::SIGTSTP);
        libc::sigaddset(&mut mask, libc::SIGTTIN);
        libc::sigaddset(&mut mask, libc::SIGTTOU);
        libc::sigaddset(&mut mask, libc::SIGCHLD);

        let rcode = libc::pthread_sigmask(libc::SIG_BLOCK, &mask, &mut old_mask);
        if rcode != 0 {
            log!("failed to call pthread_sigmask");
        }
        let rcode = libc::tcsetpgrp(1, gid);
        let given;
        if rcode == -1 {
            given = false;
            let e = errno();
            let code = e.0;
            log!("error in give_terminal_to() {}: {}", code, e);
        } else {
            given = true;
        }
        let rcode = libc::pthread_sigmask(libc::SIG_SETMASK, &old_mask, &mut mask);
        if rcode != 0 {
            log!("failed to call pthread_sigmask");
        }
        given
    }

    pub fn needs_globbing(line: &str) -> bool
    {
        let re = Regex::new(r"\*+").expect("Invalid regex ptn");
        re.is_match(line)
    }

    pub fn expand_glob(tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        for (sep, text) in tokens.iter() {
            if !sep.is_empty() || !needs_globbing(text) {
                idx += 1;
                continue;
            }

            let mut result: Vec<String> = Vec::new();
            let item = text.as_str();

            if !item.contains('*') || item.trim().starts_with('\'') || item.trim().starts_with('"') {
                result.push(item.to_string());
            } else {
                let _basename = libs::path::basename(item);
                let show_hidden = _basename.starts_with(".*");

                match glob::glob(item) {
                    Ok(paths) => {
                        let mut is_empty = true;
                        for entry in paths {
                            match entry {
                                Ok(path) => {
                                    let file_path = path.to_string_lossy();
                                    let _basename = libs::path::basename(&file_path);
                                    if _basename == ".." || _basename == "." {
                                        continue;
                                    }
                                    if _basename.starts_with('.') && !show_hidden {
                                        // skip hidden files, you may need to
                                        // type `ls .*rc` instead of `ls *rc`
                                        continue;
                                    }
                                    result.push(file_path.to_string());
                                    is_empty = false;
                                }
                                Err(e) => {
                                    log!("glob error: {:?}", e);
                                }
                            }
                        }
                        if is_empty {
                            result.push(item.to_string());
                        }
                    }
                    Err(e) => {
                        println!("glob error: {:?}", e);
                        result.push(item.to_string());
                        return;
                    }
                }
            }

            buff.push((idx, result));
            idx += 1;
        }

        for (i, result) in buff.iter().rev() {
            tokens.remove(*i);
            for (j, token) in result.iter().enumerate() {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    pub fn expand_one_env(sh: &Shell, token: &str) -> String
    {
        let re1 = Regex::new(r"^(.*?)\$([A-Za-z0-9_]+|\$|\?)(.*)$").unwrap();
        let re2 = Regex::new(r"(.*?)\$\{([A-Za-z0-9_]+|\$|\?)\}(.*)$").unwrap();
        if !re1.is_match(token) && !re2.is_match(token) {
            return token.to_string();
        }

        let mut result = String::new();
        let match_re1 = re1.is_match(token);
        let match_re2 = re2.is_match(token);
        if !match_re1 && !match_re2 {
            return token.to_string();
        }

        let cap_results = if match_re1 {
            re1.captures_iter(token)
        } else {
            re2.captures_iter(token)
        };

        for cap in cap_results {
            let head = cap[1].to_string();
            let tail = cap[3].to_string();
            let key = cap[2].to_string();
            if key == "?" {
                result.push_str(format!("{}{}", head, sh.previous_status).as_str());
            } else if key == "$" {
                unsafe {
                    let val = libc::getpid();
                    result.push_str(format!("{}{}", head, val).as_str());
                }
            } else if let Ok(val) = env::var(&key) {
                result.push_str(format!("{}{}", head, val).as_str());
            } else if let Some(val) = sh.get_env(&key) {
                result.push_str(format!("{}{}", head, val).as_str());
            } else {
                result.push_str(&head);
            }
            result.push_str(&tail);
        }

        result
    }

    pub fn need_expand_brace(line: &str) -> bool
    {
        regex::contains(line, r#"\{[^ "']*,[^ "']*,?[^ "']*\}"#)
    }

    pub fn brace_getitem(s: &str, depth: i32) -> (Vec<String>, String)
    {
        let mut out: Vec<String> = vec![String::new()];
        let mut ss = s.to_string();
        let mut tmp;
        while !ss.is_empty() {
            let c = match ss.chars().next() {
                Some(x) => x,
                None => {
                    return (out, ss);
                }
            };
            if depth > 0 && (c == ',' || c == '}') {
                return (out, ss);
            }
            if c == '{' {
                let mut sss = ss.clone();
                sss.remove(0);
                let result_groups = brace_getgroup(&sss, depth + 1);
                if let Some((out_group, s_group)) = result_groups {
                    let mut tmp_out = Vec::new();
                    for x in out.iter() {
                        for y in out_group.iter() {
                            let item = format!("{}{}", x, y);
                            tmp_out.push(item);
                        }
                    }
                    out = tmp_out;
                    ss = s_group.clone();
                    continue;
                }
            }
            // FIXME: here we mean more than one char.
            if c == '\\' && ss.len() > 1 {
                ss.remove(0);

                let c = match ss.chars().next() {
                    Some(x) => x,
                    None => return (out, ss),
                };

                tmp = format!("\\{}", c);
            } else {
                tmp = c.to_string();
            }
            let mut result = Vec::new();
            for x in out.iter() {
                let item = format!("{}{}", x, tmp);
                result.push(item);
            }
            out = result;
            ss.remove(0);
        }
        (out, ss)
    }

    pub fn brace_getgroup(s: &str, depth: i32) -> Option<(Vec<String>, String)>
    {
        let mut out: Vec<String> = Vec::new();
        let mut comma = false;
        let mut ss = s.to_string();
        while !ss.is_empty() {
            let (g, sss) = brace_getitem(ss.as_str(), depth);
            ss = sss.clone();
            if ss.is_empty() {
                break;
            }
            for x in g.iter() {
                out.push(x.clone());
            }

            let c = match ss.chars().next() {
                Some(x) => x,
                None => {
                    break;
                }
            };
            if c == '}' {
                let mut sss = ss.clone();
                sss.remove(0);
                if comma {
                    return Some((out, sss));
                }
                let mut result = Vec::new();
                for x in out.iter() {
                    let item = format!("{{{}}}", x);
                    result.push(item);
                }
                return Some((result, ss));
            }
            if c == ',' {
                comma = true;
                ss.remove(0);
            }
        }

        None
    }

    pub fn expand_brace(tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        for (sep, token) in tokens.iter() {
            if !sep.is_empty() || !need_expand_brace(token) {
                idx += 1;
                continue;
            }

            let mut result: Vec<String> = Vec::new();
            let items = brace_getitem(token, 0);
            for x in items.0 {
                result.push(x.clone());
            }
            buff.push((idx, result));
            idx += 1;
        }

        for (i, items) in buff.iter().rev() {
            tokens.remove(*i);
            for (j, token) in items.iter().enumerate() {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    pub fn expand_brace_range(tokens: &mut types::Tokens)
    {
        let re;
        if let Ok(x) = Regex::new(r#"\{(-?[0-9]+)\.\.(-?[0-9]+)(\.\.)?([0-9]+)?\}"#) {
            re = x;
        } else {
            println_stderr!("cicada: re new error");
            return;
        }

        let mut idx: usize = 0;
        let mut buff: Vec<(usize, Vec<String>)> = Vec::new();
        for (sep, token) in tokens.iter() {
            if !sep.is_empty() || !re.is_match(token) {
                idx += 1;
                continue;
            }

            // safe to unwrap here, since the `is_match` above already validated
            let caps = re.captures(token).unwrap();

            let start = match caps[1].to_string().parse::<i32>() {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!("cicada: {}", e);
                    return;
                }
            };

            let end = match caps[2].to_string().parse::<i32>() {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!("cicada: {}", e);
                    return;
                }
            };

            // incr is always positive
            let mut incr = if caps.get(4).is_none() {
                1
            } else {
                match caps[4].to_string().parse::<i32>() {
                    Ok(x) => x,
                    Err(e) => {
                        println_stderr!("cicada: {}", e);
                        return;
                    }
                }
            };
            if incr <= 1 {
                incr = 1;
            }

            let mut result: Vec<String> = Vec::new();
            let mut n = start;
            if start > end {
                while n >= end {
                    result.push(format!("{}", n));
                    n -= incr;
                }
            } else {
                while n <= end {
                    result.push(format!("{}", n));
                    n += incr;
                }
            }

            buff.push((idx, result));
            idx += 1;
        }

        for (i, items) in buff.iter().rev() {
            tokens.remove(*i);
            for (j, token) in items.iter().enumerate() {
                let sep = if token.contains(' ') { "\"" } else { "" };
                tokens.insert(*i + j, (sep.to_string(), token.clone()));
            }
        }
    }

    pub fn expand_alias(sh: &Shell, tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        let mut is_head = true;
        for (sep, text) in tokens.iter() {
            if sep.is_empty() && text == "|" {
                is_head = true;
                idx += 1;
                continue;
            }
            if is_head && text == "xargs" {
                idx += 1;
                continue;
            }

            if !is_head || !sh.is_alias(text) {
                idx += 1;
                is_head = false;
                continue;
            }

            if let Some(value) = sh.get_alias_content(text) {
                buff.push((idx, value.clone()));
            }

            idx += 1;
            is_head = false;
        }

        for (i, text) in buff.iter().rev() {
            let linfo = parsers::parser_line::parse_line(text);
            let tokens_ = linfo.tokens;
            tokens.remove(*i);
            for item in tokens_.iter().rev() {
                tokens.insert(*i, item.clone());
            }
        }
    }

    pub fn expand_home(tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();
        for (sep, text) in tokens.iter() {
            if !sep.is_empty() || !text.starts_with("~") {
                idx += 1;
                continue;
            }

            let mut s: String = text.clone();
            let ptn = r"^~(?P<tail>.*)";
            let re = Regex::new(ptn).expect("invalid re ptn");
            let home = tools::get_user_home();
            let ss = s.clone();
            let to = format!("{}$tail", home);
            let result = re.replace_all(ss.as_str(), to.as_str());
            s = result.to_string();

            buff.push((idx, s.clone()));
            idx += 1;
        }

        for (i, text) in buff.iter().rev() {
            tokens[*i].1 = text.to_string();
        }
    }

    pub fn env_in_token(token: &str) -> bool
    {
        if libs::re::re_contains(token, r"\$\{?[\$\?]\}?") {
            return true;
        }

        let ptn_env_name = r"[a-zA-Z_][a-zA-Z0-9_]*";
        let ptn_env = format!(r"\$\{{?{}\}}?", ptn_env_name);
        if !libs::re::re_contains(token, &ptn_env) {
            return false;
        }

        // do not expand env in a command substitution, e.g.:
        // - echo $(echo '$HOME')
        // - VERSION=$(foobar -h | grep 'version: v' | awk '{print $NF}')
        let ptn_cmd_sub1 = format!(r"^{}=`.*`$", ptn_env_name);
        let ptn_cmd_sub2 = format!(r"^{}=\$\(.*\)$", ptn_env_name);
        if libs::re::re_contains(token, &ptn_cmd_sub1)
            || libs::re::re_contains(token, &ptn_cmd_sub2)
            || libs::re::re_contains(token, r"^\$\(.+\)$")
        {
            return false;
        }

        // for cmd-line like `alias foo='echo $PWD'`
        let ptn_env = format!(r"='.*\$\{{?{}\}}?.*'$", ptn_env_name);
        !libs::re::re_contains(token, &ptn_env)
    }

    pub fn expand_env(sh: &Shell, tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();

        for (sep, token) in tokens.iter() {
            if sep == "`" || sep == "'" {
                idx += 1;
                continue;
            }

            if !env_in_token(token) {
                idx += 1;
                continue;
            }

            let mut _token = token.clone();
            while env_in_token(&_token) {
                _token = expand_one_env(sh, &_token);
            }
            buff.push((idx, _token));
            idx += 1;
        }

        for (i, text) in buff.iter().rev() {
            tokens[*i].1 = text.to_string();
        }
    }

    pub fn should_do_dollar_command_extension(line: &str) -> bool
    {
        libs::re::re_contains(line, r"\$\([^\)]+\)")
            && !libs::re::re_contains(line, r"='.*\$\([^\)]+\).*'$")
    }

    pub fn do_command_substitution_for_dollar(sh: &mut Shell, tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff: HashMap<usize, String> = HashMap::new();

        for (sep, token) in tokens.iter() {
            if sep == "'" || sep == "\\" || !should_do_dollar_command_extension(token) {
                idx += 1;
                continue;
            }

            let mut line = token.to_string();
            loop {
                if !should_do_dollar_command_extension(&line) {
                    break;
                }

                let ptn_cmd = r"\$\((.+)\)";
                let cmd = match libs::re::find_first_group(ptn_cmd, &line) {
                    Some(x) => x,
                    None => {
                        println_stderr!("cicada: calculator: no first group");
                        return;
                    }
                };

                let cmd_result = match CommandLine::from_line(&cmd, sh) {
                    Ok(c) => {
                        log!("run subcmd dollar: {:?}", &cmd);
                        let (term_given, cr) = core::run_pipeline(sh, &c, true, true, false);
                        if term_given {
                            unsafe {
                                let gid = libc::getpgid(0);
                                give_terminal_to(gid);
                            }
                        }

                        cr
                    }
                    Err(e) => {
                        println_stderr!("cicada: {}", e);
                        continue;
                    }
                };

                let output_txt = cmd_result.stdout.trim();

                let ptn = r"(?P<head>[^\$]*)\$\(.+\)(?P<tail>.*)";
                let re;
                if let Ok(x) = Regex::new(ptn) {
                    re = x;
                } else {
                    return;
                }

                let to = format!("${{head}}{}${{tail}}", output_txt);
                let line_ = line.clone();
                let result = re.replace(&line_, to.as_str());
                line = result.to_string();
            }

            buff.insert(idx, line.clone());
            idx += 1;
        }

        for (i, text) in buff.iter() {
            tokens[*i].1 = text.to_string();
        }
    }

    pub fn do_command_substitution_for_dot(sh: &mut Shell, tokens: &mut types::Tokens)
    {
        let mut idx: usize = 0;
        let mut buff: HashMap<usize, String> = HashMap::new();
        let re = Regex::new(r"^([^`]*)`([^`]+)`(.*)$").unwrap();

        for (sep, token) in tokens.iter() {
            let new_token: String;
            if sep == "`" {
                log!("run subcmd dot1: {:?}", token);
                let cr = match CommandLine::from_line(token, sh) {
                    Ok(c) => {
                        let (term_given, _cr) = core::run_pipeline(sh, &c, true, true, false);
                        if term_given {
                            unsafe {
                                let gid = libc::getpgid(0);
                                give_terminal_to(gid);
                            }
                        }

                        _cr
                    }
                    Err(e) => {
                        println_stderr!("cicada: {}", e);
                        continue;
                    }
                };

                new_token = cr.stdout.trim().to_string();
            } else if sep == "\"" || sep.is_empty() {
                if !re.is_match(token) {
                    idx += 1;
                    continue;
                }
                let mut _token = token.clone();
                let mut _item = String::new();
                let mut _head = String::new();
                let mut _output = String::new();
                let mut _tail = String::new();
                loop {
                    if !re.is_match(&_token) {
                        if !_token.is_empty() {
                            _item = format!("{}{}", _item, _token);
                        }
                        break;
                    }
                    for cap in re.captures_iter(&_token) {
                        _head = cap[1].to_string();
                        _tail = cap[3].to_string();
                        log!("run subcmd dot2: {:?}", &cap[2]);

                        let cr = match CommandLine::from_line(&cap[2], sh) {
                            Ok(c) => {
                                let (term_given, _cr) = core::run_pipeline(sh, &c, true, true, false);
                                if term_given {
                                    unsafe {
                                        let gid = libc::getpgid(0);
                                        give_terminal_to(gid);
                                    }
                                }

                                _cr
                            }
                            Err(e) => {
                                println_stderr!("cicada: {}", e);
                                continue;
                            }
                        };

                        _output = cr.stdout.trim().to_string();
                    }
                    _item = format!("{}{}{}", _item, _head, _output);
                    if _tail.is_empty() {
                        break;
                    }
                    _token = _tail.clone();
                }
                new_token = _item;
            } else {
                idx += 1;
                continue;
            }

            buff.insert(idx, new_token.clone());
            idx += 1;
        }

        for (i, text) in buff.iter() {
            tokens[*i].1 = text.to_string();
        }
    }

    pub fn do_command_substitution(sh: &mut Shell, tokens: &mut types::Tokens)
    {
        do_command_substitution_for_dot(sh, tokens);
        do_command_substitution_for_dollar(sh, tokens);
    }

    pub fn do_expansion(sh: &mut Shell, tokens: &mut types::Tokens)
    {
        let line = parsers::parser_line::tokens_to_line(tokens);
        if tools::is_arithmetic(&line) {
            return;
        }

        if tokens.len() >= 2 && tokens[0].1 == "export" && tokens[1].1.starts_with("PROMPT=") {
            return;
        }

        expand_alias(sh, tokens);
        expand_home(tokens);
        expand_env(sh, tokens);
        expand_brace(tokens);
        expand_glob(tokens);
        do_command_substitution(sh, tokens);
        expand_brace_range(tokens);
    }

    pub fn trim_multiline_prompts(line: &str) -> String
    {
        let line_new = libs::re::replace_all(line, r"\\\n>> ", "");
        let line_new = libs::re::replace_all(&line_new, r"\| *\n>> ", "| ");
        libs::re::replace_all(&line_new, r"(?P<NEWLINE>\n)>> ", "$NEWLINE")
    }

    pub fn proc_has_terminal() -> bool
    {
        unsafe {
            let tgid = libc::tcgetpgrp(0);
            let pgid = libc::getpgid(0);
            tgid == pgid
        }
    }
}

pub mod tokens
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    // pub fn tokens_to_redirections(tokens: &Tokens) -> Result<(Tokens, Vec<Redirection>), String>
    pub fn to_redirections(tokens: &Tokens) -> Result<(Tokens, Vec<Redirection>), String>
    {
        let mut tokens_new = Vec::new();
        let mut redirects = Vec::new();
        let mut to_be_continued = false;
        let mut to_be_continued_s1 = String::new();
        let mut to_be_continued_s2 = String::new();

        for token in tokens {
            let sep = &token.0;
            if !sep.is_empty() && !to_be_continued {
                tokens_new.push(token.clone());
                continue;
            }
            let word = &token.1;

            if to_be_continued {
                if sep.is_empty() && word.starts_with('&') {
                    return Err(String::from("bad redirection syntax near &"));
                }

                let s3 = word.to_string();
                if libs::re::re_contains(&to_be_continued_s1, r"^\d+$") {
                    if to_be_continued_s1 != "1" && to_be_continued_s1 != "2" {
                        return Err(String::from("Bad file descriptor #3"));
                    }
                    let s1 = to_be_continued_s1.clone();
                    let s2 = to_be_continued_s2.clone();
                    redirects.push((s1, s2, s3));
                } else {
                    if !to_be_continued_s1.is_empty() {
                        tokens_new.push((sep.clone(), to_be_continued_s1.to_string()));
                    }
                    redirects.push(("1".to_string(), to_be_continued_s2.clone(), s3));
                }

                to_be_continued = false;
                continue;
            }

            let ptn1 = r"^([^>]*)(>>?)([^>]+)$";
            let ptn2 = r"^([^>]*)(>>?)$";
            if !libs::re::re_contains(word, r">") {
                tokens_new.push(token.clone());
            } else if libs::re::re_contains(word, ptn1) {
                let re;
                if let Ok(x) = Regex::new(ptn1) {
                    re = x;
                } else {
                    return Err(String::from("Failed to build Regex"));
                }

                if let Some(caps) = re.captures(word) {
                    let s1 = caps.get(1).unwrap().as_str();
                    let s2 = caps.get(2).unwrap().as_str();
                    let s3 = caps.get(3).unwrap().as_str();
                    if s3.starts_with('&') && s3 != "&1" && s3 != "&2" {
                        return Err(String::from("Bad file descriptor #1"));
                    }

                    if libs::re::re_contains(s1, r"^\d+$") {
                        if s1 != "1" && s1 != "2" {
                            return Err(String::from("Bad file descriptor #2"));
                        }
                        redirects.push((s1.to_string(), s2.to_string(), s3.to_string()));
                    } else {
                        if !s1.is_empty() {
                            tokens_new.push((sep.clone(), s1.to_string()));
                        }
                        redirects.push((String::from("1"), s2.to_string(), s3.to_string()));
                    }
                }
            } else if libs::re::re_contains(word, ptn2) {
                let re;
                if let Ok(x) = Regex::new(ptn2) {
                    re = x;
                } else {
                    return Err(String::from("Failed to build Regex"));
                }

                if let Some(caps) = re.captures(word) {
                    let s1 = caps.get(1).unwrap().as_str();
                    let s2 = caps.get(2).unwrap().as_str();

                    to_be_continued = true;
                    to_be_continued_s1 = s1.to_string();
                    to_be_continued_s2 = s2.to_string();
                }
            }
        }

        if to_be_continued {
            return Err(String::from("redirection syntax error"));
        }

        Ok((tokens_new, redirects))
    }

}

pub mod types
{
    /*!
    */
    use ::
    {
        collections::{ HashMap, HashSet },
        *,
    };
    /*
    use crate::libs;
    use crate::parsers;
    use crate::parsers::parser_line::tokens_to_redirections;
    use crate::shell;
    use crate::tools;
    */
    #[derive( Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash )]
    pub struct WaitStatus(i32, i32, i32);

    impl WaitStatus
    {
        pub fn from_exited(pid: i32, status: i32) -> Self {
            WaitStatus(pid, 0, status)
        }

        pub fn from_signaled(pid: i32, sig: i32) -> Self {
            WaitStatus(pid, 1, sig)
        }

        pub fn from_stopped(pid: i32, sig: i32) -> Self {
            WaitStatus(pid, 2, sig)
        }

        pub fn from_continuted(pid: i32) -> Self {
            WaitStatus(pid, 3, 0)
        }

        pub fn from_others() -> Self {
            WaitStatus(0, 9, 9)
        }

        pub fn from_error(errno: i32) -> Self {
            WaitStatus(0, 255, errno)
        }

        pub fn empty() -> Self {
            WaitStatus(0, 0, 0)
        }

        pub fn is_error(&self) -> bool {
            self.1 == 255
        }

        pub fn is_others(&self) -> bool {
            self.1 == 9
        }

        pub fn is_signaled(&self) -> bool {
            self.1 == 1
        }

        pub fn get_errno(&self) -> nix::Error {
            nix::Error::from_raw(self.2)
        }

        pub fn is_exited(&self) -> bool {
            self.0 != 0 && self.1 == 0
        }

        pub fn is_stopped(&self) -> bool {
            self.1 == 2
        }

        pub fn is_continued(&self) -> bool {
            self.1 == 3
        }

        pub fn get_pid(&self) -> i32 {
            self.0
        }

        fn _get_signaled_status(&self) -> i32 {
            self.2 + 128
        }

        pub fn get_signal(&self) -> i32 {
            self.2
        }

        pub fn get_name(&self) -> String {
            if self.is_exited() {
                "Exited".to_string()
            } else if self.is_stopped() {
                "Stopped".to_string()
            } else if self.is_continued() {
                "Continued".to_string()
            } else if self.is_signaled() {
                "Signaled".to_string()
            } else if self.is_others() {
                "Others".to_string()
            } else if self.is_error() {
                "Error".to_string()
            } else {
                format!("unknown: {}", self.2)
            }
        }

        pub fn get_status(&self) -> i32 {
            if self.is_exited() {
                self.2
            } else {
                self._get_signaled_status()
            }
        }
    }

    impl fmt::Debug for WaitStatus
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
        {
            let mut formatter = f.debug_struct("WaitStatus");
            formatter.field("pid", &self.0);
            let name = self.get_name();
            formatter.field("name", &name);
            formatter.field("ext", &self.2);
            formatter.finish()
        }
    }

    pub type Token = (String, String);
    pub type Tokens = Vec<Token>;
    pub type Redirection = (String, String, String);

    #[derive(Debug)]
    pub struct LineInfo 
    {
        // e.g. echo 'foo
        // is not a completed line, need to turn to multiple-line mode.
        pub tokens: Tokens,
        pub is_complete: bool,
    }

    impl LineInfo 
    {
        pub fn new(tokens: Tokens) -> Self 
        {
            LineInfo 
            {
                tokens,
                is_complete: true,
            }
        }
    }

    ///
    /// command line: `ls 'foo bar' 2>&1 > /dev/null < one-file` would be:
    /// Command {
    ///     tokens: [("", "ls"), ("", "-G"), ("\'", "foo bar")],
    ///     redirects_to: [
    ///         ("2", ">", "&1"),
    ///         ("1", ">", "/dev/null"),
    ///     ],
    ///     redirect_from: Some(("<", "one-file")),
    /// }
    ///
    #[derive(Debug)]
    pub struct Command 
    {
        pub tokens: Tokens,
        pub redirects_to: Vec<Redirection>,
        pub redirect_from: Option<Token>,
    }

    #[derive(Debug)]
    pub struct CommandLine 
    {
        pub line: String,
        pub commands: Vec<Command>,
        pub envs: HashMap<String, String>,
        pub background: bool,
    }

    impl Command 
    {
        pub fn from_tokens(tokens: Tokens) -> Result<Command, String> {
            let mut tokens_new = tokens.clone();
            let mut redirects_from_type = String::new();
            let mut redirects_from_value = String::new();
            let mut has_redirect_from = tokens_new.iter().any(|x| x.1 == "<" || x.1 == "<<<");

            let mut len = tokens_new.len();
            while has_redirect_from {
                if let Some(idx) = tokens_new.iter().position(|x| x.1 == "<") {
                    redirects_from_type = "<".to_string();
                    tokens_new.remove(idx);
                    len -= 1;
                    if len > idx {
                        redirects_from_value = tokens_new.remove(idx).1;
                        len -= 1;
                    }
                }
                if let Some(idx) = tokens_new.iter().position(|x| x.1 == "<<<") {
                    redirects_from_type = "<<<".to_string();
                    tokens_new.remove(idx);
                    len -= 1;
                    if len > idx {
                        redirects_from_value = tokens_new.remove(idx).1;
                        len -= 1;
                    }
                }

                has_redirect_from = tokens_new.iter().any(|x| x.1 == "<" || x.1 == "<<<");
            }

            let tokens_final;
            let redirects_to;
            match tokens_to_redirections(&tokens_new) {
                Ok((_tokens, _redirects_to)) => {
                    tokens_final = _tokens;
                    redirects_to = _redirects_to;
                }
                Err(e) => {
                    return Err(e);
                }
            }

            let redirect_from = if redirects_from_type.is_empty() {
                None
            } else {
                Some((redirects_from_type, redirects_from_value))
            };

            Ok(Command {
                tokens: tokens_final,
                redirects_to,
                redirect_from,
            })
        }

        pub fn has_redirect_from(&self) -> bool {
            self.redirect_from.is_some() && self.redirect_from.clone().unwrap().0 == "<"
        }

        pub fn has_here_string(&self) -> bool {
            self.redirect_from.is_some() && self.redirect_from.clone().unwrap().0 == "<<<"
        }

        pub fn is_builtin(&self) -> bool {
            is::builtin(&self.tokens[0].1)
        }
    }

    #[derive(Debug, Clone, Default)]
    pub struct Job 
    {
        pub cmd: String,
        pub id: i32,
        pub gid: i32,
        pub pids: Vec<i32>,
        pub pids_stopped: HashSet<i32>,
        pub status: String,
        pub is_bg: bool,
    }

    impl Job 
    {
        pub fn all_members_stopped(&self) -> bool 
        {
            for pid in &self.pids {
                if !self.pids_stopped.contains(pid) {
                    return false;
                }
            }
            true
        }

        pub fn all_members_running(&self) -> bool {
            self.pids_stopped.is_empty()
        }
    }

    #[allow(dead_code)]
    #[derive(Clone, Debug, Default)]
    pub struct CommandResult 
    {
        pub gid: i32,
        pub status: i32,
        pub stdout: String,
        pub stderr: String,
    }

    impl CommandResult 
    {
        pub fn new() -> CommandResult {
            CommandResult {
                gid: 0,
                status: 0,
                stdout: String::new(),
                stderr: String::new(),
            }
        }

        pub fn from_status(gid: i32, status: i32) -> CommandResult {
            CommandResult {
                gid,
                status,
                stdout: String::new(),
                stderr: String::new(),
            }
        }

        pub fn error() -> CommandResult {
            CommandResult {
                gid: 0,
                status: 1,
                stdout: String::new(),
                stderr: String::new(),
            }
        }
    }

    #[allow(dead_code)]
    #[derive(Clone, Debug, Default)]
    pub struct CommandOptions 
    {
        pub background: bool,
        pub isatty: bool,
        pub capture_output: bool,
        pub envs: HashMap<String, String>,
    }

    pub fn split_tokens_by_pipes(tokens: &[Token]) -> Vec<Tokens> 
    {
        let mut cmd = Vec::new();
        let mut cmds = Vec::new();
        for token in tokens {
            let sep = &token.0;
            let value = &token.1;
            if sep.is_empty() && value == "|" {
                if cmd.is_empty() {
                    return Vec::new();
                }
                cmds.push(cmd.clone());
                cmd = Vec::new();
            } else {
                cmd.push(token.clone());
            }
        }
        if cmd.is_empty() {
            return Vec::new();
        }
        cmds.push(cmd.clone());
        cmds
    }

    pub(crate) fn drain_env_tokens(tokens: &mut Tokens) -> HashMap<String, String> 
    {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let re = Regex::new(r"^([a-zA-Z0-9_]+)=(.*)$").unwrap();
        for (sep, text) in tokens.iter() {
            if !sep.is_empty() || !regex::contains(text, r"^([a-zA-Z0-9_]+)=(.*)$") {
                break;
            }

            for cap in re.captures_iter(text) {
                let name = cap[1].to_string();
                let value = parses::line::unquote(&cap[2]);
                envs.insert(name, value);
            }

            n += 1;
        }
        if n > 0 {
            tokens.drain(0..n);
        }
        envs
    }

    impl CommandLine
    {
        pub fn from_line(line: &str, sh: &mut shell::Shell) -> Result<CommandLine, String> {
            let linfo = parses::line::parse_line(line);
            let mut tokens = linfo.tokens;
            shell::do_expansion(sh, &mut tokens);
            let envs = drain_env_tokens(&mut tokens);

            let mut background = false;
            let len = tokens.len();
            if len > 1 && tokens[len - 1].1 == "&" {
                background = true;
                tokens.pop();
            }

            let mut commands = Vec::new();
            for sub_tokens in split_tokens_by_pipes(&tokens) {
                match Command::from_tokens(sub_tokens) {
                    Ok(c) => {
                        commands.push(c);
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }

            Ok(CommandLine {
                line: line.to_string(),
                commands,
                envs,
                background,
            })
        }

        pub fn is_empty(&self) -> bool {
            self.commands.is_empty()
        }

        pub fn is_single_and_builtin(&self) -> bool {
            self.commands.len() == 1 && self.commands[0].is_builtin()
        }

        pub fn with_pipeline(&self) -> bool {
            self.commands.len() > 1
        }
    }
}

pub fn main() -> Result<(), ()>
{
    unsafe
    {
        Ok( () )
    }
}
// 1965 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
