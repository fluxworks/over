//! Implementation of Unicode Standard Annex #31 for 
//! determining which `char` values are valid in programming language identifiers.
#![feature
( 
    
 )]

#![allow
( 
    bare_trait_objects,
    deprecated,
    mismatched_lifetime_syntaxes,
    non_camel_case_types,
    non_fmt_panics,
    non_snake_case,
    non_upper_case_globals,
    static_mut_refs,
    unpredictable_function_pointer_comparisons,
    unused_attributes,
    unused_imports,
    unused_macros,
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

#![allow(unknown_lints)]
// #![feature(tool_lints)]
extern crate errno;
extern crate exec;
extern crate glob;
extern crate libc;
extern crate lineread;
extern crate nix;
extern crate regex;
extern crate rusqlite;
extern crate yaml_rust;

extern crate clap;

#[macro_use]
extern crate lazy_static;
extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::env;
use std::io::Write;
use std::sync::Arc;

use lineread::{Command, Interface, ReadResult};
*/
#[macro_use] extern crate bitflags;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate libc;
#[macro_use] extern crate nix;
#[macro_use] extern crate rand;
#[macro_use] extern crate regex as re;
#[macro_use] extern crate smallvec;
#[macro_use] extern crate time as temporal;
#[macro_use] extern crate unicode_normalization;
#[macro_use] extern crate unicode_width;

#[macro_use] pub mod macros
{
    #[macro_export] macro_rules! log
    {
        ($fmt:expr) =>
        (
            let log_file = if let Ok(x) = std::env::var("CICADA_LOG_FILE") {
                x.clone()
            } else {
                String::new()
            };

            if !log_file.is_empty() {
                use std::io::Write as _;

                let msg = $fmt;
                match std::fs::OpenOptions::new().append(true).create(true).open(&log_file) {
                    Ok(mut cfile) => {
                        let pid = ::process::read_pid();
                        let now = $crate::ctime::DateTime::now();
                        let msg = format!("[{}][{}] {}", now, pid, msg);
                        let msg = if msg.ends_with('\n') { msg } else { format!("{}\n", msg) };
                        match cfile.write_all(msg.as_bytes()) {
                            Ok(_) => {}
                            Err(_) => println!("tlog: write_all error")
                        }
                    }
                    Err(_) => println!("tlog: open file error"),
                }

            }
        );

        ($fmt:expr, $($arg:tt)*) =>
        (
            let msg = format!($fmt, $($arg)*);
            log!(&msg);
        );
    }

    #[macro_export] macro_rules! println_stderr
    {
        ($fmt:expr) =>
        (
            match writeln!(&mut ::std::io::stderr(), $fmt) {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );

        ($fmt:expr, $($arg:tt)*) =>
        (
            match writeln!(&mut ::io::stderr(), $fmt, $($arg)*)
            {
                Ok(_) => {}
                Err(e) => println!("write to stderr failed: {:?}", e)
            }
        );
    }
}

pub mod boxed
{
    pub use std::boxed::{ * };
}

pub mod builtins
{
    /*!
    */
    use ::
    {
        error::no::{ errno },
        fs::{ File },
        io::{ Write },
        os::unix::io::{ FromRawFd, RawFd },
        *,
    };
    /*
    */
    pub mod alias
    {
        /*!
        */
        use ::
        {
            builtins::
            {
                print_stderr_with_capture,
                print_stdout_with_capture,
            },
            regex::{ Regex },
            types::{ * },
            *,
        };
        /*
        */
        pub fn run
        (
            sh: &mut shell::Shell,
            cl: &CommandLine,
            cmd: &Command,
            capture: bool,
        ) -> CommandResult 
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();

            if tokens.len() == 1 {
                return show_alias_list(sh, cmd, cl, capture);
            }

            if tokens.len() > 2 {
                let info = "alias syntax error: usage: alias foo='echo foo'";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let input = &tokens[1].1;
            let re_single_read = Regex::new(r"^[a-zA-Z0-9_\.-]+$").unwrap();
            if re_single_read.is_match(input) {
                return show_single_alias(sh, input, cmd, cl, capture);
            }

            let re_to_add = Regex::new(r"^([a-zA-Z0-9_\.-]+)=(.*)$").unwrap();
            for cap in re_to_add.captures_iter(input) {
                let name = tools::unquote(&cap[1]);
                // due to limitation of `parses::parser_line`,
                // `alias foo-bar='foo bar'` will become 'foo-bar=foo bar'
                // while `alias foo_bar='foo bar'` keeps foo_bar='foo bar'
                let value = if cap[2].starts_with('"') || cap[2].starts_with('\'') {
                    tools::unquote(&cap[2])
                } else {
                    cap[2].to_string()
                };
                sh.add_alias(name.as_str(), value.as_str());
            }

            CommandResult::new()
        }

        pub fn show_alias_list
        (
            sh: &shell::Shell,
            cmd: &Command,
            cl: &CommandLine,
            capture: bool,
        ) -> CommandResult 
        {
            let mut lines = Vec::new();
            for (name, value) in sh.get_alias_list() {
                let line = format!("alias {}='{}'", name, value);
                lines.push(line);
            }
            let buffer = lines.join("\n");
            let mut cr = CommandResult::new();
            print_stdout_with_capture(&buffer, &mut cr, cl, cmd, capture);
            cr
        }

        pub fn show_single_alias
        (
            sh: &shell::Shell,
            name_to_find: &str,
            cmd: &Command,
            cl: &CommandLine,
            capture: bool,
        ) -> CommandResult 
        {
            let mut cr = CommandResult::new();
            if let Some(content) = sh.get_alias_content(name_to_find) {
                let info = format!("alias {}='{}'", name_to_find, content);
                print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
            } else {
                let info = format!(":: alias: {}: not found", name_to_find);
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
            }
            cr
        }
    }

    pub mod bg
    {
        /*!
        */
        use ::
        {
            builtins::{ print_stderr_with_capture },
            shell::{ Shell },
            types::{ * },
            *,
        };
        /*
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if sh.jobs.is_empty() {
                let info = ":: bg: no job found";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let mut job_id = -1;
            if tokens.len() == 1 {
                if let Some((gid, _)) = sh.jobs.iter().next() {
                    job_id = *gid;
                }
            }

            if tokens.len() >= 2 {
                let mut job_str = tokens[1].1.clone();
                if job_str.starts_with("%") {
                    job_str = job_str.trim_start_matches('%').to_string();
                }

                match job_str.parse::<i32>() {
                    Ok(n) => job_id = n,
                    Err(_) => {
                        let info = ":: bg: invalid job id";
                        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                }
            }
            if job_id == -1 {
                let info = ":: bg: not such job";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let gid: i32;

            {
                let mut result = sh.get_job_by_id(job_id);
                // fall back to find job by using prcess group id
                if result.is_none() {
                    result = sh.get_job_by_gid(job_id);
                }

                match result {
                    Some(job) => {
                        unsafe {
                            libc::killpg(job.gid, libc::SIGCONT);
                            gid = job.gid;
                            if job.status == "Running" {
                                let info = format!(":: bg: job {} already in background", job.id);
                                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                                return cr;
                            }
                        }

                        let info_cmd = format!("[{}]  {} &", job.id, job.cmd);
                        print_stderr_with_capture(&info_cmd, &mut cr, cl, cmd, capture);
                        cr.status = 0;
                    }
                    None => {
                        let info = ":: bg: not such job";
                        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                }
            }

            jobc::mark_job_as_running(sh, gid, true);
            cr
        }
    }

    pub mod cd
    {
        /*!
        */
        use ::
        {
            path::{ Path },
            builtins::{ print_stderr_with_capture },
            types::{ * },
            *,
        };
        /*
        */
        pub fn run
        (
            sh: &mut shell::Shell,
            cl: &CommandLine,
            cmd: &Command,
            capture: bool,
        ) -> CommandResult 
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();
            let args = parses::lines::tokens_to_args(&tokens);

            if args.len() > 2 {
                let info = ":: cd: too many argument";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let str_current_dir = tools::get_current_dir();

            let mut dir_to = if args.len() == 1 {
                let home = tools::get_user_home();
                home.to_string()
            } else {
                args[1..].join("")
            };

            if dir_to == "-" {
                if sh.previous_dir.is_empty() {
                    let info = "no previous dir";
                    print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                    return cr;
                }
                dir_to = sh.previous_dir.clone();
            } else if !dir_to.starts_with('/') {
                dir_to = format!("{}/{}", str_current_dir, dir_to);
            }

            if !Path::new(&dir_to).exists() {
                let info = format!(":: cd: {}: No such file or directory", &args[1]);
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                return cr;
            }

            match Path::new(&dir_to).canonicalize() {
                Ok(p) => {
                    dir_to = p.as_path().to_string_lossy().to_string();
                }
                Err(e) => {
                    let info = format!(":: cd: error: {}", e);
                    print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                    return cr;
                }
            }

            match env::set_current_dir(&dir_to) {
                Ok(_) => {
                    sh.current_dir = dir_to.clone();
                    if str_current_dir != dir_to {
                        sh.previous_dir = str_current_dir.clone();
                        env::set_var("PWD", &sh.current_dir);
                    };
                    cr.status = 0;
                    cr
                }
                Err(e) => {
                    let info = format!(":: cd: {}", e);
                    print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                    cr
                }
            }
        }
    }

    pub mod cinfo
    {
        /*!
        */
        use ::
        {
            builtins::{ print_stdout_with_capture },
            shell::{ Shell },
            types::{ * },
            *,
        };
        /*
        */
        pub fn run(_sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut info = vec![];
            const VERSION: &str = "0.0.0"; // env!("CARGO_PKG_VERSION");
            info.push(("version", VERSION));

            let os_name = libs::os_type::get_os_name();
            info.push(("os-name", &os_name));

            let hfile = history::get_history_file();
            info.push(("history-file", &hfile));

            let rcf = rcfile::get_rc_file();
            info.push(("rc-file", &rcf));

            let git_hash = "abcdefghi"; // env!("GIT_HASH");
            
            if !git_hash.is_empty()
            {
                info.push(("git-commit", "main" )); // env!("GIT_HASH")));
            }

            let git_branch = "main"; // env!("GIT_BRANCH");
            let mut branch = String::new();
            
            if !git_branch.is_empty()
            {
                branch.push_str(git_branch);
                let git_status = "0"; //env!("GIT_STATUS");
                
                if git_status != "0" {
                    branch.push_str(" (dirty)");
                }

                info.push(("git-branch", &branch));
            }

            info.push(("built-with", "1.91.0")); // env!("BUILD_RUSTC_VERSION")));
            info.push(("built-at", "26.1.1" )); //env!("BUILD_DATE")));

            let mut lines = Vec::new();
            
            for (k, v) in &info
            {
                // longest key above is 12-char length
                lines.push(format!("{: >12}: {}", k, v));
            }
            
            let buffer = lines.join("\n");
            let mut cr = CommandResult::new();
            print_stdout_with_capture(&buffer, &mut cr, cl, cmd, capture);
            cr
        }
    }

    pub mod exec
    {
        /*!
        */
        use ::
        {
            builtins::{ print_stderr_with_capture },
            shell::{ Shell },
            types::{ * },
            *,
        };
        /*
        */
        pub fn run(_sh: &Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();
            let args = parses::lines::tokens_to_args(&tokens);
            let len = args.len();

            if len == 1
            {
                print_stderr_with_capture("invalid usage", &mut cr, cl, cmd, capture);
                return cr;
            }

            let mut _cmd = execute::Command::new(&args[1]);
            let err = _cmd.args(&args[2..len]).exec();
            let info = format!(":: exec: {}", err);
            print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
            cr
        }
    }

    pub mod exit
    {
        /*!
        */
        use ::
        {
            builtins::{ print_stderr_with_capture },
            shell::{ Shell },
            types::{ * },
            *,
        };
        /*
        */
        pub fn run(sh: &Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();
            if tokens.len() > 2 {
                let info = ":: exit: too many arguments";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            if tokens.len() == 2 {
                let _code = &tokens[1].1;
                match _code.parse::<i32>() {
                    Ok(x) => {
                        process::exit(x);
                    }
                    Err(_) => {
                        let info = format!(":: exit: {}: numeric argument required", _code);
                        print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                        process::exit(255);
                    }
                }
            }

            for (_i, job) in sh.jobs.iter() {
                if !job.cmd.starts_with("nohup ") {
                    let mut info = String::new();
                    info.push_str("There are background jobs.");
                    info.push_str("Run `jobs` to see details; `exit 1` to force quit.");
                    print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                    return cr;
                }
            }

            process::exit(0);
            cr
        }
    }

    pub mod export
    {
        /*!
        */
        use ::
        {
            builtins::{ print_stderr_with_capture },
            regex::{ Regex },
            shell::{ Shell },
            types::{ * },
            *,
        };
        /*
        */
        pub fn run(_sh: &Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();

            let re_name_ptn = Regex::new(r"^([a-zA-Z_][a-zA-Z0-9_]*)=(.*)$").unwrap();
            for (_, text) in tokens.iter() {
                if text == "export" {
                    continue;
                }

                if !tools::is_env(text) {
                    let mut info = String::new();
                    info.push_str("export: invalid command\n");
                    info.push_str("usage: export XXX=YYY");
                    print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                    return cr;
                }

                if !re_name_ptn.is_match(text) {
                    let mut info = String::new();
                    info.push_str("export: invalid command\n");
                    info.push_str("usage: export XXX=YYY ZZ=123");
                    print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                    return cr;
                }

                for cap in re_name_ptn.captures_iter(text) {
                    let name = cap[1].to_string();
                    let token = parses::lines::unquote(&cap[2]);
                    let value = libs::path::expand_home(&token);
                    env::set_var(name, &value);
                }
            }
            cr
        }
    }

    pub mod fg
    {
        /*!
        */
        use ::
        {
            builtins::{ print_stderr_with_capture },
            shell::{self, Shell},
            types::{*},
            *,
        };
        /*
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if sh.jobs.is_empty() {
                let info = ":: fg: no job found";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let mut job_id = -1;
            if tokens.len() == 1 {
                if let Some((gid, _)) = sh.jobs.iter().next() {
                    job_id = *gid;
                }
            }

            if tokens.len() >= 2 {
                let mut job_str = tokens[1].1.clone();
                if job_str.starts_with("%") {
                    job_str = job_str.trim_start_matches('%').to_string();
                }

                match job_str.parse::<i32>() {
                    Ok(n) => job_id = n,
                    Err(_) => {
                        let info = ":: fg: invalid job id";
                        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                }
            }

            if job_id == -1 {
                let info = ":: not job id found";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let gid: i32;
            let pid_list: Vec<i32>;

            {
                let mut result = sh.get_job_by_id(job_id);
                // fall back to find job by using prcess group id
                if result.is_none() {
                    result = sh.get_job_by_gid(job_id);
                }

                match result {
                    Some(job) => {
                        print_stderr_with_capture(&job.cmd, &mut cr, cl, cmd, capture);
                        cr.status = 0;

                        unsafe {
                            if !shell::give_terminal_to(job.gid) {
                                return CommandResult::error();
                            }

                            libc::killpg(job.gid, libc::SIGCONT);
                            pid_list = job.pids.clone();
                            gid = job.gid;
                        }
                    }
                    None => {
                        let info = ":: fg: no such job";
                        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                }
            }

            unsafe {
                jobc::mark_job_as_running(sh, gid, false);

                let cr = jobc::wait_fg_job(sh, gid, &pid_list);

                let gid_shell = libc::getpgid(0);
                if !shell::give_terminal_to(gid_shell) {
                    log!("failed to give term to back to shell : {}", gid_shell);
                }

                cr
            }
        }
    }

    pub mod history
    {
        /*!
        */
        use ::
        {
            builtins::{ print_stdout_with_capture, print_stderr_with_capture },
            path::{ Path },
            rusqlite::{ Connection as Conn },
            shell::{ Shell },
            types::{ * },
            *,
        };
        /*
        */
        // #[derive(Debug, StructOpt)]
        // #[structopt(name = "history", about = "History in cicada shell")]
        #[derive( Debug )]
        struct OptMainHistory
        {
            session: bool,
            asc: bool,
            pwd: bool,
            only_id: bool,
            no_id: bool,
            show_date: bool,
            limit: i32,
            pattern: String,
            cmd: Option<SubCommand>,
        }

        // #[derive(StructOpt, Debug)]
        #[derive( Debug )]
        enum SubCommand
        {
            Add
            {
                timestamp: Option<f64>,
                input: String,
            },
            
            Delete
            {
                rowid: Vec<usize>,
            },
        }

        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult 
        {
            let mut cr = CommandResult::new();
            let hfile = history::get_history_file();
            let path = Path::new(hfile.as_str());
            if !path.exists() {
                let info = "no history file";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }
            let conn = match Conn::open(&hfile) {
                Ok(x) => x,
                Err(e) => {
                    let info = format!("history: sqlite error: {:?}", e);
                    print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                    return cr;
                }
            };

            let tokens = cmd.tokens.clone();
            let args = parses::lines::tokens_to_args(&tokens);

            let show_usage = args.len() > 1 && (args[1] == "-h" || args[1] == "--help");
            let opt = OptMain::from_iter_safe(args);
            match opt {
                Ok(opt) => match opt.cmd {
                    Some(SubCommand::Delete { rowid: rowids }) => {
                        let mut _count = 0;
                        for rowid in rowids {
                            let _deleted = delete_history_item(&conn, rowid);
                            if _deleted {
                                _count += 1;
                            }
                        }
                        if _count > 0 {
                            let info = format!("deleted {} items", _count);
                            print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                        }
                        cr
                    }
                    Some(SubCommand::Add {
                        timestamp: ts,
                        input,
                    }) => {
                        let ts = ts.unwrap_or(0 as f64);
                        add_history(sh, ts, &input);
                        cr
                    }
                    None => {
                        let (str_out, str_err) = list_current_history(sh, &conn, &opt);
                        if !str_out.is_empty() {
                            print_stdout_with_capture(&str_out, &mut cr, cl, cmd, capture);
                        }
                        if !str_err.is_empty() {
                            print_stderr_with_capture(&str_err, &mut cr, cl, cmd, capture);
                        }
                        cr
                    }
                },
                Err(e) => {
                    let info = format!("{}", e);
                    if show_usage {
                        print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                        cr.status = 0;
                    } else {
                        print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                        cr.status = 1;
                    }
                    cr
                }
            }
        }

        fn add_history(sh: &Shell, ts: f64, input: &str) 
        {
            let (tsb, tse) = (ts, ts + 1.0);
            history::add_raw(sh, input, 0, tsb, tse);
        }

        fn list_current_history(sh: &Shell, conn: &Conn, opt: &OptMainHistory ) -> (String, String) 
        {
            let mut result_stderr = String::new();
            let result_stdout = String::new();

            let history_table = history::get_history_table();
            let mut sql = format!(
                "SELECT ROWID, inp, tsb FROM {} WHERE ROWID > 0",
                history_table
            );
            if !opt.pattern.is_empty() {
                sql = format!("{} AND inp LIKE '%{}%'", sql, opt.pattern)
            }
            if opt.session {
                sql = format!("{} AND sessionid = '{}'", sql, sh.session_id)
            }
            if opt.pwd {
                sql = format!("{} AND info like '%dir:{}|%'", sql, sh.current_dir)
            }

            if opt.asc {
                sql = format!("{} ORDER BY tsb", sql);
            } else {
                sql = format!("{} order by tsb desc", sql);
            };
            sql = format!("{} limit {} ", sql, opt.limit);

            let mut stmt = match conn.prepare(&sql) {
                Ok(x) => x,
                Err(e) => {
                    let info = format!("history: prepare select error: {:?}", e);
                    result_stderr.push_str(&info);
                    return (result_stdout, result_stderr);
                }
            };

            let mut rows = match stmt.query([]) {
                Ok(x) => x,
                Err(e) => {
                    let info = format!("history: query error: {:?}", e);
                    result_stderr.push_str(&info);
                    return (result_stdout, result_stderr);
                }
            };

            let mut lines = Vec::new();
            loop {
                match rows.next() {
                    Ok(_rows) => {
                        if let Some(row) = _rows {
                            let row_id: i32 = match row.get(0) {
                                Ok(x) => x,
                                Err(e) => {
                                    let info = format!("history: error: {:?}", e);
                                    result_stderr.push_str(&info);
                                    return (result_stdout, result_stderr);
                                }
                            };
                            let inp: String = match row.get(1) {
                                Ok(x) => x,
                                Err(e) => {
                                    let info = format!("history: error: {:?}", e);
                                    result_stderr.push_str(&info);
                                    return (result_stdout, result_stderr);
                                }
                            };

                            if opt.no_id {
                                lines.push(inp.to_string());
                            } else if opt.only_id {
                                lines.push(row_id.to_string());
                            } else if opt.show_date {
                                let tsb: f64 = match row.get(2) {
                                    Ok(x) => x,
                                    Err(e) => {
                                        let info = format!("history: error: {:?}", e);
                                        result_stderr.push_str(&info);
                                        return (result_stdout, result_stderr);
                                    }
                                };
                                let dt = ctime::DateTime::from_timestamp(tsb);
                                lines.push(format!("{}: {}: {}", row_id, dt, inp));
                            } else {
                                lines.push(format!("{}: {}", row_id, inp));
                            }
                        } else {
                            break;
                        }
                    }
                    Err(e) => {
                        let info = format!("history: rows next error: {:?}", e);
                        result_stderr.push_str(&info);
                        return (result_stdout, result_stderr);
                    }
                }
            }

            if !opt.asc {
                lines.reverse();
            }

            let buffer = lines.join("\n");

            (buffer, result_stderr)
        }

        fn delete_history_item(conn: &Conn, rowid: usize) -> bool 
        {
            let history_table = history::get_history_table();
            let sql = format!("DELETE from {} where rowid = {}", history_table, rowid);
            match conn.execute(&sql, []) {
                Ok(_) => true,
                Err(e) => {
                    log!("history: error when delete: {:?}", e);
                    false
                }
            }
        }
    }

    pub mod jobs
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use crate::builtins::utils::print_stdout_with_capture;
        use crate::jobc;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut cr = CommandResult::new();
            if sh.jobs.is_empty() {
                return cr;
            }

            // update status of jobs if any
            jobc::try_wait_bg_jobs(sh, false, false);

            let mut lines = Vec::new();
            let jobs = sh.jobs.clone();
            let no_trim = cmd.tokens.len() >= 2 && cmd.tokens[1].1 == "-f";
            for (_i, job) in jobs.iter() {
                let line = jobc::get_job_line(job, !no_trim);
                lines.push(line);
            }
            let buffer = lines.join("\n");

            print_stdout_with_capture(&buffer, &mut cr, cl, cmd, capture);
            cr
        }
    }

    pub mod minfd
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::io::Write;
        use std::os::fd::AsRawFd;

        use crate::builtins::utils::print_stdout_with_capture;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(_sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut cr = CommandResult::new();

            let fd = nix::fcntl::open(
                "/dev/null",
                nix::fcntl::OFlag::empty(),
                nix::sys::stat::Mode::empty(),
            );
            match fd {
                Ok(fd) => {
                    let info = format!("{}", fd.as_raw_fd());
                    print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                }
                Err(e) => {
                    println_stderr!(":: minfd: error: {}", e);
                }
            }

            cr
        }
    }

    pub mod read
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::io;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::libs::re::re_contains;
        use crate::shell::Shell;
        use crate::tools;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        fn _find_invalid_identifier(name_list: &Vec<String>) -> Option<String> {
            for id_ in name_list {
                if !re_contains(id_, r"^[a-zA-Z_][a-zA-Z0-9_]*$") {
                    return Some(id_.to_string());
                }
            }
            None
        }

        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();

            let name_list: Vec<String>;
            if tokens.len() <= 1 {
                name_list = vec!["REPLY".to_string()];
            } else {
                name_list = tokens[1..].iter().map(|x| x.1.clone()).collect();
                if let Some(id_) = _find_invalid_identifier(&name_list) {
                    let info = format!(":: read: `{}': not a valid identifier", id_);
                    print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                    return cr;
                }
            }

            let mut buffer = String::new();

            if cmd.has_here_string() {
                if let Some(redirect_from) = &cmd.redirect_from {
                    buffer.push_str(&redirect_from.1);
                    buffer.push('\n');
                }
            } else {
                match io::stdin().read_line(&mut buffer) {
                    Ok(_) => {}
                    Err(e) => {
                        let info = format!(":: read: error in reading stdin: {:?}", e);
                        print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                }
            }

            let envs = cl.envs.clone();
            let value_list = tools::split_into_fields(sh, buffer.trim(), &envs);

            let idx_2rd_last = name_list.len() - 1;
            for i in 0..idx_2rd_last {
                let name = name_list.get(i);
                if name.is_none() {
                    let info = ":: read: name index error";
                    print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                    return cr;
                }
                let name = name.unwrap();

                let value = value_list.get(i).unwrap_or(&String::new()).clone();
                sh.set_env(name, &value);
            }

            let name_last = &name_list[idx_2rd_last];
            let value_left: String = if value_list.len() > idx_2rd_last {
                value_list[idx_2rd_last..].join(" ")
            } else {
                String::new()
            };
            sh.set_env(name_last, &value_left);
            cr
        }
    }

    pub mod set
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use structopt::StructOpt;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::builtins::utils::print_stdout_with_capture;
        use crate::parsers;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        //#[derive(Debug, StructOpt)]
        #[derive( Debug )]
        struct OptMainSet
        {
            exit_on_error: bool,
        }

        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = &cmd.tokens;
            let args = parses::lines::tokens_to_args(tokens);
            let show_usage = args.len() > 1 && (args[1] == "-h" || args[1] == "--help");

            let opt = OptMain::from_iter_safe(args);
            match opt {
                Ok(opt) => {
                    if opt.exit_on_error {
                        sh.exit_on_error = true;
                        cr
                    } else {
                        let info = ":: set: option not implemented";
                        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                        cr
                    }
                }
                Err(e) => {
                    let info = format!("{}", e);
                    if show_usage {
                        print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                        cr.status = 0;
                    } else {
                        print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                        cr.status = 1;
                    }
                    cr
                }
            }
        }
    }

    pub mod source
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use crate::builtins::utils::print_stderr_with_capture;
        use crate::parsers;
        use crate::scripting;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = &cmd.tokens;
            let args = parses::lines::tokens_to_args(tokens);

            if args.len() < 2 {
                let info = ":: source: no file specified";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let status = scripting::run_script(sh, &args);
            cr.status = status;
            cr
        }
    }

    pub mod ulimit
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use crate::builtins::utils::print_stderr_with_capture;
        use crate::builtins::utils::print_stdout_with_capture;
        use crate::parsers;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        use clap::{CommandFactory, Parser};
        use std::io::Error;
        
        #[derive(Parser)]
        #[command(name = "ulimit", about = "show / modify shell resource limits")]
        #[allow(non_snake_case)] */
        struct App
        {
            /*#[arg(short, help = "All current limits are reported.")]*/
            a: bool,
            /*#[arg(
                short,
                value_name = "NEW VALUE",
                help = "The maximum number of open file descriptors."
            )]*/
            n: Option<Option<u64>>,
            /*#[arg(
                short,
                value_name = "NEW VALUE",
                help = "The maximum size of core files created."
            )]*/
            c: Option<Option<u64>>,
            /*
            #[arg(
                short = 'S',
                help = "Set a soft limit for the given resource. (default)"
            )]*/
            S: bool,
            /* #[arg(short = 'H', help = "Set a hard limit for the given resource.")] */
            H: bool,
        }

        pub fn run(_sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let mut cr = CommandResult::new();
            let tokens = &cmd.tokens;
            let args = parses::lines::tokens_to_args(tokens);

            if args.contains(&"--help".to_string()) || args.contains(&"-h".to_string()) {
                App::command().print_help().unwrap();
                println!();
                return cr;
            }

            let app = App::parse_from(args);

            if app.H && app.S {
                println!(":: ulimit: Cannot both hard and soft.");
                cr.status = 1;
                return cr;
            }

            let mut all_stdout = String::new();
            let mut all_stderr = String::new();

            if app.a {
                report_all(&app, &mut all_stdout, &mut all_stderr);
            } else if handle_limit(app.n, "open_files", app.H, &mut all_stdout, &mut all_stderr)
                || handle_limit(
                    app.c,
                    "core_file_size",
                    app.H,
                    &mut all_stdout,
                    &mut all_stderr,
                )
            {
            } else {
                report_all(&app, &mut all_stdout, &mut all_stderr);
            }

            if !all_stdout.is_empty() {
                print_stdout_with_capture(&all_stdout, &mut cr, cl, cmd, capture);
            }
            if !all_stderr.is_empty() {
                print_stderr_with_capture(&all_stderr, &mut cr, cl, cmd, capture);
            }

            cr
        }

        fn set_limit(limit_name: &str, value: u64, for_hard: bool) -> String {
            let limit_id = match limit_name {
                "open_files" => libc::RLIMIT_NOFILE,
                "core_file_size" => libc::RLIMIT_CORE,
                _ => return String::from("invalid limit name"),
            };

            let mut rlp = libc::rlimit {
                rlim_cur: 0,
                rlim_max: 0,
            };

            unsafe {
                if libc::getrlimit(limit_id, &mut rlp) != 0 {
                    return format!(
                        ":: ulimit: error getting limit: {}",
                        Error::last_os_error()
                    );
                }
            }

            // to support armv7-linux-gnueabihf & 32-bit musl systems
            if for_hard {
                #[cfg(all(target_pointer_width = "32", target_env = "gnu"))]
                {
                    rlp.rlim_max = value as u32;
                }
                #[cfg(not(all(target_pointer_width = "32", target_env = "gnu")))]
                {
                    rlp.rlim_max = value;
                }
            } else {
                #[cfg(all(target_pointer_width = "32", target_env = "gnu"))]
                {
                    rlp.rlim_cur = value as u32;
                }
                #[cfg(not(all(target_pointer_width = "32", target_env = "gnu")))]
                {
                    rlp.rlim_cur = value;
                }
            }

            unsafe {
                if libc::setrlimit(limit_id, &rlp) != 0 {
                    return format!(
                        ":: ulimit: error setting limit: {}",
                        Error::last_os_error()
                    );
                }
            }

            String::new()
        }

        fn get_limit(limit_name: &str, single_print: bool, for_hard: bool) -> (String, String) {
            let (desc, limit_id) = match limit_name {
                "open_files" => ("open files", libc::RLIMIT_NOFILE),
                "core_file_size" => ("core file size", libc::RLIMIT_CORE),
                _ => {
                    return (
                        String::new(),
                        String::from("ulimit: error: invalid limit name"),
                    )
                }
            };

            let mut rlp = libc::rlimit {
                rlim_cur: 0,
                rlim_max: 0,
            };

            let mut result_stdout = String::new();
            let mut result_stderr = String::new();

            unsafe {
                if libc::getrlimit(limit_id, &mut rlp) != 0 {
                    result_stderr.push_str(&format!("error getting limit: {}", Error::last_os_error()));
                    return (result_stdout, result_stderr);
                }

                let to_print = if for_hard { rlp.rlim_max } else { rlp.rlim_cur };

                let info = if to_print == libc::RLIM_INFINITY {
                    if single_print {
                        "unlimited\n".to_string()
                    } else {
                        format!("{}\t\tunlimited\n", desc)
                    }
                } else if single_print {
                    format!("{}\n", to_print)
                } else {
                    format!("{}\t\t{}\n", desc, to_print)
                };

                result_stdout.push_str(&info);
            }

            (result_stdout, result_stderr)
        }

        fn report_all(app: &App, all_stdout: &mut String, all_stderr: &mut String) {
            for limit_name in &["open_files", "core_file_size"] {
                let (out, err) = get_limit(limit_name, false, app.H);
                all_stdout.push_str(&out);
                all_stderr.push_str(&err);
            }
        }

        fn handle_limit(
            limit_option: Option<Option<u64>>,
            limit_name: &str,
            for_hard: bool,
            all_stdout: &mut String,
            all_stderr: &mut String,
        ) -> bool {
            match limit_option {
                None => false,
                Some(None) => {
                    let (out, err) = get_limit(limit_name, true, for_hard);
                    all_stdout.push_str(&out);
                    all_stderr.push_str(&err);
                    true
                }
                Some(Some(value)) => {
                    let err = set_limit(limit_name, value, for_hard);
                    if !err.is_empty() {
                        all_stderr.push_str(&err);
                    }
                    true
                }
            }
        }
    }

    pub mod unalias
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use crate::builtins::utils::print_stderr_with_capture;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if tokens.len() != 2 {
                let info = ":: unalias: syntax error";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let input = &tokens[1].1;
            if !sh.remove_alias(input) {
                let info = format!(":: unalias: {}: not found", input);
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                return cr;
            }
            cr
        }
    }

    pub mod unpath
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::path::Path;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if tokens.len() != 2 {
                let info = ":: unpath: syntax error";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let input = &tokens[1].1;
            sh.remove_path(Path::new(input));
            cr
        }
    }

    pub mod unset
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use crate::builtins::utils::print_stderr_with_capture;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if tokens.len() != 2 {
                let info = ":: unset: syntax error";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let input = &tokens[1].1;
            if !sh.remove_env(input) {
                let info = format!(":: unset: invalid varname: {:?}", input);
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                return cr;
            }
            cr
        }
    }

    pub mod vox
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::env;
        use std::fs;
        use std::path::Path;
        use std::path::PathBuf;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::builtins::utils::print_stdout_with_capture;
        use crate::execute;
        use crate::parsers;
        use crate::shell::{self, Shell};
        use crate::types::{self, Command, CommandLine, CommandResult};
        */
        fn in_env() -> bool {
            env::var("VIRTUAL_ENV").is_ok_and(|x| !x.is_empty())
        }

        fn get_envs_home() -> String {
            env::var("VIRTUALENV_HOME").unwrap_or_default()
        }

        fn get_all_venvs() -> Result<Vec<String>, String> {
            let home_envs = get_envs_home();
            if home_envs.is_empty() {
                let info = String::from("you need to set VIRTUALENV_HOME to use vox");
                return Err(info);
            }
            if !Path::new(home_envs.as_str()).exists() {
                match fs::create_dir_all(home_envs.as_str()) {
                    Ok(_) => {}
                    Err(e) => {
                        let info = format!("fs create_dir_all failed: {:?}", e);
                        return Err(info);
                    }
                }
            }

            let mut venvs = Vec::new();
            let pdir = home_envs.clone();
            if let Ok(list) = fs::read_dir(home_envs) {
                for ent in list.flatten() {
                    let ent_name = ent.file_name();
                    if let Ok(path) = ent_name.into_string() {
                        let full_path = format!("{}/{}/bin/activate", pdir, path);
                        if !Path::new(full_path.as_str()).exists() {
                            continue;
                        }
                        venvs.push(path);
                    }
                }
            }

            Ok(venvs)
        }

        fn enter_env(sh: &Shell, path: &str) -> String {
            if in_env() {
                return "vox: already in env".to_string();
            }

            let home_envs = get_envs_home();
            let full_path = format!("{}/{}/bin/activate", home_envs, path);
            if !Path::new(full_path.as_str()).exists() {
                return format!("no such env: {}", full_path);
            }

            let path_env = format!("{}/{}", home_envs, path);
            env::set_var("VIRTUAL_ENV", &path_env);
            let path_new = String::from("${VIRTUAL_ENV}/bin:$PATH");
            let mut tokens: types::Tokens = Vec::new();
            tokens.push((String::new(), path_new));
            shell::expand_env(sh, &mut tokens);
            env::set_var("PATH", &tokens[0].1);
            String::new()
        }

        fn exit_env(sh: &Shell) -> String {
            if !in_env() {
                return String::from("vox: not in an env");
            }

            let env_path = match env::var("PATH") {
                Ok(x) => x,
                Err(_) => {
                    return String::from("vox: cannot read PATH env");
                }
            };

            let mut vec_path: Vec<PathBuf> = env::split_paths(&env_path).collect();
            let path_virtual_env = String::from("${VIRTUAL_ENV}/bin");
            
            let mut tokens: types::Tokens = Vec::new();
            tokens.push((String::new(), path_virtual_env));
            shell::expand_env(sh, &mut tokens);
            let pathbuf_virtual_env = PathBuf::from(tokens[0].1.clone());
            vec_path
                .iter()
                .position(|n| n == &pathbuf_virtual_env)
                .map(|e| vec_path.remove(e));
            let env_path_new = env::join_paths(vec_path).unwrap_or_default();
            env::set_var("PATH", &env_path_new);
            env::set_var("VIRTUAL_ENV", "");

            String::new()
        }

        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();
            let args = parses::lines::tokens_to_args(&tokens);
            let len = args.len();
            let subcmd = if len > 1 { &args[1] } else { "" };

            if len == 1 || (len == 2 && subcmd == "ls") {
                match get_all_venvs() {
                    Ok(venvs) => {
                        let info = venvs.join("\n");
                        print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                    Err(reason) => {
                        print_stderr_with_capture(&reason, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                }
            }

            if len == 3 && subcmd == "create" {
                let pybin = match env::var("VIRTUALENV_PYBIN") {
                    Ok(x) => x,
                    Err(_) => "python3".to_string(),
                };
                let dir_venv = get_envs_home();
                let venv_name = args[2].to_string();
                let line = format!("{} -m venv \"{}/{}\"", pybin, dir_venv, venv_name);
                print_stderr_with_capture(&line, &mut cr, cl, cmd, capture);
                let cr_list = execute::run_command_line(sh, &line, false, false);
                return cr_list[0].clone();
            }

            if len == 3 && subcmd == "enter" {
                let _err = enter_env(sh, args[2].as_str());
                if !_err.is_empty() {
                    print_stderr_with_capture(&_err, &mut cr, cl, cmd, capture);
                }
                cr
            } else if len == 2 && subcmd == "exit" {
                let _err = exit_env(sh);
                if !_err.is_empty() {
                    print_stderr_with_capture(&_err, &mut cr, cl, cmd, capture);
                }
                cr
            } else {
                let info = ":: vox: invalid option";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                cr
            }
        }
    }
    
    fn _get_std_fds(redirects: &[Redirection]) -> (Option<RawFd>, Option<RawFd>) 
    {
        if redirects.is_empty() {
            return (None, None);
        }

        let mut fd_out = None;
        let mut fd_err = None;

        for i in 0..redirects.len() {
            let item = &redirects[i];
            if item.0 == "1" {
                let mut _fd_candidate = None;

                if item.2 == "&2" {
                    let (_fd_out, _fd_err) = _get_std_fds(&redirects[i + 1..]);
                    if let Some(fd) = _fd_err {
                        _fd_candidate = Some(fd);
                    } else {
                        _fd_candidate = unsafe { Some(libc::dup(2)) };
                    }
                } else {
                    let append = item.1 == ">>";
                    if let Ok(fd) = tools::create_raw_fd_from_file(&item.2, append) {
                        _fd_candidate = Some(fd);
                    }
                }
                
                if let Some(fd) = fd_out {
                    unsafe {
                        libc::close(fd);
                    }
                }

                fd_out = _fd_candidate;
            }

            if item.0 == "2" {
                let mut _fd_candidate = None;

                if item.2 == "&1" {
                    if let Some(fd) = fd_out {
                        _fd_candidate = unsafe { Some(libc::dup(fd)) };
                    }
                } else {
                    let append = item.1 == ">>";
                    if let Ok(fd) = tools::create_raw_fd_from_file(&item.2, append) {
                        _fd_candidate = Some(fd);
                    }
                }

                if let Some(fd) = fd_err {
                    unsafe {
                        libc::close(fd);
                    }
                }

                fd_err = _fd_candidate;
            }
        }

        (fd_out, fd_err)
    }

    fn _get_dupped_stdout_fd(cmd: &Command, cl: &CommandLine) -> RawFd 
    {
        if cl.with_pipeline() {
            return 1;
        }

        let (_fd_out, _fd_err) = _get_std_fds(&cmd.redirects_to);
        if let Some(fd) = _fd_err {
            unsafe {
                libc::close(fd);
            }
        }
        if let Some(fd) = _fd_out {
            fd
        } else {
            let fd = unsafe { libc::dup(1) };
            if fd == -1 {
                let eno = errno();
                println_stderr!(":: dup: {}", eno);
            }
            fd
        }
    }

    fn _get_dupped_stderr_fd(cmd: &Command, cl: &CommandLine) -> RawFd 
    {
        if cl.with_pipeline() {
            return 2;
        }

        let (_fd_out, _fd_err) = _get_std_fds(&cmd.redirects_to);
        if let Some(fd) = _fd_out {
            unsafe {
                libc::close(fd);
            }
        }

        if let Some(fd) = _fd_err {
            fd
        } else {
            let fd = unsafe { libc::dup(2) };
            if fd == -1 {
                let eno = errno();
                println_stderr!(":: dup: {}", eno);
            }
            fd
        }
    }

    pub fn print_stdout(info: &str, cmd: &Command, cl: &CommandLine) 
    {
        let fd = _get_dupped_stdout_fd(cmd, cl);
        if fd == -1 {
            return;
        }

        unsafe {
            let mut f = File::from_raw_fd(fd);
            let info = info.trim_end_matches('\n');
            match f.write_all(info.as_bytes()) {
                Ok(_) => {}
                Err(e) => {
                    println_stderr!("write_all: error: {}", e);
                }
            }
            if !info.is_empty() {
                match f.write_all(b"\n") {
                    Ok(_) => {}
                    Err(e) => {
                        println_stderr!("write_all: error: {}", e);
                    }
                }
            }
        }
    }

    pub fn print_stderr(info: &str, cmd: &Command, cl: &CommandLine) 
    {
        let fd = _get_dupped_stderr_fd(cmd, cl);
        if fd == -1 {
            return;
        }

        unsafe {
            let mut f = File::from_raw_fd(fd);
            let info = info.trim_end_matches('\n');
            match f.write_all(info.as_bytes()) {
                Ok(_) => (),
                Err(e) => {
                    println_stderr!("write_all: error: {}", e);
                }
            }

            if !info.is_empty() {
                match f.write_all(b"\n") {
                    Ok(_) => (),
                    Err(e) => {
                        println_stderr!("write_all: error: {}", e);
                    }
                }
            }
        }
    }

    pub fn print_stderr_with_capture
    (
        info: &str,
        cr: &mut CommandResult,
        cl: &CommandLine,
        cmd: &Command,
        capture: bool,
    ) 
    {
        cr.status = 1;
        if capture {
            cr.stderr = info.to_string();
        } else {
            print_stderr(info, cmd, cl);
        }
    }

    pub fn print_stdout_with_capture
    (
        info: &str,
        cr: &mut CommandResult,
        cl: &CommandLine,
        cmd: &Command,
        capture: bool,
    )
    {
        cr.status = 0;
        if capture {
            cr.stdout = info.to_string();
        } else {
            print_stdout(info, cmd, cl);
        }
    }
}

pub mod calculator
{
    /*!
    */
    use ::
    {
        num::{ Wrapping as W },
        iter::{ Pair, Pairs },
        parses::
        {
            error::{ Error },
            pratt::{ Assoc, Op, PrattParser },
            Parser
        },
        types::{ * },
        *,
    };
    /*
    #[derive(Parser)]
    #[grammar = "calculator/grammar.pest"] */
    struct Calculator;

    lazy_static! 
    {
        static ref PRATT_PARSER: PrattParser<Rule> = 
        {
            use self::Assoc::*;
            use self::Rule::*;

            PrattParser::new()
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::infix(power, Right))
        };
    }

    pub fn eval_int(expression: Pairs<Rule>) -> i64 
    {
        PRATT_PARSER.map_primary(|primary| match primary.as_rule()
        {
            Rule::num => primary.as_str().parse::<i64>().unwrap(),
            Rule::expr => eval_int(primary.into_inner()),
            _ => unreachable!(),
        })
        .map_infix(|lhs: i64, op: Pair<Rule>, rhs: i64| match op.as_rule()
        {
                Rule::add => (W(lhs) + W(rhs)).0,
                Rule::subtract => (W(lhs) - W(rhs)).0,
                Rule::multiply => (W(lhs) * W(rhs)).0,
                Rule::divide => {
                    if rhs == 0 {
                        (lhs as f64 / 0.0) as i64
                    } else {
                        (W(lhs) / W(rhs)).0
                    }
                }
                Rule::power => lhs.pow(rhs as u32),
                _ => unreachable!(),
        }).parse(expression)
    }

    pub fn eval_float(expression: Pairs<Rule>) -> f64 
    {
        PRATT_PARSER
            .map_primary(|primary| match primary.as_rule() {
                Rule::num => primary.as_str().parse::<f64>().unwrap(),
                Rule::expr => eval_float(primary.into_inner()),
                _ => unreachable!(),
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::add => lhs + rhs,
                Rule::subtract => lhs - rhs,
                Rule::multiply => lhs * rhs,
                Rule::divide => lhs / rhs,
                Rule::power => lhs.powf(rhs),
                _ => unreachable!(),
            })
            .parse(expression)
    }

    pub fn calculate( line: &str ) -> Result<Pairs<'_, Rule>, Error<Rule>> { Calculator::parse(Rule::calculation, line) }
}

pub mod completers
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::path::Path;
    use std::sync::Arc;

    use lineread::complete::{Completer, Completion};
    use lineread::prompter::Prompter;
    use lineread::terminal::Terminal;

    use crate::libs;
    use crate::parsers;
    use crate::shell;
    use crate::tools;

    use regex::Regex;
    use std::env;

    use crate::libs;
    use crate::tools;
    */
    pub mod dots
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::borrow::Cow;
        use std::fs::File;
        use std::io::{Read, Write};
        use std::path::Path;

        use lineread::complete::escape;
        use lineread::complete::escaped_word_start;
        use lineread::complete::unescape;
        use lineread::complete::Suffix;
        use lineread::complete::{Completer, Completion};
        use lineread::prompter::Prompter;
        use lineread::terminal::Terminal;
        use yaml_rust::yaml::Hash;
        use yaml_rust::{Yaml, YamlLoader};

        use crate::execute;
        use crate::parsers;
        use crate::tools;
        */
        /// Performs completion by searching dotfiles
        pub struct DotsCompleter;

        impl<Term: Terminal> Completer<Term> for DotsCompleter {
            fn complete(
                &self,
                word: &str,
                reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                let line = reader.buffer();
                Some(complete_dots(line, word))
            }

            fn word_start(&self, line: &str, end: usize, _reader: &Prompter<Term>) -> usize {
                escaped_word_start(&line[..end])
            }

            fn quote<'a>(&self, word: &'a str) -> Cow<'a, str> {
                escape(word)
            }

            fn unquote<'a>(&self, word: &'a str) -> Cow<'a, str> {
                unescape(word)
            }
        }

        fn get_dot_file(line: &str) -> (String, String) {
            let args = parses::lines::line_to_plain_tokens(line);
            let dir = tools::get_user_completer_dir();
            let dot_file = format!("{}/{}.yaml", dir, args[0]);
            if !Path::new(&dot_file).exists() {
                return (String::new(), String::new());
            }
            let sub_cmd = if (args.len() >= 3 && !args[1].starts_with('-'))
                || (args.len() >= 2 && !args[1].starts_with('-') && line.ends_with(' '))
            {
                args[1].as_str()
            } else {
                ""
            };

            (dot_file, sub_cmd.to_string())
        }

        fn handle_lv1_string(res: &mut Vec<Completion>, value: &str, word: &str) {
            if !value.starts_with(word) && !value.starts_with('`') {
                return;
            }

            let linfo = parses::lines::parse_line(value);
            let tokens = linfo.tokens;
            if tokens.len() == 1 && tokens[0].0 == "`" {
                log!("run subcmd: {:?}", &tokens[0].1);
                let cr = execute::run(&tokens[0].1);
                let v: Vec<&str> = cr.stdout.split_whitespace().collect();
                for s in v {
                    if s.trim().is_empty() {
                        continue;
                    }
                    handle_lv1_string(res, s, word);
                }
                return;
            }

            let display = None;
            let suffix = Suffix::Default;
            res.push(Completion {
                completion: value.to_string(),
                display,
                suffix,
            });
        }

        fn handle_lv1_hash(res: &mut Vec<Completion>, h: &Hash, word: &str) {
            for v in h.values() {
                if let Yaml::Array(ref arr) = v {
                    for s in arr {
                        if let Yaml::String(value) = s {
                            if !value.starts_with(word) && !value.starts_with('`') {
                                continue;
                            }
                            handle_lv1_string(res, value, word);
                        }
                    }
                }
            }
        }

        fn complete_dots(line: &str, word: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            if line.trim().is_empty() {
                return res;
            }
            let (dot_file, sub_cmd) = get_dot_file(line);
            if dot_file.is_empty() {
                return res;
            }

            let mut f;
            match File::open(&dot_file) {
                Ok(x) => f = x,
                Err(e) => {
                    println_stderr!("\ncicada: open dot_file error: {:?}", e);
                    return res;
                }
            }

            let mut s = String::new();
            match f.read_to_string(&mut s) {
                Ok(_) => {}
                Err(e) => {
                    println_stderr!("\ncicada: read_to_string error: {:?}", e);
                    return res;
                }
            }

            let docs = match YamlLoader::load_from_str(&s) {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!("\ncicada: Bad Yaml file: {}: {:?}", dot_file, e);
                    return res;
                }
            };

            for doc in docs.iter() {
                match *doc {
                    Yaml::Array(ref v) => {
                        for x in v {
                            match *x {
                                Yaml::String(ref name) => {
                                    if !sub_cmd.is_empty() {
                                        continue;
                                    }
                                    handle_lv1_string(&mut res, name, word);
                                }
                                Yaml::Hash(ref h) => {
                                    if sub_cmd.is_empty() {
                                        for k in h.keys() {
                                            if let Yaml::String(value) = k {
                                                handle_lv1_string(&mut res, value, word);
                                            }
                                        }
                                    } else {
                                        let key = Yaml::from_str(&sub_cmd);
                                        if !h.contains_key(&key) {
                                            continue;
                                        }
                                        handle_lv1_hash(&mut res, h, word);
                                    }
                                }
                                _ => {
                                    println_stderr!("\nThis yaml file is in bad format: {}", dot_file);
                                }
                            }
                        }
                    }
                    _ => {
                        println_stderr!("\nThis yaml file is in bad format: {}", dot_file);
                    }
                }
            }
            res
        }
    }

    pub mod env
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::env;
        use std::sync::Arc;

        use lineread::complete::{Completer, Completion, Suffix};
        use lineread::prompter::Prompter;
        use lineread::terminal::Terminal;

        use crate::shell;
        */
        pub struct EnvCompleter {
            pub sh: Arc<shell::Shell>,
        }

        impl<Term: Terminal> Completer<Term> for EnvCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                let sh = Arc::try_unwrap(self.sh.clone());
                match sh {
                    Ok(x) => Some(complete_env(&x, word)),
                    Err(x) => Some(complete_env(&x, word)),
                }
            }
        }

        fn complete_env(sh: &shell::Shell, path: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            if path.trim().is_empty() {
                return res;
            }
            let mut prefix = path.to_string();
            prefix.remove(0);

            for (key, _) in env::vars_os() {
                let env_name = key.to_string_lossy().to_string();
                if env_name.starts_with(&prefix) {
                    res.push(Completion {
                        completion: format!("${}", env_name),
                        display: None,
                        suffix: Suffix::Default,
                    });
                }
            }

            // sh.envs is a just clone here; see FIXME in main.rs
            for key in sh.envs.keys() {
                if key.starts_with(&prefix) {
                    res.push(Completion {
                        completion: format!("${}", key),
                        display: None,
                        suffix: Suffix::Default,
                    });
                }
            }

            res
        }
    }

    pub mod make
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::env;
        use std::fs::File;
        use std::io::{BufRead, BufReader, Write};

        use regex::Regex;

        use lineread::complete::{Completer, Completion, Suffix};
        use lineread::prompter::Prompter;
        use lineread::terminal::Terminal;
        */
        pub struct MakeCompleter;

        impl<Term: Terminal> Completer<Term> for MakeCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                Some(complete_make(word))
            }
        }

        fn handle_file(ci: &mut Vec<Completion>, path: &str, file_path: &str, current_dir: &str) {
            if let Ok(f) = File::open(file_path) {
                let file = BufReader::new(&f);
                let re_cmd = match Regex::new(r"^ *([^ ]+):") {
                    Ok(x) => x,
                    Err(e) => {
                        println_stderr!(":: regex build error: {:?}", e);
                        return;
                    }
                };

                let re_include = match Regex::new(r"^ *include  *([^ ]+) *$") {
                    Ok(x) => x,
                    Err(e) => {
                        println_stderr!(":: regex build error: {:?}", e);
                        return;
                    }
                };

                for line in file.lines().map_while(Result::ok) {
                    if re_cmd.is_match(&line) {
                        for cap in re_cmd.captures_iter(&line) {
                            if !cap[1].starts_with(path) {
                                continue;
                            }
                            ci.push(Completion {
                                completion: cap[1].to_string(),
                                display: None,
                                suffix: Suffix::Default,
                            });
                        }
                    }
                    if re_include.is_match(&line) {
                        for cap in re_include.captures_iter(&line) {
                            let _file = &cap[1];
                            if _file.contains('/') {
                                handle_file(ci, path, _file, current_dir);
                            } else {
                                let make_file = current_dir.to_owned() + "/" + _file;
                                handle_file(ci, path, &make_file, current_dir);
                            }
                        }
                    }
                }
            }
        }

        fn complete_make(path: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            let current_dir = match env::current_dir() {
                Ok(dir) => match dir.to_str() {
                    Some(s) => s.to_string(),
                    None => {
                        println!(":: to_str error");
                        return res;
                    }
                },
                Err(e) => {
                    println!(":: get current_dir error: {:?}", e);
                    return res;
                }
            };

            let make_file = format!("{}/Makefile", current_dir);
            handle_file(&mut res, path, &make_file, &current_dir);
            res
        }
    }

    pub mod path
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::collections::HashSet;
        use std::env;
        use std::fs::read_dir;
        use std::io::Write;
        use std::iter::FromIterator;
        use std::os::unix::fs::PermissionsExt;
        use std::path::{PathBuf, MAIN_SEPARATOR};
        use std::sync::Arc;

        use lineread::complete::{Completer, Completion, Suffix};
        use lineread::terminal::Terminal;
        use lineread::Prompter;

        use crate::completers::utils;
        use crate::libs;
        use crate::parsers;
        use crate::shell;
        use crate::tools;
        */
        pub struct BinCompleter {
            pub sh: Arc<shell::Shell>,
        }
        pub struct CdCompleter;
        pub struct PathCompleter;

        fn is_env_prefix(line: &str) -> bool {
            libs::re::re_contains(line, r" *\$[a-zA-Z_][A-Za-z0-9_]*")
        }

        fn is_pipelined(path: &str) -> bool {
            if !path.contains('|') {
                return false;
            }
            !path.starts_with('"') && !path.starts_with('\'')
        }

        impl<Term: Terminal> Completer<Term> for BinCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                let sh = Arc::try_unwrap(self.sh.clone());
                match sh {
                    Ok(x) => Some(complete_bin(&x, word)),
                    Err(x) => Some(complete_bin(&x, word)),
                }
            }
        }

        impl<Term: Terminal> Completer<Term> for PathCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                Some(complete_path(word, false))
            }
        }

        impl<Term: Terminal> Completer<Term> for CdCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                Some(complete_path(word, true))
            }
        }

        fn needs_expand_home(line: &str) -> bool {
            libs::re::re_contains(line, r"( +~ +)|( +~/)|(^ *~/)|( +~ *$)")
        }

        /// Returns a sorted list of paths whose prefix matches the given path.
        pub fn complete_path(word: &str, for_dir: bool) -> Vec<Completion> {
            let is_env = is_env_prefix(word);
            let mut res = Vec::new();
            let linfo = parses::lines::parse_line(word);
            let tokens = linfo.tokens;
            let (path, path_sep) = if tokens.is_empty() {
                (String::new(), String::new())
            } else {
                let (ref _path_sep, ref _path) = tokens[tokens.len() - 1];
                (_path.clone(), _path_sep.clone())
            };

            let (_, _dir_orig, _f) = split_pathname(&path, "");
            let dir_orig = if _dir_orig.is_empty() {
                String::new()
            } else {
                _dir_orig.clone()
            };
            let mut path_extended = path.clone();
            if needs_expand_home(&path_extended) {
                utils::expand_home_string(&mut path_extended)
            }
            utils::expand_env_string(&mut path_extended);

            let (_, _dir_lookup, file_name) = split_pathname(&path_extended, "");
            let dir_lookup = if _dir_lookup.is_empty() {
                ".".to_string()
            } else {
                _dir_lookup.clone()
            };
            // let dir_lookup = _dir_lookup.unwrap_or(".");
            if let Ok(entries) = read_dir(dir_lookup) {
                for entry in entries.flatten() {
                    let pathbuf = entry.path();
                    let is_dir = pathbuf.is_dir();
                    if for_dir && !is_dir {
                        continue;
                    }

                    let entry_name = entry.file_name();
                    // TODO: Deal with non-UTF8 paths in some way
                    if let Ok(_path) = entry_name.into_string() {
                        if _path.starts_with(&file_name) {
                            let (name, display) = if !dir_orig.is_empty() {
                                (
                                    format!("{}{}{}", dir_orig, MAIN_SEPARATOR, _path),
                                    Some(_path),
                                )
                            } else {
                                (_path, None)
                            };
                            let mut name = str::replace(name.as_str(), "//", "/");
                            if path_sep.is_empty() && !is_env {
                                name = tools::escape_path(&name);
                            }
                            let mut quoted = false;
                            if !path_sep.is_empty() {
                                name = tools::wrap_sep_string(&path_sep, &name);
                                quoted = true;
                            }
                            let suffix = if is_dir {
                                if quoted {
                                    name.pop();
                                }
                                Suffix::Some(MAIN_SEPARATOR)
                            } else {
                                Suffix::Default
                            };
                            res.push(Completion {
                                completion: name,
                                display,
                                suffix,
                            });
                        }
                    }
                }
            }
            res.sort_by(|a, b| a.completion.cmp(&b.completion));
            res
        }

        // Split optional directory and prefix. (see its test cases for more details)
        fn split_pathname(path: &str, prefix: &str) -> (String, String, String) {
            if is_pipelined(path) {
                let tokens: Vec<&str> = path.rsplitn(2, '|').collect();
                let prefix = format!("{}|", tokens[1]);
                return split_pathname(tokens[0], &prefix);
            }
            match path.rfind('/') {
                Some(pos) => (
                    prefix.to_string(),
                    path[..=pos].to_string(),
                    path[pos + 1..].to_string(),
                ),
                None => (prefix.to_string(), String::new(), path.to_string()),
            }
        }

        /// Returns a sorted list of paths whose prefix matches the given path.
        fn complete_bin(sh: &shell::Shell, path: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            let (prefix, _, fname) = split_pathname(path, "");
            let env_path = match env::var("PATH") {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!(":: env error when complete_bin: {:?}", e);
                    return res;
                }
            };

            let mut checker: HashSet<String> = HashSet::new();

            // handle alias, builtins, and functions
            for func in sh.funcs.keys() {
                if !func.starts_with(&fname) {
                    continue;
                }
                if checker.contains(func) {
                    continue;
                }
                checker.insert(func.clone());
                res.push(Completion {
                    completion: func.to_owned(),
                    display: None,
                    suffix: Suffix::Default,
                });
            }
            for alias in sh.aliases.keys() {
                if !alias.starts_with(&fname) {
                    continue;
                }
                if checker.contains(alias) {
                    continue;
                }
                checker.insert(alias.clone());
                res.push(Completion {
                    completion: alias.to_owned(),
                    display: None,
                    suffix: Suffix::Default,
                });
            }

            let builtins = vec![
                "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg", "history", "jobs", "read",
                "source", "ulimit", "unalias", "vox", "minfd", "set", "unset", "unpath",
            ];
            for item in &builtins {
                if !item.starts_with(&fname) {
                    continue;
                }
                if checker.contains(*item) {
                    continue;
                }
                checker.insert(item.to_string());
                res.push(Completion {
                    completion: item.to_string(),
                    display: None,
                    suffix: Suffix::Default,
                });
            }

            let vec_path = env::split_paths(&env_path);
            let path_list: HashSet<PathBuf> = HashSet::from_iter(vec_path);

            for p in path_list {
                if let Ok(list) = read_dir(p) {
                    for entry in list.flatten() {
                        if let Ok(name) = entry.file_name().into_string() {
                            if name.starts_with(&fname) {
                                let _mode = match entry.metadata() {
                                    Ok(x) => x,
                                    Err(e) => {
                                        println_stderr!(":: metadata error: {:?}", e);
                                        continue;
                                    }
                                };
                                let mode = _mode.permissions().mode();
                                if mode & 0o111 == 0 {
                                    // not binary
                                    continue;
                                }
                                if checker.contains(&name) {
                                    continue;
                                }

                                let display = None;
                                let suffix = Suffix::Default;
                                checker.insert(name.clone());
                                // TODO: need to handle quoted: `$ "foo#bar"`
                                let name_e = tools::escape_path(&name);
                                let name_e = format!("{}{}", prefix, name_e);
                                res.push(Completion {
                                    completion: name_e,
                                    display,
                                    suffix,
                                });
                            }
                        }
                    }
                }
            }
            res
        }


    }

    pub mod ssh
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::fs::File;
        use std::io::{BufRead, BufReader};

        use regex::Regex;

        use lineread::complete::{Completer, Completion, Suffix};
        use lineread::terminal::Terminal;
        use lineread::Prompter;

        use crate::tools;
        */
        pub struct SshCompleter;

        impl<Term: Terminal> Completer<Term> for SshCompleter {
            fn complete(
                &self,
                word: &str,
                _reader: &Prompter<Term>,
                _start: usize,
                _end: usize,
            ) -> Option<Vec<Completion>> {
                Some(complete_ssh(word))
            }
        }

        fn complete_ssh(path: &str) -> Vec<Completion> {
            let mut res = Vec::new();
            let home = tools::get_user_home();
            let ssh_config = home + "/.ssh/config";
            if let Ok(f) = File::open(&ssh_config) {
                let file = BufReader::new(&f);
                let re = match Regex::new(r"^ *(?i)host +([^ ]+)") {
                    Ok(x) => x,
                    Err(e) => {
                        println!("Regex build error: {:?}", e);
                        return res;
                    }
                };
                for line in file.lines().map_while(Result::ok) {
                    if !re.is_match(&line) {
                        continue;
                    }
                    for cap in re.captures_iter(&line) {
                        if !cap[1].starts_with(path) {
                            continue;
                        }
                        res.push(Completion {
                            completion: cap[1].to_string(),
                            display: None,
                            suffix: Suffix::Default,
                        });
                    }
                }
            }
            res
        }
    }

    pub struct CicadaCompleter {
        pub sh: Arc<shell::Shell>,
    }

    fn for_make(line: &str) -> bool {
        libs::re::re_contains(line, r"^ *make ")
    }

    fn for_env(line: &str) -> bool {
        libs::re::re_contains(line, r" *\$[_a-zA-Z0-9]*$")
    }

    fn for_ssh(line: &str) -> bool {
        libs::re::re_contains(line, r"^ *(ssh|scp).* +[^ \./]+ *$")
    }

    fn for_cd(line: &str) -> bool {
        libs::re::re_contains(line, r"^ *cd +")
    }

    fn for_bin(line: &str) -> bool {
        let ptn = r"(^ *(sudo|which|nohup)? *[a-zA-Z0-9_\.-]+$)|(^.+\| *(sudo|which|nohup)? *[a-zA-Z0-9_\.-]+$)";
        libs::re::re_contains(line, ptn)
    }

    fn for_dots(line: &str) -> bool {
        let args = parses::lines::line_to_plain_tokens(line);
        let len = args.len();
        if len == 0 {
            return false;
        }
        let dir = tools::get_user_completer_dir();
        let dot_file = format!("{}/{}.yaml", dir, args[0]);
        Path::new(dot_file.as_str()).exists()
    }

    impl<Term: Terminal> Completer<Term> for CicadaCompleter {
        fn complete(
            &self,
            word: &str,
            reader: &Prompter<Term>,
            start: usize,
            _end: usize,
        ) -> Option<Vec<Completion>> {
            let line = reader.buffer();

            let completions: Option<Vec<Completion>>;
            if for_dots(line) {
                let cpl = Arc::new(dots::DotsCompleter);
                completions = cpl.complete(word, reader, start, _end);
            } else if for_ssh(line) {
                let cpl = Arc::new(ssh::SshCompleter);
                completions = cpl.complete(word, reader, start, _end);
            } else if for_make(line) {
                let cpl = Arc::new(make::MakeCompleter);
                completions = cpl.complete(word, reader, start, _end);
            } else if for_bin(line) {
                let cpl = Arc::new(path::BinCompleter {
                    sh: self.sh.clone(),
                });
                completions = cpl.complete(word, reader, start, _end);
            } else if for_env(line) {
                let cpl = Arc::new(env::EnvCompleter {
                    sh: self.sh.clone(),
                });
                completions = cpl.complete(word, reader, start, _end);
            } else if for_cd(line) {
                // `for_cd` should be put a bottom position, so that
                // `cd $SOME_ENV_<TAB>` works as expected.
                let cpl = Arc::new(path::CdCompleter);
                // completions for `cd` should not fail back to path-completion
                return cpl.complete(word, reader, start, _end);
            } else {
                completions = None;
            }

            if let Some(x) = completions {
                if !x.is_empty() {
                    return Some(x);
                }
            }

            // empty completions should fail back to path-completion,
            // so that `$ make generate /path/to/fi<Tab>` still works.
            let cpl = Arc::new(path::PathCompleter);
            cpl.complete(word, reader, start, _end)
        }

        fn word_start(&self, line: &str, end: usize, _reader: &Prompter<Term>) -> usize {
            escaped_word_start(&line[..end])
        }
    }

    pub fn escaped_word_start(line: &str) -> usize {
        let mut start_position: usize = 0;
        let mut found_bs = false;
        let mut found_space = false;
        let mut with_quote = false;
        let mut ch_quote = '\0';
        let mut extra_bytes = 0;
        for (i, c) in line.chars().enumerate() {
            if found_space {
                found_space = false;
                start_position = i + extra_bytes;
            }

            if c == '\\' {
                found_bs = true;
                continue;
            }
            if c == ' ' && !found_bs && !with_quote {
                found_space = true;
                continue;
            }

            if !with_quote && !found_bs && (c == '"' || c == '\'') {
                with_quote = true;
                ch_quote = c;
            } else if with_quote && !found_bs && ch_quote == c {
                with_quote = false;
            }

            let bytes_c = c.len_utf8();
            if bytes_c > 1 {
                extra_bytes += bytes_c - 1;
            }
            found_bs = false;
        }
        if found_space {
            start_position = line.len();
        }
        start_position
    }

    pub fn expand_home_string(text: &mut String) {
        let v = vec![
            r"(?P<head> +)~(?P<tail> +)",
            r"(?P<head> +)~(?P<tail>/)",
            r"^(?P<head> *)~(?P<tail>/)",
            r"(?P<head> +)~(?P<tail> *$)",
        ];
        for item in &v {
            let re;
            if let Ok(x) = Regex::new(item) {
                re = x;
            } else {
                return;
            }
            let home = tools::get_user_home();
            let ss = text.clone();
            let to = format!("$head{}$tail", home);
            let result = re.replace_all(ss.as_str(), to.as_str());
            *text = result.to_string();
        }
    }

    pub fn expand_env_string(text: &mut String) {
        // expand "$HOME/.local/share" to "/home/tom/.local/share"
        if !text.starts_with('$') {
            return;
        }
        let ptn = r"^\$([A-Za-z_][A-Za-z0-9_]*)";
        let mut env_value = String::new();
        match libs::re::find_first_group(ptn, text) {
            Some(x) => {
                if let Ok(val) = env::var(&x) {
                    env_value = val;
                }
            }
            None => {
                return;
            }
        }

        if env_value.is_empty() {
            return;
        }
        let t = text.clone();
        *text = libs::re::replace_all(&t, ptn, &env_value);
    }
}

pub mod collections
{
    pub use std::collections::{ * };
}

pub mod core
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::env;
    use std::ffi::{CStr, CString};
    use std::fs::File;
    use std::io::{Read, Write};
    use std::os::fd::RawFd;
    use std::os::unix::io::FromRawFd;
    use std::process;

    use libs::pipes::pipe;
    use nix::unistd::{execve, ForkResult};

    use crate::builtins;
    use crate::calculator;
    use crate::jobc;
    use crate::libs;
    use crate::parsers;
    use crate::scripting;
    use crate::shell::{self, Shell};
    use crate::tools;
    use crate::types::{CommandLine, CommandOptions, CommandResult};
    */
    fn try_run_builtin_in_subprocess(
        sh: &mut Shell,
        cl: &CommandLine,
        idx_cmd: usize,
        capture: bool,
    ) -> Option<i32> {
        if let Some(cr) = try_run_builtin(sh, cl, idx_cmd, capture) {
            return Some(cr.status);
        }
        None
    }

    fn try_run_builtin(
        sh: &mut Shell,
        cl: &CommandLine,
        idx_cmd: usize,
        capture: bool,
    ) -> Option<CommandResult> {
        // for builtin, only capture its outputs when it locates at the end
        let capture = capture && idx_cmd + 1 == cl.commands.len();

        if idx_cmd >= cl.commands.len() {
            println_stderr!("unexpected error in try_run_builtin");
            return None;
        }

        let cmd = &cl.commands[idx_cmd];
        let tokens = cmd.tokens.clone();
        let cname = tokens[0].1.clone();
        if cname == "alias" {
            let cr = builtins::alias::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "bg" {
            let cr = builtins::bg::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "cd" {
            let cr = builtins::cd::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "cinfo" {
            let cr = builtins::cinfo::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "exec" {
            let cr = builtins::exec::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "exit" {
            let cr = builtins::exit::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "export" {
            let cr = builtins::export::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "fg" {
            let cr = builtins::fg::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "history" {
            let cr = builtins::history::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "jobs" {
            let cr = builtins::jobs::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "minfd" {
            let cr = builtins::minfd::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "read" {
            let cr = builtins::read::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "set" {
            let cr = builtins::set::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "source" {
            let cr = builtins::source::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "ulimit" {
            let cr = builtins::ulimit::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unalias" {
            let cr = builtins::unalias::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unset" {
            let cr = builtins::unset::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "unpath" {
            let cr = builtins::unpath::run(sh, cl, cmd, capture);
            return Some(cr);
        } else if cname == "vox" {
            let cr = builtins::vox::run(sh, cl, cmd, capture);
            return Some(cr);
        }
        None
    }

    /// Run a pipeline (e.g. `echo hi | wc -l`)
    /// returns: (is-terminal-given, command-result)
    pub fn run_pipeline(
        sh: &mut shell::Shell,
        cl: &CommandLine,
        tty: bool,
        capture: bool,
        log_cmd: bool,
    ) -> (bool, CommandResult) {
        let mut term_given = false;
        if cl.background && capture {
            println_stderr!(":: cannot capture output of background cmd");
            return (term_given, CommandResult::error());
        }

        if let Some(cr) = try_run_calculator(&cl.line, capture) {
            return (term_given, cr);
        }

        // FIXME: move func-run into run single command
        if let Some(cr) = try_run_func(sh, cl, capture, log_cmd) {
            return (term_given, cr);
        }

        if log_cmd {
            log!("run: {}", cl.line);
        }

        let length = cl.commands.len();
        if length == 0 {
            println!(":: invalid command: cmds with empty length");
            return (false, CommandResult::error());
        }

        let mut pipes = Vec::new();
        let mut errored_pipes = false;
        for _ in 0..length - 1 {
            match pipe() {
                Ok(fds) => pipes.push(fds),
                Err(e) => {
                    errored_pipes = true;
                    println_stderr!(":: pipeline1: {}", e);
                    break;
                }
            }
        }

        if errored_pipes {
            // release fds that already created when errors occurred
            for fds in pipes {
                libs::close(fds.0);
                libs::close(fds.1);
            }
            return (false, CommandResult::error());
        }

        if pipes.len() + 1 != length {
            println!(":: invalid command: unmatched pipes count");
            return (false, CommandResult::error());
        }

        let mut pgid: i32 = 0;
        let mut fg_pids: Vec<i32> = Vec::new();

        let isatty = if tty {
            unsafe { libc::isatty(1) == 1 }
        } else {
            false
        };
        let options = CommandOptions {
            isatty,
            capture_output: capture,
            background: cl.background,
            envs: cl.envs.clone(),
        };

        let mut fds_capture_stdout = None;
        let mut fds_capture_stderr = None;
        if capture {
            match pipe() {
                Ok(fds) => fds_capture_stdout = Some(fds),
                Err(e) => {
                    println_stderr!(":: pipeline2: {}", e);
                    return (false, CommandResult::error());
                }
            }
            match pipe() {
                Ok(fds) => fds_capture_stderr = Some(fds),
                Err(e) => {
                    if let Some(fds) = fds_capture_stdout {
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                    println_stderr!(":: pipeline3: {}", e);
                    return (false, CommandResult::error());
                }
            }
        }

        let mut cmd_result = CommandResult::new();
        for i in 0..length {
            let child_id: i32 = run_single_program(
                sh,
                cl,
                i,
                &options,
                &mut pgid,
                &mut term_given,
                &mut cmd_result,
                &pipes,
                &fds_capture_stdout,
                &fds_capture_stderr,
            );

            if child_id > 0 && !cl.background {
                fg_pids.push(child_id);
            }
        }

        if cl.is_single_and_builtin() {
            return (false, cmd_result);
        }

        if cl.background {
            if let Some(job) = sh.get_job_by_gid(pgid) {
                println_stderr!("[{}] {}", job.id, job.gid);
            }
        }

        if !fg_pids.is_empty() {
            let _cr = jobc::wait_fg_job(sh, pgid, &fg_pids);
            // for capture commands, e.g. `echo foo` in `echo "hello $(echo foo)"
            // the cmd_result is already built in loop calling run_single_program()
            // above.
            if !capture {
                cmd_result = _cr;
            }
        }
        (term_given, cmd_result)
    }

    /// Run a single command.
    /// e.g. the `sort -k2` part of `ps ax | sort -k2 | head`
    #[allow(clippy::needless_range_loop)]
    #[allow(clippy::too_many_arguments)]
    fn run_single_program(
        sh: &mut shell::Shell,
        cl: &CommandLine,
        idx_cmd: usize,
        options: &CommandOptions,
        pgid: &mut i32,
        term_given: &mut bool,
        cmd_result: &mut CommandResult,
        pipes: &[(RawFd, RawFd)],
        fds_capture_stdout: &Option<(RawFd, RawFd)>,
        fds_capture_stderr: &Option<(RawFd, RawFd)>,
    ) -> i32 {
        let capture = options.capture_output;
        if cl.is_single_and_builtin() {
            if let Some(cr) = try_run_builtin(sh, cl, idx_cmd, capture) {
                *cmd_result = cr;
                return unsafe { libc::getpid() };
            }

            println_stderr!(":: error when run singler builtin");
            log!("error when run singler builtin: {:?}", cl);
            return 1;
        }

        let pipes_count = pipes.len();
        let mut fds_stdin = None;
        let cmd = cl.commands.get(idx_cmd).unwrap();

        if cmd.has_here_string() {
            match pipe() {
                Ok(fds) => fds_stdin = Some(fds),
                Err(e) => {
                    println_stderr!(":: pipeline4: {}", e);
                    return 1;
                }
            }
        }

        match libs::fork::fork() {
            Ok(ForkResult::Child) => {
                unsafe {
                    // child processes need to handle ctrl-Z
                    libc::signal(libc::SIGTSTP, libc::SIG_DFL);
                    libc::signal(libc::SIGQUIT, libc::SIG_DFL);
                }

                // close pipes unrelated to current child (left side)
                if idx_cmd > 0 {
                    for i in 0..idx_cmd - 1 {
                        let fds = pipes[i];
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                }
                // close pipes unrelated to current child (right side)
                for i in idx_cmd + 1..pipes_count {
                    let fds = pipes[i];
                    libs::close(fds.0);
                    libs::close(fds.1);
                }
                // close pipe fds for capturing stdout/stderr
                // (they're only used in last child)
                if idx_cmd < pipes_count {
                    if let Some(fds) = fds_capture_stdout {
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                    if let Some(fds) = fds_capture_stderr {
                        libs::close(fds.0);
                        libs::close(fds.1);
                    }
                }

                if idx_cmd == 0 {
                    unsafe {
                        let pid = libc::getpid();
                        libc::setpgid(0, pid);
                    }
                } else {
                    unsafe {
                        libc::setpgid(0, *pgid);
                    }
                }

                // (in child) replace stdin/stdout with read/write ends of pipe
                if idx_cmd > 0 {
                    let fds_prev = pipes[idx_cmd - 1];
                    libs::dup2(fds_prev.0, 0);
                    libs::close(fds_prev.0);
                    libs::close(fds_prev.1);
                }
                if idx_cmd < pipes_count {
                    let fds = pipes[idx_cmd];
                    libs::dup2(fds.1, 1);
                    libs::close(fds.1);
                    libs::close(fds.0);
                }

                if cmd.has_redirect_from() {
                    if let Some(redirect_from) = &cmd.redirect_from {
                        let fd = tools::get_fd_from_file(&redirect_from.clone().1);
                        if fd == -1 {
                            process::exit(1);
                        }

                        libs::dup2(fd, 0);
                        libs::close(fd);
                    }
                }

                if cmd.has_here_string() {
                    if let Some(fds) = fds_stdin {
                        libs::close(fds.1);
                        libs::dup2(fds.0, 0);
                        libs::close(fds.0);
                    }
                }

                let mut stdout_redirected = false;
                let mut stderr_redirected = false;
                for item in &cmd.redirects_to {
                    let from_ = &item.0;
                    let op_ = &item.1;
                    let to_ = &item.2;
                    if to_ == "&1" && from_ == "2" {
                        if idx_cmd < pipes_count {
                            libs::dup2(1, 2);
                        } else if !options.capture_output {
                            let fd = libs::dup(1);
                            if fd == -1 {
                                println_stderr!(":: dup error");
                                process::exit(1);
                            }
                            libs::dup2(fd, 2);
                        } else {
                            // note: capture output with redirections does not
                            // make much sense
                        }
                    } else if to_ == "&2" && from_ == "1" {
                        if idx_cmd < pipes_count || !options.capture_output {
                            let fd = libs::dup(2);
                            if fd == -1 {
                                println_stderr!(":: dup error");
                                process::exit(1);
                            }
                            libs::dup2(fd, 1);
                        } else {
                            // note: capture output with redirections does not
                            // make much sense
                        }
                    } else {
                        let append = op_ == ">>";
                        match tools::create_raw_fd_from_file(to_, append) {
                            Ok(fd) => {
                                if fd == -1 {
                                    println_stderr!(":: fork: fd error");
                                    process::exit(1);
                                }

                                if from_ == "1" {
                                    libs::dup2(fd, 1);
                                    stdout_redirected = true;
                                } else {
                                    libs::dup2(fd, 2);
                                    stderr_redirected = true;
                                }
                            }
                            Err(e) => {
                                println_stderr!(":: fork: {}", e);
                                process::exit(1);
                            }
                        }
                    }
                }

                // capture output of last process if needed.
                if idx_cmd == pipes_count && options.capture_output {
                    if !stdout_redirected {
                        if let Some(fds) = fds_capture_stdout {
                            libs::close(fds.0);
                            libs::dup2(fds.1, 1);
                            libs::close(fds.1);
                        }
                    }
                    if !stderr_redirected {
                        if let Some(fds) = fds_capture_stderr {
                            libs::close(fds.0);
                            libs::dup2(fds.1, 2);
                            libs::close(fds.1);
                        }
                    }
                }

                if cmd.is_builtin() {
                    if let Some(status) = try_run_builtin_in_subprocess(sh, cl, idx_cmd, capture) {
                        process::exit(status);
                    }
                }

                // our strings do not have '\x00' bytes in them,
                // we can use CString::new().expect() safely.
                let mut c_envs: Vec<_> = env::vars()
                    .map(|(k, v)| {
                        CString::new(format!("{}={}", k, v).as_str()).expect("CString error")
                    })
                    .collect();
                for (key, value) in cl.envs.iter() {
                    c_envs.push(
                        CString::new(format!("{}={}", key, value).as_str()).expect("CString error"),
                    );
                }

                let program = &cmd.tokens[0].1;
                let path = if program.contains('/') {
                    program.clone()
                } else {
                    libs::path::find_file_in_path(program, true)
                };
                if path.is_empty() {
                    println_stderr!(":: {}: command not found", program);
                    process::exit(127);
                }

                let c_program = CString::new(path.as_str()).expect("CString::new failed");
                let c_args: Vec<_> = cmd
                    .tokens
                    .iter()
                    .map(|x| CString::new(x.1.as_str()).expect("CString error"))
                    .collect();

                let c_args: Vec<&CStr> = c_args.iter().map(|x| x.as_c_str()).collect();
                let c_envs: Vec<&CStr> = c_envs.iter().map(|x| x.as_c_str()).collect();
                match execve(&c_program, &c_args, &c_envs) {
                    Ok(_) => {}
                    Err(e) => match e {
                        nix::Error::ENOEXEC => {
                            println_stderr!(":: {}: exec format error (ENOEXEC)", program);
                        }
                        nix::Error::ENOENT => {
                            println_stderr!(":: {}: file does not exist", program);
                        }
                        nix::Error::EACCES => {
                            println_stderr!(":: {}: Permission denied", program);
                        }
                        _ => {
                            println_stderr!(":: {}: {:?}", program, e);
                        }
                    },
                }

                process::exit(1);
            }
            Ok(ForkResult::Parent { child, .. }) => {
                let pid: i32 = child.into();
                if idx_cmd == 0 {
                    *pgid = pid;
                    unsafe {
                        // we need to wait pgid of child set to itself,
                        // before give terminal to it (for macos).
                        // 1. this loop causes `bash`, `htop` etc to go `T` status
                        //    immediate after start on linux (ubuntu).
                        // 2. but on mac, we need this loop, otherwise commands
                        //    like `vim` will go to `T` status after start.
                        if cfg!(target_os = "macos") {
                            loop {
                                let _pgid = libc::getpgid(pid);
                                if _pgid == pid {
                                    break;
                                }
                            }
                        }

                        if sh.has_terminal && options.isatty && !cl.background {
                            *term_given = shell::give_terminal_to(pid);
                        }
                    }
                }

                if options.isatty && !options.capture_output {
                    let _cmd = parses::lines::tokens_to_line(&cmd.tokens);
                    sh.insert_job(*pgid, pid, &_cmd, "Running", cl.background);
                }

                if let Some(redirect_from) = &cmd.redirect_from {
                    if redirect_from.0 == "<<<" {
                        if let Some(fds) = fds_stdin {
                            unsafe {
                                libs::close(fds.0);

                                let mut f = File::from_raw_fd(fds.1);
                                match f.write_all(redirect_from.1.clone().as_bytes()) {
                                    Ok(_) => {}
                                    Err(e) => println_stderr!(":: write_all: {}", e),
                                }
                                match f.write_all(b"\n") {
                                    Ok(_) => {}
                                    Err(e) => println_stderr!(":: write_all: {}", e),
                                }
                            }
                        }
                    }
                }

                // (in parent) close unused pipe ends
                if idx_cmd < pipes_count {
                    let fds = pipes[idx_cmd];
                    libs::close(fds.1);
                }
                if idx_cmd > 0 {
                    // close pipe end only after dupped in the child
                    let fds = pipes[idx_cmd - 1];
                    libs::close(fds.0);
                }

                if idx_cmd == pipes_count && options.capture_output {
                    let mut s_out = String::new();
                    let mut s_err = String::new();

                    unsafe {
                        if let Some(fds) = fds_capture_stdout {
                            libs::close(fds.1);

                            let mut f = File::from_raw_fd(fds.0);
                            match f.read_to_string(&mut s_out) {
                                Ok(_) => {}
                                Err(e) => println_stderr!(":: readstr: {}", e),
                            }
                        }
                        if let Some(fds) = fds_capture_stderr {
                            libs::close(fds.1);
                            let mut f_err = File::from_raw_fd(fds.0);
                            match f_err.read_to_string(&mut s_err) {
                                Ok(_) => {}
                                Err(e) => println_stderr!(":: readstr: {}", e),
                            }
                        }
                    }

                    *cmd_result = CommandResult {
                        gid: *pgid,
                        status: 0,
                        stdout: s_out.clone(),
                        stderr: s_err.clone(),
                    };
                }

                pid
            }

            Err(_) => {
                println_stderr!("Fork failed");
                *cmd_result = CommandResult::error();
                0
            }
        }
    }

    fn try_run_func(
        sh: &mut Shell,
        cl: &CommandLine,
        capture: bool,
        log_cmd: bool,
    ) -> Option<CommandResult> {
        if cl.is_empty() {
            return None;
        }

        let command = &cl.commands[0];
        if let Some(func_body) = sh.get_func(&command.tokens[0].1) {
            let mut args = vec!["cicada".to_string()];
            for token in &command.tokens {
                args.push(token.1.to_string());
            }
            if log_cmd {
                log!("run func: {:?}", &args);
            }
            let cr_list = scripting::run_lines(sh, &func_body, &args, capture);
            let mut stdout = String::new();
            let mut stderr = String::new();
            for cr in cr_list {
                stdout.push_str(cr.stdout.trim());
                stdout.push(' ');
                stderr.push_str(cr.stderr.trim());
                stderr.push(' ');
            }
            let mut cr = CommandResult::new();
            cr.stdout = stdout;
            cr.stderr = stderr;
            return Some(cr);
        }
        None
    }

    fn try_run_calculator(line: &str, capture: bool) -> Option<CommandResult> {
        if tools::is_arithmetic(line) {
            match run_calculator(line) {
                Ok(result) => {
                    let mut cr = CommandResult::new();
                    if capture {
                        cr.stdout = result.clone();
                    } else {
                        println!("{}", result);
                    }
                    return Some(cr);
                }
                Err(e) => {
                    let mut cr = CommandResult::from_status(0, 1);
                    if capture {
                        cr.stderr = e.to_string();
                    } else {
                        println_stderr!(":: calculator: {}", e);
                    }
                    return Some(cr);
                }
            }
        }
        None
    }

    pub fn run_calculator(line: &str) -> Result<String, &str> {
        let parse_result = calculator::calculate(line);
        match parse_result {
            Ok(mut calc) => {
                let expr = calc.next().unwrap().into_inner();

                if line.contains('.') {
                    Ok(format!("{}", calculator::eval_float(expr)))
                } else {
                    Ok(format!("{}", calculator::eval_int(expr)))
                }
            }
            Err(_) => Err("syntax error"),
        }
    }
}

pub mod ctime
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::fmt;
    use time::OffsetDateTime;
    */
    #[derive(Debug, PartialEq, Eq)]
    pub struct DateTime {
        odt: OffsetDateTime,
    }

    impl DateTime {
        pub fn now() -> Self {
            let odt: OffsetDateTime = match OffsetDateTime::now_local() {
                Ok(dt) => dt,
                Err(_) => OffsetDateTime::now_utc(),
            };
            DateTime { odt }
        }

        pub fn from_timestamp(ts: f64) -> Self {
            let dummy_now = Self::now();
            let offset_seconds = dummy_now.odt.offset().whole_minutes() * 60;
            let ts_nano = (ts + offset_seconds as f64) * 1000000000.0;
            let odt: OffsetDateTime = match OffsetDateTime::from_unix_timestamp_nanos(ts_nano as i128)
            {
                Ok(x) => x,
                Err(_) => OffsetDateTime::now_utc(),
            };
            DateTime { odt }
        }

        pub fn unix_timestamp(&self) -> f64 {
            self.odt.unix_timestamp_nanos() as f64 / 1000000000.0
        }
    }

    impl fmt::Display for DateTime {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:03}",
                self.odt.year(),
                self.odt.month() as u8,
                self.odt.day(),
                self.odt.hour(),
                self.odt.minute(),
                self.odt.second(),
                self.odt.millisecond(),
            )
        }
    }
}

pub mod env
{
    pub use std::env::{ * };
    // pub fn env_args_to_command_line() -> String
    pub fn args_to_command_line() -> String
    {
        let mut result = String::new();
        let env_args = env::args();
        if env_args.len() <= 1 {
            return result;
        }
        for (i, arg) in env_args.enumerate() {
            if i == 0 || arg == "-c" {
                continue;
            }
            result.push_str(arg.as_str());
        }
        result
    }
    // pub fn init_path_env()
    pub fn initialize_paths()
    {
        let mut all_paths: HashSet<PathBuf> = HashSet::new();
        for x in [
            "/usr/local/sbin",
            "/usr/local/bin",
            "/usr/sbin",
            "/usr/bin",
            "/sbin",
            "/bin",
        ] {
            let path_buf = PathBuf::from(x);
            if path_buf.exists() {
                all_paths.insert(path_buf);
            }
        }

        if let Ok(env_path) = env::var("PATH") {
            for one_path in env::split_paths(&env_path) {
                if !all_paths.contains(&one_path) {
                    all_paths.insert(one_path);
                }
            }
        }
        let path_var = env::join_paths(all_paths).unwrap_or_default();
        env::set_var("PATH", path_var);
    }
}

pub mod error
{
    pub use std::error::{ * };

    pub mod no
    {
        /*!
        Cross-platform interface to the `errno` variable. */
        use ::
        {
            error::{ Error },
            *,
        };
        /*
        */
        pub mod sys
        {
            /*!
            */
            use ::
            {
                *,
            };
            /*
            */
            pub mod unix
            {
                /*!
                Implementation of `errno` functionality for Unix systems. */
                use ::
                {
                    error::no::{ Errno },                    *,
                };
                /*
                */
                fn from_utf8_lossy(input:&[u8] ) -> &str
                {
                    match str::from_utf8(input)
                    {
                        Ok(valid ) => valid,
                        Err(error ) => unsafe { str::from_utf8_unchecked( &input[..error.valid_up_to()]) },
                    }
                }

                pub fn with_description<F, T>(err: Errno, callback: F ) -> T where
                F:FnOnce( ::result::Result<&str, Errno> ) -> T,
                {
                    unsafe
                    {
                        let mut buf = [0u8; 1024];
                        let c_str =
                        {
                            let rc = strerror_r(err.0, buf.as_mut_ptr() as *mut _, buf.len() as size_t);
                            
                            if rc != 0
                            {
                                let fm_err = match rc < 0
                                {
                                    true => errno(),
                                    false => Errno( rc ),
                                };

                                if fm_err != Errno( ERANGE) { return callback( Err( fm_err ) ); }
                            }

                            let c_str_len = strlen(buf.as_ptr() as *const _ );
                            &buf[..c_str_len]
                        };

                        callback(Ok( from_utf8_lossy(c_str  ) ) )
                        
                    }
                }

                pub const STRERROR_NAME: &str = "strerror_r";

                pub fn errno() -> Errno { unsafe { Errno(*errno_location() ) } }

                pub fn set_errno(Errno(errno): Errno) { unsafe { *errno_location() = errno; } }

                extern "C" 
                {
                    #[cfg_attr(
                        any(
                            target_os = "macos",
                            target_os = "ios",
                            target_os = "tvos",
                            target_os = "watchos",
                            target_os = "visionos",
                            target_os = "freebsd"
                        ),
                        link_name = "__error"
                    )]
                    #[cfg_attr(
                        any(
                            target_os = "openbsd",
                            target_os = "netbsd",
                            target_os = "android",
                            target_os = "espidf",
                            target_os = "vxworks",
                            target_os = "cygwin",
                            target_env = "newlib"
                        ),
                        link_name = "__errno"
                    )]
                    #[cfg_attr(
                        any(target_os = "solaris", target_os = "illumos" ),
                        link_name = "___errno"
                    )]
                    #[cfg_attr(target_os = "haiku", link_name = "_errnop" )]
                    #[cfg_attr(
                        any(
                            target_os = "linux",
                            target_os = "hurd",
                            target_os = "redox",
                            target_os = "dragonfly",
                            target_os = "emscripten",
                        ),
                        link_name = "__errno_location"
                    )]
                    #[cfg_attr(target_os = "aix", link_name = "_Errno" )]
                    #[cfg_attr(target_os = "nto", link_name = "__get_errno_ptr" )]
                    fn errno_location() -> *mut c_int;
                }
            }
            /*
            */
            pub mod windows
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
            
            #[cfg( unix )] pub use self::unix::{ * };
            #[cfg( windows )] pub use self::windows::{ * };
        }
        
        #[derive( Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash )]
        pub struct Errno( pub i32);

        impl fmt::Debug for Errno
        {
            fn fmt( &self, fmt: &mut fmt::Formatter ) -> fmt::Result
            {
                sys::with_description(*self, | desc |
                {
                    fmt.debug_struct( "Errno" )
                    .field( "code", &self.0)
                    .field( "description", &desc.ok() )
                    .finish()
                })
            }
        }

        impl fmt::Display for Errno
        {
            fn fmt( &self, fmt: &mut fmt::Formatter ) -> fmt::Result
            {
                sys::with_description(*self, |desc| match desc
                {
                    Ok(desc ) => fmt.write_str(desc ),
                    Err( fm_err ) => write!
                    (
                        fmt,
                        "OS error {} ({} returned error {})",
                        self.0,
                        sys::STRERROR_NAME,
                        fm_err.0
                    ),
                })
            }
        }

        impl From<Errno> for i32
        {
            fn from(e: Errno ) -> Self { e.0 }
        }
        
        impl Error for Errno 
        {
            fn description( &self ) -> &str 
            {
                "system error"
            }
        }
        
        impl From<Errno> for io::Error 
        {
            fn from(errno: Errno ) -> Self { io::Error::from_raw_os_error(errno.0) }
        }

        pub fn errno() -> Errno { sys::errno() }

        pub fn set_errno(err: Errno) { sys::set_errno( err ) }
    }
}

pub mod execute
{
    /*!
    A simple wrapper around the C library's `execvp` function. */
    use ::
    {
        collections::{ HashMap },
        error::{ Error as ErrorTrait, no::{ Errno, errno }, },
        ffi::{ CString, NulError, OsStr, OsString },
        io::{ self, Read, Write },
        shell::{ self, Shell },
        types::{ * },
        *,
    };
    /*
    use std::iter::{IntoIterator, Iterator};
    use std::fmt;
    use std::ptr;
    use std::os::unix::ffi::OsStrExt;
    */
    #[must_use] #[derive( Debug )]
    pub enum Error
    {
        BadArgument(NulError),
        Errno(Errno),
    }

    impl error::Error for Error
    {
        fn description(&self) -> &str
        {
            match self
            {
                &Error::BadArgument(_) => "bad argument to exec",
                &Error::Errno(_) => "couldn't exec process",
            }
        }

        fn cause(&self) -> Option<&error::Error>
        {
            match self
            {
                &Error::BadArgument(ref err) => Some(err),
                &Error::Errno(_) => None,
            }
        }
    }

    impl fmt::Display for Error
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
        {
            match self
            {
                &Error::BadArgument(ref err) => write!(f, "{}: {}", self.description(), err),
                &Error::Errno(err) => write!(f, "{}: {}", self.description(), err),
            }
        }
    }

    impl From<NulError> for Error
    {
        fn from(err: NulError) -> Error { Error::BadArgument(err) }
    }
    
    macro_rules! exec_try
    {
        ( $ expr : expr ) =>
        {
            match $expr
            {
                Ok(val) => val,
                Err(err) => return From::from(err),
            }
        };
    }
    
    pub fn execvp<S, I>(program: S, args: I) -> Error where
    S: AsRef<OsStr>, I: IntoIterator,
    I::Item: AsRef<OsStr>
    {
        unsafe
        {
            let program_cstring = exec_try!(CString::new(program.as_ref().as_bytes()));
            let arg_cstrings = exec_try!(args.into_iter().map(|arg| { CString::new(arg.as_ref().as_bytes()) }).collect::<Result<Vec<_>, _>>());            
            let mut arg_charptrs: Vec<_> = arg_cstrings.iter().map(|arg| { arg.as_ptr() }).collect();
            arg_charptrs.push(ptr::null());            
            let res = libc::execvp(program_cstring.as_ptr(), arg_charptrs.as_ptr());
            
            if res < 0 { Error::Errno(errno()) } else {  panic!("execvp returned unexpectedly") }
        }
    }
    
    pub struct Command
    {
        argv: Vec<OsString>,
    }

    impl Command
    {
        pub fn new<S: AsRef<OsStr>>(program: S) -> Command
        {
            Command
            {
                argv: vec!(program.as_ref().to_owned()),
            }
        }
        
        pub fn arg<S: AsRef<OsStr>>(&mut self, arg: S) -> &mut Command
        {
            self.argv.push(arg.as_ref().to_owned());
            self
        }
        
        pub fn args<S: AsRef<OsStr>>(&mut self, args: &[S]) -> &mut Command
        {
            for arg in args
            {
                self.arg(arg.as_ref());
            }

            self
        }
        
        pub fn exec(&mut self) -> Error { execvp(&self.argv[0], &self.argv) }
    }
    
    pub fn run_procs_for_non_tty(sh: &mut Shell)
    {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        match handle.read_to_string(&mut buffer) {
            Ok(_) => {
                log!("run non tty command: {}", &buffer);
                run_command_line(sh, &buffer, false, false);
            }
            Err(e) => {
                println!(":: stdin.read_to_string() failed: {:?}", e);
            }
        }
    }

    pub fn run_command_line
    (
        sh: &mut Shell,
        line: &str,
        tty: bool,
        capture: bool,
    ) -> Vec<CommandResult> 
    {
        let mut cr_list = Vec::new();
        let mut status = 0;
        let mut sep = String::new();
        for token in parses::lines::line_to_cmds(line) {
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

    pub fn line_to_tokens(sh: &mut Shell, line: &str) -> (Tokens, HashMap<String, String>)
    {
        let linfo = parses::lines::parse_line(line);
        let mut tokens = linfo.tokens;
        shell::do_expansion(sh, &mut tokens);
        let envs = drain_env_tokens(&mut tokens);
        (tokens, envs)
    }

    pub fn set_shell_vars(sh: &mut Shell, envs: &HashMap<String, String>)
    {
        for (name, value) in envs.iter() {
            sh.set_env(name, value);
        }
    }
    
    pub fn run_proc(sh: &mut Shell, line: &str, tty: bool, capture: bool) -> CommandResult
    {
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
                println_stderr!(":: {}", e);
                CommandResult::from_status(0, 1)
            }
        }
    }

    pub fn run_with_shell(sh: &mut Shell, line: &str) -> CommandResult
    {
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
                println_stderr!(":: {}", e);
                CommandResult::from_status(0, 1)
            }
        }
    }

    pub fn run(line: &str) -> CommandResult
    {
        let mut sh = Shell::new();
        run_with_shell(&mut sh, line)
    }
}

pub mod extend
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    // pub fn extend_bangbang(sh: &shell::Shell, line: &mut String)
    pub fn bangbang(sh: &shell::Shell, line: &mut String)
    {
        if !re_contains(line, r"!!") {
            return;
        }
        if sh.previous_cmd.is_empty() {
            return;
        }

        let re = Regex::new(r"!!").unwrap();
        let mut replaced = false;
        let mut new_line = String::new();
        let linfo = parses::lines::parse_line(line);
        for (sep, token) in linfo.tokens {
            if !sep.is_empty() {
                new_line.push_str(&sep);
            }

            if re_contains(&token, r"!!") && sep != "'" {
                let line2 = token.clone();
                let result = re.replace_all(&line2, sh.previous_cmd.as_str());
                new_line.push_str(&result);
                replaced = true;
            } else {
                new_line.push_str(&token);
            }

            if !sep.is_empty() {
                new_line.push_str(&sep);
            }
            new_line.push(' ');
        }

        *line = new_line.trim_end().to_string();
        // print full line after extending
        if replaced {
            println!("{}", line);
        }
    }
}

pub mod ffi
{
    pub use std::ffi::{ * };
}

pub mod fs
{
    pub use std::fs::{ * };
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
    extern "C"
    {
        fn gethostname(name: *mut libc::c_char, size: libc::size_t) -> libc::c_int;
    }
    // pub fn get_user_name() -> String
    pub fn username() -> String
    {
        match env::var("USER") {
            Ok(x) => {
                return x;
            }
            Err(e) => {
                log!(":: env USER error: {}", e);
            }
        }
        let cmd_result = execute::run("whoami");
        cmd_result.stdout.trim().to_string()
    }
    // pub fn get_user_home() -> String
    pub fn user_home() -> String
    {
        match env::var("HOME") {
            Ok(x) => x,
            Err(e) => {
                println_stderr!(":: env HOME error: {}", e);
                String::new()
            }
        }
    }
    // pub fn get_config_dir() -> String
    pub fn config_dir() -> String
    {
        if let Ok(x) = env::var("XDG_CONFIG_HOME")
        {
            format!("{}/cicada", x)
        }
        
        else
        {
            let home = get::user_home();
            format!("{}/.config/cicada", home)
        }
    }
    // pub fn get_user_completer_dir() -> String
    pub fn user_completer_dir() -> String
    {
        let dir_config = config_dir();
        format!("{}/completers", dir_config)
    }
    // pub fn get_hostname() -> String
    pub fn hostname() -> String
    {
        let len = 255;
        let mut buf = Vec::<u8>::with_capacity(len);

        let ptr = buf.as_mut_slice().as_mut_ptr();

        let err = unsafe { gethostname(ptr as *mut libc::c_char, len as libc::size_t) } as i32;

        match err {
            0 => {
                let real_len;
                let mut i = 0;
                loop {
                    let byte = unsafe { *(((ptr as u64) + (i as u64)) as *const u8) };
                    if byte == 0 {
                        real_len = i;
                        break;
                    }

                    i += 1;
                }
                unsafe { buf.set_len(real_len) }
                String::from_utf8_lossy(buf.as_slice()).into_owned()
            }
            _ => String::from("unknown"),
        }
    }
    // pub fn get_fd_from_file(file_name: &str) -> i32
    pub fn fd_from_file(file_name: &str) -> i32
    {
        let path = Path::new(file_name);
        let display = path.display();
        let file = match File::open(path) {
            Err(why) => {
                println_stderr!(":: {}: {}", display, why);
                return -1;
            }
            Ok(file) => file,
        };
        file.into_raw_fd()
    }
    // pub fn get_current_dir() -> String
    pub fn current_dir() -> String
    {
        let mut current_dir = PathBuf::new();
        match env::current_dir() {
            Ok(x) => current_dir = x,
            Err(e) => {
                println_stderr!("env current_dir() failed: {}", e);
            }
        }
        let mut str_current_dir = "";
        match current_dir.to_str() {
            Some(x) => str_current_dir = x,
            None => {
                println_stderr!("current_dir to str failed.");
            }
        }
        str_current_dir.to_string()
    }
}

pub mod highlight
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::collections::HashSet;
    use std::env;
    use std::fs;
    use std::ops::Range;
    use std::os::unix::fs::PermissionsExt;
    use std::sync::Arc;
    use std::sync::Mutex;

    use lineread::highlighting::{Highlighter, Style};

    use crate::parses::lines;
    use crate::shell;
    use crate::tools;
    */
    #[derive(Clone)] pub struct CicadaHighlighter;

    /// ANSI color codes wrapped with `\x1b` (ESC) and `[0;32m` (green text)
    const GREEN: &str = "\x1b[0;32m";

    lazy_static! {
        pub static ref AVAILABLE_COMMANDS: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
        pub static ref ALIASES: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
    }

    /// Initialize the available commands cache by scanning PATH directories
    pub fn init_command_cache() {
        let commands = scan_available_commands();
        if let Ok(mut cache) = AVAILABLE_COMMANDS.lock() {
            *cache = commands;
        }
    }

    /// Update aliases in the highlighter's cache
    pub fn update_aliases(sh: &shell::Shell) {
        if let Ok(mut aliases) = ALIASES.lock() {
            aliases.clear();
            for alias_name in sh.aliases.keys() {
                aliases.insert(alias_name.clone());
            }
        }
    }

    fn scan_available_commands() -> HashSet<String> {
        let mut commands = HashSet::new();

        if let Ok(path_var) = env::var("PATH") {
            for dir_path in env::split_paths(&path_var) {
                if !dir_path.is_dir() {
                    continue;
                }

                if let Ok(entries) = fs::read_dir(dir_path) {
                    for entry in entries.filter_map(Result::ok) {
                        if let Ok(file_type) = entry.file_type() {
                            if file_type.is_file() || file_type.is_symlink() {
                                if let Ok(metadata) = entry.metadata() {
                                    // Check if file is executable
                                    if metadata.permissions().mode() & 0o111 != 0 {
                                        if let Some(name) = entry.file_name().to_str() {
                                            commands.insert(name.to_string());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        commands
    }

    fn find_token_range_heuristic(
        line: &str,
        start_byte: usize,
        token: &(String, String),
    ) -> Option<Range<usize>> {
        let (sep, word) = token;

        // Find the start of the token, skipping leading whitespace from the search start position
        let mut search_area = &line[start_byte..];
        let token_start_byte =
            if let Some(non_ws_offset) = search_area.find(|c: char| !c.is_whitespace()) {
                // Calculate the actual byte index of the first non-whitespace character
                start_byte
                    + search_area
                        .char_indices()
                        .nth(non_ws_offset)
                        .map_or(0, |(idx, _)| idx)
            } else {
                return None; // Only whitespace left
            };

        search_area = &line[token_start_byte..];

        // Estimate the end byte based on the token structure
        let mut estimated_len = 0;
        let mut current_search_offset = 0;

        // Match separator prefix if needed (e.g., `"` or `'`)
        if !sep.is_empty() && search_area.starts_with(sep) {
            estimated_len += sep.len();
            current_search_offset += sep.len();
        }

        // Match the word content
        // Use starts_with for a basic check, assuming the word appears next
        if search_area[current_search_offset..].starts_with(word) {
            estimated_len += word.len();
            current_search_offset += word.len();

            // Match separator suffix if needed
            if !sep.is_empty() && search_area[current_search_offset..].starts_with(sep) {
                estimated_len += sep.len();
            }

            Some(token_start_byte..(token_start_byte + estimated_len))
        } else if word.is_empty()
            && !sep.is_empty()
            && search_area.starts_with(sep)
            && search_area[sep.len()..].starts_with(sep)
        {
            // Handle empty quoted string like "" or ''
            estimated_len += sep.len() * 2;
            Some(token_start_byte..(token_start_byte + estimated_len))
        } else {
            // Fallback: Maybe it's just the word without quotes, or a separator like `|`
            if search_area.starts_with(word) {
                Some(token_start_byte..(token_start_byte + word.len()))
            } else {
                // Could not reliably map the token back to the original string segment
                // This might happen with complex escapes or parser ambiguities
                // As a basic fallback, consume up to the next space or end of line? Unsafe.
                // Return None to signal failure for this token.
                None
            }
        }
    }

    impl Highlighter for CicadaHighlighter
    {
        fn highlight(&self, line: &str) -> Vec<(Range<usize>, Style)>
        {
            let mut styles = Vec::new();
            if line.is_empty() {
                return styles;
            }

            let line_info = parser_line::parse_line(line);
            if line_info.tokens.is_empty() {
                // If parser returns no tokens, style whole line as default
                styles.push((0..line.len(), Style::Default));
                return styles;
            }

            let mut current_byte_idx = 0;
            let mut is_start_of_segment = true;

            for token in &line_info.tokens
            {
                match find_token_range_heuristic(line, current_byte_idx, token)
                {
                    Some(token_range) =>
                    {
                        if token_range.start > current_byte_idx {
                            styles.push((current_byte_idx..token_range.start, Style::Default));
                        }

                        let (_sep, word) = token;
                        let mut current_token_style = Style::Default;

                        if is_start_of_segment && !word.is_empty()
                        {
                            if is::command(word) {
                                current_token_style = Style::AnsiColor(GREEN.to_string());
                            }
                            
                            is_start_of_segment = false;
                        }

                        styles.push((token_range.clone(), current_token_style));

                        // Check if this token marks the end of a command segment
                        if ["|", "&&", "||", ";"].contains(&word.as_str()) {
                            is_start_of_segment = true;
                        }

                        current_byte_idx = token_range.end;
                    }

                    None => {
                        // If we can't map a token, style the rest of the line as default and stop.
                        if current_byte_idx < line.len() {
                            styles.push((current_byte_idx..line.len(), Style::Default));
                        }
                        current_byte_idx = line.len(); // Mark as done
                        break; // Stop processing further tokens
                    }
                }
            }
            
            if current_byte_idx < line.len() {
                styles.push((current_byte_idx..line.len(), Style::Default));
            }

            styles
        }
    }

    pub fn create_highlighter() -> Arc<CicadaHighlighter> {
        Arc::new(CicadaHighlighter)
    }
}

pub mod history
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::collections::HashMap;
    use std::env;
    use std::fs;
    use std::io::Write;
    use std::path::Path;

    use lineread::terminal::DefaultTerminal;
    use lineread::Interface;
    use rusqlite::Connection as Conn;
    use rusqlite::Error::SqliteFailure;

    use crate::shell;
    use crate::tools;
    */
    fn init_db(hfile: &str, htable: &str) 
    {
        let path = Path::new(hfile);
        if !path.exists() {
            let _parent = match path.parent() {
                Some(x) => x,
                None => {
                    println_stderr!(":: history init - no parent found");
                    return;
                }
            };
            let parent = match _parent.to_str() {
                Some(x) => x,
                None => {
                    println_stderr!(":: parent to_str is None");
                    return;
                }
            };
            match fs::create_dir_all(parent) {
                Ok(_) => {}
                Err(e) => {
                    println_stderr!(":: histdir create error: {}", e);
                    return;
                }
            }
            match fs::File::create(hfile) {
                Ok(_) => {
                    println!(":: created history file: {}", hfile);
                }
                Err(e) => {
                    println_stderr!(":: history: file create failed: {}", e);
                }
            }
        }

        let conn = match Conn::open(hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!(":: history: open db error: {}", e);
                return;
            }
        };
        let sql = format!(
            "
            CREATE TABLE IF NOT EXISTS {}
                (inp TEXT,
                rtn INTEGER,
                tsb REAL,
                tse REAL,
                sessionid TEXT,
                out TEXT,
                info TEXT
                );
        ",
            htable
        );
        match conn.execute(&sql, []) {
            Ok(_) => {}
            Err(e) => println_stderr!(":: history: query error: {}", e),
        }
    }

    pub fn init(rl: &mut Interface<DefaultTerminal>)
    {
        let mut hist_size: usize = 99999;
        if let Ok(x) = env::var("HISTORY_SIZE") {
            if let Ok(y) = x.parse::<usize>() {
                hist_size = y;
            }
        }
        rl.set_history_size(hist_size);

        let history_table = get_history_table();
        let hfile = get_history_file();

        if !Path::new(&hfile).exists() {
            init_db(&hfile, &history_table);
        }

        let mut delete_dups = true;
        if let Ok(x) = env::var("HISTORY_DELETE_DUPS") {
            if x == "0" {
                delete_dups = false;
            }
        }
        if delete_dups {
            delete_duplicated_histories();
        }

        let conn = match Conn::open(&hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!(":: history: conn error: {}", e);
                return;
            }
        };
        let sql = format!("SELECT inp FROM {} ORDER BY tsb;", history_table);
        let mut stmt = match conn.prepare(&sql) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!(":: prepare select error: {}", e);
                return;
            }
        };

        let rows = match stmt.query_map([], |row| row.get(0)) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!(":: query select error: {}", e);
                return;
            }
        };

        let mut dict_helper: HashMap<String, bool> = HashMap::new();
        for x in rows.flatten() {
            let inp: String = x;
            if dict_helper.contains_key(&inp) {
                continue;
            }
            dict_helper.insert(inp.clone(), true);
            rl.add_history(inp.trim().to_string());
        }
    }

    pub fn get_history_file() -> String 
    {
        if let Ok(hfile) = env::var("HISTORY_FILE") {
            hfile
        } else if let Ok(d) = env::var("XDG_DATA_HOME") {
            format!("{}/{}", d, "cicada/history.sqlite")
        } else {
            let home = tools::get_user_home();
            format!("{}/{}", home, ".local/share/cicada/history.sqlite")
        }
    }

    pub fn get_history_table() -> String 
    {
        if let Ok(hfile) = env::var("HISTORY_TABLE") {
            hfile
        } else {
            String::from("cicada_history")
        }
    }

    fn delete_duplicated_histories() 
    {
        let hfile = get_history_file();
        let history_table = get_history_table();
        let conn = match Conn::open(&hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!(":: history: conn error: {}", e);
                return;
            }
        };
        let sql = format!(
            "DELETE FROM {} WHERE rowid NOT IN (
            SELECT MAX(rowid) FROM {} GROUP BY inp)",
            history_table, history_table
        );
        match conn.execute(&sql, []) {
            Ok(_) => {}
            Err(e) => match e {
                SqliteFailure(ee, msg) => {
                    if ee.extended_code == 5 {
                        log!(
                            "failed to delete dup histories: {}",
                            msg.unwrap_or("db is locked?".to_owned()),
                        );
                        return;
                    }
                    println_stderr!(":: history: delete dups error: {}: {:?}", &ee, &msg);
                }
                _ => {
                    println_stderr!(":: history: delete dup error: {}", e);
                }
            },
        }
    }

    pub fn add_raw(sh: &shell::Shell, line: &str, status: i32, tsb: f64, tse: f64) 
    {
        let hfile = get_history_file();
        let history_table = get_history_table();
        if !Path::new(&hfile).exists() {
            init_db(&hfile, &history_table);
        }

        let conn = match Conn::open(&hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!(":: history: conn error: {}", e);
                return;
            }
        };
        let sql = format!(
            "INSERT INTO \
            {} (inp, rtn, tsb, tse, sessionid, info) \
            VALUES('{}', {}, {}, {}, '{}', 'dir:{}|');",
            history_table,
            str::replace(line.trim(), "'", "''"),
            status,
            tsb,
            tse,
            sh.session_id,
            sh.current_dir,
        );
        match conn.execute(&sql, []) {
            Ok(_) => {}
            Err(e) => println_stderr!(":: history: save error: {}", e),
        }
    }

    pub fn add
    (
        sh: &shell::Shell,
        rl: &mut Interface<DefaultTerminal>,
        line: &str,
        status: i32,
        tsb: f64,
        tse: f64,
    )
    {
        add_raw(sh, line, status, tsb, tse);
        rl.add_history(line.to_string());
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
    // pub fn is_env(line: &str) -> bool
    pub fn env(line: &str) -> bool
    {
        regex::contains(line, r"^[a-zA-Z_][a-zA-Z0-9_]*=.*$")
    }
    // pub fn is_arithmetic(line: &str) -> bool
    pub fn arithmetic(line: &str) -> bool
    {
        if !re_contains(line, r"[0-9]+") {
            return false;
        }
        if !re_contains(line, r"\+|\-|\*|/|\^") {
            return false;
        }
        re_contains(line, r"^[ 0-9\.\(\)\+\-\*/\^]+[\.0-9 \)]$")
    }
    // pub fn is_shell_altering_command(line: &str) -> bool
    pub fn shell_altering_command(line: &str) -> bool
    {
        let line = line.trim();

        if re_contains(line, r"^[A-Za-z_][A-Za-z0-9_]*=.*$") { return true; }

        line.starts_with("alias ")
        || line.starts_with("export ")
        || line.starts_with("unalias ")
        || line.starts_with("unset ")
        || line.starts_with("source ")
    }
    // pub fn is_builtin(s: &str) -> bool
    pub fn builtin(s: &str) -> bool
    {
        let builtins =
        [
            "alias", "bg", "cd", "cinfo", "exec", "exit", "export", "fg", "history", "jobs", "read",
            "source", "ulimit", "unalias", "vox", "minfd", "set", "unset", "unpath",
        ];
        builtins.contains(&s)
    }
    // fn is_command(word: &str) -> bool
    pub fn command(word: &str) -> bool
    {
        if builtin(word) {
            return true;
        }
        if let Ok(aliases) = ALIASES.lock() {
            if aliases.contains(word) {
                return true;
            }
        }
        if let Ok(commands) = AVAILABLE_COMMANDS.lock() {
            if commands.contains(word) {
                return true;
            }
        }
        false
    }
}

pub mod iter
{
    pub use std::iter::{ * };
    use ::
    {
        *,
    };
    /*
    use alloc::format;
    use alloc::rc::Rc;
    #[cfg(feature = "pretty-print")]
    use alloc::string::String;
    use alloc::vec::Vec;
    use core::borrow::Borrow;
    use core::fmt;
    use core::hash::{Hash, Hasher};
    use core::ptr;
    use core::str;

    #[cfg(feature = "pretty-print")]
    use serde::ser::SerializeStruct;

    use super::line_index::LineIndex;
    use super::pairs::{self, Pairs};
    use super::queueable_token::QueueableToken;
    use super::tokens::{self, Tokens};
    use crate::span::Span;
    use crate::RuleType;

    use alloc::format;
    use alloc::rc::Rc;
    use alloc::string::String;
    use alloc::vec::Vec;
    use core::fmt;
    use core::hash::{Hash, Hasher};
    use core::iter::Filter;
    use core::ptr;
    use core::str;

    #[cfg(feature = "pretty-print")]
    use serde::ser::SerializeStruct;

    use super::flat_pairs::{self, FlatPairs};
    use super::line_index::LineIndex;
    use super::pair::{self, Pair};
    use super::queueable_token::QueueableToken;
    use super::tokens::{self, Tokens};
    use crate::RuleType;
    */
    #[derive(Clone)] pub struct Pair<'i, R> 
    {
        queue: Rc<Vec<QueueableToken<'i, R>>>,
        input: &'i str,
        start: usize,
        line_index: Rc<LineIndex>,
    }

    pub fn create_pair<'i, R: RuleType>
    (
        queue: Rc<Vec<QueueableToken<'i, R>>>,
        input: &'i str,
        line_index: Rc<LineIndex>,
        start: usize,
    ) -> Pair<'i, R> 
    {
        Pair 
        {
            queue,
            input,
            start,
            line_index,
        }
    }

    impl<'i, R: RuleType> Pair<'i, R> 
    {
        #[inline] pub fn as_rule(&self) -> R 
        {
            match self.queue[self.pair()] 
            {
                QueueableToken::End { rule, .. } => rule,
                _ => unreachable!(),
            }
        }
        
        #[inline] pub fn as_str(&self) -> &'i str 
        {
            let start = self.pos(self.start);
            let end = self.pos(self.pair());
            &self.input[start..end]
        }
        
        pub fn get_input(&self) -> &'i str { self.input }
        
        #[inline] #[deprecated(since = "2.0.0", note = "Please use `as_span` instead")]
        pub fn into_span(self) -> Span<'i> 
        {
            self.as_span()
        }
        
        #[inline] pub fn as_span(&self) -> Span<'i> 
        {
            let start = self.pos(self.start);
            let end = self.pos(self.pair());

            Span::new_internal(self.input, start, end)
        }
        
        #[inline] pub fn as_node_tag(&self) -> Option<&str> 
        {
            match &self.queue[self.pair()] {
                QueueableToken::End { tag, .. } => tag.as_ref().map(|x| x.borrow()),
                _ => None,
            }
        }
        
        #[inline] pub fn into_inner(self) -> Pairs<'i, R> 
        {
            let pair = self.pair();

            pairs::new(
                self.queue,
                self.input,
                Some(self.line_index),
                self.start + 1,
                pair,
            )
        }
        
        #[inline] pub fn tokens(self) -> Tokens<'i, R> 
        {
            let end = self.pair();

            tokens::new(self.queue, self.input, self.start, end + 1)
        }
        
        
        pub fn line_col(&self) -> (usize, usize) 
        {
            let pos = self.pos(self.start);
            self.line_index.line_col(self.input, pos)
        }

        fn pair(&self) -> usize 
        {
            match self.queue[self.start] {
                QueueableToken::Start {
                    end_token_index, ..
                } => end_token_index,
                _ => unreachable!(),
            }
        }

        fn pos(&self, index: usize) -> usize 
        {
            match self.queue[index] 
            {
                QueueableToken::Start { input_pos, .. } | QueueableToken::End { input_pos, .. } => {
                    input_pos
                }
            }
        }
    }

    impl<'i, R: RuleType> Pairs<'i, R> 
    {
        pub fn single(pair: Pair<'i, R>) -> Self {
            let end = pair.pair();
            pairs::new(
                pair.queue,
                pair.input,
                Some(pair.line_index),
                pair.start,
                end,
            )
        }
    }

    impl<R: RuleType> fmt::Debug for Pair<'_, R> 
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let pair = &mut f.debug_struct("Pair");
            pair.field("rule", &self.as_rule());
            if let Some(s) = self.as_node_tag() {
                pair.field("node_tag", &s);
            }
            pair.field("span", &self.as_span())
                .field("inner", &self.clone().into_inner().collect::<Vec<_>>())
                .finish()
        }
    }

    impl<R: RuleType> fmt::Display for Pair<'_, R> 
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
        {
            let rule = self.as_rule();
            let start = self.pos(self.start);
            let end = self.pos(self.pair());
            let mut pairs = self.clone().into_inner().peekable();

            if pairs.peek().is_none() {
                write!(f, "{:?}({}, {})", rule, start, end)
            } else {
                write!(
                    f,
                    "{:?}({}, {}, [{}])",
                    rule,
                    start,
                    end,
                    pairs
                        .map(|pair| format!("{}", pair))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }

    impl<'i, R: PartialEq> PartialEq for Pair<'i, R> 
    {
        fn eq(&self, other: &Pair<'i, R>) -> bool 
        {
            Rc::ptr_eq(&self.queue, &other.queue)
                && ptr::eq(self.input, other.input)
                && self.start == other.start
        }
    }

    impl<R: Eq> Eq for Pair<'_, R> {}

    impl<'i, R: Hash> Hash for Pair<'i, R> 
    {
        fn hash<H: Hasher>(&self, state: &mut H) 
        {
            (&*self.queue as *const Vec<QueueableToken<'i, R>>).hash(state);
            (self.input as *const str).hash(state);
            self.start.hash(state);
        }
    }
    
    #[derive(Clone)] pub struct Pairs<'i, R> 
    {
        queue: Rc<Vec<QueueableToken<'i, R>>>,
        input: &'i str,
        start: usize,
        end: usize,
        pairs_count: usize,
        line_index: Rc<LineIndex>,
    }

    pub fn create_pairs<'i, R: RuleType>
    (
        queue: Rc<Vec<QueueableToken<'i, R>>>,
        input: &'i str,
        line_index: Option<Rc<LineIndex>>,
        start: usize,
        end: usize,
    ) -> Pairs<'i, R> 
    {
        let line_index = match line_index 
        {
            Some(line_index) => line_index,
            None => {
                let last_input_pos = queue
                    .last()
                    .map(|token| match *token {
                        QueueableToken::Start { input_pos, .. }
                        | QueueableToken::End { input_pos, .. } => input_pos,
                    })
                    .unwrap_or(0);

                Rc::new(LineIndex::new(&input[..last_input_pos]))
            }
        };

        let mut pairs_count = 0;
        let mut cursor = start;
        while cursor < end {
            cursor = match queue[cursor] {
                QueueableToken::Start {
                    end_token_index, ..
                } => end_token_index,
                _ => unreachable!(),
            } + 1;
            pairs_count += 1;
        }

        Pairs {
            queue,
            input,
            start,
            end,
            pairs_count,
            line_index,
        }
    }

    impl<'i, R: RuleType> Pairs<'i, R> 
    {
        #[inline] pub fn as_str(&self) -> &'i str {
            if self.start < self.end {
                let start = self.pos(self.start);
                let end = self.pos(self.end - 1);
                &self.input[start..end]
            } else {
                ""
            }
        }
        
        pub fn get_input(&self) -> &'i str { self.input }
        
        #[inline] pub fn concat(&self) -> String 
        {
            self.clone()
                .fold(String::new(), |string, pair| string + pair.as_str())
        }
        
        #[inline] pub fn flatten(self) -> FlatPairs<'i, R> 
        {
            flat_pairs::new(
                self.queue,
                self.input,
                self.line_index,
                self.start,
                self.end,
            )
        }
        
        #[inline] pub fn find_first_tagged(&self, tag: &'i str) -> Option<Pair<'i, R>> {
            self.clone().find_tagged(tag).next()
        }
        
        #[inline] pub fn find_tagged(
            self,
            tag: &'i str,
        ) -> Filter<FlatPairs<'i, R>, impl FnMut(&Pair<'i, R>) -> bool + 'i> {
            self.flatten()
                .filter(move |pair: &Pair<'i, R>| matches!(pair.as_node_tag(), Some(nt) if nt == tag))
        }
        
        #[inline] pub fn tokens(self) -> Tokens<'i, R> {
            tokens::new(self.queue, self.input, self.start, self.end)
        }
        
        #[inline] pub fn peek(&self) -> Option<Pair<'i, R>> {
            if self.start < self.end {
                Some(pair::new(
                    Rc::clone(&self.queue),
                    self.input,
                    Rc::clone(&self.line_index),
                    self.start,
                ))
            } else {
                None
            }
        }
        
        pub fn is_empty(&self) -> bool {
            self.pairs_count == 0
        }
        

        fn pair(&self) -> usize 
        {
            match self.queue[self.start] {
                QueueableToken::Start {
                    end_token_index, ..
                } => end_token_index,
                _ => unreachable!(),
            }
        }

        fn pair_from_end(&self) -> usize 
        {
            match self.queue[self.end - 1] {
                QueueableToken::End {
                    start_token_index, ..
                } => start_token_index,
                _ => unreachable!(),
            }
        }

        fn pos(&self, index: usize) -> usize 
        {
            match self.queue[index] {
                QueueableToken::Start { input_pos, .. } | QueueableToken::End { input_pos, .. } => {
                    input_pos
                }
            }
        }
    }

    impl<R: RuleType> ExactSizeIterator for Pairs<'_, R> 
    {
        #[inline] fn len(&self) -> usize {
            self.pairs_count
        }
    }

    impl<'i, R: RuleType> Iterator for Pairs<'i, R> 
    {
        type Item = Pair<'i, R>;

        fn next(&mut self) -> Option<Self::Item> {
            let pair = self.peek()?;

            self.start = self.pair() + 1;
            self.pairs_count -= 1;
            Some(pair)
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            let len = <Self as ExactSizeIterator>::len(self);
            (len, Some(len))
        }
    }

    impl<R: RuleType> DoubleEndedIterator for Pairs<'_, R> 
    {
        fn next_back(&mut self) -> Option<Self::Item> {
            if self.end <= self.start {
                return None;
            }

            self.end = self.pair_from_end();
            self.pairs_count -= 1;

            let pair = pair::new(
                Rc::clone(&self.queue),
                self.input,
                Rc::clone(&self.line_index),
                self.end,
            );

            Some(pair)
        }
    }

    impl<R: RuleType> fmt::Debug for Pairs<'_, R>
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_list().entries(self.clone()).finish()
        }
    }

    impl<R: RuleType> fmt::Display for Pairs<'_, R> 
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "[{}]",
                self.clone()
                    .map(|pair| format!("{}", pair))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }

    impl<'i, R: PartialEq> PartialEq for Pairs<'i, R> 
    {
        fn eq(&self, other: &Pairs<'i, R>) -> bool {
            Rc::ptr_eq(&self.queue, &other.queue)
                && ptr::eq(self.input, other.input)
                && self.start == other.start
                && self.end == other.end
        }
    }

    impl<R: Eq> Eq for Pairs<'_, R> {}

    impl<'i, R: Hash> Hash for Pairs<'i, R> 
    {
        fn hash<H: Hasher>(&self, state: &mut H) {
            (&*self.queue as *const Vec<QueueableToken<'i, R>>).hash(state);
            (self.input as *const str).hash(state);
            self.start.hash(state);
            self.end.hash(state);
        }
    }
}

pub mod jobc
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::io::Write;

    use nix::sys::signal::Signal;
    use nix::sys::wait::waitpid;
    use nix::sys::wait::WaitPidFlag as WF;
    use nix::sys::wait::WaitStatus as WS;
    use nix::unistd::Pid;

    use crate::shell;
    use crate::signals;
    use crate::types::{self, CommandResult};
    */
    pub fn get_job_line(job: &types::Job, trim: bool) -> String 
    {
        let mut cmd = job.cmd.clone();
        if trim && cmd.len() > 50 {
            cmd.truncate(50);
            cmd.push_str(" ...");
        }
        let _cmd = if job.is_bg && job.status == "Running" {
            format!("{} &", cmd)
        } else {
            cmd
        };
        format!("[{}] {}  {}   {}", job.id, job.gid, job.status, _cmd)
    }

    pub fn print_job(job: &types::Job) {
        let line = get_job_line(job, true);
        println_stderr!("{}", line);
    }

    pub fn mark_job_as_done(sh: &mut shell::Shell, gid: i32, pid: i32, reason: &str) {
        if let Some(mut job) = sh.remove_pid_from_job(gid, pid) {
            job.status = reason.to_string();
            if job.is_bg {
                println_stderr!("");
                print_job(&job);
            }
        }
    }

    pub fn mark_job_as_stopped(sh: &mut shell::Shell, gid: i32, report: bool) {
        sh.mark_job_as_stopped(gid);
        if !report {
            return;
        }

        // add an extra line to separate output of fg commands if any.
        if let Some(job) = sh.get_job_by_gid(gid) {
            println_stderr!("");
            print_job(job);
        }
    }

    pub fn mark_job_member_stopped(sh: &mut shell::Shell, pid: i32, gid: i32, report: bool) {
        let _gid = if gid == 0 {
            unsafe { libc::getpgid(pid) }
        } else {
            gid
        };

        if let Some(job) = sh.mark_job_member_stopped(pid, gid) {
            if job.all_members_stopped() {
                mark_job_as_stopped(sh, gid, report);
            }
        }
    }

    pub fn mark_job_member_continued(sh: &mut shell::Shell, pid: i32, gid: i32) {
        let _gid = if gid == 0 {
            unsafe { libc::getpgid(pid) }
        } else {
            gid
        };

        if let Some(job) = sh.mark_job_member_continued(pid, gid) {
            if job.all_members_running() {
                mark_job_as_running(sh, gid, true);
            }
        }
    }

    pub fn mark_job_as_running(sh: &mut shell::Shell, gid: i32, bg: bool) {
        sh.mark_job_as_running(gid, bg);
    }

    #[allow(unreachable_patterns)]
    pub fn waitpidx(wpid: i32, block: bool) -> types::WaitStatus {
        let options = if block {
            Some(WF::WUNTRACED | WF::WCONTINUED)
        } else {
            Some(WF::WUNTRACED | WF::WCONTINUED | WF::WNOHANG)
        };
        match waitpid(Pid::from_raw(wpid), options) {
            Ok(WS::Exited(pid, status)) => {
                let pid = i32::from(pid);
                types::WaitStatus::from_exited(pid, status)
            }
            Ok(WS::Stopped(pid, sig)) => {
                let pid = i32::from(pid);
                types::WaitStatus::from_stopped(pid, sig as i32)
            }
            Ok(WS::Continued(pid)) => {
                let pid = i32::from(pid);
                types::WaitStatus::from_continuted(pid)
            }
            Ok(WS::Signaled(pid, sig, _core_dumped)) => {
                let pid = i32::from(pid);
                types::WaitStatus::from_signaled(pid, sig as i32)
            }
            Ok(WS::StillAlive) => types::WaitStatus::empty(),
            Ok(_others) => {
                // this is for PtraceEvent and PtraceSyscall on Linux,
                // unreachable on other platforms.
                types::WaitStatus::from_others()
            }
            Err(e) => types::WaitStatus::from_error(e as i32),
        }
    }

    pub fn wait_fg_job(sh: &mut shell::Shell, gid: i32, pids: &[i32]) -> CommandResult {
        let mut cmd_result = CommandResult::new();
        let mut count_waited = 0;
        let count_child = pids.len();
        if count_child == 0 {
            return cmd_result;
        }
        let pid_last = pids.last().unwrap();

        loop {
            let ws = waitpidx(-1, true);
            // here when we calling waitpidx(), all signals should have
            // been masked. There should no errors (ECHILD/EINTR etc) happen.
            if ws.is_error() {
                let err = ws.get_errno();
                if err == nix::Error::ECHILD {
                    break;
                }

                log!("jobc unexpected waitpid error: {}", err);
                cmd_result = CommandResult::from_status(gid, err as i32);
                break;
            }

            let pid = ws.get_pid();
            let is_a_fg_child = pids.contains(&pid);
            if is_a_fg_child && !ws.is_continued() {
                count_waited += 1;
            }

            if ws.is_exited() {
                if is_a_fg_child {
                    mark_job_as_done(sh, gid, pid, "Done");
                } else {
                    let status = ws.get_status();
                    signals::insert_reap_map(pid, status);
                }
            } else if ws.is_stopped() {
                if is_a_fg_child {
                    // for stop signal of fg job (current job)
                    // i.e. Ctrl-Z is pressed on the fg job
                    mark_job_member_stopped(sh, pid, gid, true);
                } else {
                    // for stop signal of bg jobs
                    signals::insert_stopped_map(pid);
                    mark_job_member_stopped(sh, pid, 0, false);
                }
            } else if ws.is_continued() {
                if !is_a_fg_child {
                    signals::insert_cont_map(pid);
                }
                continue;
            } else if ws.is_signaled() {
                if is_a_fg_child {
                    mark_job_as_done(sh, gid, pid, "Killed");
                } else {
                    signals::killed_map_insert(pid, ws.get_signal());
                }
            }

            if is_a_fg_child && pid == *pid_last {
                let status = ws.get_status();
                cmd_result.status = status;
            }

            if count_waited >= count_child {
                break;
            }
        }
        cmd_result
    }

    pub fn try_wait_bg_jobs(sh: &mut shell::Shell, report: bool, sig_handler_enabled: bool) {
        if sh.jobs.is_empty() {
            return;
        }

        if !sig_handler_enabled {
            // we need to wait pids in case CICADA_ENABLE_SIG_HANDLER=0
            signals::handle_sigchld(Signal::SIGCHLD as i32);
        }

        let jobs = sh.jobs.clone();
        for (_i, job) in jobs.iter() {
            for pid in job.pids.iter() {
                if let Some(_status) = signals::pop_reap_map(*pid) {
                    mark_job_as_done(sh, job.gid, *pid, "Done");
                    continue;
                }

                if let Some(sig) = signals::killed_map_pop(*pid) {
                    let reason = if sig == Signal::SIGQUIT as i32 {
                        format!("Quit: {}", sig)
                    } else if sig == Signal::SIGINT as i32 {
                        format!("Interrupt: {}", sig)
                    } else if sig == Signal::SIGKILL as i32 {
                        format!("Killed: {}", sig)
                    } else if sig == Signal::SIGTERM as i32 {
                        format!("Terminated: {}", sig)
                    } else {
                        format!("Killed: {}", sig)
                    };
                    mark_job_as_done(sh, job.gid, *pid, &reason);
                    continue;
                }

                if signals::pop_stopped_map(*pid) {
                    mark_job_member_stopped(sh, *pid, job.gid, report);
                } else if signals::pop_cont_map(*pid) {
                    mark_job_member_continued(sh, *pid, job.gid);
                }
            }
        }
    }
}

pub mod libs
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    pub mod colored
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        */
        // cicada special
        pub const SEQ: &str = "\x01";
        pub const END_SEQ: &str = "\x02";
        pub const ESC: &str = "\x1B";

        // Set
        pub const BOLD: &str = "\x01\x1B[1m\x02";
        pub const DIM: &str = "\x01\x1B[2m\x02";
        pub const UNDERLINED: &str = "\x01\x1B[4m\x02";
        pub const BLINK: &str = "\x01\x1B[5m\x02";
        pub const REVERSE: &str = "\x01\x1B[7m\x02";
        pub const HIDDEN: &str = "\x01\x1B[8m\x02";

        // Reset
        pub const RESET: &str = "\x01\x1B[0m\x02";
        pub const RESET_BOLD: &str = "\x01\x1B[21m\x02";
        pub const RESET_DIM: &str = "\x01\x1B[22m\x02";
        pub const RESET_UNDERLINED: &str = "\x01\x1B[24m\x02";
        pub const RESET_BLINK: &str = "\x01\x1B[25m\x02";
        pub const RESET_REVERSE: &str = "\x01\x1B[27m\x02";
        pub const RESET_HIDDEN: &str = "\x01\x1B[28m\x02";

        // Foreground (text)
        pub const DEFAULT: &str = "\x01\x1B[39m\x02";
        pub const BLACK: &str = "\x01\x1B[30m\x02";
        pub const RED: &str = "\x01\x1B[31m\x02";
        pub const GREEN: &str = "\x01\x1B[32m\x02";
        pub const YELLOW: &str = "\x01\x1B[33m\x02";
        pub const BLUE: &str = "\x01\x1B[34m\x02";
        pub const MAGENTA: &str = "\x01\x1B[35m\x02";
        pub const CYAN: &str = "\x01\x1B[36m\x02";
        pub const GRAY_L: &str = "\x01\x1B[37m\x02";

        pub const GRAY_D: &str = "\x01\x1B[90m\x02";
        pub const RED_L: &str = "\x01\x1B[91m\x02";
        pub const GREEN_L: &str = "\x01\x1B[92m\x02";
        pub const YELLOW_L: &str = "\x01\x1B[93m\x02";
        pub const BLUE_L: &str = "\x01\x1B[94m\x02";
        pub const MAGENTA_L: &str = "\x01\x1B[95m\x02";
        pub const CYAN_L: &str = "\x01\x1B[96m\x02";
        pub const WHITE: &str = "\x01\x1B[97m\x02";

        pub const BLUE_B: &str = "\x01\x1B[34m\x1B[1m\x02";
        pub const BLACK_B: &str = "\x01\x1B[30m\x1B[1m\x02";
        pub const WHITE_B: &str = "\x01\x1B[97m\x1B[1m\x02";
        pub const RED_B: &str = "\x01\x1B[31m\x1B[1m\x02";
        pub const GREEN_B: &str = "\x01\x1B[32m\x1B[1m\x02";

        // Background
        pub const DEFAULT_BG: &str = "\x01\x1B[49m\x02";
        pub const BLACK_BG: &str = "\x01\x1B[40m\x02";
        pub const RED_BG: &str = "\x01\x1B[41m\x02";
        pub const GREEN_BG: &str = "\x01\x1B[42m\x02";
        pub const YELLOW_BG: &str = "\x01\x1B[43m\x02";
        pub const BLUE_BG: &str = "\x01\x1B[44m\x02";
        pub const MAGENTA_BG: &str = "\x01\x1B[45m\x02";
        pub const CYAN_BG: &str = "\x01\x1B[46m\x02";
        pub const GRAY_L_BG: &str = "\x01\x1B[47m\x02";

        pub const GRAY_D_BG: &str = "\x01\x1B[100m\x02";
        pub const RED_L_BG: &str = "\x01\x1B[101m\x02";
        pub const GREEN_L_BG: &str = "\x01\x1B[102m\x02";
        pub const YELLOW_L_BG: &str = "\x01\x1B[103m\x02";
        pub const BLUE_L_BG: &str = "\x01\x1B[104m\x02";
        pub const MAGENTA_L_BG: &str = "\x01\x1B[105m\x02";
        pub const CYAN_L_BG: &str = "\x01\x1B[106m\x02";
        pub const WHITE_BG: &str = "\x01\x1B[107m\x02";
    }

    pub mod fork
    {
        /*!
        */
        use ::
        {
            nix::
            {
                unistd::{fork as nix_fork, ForkResult},
                Result,
            },
            *,
        };
        /*
        use nix::unistd::{fork as nix_fork, ForkResult};
        use nix::Result;
        */
        pub fn fork() -> Result<ForkResult>
        {
            unsafe { nix_fork() }
        }
    }

    pub mod os_type
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use crate::execute;
        */
        pub fn get_os_name() -> String {
            let uname = get_uname();
            if uname.to_lowercase() == "darwin" {
                get_macos_name()
            } else {
                get_other_os_name()
            }
        }

        pub fn get_other_os_name() -> String {
            let mut name = get_release_value("PRETTY_NAME");
            if !name.is_empty() {
                return name;
            }
            name = get_release_value("DISTRIB_DESCRIPTION");
            if !name.is_empty() {
                return name;
            }
            name = get_release_value("IMAGE_DESCRIPTION");
            if !name.is_empty() {
                return name;
            }
            get_uname_mo()
        }

        pub fn get_release_value(ptn: &str) -> String {
            let line = format!(
                "grep -i '{}' /etc/*release* 2>&1 | grep -o '=.*' | tr '\"=' ' '",
                ptn
            );
            let cr = execute::run(&line);
            cr.stdout.trim().to_string()
        }

        pub fn get_uname() -> String {
            let cr = execute::run("uname");
            cr.stdout.trim().to_string()
        }

        pub fn get_uname_mo() -> String {
            let cr = execute::run("uname -m -o");
            cr.stdout.trim().to_string()
        }

        pub fn get_macos_name() -> String {
            let mut os_name = get_osx_codename();
            let ver = get_osx_version();
            if !ver.is_empty() {
                os_name.push(' ');
                os_name.push_str(&ver);
            }
            os_name
        }

        pub fn get_osx_codename() -> String {
            let cr = execute::run("grep -o 'SOFTWARE LICENSE AGREEMENT FOR .*[a-zA-Z]' '/System/Library/CoreServices/Setup Assistant.app/Contents/Resources/en.lproj/OSXSoftwareLicense.rtf' | sed 's/SOFTWARE LICENSE AGREEMENT FOR *//'");
            cr.stdout.trim().to_string()
        }

        pub fn get_osx_version() -> String {
            let cr = execute::run("sw_vers -productVersion");
            cr.stdout.trim().to_string()
        }
    }

    pub mod path
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::borrow::Cow;
        use std::env;
        use std::fs::read_dir;
        use std::io::{ErrorKind, Write};
        use std::os::unix::fs::PermissionsExt;

        use regex::Regex;

        use crate::tools;
        */
        pub fn basename(path: &str) -> Cow<'_, str> {
            let mut pieces = path.rsplit('/');
            match pieces.next() {
                Some(p) => p.into(),
                None => path.into(),
            }
        }

        pub fn expand_home(text: &str) -> String {
            let mut s: String = text.to_string();
            let v = vec![
                r"(?P<head> +)~(?P<tail> +)",
                r"(?P<head> +)~(?P<tail>/)",
                r"^(?P<head> *)~(?P<tail>/)",
                r"(?P<head> +)~(?P<tail> *$)",
            ];
            for item in &v {
                let re;
                if let Ok(x) = Regex::new(item) {
                    re = x;
                } else {
                    return String::new();
                }
                let home = tools::get_user_home();
                let ss = s.clone();
                let to = format!("$head{}$tail", home);
                let result = re.replace_all(ss.as_str(), to.as_str());
                s = result.to_string();
            }
            s
        }

        pub fn find_file_in_path(filename: &str, exec: bool) -> String {
            let env_path = match env::var("PATH") {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!(":: error with env PATH: {:?}", e);
                    return String::new();
                }
            };
            let vec_path = env::split_paths(&env_path);
            for p in vec_path {
                match read_dir(&p) {
                    Ok(list) => {
                        for entry in list.flatten() {
                            if let Ok(name) = entry.file_name().into_string() {
                                if name != filename {
                                    continue;
                                }

                                if exec {
                                    let _mode = match entry.metadata() {
                                        Ok(x) => x,
                                        Err(e) => {
                                            println_stderr!(":: metadata error: {:?}", e);
                                            continue;
                                        }
                                    };
                                    let mode = _mode.permissions().mode();
                                    if mode & 0o111 == 0 {
                                        // not binary
                                        continue;
                                    }
                                }

                                return entry.path().to_string_lossy().to_string();
                            }
                        }
                    }
                    Err(e) => {
                        if e.kind() == ErrorKind::NotFound {
                            continue;
                        }
                        log!(":: fs read_dir error: {}: {}", p.display(), e);
                    }
                }
            }
            String::new()
        }

        pub fn current_dir() -> String {
            let _current_dir = match env::current_dir() {
                Ok(x) => x,
                Err(e) => {
                    log!(":: PROMPT: env current_dir error: {}", e);
                    return String::new();
                }
            };
            let current_dir = match _current_dir.to_str() {
                Some(x) => x,
                None => {
                    log!(":: PROMPT: to_str error");
                    return String::new();
                }
            };

            current_dir.to_string()
        }
    }

    pub mod pipes
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use libc::c_int;
        use nix::Error;
        use std::mem;
        use std::os::fd::RawFd;
        */
        pub fn pipe() -> std::result::Result<(RawFd, RawFd), Error> {
            let mut fds = mem::MaybeUninit::<[c_int; 2]>::uninit();
            let res = unsafe { libc::pipe(fds.as_mut_ptr() as *mut c_int) };
            Error::result(res)?;
            unsafe { Ok((fds.assume_init()[0], fds.assume_init()[1])) }
        }
    }

    pub mod progopts
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        */
        pub fn is_login(args: &[String]) -> bool {
            if !args.is_empty() && args[0].starts_with("-") {
                return true;
            }

            if args.len() > 1 && (args[1] == "--login" || args[1] == "-l") {
                return true;
            }

            if let Ok(term_program) = std::env::var("TERM_PROGRAM") {
                if term_program == "vscode" {
                    return true;
                }
            }

            false
        }

        pub fn is_script(args: &[String]) -> bool {
            args.len() > 1 && !args[1].starts_with("-")
        }

        pub fn is_command_string(args: &[String]) -> bool {
            args.len() > 1 && args[1] == "-c"
        }

        pub fn is_non_tty() -> bool {
            unsafe { libc::isatty(0) == 0 }
        }
    }

    pub mod re
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        */
        pub fn find_first_group(ptn: &str, text: &str) -> Option<String> 
        {
            let re = match regex::Regex::new(ptn) {
                Ok(x) => x,
                Err(_) => return None,
            };
            match re.captures(text) {
                Some(caps) => {
                    if let Some(x) = caps.get(1) {
                        return Some(x.as_str().to_owned());
                    }
                }
                None => {
                    return None;
                }
            }
            None
        }

        pub fn re_contains(text: &str, ptn: &str) -> bool {
            let re = match regex::Regex::new(ptn) {
                Ok(x) => x,
                Err(e) => {
                    println!("Regex new error: {:?}", e);
                    return false;
                }
            };
            re.is_match(text)
        }

        pub fn replace_all(text: &str, ptn: &str, ptn_to: &str) -> String 
        {
            let re = regex::Regex::new(ptn).unwrap();
            let result = re.replace_all(text, ptn_to);
            result.to_string()
        }
    }

    pub mod term_size
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use libc::{c_int, c_ulong, winsize, STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO};
        use std::mem::zeroed;
        */
        // Unfortunately the actual command is not standardised...
        #[cfg(any(target_os = "linux", target_os = "android"))]
        static TIOCGWINSZ: c_ulong = 0x5413;

        #[cfg(any(
            target_os = "macos",
            target_os = "ios",
            target_os = "dragonfly",
            target_os = "freebsd",
            target_os = "netbsd",
            target_os = "openbsd"
        ))]
        static TIOCGWINSZ: c_ulong = 0x40087468;

        #[cfg(target_os = "solaris")]
        static TIOCGWINSZ: c_ulong = 0x5468;

        extern "C" {
            fn ioctl(fd: c_int, request: c_ulong, ...) -> c_int;
        }

        /// Runs the ioctl command. Returns (0, 0) if all of the streams are not to a terminal, or
        /// there is an error. (0, 0) is an invalid size to have anyway, which is why
        /// it can be used as a nil value.
        unsafe fn get_dimensions_any() -> winsize {
            let mut window: winsize = zeroed();
            let mut result = ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut window);

            if result == -1 {
                window = zeroed();
                result = ioctl(STDIN_FILENO, TIOCGWINSZ, &mut window);
                if result == -1 {
                    window = zeroed();
                    result = ioctl(STDERR_FILENO, TIOCGWINSZ, &mut window);
                    if result == -1 {
                        return zeroed();
                    }
                }
            }
            window
        }

        /// Query the current processes's output (`stdout`), input (`stdin`), and error (`stderr`) in
        /// that order, in the attempt to dtermine terminal width. If one of those streams is actually
        /// a tty, this function returns its width and height as a number of characters.
        ///
        /// # Errors
        ///
        /// If *all* of the streams are not ttys or return any errors this function will return `None`.
        ///
        /// # Example
        ///
        /// To get the dimensions of your terminal window, simply use the following:
        ///
        /// ```ignore
        /// if let Some((w, h)) = term_size::dimensions() {
        ///     println!("Width: {}\nHeight: {}", w, h);
        /// } else {
        ///     println!("Unable to get term size :(")
        /// }
        /// ```
        pub fn dimensions() -> Option<(usize, usize)> {
            let w = unsafe { get_dimensions_any() };

            if w.ws_col == 0 || w.ws_row == 0 {
                None
            } else {
                Some((w.ws_col as usize, w.ws_row as usize))
            }
        }
    }
    
    pub fn close(fd: i32) {
        unsafe {
            libc::close(fd);
        }
    }

    pub fn dup(fd: i32) -> i32 {
        unsafe { libc::dup(fd) }
    }

    pub fn dup2(src: i32, dst: i32) {
        unsafe {
            libc::dup2(src, dst);
        }
    }
}

pub mod num
{
    pub use std::num::{ * };
}

pub mod os
{
    pub use std::os::{ fd as _, * };
    
    pub mod fd
    {
        pub use std::os::fd::{ * };
        

        pub fn create_raw_fd_from_file(file_name: &str, append: bool) -> Result<i32, String>
        {
            let mut oos = OpenOptions::new();
            if append {
                oos.append(true);
            } else {
                oos.write(true);
                oos.truncate(true);
            }
            match oos.create(true).open(file_name) {
                Ok(x) => {
                    let fd = x.into_raw_fd();
                    Ok(fd)
                }
                Err(e) => Err(format!("{}", e)),
            }
        }
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
    use crate::error::Error;
    use crate::iterators::Pairs;
    use crate::RuleType;
    
    pub use crate::position::Position;
    pub use crate::span::{merge_spans, Lines, LinesSpan, Span};
    pub use crate::stack::Stack;
    pub use crate::token::Token;
    use core::fmt::Debug;
    use core::hash::Hash;
    */
    pub trait RuleType: Copy + Debug + Eq + Hash + Ord {}

    impl<T: Copy + Debug + Eq + Hash + Ord> RuleType for T {}

    pub mod error
    {
        /*!
        Types for different kinds of parsing failures. */
        use ::
        {
            *,
        };
        /*
        use crate::parser_state::{ParseAttempts, ParsingToken, RulesCallStack};
        use alloc::borrow::Cow;
        use alloc::borrow::ToOwned;
        use alloc::boxed::Box;
        use alloc::collections::{BTreeMap, BTreeSet};
        use alloc::format;
        use alloc::string::String;
        use alloc::string::ToString;
        use alloc::vec;
        use alloc::vec::Vec;
        use core::cmp;
        use core::fmt;
        use core::mem;

        use crate::position::Position;
        use crate::span::Span;
        use crate::RuleType;
        */
        /// Parse-related error type.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct Error<R> 
        {
            /// Variant of the error
            pub variant: ErrorVariant<R>,
            /// Location within the input string
            pub location: InputLocation,
            /// Line/column within the input string
            pub line_col: LineColLocation,
            path: Option<String>,
            line: String,
            continued_line: Option<String>,
            parse_attempts: Option<ParseAttempts<R>>,
        }

        impl<R: RuleType> core::error::Error for Error<R> {}

        /// Different kinds of parsing errors.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub enum ErrorVariant<R> 
        {
            /// Generated parsing error with expected and unexpected `Rule`s
            ParsingError 
            {
                /// Positive attempts
                positives: Vec<R>,
                /// Negative attempts
                negatives: Vec<R>,
            },
            /// Custom error with a message
            CustomError 
            {
                /// Short explanation
                message: String,
            },
        }

        impl<R: RuleType> core::error::Error for ErrorVariant<R> {}

        /// Where an `Error` has occurred.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub enum InputLocation {
            /// `Error` was created by `Error::new_from_pos`
            Pos(usize),
            /// `Error` was created by `Error::new_from_span`
            Span((usize, usize)),
        }

        /// Line/column where an `Error` has occurred.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub enum LineColLocation {
            /// Line/column pair if `Error` was created by `Error::new_from_pos`
            Pos((usize, usize)),
            /// Line/column pairs if `Error` was created by `Error::new_from_span`
            Span((usize, usize), (usize, usize)),
        }

        impl From<Position<'_>> for LineColLocation {
            fn from(value: Position<'_>) -> Self {
                Self::Pos(value.line_col())
            }
        }

        impl From<Span<'_>> for LineColLocation {
            fn from(value: Span<'_>) -> Self {
                let (start, end) = value.split();
                Self::Span(start.line_col(), end.line_col())
            }
        }

        /// Function mapping rule to its helper message defined by user.
        pub type RuleToMessageFn<R> = Box<dyn Fn(&R) -> Option<String>>;
        /// Function mapping string element to bool denoting whether it's a whitespace defined by user.
        pub type IsWhitespaceFn = Box<dyn Fn(String) -> bool>;

        impl ParsingToken {
            pub fn is_whitespace(&self, is_whitespace: &IsWhitespaceFn) -> bool {
                match self {
                    ParsingToken::Sensitive { token } => is_whitespace(token.clone()),
                    ParsingToken::Insensitive { token } => is_whitespace(token.clone()),
                    ParsingToken::Range { .. } => false,
                    ParsingToken::BuiltInRule => false,
                }
            }
        }

        impl<R: RuleType> ParseAttempts<R> {
            /// Helper formatting function to get message informing about tokens we've
            /// (un)expected to see.
            /// Used as a part of `parse_attempts_error`.
            fn tokens_helper_messages(
                &self,
                is_whitespace_fn: &IsWhitespaceFn,
                spacing: &str,
            ) -> Vec<String> {
                let mut helper_messages = Vec::new();
                let tokens_header_pairs = vec![
                    (self.expected_tokens(), "expected"),
                    (self.unexpected_tokens(), "unexpected"),
                ];

                for (tokens, header) in &tokens_header_pairs {
                    if tokens.is_empty() {
                        continue;
                    }

                    let mut helper_tokens_message = format!("{spacing}note: {header} ");
                    helper_tokens_message.push_str(if tokens.len() == 1 {
                        "token: "
                    } else {
                        "one of tokens: "
                    });

                    let expected_tokens_set: BTreeSet<String> = tokens
                        .iter()
                        .map(|token| {
                            if token.is_whitespace(is_whitespace_fn) {
                                String::from("WHITESPACE")
                            } else {
                                format!("`{}`", token)
                            }
                        })
                        .collect();

                    helper_tokens_message.push_str(
                        &expected_tokens_set
                            .iter()
                            .cloned()
                            .collect::<Vec<String>>()
                            .join(", "),
                    );
                    helper_messages.push(helper_tokens_message);
                }

                helper_messages
            }
        }

        impl<R: RuleType> Error<R> {
            /// Creates `Error` from `ErrorVariant` and `Position`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use pest::error::{Error, ErrorVariant};
            /// # use pest::Position;
            /// # #[allow(non_camel_case_types)]
            /// # #[allow(dead_code)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// # enum Rule {
            /// #     open_paren,
            /// #     closed_paren
            /// # }
            /// # let input = "";
            /// # let pos = Position::from_start(input);
            /// let error = Error::new_from_pos(
            ///     ErrorVariant::ParsingError {
            ///         positives: vec![Rule::open_paren],
            ///         negatives: vec![Rule::closed_paren],
            ///     },
            ///     pos
            /// );
            ///
            /// println!("{}", error);
            /// ```
            pub fn new_from_pos(variant: ErrorVariant<R>, pos: Position<'_>) -> Error<R> {
                let visualize_ws = pos.match_char('\n') || pos.match_char('\r');
                let line_of = pos.line_of();
                let line = if visualize_ws {
                    visualize_whitespace(line_of)
                } else {
                    line_of.replace(&['\r', '\n'][..], "")
                };
                Error {
                    variant,
                    location: InputLocation::Pos(pos.pos()),
                    path: None,
                    line,
                    continued_line: None,
                    line_col: LineColLocation::Pos(pos.line_col()),
                    parse_attempts: None,
                }
            }

            /// Wrapper function to track `parse_attempts` as a result
            /// of `state` function call in `parser_state.rs`.
            pub fn new_from_pos_with_parsing_attempts(
                variant: ErrorVariant<R>,
                pos: Position<'_>,
                parse_attempts: ParseAttempts<R>,
            ) -> Error<R> {
                let mut error = Self::new_from_pos(variant, pos);
                error.parse_attempts = Some(parse_attempts);
                error
            }

            /// Creates `Error` from `ErrorVariant` and `Span`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use pest::error::{Error, ErrorVariant};
            /// # use pest::{Position, Span};
            /// # #[allow(non_camel_case_types)]
            /// # #[allow(dead_code)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// # enum Rule {
            /// #     open_paren,
            /// #     closed_paren
            /// # }
            /// # let input = "";
            /// # let start = Position::from_start(input);
            /// # let end = start.clone();
            /// # let span = start.span(&end);
            /// let error = Error::new_from_span(
            ///     ErrorVariant::ParsingError {
            ///         positives: vec![Rule::open_paren],
            ///         negatives: vec![Rule::closed_paren],
            ///     },
            ///     span
            /// );
            ///
            /// println!("{}", error);
            /// ```
            pub fn new_from_span(variant: ErrorVariant<R>, span: Span<'_>) -> Error<R> {
                let end = span.end_pos();
                let mut end_line_col = end.line_col();
                // end position is after a \n, so we want to point to the visual lf symbol
                if end_line_col.1 == 1 {
                    let mut visual_end = end;
                    visual_end.skip_back(1);
                    let lc = visual_end.line_col();
                    end_line_col = (lc.0, lc.1 + 1);
                };

                let mut line_iter = span.lines();
                let sl = line_iter.next().unwrap_or("");
                let mut chars = span.as_str().chars();
                let visualize_ws = matches!(chars.next(), Some('\n') | Some('\r'))
                    || matches!(chars.last(), Some('\n') | Some('\r'));
                let start_line = if visualize_ws {
                    visualize_whitespace(sl)
                } else {
                    sl.to_owned().replace(&['\r', '\n'][..], "")
                };
                let ll = line_iter.last();
                let continued_line = if visualize_ws {
                    ll.map(str::to_owned)
                } else {
                    ll.map(visualize_whitespace)
                };

                Error {
                    variant,
                    location: InputLocation::Span((span.start(), end.pos())),
                    path: None,
                    line: start_line,
                    continued_line,
                    line_col: LineColLocation::Span(span.start_pos().line_col(), end_line_col),
                    parse_attempts: None,
                }
            }

            /// Returns `Error` variant with `path` which is shown when formatted with `Display`.
            ///
            /// # Examples
            ///
            /// ```
            /// # use pest::error::{Error, ErrorVariant};
            /// # use pest::Position;
            /// # #[allow(non_camel_case_types)]
            /// # #[allow(dead_code)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// # enum Rule {
            /// #     open_paren,
            /// #     closed_paren
            /// # }
            /// # let input = "";
            /// # let pos = Position::from_start(input);
            /// Error::new_from_pos(
            ///     ErrorVariant::ParsingError {
            ///         positives: vec![Rule::open_paren],
            ///         negatives: vec![Rule::closed_paren],
            ///     },
            ///     pos
            /// ).with_path("file.rs");
            /// ```
            pub fn with_path(mut self, path: &str) -> Error<R> {
                self.path = Some(path.to_owned());

                self
            }

            /// Returns the path set using [`Error::with_path()`].
            ///
            /// # Examples
            ///
            /// ```
            /// # use pest::error::{Error, ErrorVariant};
            /// # use pest::Position;
            /// # #[allow(non_camel_case_types)]
            /// # #[allow(dead_code)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// # enum Rule {
            /// #     open_paren,
            /// #     closed_paren
            /// # }
            /// # let input = "";
            /// # let pos = Position::from_start(input);
            /// # let error = Error::new_from_pos(
            /// #     ErrorVariant::ParsingError {
            /// #         positives: vec![Rule::open_paren],
            /// #         negatives: vec![Rule::closed_paren],
            /// #     },
            /// #     pos);
            /// let error = error.with_path("file.rs");
            /// assert_eq!(Some("file.rs"), error.path());
            /// ```
            pub fn path(&self) -> Option<&str> {
                self.path.as_deref()
            }

            /// Returns the line that the error is on.
            pub fn line(&self) -> &str {
                self.line.as_str()
            }

            /// Renames all `Rule`s if this is a [`ParsingError`]. It does nothing when called on a
            /// [`CustomError`].
            ///
            /// Useful in order to rename verbose rules or have detailed per-`Rule` formatting.
            ///
            /// [`ParsingError`]: enum.ErrorVariant.html#variant.ParsingError
            /// [`CustomError`]: enum.ErrorVariant.html#variant.CustomError
            ///
            /// # Examples
            ///
            /// ```
            /// # use pest::error::{Error, ErrorVariant};
            /// # use pest::Position;
            /// # #[allow(non_camel_case_types)]
            /// # #[allow(dead_code)]
            /// # #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            /// # enum Rule {
            /// #     open_paren,
            /// #     closed_paren
            /// # }
            /// # let input = "";
            /// # let pos = Position::from_start(input);
            /// Error::new_from_pos(
            ///     ErrorVariant::ParsingError {
            ///         positives: vec![Rule::open_paren],
            ///         negatives: vec![Rule::closed_paren],
            ///     },
            ///     pos
            /// ).renamed_rules(|rule| {
            ///     match *rule {
            ///         Rule::open_paren => "(".to_owned(),
            ///         Rule::closed_paren => "closed paren".to_owned()
            ///     }
            /// });
            /// ```
            pub fn renamed_rules<F>(mut self, f: F) -> Error<R>
            where
                F: FnMut(&R) -> String,
            {
                let variant = match self.variant {
                    ErrorVariant::ParsingError {
                        positives,
                        negatives,
                    } => {
                        let message = Error::parsing_error_message(&positives, &negatives, f);
                        ErrorVariant::CustomError { message }
                    }
                    variant => variant,
                };

                self.variant = variant;

                self
            }

            /// Get detailed information about errored rules sequence.
            /// Returns `Some(results)` only for `ParsingError`.
            pub fn parse_attempts(&self) -> Option<ParseAttempts<R>> {
                self.parse_attempts.clone()
            }

            /// Get error message based on parsing attempts.
            /// Returns `None` in case self `parse_attempts` is `None`.
            pub fn parse_attempts_error(
                &self,
                input: &str,
                rule_to_message: &RuleToMessageFn<R>,
                is_whitespace: &IsWhitespaceFn,
            ) -> Option<Error<R>> {
                let attempts = if let Some(ref parse_attempts) = self.parse_attempts {
                    parse_attempts.clone()
                } else {
                    return None;
                };

                let spacing = self.spacing() + "   ";
                let error_position = attempts.max_position;
                let message = {
                    let mut help_lines: Vec<String> = Vec::new();
                    help_lines.push(String::from("error: parsing error occurred."));

                    // Note: at least one of `(un)expected_tokens` must not be empty.
                    for tokens_helper_message in attempts.tokens_helper_messages(is_whitespace, &spacing) {
                        help_lines.push(tokens_helper_message);
                    }

                    let call_stacks = attempts.call_stacks();
                    // Group call stacks by their parents so that we can print common header and
                    // several sub helper messages.
                    let mut call_stacks_parents_groups: BTreeMap<Option<R>, Vec<RulesCallStack<R>>> =
                        BTreeMap::new();
                    for call_stack in call_stacks {
                        call_stacks_parents_groups
                            .entry(call_stack.parent)
                            .or_default()
                            .push(call_stack);
                    }

                    for (group_parent, group) in call_stacks_parents_groups {
                        if let Some(parent_rule) = group_parent {
                            let mut contains_meaningful_info = false;
                            help_lines.push(format!(
                                "{spacing}help: {}",
                                if let Some(message) = rule_to_message(&parent_rule) {
                                    contains_meaningful_info = true;
                                    message
                                } else {
                                    String::from("[Unknown parent rule]")
                                }
                            ));
                            for call_stack in group {
                                if let Some(r) = call_stack.deepest.get_rule() {
                                    if let Some(message) = rule_to_message(r) {
                                        contains_meaningful_info = true;
                                        help_lines.push(format!("{spacing}      - {message}"));
                                    }
                                }
                            }
                            if !contains_meaningful_info {
                                // Have to remove useless line for unknown parent rule.
                                help_lines.pop();
                            }
                        } else {
                            for call_stack in group {
                                // Note that `deepest` rule may be `None`. E.g. in case it corresponds
                                // to WHITESPACE expected token which has no parent rule (on the top level
                                // parsing).
                                if let Some(r) = call_stack.deepest.get_rule() {
                                    let helper_message = rule_to_message(r);
                                    if let Some(helper_message) = helper_message {
                                        help_lines.push(format!("{spacing}help: {helper_message}"));
                                    }
                                }
                            }
                        }
                    }

                    help_lines.join("\n")
                };
                let error = Error::new_from_pos(
                    ErrorVariant::CustomError { message },
                    Position::new_internal(input, error_position),
                );
                Some(error)
            }

            fn start(&self) -> (usize, usize) {
                match self.line_col {
                    LineColLocation::Pos(line_col) => line_col,
                    LineColLocation::Span(start_line_col, _) => start_line_col,
                }
            }

            fn spacing(&self) -> String {
                let line = match self.line_col {
                    LineColLocation::Pos((line, _)) => line,
                    LineColLocation::Span((start_line, _), (end_line, _)) => cmp::max(start_line, end_line),
                };

                let line_str_len = format!("{}", line).len();

                let mut spacing = String::new();
                for _ in 0..line_str_len {
                    spacing.push(' ');
                }

                spacing
            }

            fn underline(&self) -> String {
                let mut underline = String::new();

                let mut start = self.start().1;
                let end = match self.line_col {
                    LineColLocation::Span(_, (_, mut end)) => {
                        let inverted_cols = start > end;
                        if inverted_cols {
                            mem::swap(&mut start, &mut end);
                            start -= 1;
                            end += 1;
                        }

                        Some(end)
                    }
                    _ => None,
                };
                let offset = start - 1;
                let line_chars = self.line.chars();

                for c in line_chars.take(offset) {
                    match c {
                        '\t' => underline.push('\t'),
                        _ => underline.push(' '),
                    }
                }

                if let Some(end) = end {
                    underline.push('^');
                    if end - start > 1 {
                        for _ in 2..(end - start) {
                            underline.push('-');
                        }
                        underline.push('^');
                    }
                } else {
                    underline.push_str("^---")
                }

                underline
            }

            fn message(&self) -> String {
                self.variant.message().to_string()
            }

            fn parsing_error_message<F>(positives: &[R], negatives: &[R], mut f: F) -> String
            where
                F: FnMut(&R) -> String,
            {
                match (negatives.is_empty(), positives.is_empty()) {
                    (false, false) => format!(
                        "unexpected {}; expected {}",
                        Error::enumerate(negatives, &mut f),
                        Error::enumerate(positives, &mut f)
                    ),
                    (false, true) => format!("unexpected {}", Error::enumerate(negatives, &mut f)),
                    (true, false) => format!("expected {}", Error::enumerate(positives, &mut f)),
                    (true, true) => "unknown parsing error".to_owned(),
                }
            }

            fn enumerate<F>(rules: &[R], f: &mut F) -> String
            where
                F: FnMut(&R) -> String,
            {
                match rules.len() {
                    1 => f(&rules[0]),
                    2 => format!("{} or {}", f(&rules[0]), f(&rules[1])),
                    l => {
                        let non_separated = f(&rules[l - 1]);
                        let separated = rules
                            .iter()
                            .take(l - 1)
                            .map(f)
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!("{}, or {}", separated, non_separated)
                    }
                }
            }

            pub fn format(&self) -> String {
                let spacing = self.spacing();
                let path = self
                    .path
                    .as_ref()
                    .map(|path| format!("{}:", path))
                    .unwrap_or_default();

                let pair = (self.line_col.clone(), &self.continued_line);
                if let (LineColLocation::Span(_, end), Some(ref continued_line)) = pair {
                    let has_line_gap = end.0 - self.start().0 > 1;
                    if has_line_gap {
                        format!(
                            "{s    }--> {p}{ls}:{c}\n\
                            {s    } |\n\
                            {ls:w$} | {line}\n\
                            {s    } | ...\n\
                            {le:w$} | {continued_line}\n\
                            {s    } | {underline}\n\
                            {s    } |\n\
                            {s    } = {message}",
                            s = spacing,
                            w = spacing.len(),
                            p = path,
                            ls = self.start().0,
                            le = end.0,
                            c = self.start().1,
                            line = self.line,
                            continued_line = continued_line,
                            underline = self.underline(),
                            message = self.message()
                        )
                    } else {
                        format!(
                            "{s    }--> {p}{ls}:{c}\n\
                            {s    } |\n\
                            {ls:w$} | {line}\n\
                            {le:w$} | {continued_line}\n\
                            {s    } | {underline}\n\
                            {s    } |\n\
                            {s    } = {message}",
                            s = spacing,
                            w = spacing.len(),
                            p = path,
                            ls = self.start().0,
                            le = end.0,
                            c = self.start().1,
                            line = self.line,
                            continued_line = continued_line,
                            underline = self.underline(),
                            message = self.message()
                        )
                    }
                } else {
                    format!(
                        "{s}--> {p}{l}:{c}\n\
                        {s} |\n\
                        {l} | {line}\n\
                        {s} | {underline}\n\
                        {s} |\n\
                        {s} = {message}",
                        s = spacing,
                        p = path,
                        l = self.start().0,
                        c = self.start().1,
                        line = self.line,
                        underline = self.underline(),
                        message = self.message()
                    )
                }
            }

            #[cfg(feature = "miette-error")]
            /// Turns an error into a [miette](crates.io/miette) Diagnostic.
            pub fn into_miette(self) -> impl ::miette::Diagnostic {
                miette_adapter::MietteAdapter(self)
            }
        }

        impl<R: RuleType> ErrorVariant<R> {
            ///
            /// Returns the error message for [`ErrorVariant`]
            ///
            /// If [`ErrorVariant`] is [`CustomError`], it returns a
            /// [`Cow::Borrowed`] reference to [`message`]. If [`ErrorVariant`] is [`ParsingError`], a
            /// [`Cow::Owned`] containing "expected [ErrorVariant::ParsingError::positives] [ErrorVariant::ParsingError::negatives]" is returned.
            ///
            /// [`ErrorVariant`]: enum.ErrorVariant.html
            /// [`CustomError`]: enum.ErrorVariant.html#variant.CustomError
            /// [`ParsingError`]: enum.ErrorVariant.html#variant.ParsingError
            /// [`Cow::Owned`]: https://doc.rust-lang.org/std/borrow/enum.Cow.html#variant.Owned
            /// [`Cow::Borrowed`]: https://doc.rust-lang.org/std/borrow/enum.Cow.html#variant.Borrowed
            /// [`message`]: enum.ErrorVariant.html#variant.CustomError.field.message
            /// # Examples
            ///
            /// ```
            /// # use pest::error::ErrorVariant;
            /// let variant = ErrorVariant::<()>::CustomError {
            ///     message: String::from("unexpected error")
            /// };
            ///
            /// println!("{}", variant.message());
            pub fn message(&self) -> Cow<'_, str> {
                match self {
                    ErrorVariant::ParsingError {
                        ref positives,
                        ref negatives,
                    } => Cow::Owned(Error::parsing_error_message(positives, negatives, |r| {
                        format!("{:?}", r)
                    })),
                    ErrorVariant::CustomError { ref message } => Cow::Borrowed(message),
                }
            }
        }

        impl<R: RuleType> fmt::Display for Error<R> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.format())
            }
        }

        impl<R: RuleType> fmt::Display for ErrorVariant<R> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    ErrorVariant::ParsingError { .. } => write!(f, "parsing error: {}", self.message()),
                    ErrorVariant::CustomError { .. } => write!(f, "{}", self.message()),
                }
            }
        }

        fn visualize_whitespace(input: &str) -> String {
            input.to_owned().replace('\r', "␍").replace('\n', "␊")
        }

        #[cfg(feature = "miette-error")]
        mod miette_adapter {
            use alloc::string::ToString;
            use core::fmt;
            use std::boxed::Box;

            use crate::error::LineColLocation;

            use super::{Error, RuleType};

            use miette::{Diagnostic, LabeledSpan, SourceCode};

            #[derive( Debug )]
            pub struct MietteAdapter<R: RuleType>(pub Error<R>);

            impl<R: RuleType> Diagnostic for MietteAdapter<R> {
                fn source_code(&self) -> Option<&dyn SourceCode> {
                    Some(&self.0.line)
                }

                fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan>>> {
                    let message = self.0.variant.message().to_string();

                    let (offset, length) = match self.0.line_col {
                        LineColLocation::Pos((_, c)) => (c - 1, 1),
                        LineColLocation::Span((_, start_c), (_, end_c)) => {
                            (start_c - 1, end_c - start_c + 1)
                        }
                    };

                    let span = LabeledSpan::new(Some(message), offset, length);

                    Some(Box::new(std::iter::once(span)))
                }

                fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
                    Some(Box::new(self.0.message()))
                }
            }

            impl<R: RuleType> fmt::Display for MietteAdapter<R> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "Failure to parse at {:?}", self.0.line_col)
                }
            }

            impl<R> core::error::Error for MietteAdapter<R>
            where
                R: RuleType,
                Self: fmt::Debug + fmt::Display,
            {
            }
        }


    }

    pub mod locust
    {
        /*!
        */
        use ::
        {
            types::{ * },
            *,
        };
        /*
        use pest::error::Error;
        use pest::iterators::Pairs;
        use pest::Parser;
        */
        // #[derive(Parser)]
        struct Locust;

        pub fn parse_lines( lines: &str ) -> Result<Pairs<'_, Rule>, Error<Rule>>
        {
            Locust::parse(Rule::EXP, lines)
        }
    }

    pub mod lines
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use regex::Regex;
        use crate::libs;
        use crate::tools;
        use crate::types::{LineInfo, Redirection, Tokens};
        */
        pub fn line_to_plain_tokens(line: &str) -> Vec<String> 
        {
            let mut result = Vec::new();
            let linfo = parse_line(line);
            for (_, r) in linfo.tokens {
                result.push(r.clone());
            }
            result
        }

        pub fn tokens_to_args(tokens: &Tokens) -> Vec<String> 
        {
            let mut result = Vec::new();
            for s in tokens {
                result.push(s.1.clone());
            }
            result
        }

        pub fn tokens_to_line(tokens: &Tokens) -> String 
        {
            let mut result = String::new();
            for t in tokens {
                if t.0.is_empty() {
                    result.push_str(&t.1);
                } else {
                    let s = tools::wrap_sep_string(&t.0, &t.1);
                    result.push_str(&s);
                }
                result.push(' ');
            }
            if result.ends_with(' ') {
                let len = result.len();
                result.truncate(len - 1);
            }
            result
        }
        
        pub fn line_to_cmds(line: &str) -> Vec<String> 
        {
            let mut result = Vec::new();
            let mut sep = String::new();
            let mut token = String::new();
            let mut has_backslash = false;
            let len = line.chars().count();
            for (i, c) in line.chars().enumerate() {
                if has_backslash {
                    token.push('\\');
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                if c == '\\' && sep != "'" {
                    has_backslash = true;
                    continue;
                }

                if c == '#' {
                    if sep.is_empty() {
                        break;
                    } else {
                        token.push(c);
                        continue;
                    }
                }
                if c == '\'' || c == '"' || c == '`' {
                    if sep.is_empty() {
                        sep.push(c);
                        token.push(c);
                        continue;
                    } else if sep == c.to_string() {
                        token.push(c);
                        sep = String::new();
                        continue;
                    } else {
                        token.push(c);
                        continue;
                    }
                }
                if c == '&' || c == '|' {
                    // needs watch ahead here
                    if sep.is_empty() {
                        if i + 1 == len {
                            // for bg commands, e.g. `ls &`
                            token.push(c);
                            continue;
                        } else {
                            let c_next = match line.chars().nth(i + 1) {
                                Some(x) => x,
                                None => {
                                    println!("chars nth error - should never happen");
                                    continue;
                                }
                            };

                            if c_next != c {
                                token.push(c);
                                continue;
                            }
                        }
                    }

                    if sep.is_empty() {
                        sep.push(c);
                        continue;
                    } else if c.to_string() == sep {
                        let _token = token.trim().to_string();
                        if !_token.is_empty() {
                            result.push(_token);
                        }
                        token = String::new();
                        result.push(format!("{}{}", sep, sep));
                        sep = String::new();
                        continue;
                    } else {
                        token.push(c);
                        continue;
                    }
                }
                if c == ';' {
                    if sep.is_empty() {
                        let _token = token.trim().to_string();
                        if !_token.is_empty() {
                            result.push(_token);
                        }
                        result.push(String::from(";"));
                        token = String::new();
                        continue;
                    } else {
                        token.push(c);
                        continue;
                    }
                }
                token.push(c);
            }
            if !token.is_empty() {
                result.push(token.trim().to_string());
            }
            result
        }
        
        pub fn parse_line(line: &str) -> LineInfo 
        {
            let mut result = Vec::new();
            if tools::is_arithmetic(line) {
                for x in line.split(' ') {
                    result.push((String::from(""), x.to_string()));
                }
                return LineInfo::new(result);
            }

            let mut sep = String::new();
            let mut sep_second = String::new();
            let mut token = String::new();
            let mut has_backslash = false;
            let mut met_parenthesis = false;
            let mut new_round = true;
            let mut skip_next = false;
            let mut has_dollar = false;
            let mut parens_left_ignored = false;
            let mut sep_made = String::new();
            let mut semi_ok = false;
            let count_chars = line.chars().count();
            for (i, c) in line.chars().enumerate() {
                if skip_next {
                    skip_next = false;
                    continue;
                }

                if has_backslash && sep.is_empty() && (c == '>' || c == '<') {
                    sep_made = String::from("'");
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                if has_backslash && sep == "\"" && c != '\"' {
                    token.push('\\');
                    token.push(c);
                    has_backslash = false;
                    continue;
                }

                if has_backslash {
                    if new_round && sep.is_empty() && (c == '|' || c == '$') && token.is_empty() {
                        sep = String::from("\\");
                        token = format!("{}", c);
                    } else {
                        token.push(c);
                    }
                    new_round = false;
                    has_backslash = false;
                    continue;
                }

                if c == '$' {
                    has_dollar = true;
                }

                // for cases like: echo $(foo bar)
                if c == '(' && sep.is_empty() {
                    if !has_dollar && token.is_empty() {
                        // temp solution for cmd like `(ls)`, `(ls -lh)`
                        parens_left_ignored = true;
                        continue;
                    }
                    met_parenthesis = true;
                }
                if c == ')' {
                    if parens_left_ignored && !has_dollar {
                        // temp solution for cmd like `(ls)`, `(ls -lh)`
                        if i == count_chars - 1
                            || (i + 1 < count_chars && line.chars().nth(i + 1).unwrap() == ' ')
                        {
                            continue;
                        }
                    }
                    if sep.is_empty() {
                        met_parenthesis = false;
                    }
                }

                if c == '\\' {
                    if sep == "'" || !sep_second.is_empty() {
                        token.push(c)
                    } else {
                        has_backslash = true;
                    }
                    continue;
                }

                if new_round {
                    if c == ' ' {
                        continue;
                    } else if c == '"' || c == '\'' || c == '`' {
                        sep = c.to_string();
                        new_round = false;
                        continue;
                    }

                    sep = String::new();

                    if c == '#' {
                        // handle inline comments
                        break;
                    }

                    if c == '|' {
                        if i + 1 < count_chars && line.chars().nth(i + 1).unwrap() == '|' {
                            result.push((String::from(""), "||".to_string()));
                            skip_next = true;
                        } else {
                            result.push((String::from(""), "|".to_string()));
                        }
                        new_round = true;
                        continue;
                    }

                    token.push(c);
                    new_round = false;
                    continue;
                }

                if c == '|' && !has_backslash {
                    if semi_ok {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push((sep_made.to_string(), token));
                            sep_made = String::new();
                        } else {
                            result.push((sep.to_string(), token));
                        }
                        result.push((String::from(""), "|".to_string()));
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        semi_ok = false;
                        continue;
                    } else if !met_parenthesis && sep_second.is_empty() && sep.is_empty() {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push((sep_made.to_string(), token));
                            sep_made = String::new();
                        } else {
                            result.push((String::from(""), token));
                        }
                        result.push((String::from(""), "|".to_string()));
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        continue;
                    }
                }

                if c == ' ' {
                    if semi_ok {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push((sep_made.to_string(), token));
                            sep_made = String::new();
                        } else {
                            result.push((sep.to_string(), token));
                        }
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        semi_ok = false;
                        continue;
                    }

                    if has_backslash {
                        has_backslash = false;
                        token.push(c);
                        continue;
                    }

                    if met_parenthesis {
                        token.push(c);
                        continue;
                    }

                    if sep == "\\" {
                        result.push((String::from("\\"), token));
                        token = String::new();
                        new_round = true;
                        continue;
                    }

                    if sep.is_empty() {
                        if sep_second.is_empty() {
                            if sep.is_empty() && !sep_made.is_empty() {
                                result.push((sep_made.clone(), token));
                                sep_made = String::new();
                            } else {
                                result.push((String::from(""), token));
                            }
                            token = String::new();
                            new_round = true;
                            continue;
                        } else {
                            token.push(c);
                            continue;
                        }
                    } else {
                        token.push(c);
                        continue;
                    }
                }

                if c == '\'' || c == '"' || c == '`' {
                    if has_backslash {
                        has_backslash = false;
                        token.push(c);
                        continue;
                    }

                    if sep != c.to_string() && semi_ok {
                        if sep.is_empty() && !sep_made.is_empty() {
                            result.push((sep_made.to_string(), token));
                            sep_made = String::new();
                        } else {
                            result.push((sep.to_string(), token));
                        }
                        sep = String::new();
                        sep_second = String::new();
                        token = String::new();
                        new_round = true;
                        semi_ok = false;
                        // do not use continue here!
                    }

                    if sep != c.to_string() && met_parenthesis {
                        token.push(c);
                        continue;
                    }
                    if sep.is_empty() && !sep_second.is_empty() && sep_second != c.to_string() {
                        token.push(c);
                        continue;
                    }

                    if sep.is_empty() {
                        let is_an_env = libs::re::re_contains(&token, r"^[a-zA-Z0-9_]+=.*$");
                        if !is_an_env && (c == '\'' || c == '"') {
                            sep = c.to_string();
                            continue;
                        }

                        token.push(c);
                        if sep_second.is_empty() {
                            sep_second = c.to_string();
                        } else if sep_second == c.to_string() {
                            sep_second = String::new();
                        }
                        continue;
                    } else if sep == c.to_string() {
                        semi_ok = true;
                        continue;
                    } else {
                        token.push(c);
                    }
                } else {
                    if has_backslash {
                        has_backslash = false;
                        if sep == "\"" || sep == "'" {
                            token.push('\\');
                        }
                    }
                    token.push(c);
                }
            }
            if !token.is_empty() || semi_ok {
                if sep.is_empty() && !sep_made.is_empty() {
                    result.push((sep_made.clone(), token));
                } else {
                    result.push((sep.clone(), token));
                }
            }

            let mut is_line_complete = true;
            if !result.is_empty() {
                let token_last = result[result.len() - 1].clone();
                if token_last.0.is_empty() && token_last.1 == "|" {
                    is_line_complete = false;
                }
            }

            if !sep.is_empty() {
                is_line_complete = semi_ok;
            }
            if has_backslash {
                is_line_complete = false;
            }

            LineInfo {
                tokens: result,
                is_complete: is_line_complete,
            }
        }

        pub fn tokens_to_redirections(tokens: &Tokens) -> Result<(Tokens, Vec<Redirection>), String>
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

        pub fn unquote(text: &str) -> String 
        {
            let mut new_str = String::from(text);

            for &c in ['"', '\''].iter() 
            {
                if text.starts_with(c) && text.ends_with(c) 
                {
                    new_str.remove(0);
                    new_str.pop();
                    break;
                }
            }
            new_str
        }
    }

    pub mod pratt
    {
        /*!
        Constructs useful in prefix, postfix, and infix operator parsing with the Pratt parsing method.*/
        use ::
        {
            *,
        };
        /*
        use core::iter::Peekable;
        use core::marker::PhantomData;
        use core::ops::BitOr;

        use alloc::boxed::Box;
        use alloc::collections::BTreeMap;

        use crate::iterators::Pair;
        use crate::RuleType;
        */
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum Assoc
        {
            /// Left operator associativity. Evaluate expressions from left-to-right.
            Left,
            /// Right operator associativity. Evaluate expressions from right-to-left.
            Right,
        }

        type Prec = u32;
        const PREC_STEP: Prec = 10;
        
        pub struct Op<R: RuleType>
        {
            rule: R,
            affix: Affix,
            next: Option<Box<Op<R>>>,
        }

        enum Affix
        {
            Prefix,
            Postfix,
            Infix(Assoc),
        }

        impl<R: RuleType> Op<R> 
        {
            /// Defines `rule` as a prefix unary operator.
            pub fn prefix(rule: R) -> Self {
                Self {
                    rule,
                    affix: Affix::Prefix,
                    next: None,
                }
            }

            /// Defines `rule` as a postfix unary operator.
            pub fn postfix(rule: R) -> Self {
                Self {
                    rule,
                    affix: Affix::Postfix,
                    next: None,
                }
            }

            /// Defines `rule` as an infix binary operator with associativity `assoc`.
            pub fn infix(rule: R, assoc: Assoc) -> Self {
                Self {
                    rule,
                    affix: Affix::Infix(assoc),
                    next: None,
                }
            }
        }

        impl<R: RuleType> BitOr for Op<R> 
        {
            type Output = Self;

            fn bitor(mut self, rhs: Self) -> Self {
                fn assign_next<R: RuleType>(op: &mut Op<R>, next: Op<R>) {
                    if let Some(ref mut child) = op.next {
                        assign_next(child, next);
                    } else {
                        op.next = Some(Box::new(next));
                    }
                }

                assign_next(&mut self, rhs);
                self
            }
        }
        
        pub struct PrattParser<R: RuleType> 
        {
            prec: Prec,
            ops: BTreeMap<R, (Affix, Prec)>,
            has_prefix: bool,
            has_postfix: bool,
            has_infix: bool,
        }

        impl<R: RuleType> Default for PrattParser<R> 
        {
            fn default() -> Self {
                Self::new()
            }
        }

        impl<R: RuleType> PrattParser<R> 
        {
            pub fn new() -> Self 
            {
                Self {
                    prec: PREC_STEP,
                    ops: BTreeMap::new(),
                    has_prefix: false,
                    has_postfix: false,
                    has_infix: false,
                }
            }
            
            pub fn op(mut self, op: Op<R>) -> Self 
            {
                self.prec += PREC_STEP;
                let mut iter = Some(op);
                while let Some(Op { rule, affix, next }) = iter.take() {
                    match affix {
                        Affix::Prefix => self.has_prefix = true,
                        Affix::Postfix => self.has_postfix = true,
                        Affix::Infix(_) => self.has_infix = true,
                    }
                    self.ops.insert(rule, (affix, self.prec));
                    iter = next.map(|op| *op);
                }
                self
            }
            
            pub fn map_primary<'pratt, 'a, 'i, X, T>( &'pratt self, primary: X ) -> PrattParserMap<'pratt, 'a, 'i, R, X, T> where
            X: FnMut(Pair<'i, R>) -> T,
            R: 'pratt
            {
                PrattParserMap
                {
                    pratt: self,
                    primary,
                    prefix: None,
                    postfix: None,
                    infix: None,
                    phantom: PhantomData,
                }
            }
        }

        type PrefixFn<'a, 'i, R, T> = Box<dyn FnMut(Pair<'i, R>, T) -> T + 'a>;
        type PostfixFn<'a, 'i, R, T> = Box<dyn FnMut(T, Pair<'i, R>) -> T + 'a>;
        type InfixFn<'a, 'i, R, T> = Box<dyn FnMut(T, Pair<'i, R>, T) -> T + 'a>;
        
        pub struct PrattParserMap<'pratt, 'a, 'i, R, F, T> where
        R: RuleType,
        F: FnMut(Pair<'i, R>) -> T
        {
            pratt: &'pratt PrattParser<R>,
            primary: F,
            prefix: Option<PrefixFn<'a, 'i, R, T>>,
            postfix: Option<PostfixFn<'a, 'i, R, T>>,
            infix: Option<InfixFn<'a, 'i, R, T>>,
            phantom: PhantomData<T>,
        }

        impl<'pratt, 'a, 'i, R, F, T> PrattParserMap<'pratt, 'a, 'i, R, F, T> where
        R: RuleType + 'pratt,
        F: FnMut(Pair<'i, R>) -> T,
        {
            
            pub fn map_prefix<X>(mut self, prefix: X) -> Self
            where
                X: FnMut(Pair<'i, R>, T) -> T + 'a,
            {
                self.prefix = Some(Box::new(prefix));
                self
            }
            
            pub fn map_postfix<X>(mut self, postfix: X) -> Self
            where
                X: FnMut(T, Pair<'i, R>) -> T + 'a,
            {
                self.postfix = Some(Box::new(postfix));
                self
            }
            
            pub fn map_infix<X>(mut self, infix: X) -> Self
            where
                X: FnMut(T, Pair<'i, R>, T) -> T + 'a,
            {
                self.infix = Some(Box::new(infix));
                self
            }
            
            pub fn parse<P: Iterator<Item = Pair<'i, R>>>(&mut self, pairs: P) -> T
            {
                self.expr(&mut pairs.peekable(), 0)
            }

            fn expr<P: Iterator<Item = Pair<'i, R>>>(&mut self, pairs: &mut Peekable<P>, rbp: Prec) -> T
            {
                let mut lhs = self.nud(pairs);
                while rbp < self.lbp(pairs) {
                    lhs = self.led(pairs, lhs);
                }
                lhs
            }
            
            fn nud<P: Iterator<Item = Pair<'i, R>>>(&mut self, pairs: &mut Peekable<P>) -> T
            {
                let pair = pairs.next().expect("Pratt parsing expects non-empty Pairs");
                match self.pratt.ops.get(&pair.as_rule()) {
                    Some((Affix::Prefix, prec)) => {
                        let rhs = self.expr(pairs, *prec - 1);
                        match self.prefix.as_mut() {
                            Some(prefix) => prefix(pair, rhs),
                            None => panic!("Could not map {}, no `.map_prefix(...)` specified", pair),
                        }
                    }
                    None => (self.primary)(pair),
                    _ => panic!("Expected prefix or primary expression, found {}", pair),
                }
            }
            
            fn led<P: Iterator<Item = Pair<'i, R>>>(&mut self, pairs: &mut Peekable<P>, lhs: T) -> T
            {
                let pair = pairs.next().unwrap();
                match self.pratt.ops.get(&pair.as_rule()) {
                    Some((Affix::Infix(assoc), prec)) => {
                        let rhs = match *assoc {
                            Assoc::Left => self.expr(pairs, *prec),
                            Assoc::Right => self.expr(pairs, *prec - 1),
                        };
                        match self.infix.as_mut() {
                            Some(infix) => infix(lhs, pair, rhs),
                            None => panic!("Could not map {}, no `.map_infix(...)` specified", pair),
                        }
                    }
                    Some((Affix::Postfix, _)) => match self.postfix.as_mut() {
                        Some(postfix) => postfix(lhs, pair),
                        None => panic!("Could not map {}, no `.map_postfix(...)` specified", pair),
                    },
                    _ => panic!("Expected postfix or infix expression, found {}", pair),
                }
            }
            
            fn lbp<P: Iterator<Item = Pair<'i, R>>>(&mut self, pairs: &mut Peekable<P>) -> Prec
            {
                match pairs.peek()
                {
                    Some(pair) => match self.pratt.ops.get(&pair.as_rule())
                    {
                        Some((_, prec)) => *prec,
                        None => panic!("Expected operator, found {}", pair),
                    },
                    None => 0,
                }
            }
        }
    }
    
    pub trait Parser<R: RuleType>
    {
        fn parse(rule: R, input: &str) -> Result<Pairs<'_, R>, Error<R>>;
    }
}

pub mod path
{
    pub use std::path::{ * };
    // pub fn escape_path(path: &str) -> String
    pub fn escape(path: &str) -> String
    {
        let re = Regex::new(r##"(?P<c>[!\(\)<>,\?\]\[\{\} \\'"`*\^#|$&;])"##).unwrap();
        re.replace_all(path, "\\$c").to_string()
    }
}

pub mod process
{
    pub use std::process::{ * };
    pub fn read_pid() -> i32 { unsafe { libc::getpid() } }
}

pub mod prompt
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use crate::libs;
    use crate::shell;

    use self::main::get_prompt_string;
    use self::main::render_prompt;
    pub use self::multilines::EnterFunction;
    */
    pub mod main
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::env;

        use crate::execute;
        use crate::libs;
        use crate::shell;
        */
        const DEFAULT_PROMPT: &str = "${COLOR_STATUS}$USER${RESET}\
            @${COLOR_STATUS}$HOSTNAME${RESET}: \
            ${COLOR_STATUS}$CWD${RESET}$ ";
        use super::preset::apply_preset_item;
        use super::preset::apply_pyenv;

        fn is_prefix_char(c: char) -> bool 
        {
            c == '[' || c == '{'
        }

        fn is_suffix_char(c: char) -> bool 
        {
            c == ']' || c == '}'
        }

        fn is_prompt_item_char(c: char, token: &str) -> bool 
        {
            let s = c.to_string();
            if token.is_empty() {
                libs::re::re_contains(&s, r#"^[a-zA-Z_]$"#)
            } else {
                libs::re::re_contains(&s, r#"^[a-zA-Z0-9_]$"#)
            }
        }

        pub fn get_prompt_string() -> String 
        {
            if let Ok(x) = env::var("PROMPT") {
                return x;
            }
            DEFAULT_PROMPT.to_string()
        }

        fn apply_prompt_item(sh: &shell::Shell, result: &mut String, token: &str) 
        {
            if let Some(x) = sh.get_env(token) {
                result.push_str(&x);
                return;
            }
            apply_preset_item(sh, result, token);
        }

        fn apply_command(result: &mut String, token: &str, prefix: &str, suffix: &str) 
        {
            let cr = execute::run(token);
            let output = cr.stdout.trim();
            if !output.is_empty() {
                result.push_str(prefix);
                result.push_str(output);
                result.push_str(suffix);
            }
        }

        pub fn render_prompt(sh: &shell::Shell, ps: &str) -> String 
        {
            let mut prompt = String::new();
            apply_pyenv(&mut prompt);

            let mut met_dollar = false;
            let mut met_brace = false;
            let mut met_paren = false;
            let mut token = String::new();
            let mut prefix = String::new();
            let mut suffix = String::new();

            for c in ps.chars() 
            {
                if met_dollar {
                    if c == '(' && !met_brace && !met_paren {
                        met_paren = true;
                        continue;
                    }
                    if c == ')' && met_paren {
                        apply_command(&mut prompt, &token, &prefix, &suffix);
                        token.clear();
                        prefix.clear();
                        suffix.clear();
                        met_dollar = false;
                        met_paren = false;
                        continue;
                    }
                    if c == '{' && !met_brace && !met_paren {
                        met_brace = true;
                        continue;
                    } else if c == '}' && met_brace {
                        apply_prompt_item(sh, &mut prompt, &token);
                        token.clear();
                        met_dollar = false;
                        met_brace = false;
                        continue;
                    } else if c == '$' {
                        if token.is_empty() {
                            // to make single $ as a plain $
                            prompt.push('$');
                            met_dollar = true;
                            continue;
                        } else {
                            apply_prompt_item(sh, &mut prompt, &token);
                            token.clear();
                            // met_dollar is still true
                            continue;
                        }
                    } else if met_paren {
                        if is_prefix_char(c) {
                            prefix.push(c);
                        } else if is_suffix_char(c) {
                            suffix.push(c);
                        } else {
                            token.push(c);
                        }
                        continue;
                    } else if is_prompt_item_char(c, &token) {
                        token.push(c);
                        continue;
                    } else if token.is_empty() {
                        prompt.push('$');
                        prompt.push(c);
                        met_dollar = false;
                        continue;
                    }
                }

                if c == '$' {
                    met_dollar = true;
                    continue;
                }

                if !token.is_empty() {
                    apply_prompt_item(sh, &mut prompt, &token);
                    token.clear();
                }
                prompt.push(c);
                met_dollar = false;
            }

            if !token.is_empty() 
            {
                apply_prompt_item(sh, &mut prompt, &token);
                met_dollar = false;
            }

            if met_dollar 
            {
                // for cases like PROMPT='$$'
                prompt.push('$');
            }

            if prompt.trim().is_empty()
            {
                return format!("cicada-{} >> ", ""); // env!("CARGO_PKG_VERSION"));
            }
            
            prompt
        }
    }

    pub mod multilines
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use lineread::{Function, Prompter, Terminal};
        use std::io;

        use crate::parses::lines;
        */
        pub struct EnterFunction;

        impl<T: Terminal> Function<T> for EnterFunction 
        {
            fn execute(&self, prompter: &mut Prompter<T>, count: i32, _ch: char) -> io::Result<()> {
                let buf = prompter.buffer();
                let linfo = parser_line::parse_line(buf);
                if linfo.is_complete {
                    prompter.accept_input()
                } else if count > 0 {
                    match prompter.insert(count as usize, '\n') {
                        Ok(_) => {}
                        Err(e) => {
                            println!("sub-prompt error: {}", e);
                        }
                    }
                    prompter.insert_str(">> ")
                } else {
                    Ok(())
                }
            }
        }
    }

    pub mod preset
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use std::env;
        use std::fs::File;
        use std::io::{Read, Write};
        use std::path::Path;

        use crate::libs;
        use crate::shell;
        use crate::tools;
        */
        fn apply_seq(prompt: &mut String) {
            prompt.push_str(libs::colored::SEQ);
        }

        fn apply_end_seq(prompt: &mut String) {
            prompt.push_str(libs::colored::END_SEQ);
        }

        fn apply_esc(prompt: &mut String) {
            prompt.push_str(libs::colored::ESC);
        }

        fn apply_underlined(prompt: &mut String) {
            prompt.push_str(libs::colored::UNDERLINED);
        }

        fn apply_user(prompt: &mut String) {
            let username = tools::get_user_name();
            prompt.push_str(&username);
        }

        fn apply_black(prompt: &mut String) {
            prompt.push_str(libs::colored::BLACK);
        }

        fn apply_black_b(prompt: &mut String) {
            prompt.push_str(libs::colored::BLACK_B);
        }

        fn apply_black_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::BLACK_BG);
        }

        fn apply_blue(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE);
        }

        fn apply_blue_b(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE_B);
        }

        fn apply_blue_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE_BG);
        }

        fn apply_bold(prompt: &mut String) {
            prompt.push_str(libs::colored::BOLD);
        }

        fn apply_green(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN);
        }

        fn apply_green_b(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN_B);
        }

        fn apply_green_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN_BG);
        }

        fn apply_red(prompt: &mut String) {
            prompt.push_str(libs::colored::RED);
        }

        fn apply_red_b(prompt: &mut String) {
            prompt.push_str(libs::colored::RED_B);
        }

        fn apply_red_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::RED_BG);
        }

        fn apply_white(prompt: &mut String) {
            prompt.push_str(libs::colored::WHITE);
        }

        fn apply_white_b(prompt: &mut String) {
            prompt.push_str(libs::colored::WHITE_B);
        }

        fn apply_white_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::WHITE_BG);
        }

        fn apply_hidden(prompt: &mut String) {
            prompt.push_str(libs::colored::HIDDEN);
        }

        fn apply_reset(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET);
        }

        fn apply_reverse(prompt: &mut String) {
            prompt.push_str(libs::colored::REVERSE);
        }

        fn apply_dim(prompt: &mut String) {
            prompt.push_str(libs::colored::DIM);
        }

        fn apply_blink(prompt: &mut String) {
            prompt.push_str(libs::colored::BLINK);
        }

        fn apply_reset_underlined(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_UNDERLINED);
        }

        fn apply_reset_dim(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_DIM);
        }

        fn apply_reset_reverse(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_REVERSE);
        }

        fn apply_reset_hidden(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_HIDDEN);
        }

        fn apply_reset_blink(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_BLINK);
        }

        fn apply_reset_bold(prompt: &mut String) {
            prompt.push_str(libs::colored::RESET_BOLD);
        }

        fn apply_default(prompt: &mut String) {
            prompt.push_str(libs::colored::DEFAULT);
        }

        fn apply_default_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::DEFAULT_BG);
        }

        fn apply_cyan(prompt: &mut String) {
            prompt.push_str(libs::colored::CYAN);
        }

        fn apply_cyan_l(prompt: &mut String) {
            prompt.push_str(libs::colored::CYAN_L);
        }

        fn apply_cyan_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::CYAN_BG);
        }

        fn apply_cyan_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::CYAN_L_BG);
        }

        fn apply_red_l(prompt: &mut String) {
            prompt.push_str(libs::colored::RED_L);
        }

        fn apply_red_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::RED_L_BG);
        }

        fn apply_green_l(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN_L);
        }

        fn apply_green_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::GREEN_L_BG);
        }

        fn apply_gray_l(prompt: &mut String) {
            prompt.push_str(libs::colored::GRAY_L);
        }

        fn apply_gray_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::GRAY_L_BG);
        }

        fn apply_gray_d(prompt: &mut String) {
            prompt.push_str(libs::colored::GRAY_D);
        }

        fn apply_gray_d_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::GRAY_D_BG);
        }

        fn apply_magenta(prompt: &mut String) {
            prompt.push_str(libs::colored::MAGENTA);
        }

        fn apply_magenta_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::MAGENTA_BG);
        }

        fn apply_magenta_l(prompt: &mut String) {
            prompt.push_str(libs::colored::MAGENTA_L);
        }

        fn apply_magenta_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::MAGENTA_L_BG);
        }

        fn apply_yellow(prompt: &mut String) {
            prompt.push_str(libs::colored::YELLOW);
        }

        fn apply_yellow_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::YELLOW_BG);
        }

        fn apply_yellow_l(prompt: &mut String) {
            prompt.push_str(libs::colored::YELLOW_L);
        }

        fn apply_yellow_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::YELLOW_L_BG);
        }

        fn apply_blue_l(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE_L);
        }

        fn apply_blue_l_bg(prompt: &mut String) {
            prompt.push_str(libs::colored::BLUE_L_BG);
        }

        fn apply_color_status(sh: &shell::Shell, prompt: &mut String) {
            if sh.previous_status == 0 {
                prompt.push_str(libs::colored::GREEN_B);
            } else {
                prompt.push_str(libs::colored::RED_B);
            }
        }

        fn _find_git_root() -> String {
            let current_dir = libs::path::current_dir();
            let dir_git = format!("{}/.git", current_dir);
            if Path::new(&dir_git).exists() {
                return current_dir;
            }

            let mut _dir = current_dir.clone();
            while Path::new(&_dir).parent().is_some() {
                match Path::new(&_dir).parent() {
                    Some(p) => {
                        _dir = p.to_string_lossy().to_string();
                        let dir_git = format!("{}/.git", _dir);
                        if Path::new(&dir_git).exists() {
                            return _dir;
                        }
                    }
                    None => {
                        break;
                    }
                }
            }

            String::new()
        }

        fn apply_gitbr(prompt: &mut String) {
            let git_root = _find_git_root();
            if git_root.is_empty() {
                return;
            }

            let file_head = format!("{}/.git/HEAD", git_root);
            if !Path::new(&file_head).exists() {
                return;
            }

            let mut file;
            match File::open(&file_head) {
                Ok(x) => file = x,
                Err(e) => {
                    println!(":: .git/HEAD err: {:?}", e);
                    return;
                }
            }
            let mut text = String::new();
            match file.read_to_string(&mut text) {
                Ok(_) => {}
                Err(e) => {
                    println!(":: read_to_string error: {:?}", e);
                    return;
                }
            }

            if let Some(branch) = libs::re::find_first_group(r"^[a-z]+: ?[a-z]+/[a-z]+/(.+)$", text.trim())
            {
                apply_blue_b(prompt);
                if let Ok(x) = env::var("CICADA_GITBR_PREFIX") {
                    prompt.push_str(&x);
                }

                let _len_default: i32 = 32;
                let mut len_max = if let Ok(x) = env::var("CICADA_GITBR_MAX_LEN") {
                    match x.parse::<i32>() {
                        Ok(n) => n,
                        Err(_) => _len_default,
                    }
                } else {
                    _len_default
                };
                if len_max <= 0 {
                    len_max = _len_default;
                }

                if branch.len() as i32 <= len_max {
                    prompt.push_str(&branch);
                } else {
                    let len = branch.len() as i32;
                    let offset = (len - len_max + 2) as usize;
                    let branch_short = format!("..{}", &branch[offset..]);
                    prompt.push_str(&branch_short);
                }
                if let Ok(x) = env::var("CICADA_GITBR_SUFFIX") {
                    prompt.push_str(&x);
                }
                apply_reset(prompt);
            }
        }

        fn apply_cwd(prompt: &mut String) {
            let _current_dir = match env::current_dir() {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!(":: PROMPT: env current_dir error: {}", e);
                    return;
                }
            };
            let current_dir = match _current_dir.to_str() {
                Some(x) => x,
                None => {
                    println_stderr!(":: PROMPT: to_str error");
                    return;
                }
            };
            let _tokens: Vec<&str> = current_dir.split('/').collect();

            let last = match _tokens.last() {
                Some(x) => x,
                None => {
                    log!(":: PROMPT: token last error");
                    return;
                }
            };

            let home = tools::get_user_home();
            let pwd = if last.is_empty() {
                "/"
            } else if current_dir == home {
                "~"
            } else {
                last
            };
            prompt.push_str(pwd);
        }

        fn apply_hostname(prompt: &mut String) {
            let hostname = tools::get_hostname();
            prompt.push_str(&hostname);
        }

        fn apply_newline(prompt: &mut String) {
            prompt.push('\n');
        }

        pub fn apply_pyenv(prompt: &mut String) {
            if let Ok(x) = env::var("VIRTUAL_ENV") {
                if !x.is_empty() {
                    let _tokens: Vec<&str> = x.split('/').collect();
                    let env_name = match _tokens.last() {
                        Some(x) => x,
                        None => {
                            log!("prompt token last error");
                            return;
                        }
                    };

                    apply_blue_b(prompt);
                    prompt.push('(');
                    prompt.push_str(env_name);
                    prompt.push(')');
                    apply_reset(prompt);
                }
            }
        }

        pub fn apply_preset_item(sh: &shell::Shell, prompt: &mut String, token: &str) {
            match token.to_ascii_lowercase().as_ref() {
                "black" => apply_black(prompt),
                "black_b" => apply_black_b(prompt),
                "black_bg" => apply_black_bg(prompt),
                "blink" => apply_blink(prompt),
                "blue" => apply_blue(prompt),
                "blue_b" => apply_blue_b(prompt),
                "blue_bg" => apply_blue_bg(prompt),
                "blue_l" => apply_blue_l(prompt),
                "blue_l_bg" => apply_blue_l_bg(prompt),
                "bold" => apply_bold(prompt),
                "color_status" => apply_color_status(sh, prompt),
                "cwd" => apply_cwd(prompt),
                "cyan" => apply_cyan(prompt),
                "cyan_bg" => apply_cyan_bg(prompt),
                "cyan_l" => apply_cyan_l(prompt),
                "cyan_l_bg" => apply_cyan_l_bg(prompt),
                "default" => apply_default(prompt),
                "default_bg" => apply_default_bg(prompt),
                "dim" => apply_dim(prompt),
                "end_seq" => apply_end_seq(prompt),
                "esc" => apply_esc(prompt),
                "gitbr" => apply_gitbr(prompt),
                "gray_d" => apply_gray_d(prompt),
                "gray_d_bg" => apply_gray_d_bg(prompt),
                "gray_l" => apply_gray_l(prompt),
                "gray_l_bg" => apply_gray_l_bg(prompt),
                "green" => apply_green(prompt),
                "green_b" => apply_green_b(prompt),
                "green_bg" => apply_green_bg(prompt),
                "green_l" => apply_green_l(prompt),
                "green_l_bg" => apply_green_l_bg(prompt),
                "hidden" => apply_hidden(prompt),
                "hostname" => apply_hostname(prompt),
                "magenta" => apply_magenta(prompt),
                "magenta_bg" => apply_magenta_bg(prompt),
                "magenta_l" => apply_magenta_l(prompt),
                "magenta_l_bg" => apply_magenta_l_bg(prompt),
                "newline" => apply_newline(prompt),
                "red" => apply_red(prompt),
                "red_b" => apply_red_b(prompt),
                "red_bg" => apply_red_bg(prompt),
                "red_l" => apply_red_l(prompt),
                "red_l_bg" => apply_red_l_bg(prompt),
                "reset" => apply_reset(prompt),
                "reset_blink" => apply_reset_blink(prompt),
                "reset_bold" => apply_reset_bold(prompt),
                "reset_dim" => apply_reset_dim(prompt),
                "reset_hidden" => apply_reset_hidden(prompt),
                "reset_reverse" => apply_reset_reverse(prompt),
                "reset_underlined" => apply_reset_underlined(prompt),
                "reverse" => apply_reverse(prompt),
                "seq" => apply_seq(prompt),
                "underlined" => apply_underlined(prompt),
                "user" => apply_user(prompt),
                "white" => apply_white(prompt),
                "white_b" => apply_white_b(prompt),
                "white_bg" => apply_white_bg(prompt),
                "yellow" => apply_yellow(prompt),
                "yellow_bg" => apply_yellow_bg(prompt),
                "yellow_l" => apply_yellow_l(prompt),
                "yellow_l_bg" => apply_yellow_l_bg(prompt),
                _ => (),
            }
        }
    }

    pub fn get_prompt_len(prompt: &str) -> i32 
    {
        let mut count = 0;
        let mut met_x01 = false;
        for c in prompt.chars() {
            if c == '\x01' {
                met_x01 = true;
                continue;
            } else if c == '\x02' {
                met_x01 = false;
                continue;
            }
            if !met_x01 {
                count += 1;
            }
        }
        count
    }

    pub fn get_prompt(sh: &shell::Shell) -> String 
    {
        let ps = get_prompt_string();
        let mut prompt = render_prompt(sh, &ps);
        if let Some((w, _h)) = libs::term_size::dimensions() {
            if get_prompt_len(&prompt) > (w / 2) as i32
                && !libs::re::re_contains(&ps, r#"(?i)\$\{?newline.\}?"#)
            {
                prompt.push_str("\n$ ");
            }
        } else {
            log!("ERROR: Failed to get term size");
        }
        prompt
    }
}

pub mod rcfile
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::path::Path;

    use crate::scripting;
    use crate::shell;
    use crate::tools;
    */
    pub fn get_rc_file() -> String 
    {
        let dir_config = tools::get_config_dir();
        let rc_file = format!("{}/cicadarc", dir_config);
        if Path::new(&rc_file).exists() {
            return rc_file;
        }

        // fail back to $HOME/.cicadarc
        let home = tools::get_user_home();
        let rc_file_home = format!("{}/{}", home, ".cicadarc");
        if Path::new(&rc_file_home).exists() {
            return rc_file_home;
        }

        // use std path if both absent
        rc_file
    }

    pub fn load_rc_files(sh: &mut shell::Shell) 
    {
        let rc_file = get_rc_file();
        if !Path::new(&rc_file).exists() {
            return;
        }

        let args = vec!["source".to_string(), rc_file];
        scripting::run_script(sh, &args);
    }
}

pub mod regex
{
    /*!
    */
    pub use re::{ * };
    use ::
    {
        *,
    };
    /*
    */
    pub fn unquote(s: &str) -> String
    {
        let args = parses::lines::line_to_plain_tokens(s);
        if args.is_empty() {
            return String::new();
        }
        args[0].clone()
    }
}

pub mod rusqlite
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    pub type BoxedAuthorizer = Box<dyn for<'c> FnMut(AuthContext<'c>) -> Authorization + Send + 'static>;

    #[derive( Debug )]
    pub struct PreUpdateNewValueAccessor
    {
        // db: *mut ffi::sqlite3,
        db: *mut (),
        new_row_id: i64,
    }

    #[derive(Debug)]
    pub struct PreUpdateOldValueAccessor
    {
        //db: *mut ffi::sqlite3,
        db: *mut (),
        old_row_id: i64,
    }

    #[derive(Debug)]
    pub enum PreUpdateCase
    {
        Insert(PreUpdateNewValueAccessor),
        Delete(PreUpdateOldValueAccessor),
        Update
        {
            old_value_accessor: PreUpdateOldValueAccessor,
            new_value_accessor: PreUpdateNewValueAccessor,
        },
        Unknown,
    }

    #[non_exhaustive] #[derive( Clone, Copy, Debug, Eq, PartialEq )]
    pub enum AuthAction<'c>
    {
        Unknown
        {
            code: i32,
            arg1: Option<&'c str>,
            arg2: Option<&'c str>,
        },

        CreateIndex
        {
            index_name: &'c str,
            table_name: &'c str,
        },

        CreateTable
        {
            table_name: &'c str,
        },

        CreateTempIndex
        {
            index_name: &'c str,
            table_name: &'c str,
        },

        CreateTempTable
        {
            table_name: &'c str,
        },

        CreateTempTrigger
        {
            trigger_name: &'c str,
            table_name: &'c str,
        },

        CreateTempView
        {
            view_name: &'c str,
        },

        CreateTrigger
        {
            trigger_name: &'c str,
            table_name: &'c str,
        },

        CreateView
        {
            view_name: &'c str,
        },

        Delete
        {
            table_name: &'c str,
        },

        DropIndex
        {
            index_name: &'c str,
            table_name: &'c str,
        },

        DropTable
        {
            table_name: &'c str,
        },

        DropTempIndex
        {
            index_name: &'c str,
            table_name: &'c str,
        },

        DropTempTable 
        {
            table_name: &'c str,
        },

        DropTempTrigger 
        {
            trigger_name: &'c str,
            table_name: &'c str,
        },

        DropTempView 
        {
            view_name: &'c str,
        },

        DropTrigger 
        {
            trigger_name: &'c str,
            table_name: &'c str,
        },

        DropView 
        {
            view_name: &'c str,
        },

        Insert 
        {
            table_name: &'c str,
        },

        Pragma 
        {
            pragma_name: &'c str,
            pragma_value: Option<&'c str>,
        },

        Read 
        {
            table_name: &'c str,
            column_name: &'c str,
        },

        Select,
        Transaction 
        {
            operation: TransactionOperation,
        },

        Update 
        {
            table_name: &'c str,
            column_name: &'c str,
        },

        Attach 
        {
            filename: &'c str,
        },

        Detach 
        {
            database_name: &'c str,
        },

        AlterTable 
        {
            database_name: &'c str,
            table_name: &'c str,
        },

        Reindex
        {
            index_name: &'c str,
        },

        Analyze
        {
            table_name: &'c str,
        },

        CreateVtable
        {
            table_name: &'c str,
            module_name: &'c str,
        },

        DropVtable
        {
            table_name: &'c str,
            module_name: &'c str,
        },

        Function
        {
            function_name: &'c str,
        },
        
        Savepoint
        {
            operation: TransactionOperation,
            savepoint_name: &'c str,
        },

        Recursive,
    }
    
    #[derive( Clone, Copy, Debug, Eq, PartialEq )]
    pub struct AuthContext<'c>
    {
        pub action: AuthAction<'c>,
        pub database_name: Option<&'c str>,
        pub accessor: Option<&'c str>,
    }

    #[repr( i32 )] #[non_exhaustive] #[derive( Clone, Copy, Debug, Eq, PartialEq )]
    pub enum Action
    {
        UNKNOWN = -1,
        SQLITE_DELETE = 9,  // ffi::SQLITE_DELETE,
        SQLITE_INSERT = 18, // ffi::SQLITE_INSERT,
        SQLITE_UPDATE = 23, // ffi::SQLITE_UPDATE,
    }

    pub struct InnerConnection
    {
        // pub db: *mut ffi::sqlite3,
        pub db:(),
        // interrupt_lock: Arc<Mutex<*mut ffi::sqlite3>>,
        interrupt_lock: Arc<Mutex<*mut ()>>,
        pub commit_hook: Option<Box<dyn FnMut() -> bool + Send>>,
        pub rollback_hook: Option<Box<dyn FnMut() + Send>>,
        pub update_hook: Option<Box<dyn FnMut(Action, &str, &str, i64) + Send>>,
        pub progress_handler: Option<Box<dyn FnMut() -> bool + Send>>,
        pub authorizer: Option<BoxedAuthorizer>,
        pub preupdate_hook: Option<Box<dyn FnMut(Action, &str, &str, &PreUpdateCase) + Send>,>,
        owned: bool,
    }

    pub struct Connection
    {
        db: RefCell<InnerConnection>,
        cache: StatementCache,
        transaction_behavior: TransactionBehavior,
    }
}

pub mod scripts
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::fs::File;
    use std::io::{ErrorKind, Read, Write};
    use std::path::Path;

    use pest::iterators::Pair;
    use regex::{Regex, RegexBuilder};

    use crate::execute;
    use crate::libs;
    use crate::parsers;
    use crate::shell;
    use crate::types;
    use crate::types::CommandResult;
    */
    pub fn run_script(sh: &mut shell::Shell, args: &Vec<String>) -> i32 
    {
        let src_file = &args[1];
        let full_src_file: String;
        if src_file.contains('/') {
            full_src_file = src_file.clone();
        } else {
            let full_path = libs::path::find_file_in_path(src_file, false);
            if full_path.is_empty() {
                // not in PATH and not in current work directory
                if !Path::new(src_file).exists() {
                    println_stderr!(":: {}: no such file", src_file);
                    return 1;
                }
                full_src_file = format!("./{}", src_file);
            } else {
                full_src_file = full_path.clone();
            }
        }

        if !Path::new(&full_src_file).exists() {
            println_stderr!(":: {}: no such file", src_file);
            return 1;
        }
        if Path::new(&full_src_file).is_dir() {
            println_stderr!(":: {}: is a directory", src_file);
            return 1;
        }

        let mut file;
        match File::open(&full_src_file) {
            Ok(x) => file = x,
            Err(e) => {
                println_stderr!(
                    ":: {}: failed to open file - {:?}",
                    &full_src_file,
                    e.kind()
                );
                return 1;
            }
        }
        let mut text = String::new();
        match file.read_to_string(&mut text) {
            Ok(_) => {}
            Err(e) => {
                match e.kind() {
                    ErrorKind::InvalidData => {
                        println_stderr!(":: {}: not a valid script file", &full_src_file);
                    }
                    _ => {
                        println_stderr!(":: {}: error: {:?}", &full_src_file, e);
                    }
                }
                return 1;
            }
        }

        if text.contains("\\\n") {
            let re = RegexBuilder::new(r#"([ \t]*\\\n[ \t]+)|([ \t]+\\\n[ \t]*)"#)
                .multi_line(true)
                .build()
                .unwrap();
            text = re.replace_all(&text, " ").to_string();

            let re = RegexBuilder::new(r#"\\\n"#)
                .multi_line(true)
                .build()
                .unwrap();
            text = re.replace_all(&text, "").to_string();
        }

        let re_func_head =
            Regex::new(r"^function ([a-zA-Z_-][a-zA-Z0-9_-]*) *(?:\(\))? *\{$").unwrap();
        let re_func_tail = Regex::new(r"^\}$").unwrap();
        let mut text_new = String::new();
        let mut enter_func = false;
        let mut func_name = String::new();
        let mut func_body = String::new();
        for line in text.clone().lines() {
            if re_func_head.is_match(line.trim()) {
                enter_func = true;
                let cap = re_func_head.captures(line.trim()).unwrap();
                func_name = cap[1].to_string();
                func_body = String::new();
                continue;
            }
            if re_func_tail.is_match(line.trim()) {
                sh.set_func(&func_name, &func_body);
                enter_func = false;
                continue;
            }
            if enter_func {
                func_body.push_str(line);
                func_body.push('\n');
            } else {
                text_new.push_str(line);
                text_new.push('\n');
            }
        }

        let mut status = 0;
        let cr_list = run_lines(sh, &text_new, args, false);
        if let Some(last) = cr_list.last() {
            status = last.status;
        }

        // FIXME: We probably need to fix the issue in the `set` builtin,
        // which currently set `exit_on_error` at the shell session level,
        // we should instead set in a script-level.
        // Here is a work-around ugly fix.
        sh.exit_on_error = false;

        status
    }

    pub fn run_lines(
        sh: &mut shell::Shell,
        lines: &str,
        args: &Vec<String>,
        capture: bool,
    ) -> Vec<CommandResult> 
    {
        let mut cr_list = Vec::new();
        match parsers::locust::parse_lines(lines) {
            Ok(pairs_exp) => {
                for pair in pairs_exp {
                    let (mut _cr_list, _cont, _brk) = run_exp(sh, pair, args, false, capture);
                    cr_list.append(&mut _cr_list);
                }
            }
            Err(e) => {
                println_stderr!("syntax error: {:?}", e);
                return cr_list;
            }
        }
        cr_list
    }

    fn expand_args(line: &str, args: &[String]) -> String 
    {
        let linfo = parses::lines::parse_line(line);
        let mut tokens = linfo.tokens;
        expand_args_in_tokens(&mut tokens, args);
        parses::lines::tokens_to_line(&tokens)
    }

    fn expand_line_to_toknes(line: &str, args: &[String], sh: &mut shell::Shell) -> types::Tokens 
    {
        let linfo = parses::lines::parse_line(line);
        let mut tokens = linfo.tokens;
        expand_args_in_tokens(&mut tokens, args);
        shell::do_expansion(sh, &mut tokens);
        tokens
    }

    fn is_args_in_token(token: &str) -> bool 
    {
        libs::re::re_contains(token, r"\$\{?[0-9@]+\}?")
    }

    fn expand_args_for_single_token(token: &str, args: &[String]) -> String 
    {
        let re = Regex::new(r"^(.*?)\$\{?([0-9]+|@)\}?(.*)$").unwrap();
        if !re.is_match(token) {
            return token.to_string();
        }

        let mut result = String::new();
        let mut _token = token.to_string();
        let mut _head = String::new();
        let mut _output = String::new();
        let mut _tail = String::new();
        loop {
            if !re.is_match(&_token) {
                if !_token.is_empty() {
                    result.push_str(&_token);
                }
                break;
            }
            for cap in re.captures_iter(&_token) {
                _head = cap[1].to_string();
                _tail = cap[3].to_string();
                let _key = cap[2].to_string();
                if _key == "@" {
                    result.push_str(format!("{}{}", _head, args[1..].join(" ")).as_str());
                } else if let Ok(arg_idx) = _key.parse::<usize>() {
                    if arg_idx < args.len() {
                        result.push_str(format!("{}{}", _head, args[arg_idx]).as_str());
                    } else {
                        result.push_str(&_head);
                    }
                } else {
                    result.push_str(&_head);
                }
            }

            if _tail.is_empty() {
                break;
            }
            _token = _tail.clone();
        }
        result
    }

    fn expand_args_in_tokens(tokens: &mut types::Tokens, args: &[String]) 
    {
        let mut idx: usize = 0;
        let mut buff = Vec::new();

        for (sep, token) in tokens.iter() {
            if sep == "`" || sep == "'" || !is_args_in_token(token) {
                idx += 1;
                continue;
            }

            let _token = expand_args_for_single_token(token, args);
            buff.push((idx, _token));
            idx += 1;
        }

        for (i, text) in buff.iter().rev() {
            tokens[*i].1 = text.to_string();
        }
    }

    fn run_exp_test_br
    (
        sh: &mut shell::Shell,
        pair_br: Pair<parsers::locust::Rule>,
        args: &Vec<String>,
        in_loop: bool,
        capture: bool,
    ) -> (Vec<CommandResult>, bool, bool, bool) 
    {
        let mut cr_list = Vec::new();
        let pairs = pair_br.into_inner();
        let mut test_pass = false;
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::IF_HEAD
                || rule == parsers::locust::Rule::IF_ELSEIF_HEAD
                || rule == parsers::locust::Rule::WHILE_HEAD
            {
                let pairs_test: Vec<Pair<parsers::locust::Rule>> = pair.into_inner().collect();
                let pair_test = &pairs_test[0];
                let line = pair_test.as_str().trim();
                let line_new = expand_args(line, &args[1..]);
                let mut _cr_list = execute::run_command_line(sh, &line_new, true, capture);
                if let Some(last) = _cr_list.last() {
                    if last.status == 0 {
                        test_pass = true;
                    }
                }
                continue;
            }

            if rule == parsers::locust::Rule::KW_ELSE {
                test_pass = true;
                continue;
            }

            if rule == parsers::locust::Rule::EXP_BODY {
                if !test_pass {
                    return (cr_list, false, false, false);
                }
                let (mut _cr_list, _cont, _brk) = run_exp(sh, pair, args, in_loop, capture);
                cr_list.append(&mut _cr_list);
                // branch executed successfully
                return (cr_list, true, _cont, _brk);
            }

            unreachable!();
        }
        (cr_list, test_pass, false, false)
    }

    fn run_exp_if
    (
        sh: &mut shell::Shell,
        pair_if: Pair<parsers::locust::Rule>,
        args: &Vec<String>,
        in_loop: bool,
        capture: bool,
    ) -> (Vec<CommandResult>, bool, bool) 
    {
        let mut cr_list = Vec::new();
        let pairs = pair_if.into_inner();
        let mut met_continue = false;
        let mut met_break = false;
        for pair in pairs {
            let (mut _cr_list, passed, _cont, _brk) =
                run_exp_test_br(sh, pair, args, in_loop, capture);
            met_continue = _cont;
            met_break = _brk;
            cr_list.append(&mut _cr_list);
            // break at first successful branch
            if passed {
                break;
            }
        }
        (cr_list, met_continue, met_break)
    }

    fn get_for_result_from_init
    (
        sh: &mut shell::Shell,
        pair_init: Pair<parsers::locust::Rule>,
        args: &[String],
    ) -> Vec<String> 
    {
        let mut result: Vec<String> = Vec::new();
        let pairs = pair_init.into_inner();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::TEST {
                let line = pair.as_str().trim();
                let tokens = expand_line_to_toknes(line, &args[1..], sh);
                for (sep, token) in tokens {
                    if sep.is_empty() {
                        for x in token.split_whitespace() {
                            result.push(x.to_string());
                        }
                    } else {
                        result.push(token.clone());
                    }
                }
            }
        }
        result
    }

    fn get_for_result_list
    (
        sh: &mut shell::Shell,
        pair_head: Pair<parsers::locust::Rule>,
        args: &[String],
    ) -> Vec<String> 
    {
        let pairs = pair_head.into_inner();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_INIT {
                return get_for_result_from_init(sh, pair, args);
            }
        }
        Vec::new()
    }

    fn get_for_var_name(pair_head: Pair<parsers::locust::Rule>) -> String 
    {
        let pairs = pair_head.into_inner();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_INIT {
                let pairs_init = pair.into_inner();
                for pair_init in pairs_init {
                    let rule_init = pair_init.as_rule();
                    if rule_init == parsers::locust::Rule::FOR_VAR {
                        let line = pair_init.as_str().trim();
                        return line.to_string();
                    }
                }
            }
        }
        String::new()
    }

    fn run_exp_for
    (
        sh: &mut shell::Shell,
        pair_for: Pair<parsers::locust::Rule>,
        args: &Vec<String>,
        capture: bool,
    ) -> Vec<CommandResult> 
    {
        let mut cr_list = Vec::new();
        let pairs = pair_for.into_inner();
        let mut result_list: Vec<String> = Vec::new();
        let mut var_name: String = String::new();
        for pair in pairs {
            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::FOR_HEAD {
                var_name = get_for_var_name(pair.clone());
                result_list = get_for_result_list(sh, pair.clone(), args);
                continue;
            }
            if rule == parsers::locust::Rule::EXP_BODY {
                for value in &result_list {
                    sh.set_env(&var_name, value);
                    let (mut _cr_list, _cont, _brk) = run_exp(sh, pair.clone(), args, true, capture);
                    cr_list.append(&mut _cr_list);
                    if _brk {
                        break;
                    }
                }
            }
        }
        cr_list
    }

    fn run_exp_while
    (
        sh: &mut shell::Shell,
        pair_while: Pair<parsers::locust::Rule>,
        args: &Vec<String>,
        capture: bool,
    ) -> Vec<CommandResult> 
    {
        let mut cr_list = Vec::new();
        loop {
            let (mut _cr_list, passed, _cont, _brk) =
                run_exp_test_br(sh, pair_while.clone(), args, true, capture);
            cr_list.append(&mut _cr_list);
            if !passed || _brk {
                break;
            }
        }
        cr_list
    }

    fn run_exp
    (
        sh: &mut shell::Shell,
        pair_in: Pair<parsers::locust::Rule>,
        args: &Vec<String>,
        in_loop: bool,
        capture: bool,
    ) -> (Vec<CommandResult>, bool, bool) 
    {
        let mut cr_list = Vec::new();
        let pairs = pair_in.into_inner();
        for pair in pairs {
            let line = pair.as_str().trim();
            if line.is_empty() {
                continue;
            }

            let rule = pair.as_rule();
            if rule == parsers::locust::Rule::CMD {
                if line == "continue" {
                    if in_loop {
                        return (cr_list, true, false);
                    } else {
                        println_stderr!(":: continue: only meaningful in loops");
                        continue;
                    }
                }
                if line == "break" {
                    if in_loop {
                        return (cr_list, false, true);
                    } else {
                        println_stderr!(":: break: only meaningful in loops");
                        continue;
                    }
                }

                let line_new = expand_args(line, &args[1..]);
                let mut _cr_list = execute::run_command_line(sh, &line_new, true, capture);
                cr_list.append(&mut _cr_list);
                if let Some(last) = cr_list.last() {
                    let status = last.status;
                    if status != 0 && sh.exit_on_error {
                        return (cr_list, false, false);
                    }
                }
            } else if rule == parsers::locust::Rule::EXP_IF {
                let (mut _cr_list, _cont, _brk) = run_exp_if(sh, pair, args, in_loop, capture);
                cr_list.append(&mut _cr_list);
                if _cont {
                    return (cr_list, true, false);
                }
                if _brk {
                    return (cr_list, false, true);
                }
            } else if rule == parsers::locust::Rule::EXP_FOR {
                let mut _cr_list = run_exp_for(sh, pair, args, capture);
                cr_list.append(&mut _cr_list);
            } else if rule == parsers::locust::Rule::EXP_WHILE {
                let mut _cr_list = run_exp_while(sh, pair, args, capture);
                cr_list.append(&mut _cr_list);
            }
        }
        (cr_list, false, false)
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
    use std::path::{Path, PathBuf};

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
        pub fn new() -> Shell 
        {
            let uuid = Uuid::new_v4().as_hyphenated().to_string();
            let current_dir = tools::get_current_dir();
            // TODO: the shell proc may have terminal later
            // e.g. $ cicada foo.sh &
            // then with a $ fg
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

        pub fn insert_job(&mut self, gid: i32, pid: i32, cmd: &str, status: &str, bg: bool)
        {
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

        pub fn get_job_by_id(&self, job_id: i32) -> Option<&types::Job> 
        {
            self.jobs.get(&job_id)
        }

        pub fn mark_job_member_continued(&mut self, pid: i32, gid: i32) -> Option<&types::Job>
        {
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

        pub fn mark_job_member_stopped(&mut self, pid: i32, gid: i32) -> Option<&types::Job>
        {
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

        pub fn get_job_by_gid(&self, gid: i32) -> Option<&types::Job> 
        {
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

        pub fn mark_job_as_running(&mut self, gid: i32, bg: bool) 
        {
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

        pub fn mark_job_as_stopped(&mut self, gid: i32) 
        {
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

        pub fn remove_pid_from_job(&mut self, gid: i32, pid: i32) -> Option<types::Job>
        {
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
        
        pub fn set_env(&mut self, name: &str, value: &str) 
        {
            if env::var(name).is_ok() {
                env::set_var(name, value);
            } else {
                self.envs.insert(name.to_string(), value.to_string());
            }
        }
        
        pub fn get_env(&self, name: &str) -> Option<String> 
        {
            match self.envs.get(name) {
                Some(x) => Some(x.to_string()),
                None => env::var(name).ok(),
            }
        }
        
        pub fn remove_env(&mut self, name: &str) -> bool 
        {
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

        pub fn remove_path(&mut self, path_to_remove: &Path) 
        {
            if let Ok(paths) = env::var("PATH") {
                let mut paths_new: Vec<PathBuf> = env::split_paths(&paths).collect();
                paths_new.retain(|x| x != path_to_remove);
                let joined = env::join_paths(paths_new).unwrap_or_default();
                env::set_var("PATH", joined);
            }
        }

        fn remove_func(&mut self, name: &str) 
        {
            self.funcs.remove(name);
        }

        pub fn set_func(&mut self, name: &str, value: &str) 
        {
            self.funcs.insert(name.to_string(), value.to_string());
        }

        pub fn get_func(&self, name: &str) -> Option<String> 
        {
            self.funcs.get(name).map(|x| x.to_string())
        }

        pub fn get_alias_list(&self) -> Vec<(String, String)>
        
        {
            let mut result = Vec::new();
            for (name, value) in &self.aliases {
                result.push((name.clone(), value.clone()));
            }
            result
        }

        pub fn add_alias(&mut self, name: &str, value: &str)
        {
            self.aliases.insert(name.to_string(), value.to_string());
        }

        pub fn is_alias(&self, name: &str) -> bool
        {
            self.aliases.contains_key(name)
        }

        pub fn remove_alias(&mut self, name: &str) -> bool
        {
            let opt = self.aliases.remove(name);
            opt.is_some()
        }

        pub fn get_alias_content(&self, name: &str) -> Option<String>
        {
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
        
        pub fn split_into_fields
        (
            &self,
            line: &str,
            envs: &HashMap<String, String>,
        ) -> Vec<String>
        {
            let ifs_chars;
            if envs.contains_key("IFS") {
                ifs_chars = envs[&"IFS".to_string()].chars().collect();
            } else if let Some(x) = self.get_env("IFS") {
                ifs_chars = x.chars().collect();
            } else if let Ok(x) = env::var("IFS") {
                ifs_chars = x.chars().collect();
            } else {
                ifs_chars = vec![];
            }

            if ifs_chars.is_empty() {
                line.split(&[' ', '\t', '\n'][..])
                    .map(|x| x.to_string())
                    .collect()
            } else {
                line.split(&ifs_chars[..]).map(|x| x.to_string()).collect()
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

    fn needs_globbing(line: &str) -> bool 
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

    fn expand_one_env(sh: &Shell, token: &str) -> String 
    {
        // do not combine these two into one: `\{?..\}?`,
        // otherwize `}` in `{print $NF}` would gone.
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

    fn need_expand_brace(line: &str) -> bool 
    {
        libs::re::re_contains(line, r#"\{[^ "']*,[^ "']*,?[^ "']*\}"#)
    }

    fn brace_getitem(s: &str, depth: i32) -> (Vec<String>, String) 
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

    fn brace_getgroup(s: &str, depth: i32) -> Option<(Vec<String>, String)> 
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

    fn expand_brace(tokens: &mut types::Tokens)
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

    fn expand_brace_range(tokens: &mut types::Tokens)
    {
        let re;
        if let Ok(x) = Regex::new(r#"\{(-?[0-9]+)\.\.(-?[0-9]+)(\.\.)?([0-9]+)?\}"#) {
            re = x;
        } else {
            println_stderr!(":: re new error");
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
                    println_stderr!(":: {}", e);
                    return;
                }
            };

            let end = match caps[2].to_string().parse::<i32>() {
                Ok(x) => x,
                Err(e) => {
                    println_stderr!(":: {}", e);
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
                        println_stderr!(":: {}", e);
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

    fn expand_alias(sh: &Shell, tokens: &mut types::Tokens) 
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
            let linfo = parses::lines::parse_line(text);
            let tokens_ = linfo.tokens;
            tokens.remove(*i);
            for item in tokens_.iter().rev() {
                tokens.insert(*i, item.clone());
            }
        }
    }

    fn expand_home(tokens: &mut types::Tokens) 
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

    fn env_in_token(token: &str) -> bool 
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

    fn should_do_dollar_command_extension(line: &str) -> bool 
    {
        libs::re::re_contains(line, r"\$\([^\)]+\)")
            && !libs::re::re_contains(line, r"='.*\$\([^\)]+\).*'$")
    }

    fn do_command_substitution_for_dollar(sh: &mut Shell, tokens: &mut types::Tokens) 
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
                        println_stderr!(":: calculator: no first group");
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
                        println_stderr!(":: {}", e);
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

    fn do_command_substitution_for_dot(sh: &mut Shell, tokens: &mut types::Tokens) 
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
                        println_stderr!(":: {}", e);
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
                                println_stderr!(":: {}", e);
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

    fn do_command_substitution(sh: &mut Shell, tokens: &mut types::Tokens) 
    {
        do_command_substitution_for_dot(sh, tokens);
        do_command_substitution_for_dollar(sh, tokens);
    }

    pub fn do_expansion(sh: &mut Shell, tokens: &mut types::Tokens) 
    {
        let line = parses::lines::tokens_to_line(tokens);
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
        // remove sub-prompts from multiple line mode
        // 1. assuming '\n' char cannot be typed manually?
        // 2. `>>` is defined as `src/prompt/multilines.rs`
        let line_new = libs::re::replace_all(line, r"\\\n>> ", "");
        let line_new = libs::re::replace_all(&line_new, r"\| *\n>> ", "| ");
        libs::re::replace_all(&line_new, r"(?P<NEWLINE>\n)>> ", "$NEWLINE")
    }

    fn proc_has_terminal() -> bool 
    {
        unsafe {
            let tgid = libc::tcgetpgrp(0);
            let pgid = libc::getpgid(0);
            tgid == pgid
        }
    }
}

pub mod signals
{
    /*!
    */
    use ::
    {
        error::no::{ errno, set_errno },
        nix::
        {
            sys::
            {
                wait::{ waitpid, WaitPidFlag as WF, WaitStatus as WS },
                signal
            },
            unistd::{ Pid },
        },
        sync::{ Mutex },
        *,
    };
    /*
    */
    lazy_static! 
    {
        static ref REAP_MAP: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
        static ref STOP_MAP: Mutex<HashSet<i32>> = Mutex::new(HashSet::new());
        static ref CONT_MAP: Mutex<HashSet<i32>> = Mutex::new(HashSet::new());
        static ref KILL_MAP: Mutex<HashMap<i32, i32>> = Mutex::new(HashMap::new());
    }

    pub fn killed_map_insert(pid: i32, sig: i32)
    {
        if let Ok(mut m) = KILL_MAP.try_lock() {
            m.insert(pid, sig);
        }
    }

    pub fn killed_map_pop(pid: i32) -> Option<i32>
    {
        if let Ok(mut m) = KILL_MAP.try_lock() {
            m.remove(&pid)
        } else {
            None
        }
    }

    pub fn insert_cont_map(pid: i32)
    {
        if let Ok(mut m) = CONT_MAP.try_lock() {
            m.insert(pid);
        }
    }

    pub fn pop_cont_map(pid: i32) -> bool
    {
        match CONT_MAP.try_lock() {
            Ok(mut m) => m.remove(&pid),
            Err(_) => false,
        }
    }

    pub fn insert_stopped_map(pid: i32)
    {
        if let Ok(mut m) = STOP_MAP.try_lock() {
            m.insert(pid);
        }
    }

    pub fn pop_stopped_map(pid: i32) -> bool
    {
        match STOP_MAP.try_lock() {
            Ok(mut m) => m.remove(&pid),
            Err(_) => false,
        }
    }

    pub fn insert_reap_map(pid: i32, status: i32)
    {
        if let Ok(mut m) = REAP_MAP.try_lock() {
            m.insert(pid, status);
        }
    }

    pub fn pop_reap_map(pid: i32) -> Option<i32>
    {
        match REAP_MAP.try_lock() {
            Ok(mut m) => m.remove(&pid),
            Err(_) => None,
        }
    }

    pub fn block_signals()
    {
        let mut sigset = signal::SigSet::empty();
        sigset.add(signal::SIGCHLD);
        match signal::sigprocmask(signal::SigmaskHow::SIG_BLOCK, Some(&sigset), None) {
            Ok(_) => {}
            Err(e) => {
                log!("sigprocmask block error: {:?}", e);
            }
        }
    }

    pub fn unblock_signals()
    {
        let mut sigset = signal::SigSet::empty();
        sigset.add(signal::SIGCHLD);
        match signal::sigprocmask(signal::SigmaskHow::SIG_UNBLOCK, Some(&sigset), None) {
            Ok(_) => {}
            Err(e) => {
                log!("sigprocmask unblock error: {:?}", e);
            }
        }
    }
    
    pub extern "C" fn handle_sigchld(_sig: i32) 
    {
        let saved_errno = errno();
        let options = Some(WF::WUNTRACED | WF::WNOHANG | WF::WCONTINUED);

        loop
        {
            match waitpid(Pid::from_raw(-1), options)
            {
                Ok(WS::Exited(pid, status)) =>
                {
                    insert_reap_map(i32::from(pid), status);
                }

                Ok(WS::Stopped(pid, _sig)) =>
                {
                    insert_stopped_map(i32::from(pid));
                }

                Ok(WS::Continued(pid)) =>
                {
                    // NOTE: SIGCHLD generated by SIGCONT is not reliable
                    // on Mac (both for signal handler or sync waitpid).
                    insert_cont_map(i32::from(pid));
                }

                Ok(WS::Signaled(pid, sig, _core_dumped)) =>
                {
                    killed_map_insert(i32::from(pid), sig as i32);
                }

                Ok(WS::StillAlive) =>
                {
                    break;
                }

                Ok(_others) =>
                {
                    // log!("sigchld others: {:?}", _others);
                }

                Err(e) =>
                {
                    if e == nix::Error::ECHILD {
                        break;
                    }

                    log!("chld waitpid error: {:?}", e);
                    break;
                }
            }
        }

        set_errno(saved_errno);
    }

    pub fn setup_sigchld_handler()
    {
        let sigset = signal::SigSet::empty();
        let handler = signal::SigHandler::Handler(handle_sigchld);
        // automatically restart system calls interrupted by this signal handler
        let flags = signal::SaFlags::SA_RESTART;
        let sa = signal::SigAction::new(handler, flags, sigset);
        unsafe {
            match signal::sigaction(signal::SIGCHLD, &sa) {
                Ok(_) => {}
                Err(e) => {
                    log!("sigaction error: {:?}", e);
                }
            }
        }
    }
}

pub mod str
{
    pub use std::str::{ * };
    // pub fn wrap_sep_string(sep: &str, s: &str) -> String
    pub fn wrap_separator_str(sep: &str, s: &str) -> String
    {
        let mut _token = String::new();
        let mut met_subsep = false;
        // let set previous_subsep to any char except '`' or '"'
        let mut previous_subsep = 'N';
        for c in s.chars() {
            // handle cmds like: export DIR=`brew --prefix openssl`/include
            // or like: export foo="hello world"
            if sep.is_empty() && (c == '`' || c == '"') {
                if !met_subsep {
                    met_subsep = true;
                    previous_subsep = c;
                } else if c == previous_subsep {
                    met_subsep = false;
                    previous_subsep = 'N';
                }
            }
            if c.to_string() == sep {
                _token.push('\\');
            }
            if c == ' ' && sep.is_empty() && !met_subsep {
                _token.push('\\');
            }
            _token.push(c);
        }
        format!("{}{}{}", sep, _token, sep)
    }
}

pub mod sync
{
    pub use std::sync::{ * };
}

pub mod types
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use regex::Regex;
    use std::collections::{HashMap, HashSet};
    use std::fmt;

    use crate::libs;
    use crate::parsers;
    use crate::parses::lines::tokens_to_redirections;
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
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

    #[derive( Debug )]
    pub struct LineInfo 
    {
        // e.g. echo 'foo
        // is not a completed line, need to turn to multiple-line mode.
        pub tokens: Tokens,
        pub is_complete: bool,
    }

    impl LineInfo 
    {
        pub fn new(tokens: Tokens) -> Self {
            LineInfo {
                tokens,
                is_complete: true,
            }
        }
    }
    
    #[derive( Debug )]
    pub struct Command 
    {
        pub tokens: Tokens,
        pub redirects_to: Vec<Redirection>,
        pub redirect_from: Option<Token>,
    }

    #[derive( Debug )]
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

    #[derive( Clone, Debug, Default )]
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
        pub fn all_members_stopped(&self) -> bool {
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
    
    #[derive( Clone, Debug, Default )]
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
    
    #[derive( Clone, Debug, Default )]
    pub struct CommandOptions 
    {
        pub background: bool,
        pub isatty: bool,
        pub capture_output: bool,
        pub envs: HashMap<String, String>,
    }

    fn split_tokens_by_pipes(tokens: &[Token]) -> Vec<Tokens> 
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

    pub fn drain_env_tokens(tokens: &mut Tokens) -> HashMap<String, String> 
    {
        let mut envs: HashMap<String, String> = HashMap::new();
        let mut n = 0;
        let re = Regex::new(r"^([a-zA-Z0-9_]+)=(.*)$").unwrap();
        for (sep, text) in tokens.iter() {
            if !sep.is_empty() || !libs::re::re_contains(text, r"^([a-zA-Z0-9_]+)=(.*)$") {
                break;
            }

            for cap in re.captures_iter(text) {
                let name = cap[1].to_string();
                let value = parses::lines::unquote(&cap[2]);
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
            let linfo = parses::lines::parse_line(line);
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

        pub fn with_pipeline(&self) -> bool {
            self.commands.len() > 1
        }

        pub fn is_single_and_builtin(&self) -> bool {
            self.commands.len() == 1 && self.commands[0].is_builtin()
        }
    }

    #[derive( Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash )]
    pub enum Rule
    {
        WHITESPACE( String ),
        KW_IF( String ),
        KW_FI( String ),
        KW_FOR( String ),
        KW_ELSE( String ),
        KW_ELSEIF( String ),
        KW_WHILE( String ),
        KW_DONE( String ),
        KW_LIST( String ),
        DUMMY_DO( String ),
        DUMMY_THEN( String ),
        TEST( String ),
        CMD_END( String ),
        CMD_NORMAL( String ),
        CMD( String ),
        IF_HEAD( String ),
        EXP_BODY( String ),
        IF_ELSEIF_HEAD( String ),
        IF_IF_BR( String ),
        IF_ELSEIF_BR( String ),
        IF_ELSE_BR( String ),
        EXP_IF( String ),
        FOR_VAR( String ),
        FOR_INIT( String ),
        FOR_HEAD( String ),
        EXP_FOR( String ),
        WHILE_HEAD( String ),
        EXP_WHILE( String ),
        EXP( String ),
    }
}

pub fn main() -> Result<(), error::parse::ParseError>
{
    unsafe
    {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
        libc::signal(libc::SIGTSTP, libc::SIG_IGN);
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);

        env::initialize_paths();

        let mut sh = shell::Shell::new();
        let args: Vec<String> = env::args().collect();

        if libs::progopts::is_login(&args)
        {
            rcfile::load_rc_files(&mut sh);
            sh.is_login = true;
        }
        
        highlight::init_command_cache();
        highlight::update_aliases(&sh);

        if libs::progopts::is_script(&args)
        {
            log!("run script: {:?} ", &args);
            let status = scripting::run_script(&mut sh, &args);
            std::process::exit(status);
        }

        if libs::progopts::is_command_string(&args)
        {
            let line = tools::env_args_to_command_line();
            log!("run with -c args: {}", &line);
            execute::run_command_line(&mut sh, &line, false, false);
            std::process::exit(sh.previous_status);
        }

        if libs::progopts::is_non_tty()
        {
            execute::run_procs_for_non_tty(&mut sh);
            return;
        }

        let mut rl;
        match Interface::new("cicada")
        {
            Ok(x) => rl = x,
            Err(e) =>
            {
                println!(":: lineread error: {}", e);
                return;
            }
        }

        rl.define_function("enter-function", Arc::new(prompt::EnterFunction));
        rl.bind_sequence("\r", Command::from_str("enter-function"));
        let highlighter = highlight::create_highlighter();
        rl.set_highlighter(highlighter);
        history::init(&mut rl);
        rl.set_completer(Arc::new(completers::CicadaCompleter { sh: Arc::new(sh.clone()) }));

        let sig_handler_enabled = tools::is_signal_handler_enabled();
        
        if sig_handler_enabled
        {
            signals::setup_sigchld_handler();
            signals::block_signals();
        }

        loop
        {
            let prompt = prompt::get_prompt(&sh);
            match rl.set_prompt(&prompt)
            {
                Ok(_) => {}
                Err(e) => { println_stderr!(":: prompt error: {}", e); }
            }

            if sig_handler_enabled { signals::unblock_signals(); }
            
            match rl.read_line()
            {
                Ok(ReadResult::Input(line)) =>
                {
                    if sig_handler_enabled { signals::block_signals(); }

                    let line = shell::trim_multiline_prompts(&line);
                    
                    if line.trim() == ""
                    {
                        jobc::try_wait_bg_jobs(&mut sh, true, sig_handler_enabled);
                        continue;
                    }

                    sh.cmd = line.clone();

                    let tsb = ctime::DateTime::now().unix_timestamp();
                    let mut line = line.clone();                
                    tools::extend_bangbang(&sh, &mut line);

                    let mut status = 0;
                    let cr_list = execute::run_command_line(&mut sh, &line, true, false);

                    if let Some(last) = cr_list.last() { status = last.status; }

                    let tse = ctime::DateTime::now().unix_timestamp();

                    if !sh.cmd.starts_with(' ') && line != sh.previous_cmd
                    {
                        history::add(&sh, &mut rl, &line, status, tsb, tse);
                        sh.previous_cmd = line.clone();
                    }

                    if tools::is_shell_altering_command(&line)
                    {
                        rl.set_completer
                        (
                            Arc::new
                            (
                                completers::CicadaCompleter
                                {
                                    sh: Arc::new(sh.clone()),
                                }
                            )
                        );
                        
                        highlight::update_aliases(&sh);
                    }

                    jobc::try_wait_bg_jobs(&mut sh, true, sig_handler_enabled);
                    continue;
                }

                Ok(ReadResult::Eof) =>
                {
                    if let Ok(x) = env::var("NO_EXIT_ON_CTRL_D")
                    {
                        if x == "1" { println!(); }
                    }
                    
                    else
                    {
                        println!("exit");
                        break;
                    }
                }

                Ok(ReadResult::Signal(s)) => { println_stderr!("readline signal: {:?}", s); }
                
                Err(e) =>
                {
                    println_stderr!("readline error: {}", e);
                    let gid = libc::getpgid(0);
                    shell::give_terminal_to(gid);
                }
            }
            
            if sig_handler_enabled { signals::block_signals(); }

            Ok(())
        }
    }
}
// 11468 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
