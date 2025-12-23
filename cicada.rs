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
#[macro_use] extern crate rand;
#[macro_use] extern crate regex as re;
#[macro_use] extern crate smallvec;
#[macro_use] extern crate time as temporal;
#[macro_use] extern crate unicode_normalization;
#[macro_use] extern crate unicode_width;

#[macro_use] pub mod macros
{

}

#[macro_use] pub mod tlog
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

#[macro_use] pub mod tools
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

pub mod builtins
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::fs::File;
    use std::io::Write;
    use std::os::unix::io::{FromRawFd, RawFd};

    use errno::errno;

    use crate::tools;
    use crate::types::{Command, CommandLine, CommandResult, Redirection};
    */
    pub mod alias
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        use regex::Regex;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::builtins::utils::print_stdout_with_capture;
        use crate::shell;
        use crate::tools;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run
        (
            sh: &mut shell::Shell,
            cl: &CommandLine,
            cmd: &Command,
            capture: bool,
        ) -> CommandResult {
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

        fn show_alias_list(
            sh: &shell::Shell,
            cmd: &Command,
            cl: &CommandLine,
            capture: bool,
        ) -> CommandResult {
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

        fn show_single_alias(
            sh: &shell::Shell,
            name_to_find: &str,
            cmd: &Command,
            cl: &CommandLine,
            capture: bool,
        ) -> CommandResult {
            let mut cr = CommandResult::new();
            if let Some(content) = sh.get_alias_content(name_to_find) {
                let info = format!("alias {}='{}'", name_to_find, content);
                print_stdout_with_capture(&info, &mut cr, cl, cmd, capture);
            } else {
                let info = format!("cicada: alias: {}: not found", name_to_find);
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
            *,
        };
        /*
        use crate::builtins::utils::print_stderr_with_capture;
        use crate::jobc;
        use crate::libc;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if sh.jobs.is_empty() {
                let info = "cicada: bg: no job found";
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
                        let info = "cicada: bg: invalid job id";
                        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                }
            }
            if job_id == -1 {
                let info = "cicada: bg: not such job";
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
                                let info = format!("cicada: bg: job {} already in background", job.id);
                                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                                return cr;
                            }
                        }

                        let info_cmd = format!("[{}]  {} &", job.id, job.cmd);
                        print_stderr_with_capture(&info_cmd, &mut cr, cl, cmd, capture);
                        cr.status = 0;
                    }
                    None => {
                        let info = "cicada: bg: not such job";
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
            *,
        };
        /*
        use std::env;
        use std::path::Path;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::parsers;
        use crate::shell;
        use crate::tools;
        use crate::types::{Command, CommandLine, CommandResult};
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
            let args = parsers::parser_line::tokens_to_args(&tokens);

            if args.len() > 2 {
                let info = "cicada: cd: too many argument";
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
                let info = format!("cicada: cd: {}: No such file or directory", &args[1]);
                print_stderr_with_capture(&info, &mut cr, cl, cmd, capture);
                return cr;
            }

            match Path::new(&dir_to).canonicalize() {
                Ok(p) => {
                    dir_to = p.as_path().to_string_lossy().to_string();
                }
                Err(e) => {
                    let info = format!("cicada: cd: error: {}", e);
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
                    let info = format!("cicada: cd: {}", e);
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
            *,
        };
        /*
        use crate::builtins::utils::print_stdout_with_capture;
        use crate::history;
        use crate::libs;
        use crate::rcfile;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(_sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let mut info = vec![];
            const VERSION: &str = env!("CARGO_PKG_VERSION");
            info.push(("version", VERSION));

            let os_name = libs::os_type::get_os_name();
            info.push(("os-name", &os_name));

            let hfile = history::get_history_file();
            info.push(("history-file", &hfile));

            let rcf = rcfile::get_rc_file();
            info.push(("rc-file", &rcf));

            let git_hash = env!("GIT_HASH");
            if !git_hash.is_empty() {
                info.push(("git-commit", env!("GIT_HASH")));
            }

            let git_branch = env!("GIT_BRANCH");
            let mut branch = String::new();
            if !git_branch.is_empty() {
                branch.push_str(git_branch);
                let git_status = env!("GIT_STATUS");
                if git_status != "0" {
                    branch.push_str(" (dirty)");
                }
                info.push(("git-branch", &branch));
            }

            info.push(("built-with", env!("BUILD_RUSTC_VERSION")));
            info.push(("built-at", env!("BUILD_DATE")));

            let mut lines = Vec::new();
            for (k, v) in &info {
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
            *,
        };
        /*
        use exec;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::parsers;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(_sh: &Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();
            let args = parsers::parser_line::tokens_to_args(&tokens);
            let len = args.len();
            if len == 1 {
                print_stderr_with_capture("invalid usage", &mut cr, cl, cmd, capture);
                return cr;
            }

            let mut _cmd = exec::Command::new(&args[1]);
            let err = _cmd.args(&args[2..len]).exec();
            let info = format!("cicada: exec: {}", err);
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
            *,
        };
        /*
        use std::process;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(sh: &Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let mut cr = CommandResult::new();
            let tokens = cmd.tokens.clone();
            if tokens.len() > 2 {
                let info = "cicada: exit: too many arguments";
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
                        let info = format!("cicada: exit: {}: numeric argument required", _code);
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
            *,
        };
        /*
        use regex::Regex;
        use std::env;

        use crate::libs;
        use crate::parsers;
        use crate::tools;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(_sh: &Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
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
                    let token = parsers::parser_line::unquote(&cap[2]);
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
            *,
        };
        /*
        use libc;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::jobc;
        use crate::shell::{self, Shell};
        use crate::types::{Command, CommandLine, CommandResult};
        */
        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult
        {
            let tokens = cmd.tokens.clone();
            let mut cr = CommandResult::new();

            if sh.jobs.is_empty() {
                let info = "cicada: fg: no job found";
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
                        let info = "cicada: fg: invalid job id";
                        print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                        return cr;
                    }
                }
            }

            if job_id == -1 {
                let info = "cicada: not job id found";
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
                        let info = "cicada: fg: no such job";
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
            *,
        };
        /*
        use std::path::Path;

        use rusqlite::Connection as Conn;
        use structopt::StructOpt;

        use crate::builtins::utils::print_stderr_with_capture;
        use crate::builtins::utils::print_stdout_with_capture;
        use crate::ctime;
        use crate::history;
        use crate::parsers;
        use crate::shell::Shell;
        use crate::types::{Command, CommandLine, CommandResult};
        */
        #[derive(Debug, StructOpt)]
        #[structopt(name = "history", about = "History in cicada shell")]
        struct OptMain {
            #[structopt(short, long, help = "For current session only")]
            session: bool,

            #[structopt(short, long, help = "Search old items first")]
            asc: bool,

            #[structopt(short, long, help = "For current directory only")]
            pwd: bool,

            #[structopt(short, long, help = "Only show ROWID")]
            only_id: bool,

            #[structopt(short, long, help = "Do not show ROWID")]
            no_id: bool,

            #[structopt(short = "d", long, help = "Show date")]
            show_date: bool,

            #[structopt(short, long, default_value = "20")]
            limit: i32,

            #[structopt(
                name = "PATTERN",
                default_value = "",
                help = "You can use % to match anything"
            )]
            pattern: String,

            #[structopt(subcommand)]
            cmd: Option<SubCommand>,
        }

        #[derive(StructOpt, Debug)]
        enum SubCommand {
            #[structopt(about = "Add new item into history")]
            Add {
                #[structopt(short = "t", long, help = "Specify a timestamp for the new item")]
                timestamp: Option<f64>,

                #[structopt(name = "INPUT", help = "input to be added into history")]
                input: String,
            },
            #[structopt(about = "Delete item from history")]
            Delete {
                #[structopt(name = "ROWID", help = "Row IDs of item to delete")]
                rowid: Vec<usize>,
            },
        }

        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
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
            let args = parsers::parser_line::tokens_to_args(&tokens);

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

        fn add_history(sh: &Shell, ts: f64, input: &str) {
            let (tsb, tse) = (ts, ts + 1.0);
            history::add_raw(sh, input, 0, tsb, tse);
        }

        fn list_current_history(sh: &Shell, conn: &Conn, opt: &OptMain) -> (String, String) {
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

        fn delete_history_item(conn: &Conn, rowid: usize) -> bool {
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
                    println_stderr!("cicada: minfd: error: {}", e);
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
                    let info = format!("cicada: read: `{}': not a valid identifier", id_);
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
                        let info = format!("cicada: read: error in reading stdin: {:?}", e);
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
                    let info = "cicada: read: name index error";
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
        #[derive(Debug, StructOpt)]
        #[structopt(name = "set", about = "Set shell options (BETA)")]
        struct OptMain {
            #[structopt(short, help = "exit on error status")]
            exit_on_error: bool,
        }

        pub fn run(sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let mut cr = CommandResult::new();
            let tokens = &cmd.tokens;
            let args = parsers::parser_line::tokens_to_args(tokens);
            let show_usage = args.len() > 1 && (args[1] == "-h" || args[1] == "--help");

            let opt = OptMain::from_iter_safe(args);
            match opt {
                Ok(opt) => {
                    if opt.exit_on_error {
                        sh.exit_on_error = true;
                        cr
                    } else {
                        let info = "cicada: set: option not implemented";
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
            let args = parsers::parser_line::tokens_to_args(tokens);

            if args.len() < 2 {
                let info = "cicada: source: no file specified";
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
        */
        #[derive(Parser)]
        #[command(name = "ulimit", about = "show / modify shell resource limits")]
        #[allow(non_snake_case)]
        struct App {
            #[arg(short, help = "All current limits are reported.")]
            a: bool,
            #[arg(
                short,
                value_name = "NEW VALUE",
                help = "The maximum number of open file descriptors."
            )]
            n: Option<Option<u64>>,
            #[arg(
                short,
                value_name = "NEW VALUE",
                help = "The maximum size of core files created."
            )]
            c: Option<Option<u64>>,
            #[arg(
                short = 'S',
                help = "Set a soft limit for the given resource. (default)"
            )]
            S: bool,
            #[arg(short = 'H', help = "Set a hard limit for the given resource.")]
            H: bool,
        }

        pub fn run(_sh: &mut Shell, cl: &CommandLine, cmd: &Command, capture: bool) -> CommandResult {
            let mut cr = CommandResult::new();
            let tokens = &cmd.tokens;
            let args = parsers::parser_line::tokens_to_args(tokens);

            if args.contains(&"--help".to_string()) || args.contains(&"-h".to_string()) {
                App::command().print_help().unwrap();
                println!();
                return cr;
            }

            let app = App::parse_from(args);

            if app.H && app.S {
                println!("cicada: ulimit: Cannot both hard and soft.");
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
                        "cicada: ulimit: error getting limit: {}",
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
                        "cicada: ulimit: error setting limit: {}",
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
                let info = "cicada: unalias: syntax error";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let input = &tokens[1].1;
            if !sh.remove_alias(input) {
                let info = format!("cicada: unalias: {}: not found", input);
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
                let info = "cicada: unpath: syntax error";
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
                let info = "cicada: unset: syntax error";
                print_stderr_with_capture(info, &mut cr, cl, cmd, capture);
                return cr;
            }

            let input = &tokens[1].1;
            if !sh.remove_env(input) {
                let info = format!("cicada: unset: invalid varname: {:?}", input);
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
            let args = parsers::parser_line::tokens_to_args(&tokens);
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
                let info = "cicada: vox: invalid option";
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
                println_stderr!("cicada: dup: {}", eno);
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
                println_stderr!("cicada: dup: {}", eno);
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
        *,
    };
    /*
    use std::num::Wrapping as W;

    use pest::iterators::{Pair, Pairs};
    use pest::pratt_parser::{Assoc, Op, PrattParser};
    use pest::Parser;
    */
    #[derive(Parser)]
    #[grammar = "calculator/grammar.pest"]
    struct Calculator;

    lazy_static! 
    {
        static ref PRATT_PARSER: PrattParser<Rule> = 
        {
            use Assoc::*;
            use Rule::*;

            PrattParser::new()
                .op(Op::infix(add, Left) | Op::infix(subtract, Left))
                .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
                .op(Op::infix(power, Right))
        };
    }

    pub fn eval_int(expression: Pairs<Rule>) -> i64 
    {
        PRATT_PARSER
            .map_primary(|primary| match primary.as_rule() {
                Rule::num => primary.as_str().parse::<i64>().unwrap(),
                Rule::expr => eval_int(primary.into_inner()),
                _ => unreachable!(),
            })
            .map_infix(|lhs: i64, op: Pair<Rule>, rhs: i64| match op.as_rule() {
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
            })
            .parse(expression)
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

    pub fn calculate(
        line: &str,
    ) -> Result<pest::iterators::Pairs<'_, Rule>, pest::error::Error<Rule>> {
        Calculator::parse(Rule::calculation, line)
    }
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
            let args = parsers::parser_line::line_to_plain_tokens(line);
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

            let linfo = parsers::parser_line::parse_line(value);
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
                        println_stderr!("cicada: regex build error: {:?}", e);
                        return;
                    }
                };

                let re_include = match Regex::new(r"^ *include  *([^ ]+) *$") {
                    Ok(x) => x,
                    Err(e) => {
                        println_stderr!("cicada: regex build error: {:?}", e);
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
                        println!("cicada: to_str error");
                        return res;
                    }
                },
                Err(e) => {
                    println!("cicada: get current_dir error: {:?}", e);
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
            let linfo = parsers::parser_line::parse_line(word);
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
                    println_stderr!("cicada: env error when complete_bin: {:?}", e);
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
                                        println_stderr!("cicada: metadata error: {:?}", e);
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
        let args = parsers::parser_line::line_to_plain_tokens(line);
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
            println_stderr!("cicada: cannot capture output of background cmd");
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
            println!("cicada: invalid command: cmds with empty length");
            return (false, CommandResult::error());
        }

        let mut pipes = Vec::new();
        let mut errored_pipes = false;
        for _ in 0..length - 1 {
            match pipe() {
                Ok(fds) => pipes.push(fds),
                Err(e) => {
                    errored_pipes = true;
                    println_stderr!("cicada: pipeline1: {}", e);
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
            println!("cicada: invalid command: unmatched pipes count");
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
                    println_stderr!("cicada: pipeline2: {}", e);
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
                    println_stderr!("cicada: pipeline3: {}", e);
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

            println_stderr!("cicada: error when run singler builtin");
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
                    println_stderr!("cicada: pipeline4: {}", e);
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
                                println_stderr!("cicada: dup error");
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
                                println_stderr!("cicada: dup error");
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
                                    println_stderr!("cicada: fork: fd error");
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
                                println_stderr!("cicada: fork: {}", e);
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
                    println_stderr!("cicada: {}: command not found", program);
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
                            println_stderr!("cicada: {}: exec format error (ENOEXEC)", program);
                        }
                        nix::Error::ENOENT => {
                            println_stderr!("cicada: {}: file does not exist", program);
                        }
                        nix::Error::EACCES => {
                            println_stderr!("cicada: {}: Permission denied", program);
                        }
                        _ => {
                            println_stderr!("cicada: {}: {:?}", program, e);
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
                    let _cmd = parsers::parser_line::tokens_to_line(&cmd.tokens);
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
                                    Err(e) => println_stderr!("cicada: write_all: {}", e),
                                }
                                match f.write_all(b"\n") {
                                    Ok(_) => {}
                                    Err(e) => println_stderr!("cicada: write_all: {}", e),
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
                                Err(e) => println_stderr!("cicada: readstr: {}", e),
                            }
                        }
                        if let Some(fds) = fds_capture_stderr {
                            libs::close(fds.1);
                            let mut f_err = File::from_raw_fd(fds.0);
                            match f_err.read_to_string(&mut s_err) {
                                Ok(_) => {}
                                Err(e) => println_stderr!("cicada: readstr: {}", e),
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
                        println_stderr!("cicada: calculator: {}", e);
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

pub mod execute
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    use std::collections::HashMap;
    use std::io::{self, Read, Write};

    use crate::core;
    use crate::parsers;
    use crate::shell::{self, Shell};
    use crate::types::{drain_env_tokens, CommandLine, CommandResult, Tokens};
    */
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
                println!("cicada: stdin.read_to_string() failed: {:?}", e);
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

    pub fn line_to_tokens(sh: &mut Shell, line: &str) -> (Tokens, HashMap<String, String>)
    {
        let linfo = parsers::parser_line::parse_line(line);
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
                println_stderr!("cicada: {}", e);
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
                println_stderr!("cicada: {}", e);
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

    use crate::parsers::parser_line;
    use crate::shell;
    use crate::tools;
    */
    #[derive(Clone)]
    pub struct CicadaHighlighter;

    /// ANSI color codes wrapped with `\x1b` (ESC) and `[0;32m` (green text)
    const GREEN: &str = "\x1b[0;32m";

    lazy_static! {
        static ref AVAILABLE_COMMANDS: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
        static ref ALIASES: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
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

    fn is_command(word: &str) -> bool {
        if tools::is_builtin(word) {
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

    impl Highlighter for CicadaHighlighter {
        fn highlight(&self, line: &str) -> Vec<(Range<usize>, Style)> {
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

            for token in &line_info.tokens {
                // Find the range in the original line for this token
                match find_token_range_heuristic(line, current_byte_idx, token) {
                    Some(token_range) => {
                        // Style potential whitespace before the token
                        if token_range.start > current_byte_idx {
                            styles.push((current_byte_idx..token_range.start, Style::Default));
                        }

                        let (_sep, word) = token;
                        let mut current_token_style = Style::Default;

                        if is_start_of_segment && !word.is_empty() {
                            if is_command(word) {
                                current_token_style = Style::AnsiColor(GREEN.to_string());
                            }
                            // Only the first non-empty token in a segment can be a command
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

            // Style any remaining characters after the last processed token
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
                    println_stderr!("cicada: history init - no parent found");
                    return;
                }
            };
            let parent = match _parent.to_str() {
                Some(x) => x,
                None => {
                    println_stderr!("cicada: parent to_str is None");
                    return;
                }
            };
            match fs::create_dir_all(parent) {
                Ok(_) => {}
                Err(e) => {
                    println_stderr!("cicada: histdir create error: {}", e);
                    return;
                }
            }
            match fs::File::create(hfile) {
                Ok(_) => {
                    println!("cicada: created history file: {}", hfile);
                }
                Err(e) => {
                    println_stderr!("cicada: history: file create failed: {}", e);
                }
            }
        }

        let conn = match Conn::open(hfile) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: history: open db error: {}", e);
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
            Err(e) => println_stderr!("cicada: history: query error: {}", e),
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
                println_stderr!("cicada: history: conn error: {}", e);
                return;
            }
        };
        let sql = format!("SELECT inp FROM {} ORDER BY tsb;", history_table);
        let mut stmt = match conn.prepare(&sql) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: prepare select error: {}", e);
                return;
            }
        };

        let rows = match stmt.query_map([], |row| row.get(0)) {
            Ok(x) => x,
            Err(e) => {
                println_stderr!("cicada: query select error: {}", e);
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
                println_stderr!("cicada: history: conn error: {}", e);
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
                    println_stderr!("cicada: history: delete dups error: {}: {:?}", &ee, &msg);
                }
                _ => {
                    println_stderr!("cicada: history: delete dup error: {}", e);
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
                println_stderr!("cicada: history: conn error: {}", e);
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
            Err(e) => println_stderr!("cicada: history: save error: {}", e),
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
            *,
        };
        /*
        use nix::unistd::{fork as nix_fork, ForkResult};
        use nix::Result;
        */
        pub fn fork() -> Result<ForkResult> {
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
                    println_stderr!("cicada: error with env PATH: {:?}", e);
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
                                            println_stderr!("cicada: metadata error: {:?}", e);
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
                        log!("cicada: fs read_dir error: {}: {}", p.display(), e);
                    }
                }
            }
            String::new()
        }

        pub fn current_dir() -> String {
            let _current_dir = match env::current_dir() {
                Ok(x) => x,
                Err(e) => {
                    log!("cicada: PROMPT: env current_dir error: {}", e);
                    return String::new();
                }
            };
            let current_dir = match _current_dir.to_str() {
                Some(x) => x,
                None => {
                    log!("cicada: PROMPT: to_str error");
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

pub mod parsers
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

        fn is_prefix_char(c: char) -> bool {
            c == '[' || c == '{'
        }

        fn is_suffix_char(c: char) -> bool {
            c == ']' || c == '}'
        }

        fn is_prompt_item_char(c: char, token: &str) -> bool {
            let s = c.to_string();
            if token.is_empty() {
                libs::re::re_contains(&s, r#"^[a-zA-Z_]$"#)
            } else {
                libs::re::re_contains(&s, r#"^[a-zA-Z0-9_]$"#)
            }
        }

        pub fn get_prompt_string() -> String {
            if let Ok(x) = env::var("PROMPT") {
                return x;
            }
            DEFAULT_PROMPT.to_string()
        }

        fn apply_prompt_item(sh: &shell::Shell, result: &mut String, token: &str) {
            if let Some(x) = sh.get_env(token) {
                result.push_str(&x);
                return;
            }
            apply_preset_item(sh, result, token);
        }

        fn apply_command(result: &mut String, token: &str, prefix: &str, suffix: &str) {
            let cr = execute::run(token);
            let output = cr.stdout.trim();
            if !output.is_empty() {
                result.push_str(prefix);
                result.push_str(output);
                result.push_str(suffix);
            }
        }

        pub fn render_prompt(sh: &shell::Shell, ps: &str) -> String {
            let mut prompt = String::new();
            apply_pyenv(&mut prompt);

            let mut met_dollar = false;
            let mut met_brace = false;
            let mut met_paren = false;
            let mut token = String::new();
            let mut prefix = String::new();
            let mut suffix = String::new();
            for c in ps.chars() {
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

            if !token.is_empty() {
                apply_prompt_item(sh, &mut prompt, &token);
                met_dollar = false;
            }

            if met_dollar {
                // for cases like PROMPT='$$'
                prompt.push('$');
            }

            if prompt.trim().is_empty() {
                return format!("cicada-{} >> ", env!("CARGO_PKG_VERSION"));
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

        use crate::parsers::parser_line;
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
                    println!("cicada: .git/HEAD err: {:?}", e);
                    return;
                }
            }
            let mut text = String::new();
            match file.read_to_string(&mut text) {
                Ok(_) => {}
                Err(e) => {
                    println!("cicada: read_to_string error: {:?}", e);
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
                    println_stderr!("cicada: PROMPT: env current_dir error: {}", e);
                    return;
                }
            };
            let current_dir = match _current_dir.to_str() {
                Some(x) => x,
                None => {
                    println_stderr!("cicada: PROMPT: to_str error");
                    return;
                }
            };
            let _tokens: Vec<&str> = current_dir.split('/').collect();

            let last = match _tokens.last() {
                Some(x) => x,
                None => {
                    log!("cicada: PROMPT: token last error");
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

pub mod scripting
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
                    println_stderr!("cicada: {}: no such file", src_file);
                    return 1;
                }
                full_src_file = format!("./{}", src_file);
            } else {
                full_src_file = full_path.clone();
            }
        }

        if !Path::new(&full_src_file).exists() {
            println_stderr!("cicada: {}: no such file", src_file);
            return 1;
        }
        if Path::new(&full_src_file).is_dir() {
            println_stderr!("cicada: {}: is a directory", src_file);
            return 1;
        }

        let mut file;
        match File::open(&full_src_file) {
            Ok(x) => file = x,
            Err(e) => {
                println_stderr!(
                    "cicada: {}: failed to open file - {:?}",
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
                        println_stderr!("cicada: {}: not a valid script file", &full_src_file);
                    }
                    _ => {
                        println_stderr!("cicada: {}: error: {:?}", &full_src_file, e);
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
        let linfo = parsers::parser_line::parse_line(line);
        let mut tokens = linfo.tokens;
        expand_args_in_tokens(&mut tokens, args);
        parsers::parser_line::tokens_to_line(&tokens)
    }

    fn expand_line_to_toknes(line: &str, args: &[String], sh: &mut shell::Shell) -> types::Tokens 
    {
        let linfo = parsers::parser_line::parse_line(line);
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
                        println_stderr!("cicada: continue: only meaningful in loops");
                        continue;
                    }
                }
                if line == "break" {
                    if in_loop {
                        return (cr_list, false, true);
                    } else {
                        println_stderr!("cicada: break: only meaningful in loops");
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
    */
}

pub mod signals
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

pub mod types
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

pub fn main() -> Result<(), error::parse::ParseError>
{
    unsafe
    {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
        libc::signal(libc::SIGTSTP, libc::SIG_IGN);
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);

        tools::init_path_env();

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
                println!("cicada: lineread error: {}", e);
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
                Err(e) => { println_stderr!("cicada: prompt error: {}", e); }
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
// 6605 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
