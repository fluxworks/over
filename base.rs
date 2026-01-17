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
*/
#[macro_use] extern crate libc;
#[macro_use] extern crate time as temporal;

#[macro_use] pub mod macros
{   
    /*!
    */
    pub use std::
    {
        assert, assert_eq, assert_ne, cfg, column, compile_error, concat, dbg, debug_assert, debug_assert_eq, 
        debug_assert_ne, env, eprint, eprintln, file, format, format_args, include, include_bytes, include_str, 
        is_x86_feature_detected, line, matches, module_path, option_env, panic, print, println, stringify, 
        thread_local, todo, unimplemented, unreachable, vec, write, writeln
    };

    use ::
    {
        cell::{ Cell },
        mem::{ MaybeUninit },
        sync::{ Once, ONCE_INIT },
        *
    };
}

pub mod arch
{
    pub use std::arch::{ * };
}

pub mod arrays
{
    /*!
    Arr | A container which can hold an arbitrary number of elements of a single type. */
    pub use std::array::{ * };
}

pub mod ascii
{
    pub use std::ascii::{ * };
}

pub mod borrow
{
    pub use std::borrow::{ * };
}

pub mod boxed
{
    pub use std::boxed::{ * };
}

pub mod cell
{
    pub use std::cell::{ * };
}

pub mod char
{
    /*!
    Character stream used for parsing. */
    pub use std::char::{ * };
}

pub mod clone
{
    pub use std::clone::{ * };
}

pub mod cmp
{
    pub use std::cmp::{ * };
}

pub mod collections
{
    pub use std::collections::{ * };
}

pub mod convert
{
    pub use std::convert::{ * };
}

pub mod default
{
    pub use std::default::{ * };
}

pub mod env
{
    /*!
    */
    pub use std::env::{ * };
}

pub mod error
{
    /*!
    Error Handling */
    pub use std::error::{ * };
    use ::
    {
        *,
    };
    /*
    */    
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
                    error::no::{ Errno },
                    libc::{ c_int, ERANGE, size_t, strerror_r, strlen },
                    *,
                };
                /*
                */
                fn from_utf8_lossy(input:&[u8] ) -> &str
                {
                    match str::from_utf8(input )
                    {
                        Ok(valid ) => valid,
                        Err(error ) => unsafe { str::from_utf8_unchecked( &input[..error.valid_up_to()]) },
                    }
                }

                pub fn with_description<F, T>(err: Errno, callback: F) -> T where
                F:FnOnce( ::result::Result<&str, Errno> ) -> T,
                {
                    unsafe
                    {
                        let mut buf = [0u8; 1024];
                        let c_str = 
                        {
                            let rc = strerror_r(err.0, buf.as_mut_ptr() as *mut _, buf.len() as size_t );
                            
                            if rc != 0
                            {
                                let fm_err = match rc < 0 {
                                    true => errno(),
                                    false => Errno( rc ),
                                };
                                if fm_err != Errno( ERANGE) {
                                    return callback(Err( fm_err  ) );
                                }
                            }

                            let c_str_len = strlen( buf.as_ptr() as *const _ );
                            &buf[..c_str_len]
                        };

                        callback( Ok( from_utf8_lossy( c_str ) ) )
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
                        any( target_os = "solaris", target_os = "illumos" ),
                        link_name = "___errno"
                    )]
                    #[cfg_attr( target_os = "haiku", link_name = "_errnop" )]
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
                    #[cfg_attr( target_os = "aix", link_name = "_Errno" )]
                    #[cfg_attr( target_os = "nto", link_name = "__get_errno_ptr" )]
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
                sys::with_description(*self, |desc|
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
                sys::with_description(*self, |desc| match desc {
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

        impl From<Errno> for i32 {
            fn from(e: Errno) -> Self {
                e.0
            }
        }
        
        impl Error for Errno {

            #[allow(deprecated )]
            fn description( &self ) -> &str { "system error" }
        }
        
        impl From<Errno> for io::Error {
            fn from(errno: Errno) -> Self {
                io::Error::from_raw_os_error(errno.0)
            }
        }

        pub fn errno() -> Errno {
            sys::errno() }

        pub fn set_errno(err: Errno) {
            sys::set_errno(err ) }

    }
}

pub mod expand
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
}

pub mod f32
{
    pub use std::f32::{ * };
}

pub mod f64
{
    pub use std::f64::{ * };
}

pub mod ffi
{
    pub use std::ffi::{ * };
}

pub mod fmt
{
    /*!
    */
    pub use std::fmt::{ * };
}

pub mod fs
{
    pub use std::fs::{ * };
}

pub mod has
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

pub mod hash
{
    pub use std::hash::{ * };
}

pub mod hint
{
    /*!
    */
    pub use std::hint::{ * };
}

pub mod i8
{
    pub use std::i8::{ * };
}

pub mod i16
{
    pub use std::i16::{ * };
}

pub mod i32
{
    pub use std::i32::{ * };
}

pub mod i64
{
    pub use std::i64::{ * };
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
}

pub mod isize
{
    pub use std::isize::{ * };
}

pub mod i128
{
    pub use std::i128::{ * };
}

pub mod io
{
    pub use std::io::{ * };
}

pub mod iter
{
    pub use std::iter::{ * };
}

pub mod marker
{
    pub use std::marker::{ * };
}

pub mod num
{
    pub use std::num::{ * };
}

pub mod mem
{
    pub use std::mem::{ * };
}

pub mod ops
{
    pub use std::ops::{ * };
}

pub mod os
{
    pub use std::os::{ * };
}

pub mod option
{
    pub use std::option::{ * };
}

pub mod panic
{
    pub use std::panic::{ * };
}

pub mod path
{
    pub use std::path::{ * };
}

pub mod process
{
    pub use std::process::{ * };
}

pub mod ptr
{
    pub use std::ptr::{ * };
}

pub mod rc
{
    pub use std::rc::{ * };
}

pub mod result
{
    pub use std::result::{ * };
}

pub mod slice
{
    pub use std::slice::{ * };
}

pub mod str
{
    pub use std::str::{ * };
}

pub mod string
{
    pub use std::string::{ * };
}

pub mod sync
{
    pub use std::sync::{ * };
}

pub mod thread
{
    pub use std::thread::{ * };
}

pub mod u8
{
    pub use std::u8::{ * };
}

pub mod u16
{
    pub use std::u16::{ * };
}

pub mod u32
{
    pub use std::u32::{ * };
}

pub mod u64
{
    pub use std::u64::{ * };
}

pub mod u128
{
    pub use std::u128::{ * };
}

pub mod usize
{
    pub use std::usize::{ * };
}

pub mod vec
{
    pub use std::vec::{ * };
}

pub fn main() -> Result<(), ()>
{
    unsafe
    {
        Ok( () )
    }
}
// 78848 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
