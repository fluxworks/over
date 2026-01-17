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

pub mod alloc
{
    pub use std::alloc::{ * };
    use ::
    {
        ffi::{c_char, CStr},
        marker::{PhantomData},
        ptr::{NonNull},
        *,
    };
    /*
    */
    #[repr(transparent)] pub struct SqliteMallocString
    {
        ptr: NonNull<c_char>,
        _boo: PhantomData<Box<[c_char]>>,
    }

    impl SqliteMallocString
    {
        #[inline] pub unsafe fn from_raw_nonnull(ptr: NonNull<c_char>) -> Self
        {
            Self 
            {
                ptr,
                _boo: PhantomData,
            }
        }
        
        #[inline] pub unsafe fn from_raw(ptr: *mut c_char) -> Option<Self> { NonNull::new(ptr).map(|p| Self::from_raw_nonnull(p)) }
        #[inline] pub fn into_inner(self) -> NonNull<c_char>
        {
            let p = self.ptr;
            std::mem::forget(self);
            p
        }
        
        #[inline] pub fn into_raw(self) -> *mut c_char { self.into_inner().as_ptr() }
        #[inline] pub fn as_ptr(&self) -> *const c_char { self.ptr.as_ptr() }
        #[inline] pub fn as_cstr(&self) -> &CStr { unsafe { CStr::from_ptr(self.as_ptr()) } }
        #[inline] pub fn to_string_lossy(&self) -> ::borrow::Cow<'_, str> { self.as_cstr().to_string_lossy() }
        pub fn from_str(s: &str) -> Self
        {
            unsafe
            {
                let s = if s.as_bytes().contains(&0) { ::borrow::Cow::Owned(make_nonnull(s)) }
                else { ::borrow::Cow::Borrowed(s) };

                debug_assert!(!s.as_bytes().contains(&0));
                
                let bytes: &[u8] = s.as_ref().as_bytes();
                let src_ptr: *const c_char = bytes.as_ptr().cast();
                let src_len = bytes.len();
                let maybe_len_plus_1 = s.len().checked_add(1).and_then(|v| v.try_into().ok());
                let res_ptr = maybe_len_plus_1
                .and_then(|len_to_alloc|
                {
                    debug_assert!(len_to_alloc > 0);
                    debug_assert_eq!((len_to_alloc - 1) as usize, src_len);
                    NonNull::new(system::sql::sqlite3_malloc64(len_to_alloc).cast::<c_char>())
                })
                .unwrap_or_else(||
                {
                    let len = s.len().saturating_add(1).min(isize::MAX as usize);
                    let layout = Layout::from_size_align_unchecked(len, 1);
                    handle_alloc_error(layout);
                });
                
                let buf: *mut c_char = res_ptr.as_ptr().cast::<c_char>();
                src_ptr.copy_to_nonoverlapping(buf, src_len);
                buf.add(src_len).write(0);
                debug_assert_eq!(CStr::from_ptr(res_ptr.as_ptr()).to_bytes(), bytes);
                Self::from_raw_nonnull(res_ptr)
            }
        }
    }

    const NUL_REPLACE: &str = "␀";

    #[cold] pub fn make_nonnull(v: &str) -> String { v.replace('\0', NUL_REPLACE) }
    
    pub fn allocate(s:&str) -> *mut c_char
    {
        SqliteMallocString::from_str(s).into_raw()
    }
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

    pub mod sqlite
    {
        /*!
        */
        use ::
        {
            ffi::{ c_char, c_int, NulError },
            path::{ PathBuf },
            *,
        };

        use system::sql as ffi;
        /*
        use crate::types::FromSqlError;
        use crate::types::Type;
        use crate::{errmsg_to_string, ffi, Result};
        */
        macro_rules! err
        {
            ($code:expr $(,)?) => { ::error::sqlite::error_from_sqlite_code($code, None) };
            ($code:expr, $msg:literal $(,)?) => { ::error::sqlite::error_from_sqlite_code($code, Some(format!($msg))) };
            ($code:expr, $err:expr $(,)?) => { ::error::sqlite::error_from_sqlite_code($code, Some(format!($err))) };
            ($code:expr, $fmt:expr, $($arg:tt)*) => { ::error::sqlite::error_from_sqlite_code($code, Some(format!($fmt, $($arg)*))) };
        }

        #[non_exhaustive] #[derive(Debug)]
        pub enum Error
        {
            SqliteFailure(ffi::Error, Option<String>),
            SqliteSingleThreadedMode,
            FromSqlConversionFailure(usize, Type, Box<dyn error::Error + Send + Sync + 'static>),
            IntegralValueOutOfRange(usize, i64),
            Utf8Error(str::Utf8Error),
            NulError(NulError),
            InvalidParameterName(String),
            InvalidPath(PathBuf),
            ExecuteReturnedResults,
            QueryReturnedNoRows,
            QueryReturnedMoreThanOneRow,
            InvalidColumnIndex(usize),
            InvalidColumnName(String),
            InvalidColumnType(usize, String, Type),
            StatementChangedRows(usize),
            InvalidFunctionParameterType(usize, Type),
            InvalidFilterParameterType(usize, Type),
            UserFunctionError(Box<dyn error::Error + Send + Sync + 'static>),
            ToSqlConversionFailure(Box<dyn error::Error + Send + Sync + 'static>),
            InvalidQuery,
            ModuleError(String),
            UnwindingPanic,
            GetAuxWrongType,
            MultipleStatement,
            InvalidParameterCount(usize, usize),
            BlobSizeError,
            SqlInputError 
            {
                error: ffi::Error,
                msg: String,
                sql: String,
                offset: c_int,
            },
            InitError(ffi::InitError),
            InvalidDatabaseIndex(usize),
        }

        impl PartialEq for Error 
        {
            fn eq(&self, other: &Self) -> bool 
            {
                match (self, other) 
                {
                    (Self::SqliteFailure(e1, s1), Self::SqliteFailure(e2, s2)) => e1 == e2 && s1 == s2,
                    (Self::SqliteSingleThreadedMode, Self::SqliteSingleThreadedMode) => true,
                    (Self::IntegralValueOutOfRange(i1, n1), Self::IntegralValueOutOfRange(i2, n2)) => {
                        i1 == i2 && n1 == n2
                    }
                    (Self::Utf8Error(e1), Self::Utf8Error(e2)) => e1 == e2,
                    (Self::NulError(e1), Self::NulError(e2)) => e1 == e2,
                    (Self::InvalidParameterName(n1), Self::InvalidParameterName(n2)) => n1 == n2,
                    (Self::InvalidPath(p1), Self::InvalidPath(p2)) => p1 == p2,
                    (Self::ExecuteReturnedResults, Self::ExecuteReturnedResults) => true,
                    (Self::QueryReturnedNoRows, Self::QueryReturnedNoRows) => true,
                    (Self::QueryReturnedMoreThanOneRow, Self::QueryReturnedMoreThanOneRow) => true,
                    (Self::InvalidColumnIndex(i1), Self::InvalidColumnIndex(i2)) => i1 == i2,
                    (Self::InvalidColumnName(n1), Self::InvalidColumnName(n2)) => n1 == n2,
                    (Self::InvalidColumnType(i1, n1, t1), Self::InvalidColumnType(i2, n2, t2)) => {
                        i1 == i2 && t1 == t2 && n1 == n2
                    }
                    (Self::StatementChangedRows(n1), Self::StatementChangedRows(n2)) => n1 == n2,
                    #[cfg(feature = "functions")]
                    (
                        Self::InvalidFunctionParameterType(i1, t1),
                        Self::InvalidFunctionParameterType(i2, t2),
                    ) => i1 == i2 && t1 == t2,
                    #[cfg(feature = "vtab")]
                    (
                        Self::InvalidFilterParameterType(i1, t1),
                        Self::InvalidFilterParameterType(i2, t2),
                    ) => i1 == i2 && t1 == t2,
                    (Self::InvalidQuery, Self::InvalidQuery) => true,
                    #[cfg(feature = "vtab")]
                    (Self::ModuleError(s1), Self::ModuleError(s2)) => s1 == s2,
                    (Self::UnwindingPanic, Self::UnwindingPanic) => true,
                    #[cfg(feature = "functions")]
                    (Self::GetAuxWrongType, Self::GetAuxWrongType) => true,
                    (Self::InvalidParameterCount(i1, n1), Self::InvalidParameterCount(i2, n2)) => {
                        i1 == i2 && n1 == n2
                    }
                    #[cfg(feature = "blob")]
                    (Self::BlobSizeError, Self::BlobSizeError) => true,
                    #[cfg(feature = "modern_sqlite")]
                    (
                        Self::SqlInputError {
                            error: e1,
                            msg: m1,
                            sql: s1,
                            offset: o1,
                        },
                        Self::SqlInputError {
                            error: e2,
                            msg: m2,
                            sql: s2,
                            offset: o2,
                        },
                    ) => e1 == e2 && m1 == m2 && s1 == s2 && o1 == o2,
                    #[cfg(feature = "loadable_extension")]
                    (Self::InitError(e1), Self::InitError(e2)) => e1 == e2,
                    #[cfg(feature = "modern_sqlite")]
                    (Self::InvalidDatabaseIndex(i1), Self::InvalidDatabaseIndex(i2)) => i1 == i2,
                    (..) => false,
                }
            }
        }

        impl From<str::Utf8Error> for Error
        {
            #[cold] fn from(err: str::Utf8Error) -> Self { Self::Utf8Error(err) }
        }

        impl From<NulError> for Error
        {
            #[cold] fn from(err: NulError) -> Self { Self::NulError(err) }
        }

        const UNKNOWN_COLUMN: usize = usize::MAX;
        
        impl From<FromSqlError> for Error
        {
            #[cold] fn from(err: FromSqlError) -> Self
            {
                match err
                {
                    FromSqlError::OutOfRange(val) => Self::IntegralValueOutOfRange(UNKNOWN_COLUMN, val),
                    FromSqlError::InvalidBlobSize { .. } => { Self::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Blob, Box::new(err)) }
                    FromSqlError::Other(source) => { Self::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Null, source) }
                    _ => Self::FromSqlConversionFailure(UNKNOWN_COLUMN, Type::Null, Box::new(err)),
                }
            }
        }
        
        impl From<ffi::InitError> for Error
        {
            #[cold] fn from(err: ffi::InitError) -> Self { Self::InitError(err) }
        }

        impl fmt::Display for Error
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {
                match *self
                {
                    Self::SqliteFailure(ref err, None) => err.fmt(f),
                    Self::SqliteFailure(_, Some(ref s)) => write!(f, "{s}"),
                    Self::SqliteSingleThreadedMode => write!( f, "SQLite was compiled or configured for single-threaded use only" ),
                    Self::FromSqlConversionFailure(i, ref t, ref err) =>
                    {
                        if i != UNKNOWN_COLUMN { write!(f, "Conversion error from type {t} at index: {i}, {err}") }
                        else { err.fmt(f) }
                    }
                    Self::IntegralValueOutOfRange(col, val) =>
                    {
                        if col != UNKNOWN_COLUMN { write!(f, "Integer {val} out of range at index {col}") }
                        else { write!(f, "Integer {val} out of range") }
                    }
                    Self::Utf8Error(ref err) => err.fmt(f),
                    Self::NulError(ref err) => err.fmt(f),
                    Self::InvalidParameterName(ref name) => write!(f, "Invalid parameter name: {name}"),
                    Self::InvalidPath(ref p) => write!(f, "Invalid path: {}", p.to_string_lossy()),
                    Self::ExecuteReturnedResults => { write!(f, "Execute returned results - did you mean to call query?") }
                    Self::QueryReturnedNoRows => write!(f, "Query returned no rows"),
                    Self::QueryReturnedMoreThanOneRow => write!(f, "Query returned more than one row"),
                    Self::InvalidColumnIndex(i) => write!(f, "Invalid column index: {i}"),
                    Self::InvalidColumnName(ref name) => write!(f, "Invalid column name: {name}"),
                    Self::InvalidColumnType(i, ref name, ref t) => { write!(f, "Invalid column type {t} at index: {i}, name: {name}") }
                    Self::InvalidParameterCount(i1, n1) => write!( f, "Wrong number of parameters passed to query. Got {i1}, needed {n1}" ),
                    Self::StatementChangedRows(i) => write!(f, "Query changed {i} rows"),
                    Self::InvalidFunctionParameterType(i, ref t) => { write!(f, "Invalid function parameter type {t} at index {i}") }
                    Self::InvalidFilterParameterType(i, ref t) => { write!(f, "Invalid filter parameter type {t} at index {i}") }
                    Self::UserFunctionError(ref err) => err.fmt(f),
                    Self::ToSqlConversionFailure(ref err) => err.fmt(f),
                    Self::InvalidQuery => write!(f, "Query is not read-only"),
                    Self::ModuleError(ref desc) => write!(f, "{desc}"),
                    Self::UnwindingPanic => write!(f, "unwinding panic"),
                    Self::GetAuxWrongType => write!(f, "get_aux called with wrong type"),
                    Self::MultipleStatement => write!(f, "Multiple statements provided"),
                    Self::BlobSizeError => "Blob size is insufficient".fmt(f),
                    Self::SqlInputError
                    {
                        ref msg,
                        offset,
                        ref sql,
                        ..
                    } => write!(f, "{msg} in {sql} at offset {offset}"),
                    Self::InitError(ref err) => err.fmt(f),
                    Self::InvalidDatabaseIndex(i) => write!(f, "Invalid database index: {i}"),
                }
            }
        }

        impl error::Error for Error
        {
            fn source(&self) -> Option<&(dyn error::Error + 'static)>
            {
                match *self
                {
                    Self::SqliteFailure(ref err, _) => Some(err),
                    Self::Utf8Error(ref err) => Some(err),
                    Self::NulError(ref err) => Some(err),
                    Self::IntegralValueOutOfRange(..)
                    | Self::SqliteSingleThreadedMode
                    | Self::InvalidParameterName(_)
                    | Self::ExecuteReturnedResults
                    | Self::QueryReturnedNoRows
                    | Self::QueryReturnedMoreThanOneRow
                    | Self::InvalidColumnIndex(_)
                    | Self::InvalidColumnName(_)
                    | Self::InvalidColumnType(..)
                    | Self::InvalidPath(_)
                    | Self::InvalidParameterCount(..)
                    | Self::StatementChangedRows(_)
                    | Self::InvalidQuery
                    | Self::MultipleStatement => None,
                    Self::InvalidFunctionParameterType(..) => None,
                    Self::InvalidFilterParameterType(..) => None,
                    Self::UserFunctionError(ref err) => Some(&**err),
                    Self::FromSqlConversionFailure(_, _, ref err)
                    | Self::ToSqlConversionFailure(ref err) => Some(&**err),
                    Self::ModuleError(_) => None,
                    Self::UnwindingPanic => None,
                    Self::GetAuxWrongType => None,
                    Self::BlobSizeError => None,
                    Self::SqlInputError { ref error, .. } => Some(error),
                    Self::InitError(ref err) => Some(err),
                    Self::InvalidDatabaseIndex(_) => None,
                }
            }
        }

        impl Error
        {
            #[inline] #[must_use] pub fn sqlite_error(&self) -> Option<&ffi::Error>
            {
                match self
                {
                    Self::SqliteFailure(error, _) => Some(error),
                    _ => None,
                }
            }
            
            #[inline] #[must_use] pub fn sqlite_error_code(&self) -> Option<ffi::ErrorCode> { self.sqlite_error().map(|error| error.code) }
        }
        
        #[cold] pub fn error_from_sqlite_code(code: c_int, message: Option<String>) -> Error { Error::SqliteFailure(ffi::Error::new(code), message) }
        
        #[cold] pub unsafe fn error_from_handle(db: *mut ffi::sqlite3, code: c_int) -> Error { error_from_sqlite_code(code, error_msg(db, code)) }

        unsafe fn error_msg(db: *mut ffi::sqlite3, code: c_int) -> Option<String>
        {
            if db.is_null() || ffi::sqlite3_errcode(db) != code
            {
                let err_str = ffi::sqlite3_errstr(code);

                if err_str.is_null() { None }
                else { Some(errmsg_to_string(err_str)) }
            }
            else { Some(errmsg_to_string(ffi::sqlite3_errmsg(db))) }
        }

        pub unsafe fn decode_result_raw(db: *mut ffi::sqlite3, code: c_int) -> Result<()>
        {
            if code == ffi::SQLITE_OK { Ok(()) }
            else { Err(error_from_handle(db, code)) }
        }

        #[cold] pub unsafe fn error_with_offset(db: *mut ffi::sqlite3, code: c_int, sql: &str) -> Error
        {
            if db.is_null() { error_from_sqlite_code(code, None) }
            else
            {
                let error = ffi::Error::new(code);
                let msg = error_msg(db, code);
                if ffi::ErrorCode::Unknown == error.code
                {
                    let offset = ffi::sqlite3_error_offset(db);
                    if offset >= 0
                    {
                        return Error::SqlInputError
                        {
                            error,
                            msg: msg.unwrap_or("error".to_owned()),
                            sql: sql.to_owned(),
                            offset,
                        };
                    }
                }
                Error::SqliteFailure(error, msg)
            }
        }

        pub fn check(code: c_int) -> Result<()> {
            if code != ffi::SQLITE_OK {
                Err(error_from_sqlite_code(code, None))
            } else {
                Ok(())
            }
        }
        
        pub unsafe fn to_sqlite_error(e: &Error, m: *mut *mut c_char) -> c_int
        {
            match e
            {
                Error::SqliteFailure(e, s) => 
                {
                    if let Some(s) = s { *m = ::alloc::allocate(s); }
                    e.extended_code
                }
                
                err =>
                {
                    *err_msg = ::alloc::allocate(&err.to_string());
                    ffi::SQLITE_ERROR
                }
            }
        }
    }
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

pub mod system
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    libsqlite3 v0.36.0*/
    pub mod sql
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        */
        pub const SQLITE_DELETE: i32 = 9;
        pub const SQLITE_ERROR:  i32 = 1;
        pub const SQLITE_INSERT: i32 = 18;
        pub const SQLITE_OK: i32 = 0;
        pub const SQLITE_UPDATE: i32 = 23;
        
        pub type sqlite_int64 = ::ffi::c_longlong;
        pub type sqlite_uint64 = ::ffi::c_ulonglong;
        pub type sqlite3_int64 = sqlite_int64;
        pub type sqlite3_uint64 = sqlite_uint64;
        
        unsafe extern "C"
        {
            pub fn sqlite3_threadsafe() -> ::ffi::c_int;
            pub fn sqlite3_malloc64(arg1: sqlite3_uint64) -> *mut ::ffi::c_void;
        }
        
        #[repr(C)] #[derive(Debug, Copy, Clone)]
        pub struct sqlite3
        {
            _unused: [u8; 0],
        }
    }
    /*
    rusqlite v0.38.0*/
    pub mod sqlite
    {
        /*!
        */
        use ::
        {
            cell::{ RefCell },
            sync::{ Arc, Mutex },
            *,
        };

        use super::sql;
        /*
        */
        #[repr(i32)] #[non_exhaustive] #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum Action
        {
            UNKNOWN = -1,
            SQLITE_DELETE = sql::SQLITE_DELETE,
            SQLITE_INSERT = sql::SQLITE_INSERT,
            SQLITE_UPDATE = sql::SQLITE_UPDATE,
        }

        #[non_exhaustive] #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum TransactionOperation
        {
            Unknown,
            Begin,
            Release,
            Rollback,
        }

        #[non_exhaustive] #[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub struct AuthContext<'c>
        {
            pub action: AuthAction<'c>,
            pub database_name: Option<&'c str>,
            pub accessor: Option<&'c str>,
        }

        #[non_exhaustive] #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum Authorization
        {
            Allow,
            Ignore,
            Deny,
        }

        pub type BoxedAuthorizer = Box<dyn for<'c> FnMut(AuthContext<'c>) -> Authorization + Send + 'static>;
        
        pub struct InnerConnection
        {
            pub db:*mut sql::sqlite3,
            interrupt_lock:Arc<Mutex<*mut sql::sqlite3>>,
            pub commit_hook:Option<Box<dyn FnMut() -> bool + Send>>,
            pub rollback_hook:Option<Box<dyn FnMut() + Send>>,
            pub update_hook:Option<Box<dyn FnMut(Action, &str, &str, i64) + Send>>,
            pub progress_handler:Option<Box<dyn FnMut() -> bool + Send>>,
            pub authorizer:Option<BoxedAuthorizer>,
            owned:bool
        }

        #[non_exhaustive] #[derive( Copy, Clone )]
        pub enum TransactionBehavior
        {
            Deferred,
            Immediate,
            Exclusive
        }
        
        pub struct Connection
        {
            db:RefCell<InnerConnection>,
            transaction_behavior:TransactionBehavior,
        }
    }
}
/*
*/
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
// 1310 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
