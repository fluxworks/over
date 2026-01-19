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
    use std::convert::TryInto;
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
        use types::DataType;
        use ::
        {
            ffi::{ c_char, c_int, CStr, NulError },
            fmt::{ Debug },
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

        pub const UNKNOWN_COLUMN: usize = usize::MAX;
        pub type Result<T, E = Error> = result::Result<T, E>;

        #[non_exhaustive] #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum InitError
        {
            VersionMismatch { compile_time: i32, runtime: i32 },
            NullFunctionPointer,
        }
        
        impl fmt::Display for InitError
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
            {
                match *self
                {
                    Self::VersionMismatch
                    {
                        compile_time,
                        runtime
                    } => { write!(f, "SQLite version mismatch: {runtime} < {compile_time}") }
                    Self::NullFunctionPointer => { write!(f, "Some sqlite3_api_routines fields are null") }
                }
            }
        }
        
        impl error::Error for InitError {}

        #[non_exhaustive] #[derive(Debug)]
        pub enum Error
        {
            SqliteFailure(ffi::Error, Option<String>),
            SqliteSingleThreadedMode,
            FromSqlConversionFailure(usize, DataType, Box<dyn error::Error + Send + Sync + 'static>),
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
            InvalidColumnType(usize, String, DataType),
            StatementChangedRows(usize),
            InvalidFunctionParameterType(usize, DataType),
            InvalidFilterParameterType(usize, DataType),
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
            InitError(InitError),
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
                    (
                        Self::InvalidFunctionParameterType(i1, t1),
                        Self::InvalidFunctionParameterType(i2, t2),
                    ) => i1 == i2 && t1 == t2,
                    (
                        Self::InvalidFilterParameterType(i1, t1),
                        Self::InvalidFilterParameterType(i2, t2),
                    ) => i1 == i2 && t1 == t2,
                    (Self::InvalidQuery, Self::InvalidQuery) => true,
                    (Self::ModuleError(s1), Self::ModuleError(s2)) => s1 == s2,
                    (Self::UnwindingPanic, Self::UnwindingPanic) => true,
                    (Self::GetAuxWrongType, Self::GetAuxWrongType) => true,
                    (Self::InvalidParameterCount(i1, n1), Self::InvalidParameterCount(i2, n2)) => {
                        i1 == i2 && n1 == n2
                    }
                    (Self::BlobSizeError, Self::BlobSizeError) => true,
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
                    (Self::InitError(e1), Self::InitError(e2)) => e1 == e2,
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

        #[non_exhaustive] #[derive(Debug)]
        pub enum FromSqlError
        {
            InvalidType,
            OutOfRange(i64),
            InvalidBlobSize
            {
                expected_size: usize,
                blob_size: usize,
            },
            
            Other(Box<dyn error::Error + Send + Sync + 'static>),
        }

        impl error::Error for FromSqlError 
        {
            fn source(&self) -> Option<&(dyn error::Error + 'static)> {
                if let Self::Other(ref err) = self {
                    Some(&**err)
                } else {
                    None
                }
            }
        }

        impl fmt::Display for FromSqlError
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result 
            {
                match *self 
                {
                    Self::InvalidType => write!(f, "Invalid type"),
                    Self::OutOfRange(i) => write!(f, "Value {i} out of range"),
                    Self::InvalidBlobSize 
                    {
                        expected_size,
                        blob_size,
                    } => 
                    {
                        write!(
                            f,
                            "Cannot read {expected_size} byte value out of {blob_size} byte blob"
                        )
                    }
                    Self::Other(ref err) => Debug::fmt(&err, f),
                }
            }
        }
                
        impl From<FromSqlError> for Error
        {
            #[cold] fn from(err: FromSqlError) -> Self
            {
                match err
                {
                    FromSqlError::OutOfRange(val) => Self::IntegralValueOutOfRange(UNKNOWN_COLUMN, val),
                    FromSqlError::InvalidBlobSize { .. } => { Self::FromSqlConversionFailure(UNKNOWN_COLUMN, DataType::Blob, Box::new(err)) }
                    FromSqlError::Other(source) => { Self::FromSqlConversionFailure(UNKNOWN_COLUMN, DataType::Null, source) }
                    _ => Self::FromSqlConversionFailure(UNKNOWN_COLUMN, DataType::Null, Box::new(err)),
                }
            }
        }
        
        impl From<InitError> for Error
        {
            #[cold] fn from(err: InitError) -> Self { Self::InitError(err) }
        }

        impl fmt::Display for Error
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {
                match *self
                {
                    Self::SqliteFailure(ref err, None) => Debug::fmt(&err, f),
                    Self::SqliteFailure(_, Some(ref s)) => write!(f, "{s}"),
                    Self::SqliteSingleThreadedMode => write!( f, "SQLite was compiled or configured for single-threaded use only" ),
                    Self::FromSqlConversionFailure(i, ref t, ref err) =>
                    {
                        if i != UNKNOWN_COLUMN { write!(f, "Conversion error from type {t} at index: {i}, {err}") }
                        else { Debug::fmt(&err, f) }
                    }
                    Self::IntegralValueOutOfRange(col, val) =>
                    {
                        if col != UNKNOWN_COLUMN { write!(f, "Integer {val} out of range at index {col}") }
                        else { write!(f, "Integer {val} out of range") }
                    }
                    Self::Utf8Error(ref err) => Debug::fmt(&err, f),
                    Self::NulError(ref err) => Debug::fmt(&err, f),
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
                    Self::UserFunctionError(ref err) => Debug::fmt(&err, f),
                    Self::ToSqlConversionFailure(ref err) => Debug::fmt(&err, f),
                    Self::InvalidQuery => write!(f, "Query is not read-only"),
                    Self::ModuleError(ref desc) => write!(f, "{desc}"),
                    Self::UnwindingPanic => write!(f, "unwinding panic"),
                    Self::GetAuxWrongType => write!(f, "get_aux called with wrong type"),
                    Self::MultipleStatement => write!(f, "Multiple statements provided"),
                    Self::BlobSizeError => write!(f, "Blob size is insufficient"),
                    Self::SqlInputError
                    {
                        ref msg,
                        offset,
                        ref sql,
                        ..
                    } => write!(f, "{msg} in {sql} at offset {offset}"),
                    Self::InitError(ref err) => Debug::fmt(&err, f),
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
        
        pub unsafe fn errmsg_to_string(errmsg: *const c_char) -> String
        {
            CStr::from_ptr(errmsg).to_string_lossy().into_owned()
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

        pub fn check(code: c_int) -> Result<()>
        {
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
                    *m = ::alloc::allocate(&err.to_string());
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
            ffi::{ c_int, c_uint, c_longlong, c_ulonglong, CStr },
            *,
        };
        /*
        */
        pub const SQLITE_VERSION: &str = "3.51.2";
        pub const SQLITE_VERSION_NUMBER: i32 = 3051002;
        pub const SQLITE_SOURCE_ID: &str = "2026-01-09 17:27:48 b270f8339eb13b504d0b2ba154ebca966b7dde08e40c3ed7d559749818cb2075";
        pub const SQLITE_SCM_BRANCH: &str = "branch-3.51";
        pub const SQLITE_SCM_TAGS: &str = "release version-3.51.2";
        pub const SQLITE_SCM_DATETIME: &str = "2026-01-09T17:27:48.405Z";
        pub const SQLITE_OK: i32 = 0;
        pub const SQLITE_ERROR: i32 = 1;
        pub const SQLITE_INTERNAL: i32 = 2;
        pub const SQLITE_PERM: i32 = 3;
        pub const SQLITE_ABORT: i32 = 4;
        pub const SQLITE_BUSY: i32 = 5;
        pub const SQLITE_LOCKED: i32 = 6;
        pub const SQLITE_NOMEM: i32 = 7;
        pub const SQLITE_READONLY: i32 = 8;
        pub const SQLITE_INTERRUPT: i32 = 9;
        pub const SQLITE_IOERR: i32 = 10;
        pub const SQLITE_CORRUPT: i32 = 11;
        pub const SQLITE_NOTFOUND: i32 = 12;
        pub const SQLITE_FULL: i32 = 13;
        pub const SQLITE_CANTOPEN: i32 = 14;
        pub const SQLITE_PROTOCOL: i32 = 15;
        pub const SQLITE_EMPTY: i32 = 16;
        pub const SQLITE_SCHEMA: i32 = 17;
        pub const SQLITE_TOOBIG: i32 = 18;
        pub const SQLITE_CONSTRAINT: i32 = 19;
        pub const SQLITE_MISMATCH: i32 = 20;
        pub const SQLITE_MISUSE: i32 = 21;
        pub const SQLITE_NOLFS: i32 = 22;
        pub const SQLITE_AUTH: i32 = 23;
        pub const SQLITE_FORMAT: i32 = 24;
        pub const SQLITE_RANGE: i32 = 25;
        pub const SQLITE_NOTADB: i32 = 26;
        pub const SQLITE_NOTICE: i32 = 27;
        pub const SQLITE_WARNING: i32 = 28;
        pub const SQLITE_ROW: i32 = 100;
        pub const SQLITE_DONE: i32 = 101;
        pub const SQLITE_ERROR_MISSING_COLLSEQ: i32 = 257;
        pub const SQLITE_ERROR_RETRY: i32 = 513;
        pub const SQLITE_ERROR_SNAPSHOT: i32 = 769;
        pub const SQLITE_ERROR_RESERVESIZE: i32 = 1025;
        pub const SQLITE_ERROR_KEY: i32 = 1281;
        pub const SQLITE_ERROR_UNABLE: i32 = 1537;
        pub const SQLITE_IOERR_READ: i32 = 266;
        pub const SQLITE_IOERR_SHORT_READ: i32 = 522;
        pub const SQLITE_IOERR_WRITE: i32 = 778;
        pub const SQLITE_IOERR_FSYNC: i32 = 1034;
        pub const SQLITE_IOERR_DIR_FSYNC: i32 = 1290;
        pub const SQLITE_IOERR_TRUNCATE: i32 = 1546;
        pub const SQLITE_IOERR_FSTAT: i32 = 1802;
        pub const SQLITE_IOERR_UNLOCK: i32 = 2058;
        pub const SQLITE_IOERR_RDLOCK: i32 = 2314;
        pub const SQLITE_IOERR_DELETE: i32 = 2570;
        pub const SQLITE_IOERR_BLOCKED: i32 = 2826;
        pub const SQLITE_IOERR_NOMEM: i32 = 3082;
        pub const SQLITE_IOERR_ACCESS: i32 = 3338;
        pub const SQLITE_IOERR_CHECKRESERVEDLOCK: i32 = 3594;
        pub const SQLITE_IOERR_LOCK: i32 = 3850;
        pub const SQLITE_IOERR_CLOSE: i32 = 4106;
        pub const SQLITE_IOERR_DIR_CLOSE: i32 = 4362;
        pub const SQLITE_IOERR_SHMOPEN: i32 = 4618;
        pub const SQLITE_IOERR_SHMSIZE: i32 = 4874;
        pub const SQLITE_IOERR_SHMLOCK: i32 = 5130;
        pub const SQLITE_IOERR_SHMMAP: i32 = 5386;
        pub const SQLITE_IOERR_SEEK: i32 = 5642;
        pub const SQLITE_IOERR_DELETE_NOENT: i32 = 5898;
        pub const SQLITE_IOERR_MMAP: i32 = 6154;
        pub const SQLITE_IOERR_GETTEMPPATH: i32 = 6410;
        pub const SQLITE_IOERR_CONVPATH: i32 = 6666;
        pub const SQLITE_IOERR_VNODE: i32 = 6922;
        pub const SQLITE_IOERR_AUTH: i32 = 7178;
        pub const SQLITE_IOERR_BEGIN_ATOMIC: i32 = 7434;
        pub const SQLITE_IOERR_COMMIT_ATOMIC: i32 = 7690;
        pub const SQLITE_IOERR_ROLLBACK_ATOMIC: i32 = 7946;
        pub const SQLITE_IOERR_DATA: i32 = 8202;
        pub const SQLITE_IOERR_CORRUPTFS: i32 = 8458;
        pub const SQLITE_IOERR_IN_PAGE: i32 = 8714;
        pub const SQLITE_IOERR_BADKEY: i32 = 8970;
        pub const SQLITE_IOERR_CODEC: i32 = 9226;
        pub const SQLITE_LOCKED_SHAREDCACHE: i32 = 262;
        pub const SQLITE_LOCKED_VTAB: i32 = 518;
        pub const SQLITE_BUSY_RECOVERY: i32 = 261;
        pub const SQLITE_BUSY_SNAPSHOT: i32 = 517;
        pub const SQLITE_BUSY_TIMEOUT: i32 = 773;
        pub const SQLITE_CANTOPEN_NOTEMPDIR: i32 = 270;
        pub const SQLITE_CANTOPEN_ISDIR: i32 = 526;
        pub const SQLITE_CANTOPEN_FULLPATH: i32 = 782;
        pub const SQLITE_CANTOPEN_CONVPATH: i32 = 1038;
        pub const SQLITE_CANTOPEN_DIRTYWAL: i32 = 1294;
        pub const SQLITE_CANTOPEN_SYMLINK: i32 = 1550;
        pub const SQLITE_CORRUPT_VTAB: i32 = 267;
        pub const SQLITE_CORRUPT_SEQUENCE: i32 = 523;
        pub const SQLITE_CORRUPT_INDEX: i32 = 779;
        pub const SQLITE_READONLY_RECOVERY: i32 = 264;
        pub const SQLITE_READONLY_CANTLOCK: i32 = 520;
        pub const SQLITE_READONLY_ROLLBACK: i32 = 776;
        pub const SQLITE_READONLY_DBMOVED: i32 = 1032;
        pub const SQLITE_READONLY_CANTINIT: i32 = 1288;
        pub const SQLITE_READONLY_DIRECTORY: i32 = 1544;
        pub const SQLITE_ABORT_ROLLBACK: i32 = 516;
        pub const SQLITE_CONSTRAINT_CHECK: i32 = 275;
        pub const SQLITE_CONSTRAINT_COMMITHOOK: i32 = 531;
        pub const SQLITE_CONSTRAINT_FOREIGNKEY: i32 = 787;
        pub const SQLITE_CONSTRAINT_FUNCTION: i32 = 1043;
        pub const SQLITE_CONSTRAINT_NOTNULL: i32 = 1299;
        pub const SQLITE_CONSTRAINT_PRIMARYKEY: i32 = 1555;
        pub const SQLITE_CONSTRAINT_TRIGGER: i32 = 1811;
        pub const SQLITE_CONSTRAINT_UNIQUE: i32 = 2067;
        pub const SQLITE_CONSTRAINT_VTAB: i32 = 2323;
        pub const SQLITE_CONSTRAINT_ROWID: i32 = 2579;
        pub const SQLITE_CONSTRAINT_PINNED: i32 = 2835;
        pub const SQLITE_CONSTRAINT_DATATYPE: i32 = 3091;
        pub const SQLITE_NOTICE_RECOVER_WAL: i32 = 283;
        pub const SQLITE_NOTICE_RECOVER_ROLLBACK: i32 = 539;
        pub const SQLITE_NOTICE_RBU: i32 = 795;
        pub const SQLITE_WARNING_AUTOINDEX: i32 = 284;
        pub const SQLITE_AUTH_USER: i32 = 279;
        pub const SQLITE_OK_LOAD_PERMANENTLY: i32 = 256;
        pub const SQLITE_OK_SYMLINK: i32 = 512;
        pub const SQLITE_OPEN_READONLY: i32 = 1;
        pub const SQLITE_OPEN_READWRITE: i32 = 2;
        pub const SQLITE_OPEN_CREATE: i32 = 4;
        pub const SQLITE_OPEN_DELETEONCLOSE: i32 = 8;
        pub const SQLITE_OPEN_EXCLUSIVE: i32 = 16;
        pub const SQLITE_OPEN_AUTOPROXY: i32 = 32;
        pub const SQLITE_OPEN_URI: i32 = 64;
        pub const SQLITE_OPEN_MEMORY: i32 = 128;
        pub const SQLITE_OPEN_MAIN_DB: i32 = 256;
        pub const SQLITE_OPEN_TEMP_DB: i32 = 512;
        pub const SQLITE_OPEN_TRANSIENT_DB: i32 = 1024;
        pub const SQLITE_OPEN_MAIN_JOURNAL: i32 = 2048;
        pub const SQLITE_OPEN_TEMP_JOURNAL: i32 = 4096;
        pub const SQLITE_OPEN_SUBJOURNAL: i32 = 8192;
        pub const SQLITE_OPEN_SUPER_JOURNAL: i32 = 16384;
        pub const SQLITE_OPEN_NOMUTEX: i32 = 32768;
        pub const SQLITE_OPEN_FULLMUTEX: i32 = 65536;
        pub const SQLITE_OPEN_SHAREDCACHE: i32 = 131072;
        pub const SQLITE_OPEN_PRIVATECACHE: i32 = 262144;
        pub const SQLITE_OPEN_WAL: i32 = 524288;
        pub const SQLITE_OPEN_NOFOLLOW: i32 = 16777216;
        pub const SQLITE_OPEN_EXRESCODE: i32 = 33554432;
        pub const SQLITE_OPEN_MASTER_JOURNAL: i32 = 16384;
        pub const SQLITE_IOCAP_ATOMIC: i32 = 1;
        pub const SQLITE_IOCAP_ATOMIC512: i32 = 2;
        pub const SQLITE_IOCAP_ATOMIC1K: i32 = 4;
        pub const SQLITE_IOCAP_ATOMIC2K: i32 = 8;
        pub const SQLITE_IOCAP_ATOMIC4K: i32 = 16;
        pub const SQLITE_IOCAP_ATOMIC8K: i32 = 32;
        pub const SQLITE_IOCAP_ATOMIC16K: i32 = 64;
        pub const SQLITE_IOCAP_ATOMIC32K: i32 = 128;
        pub const SQLITE_IOCAP_ATOMIC64K: i32 = 256;
        pub const SQLITE_IOCAP_SAFE_APPEND: i32 = 512;
        pub const SQLITE_IOCAP_SEQUENTIAL: i32 = 1024;
        pub const SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN: i32 = 2048;
        pub const SQLITE_IOCAP_POWERSAFE_OVERWRITE: i32 = 4096;
        pub const SQLITE_IOCAP_IMMUTABLE: i32 = 8192;
        pub const SQLITE_IOCAP_BATCH_ATOMIC: i32 = 16384;
        pub const SQLITE_IOCAP_SUBPAGE_READ: i32 = 32768;
        pub const SQLITE_LOCK_NONE: i32 = 0;
        pub const SQLITE_LOCK_SHARED: i32 = 1;
        pub const SQLITE_LOCK_RESERVED: i32 = 2;
        pub const SQLITE_LOCK_PENDING: i32 = 3;
        pub const SQLITE_LOCK_EXCLUSIVE: i32 = 4;
        pub const SQLITE_SYNC_NORMAL: i32 = 2;
        pub const SQLITE_SYNC_FULL: i32 = 3;
        pub const SQLITE_SYNC_DATAONLY: i32 = 16;
        pub const SQLITE_FCNTL_LOCKSTATE: i32 = 1;
        pub const SQLITE_FCNTL_GET_LOCKPROXYFILE: i32 = 2;
        pub const SQLITE_FCNTL_SET_LOCKPROXYFILE: i32 = 3;
        pub const SQLITE_FCNTL_LAST_ERRNO: i32 = 4;
        pub const SQLITE_FCNTL_SIZE_HINT: i32 = 5;
        pub const SQLITE_FCNTL_CHUNK_SIZE: i32 = 6;
        pub const SQLITE_FCNTL_FILE_POINTER: i32 = 7;
        pub const SQLITE_FCNTL_SYNC_OMITTED: i32 = 8;
        pub const SQLITE_FCNTL_WIN32_AV_RETRY: i32 = 9;
        pub const SQLITE_FCNTL_PERSIST_WAL: i32 = 10;
        pub const SQLITE_FCNTL_OVERWRITE: i32 = 11;
        pub const SQLITE_FCNTL_VFSNAME: i32 = 12;
        pub const SQLITE_FCNTL_POWERSAFE_OVERWRITE: i32 = 13;
        pub const SQLITE_FCNTL_PRAGMA: i32 = 14;
        pub const SQLITE_FCNTL_BUSYHANDLER: i32 = 15;
        pub const SQLITE_FCNTL_TEMPFILENAME: i32 = 16;
        pub const SQLITE_FCNTL_MMAP_SIZE: i32 = 18;
        pub const SQLITE_FCNTL_TRACE: i32 = 19;
        pub const SQLITE_FCNTL_HAS_MOVED: i32 = 20;
        pub const SQLITE_FCNTL_SYNC: i32 = 21;
        pub const SQLITE_FCNTL_COMMIT_PHASETWO: i32 = 22;
        pub const SQLITE_FCNTL_WIN32_SET_HANDLE: i32 = 23;
        pub const SQLITE_FCNTL_WAL_BLOCK: i32 = 24;
        pub const SQLITE_FCNTL_ZIPVFS: i32 = 25;
        pub const SQLITE_FCNTL_RBU: i32 = 26;
        pub const SQLITE_FCNTL_VFS_POINTER: i32 = 27;
        pub const SQLITE_FCNTL_JOURNAL_POINTER: i32 = 28;
        pub const SQLITE_FCNTL_WIN32_GET_HANDLE: i32 = 29;
        pub const SQLITE_FCNTL_PDB: i32 = 30;
        pub const SQLITE_FCNTL_BEGIN_ATOMIC_WRITE: i32 = 31;
        pub const SQLITE_FCNTL_COMMIT_ATOMIC_WRITE: i32 = 32;
        pub const SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE: i32 = 33;
        pub const SQLITE_FCNTL_LOCK_TIMEOUT: i32 = 34;
        pub const SQLITE_FCNTL_DATA_VERSION: i32 = 35;
        pub const SQLITE_FCNTL_SIZE_LIMIT: i32 = 36;
        pub const SQLITE_FCNTL_CKPT_DONE: i32 = 37;
        pub const SQLITE_FCNTL_RESERVE_BYTES: i32 = 38;
        pub const SQLITE_FCNTL_CKPT_START: i32 = 39;
        pub const SQLITE_FCNTL_EXTERNAL_READER: i32 = 40;
        pub const SQLITE_FCNTL_CKSM_FILE: i32 = 41;
        pub const SQLITE_FCNTL_RESET_CACHE: i32 = 42;
        pub const SQLITE_FCNTL_NULL_IO: i32 = 43;
        pub const SQLITE_FCNTL_BLOCK_ON_CONNECT: i32 = 44;
        pub const SQLITE_FCNTL_FILESTAT: i32 = 45;
        pub const SQLITE_GET_LOCKPROXYFILE: i32 = 2;
        pub const SQLITE_SET_LOCKPROXYFILE: i32 = 3;
        pub const SQLITE_LAST_ERRNO: i32 = 4;
        pub const SQLITE_ACCESS_EXISTS: i32 = 0;
        pub const SQLITE_ACCESS_READWRITE: i32 = 1;
        pub const SQLITE_ACCESS_READ: i32 = 2;
        pub const SQLITE_SHM_UNLOCK: i32 = 1;
        pub const SQLITE_SHM_LOCK: i32 = 2;
        pub const SQLITE_SHM_SHARED: i32 = 4;
        pub const SQLITE_SHM_EXCLUSIVE: i32 = 8;
        pub const SQLITE_SHM_NLOCK: i32 = 8;
        pub const SQLITE_CONFIG_SINGLETHREAD: i32 = 1;
        pub const SQLITE_CONFIG_MULTITHREAD: i32 = 2;
        pub const SQLITE_CONFIG_SERIALIZED: i32 = 3;
        pub const SQLITE_CONFIG_MALLOC: i32 = 4;
        pub const SQLITE_CONFIG_GETMALLOC: i32 = 5;
        pub const SQLITE_CONFIG_SCRATCH: i32 = 6;
        pub const SQLITE_CONFIG_PAGECACHE: i32 = 7;
        pub const SQLITE_CONFIG_HEAP: i32 = 8;
        pub const SQLITE_CONFIG_MEMSTATUS: i32 = 9;
        pub const SQLITE_CONFIG_MUTEX: i32 = 10;
        pub const SQLITE_CONFIG_GETMUTEX: i32 = 11;
        pub const SQLITE_CONFIG_LOOKASIDE: i32 = 13;
        pub const SQLITE_CONFIG_PCACHE: i32 = 14;
        pub const SQLITE_CONFIG_GETPCACHE: i32 = 15;
        pub const SQLITE_CONFIG_LOG: i32 = 16;
        pub const SQLITE_CONFIG_URI: i32 = 17;
        pub const SQLITE_CONFIG_PCACHE2: i32 = 18;
        pub const SQLITE_CONFIG_GETPCACHE2: i32 = 19;
        pub const SQLITE_CONFIG_COVERING_INDEX_SCAN: i32 = 20;
        pub const SQLITE_CONFIG_SQLLOG: i32 = 21;
        pub const SQLITE_CONFIG_MMAP_SIZE: i32 = 22;
        pub const SQLITE_CONFIG_WIN32_HEAPSIZE: i32 = 23;
        pub const SQLITE_CONFIG_PCACHE_HDRSZ: i32 = 24;
        pub const SQLITE_CONFIG_PMASZ: i32 = 25;
        pub const SQLITE_CONFIG_STMTJRNL_SPILL: i32 = 26;
        pub const SQLITE_CONFIG_SMALL_MALLOC: i32 = 27;
        pub const SQLITE_CONFIG_SORTERREF_SIZE: i32 = 28;
        pub const SQLITE_CONFIG_MEMDB_MAXSIZE: i32 = 29;
        pub const SQLITE_CONFIG_ROWID_IN_VIEW: i32 = 30;
        pub const SQLITE_DBCONFIG_MAINDBNAME: i32 = 1000;
        pub const SQLITE_DBCONFIG_LOOKASIDE: i32 = 1001;
        pub const SQLITE_DBCONFIG_ENABLE_FKEY: i32 = 1002;
        pub const SQLITE_DBCONFIG_ENABLE_TRIGGER: i32 = 1003;
        pub const SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER: i32 = 1004;
        pub const SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION: i32 = 1005;
        pub const SQLITE_DBCONFIG_NO_CKPT_ON_CLOSE: i32 = 1006;
        pub const SQLITE_DBCONFIG_ENABLE_QPSG: i32 = 1007;
        pub const SQLITE_DBCONFIG_TRIGGER_EQP: i32 = 1008;
        pub const SQLITE_DBCONFIG_RESET_DATABASE: i32 = 1009;
        pub const SQLITE_DBCONFIG_DEFENSIVE: i32 = 1010;
        pub const SQLITE_DBCONFIG_WRITABLE_SCHEMA: i32 = 1011;
        pub const SQLITE_DBCONFIG_LEGACY_ALTER_TABLE: i32 = 1012;
        pub const SQLITE_DBCONFIG_DQS_DML: i32 = 1013;
        pub const SQLITE_DBCONFIG_DQS_DDL: i32 = 1014;
        pub const SQLITE_DBCONFIG_ENABLE_VIEW: i32 = 1015;
        pub const SQLITE_DBCONFIG_LEGACY_FILE_FORMAT: i32 = 1016;
        pub const SQLITE_DBCONFIG_TRUSTED_SCHEMA: i32 = 1017;
        pub const SQLITE_DBCONFIG_STMT_SCANSTATUS: i32 = 1018;
        pub const SQLITE_DBCONFIG_REVERSE_SCANORDER: i32 = 1019;
        pub const SQLITE_DBCONFIG_ENABLE_ATTACH_CREATE: i32 = 1020;
        pub const SQLITE_DBCONFIG_ENABLE_ATTACH_WRITE: i32 = 1021;
        pub const SQLITE_DBCONFIG_ENABLE_COMMENTS: i32 = 1022;
        pub const SQLITE_DBCONFIG_MAX: i32 = 1022;
        pub const SQLITE_SETLK_BLOCK_ON_CONNECT: i32 = 1;
        pub const SQLITE_DENY: i32 = 1;
        pub const SQLITE_IGNORE: i32 = 2;
        pub const SQLITE_CREATE_INDEX: i32 = 1;
        pub const SQLITE_CREATE_TABLE: i32 = 2;
        pub const SQLITE_CREATE_TEMP_INDEX: i32 = 3;
        pub const SQLITE_CREATE_TEMP_TABLE: i32 = 4;
        pub const SQLITE_CREATE_TEMP_TRIGGER: i32 = 5;
        pub const SQLITE_CREATE_TEMP_VIEW: i32 = 6;
        pub const SQLITE_CREATE_TRIGGER: i32 = 7;
        pub const SQLITE_CREATE_VIEW: i32 = 8;
        pub const SQLITE_DELETE: i32 = 9;
        pub const SQLITE_DROP_INDEX: i32 = 10;
        pub const SQLITE_DROP_TABLE: i32 = 11;
        pub const SQLITE_DROP_TEMP_INDEX: i32 = 12;
        pub const SQLITE_DROP_TEMP_TABLE: i32 = 13;
        pub const SQLITE_DROP_TEMP_TRIGGER: i32 = 14;
        pub const SQLITE_DROP_TEMP_VIEW: i32 = 15;
        pub const SQLITE_DROP_TRIGGER: i32 = 16;
        pub const SQLITE_DROP_VIEW: i32 = 17;
        pub const SQLITE_INSERT: i32 = 18;
        pub const SQLITE_PRAGMA: i32 = 19;
        pub const SQLITE_READ: i32 = 20;
        pub const SQLITE_SELECT: i32 = 21;
        pub const SQLITE_TRANSACTION: i32 = 22;
        pub const SQLITE_UPDATE: i32 = 23;
        pub const SQLITE_ATTACH: i32 = 24;
        pub const SQLITE_DETACH: i32 = 25;
        pub const SQLITE_ALTER_TABLE: i32 = 26;
        pub const SQLITE_REINDEX: i32 = 27;
        pub const SQLITE_ANALYZE: i32 = 28;
        pub const SQLITE_CREATE_VTABLE: i32 = 29;
        pub const SQLITE_DROP_VTABLE: i32 = 30;
        pub const SQLITE_FUNCTION: i32 = 31;
        pub const SQLITE_SAVEPOINT: i32 = 32;
        pub const SQLITE_COPY: i32 = 0;
        pub const SQLITE_RECURSIVE: i32 = 33;
        pub const SQLITE_TRACE_STMT: c_uint = 1;
        pub const SQLITE_TRACE_PROFILE: c_uint = 2;
        pub const SQLITE_TRACE_ROW: c_uint = 4;
        pub const SQLITE_TRACE_CLOSE: c_uint = 8;
        pub const SQLITE_LIMIT_LENGTH: i32 = 0;
        pub const SQLITE_LIMIT_SQL_LENGTH: i32 = 1;
        pub const SQLITE_LIMIT_COLUMN: i32 = 2;
        pub const SQLITE_LIMIT_EXPR_DEPTH: i32 = 3;
        pub const SQLITE_LIMIT_COMPOUND_SELECT: i32 = 4;
        pub const SQLITE_LIMIT_VDBE_OP: i32 = 5;
        pub const SQLITE_LIMIT_FUNCTION_ARG: i32 = 6;
        pub const SQLITE_LIMIT_ATTACHED: i32 = 7;
        pub const SQLITE_LIMIT_LIKE_PATTERN_LENGTH: i32 = 8;
        pub const SQLITE_LIMIT_VARIABLE_NUMBER: i32 = 9;
        pub const SQLITE_LIMIT_TRIGGER_DEPTH: i32 = 10;
        pub const SQLITE_LIMIT_WORKER_THREADS: i32 = 11;
        pub const SQLITE_PREPARE_PERSISTENT: c_uint = 1;
        pub const SQLITE_PREPARE_NORMALIZE: c_uint = 2;
        pub const SQLITE_PREPARE_NO_VTAB: c_uint = 4;
        pub const SQLITE_PREPARE_DONT_LOG: c_uint = 16;
        pub const SQLITE_INTEGER: i32 = 1;
        pub const SQLITE_FLOAT: i32 = 2;
        pub const SQLITE_BLOB: i32 = 4;
        pub const SQLITE_NULL: i32 = 5;
        pub const SQLITE_TEXT: i32 = 3;
        pub const SQLITE3_TEXT: i32 = 3;
        pub const SQLITE_UTF8: i32 = 1;
        pub const SQLITE_UTF16LE: i32 = 2;
        pub const SQLITE_UTF16BE: i32 = 3;
        pub const SQLITE_UTF16: i32 = 4;
        pub const SQLITE_ANY: i32 = 5;
        pub const SQLITE_UTF16_ALIGNED: i32 = 8;
        pub const SQLITE_DETERMINISTIC: i32 = 2048;
        pub const SQLITE_DIRECTONLY: i32 = 524288;
        pub const SQLITE_SUBTYPE: i32 = 1048576;
        pub const SQLITE_INNOCUOUS: i32 = 2097152;
        pub const SQLITE_RESULT_SUBTYPE: i32 = 16777216;
        pub const SQLITE_SELFORDER1: i32 = 33554432;
        pub const SQLITE_WIN32_DATA_DIRECTORY_TYPE: i32 = 1;
        pub const SQLITE_WIN32_TEMP_DIRECTORY_TYPE: i32 = 2;
        pub const SQLITE_TXN_NONE: i32 = 0;
        pub const SQLITE_TXN_READ: i32 = 1;
        pub const SQLITE_TXN_WRITE: i32 = 2;
        pub const SQLITE_INDEX_SCAN_UNIQUE: i32 = 1;
        pub const SQLITE_INDEX_SCAN_HEX: i32 = 2;
        pub const SQLITE_INDEX_CONSTRAINT_EQ: i32 = 2;
        pub const SQLITE_INDEX_CONSTRAINT_GT: i32 = 4;
        pub const SQLITE_INDEX_CONSTRAINT_LE: i32 = 8;
        pub const SQLITE_INDEX_CONSTRAINT_LT: i32 = 16;
        pub const SQLITE_INDEX_CONSTRAINT_GE: i32 = 32;
        pub const SQLITE_INDEX_CONSTRAINT_MATCH: i32 = 64;
        pub const SQLITE_INDEX_CONSTRAINT_LIKE: i32 = 65;
        pub const SQLITE_INDEX_CONSTRAINT_GLOB: i32 = 66;
        pub const SQLITE_INDEX_CONSTRAINT_REGEXP: i32 = 67;
        pub const SQLITE_INDEX_CONSTRAINT_NE: i32 = 68;
        pub const SQLITE_INDEX_CONSTRAINT_ISNOT: i32 = 69;
        pub const SQLITE_INDEX_CONSTRAINT_ISNOTNULL: i32 = 70;
        pub const SQLITE_INDEX_CONSTRAINT_ISNULL: i32 = 71;
        pub const SQLITE_INDEX_CONSTRAINT_IS: i32 = 72;
        pub const SQLITE_INDEX_CONSTRAINT_LIMIT: i32 = 73;
        pub const SQLITE_INDEX_CONSTRAINT_OFFSET: i32 = 74;
        pub const SQLITE_INDEX_CONSTRAINT_FUNCTION: i32 = 150;
        pub const SQLITE_MUTEX_FAST: i32 = 0;
        pub const SQLITE_MUTEX_RECURSIVE: i32 = 1;
        pub const SQLITE_MUTEX_STATIC_MAIN: i32 = 2;
        pub const SQLITE_MUTEX_STATIC_MEM: i32 = 3;
        pub const SQLITE_MUTEX_STATIC_MEM2: i32 = 4;
        pub const SQLITE_MUTEX_STATIC_OPEN: i32 = 4;
        pub const SQLITE_MUTEX_STATIC_PRNG: i32 = 5;
        pub const SQLITE_MUTEX_STATIC_LRU: i32 = 6;
        pub const SQLITE_MUTEX_STATIC_LRU2: i32 = 7;
        pub const SQLITE_MUTEX_STATIC_PMEM: i32 = 7;
        pub const SQLITE_MUTEX_STATIC_APP1: i32 = 8;
        pub const SQLITE_MUTEX_STATIC_APP2: i32 = 9;
        pub const SQLITE_MUTEX_STATIC_APP3: i32 = 10;
        pub const SQLITE_MUTEX_STATIC_VFS1: i32 = 11;
        pub const SQLITE_MUTEX_STATIC_VFS2: i32 = 12;
        pub const SQLITE_MUTEX_STATIC_VFS3: i32 = 13;
        pub const SQLITE_MUTEX_STATIC_MASTER: i32 = 2;
        pub const SQLITE_TESTCTRL_FIRST: i32 = 5;
        pub const SQLITE_TESTCTRL_PRNG_SAVE: i32 = 5;
        pub const SQLITE_TESTCTRL_PRNG_RESTORE: i32 = 6;
        pub const SQLITE_TESTCTRL_PRNG_RESET: i32 = 7;
        pub const SQLITE_TESTCTRL_FK_NO_ACTION: i32 = 7;
        pub const SQLITE_TESTCTRL_BITVEC_TEST: i32 = 8;
        pub const SQLITE_TESTCTRL_FAULT_INSTALL: i32 = 9;
        pub const SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS: i32 = 10;
        pub const SQLITE_TESTCTRL_PENDING_BYTE: i32 = 11;
        pub const SQLITE_TESTCTRL_ASSERT: i32 = 12;
        pub const SQLITE_TESTCTRL_ALWAYS: i32 = 13;
        pub const SQLITE_TESTCTRL_RESERVE: i32 = 14;
        pub const SQLITE_TESTCTRL_JSON_SELFCHECK: i32 = 14;
        pub const SQLITE_TESTCTRL_OPTIMIZATIONS: i32 = 15;
        pub const SQLITE_TESTCTRL_ISKEYWORD: i32 = 16;
        pub const SQLITE_TESTCTRL_GETOPT: i32 = 16;
        pub const SQLITE_TESTCTRL_SCRATCHMALLOC: i32 = 17;
        pub const SQLITE_TESTCTRL_INTERNAL_FUNCTIONS: i32 = 17;
        pub const SQLITE_TESTCTRL_LOCALTIME_FAULT: i32 = 18;
        pub const SQLITE_TESTCTRL_EXPLAIN_STMT: i32 = 19;
        pub const SQLITE_TESTCTRL_ONCE_RESET_THRESHOLD: i32 = 19;
        pub const SQLITE_TESTCTRL_NEVER_CORRUPT: i32 = 20;
        pub const SQLITE_TESTCTRL_VDBE_COVERAGE: i32 = 21;
        pub const SQLITE_TESTCTRL_BYTEORDER: i32 = 22;
        pub const SQLITE_TESTCTRL_ISINIT: i32 = 23;
        pub const SQLITE_TESTCTRL_SORTER_MMAP: i32 = 24;
        pub const SQLITE_TESTCTRL_IMPOSTER: i32 = 25;
        pub const SQLITE_TESTCTRL_PARSER_COVERAGE: i32 = 26;
        pub const SQLITE_TESTCTRL_RESULT_INTREAL: i32 = 27;
        pub const SQLITE_TESTCTRL_PRNG_SEED: i32 = 28;
        pub const SQLITE_TESTCTRL_EXTRA_SCHEMA_CHECKS: i32 = 29;
        pub const SQLITE_TESTCTRL_SEEK_COUNT: i32 = 30;
        pub const SQLITE_TESTCTRL_TRACEFLAGS: i32 = 31;
        pub const SQLITE_TESTCTRL_TUNE: i32 = 32;
        pub const SQLITE_TESTCTRL_LOGEST: i32 = 33;
        pub const SQLITE_TESTCTRL_USELONGDOUBLE: i32 = 34;
        pub const SQLITE_TESTCTRL_LAST: i32 = 34;
        pub const SQLITE_STATUS_MEMORY_USED: i32 = 0;
        pub const SQLITE_STATUS_PAGECACHE_USED: i32 = 1;
        pub const SQLITE_STATUS_PAGECACHE_OVERFLOW: i32 = 2;
        pub const SQLITE_STATUS_SCRATCH_USED: i32 = 3;
        pub const SQLITE_STATUS_SCRATCH_OVERFLOW: i32 = 4;
        pub const SQLITE_STATUS_MALLOC_SIZE: i32 = 5;
        pub const SQLITE_STATUS_PARSER_STACK: i32 = 6;
        pub const SQLITE_STATUS_PAGECACHE_SIZE: i32 = 7;
        pub const SQLITE_STATUS_SCRATCH_SIZE: i32 = 8;
        pub const SQLITE_STATUS_MALLOC_COUNT: i32 = 9;
        pub const SQLITE_DBSTATUS_LOOKASIDE_USED: i32 = 0;
        pub const SQLITE_DBSTATUS_CACHE_USED: i32 = 1;
        pub const SQLITE_DBSTATUS_SCHEMA_USED: i32 = 2;
        pub const SQLITE_DBSTATUS_STMT_USED: i32 = 3;
        pub const SQLITE_DBSTATUS_LOOKASIDE_HIT: i32 = 4;
        pub const SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE: i32 = 5;
        pub const SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL: i32 = 6;
        pub const SQLITE_DBSTATUS_CACHE_HIT: i32 = 7;
        pub const SQLITE_DBSTATUS_CACHE_MISS: i32 = 8;
        pub const SQLITE_DBSTATUS_CACHE_WRITE: i32 = 9;
        pub const SQLITE_DBSTATUS_DEFERRED_FKS: i32 = 10;
        pub const SQLITE_DBSTATUS_CACHE_USED_SHARED: i32 = 11;
        pub const SQLITE_DBSTATUS_CACHE_SPILL: i32 = 12;
        pub const SQLITE_DBSTATUS_TEMPBUF_SPILL: i32 = 13;
        pub const SQLITE_DBSTATUS_MAX: i32 = 13;
        pub const SQLITE_STMTSTATUS_FULLSCAN_STEP: i32 = 1;
        pub const SQLITE_STMTSTATUS_SORT: i32 = 2;
        pub const SQLITE_STMTSTATUS_AUTOINDEX: i32 = 3;
        pub const SQLITE_STMTSTATUS_VM_STEP: i32 = 4;
        pub const SQLITE_STMTSTATUS_REPREPARE: i32 = 5;
        pub const SQLITE_STMTSTATUS_RUN: i32 = 6;
        pub const SQLITE_STMTSTATUS_FILTER_MISS: i32 = 7;
        pub const SQLITE_STMTSTATUS_FILTER_HIT: i32 = 8;
        pub const SQLITE_STMTSTATUS_MEMUSED: i32 = 99;
        pub const SQLITE_CHECKPOINT_NOOP: i32 = -1;
        pub const SQLITE_CHECKPOINT_PASSIVE: i32 = 0;
        pub const SQLITE_CHECKPOINT_FULL: i32 = 1;
        pub const SQLITE_CHECKPOINT_RESTART: i32 = 2;
        pub const SQLITE_CHECKPOINT_TRUNCATE: i32 = 3;
        pub const SQLITE_VTAB_CONSTRAINT_SUPPORT: i32 = 1;
        pub const SQLITE_VTAB_INNOCUOUS: i32 = 2;
        pub const SQLITE_VTAB_DIRECTONLY: i32 = 3;
        pub const SQLITE_VTAB_USES_ALL_SCHEMAS: i32 = 4;
        pub const SQLITE_ROLLBACK: i32 = 1;
        pub const SQLITE_FAIL: i32 = 3;
        pub const SQLITE_REPLACE: i32 = 5;
        pub const SQLITE_SCANSTAT_NLOOP: i32 = 0;
        pub const SQLITE_SCANSTAT_NVISIT: i32 = 1;
        pub const SQLITE_SCANSTAT_EST: i32 = 2;
        pub const SQLITE_SCANSTAT_NAME: i32 = 3;
        pub const SQLITE_SCANSTAT_EXPLAIN: i32 = 4;
        pub const SQLITE_SCANSTAT_SELECTID: i32 = 5;
        pub const SQLITE_SCANSTAT_PARENTID: i32 = 6;
        pub const SQLITE_SCANSTAT_NCYCLE: i32 = 7;
        pub const SQLITE_SCANSTAT_COMPLEX: i32 = 1;
        pub const SQLITE_SERIALIZE_NOCOPY: c_uint = 1;
        pub const SQLITE_DESERIALIZE_FREEONCLOSE: c_uint = 1;
        pub const SQLITE_DESERIALIZE_RESIZEABLE: c_uint = 2;
        pub const SQLITE_DESERIALIZE_READONLY: c_uint = 4;
        pub const SQLITE_CARRAY_INT32: i32 = 0;
        pub const SQLITE_CARRAY_INT64: i32 = 1;
        pub const SQLITE_CARRAY_DOUBLE: i32 = 2;
        pub const SQLITE_CARRAY_TEXT: i32 = 3;
        pub const SQLITE_CARRAY_BLOB: i32 = 4;
        pub const CARRAY_INT32: i32 = 0;
        pub const CARRAY_INT64: i32 = 1;
        pub const CARRAY_DOUBLE: i32 = 2;
        pub const CARRAY_TEXT: i32 = 3;
        pub const CARRAY_BLOB: i32 = 4;
        pub const NOT_WITHIN: i32 = 0;
        pub const PARTLY_WITHIN: i32 = 1;
        pub const FULLY_WITHIN: i32 = 2;
        pub const SQLITE_SESSION_OBJCONFIG_SIZE: i32 = 1;
        pub const SQLITE_SESSION_OBJCONFIG_ROWID: i32 = 2;
        pub const SQLITE_CHANGESETSTART_INVERT: i32 = 2;
        pub const SQLITE_CHANGESETAPPLY_NOSAVEPOINT: i32 = 1;
        pub const SQLITE_CHANGESETAPPLY_INVERT: i32 = 2;
        pub const SQLITE_CHANGESETAPPLY_IGNORENOOP: i32 = 4;
        pub const SQLITE_CHANGESETAPPLY_FKNOACTION: i32 = 8;
        pub const SQLITE_CHANGESET_DATA: i32 = 1;
        pub const SQLITE_CHANGESET_NOTFOUND: i32 = 2;
        pub const SQLITE_CHANGESET_CONFLICT: i32 = 3;
        pub const SQLITE_CHANGESET_CONSTRAINT: i32 = 4;
        pub const SQLITE_CHANGESET_FOREIGN_KEY: i32 = 5;
        pub const SQLITE_CHANGESET_OMIT: i32 = 0;
        pub const SQLITE_CHANGESET_REPLACE: i32 = 1;
        pub const SQLITE_CHANGESET_ABORT: i32 = 2;
        pub const SQLITE_SESSION_CONFIG_STRMSIZE: i32 = 1;
        pub const FTS5_TOKENIZE_QUERY: i32 = 1;
        pub const FTS5_TOKENIZE_PREFIX: i32 = 2;
        pub const FTS5_TOKENIZE_DOCUMENT: i32 = 4;
        pub const FTS5_TOKENIZE_AUX: i32 = 8;
        pub const FTS5_TOKEN_COLOCATED: i32 = 1;
        
        pub type sqlite_int64 = c_longlong;
        pub type sqlite_uint64 = c_ulonglong;
        pub type sqlite3_int64 = sqlite_int64;
        pub type sqlite3_uint64 = sqlite_uint64;
        
        unsafe extern "C"
        {
            pub fn sqlite3_errcode(db: *mut sqlite3) -> ::ffi::c_int;
            pub fn sqlite3_errstr(arg1: ::ffi::c_int) -> *const ::ffi::c_char;
            pub fn sqlite3_threadsafe() -> ::ffi::c_int;
            pub fn sqlite3_malloc64(arg1: sqlite3_uint64) -> *mut ::ffi::c_void;
            pub fn sqlite3_errmsg(arg1: *mut sqlite3) -> *const ::ffi::c_char;
            pub fn sqlite3_error_offset(db: *mut sqlite3) -> ::ffi::c_int;
            pub fn sqlite3_set_errmsg( db: *mut sqlite3, errcode: ::ffi::c_int, zErrMsg: *const ::ffi::c_char ) -> ::ffi::c_int;
        }
        
        #[repr(C)] #[derive(Debug, Copy, Clone)]
        pub struct sqlite3
        {
            _unused: [u8; 0],
        }

        #[non_exhaustive] #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum ErrorCode 
        {
            InternalMalfunction,
            PermissionDenied,
            OperationAborted,
            DatabaseBusy,
            DatabaseLocked,
            OutOfMemory,
            ReadOnly,
            OperationInterrupted,
            SystemIoFailure,
            DatabaseCorrupt,
            NotFound,
            DiskFull,
            CannotOpen,
            FileLockingProtocolFailed,
            SchemaChanged,
            TooBig,
            ConstraintViolation,
            TypeMismatch,
            ApiMisuse,
            NoLargeFileSupport,
            AuthorizationForStatementDenied,
            ParameterOutOfRange,
            NotADatabase,
            Unknown,
        }

        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct Error
        {
            pub code: ErrorCode,
            pub extended_code: c_int,
        }
        
        impl Error
        {
            #[must_use]
            pub fn new(result_code: c_int) -> Self 
            {
                let code = match result_code & 0xff 
                {
                    SQLITE_INTERNAL => ErrorCode::InternalMalfunction,
                    SQLITE_PERM => ErrorCode::PermissionDenied,
                    SQLITE_ABORT => ErrorCode::OperationAborted,
                    SQLITE_BUSY => ErrorCode::DatabaseBusy,
                    SQLITE_LOCKED => ErrorCode::DatabaseLocked,
                    SQLITE_NOMEM => ErrorCode::OutOfMemory,
                    SQLITE_READONLY => ErrorCode::ReadOnly,
                    SQLITE_INTERRUPT => ErrorCode::OperationInterrupted,
                    SQLITE_IOERR => ErrorCode::SystemIoFailure,
                    SQLITE_CORRUPT => ErrorCode::DatabaseCorrupt,
                    SQLITE_NOTFOUND => ErrorCode::NotFound,
                    SQLITE_FULL => ErrorCode::DiskFull,
                    SQLITE_CANTOPEN => ErrorCode::CannotOpen,
                    SQLITE_PROTOCOL => ErrorCode::FileLockingProtocolFailed,
                    SQLITE_SCHEMA => ErrorCode::SchemaChanged,
                    SQLITE_TOOBIG => ErrorCode::TooBig,
                    SQLITE_CONSTRAINT => ErrorCode::ConstraintViolation,
                    SQLITE_MISMATCH => ErrorCode::TypeMismatch,
                    SQLITE_MISUSE => ErrorCode::ApiMisuse,
                    SQLITE_NOLFS => ErrorCode::NoLargeFileSupport,
                    SQLITE_AUTH => ErrorCode::AuthorizationForStatementDenied,
                    SQLITE_RANGE => ErrorCode::ParameterOutOfRange,
                    SQLITE_NOTADB => ErrorCode::NotADatabase,
                    _ => ErrorCode::Unknown,
                };

                Self {
                    code,
                    extended_code: result_code,
                }
            }
        }

        impl fmt::Display for Error
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {
                write!
                (
                    f,
                    "Error code {}: {}",
                    self.extended_code,
                    code_to_str(self.extended_code)
                )
            }
        }

        impl error::Error for Error
        {
            fn description(&self) -> &str { code_to_str(self.extended_code) }
        }

        #[must_use] pub fn code_to_str(code: c_int) -> &'static str
        {
            unsafe
            {
                let err_str = sqlite3_errstr(code);
                if err_str.is_null() { "Unknown errod code" }
                else { CStr::from_ptr(err_str).to_str().unwrap() }
            }
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

pub mod types
{
    use ::
    {
        *,
    };

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum DataType
    {
        Null,
        Integer,
        Real,
        Text,
        Blob,
    }

    impl fmt::Display for DataType 
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match *self {
                Self::Null => f.pad("Null"),
                Self::Integer => f.pad("Integer"),
                Self::Real => f.pad("Real"),
                Self::Text => f.pad("Text"),
                Self::Blob => f.pad("Blob"),
            }
        }
    }
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
// 2045 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
