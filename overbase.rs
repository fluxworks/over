//! OVER: the best data format.
//! OVERBASE | File and Flash database for OVER format
#![feature
(
	
)]

#![allow
(
	unused_imports,
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
#[macro_use]
extern crate lazy_static;
extern crate num_bigint;
extern crate num_rational;
extern crate num_traits;

*/
/// Indent step in .over files.
pub const INDENT_STEP: usize = 4;
/// Result type for this crate.
pub type OverResult<T> = Result<T, OverError>;

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
	#[macro_export] macro_rules! map
	{
		{ } => { ::collections::HashMap::new() };		
		{ $( $key:expr => $value:expr ),+ , } => { map!{ $( $key => $value),+ } };
		{ $( $key:expr => $value:expr ),* } =>
		{{
				let mut _map = ::collections::HashMap::new();
				$( let _ = _map.insert($key, $value); )*
				_map
		}}
	}
	/// Given an int, creates and returns a `BigInt`.
	#[macro_export] macro_rules! int
	{
		($int:expr) => {{
			use num_bigint::BigInt;

			let _b: BigInt = $int.into();
			_b
		}};
	}
	/// Given two ints, creates and returns a `BigRational`.
	#[macro_export] macro_rules! frac
	{
		($int1:expr, $int2:expr) => {{
			::num_rational::BigRational::new($int1.into(), $int2.into())
		}};
	}
	/// Given a list of elements, converts each element to a `Value` and returns an `Arr` containing a
	/// vector of the values.
	#[macro_export] macro_rules! arr
	{
		[] => {
			$crate::arr::Arr::from_vec(vec![]).unwrap()
		};
		[ $( $elem:expr ),+ , ] => {
			// Rule with trailing comma.
			try_arr![ $( $elem ),+ ].unwrap()
		};
		[ $( $elem:expr ),+ ] => {
			try_arr![ $( $elem ),+ ].unwrap()
		};
	}
	/// Given a list of elements, converts each element to a `Value` and returns an `Arr` containing a
	/// vector of the values.
	#[macro_export] macro_rules! try_arr
	{
		[ $( $elem:expr ),+ , ] => {
			// Rule with trailing comma.
			try_arr![ $( $elem ),+ ]
		};
		[ $( $elem:expr ),+ ] => {
			{
				$crate::arr::Arr::from_vec(vec![ $( $elem.into() ),+ ])
			}
		};
	}
	/// As a list of items, converts each item to `Value`s and returns a `Tup` containing a vector of the values.
	#[macro_export] macro_rules! tup
	{
		( $( $elem:expr ),* , ) => {
			tup!( $( $elem ),* )
		};
		( $( $elem:expr ),* ) => {
			{
				$crate::tup::Tup::from_vec(vec![ $( $elem.into() ),+ ])
			}
		};
	}
	/// Given a list of field/value pairs, returns an `Obj` containing each pair.
	#[macro_export] macro_rules! obj
	{
		{} => {
			$crate::obj::Obj::from_map_unchecked(::std::collections::HashMap::new())
		};
		{ $( $field:expr => $inner:expr ),+ , } => {
			// Rule with trailing comma.
			try_obj!{ $( $field => $inner ),+ }.unwrap()
		};
		{ $( $field:expr => $inner:expr ),+ } => {
			try_obj!{ $( $field => $inner ),+ }.unwrap()
		};
	}
	/// Given a list of field to `Value` pairs, returns an `Obj` with the fields and values.
	#[macro_export] macro_rules! try_obj
	{
		{ $( $field:expr => $inner:expr ),+ , } => {
			// Rule with trailing comma.
			try_obj!{ $( $field => $inner ),* };
		};
		{ $( $field:expr => $inner:expr ),+ } => {
			#[allow(clippy::useless_let_if_seq)]
			{
				use $crate::obj::Obj;

				let mut _map = ::std::collections::HashMap::new();
				let mut _parent: Option<$crate::value::Value> = None;

				$(
					if $field == "^" {
						_parent = Some($inner.into());
					} else {
						_map.insert($field.into(), $inner.into());
					}
				)*

				match _parent {
					Some(parent) => match parent.get_obj() {
						Ok(parent) => Obj::from_map_with_parent(_map, parent),
						e @ Err(_) => e,
					}
					None => Obj::from_map(_map),
				}
			}
		};
	}
	
	#[macro_export] macro_rules! __lazy_static_create
	{
		($NAME:ident, $T:ty) =>
		{
			static $NAME: ::sync::Lazy<$T> = ::sync::Lazy::INIT;
		};
	}
	
	#[macro_export(local_inner_macros)] macro_rules! __lazy_static_internal
	{
		// optional visibility restrictions are wrapped in `()` to allow for
		// explicitly passing otherwise implicit information about private items
		($(#[$attr:meta])* ($($vis:tt)*) static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
			__lazy_static_internal!(@MAKE TY, $(#[$attr])*, ($($vis)*), $N);
			__lazy_static_internal!(@TAIL, $N : $T = $e);
			lazy_static!($($t)*);
		};
		(@TAIL, $N:ident : $T:ty = $e:expr) => {
			impl ::ops::Deref for $N {
				type Target = $T;
				fn deref(&self) -> &$T {
					#[inline(always)]
					fn __static_ref_initialize() -> $T { $e }

					#[inline(always)]
					fn __stability() -> &'static $T {
						__lazy_static_create!(LAZY, $T);
						LAZY.get(__static_ref_initialize)
					}
					__stability()
				}
			}
			impl $crate::LazyStatic for $N {
				fn initialize(lazy: &Self) {
					let _ = &**lazy;
				}
			}
		};
		// `vis` is wrapped in `()` to prevent parsing ambiguity
		(@MAKE TY, $(#[$attr:meta])*, ($($vis:tt)*), $N:ident) => {
			#[allow(missing_copy_implementations)]
			#[allow(non_camel_case_types)]
			#[allow(dead_code)]
			$(#[$attr])*
			$($vis)* struct $N {__private_field: ()}
			#[doc(hidden)]
			#[allow(non_upper_case_globals)]
			$($vis)* static $N: $N = $N {__private_field: ()};
		};
		() => ()
	}

	#[macro_export(local_inner_macros)] macro_rules! lazy_static
	{
		($(#[$attr:meta])* static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
			// use `()` to explicitly forward the information about private items
			__lazy_static_internal!($(#[$attr])* () static ref $N : $T = $e; $($t)*);
		};
		($(#[$attr:meta])* pub static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
			__lazy_static_internal!($(#[$attr])* (pub) static ref $N : $T = $e; $($t)*);
		};
		($(#[$attr:meta])* pub ($($vis:tt)+) static ref $N:ident : $T:ty = $e:expr; $($t:tt)*) => {
			__lazy_static_internal!($(#[$attr])* (pub ($($vis)+)) static ref $N : $T = $e; $($t)*);
		};
		() => ()
	}

}

pub mod arr
{
	/*!
	Arr | An array container which can hold an arbitrary number of elements of a single type.*/
	use ::
	{
		fmt::{ self, Format },
		slice::{ Iter },
		sync::{ Arc },
		types::{ Type },
		value::{ Value },
		OverError, OverResult, INDENT_STEP,
		*,
	};
	/*
	*/
	#[derive(Clone, Debug)]
	struct ArrInner {
		vec: Vec<Value>,
		inner_t: Type,
	}

	/// `Arr` struct.
	#[derive(Clone, Debug)]
	pub struct Arr {
		inner: Arc<ArrInner>,
	}

	impl Arr {
		/// Returns a new `Arr` from the given vector of `Value`s.
		pub fn from_vec(vec: Vec<Value>) -> OverResult<Arr> {
			let mut tcur = Type::Any;
			let mut has_any = true;

			for value in &vec {
				let tnew = value.get_type();

				if has_any {
					match Type::most_specific(&tcur, &tnew) {
						Some((t, any)) => {
							tcur = t;
							has_any = any;
						}
						None => return Err(OverError::ArrTypeMismatch(tcur, tnew)),
					}
				} else if tcur != tnew {
					return Err(OverError::ArrTypeMismatch(tcur, tnew));
				}
			}

			Ok(Arr {
				inner: Arc::new(ArrInner { vec, inner_t: tcur }),
			})
		}

		/// Returns a new `Arr` from the given vector of `Value`s without checking whether every value
		/// in `vec` is the same type.
		///
		/// It is much faster than the safe version, [`from_vec`], if you know every element in `vec` is
		/// of type `inner_t`.
		pub fn from_vec_unchecked(vec: Vec<Value>, inner_t: Type) -> Arr {
			Arr {
				inner: Arc::new(ArrInner { vec, inner_t }),
			}
		}

		/// Returns a reference to the inner vec of this `Arr`.
		pub fn vec_ref(&self) -> &Vec<Value> {
			&self.inner.vec
		}

		/// Iterates over each `Value` in `self`, applying `Fn` `f`.
		pub fn with_each<F>(&self, mut f: F)
		where
			F: FnMut(&Value),
		{
			for value in &self.inner.vec {
				f(value)
			}
		}

		/// Gets the value at `index`.
		/// Returns an error if `index` is out of bounds.
		pub fn get(&self, index: usize) -> OverResult<Value> {
			if index >= self.inner.vec.len() {
				Err(OverError::ArrOutOfBounds(index))
			} else {
				Ok(self.inner.vec[index].clone())
			}
		}

		/// Returns the type of all elements in this `Arr`.
		pub fn inner_type(&self) -> Type {
			self.inner.inner_t.clone()
		}

		/// Returns the length of this `Arr`.
		pub fn len(&self) -> usize {
			self.inner.vec.len()
		}

		/// Returns whether this `Arr` is empty.
		pub fn is_empty(&self) -> bool {
			self.inner.vec.is_empty()
		}

		/// Returns whether `self` and `other` point to the same data.
		pub fn ptr_eq(&self, other: &Self) -> bool {
			Arc::ptr_eq(&self.inner, &other.inner)
		}

		/// Returns an iterator over the Arr.
		pub fn iter(&self) -> Iter<Value> {
			self.vec_ref().iter()
		}
	}

	impl Default for Arr
    {
		fn default() -> Self {
			Self::from_vec_unchecked(vec![], Type::Any)
		}
	}

	impl fmt::Display for Arr
    {
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			write!(f, "{}", self.format(true, INDENT_STEP))
		}
	}

	impl PartialEq for Arr
    {
		fn eq(&self, other: &Self) -> bool {
			// Quickly return false if the types don't match.
			if self.inner.inner_t != other.inner.inner_t {
				return false;
			}

			self.inner.vec == other.inner.vec
		}
	}

}

pub mod cell
{
	pub use std::cell::{ * };
}

pub mod collections
{
	pub use std::collections::{ * };
}

pub mod convert
{
	pub use std::convert::{ * };
}

pub mod error
{
	/*!
	*/
	pub use std::error::{ * };
	use ::
	{
		parse::over::error::ParseError,
		types::{ Type },
		*,
	};
	/*
	*/
	/// The fabulous OVER error type.
	#[derive(Debug, PartialEq, Eq)]
	pub enum OverError
	{
		ArrOutOfBounds(usize),
		ArrTypeMismatch(Type, Type),
		FieldNotFound(String),
		InvalidFieldName(String),
		NoParentFound,
		ParseError(String),
		TupOutOfBounds(usize),
		TupTypeMismatch(Type, Type, usize),
		TypeMismatch(Type, Type),
		IoError(String),
	}

	impl fmt::Display for OverError
	{
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			use self::OverError::*;

			match *self {
				ArrOutOfBounds(ref index) => write!(f, "Arr index {} out of bounds", index),
				ArrTypeMismatch(ref expected, ref found) => write!(
					f,
					"Arr inner types do not match: expected {}, found {}",
					expected, found
				),
				FieldNotFound(ref field) => write!(f, "Field not found: \"{}\"", field),
				InvalidFieldName(ref field) => write!(f, "Invalid field name: \"{}\"", field),
				NoParentFound => write!(f, "No parent found for this obj"),
				TupOutOfBounds(ref index) => write!(f, "Tup index {} out of bounds", index),
				TupTypeMismatch(ref expected, ref found, ref index) => write!(
					f,
					"Tup inner types do not match at index {}: expected {}, found {}",
					index, expected, found
				),
				TypeMismatch(ref expected, ref found) => {
					write!(f, "Type mismatch: expected {}, found {}", expected, found)
				}

				ParseError(ref error) | IoError(ref error) => write!(f, "{}", error),
			}
		}
	}

	impl Error for OverError
    {
		fn description(&self) -> &str {
			use self::OverError::*;

			match *self {
				ArrOutOfBounds(_) => "Arr index out of bounds",
				ArrTypeMismatch(_, _) => "Arr inner types do not match",
				FieldNotFound(_) => "Field not found",
				InvalidFieldName(_) => "Invalid field name",
				NoParentFound => "No parent found for this obj",
				TupOutOfBounds(_) => "Tup index out of bounds",
				TupTypeMismatch(_, _, _) => "Tup inner types do not match",
				TypeMismatch(_, _) => "Type mismatch",

				ParseError(ref error) | IoError(ref error) => error,
			}
		}
	}

	impl From<io::Error> for OverError
    {
		fn from(e: io::Error) -> Self {
			OverError::IoError(format!("{}", e))
		}
	}

	impl From<ParseError> for OverError
    {
		fn from(e: ParseError) -> Self {
			OverError::ParseError(format!("{}", e))
		}
	}

} pub use self::error::OverError;

pub mod fs
{
	pub use std::fs::{ * };
	
	/// Writes a string to a file.
	// pub fn write_file_str( ... ) -> io::Result<()>
	pub fn write_str(fname: &str, contents: &str) -> io::Result<()>
	{
		use std::io::Write;
		let mut file = File::create(fname)?;
		file.write_all(contents.as_bytes())?;
		Ok(())
	}
}

pub mod fmt
{
	/*!
	Module containing functions for formatting output of objects. */
	pub use std::fmt::{ * };
	use ::
	{
	*,
	};
	/*
	use crate::arr::Arr;
	use crate::obj::Obj;
	use crate::tup::Tup;
	use crate::value::Value;
	use crate::INDENT_STEP;
	use num_bigint::BigInt;
	use num_rational::BigRational;
	use num_traits::One;
	*/
	/// Trait for formatting a .over representation of an object.
	pub trait Format
	{
		fn format(&self, full: bool, indent_amt: usize) -> String;
	}

	impl Format for BigRational
	{
		fn format(&self, _full: bool, _indent_amt: usize) -> String
		{
			let frac_fmt = format!("{}", *self);
			
			if *self.denom() == BigInt::one() { format!("{}.0", frac_fmt) }
			else { frac_fmt }
		}
	}

	impl Format for char
	{
		fn format(&self, _full: bool, _indent_amt: usize) -> String {
			if let Some(s) = get_char_map(*self) {
				format!("\'{}\'", s)
			} else {
				format!("\'{}\'", *self)
			}
		}
	}

	impl Format for String
	{
		fn format(&self, _full: bool, _indent_amt: usize) -> String {
			format!("\"{}\"", replace_all(self))
		}
	}

	impl Format for Value
	{
		fn format(&self, _full: bool, indent_amt: usize) -> String {
			match *self {
				Value::Null => String::from("null"),

				Value::Bool(ref inner) => {
					if *inner {
						String::from("true")
					} else {
						String::from("false")
					}
				}

				Value::Int(ref inner) => format!("{}", inner),

				Value::Frac(ref inner) => inner.format(true, indent_amt),
				Value::Char(ref inner) => inner.format(true, indent_amt),
				Value::Str(ref inner) => inner.format(true, indent_amt),
				Value::Arr(ref inner) => inner.format(true, indent_amt),
				Value::Tup(ref inner) => inner.format(true, indent_amt),
				Value::Obj(ref inner) => inner.format(true, indent_amt),
			}
		}
	}

	impl Format for Arr
	{
		fn format(&self, full: bool, indent_amt: usize) -> String {
			match self.len() {
				0 => {
					if full {
						String::from("[]")
					} else {
						String::new()
					}
				}
				1 => {
					let f = self.get(0).unwrap().format(true, indent_amt);
					if full {
						format!("[{}]", f)
					} else {
						f
					}
				}
				_ => {
					let mut s = if full {
						String::from("[\n")
					} else {
						String::new()
					};

					self.with_each(|value| {
						s.push_str(&format!(
							"{}{}\n",
							indent(indent_amt),
							value.format(true, indent_amt + INDENT_STEP)
						))
					});

					if full {
						let actual_indent_amt = if indent_amt == 0 {
							0
						} else {
							indent_amt - INDENT_STEP
						};
						s.push_str(&format!("{}]", indent(actual_indent_amt)));
					}
					s
				}
			}
		}
	}

	impl Format for Tup
	{
		fn format(&self, full: bool, indent_amt: usize) -> String {
			match self.len() {
				0 => {
					if full {
						String::from("()")
					} else {
						String::new()
					}
				}
				1 => {
					let f = self.get(0).unwrap().format(true, indent_amt);
					if full {
						format!("({})", f)
					} else {
						f
					}
				}
				_ => {
					let mut s = if full {
						String::from("(\n")
					} else {
						String::new()
					};

					self.with_each(|value| {
						s.push_str(&format!(
							"{}{}\n",
							indent(indent_amt),
							value.format(true, indent_amt + INDENT_STEP)
						))
					});

					if full {
						s.push_str(&format!("{})", indent(indent_amt - INDENT_STEP)));
					}
					s
				}
			}
		}
	}

	impl Format for Obj
	{
		fn format(&self, full: bool, indent_amt: usize) -> String {
			if self.is_empty() && !self.has_parent() {
				if full {
					String::from("{}")
				} else {
					String::new()
				}
			} else {
				let mut s = if full {
					String::from("{\n")
				} else {
					String::new()
				};

				if let Some(parent) = self.get_parent() {
					s.push_str(&format!(
						"{}^: {}\n",
						indent(indent_amt),
						parent.format(true, indent_amt + INDENT_STEP)
					));
				}

				self.with_each(|field, value| {
					s.push_str(&format!(
						"{}{}: {}\n",
						indent(indent_amt),
						field,
						value.format(true, indent_amt + INDENT_STEP)
					));
				});

				if full {
					s.push_str(&format!("{}}}", indent(indent_amt - INDENT_STEP)));
				}
				s
			}
		}
	}
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
	// pub fn get_char_map(ch: char) -> Option<&'static str>
	pub fn escaped_str(ch: char) -> Option<&'static str>
	{
		match ch
		{
			'\\' => Some("\\\\"),
			'\"' => Some("\\\""),
			'\'' => Some("\\\'"),
			'$' => Some("\\$"),
			'\n' => Some("\\n"),
			'\r' => Some("\\r"),
			'\t' => Some("\\t"),
			_ => None,
		}
	}
	/// If `ch` preceded by a backslash together form an escape character, then return this char.
	/// Otherwise, return None.
	//pub fn get_escape_char(ch: char) -> Option<char>
	pub fn escaped_character(ch: char) -> Option<char>
	{
		match ch
		{
			'\\' => Some('\\'),
			'"' => Some('"'),
			'\'' => Some('\''),
			'$' => Some('$'),
			'n' => Some('\n'),
			'r' => Some('\r'),
			't' => Some('\t'),
			_ => None,
		}
	}
}

pub mod hash
{
	pub use std::hash::{ * };
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
	/// Returns true if `ch` is an ASCII decimal digit.
	// pub fn is_digit(...) -> bool
	pub fn digit( ch:char ) -> bool
	{
		match ch
		{
			'0'...'9' => true,
			_ => false,
		}
	}
	/// Returns true if this character signifies the legal end of a value.
	// pub fn is_value_end_char(ch: char) -> bool {
	pub fn value_end_char(ch: char) -> bool {
		whitespace(ch) || end_delimiter(ch) || operator(ch)
	}
	/// Returns true if the character is either whitespace or '#' (start of a comment).
	//pub fn is_whitespace(ch: char) -> bool {
	pub fn whitespace(ch: char) -> bool
	{
		ch.is_whitespace() || ch == '#'
	}
	//pub fn is_end_delimiter(ch: char) -> bool
	pub fn end_delimiter(ch: char) -> bool
	{
		match ch
		{
			')' | ']' | '}' | '>' => true,
			_ => false,
		}
	}
	//pub fn is_numeric_char(ch: char) -> bool {
	pub fn numeric_char(ch: char) -> bool
	{
		match ch
		{
			_ch if is_digit(_ch) => true,
			'.' | ',' => true,
			_ => false,
		}
	}
	//pub fn is_priority_operator(ch: char) -> bool {
	pub fn priority_operator(ch: char) -> bool
	{
		match ch
		{
			'*' | '/' | '%' => true,
			_ => false,
		}
	}
	//pub fn is_operator(ch: char) -> bool {
	pub fn operator(ch: char) -> bool
	{
		match ch
		{
			'+' | '-' | '*' | '/' | '%' => true,
			_ => false,
		}
	}
	//pub fn is_reserved(field: &str) -> bool {
	pub fn reserved(field: &str) -> bool
	{
		match field
		{
			"@" | "null" | "true" | "false" | "Obj" | "Str" | "Arr" | "Tup" => true,
			_ => false,
		}
	}
}

pub mod marker
{
	pub use std::marker::{ * };
}

pub mod mem
{
	pub use std::mem::{ * };
}

pub mod num
{
	pub use std::num::{ * };
	
	pub mod rational
	{
		/*!
		Rational numbers */
		use ::
		{
            error::{ Error },
            fmt::{Binary, Display, Formatter, LowerExp, LowerHex, Octal, UpperExp, UpperHex},
            hash::{Hash, Hasher},
            num::
            {
                big::{BigInt, BigUint, Sign, ToBigInt},
                integer::{ Integer },
                traits::
                {
                    Bounded, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, ConstOne, ConstZero, FromPrimitive,
                    Inv, Num, NumCast, One, Pow, Signed, ToPrimitive, Unsigned, Zero, float::FloatCore,
                }

            },
            ops::{Add, Div, Mul, Neg, Rem, ShlAssign, Sub},
            str::{ FromStr },
			*,
		};
		/*
		*/
        pub mod pow
        {
            /*!
            */
            use ::
            {
                num::
                {
                    integer::Integer,
                    traits::{One, Pow},
                },
                *,
            };
            /*
            use crate::Ratio;

            use core::cmp;
            use num_integer::Integer;
            use num_traits::{One, Pow};
            */
            macro_rules! pow_unsigned_impl
            {
                (@ $exp:ty) =>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: $exp) -> Ratio<T> {
                        Ratio::new_raw(self.numer.pow(expon), self.denom.pow(expon))
                    }
                };
                ($exp:ty) => {
                    impl<T: Clone + Integer + Pow<$exp, Output = T>> Pow<$exp> for Ratio<T> {
                        pow_unsigned_impl!(@ $exp);
                    }
                    impl<'a, T: Clone + Integer> Pow<$exp> for &'a Ratio<T>
                    where
                        &'a T: Pow<$exp, Output = T>,
                    {
                        pow_unsigned_impl!(@ $exp);
                    }
                    impl<'b, T: Clone + Integer + Pow<$exp, Output = T>> Pow<&'b $exp> for Ratio<T> {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                    impl<'a, 'b, T: Clone + Integer> Pow<&'b $exp> for &'a Ratio<T>
                    where
                        &'a T: Pow<$exp, Output = T>,
                    {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                };
            }
            
            macro_rules! pow_signed_impl
            {
                (@ &'b BigInt, BigUint) =>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: &'b BigInt) -> Ratio<T> {
                        match expon.sign() {
                            Sign::NoSign => One::one(),
                            Sign::Minus => {
                                Pow::pow(self, expon.magnitude()).into_recip()
                            }
                            Sign::Plus => Pow::pow(self, expon.magnitude()),
                        }
                    }
                };
                (@ $exp:ty, $unsigned:ty) =>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: $exp) -> Ratio<T> {
                        match expon.cmp(&0) {
                            cmp::Ordering::Equal => One::one(),
                            cmp::Ordering::Less => {
                                let expon = expon.wrapping_abs() as $unsigned;
                                Pow::pow(self, expon).into_recip()
                            }
                            cmp::Ordering::Greater => Pow::pow(self, expon as $unsigned),
                        }
                    }
                };
                ($exp:ty, $unsigned:ty) => {
                    impl<T: Clone + Integer + Pow<$unsigned, Output = T>> Pow<$exp> for Ratio<T> {
                        pow_signed_impl!(@ $exp, $unsigned);
                    }
                    impl<'a, T: Clone + Integer> Pow<$exp> for &'a Ratio<T>
                    where
                        &'a T: Pow<$unsigned, Output = T>,
                    {
                        pow_signed_impl!(@ $exp, $unsigned);
                    }
                    impl<'b, T: Clone + Integer + Pow<$unsigned, Output = T>> Pow<&'b $exp> for Ratio<T> {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                    impl<'a, 'b, T: Clone + Integer> Pow<&'b $exp> for &'a Ratio<T>
                    where
                        &'a T: Pow<$unsigned, Output = T>,
                    {
                        type Output = Ratio<T>;
                        #[inline]
                        fn pow(self, expon: &'b $exp) -> Ratio<T> {
                            Pow::pow(self, *expon)
                        }
                    }
                };
            }
            
            pow_unsigned_impl!(u8);
            pow_unsigned_impl!(u16);
            pow_unsigned_impl!(u32);
            pow_unsigned_impl!(u64);
            pow_unsigned_impl!(u128);
            pow_unsigned_impl!(usize);
            
            pow_signed_impl!(i8, u8);
            pow_signed_impl!(i16, u16);
            pow_signed_impl!(i32, u32);
            pow_signed_impl!(i64, u64);
            pow_signed_impl!(i128, u128);
            pow_signed_impl!(isize, usize);
            
            mod bigint
            {
                use super::*;
                use num::big::{BigInt, BigUint, Sign};

                impl<T: Clone + Integer + for<'b> Pow<&'b BigUint, Output = T>> Pow<BigUint> for Ratio<T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigUint) -> Ratio<T>
                    {
                        Pow::pow(self, &expon)
                    }
                }
                
                impl<'a, T: Clone + Integer> Pow<BigUint> for &'a Ratio<T> where
                &'a T: for<'b> Pow<&'b BigUint, Output = T>,
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigUint) -> Ratio<T>
                    {
                        Pow::pow(self, &expon)
                    }
                }
                
                impl<'b, T: Clone + Integer + Pow<&'b BigUint, Output = T>> Pow<&'b BigUint> for Ratio<T>
                {
                    pow_unsigned_impl!(@ &'b BigUint);
                }
                
                impl<'a, 'b, T: Clone + Integer> Pow<&'b BigUint> for &'a Ratio<T> where
                &'a T: Pow<&'b BigUint, Output = T>,
                {
                    pow_unsigned_impl!(@ &'b BigUint);
                }

                impl<T: Clone + Integer + for<'b> Pow<&'b BigUint, Output = T>> Pow<BigInt> for Ratio<T>
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigInt) -> Ratio<T>
                    {
                        Pow::pow(self, &expon)
                    }
                }
                
                impl<'a, T: Clone + Integer> Pow<BigInt> for &'a Ratio<T> where
                &'a T: for<'b> Pow<&'b BigUint, Output = T>,
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigInt) -> Ratio<T>
                    {
                        Pow::pow(self, &expon)
                    }
                }
                
                impl<'b, T: Clone + Integer + Pow<&'b BigUint, Output = T>> Pow<&'b BigInt> for Ratio<T>
                {
                    pow_signed_impl!(@ &'b BigInt, BigUint);
                }
                
                impl<'a, 'b, T: Clone + Integer> Pow<&'b BigInt> for &'a Ratio<T> where
                &'a T: Pow<&'b BigUint, Output = T>,
                {
                    pow_signed_impl!(@ &'b BigInt, BigUint);
                }
            }
        }

		pub fn frac_from_whole_and_dec(whole: BigInt, decimal: BigInt, dec_len: usize) -> BigRational
		{
			let denom = pow(BigInt::from_u8(10).unwrap(), dec_len);
			BigRational::new(whole, 1.into()) + BigRational::new(decimal, denom)
		}
	}
    
    pub mod big
    {
        /*!
        Big Integer Types for Rust */
        use ::
        {
            *,
        };
        /*
        #[macro_use]
        extern crate alloc;

        #[cfg(feature = "std")]
        extern crate std;

        use core::fmt;
        */
        pub mod int
        {
            /*!
            `Add`/`Sub` ops may flip from `BigInt` to its `BigUint` magnitude */
            use ::
            {
                *,
            };
            /*
            use alloc::string::String;
            use alloc::vec::Vec;
            use core::cmp::Ordering::{self, Equal};
            use core::default::Default;
            use core::fmt;
            use core::hash;
            use core::ops::{Neg, Not};
            use core::str;

            use num_integer::{Integer, Roots};
            use num_traits::{ConstZero, Num, One, Pow, Signed, Zero};

            use self::Sign::{Minus, NoSign, Plus};

            use crate::big_digit::BigDigit;
            use crate::biguint::to_str_radix_reversed;
            use crate::biguint::{BigUint, IntDigits, U32Digits, U64Digits};
            */
            pub mod addition
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

            pub mod division
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
            
            pub mod multiplication
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
            
            pub mod subtraction
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
            
            pub mod arbitrary
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
            
            pub mod bits
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
            
            pub mod convert
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
            
            pub mod power
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
            
            pub mod serde
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
            
            pub mod shift
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
            /// A `Sign` is a [`BigInt`]'s composing element.
            #[derive(PartialEq, PartialOrd, Eq, Ord, Copy, Clone, Debug, Hash)]
            pub enum Sign {
                Minus,
                NoSign,
                Plus,
            }

            impl Neg for Sign {
                type Output = Sign;

                /// Negate `Sign` value.
                #[inline] fn neg(self) -> Sign {
                    match self {
                        Minus => Plus,
                        NoSign => NoSign,
                        Plus => Minus,
                    }
                }
            }
            /// A big signed integer type.
            pub struct BigInt {
                sign: Sign,
                data: BigUint,
            }

            // Note: derived `Clone` doesn't specialize `clone_from`,
            // but we want to keep the allocation in `data`.
            impl Clone for BigInt {
                #[inline] fn clone(&self) -> Self {
                    BigInt {
                        sign: self.sign,
                        data: self.data.clone(),
                    }
                }

                #[inline] fn clone_from(&mut self, other: &Self) {
                    self.sign = other.sign;
                    self.data.clone_from(&other.data);
                }
            }

            impl hash::Hash for BigInt {
                #[inline] fn hash<H: hash::Hasher>(&self, state: &mut H) {
                    debug_assert!((self.sign != NoSign) ^ self.data.is_zero());
                    self.sign.hash(state);
                    if self.sign != NoSign {
                        self.data.hash(state);
                    }
                }
            }

            impl PartialEq for BigInt {
                #[inline] fn eq(&self, other: &BigInt) -> bool {
                    debug_assert!((self.sign != NoSign) ^ self.data.is_zero());
                    debug_assert!((other.sign != NoSign) ^ other.data.is_zero());
                    self.sign == other.sign && (self.sign == NoSign || self.data == other.data)
                }
            }

            impl Eq for BigInt {}

            impl PartialOrd for BigInt {
                #[inline] fn partial_cmp(&self, other: &BigInt) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }

            impl Ord for BigInt {
                #[inline] fn cmp(&self, other: &BigInt) -> Ordering {
                    debug_assert!((self.sign != NoSign) ^ self.data.is_zero());
                    debug_assert!((other.sign != NoSign) ^ other.data.is_zero());
                    let scmp = self.sign.cmp(&other.sign);
                    if scmp != Equal {
                        return scmp;
                    }

                    match self.sign {
                        NoSign => Equal,
                        Plus => self.data.cmp(&other.data),
                        Minus => other.data.cmp(&self.data),
                    }
                }
            }

            impl Default for BigInt {
                #[inline] fn default() -> BigInt {
                    Self::ZERO
                }
            }

            impl fmt::Debug for BigInt {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Display::fmt(self, f)
                }
            }

            impl fmt::Display for BigInt {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "", &self.data.to_str_radix(10))
                }
            }

            impl fmt::Binary for BigInt {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0b", &self.data.to_str_radix(2))
                }
            }

            impl fmt::Octal for BigInt {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0o", &self.data.to_str_radix(8))
                }
            }

            impl fmt::LowerHex for BigInt {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(!self.is_negative(), "0x", &self.data.to_str_radix(16))
                }
            }

            impl fmt::UpperHex for BigInt {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    let mut s = self.data.to_str_radix(16);
                    s.make_ascii_uppercase();
                    f.pad_integral(!self.is_negative(), "0x", &s)
                }
            }

            // !-2 = !...f fe = ...0 01 = +1
            // !-1 = !...f ff = ...0 00 =  0
            // ! 0 = !...0 00 = ...f ff = -1
            // !+1 = !...0 01 = ...f fe = -2
            impl Not for BigInt {
                type Output = BigInt;

                fn not(mut self) -> BigInt {
                    match self.sign {
                        NoSign | Plus => {
                            self.data += 1u32;
                            self.sign = Minus;
                        }
                        Minus => {
                            self.data -= 1u32;
                            self.sign = if self.data.is_zero() { NoSign } else { Plus };
                        }
                    }
                    self
                }
            }

            impl Not for &BigInt {
                type Output = BigInt;

                fn not(self) -> BigInt {
                    match self.sign {
                        NoSign => -BigInt::one(),
                        Plus => -BigInt::from(&self.data + 1u32),
                        Minus => BigInt::from(&self.data - 1u32),
                    }
                }
            }

            impl Zero for BigInt {
                #[inline] fn zero() -> BigInt {
                    Self::ZERO
                }

                #[inline] fn set_zero(&mut self) {
                    self.data.set_zero();
                    self.sign = NoSign;
                }

                #[inline] fn is_zero(&self) -> bool {
                    self.sign == NoSign
                }
            }

            impl ConstZero for BigInt {
                // forward to the inherent const
                const ZERO: Self = Self::ZERO;
            }

            impl One for BigInt {
                #[inline] fn one() -> BigInt {
                    BigInt {
                        sign: Plus,
                        data: BigUint::one(),
                    }
                }

                #[inline] fn set_one(&mut self) {
                    self.data.set_one();
                    self.sign = Plus;
                }

                #[inline] fn is_one(&self) -> bool {
                    self.sign == Plus && self.data.is_one()
                }
            }

            impl Signed for BigInt {
                #[inline] fn abs(&self) -> BigInt {
                    match self.sign {
                        Plus | NoSign => self.clone(),
                        Minus => BigInt::from(self.data.clone()),
                    }
                }

                #[inline] fn abs_sub(&self, other: &BigInt) -> BigInt {
                    if *self <= *other {
                        Self::ZERO
                    } else {
                        self - other
                    }
                }

                #[inline] fn signum(&self) -> BigInt {
                    match self.sign {
                        Plus => BigInt::one(),
                        Minus => -BigInt::one(),
                        NoSign => Self::ZERO,
                    }
                }

                #[inline] fn is_positive(&self) -> bool {
                    self.sign == Plus
                }

                #[inline] fn is_negative(&self) -> bool {
                    self.sign == Minus
                }
            }

            trait UnsignedAbs {
                type Unsigned;

                fn checked_uabs(self) -> CheckedUnsignedAbs<Self::Unsigned>;
            }

            enum CheckedUnsignedAbs<T> {
                Positive(T),
                Negative(T),
            }
            use self::CheckedUnsignedAbs::{Negative, Positive};

            macro_rules! impl_unsigned_abs {
                ($Signed:ty, $Unsigned:ty) => {
                    impl UnsignedAbs for $Signed {
                        type Unsigned = $Unsigned;

                        #[inline]
                        fn checked_uabs(self) -> CheckedUnsignedAbs<Self::Unsigned> {
                            if self >= 0 {
                                Positive(self as $Unsigned)
                            } else {
                                Negative(self.wrapping_neg() as $Unsigned)
                            }
                        }
                    }
                };
            }
            impl_unsigned_abs!(i8, u8);
            impl_unsigned_abs!(i16, u16);
            impl_unsigned_abs!(i32, u32);
            impl_unsigned_abs!(i64, u64);
            impl_unsigned_abs!(i128, u128);
            impl_unsigned_abs!(isize, usize);

            impl Neg for BigInt {
                type Output = BigInt;

                #[inline] fn neg(mut self) -> BigInt {
                    self.sign = -self.sign;
                    self
                }
            }

            impl Neg for &BigInt {
                type Output = BigInt;

                #[inline] fn neg(self) -> BigInt {
                    -self.clone()
                }
            }

            impl Integer for BigInt {
                #[inline] fn div_rem(&self, other: &BigInt) -> (BigInt, BigInt) {
                    // r.sign == self.sign
                    let (d_ui, r_ui) = self.data.div_rem(&other.data);
                    let d = BigInt::from_biguint(self.sign, d_ui);
                    let r = BigInt::from_biguint(self.sign, r_ui);
                    if other.is_negative() {
                        (-d, r)
                    } else {
                        (d, r)
                    }
                }

                #[inline] fn div_floor(&self, other: &BigInt) -> BigInt {
                    let (d_ui, m) = self.data.div_mod_floor(&other.data);
                    let d = BigInt::from(d_ui);
                    match (self.sign, other.sign) {
                        (Plus, Plus) | (NoSign, Plus) | (Minus, Minus) => d,
                        (Plus, Minus) | (NoSign, Minus) | (Minus, Plus) => {
                            if m.is_zero() {
                                -d
                            } else {
                                -d - 1u32
                            }
                        }
                        (_, NoSign) => unreachable!(),
                    }
                }

                #[inline] fn mod_floor(&self, other: &BigInt) -> BigInt {
                    // m.sign == other.sign
                    let m_ui = self.data.mod_floor(&other.data);
                    let m = BigInt::from_biguint(other.sign, m_ui);
                    match (self.sign, other.sign) {
                        (Plus, Plus) | (NoSign, Plus) | (Minus, Minus) => m,
                        (Plus, Minus) | (NoSign, Minus) | (Minus, Plus) => {
                            if m.is_zero() {
                                m
                            } else {
                                other - m
                            }
                        }
                        (_, NoSign) => unreachable!(),
                    }
                }

                fn div_mod_floor(&self, other: &BigInt) -> (BigInt, BigInt) {
                    // m.sign == other.sign
                    let (d_ui, m_ui) = self.data.div_mod_floor(&other.data);
                    let d = BigInt::from(d_ui);
                    let m = BigInt::from_biguint(other.sign, m_ui);
                    match (self.sign, other.sign) {
                        (Plus, Plus) | (NoSign, Plus) | (Minus, Minus) => (d, m),
                        (Plus, Minus) | (NoSign, Minus) | (Minus, Plus) => {
                            if m.is_zero() {
                                (-d, m)
                            } else {
                                (-d - 1u32, other - m)
                            }
                        }
                        (_, NoSign) => unreachable!(),
                    }
                }

                #[inline] fn div_ceil(&self, other: &Self) -> Self {
                    let (d_ui, m) = self.data.div_mod_floor(&other.data);
                    let d = BigInt::from(d_ui);
                    match (self.sign, other.sign) {
                        (Plus, Minus) | (NoSign, Minus) | (Minus, Plus) => -d,
                        (Plus, Plus) | (NoSign, Plus) | (Minus, Minus) => {
                            if m.is_zero() {
                                d
                            } else {
                                d + 1u32
                            }
                        }
                        (_, NoSign) => unreachable!(),
                    }
                }

                /// Calculates the Greatest Common Divisor (GCD) of the number and `other`.
                ///
                /// The result is always positive.
                #[inline] fn gcd(&self, other: &BigInt) -> BigInt {
                    BigInt::from(self.data.gcd(&other.data))
                }

                /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
                #[inline] fn lcm(&self, other: &BigInt) -> BigInt {
                    BigInt::from(self.data.lcm(&other.data))
                }

                /// Calculates the Greatest Common Divisor (GCD) and
                /// Lowest Common Multiple (LCM) together.
                #[inline] fn gcd_lcm(&self, other: &BigInt) -> (BigInt, BigInt) {
                    let (gcd, lcm) = self.data.gcd_lcm(&other.data);
                    (BigInt::from(gcd), BigInt::from(lcm))
                }

                /// Greatest common divisor, least common multiple, and Bézout coefficients.
                #[inline] fn extended_gcd_lcm(&self, other: &BigInt) -> (num_integer::ExtendedGcd<BigInt>, BigInt) {
                    let egcd = self.extended_gcd(other);
                    let lcm = if egcd.gcd.is_zero() {
                        Self::ZERO
                    } else {
                        BigInt::from(&self.data / &egcd.gcd.data * &other.data)
                    };
                    (egcd, lcm)
                }

                /// Deprecated, use `is_multiple_of` instead.
                #[inline] fn divides(&self, other: &BigInt) -> bool {
                    self.is_multiple_of(other)
                }

                /// Returns `true` if the number is a multiple of `other`.
                #[inline] fn is_multiple_of(&self, other: &BigInt) -> bool {
                    self.data.is_multiple_of(&other.data)
                }

                /// Returns `true` if the number is divisible by `2`.
                #[inline] fn is_even(&self) -> bool {
                    self.data.is_even()
                }

                /// Returns `true` if the number is not divisible by `2`.
                #[inline] fn is_odd(&self) -> bool {
                    self.data.is_odd()
                }

                /// Rounds up to nearest multiple of argument.
                #[inline] fn next_multiple_of(&self, other: &Self) -> Self {
                    let m = self.mod_floor(other);
                    if m.is_zero() {
                        self.clone()
                    } else {
                        self + (other - m)
                    }
                }
                /// Rounds down to nearest multiple of argument.
                #[inline] fn prev_multiple_of(&self, other: &Self) -> Self {
                    self - self.mod_floor(other)
                }

                fn dec(&mut self) {
                    *self -= 1u32;
                }

                fn inc(&mut self) {
                    *self += 1u32;
                }
            }

            impl Roots for BigInt {
                fn nth_root(&self, n: u32) -> Self {
                    assert!(
                        !(self.is_negative() && n.is_even()),
                        "root of degree {} is imaginary",
                        n
                    );

                    BigInt::from_biguint(self.sign, self.data.nth_root(n))
                }

                fn sqrt(&self) -> Self {
                    assert!(!self.is_negative(), "square root is imaginary");

                    BigInt::from_biguint(self.sign, self.data.sqrt())
                }

                fn cbrt(&self) -> Self {
                    BigInt::from_biguint(self.sign, self.data.cbrt())
                }
            }

            impl IntDigits for BigInt {
                #[inline] fn digits(&self) -> &[BigDigit] {
                    self.data.digits()
                }
                #[inline] fn digits_mut(&mut self) -> &mut Vec<BigDigit> {
                    self.data.digits_mut()
                }
                #[inline] fn normalize(&mut self) {
                    self.data.normalize();
                    if self.data.is_zero() {
                        self.sign = NoSign;
                    }
                }
                #[inline] fn capacity(&self) -> usize {
                    self.data.capacity()
                }
                #[inline] fn len(&self) -> usize {
                    self.data.len()
                }
            }
            /// A generic trait for converting a value to a [`BigInt`]. This may return
            /// `None` when converting from `f32` or `f64`, and will always succeed
            /// when converting from any integer or unsigned primitive, or [`BigUint`].
            pub trait ToBigInt {
                /// Converts the value of `self` to a [`BigInt`].
                fn to_bigint(&self) -> Option<BigInt>;
            }

            impl BigInt {
                /// A constant `BigInt` with value 0, useful for static initialization.
                pub const ZERO: Self = BigInt {
                    sign: NoSign,
                    data: BigUint::ZERO,
                };

                /// Creates and initializes a [`BigInt`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline]
                pub fn new(sign: Sign, digits: Vec<u32>) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::new(digits))
                }

                /// Creates and initializes a [`BigInt`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline]
                pub fn from_biguint(mut sign: Sign, mut data: BigUint) -> BigInt {
                    if sign == NoSign {
                        data.assign_from_slice(&[]);
                    } else if data.is_zero() {
                        sign = NoSign;
                    }

                    BigInt { sign, data }
                }

                /// Creates and initializes a [`BigInt`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline]
                pub fn from_slice(sign: Sign, slice: &[u32]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_slice(slice))
                }

                /// Reinitializes a [`BigInt`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline]
                pub fn assign_from_slice(&mut self, sign: Sign, slice: &[u32]) {
                    if sign == NoSign {
                        self.set_zero();
                    } else {
                        self.data.assign_from_slice(slice);
                        self.sign = if self.data.is_zero() { NoSign } else { sign };
                    }
                }

                /// Creates and initializes a [`BigInt`].
                ///
                /// The bytes are in big-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, Sign};
                ///
                /// assert_eq!(BigInt::from_bytes_be(Sign::Plus, b"A"),
                ///            BigInt::parse_bytes(b"65", 10).unwrap());
                /// assert_eq!(BigInt::from_bytes_be(Sign::Plus, b"AA"),
                ///            BigInt::parse_bytes(b"16705", 10).unwrap());
                /// assert_eq!(BigInt::from_bytes_be(Sign::Plus, b"AB"),
                ///            BigInt::parse_bytes(b"16706", 10).unwrap());
                /// assert_eq!(BigInt::from_bytes_be(Sign::Plus, b"Hello world!"),
                ///            BigInt::parse_bytes(b"22405534230753963835153736737", 10).unwrap());
                /// ```
                #[inline]
                pub fn from_bytes_be(sign: Sign, bytes: &[u8]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_bytes_be(bytes))
                }

                /// Creates and initializes a [`BigInt`].
                ///
                /// The bytes are in little-endian byte order.
                #[inline]
                pub fn from_bytes_le(sign: Sign, bytes: &[u8]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_bytes_le(bytes))
                }

                /// Creates and initializes a [`BigInt`] from an array of bytes in
                /// two's complement binary representation.
                ///
                /// The digits are in big-endian base 2<sup>8</sup>.
                #[inline]
                pub fn from_signed_bytes_be(digits: &[u8]) -> BigInt {
                    convert::from_signed_bytes_be(digits)
                }

                /// Creates and initializes a [`BigInt`] from an array of bytes in two's complement.
                ///
                /// The digits are in little-endian base 2<sup>8</sup>.
                #[inline]
                pub fn from_signed_bytes_le(digits: &[u8]) -> BigInt {
                    convert::from_signed_bytes_le(digits)
                }

                /// Creates and initializes a [`BigInt`].
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, ToBigInt};
                ///
                /// assert_eq!(BigInt::parse_bytes(b"1234", 10), ToBigInt::to_bigint(&1234));
                /// assert_eq!(BigInt::parse_bytes(b"ABCD", 16), ToBigInt::to_bigint(&0xABCD));
                /// assert_eq!(BigInt::parse_bytes(b"G", 16), None);
                /// ```
                #[inline]
                pub fn parse_bytes(buf: &[u8], radix: u32) -> Option<BigInt> {
                    let s = str::from_utf8(buf).ok()?;
                    BigInt::from_str_radix(s, radix).ok()
                }

                /// Creates and initializes a [`BigInt`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                ///
                /// The bytes are in big-endian byte order.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, Sign};
                ///
                /// let inbase190 = vec![15, 33, 125, 12, 14];
                /// let a = BigInt::from_radix_be(Sign::Minus, &inbase190, 190).unwrap();
                /// assert_eq!(a.to_radix_be(190), (Sign:: Minus, inbase190));
                /// ```
                pub fn from_radix_be(sign: Sign, buf: &[u8], radix: u32) -> Option<BigInt> {
                    let u = BigUint::from_radix_be(buf, radix)?;
                    Some(BigInt::from_biguint(sign, u))
                }

                /// Creates and initializes a [`BigInt`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                ///
                /// The bytes are in little-endian byte order.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, Sign};
                ///
                /// let inbase190 = vec![14, 12, 125, 33, 15];
                /// let a = BigInt::from_radix_be(Sign::Minus, &inbase190, 190).unwrap();
                /// assert_eq!(a.to_radix_be(190), (Sign::Minus, inbase190));
                /// ```
                pub fn from_radix_le(sign: Sign, buf: &[u8], radix: u32) -> Option<BigInt> {
                    let u = BigUint::from_radix_le(buf, radix)?;
                    Some(BigInt::from_biguint(sign, u))
                }

                /// Returns the sign and the byte representation of the [`BigInt`] in big-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{ToBigInt, Sign};
                ///
                /// let i = -1125.to_bigint().unwrap();
                /// assert_eq!(i.to_bytes_be(), (Sign::Minus, vec![4, 101]));
                /// ```
                #[inline]
                pub fn to_bytes_be(&self) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_bytes_be())
                }

                /// Returns the sign and the byte representation of the [`BigInt`] in little-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{ToBigInt, Sign};
                ///
                /// let i = -1125.to_bigint().unwrap();
                /// assert_eq!(i.to_bytes_le(), (Sign::Minus, vec![101, 4]));
                /// ```
                #[inline]
                pub fn to_bytes_le(&self) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_bytes_le())
                }

                /// Returns the sign and the `u32` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, Sign};
                ///
                /// assert_eq!(BigInt::from(-1125).to_u32_digits(), (Sign::Minus, vec![1125]));
                /// assert_eq!(BigInt::from(4294967295u32).to_u32_digits(), (Sign::Plus, vec![4294967295]));
                /// assert_eq!(BigInt::from(4294967296u64).to_u32_digits(), (Sign::Plus, vec![0, 1]));
                /// assert_eq!(BigInt::from(-112500000000i64).to_u32_digits(), (Sign::Minus, vec![830850304, 26]));
                /// assert_eq!(BigInt::from(112500000000i64).to_u32_digits(), (Sign::Plus, vec![830850304, 26]));
                /// ```
                #[inline]
                pub fn to_u32_digits(&self) -> (Sign, Vec<u32>) {
                    (self.sign, self.data.to_u32_digits())
                }

                /// Returns the sign and the `u64` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, Sign};
                ///
                /// assert_eq!(BigInt::from(-1125).to_u64_digits(), (Sign::Minus, vec![1125]));
                /// assert_eq!(BigInt::from(4294967295u32).to_u64_digits(), (Sign::Plus, vec![4294967295]));
                /// assert_eq!(BigInt::from(4294967296u64).to_u64_digits(), (Sign::Plus, vec![4294967296]));
                /// assert_eq!(BigInt::from(-112500000000i64).to_u64_digits(), (Sign::Minus, vec![112500000000]));
                /// assert_eq!(BigInt::from(112500000000i64).to_u64_digits(), (Sign::Plus, vec![112500000000]));
                /// assert_eq!(BigInt::from(1u128 << 64).to_u64_digits(), (Sign::Plus, vec![0, 1]));
                /// ```
                #[inline]
                pub fn to_u64_digits(&self) -> (Sign, Vec<u64>) {
                    (self.sign, self.data.to_u64_digits())
                }

                /// Returns an iterator of `u32` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigInt;
                ///
                /// assert_eq!(BigInt::from(-1125).iter_u32_digits().collect::<Vec<u32>>(), vec![1125]);
                /// assert_eq!(BigInt::from(4294967295u32).iter_u32_digits().collect::<Vec<u32>>(), vec![4294967295]);
                /// assert_eq!(BigInt::from(4294967296u64).iter_u32_digits().collect::<Vec<u32>>(), vec![0, 1]);
                /// assert_eq!(BigInt::from(-112500000000i64).iter_u32_digits().collect::<Vec<u32>>(), vec![830850304, 26]);
                /// assert_eq!(BigInt::from(112500000000i64).iter_u32_digits().collect::<Vec<u32>>(), vec![830850304, 26]);
                /// ```
                #[inline]
                pub fn iter_u32_digits(&self) -> U32Digits<'_> {
                    self.data.iter_u32_digits()
                }

                /// Returns an iterator of `u64` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigInt;
                ///
                /// assert_eq!(BigInt::from(-1125).iter_u64_digits().collect::<Vec<u64>>(), vec![1125u64]);
                /// assert_eq!(BigInt::from(4294967295u32).iter_u64_digits().collect::<Vec<u64>>(), vec![4294967295u64]);
                /// assert_eq!(BigInt::from(4294967296u64).iter_u64_digits().collect::<Vec<u64>>(), vec![4294967296u64]);
                /// assert_eq!(BigInt::from(-112500000000i64).iter_u64_digits().collect::<Vec<u64>>(), vec![112500000000u64]);
                /// assert_eq!(BigInt::from(112500000000i64).iter_u64_digits().collect::<Vec<u64>>(), vec![112500000000u64]);
                /// assert_eq!(BigInt::from(1u128 << 64).iter_u64_digits().collect::<Vec<u64>>(), vec![0, 1]);
                /// ```
                #[inline]
                pub fn iter_u64_digits(&self) -> U64Digits<'_> {
                    self.data.iter_u64_digits()
                }

                /// Returns the two's-complement byte representation of the [`BigInt`] in big-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::ToBigInt;
                ///
                /// let i = -1125.to_bigint().unwrap();
                /// assert_eq!(i.to_signed_bytes_be(), vec![251, 155]);
                /// ```
                #[inline]
                pub fn to_signed_bytes_be(&self) -> Vec<u8> {
                    convert::to_signed_bytes_be(self)
                }

                /// Returns the two's-complement byte representation of the [`BigInt`] in little-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::ToBigInt;
                ///
                /// let i = -1125.to_bigint().unwrap();
                /// assert_eq!(i.to_signed_bytes_le(), vec![155, 251]);
                /// ```
                #[inline]
                pub fn to_signed_bytes_le(&self) -> Vec<u8> {
                    convert::to_signed_bytes_le(self)
                }

                /// Returns the integer formatted as a string in the given radix.
                /// `radix` must be in the range `2...36`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigInt;
                ///
                /// let i = BigInt::parse_bytes(b"ff", 16).unwrap();
                /// assert_eq!(i.to_str_radix(16), "ff");
                /// ```
                #[inline]
                pub fn to_str_radix(&self, radix: u32) -> String {
                    let mut v = to_str_radix_reversed(&self.data, radix);

                    if self.is_negative() {
                        v.push(b'-');
                    }

                    v.reverse();
                    unsafe { String::from_utf8_unchecked(v) }
                }

                /// Returns the integer in the requested base in big-endian digit order.
                /// The output is not given in a human readable alphabet but as a zero
                /// based `u8` number.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, Sign};
                ///
                /// assert_eq!(BigInt::from(-0xFFFFi64).to_radix_be(159),
                ///            (Sign::Minus, vec![2, 94, 27]));
                /// // 0xFFFF = 65535 = 2*(159^2) + 94*159 + 27
                /// ```
                #[inline]
                pub fn to_radix_be(&self, radix: u32) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_radix_be(radix))
                }

                /// Returns the integer in the requested base in little-endian digit order.
                /// The output is not given in a human readable alphabet but as a zero
                /// based `u8` number.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, Sign};
                ///
                /// assert_eq!(BigInt::from(-0xFFFFi64).to_radix_le(159),
                ///            (Sign::Minus, vec![27, 94, 2]));
                /// // 0xFFFF = 65535 = 27 + 94*159 + 2*(159^2)
                /// ```
                #[inline]
                pub fn to_radix_le(&self, radix: u32) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_radix_le(radix))
                }

                /// Returns the sign of the [`BigInt`] as a [`Sign`].
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, Sign};
                ///
                /// assert_eq!(BigInt::from(1234).sign(), Sign::Plus);
                /// assert_eq!(BigInt::from(-4321).sign(), Sign::Minus);
                /// assert_eq!(BigInt::ZERO.sign(), Sign::NoSign);
                /// ```
                #[inline]
                pub fn sign(&self) -> Sign {
                    self.sign
                }

                /// Returns the magnitude of the [`BigInt`] as a [`BigUint`].
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, BigUint};
                /// use num_traits::Zero;
                ///
                /// assert_eq!(BigInt::from(1234).magnitude(), &BigUint::from(1234u32));
                /// assert_eq!(BigInt::from(-4321).magnitude(), &BigUint::from(4321u32));
                /// assert!(BigInt::ZERO.magnitude().is_zero());
                /// ```
                #[inline]
                pub fn magnitude(&self) -> &BigUint {
                    &self.data
                }

                /// Convert this [`BigInt`] into its [`Sign`] and [`BigUint`] magnitude,
                /// the reverse of [`BigInt::from_biguint()`].
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigInt, BigUint, Sign};
                ///
                /// assert_eq!(BigInt::from(1234).into_parts(), (Sign::Plus, BigUint::from(1234u32)));
                /// assert_eq!(BigInt::from(-4321).into_parts(), (Sign::Minus, BigUint::from(4321u32)));
                /// assert_eq!(BigInt::ZERO.into_parts(), (Sign::NoSign, BigUint::ZERO));
                /// ```
                #[inline]
                pub fn into_parts(self) -> (Sign, BigUint) {
                    (self.sign, self.data)
                }

                /// Determines the fewest bits necessary to express the [`BigInt`],
                /// not including the sign.
                #[inline]
                pub fn bits(&self) -> u64 {
                    self.data.bits()
                }

                /// Converts this [`BigInt`] into a [`BigUint`], if it's not negative.
                #[inline]
                pub fn to_biguint(&self) -> Option<BigUint> {
                    match self.sign {
                        Plus => Some(self.data.clone()),
                        NoSign => Some(BigUint::ZERO),
                        Minus => None,
                    }
                }

                #[inline]
                pub fn checked_add(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self + v)
                }

                #[inline]
                pub fn checked_sub(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self - v)
                }

                #[inline]
                pub fn checked_mul(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self * v)
                }

                #[inline]
                pub fn checked_div(&self, v: &BigInt) -> Option<BigInt> {
                    if v.is_zero() {
                        return None;
                    }
                    Some(self / v)
                }

                /// Returns `self ^ exponent`.
                pub fn pow(&self, exponent: u32) -> Self {
                    Pow::pow(self, exponent)
                }

                /// Returns `(self ^ exponent) mod modulus`
                ///
                /// Note that this rounds like `mod_floor`, not like the `%` operator,
                /// which makes a difference when given a negative `self` or `modulus`.
                /// The result will be in the interval `[0, modulus)` for `modulus > 0`,
                /// or in the interval `(modulus, 0]` for `modulus < 0`
                ///
                /// Panics if the exponent is negative or the modulus is zero.
                pub fn modpow(&self, exponent: &Self, modulus: &Self) -> Self {
                    power::modpow(self, exponent, modulus)
                }

                /// Returns the modular multiplicative inverse if it exists, otherwise `None`.
                ///
                /// This solves for `x` such that `self * x ≡ 1 (mod modulus)`.
                /// Note that this rounds like `mod_floor`, not like the `%` operator,
                /// which makes a difference when given a negative `self` or `modulus`.
                /// The solution will be in the interval `[0, modulus)` for `modulus > 0`,
                /// or in the interval `(modulus, 0]` for `modulus < 0`,
                /// and it exists if and only if `gcd(self, modulus) == 1`.
                ///
                /// ```
                /// use num_bigint::BigInt;
                /// use num_integer::Integer;
                /// use num_traits::{One, Zero};
                ///
                /// let m = BigInt::from(383);
                ///
                /// // Trivial cases
                /// assert_eq!(BigInt::zero().modinv(&m), None);
                /// assert_eq!(BigInt::one().modinv(&m), Some(BigInt::one()));
                /// let neg1 = &m - 1u32;
                /// assert_eq!(neg1.modinv(&m), Some(neg1));
                ///
                /// // Positive self and modulus
                /// let a = BigInt::from(271);
                /// let x = a.modinv(&m).unwrap();
                /// assert_eq!(x, BigInt::from(106));
                /// assert_eq!(x.modinv(&m).unwrap(), a);
                /// assert_eq!((&a * x).mod_floor(&m), BigInt::one());
                ///
                /// // Negative self and positive modulus
                /// let b = -&a;
                /// let x = b.modinv(&m).unwrap();
                /// assert_eq!(x, BigInt::from(277));
                /// assert_eq!((&b * x).mod_floor(&m), BigInt::one());
                ///
                /// // Positive self and negative modulus
                /// let n = -&m;
                /// let x = a.modinv(&n).unwrap();
                /// assert_eq!(x, BigInt::from(-277));
                /// assert_eq!((&a * x).mod_floor(&n), &n + 1);
                ///
                /// // Negative self and modulus
                /// let x = b.modinv(&n).unwrap();
                /// assert_eq!(x, BigInt::from(-106));
                /// assert_eq!((&b * x).mod_floor(&n), &n + 1);
                /// ```
                pub fn modinv(&self, modulus: &Self) -> Option<Self> {
                    let result = self.data.modinv(&modulus.data)?;
                    // The sign of the result follows the modulus, like `mod_floor`.
                    let (sign, mag) = match (self.is_negative(), modulus.is_negative()) {
                        (false, false) => (Plus, result),
                        (true, false) => (Plus, &modulus.data - result),
                        (false, true) => (Minus, &modulus.data - result),
                        (true, true) => (Minus, result),
                    };
                    Some(BigInt::from_biguint(sign, mag))
                }

                /// Returns the truncated principal square root of `self` --
                /// see [`num_integer::Roots::sqrt()`].
                pub fn sqrt(&self) -> Self {
                    Roots::sqrt(self)
                }

                /// Returns the truncated principal cube root of `self` --
                /// see [`num_integer::Roots::cbrt()`].
                pub fn cbrt(&self) -> Self {
                    Roots::cbrt(self)
                }

                /// Returns the truncated principal `n`th root of `self` --
                /// See [`num_integer::Roots::nth_root()`].
                pub fn nth_root(&self, n: u32) -> Self {
                    Roots::nth_root(self, n)
                }

                /// Returns the number of least-significant bits that are zero,
                /// or `None` if the entire number is zero.
                pub fn trailing_zeros(&self) -> Option<u64> {
                    self.data.trailing_zeros()
                }

                /// Returns whether the bit in position `bit` is set,
                /// using the two's complement for negative numbers
                pub fn bit(&self, bit: u64) -> bool {
                    if self.is_negative() {
                        // Let the binary representation of a number be
                        //   ... 0  x 1 0 ... 0
                        // Then the two's complement is
                        //   ... 1 !x 1 0 ... 0
                        // where !x is obtained from x by flipping each bit
                        if bit >= u64::from(crate::big_digit::BITS) * self.len() as u64 {
                            true
                        } else {
                            let trailing_zeros = self.data.trailing_zeros().unwrap();
                            match Ord::cmp(&bit, &trailing_zeros) {
                                Ordering::Less => false,
                                Ordering::Equal => true,
                                Ordering::Greater => !self.data.bit(bit),
                            }
                        }
                    } else {
                        self.data.bit(bit)
                    }
                }

                /// Sets or clears the bit in the given position,
                /// using the two's complement for negative numbers
                ///
                /// Note that setting/clearing a bit (for positive/negative numbers,
                /// respectively) greater than the current bit length, a reallocation
                /// may be needed to store the new digits
                pub fn set_bit(&mut self, bit: u64, value: bool) {
                    match self.sign {
                        Sign::Plus => self.data.set_bit(bit, value),
                        Sign::Minus => bits::set_negative_bit(self, bit, value),
                        Sign::NoSign => {
                            if value {
                                self.data.set_bit(bit, true);
                                self.sign = Sign::Plus;
                            } else {
                                // Clearing a bit for zero is a no-op
                            }
                        }
                    }
                    // The top bit may have been cleared, so normalize
                    self.normalize();
                }
            }

            impl num_traits::FromBytes for BigInt {
                type Bytes = [u8];

                fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_signed_bytes_be(bytes)
                }

                fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_signed_bytes_le(bytes)
                }
            }

            impl num_traits::ToBytes for BigInt {
                type Bytes = Vec<u8>;

                fn to_be_bytes(&self) -> Self::Bytes {
                    self.to_signed_bytes_be()
                }

                fn to_le_bytes(&self) -> Self::Bytes {
                    self.to_signed_bytes_le()
                }
            }
        }
        
        pub mod rand
        {
            /*!
            */
            use ::
            {
                *,
            };
            /*
            use rand::distributions::uniform::{SampleBorrow, SampleUniform, UniformSampler};
            use rand::prelude::*;

            use crate::BigInt;
            use crate::BigUint;
            use crate::Sign::*;

            use crate::biguint::biguint_from_vec;

            use num_integer::Integer;
            use num_traits::{ToPrimitive, Zero};
            */
            /// A trait for sampling random big integers.
            ///
            /// The `rand` feature must be enabled to use this. See crate-level documentation for details.
            pub trait RandBigInt
            {
                /// Generate a random [`BigUint`] of the given bit size.
                fn gen_biguint(&mut self, bit_size: u64) -> BigUint;

                /// Generate a random [ BigInt`] of the given bit size.
                fn gen_bigint(&mut self, bit_size: u64) -> BigInt;

                /// Generate a random [`BigUint`] less than the given bound. Fails
                /// when the bound is zero.
                fn gen_biguint_below(&mut self, bound: &BigUint) -> BigUint;

                /// Generate a random [`BigUint`] within the given range. The lower
                /// bound is inclusive; the upper bound is exclusive. Fails when
                /// the upper bound is not greater than the lower bound.
                fn gen_biguint_range(&mut self, lbound: &BigUint, ubound: &BigUint) -> BigUint;

                /// Generate a random [`BigInt`] within the given range. The lower
                /// bound is inclusive; the upper bound is exclusive. Fails when
                /// the upper bound is not greater than the lower bound.
                fn gen_bigint_range(&mut self, lbound: &BigInt, ubound: &BigInt) -> BigInt;
            }

            fn gen_bits<R: Rng + ?Sized>(rng: &mut R, data: &mut [u32], rem: u64) {
                // `fill` is faster than many `gen::<u32>` calls
                rng.fill(data);
                if rem > 0 {
                    let last = data.len() - 1;
                    data[last] >>= 32 - rem;
                }
            }

            impl<R: Rng + ?Sized> RandBigInt for R {
                cfg_digit!(
                    fn gen_biguint(&mut self, bit_size: u64) -> BigUint {
                        let (digits, rem) = bit_size.div_rem(&32);
                        let len = (digits + (rem > 0) as u64)
                            .to_usize()
                            .expect("capacity overflow");
                        let mut data = vec![0u32; len];
                        gen_bits(self, &mut data, rem);
                        biguint_from_vec(data)
                    }

                    fn gen_biguint(&mut self, bit_size: u64) -> BigUint {
                        use core::slice;

                        let (digits, rem) = bit_size.div_rem(&32);
                        let len = (digits + (rem > 0) as u64)
                            .to_usize()
                            .expect("capacity overflow");
                        let native_digits = Integer::div_ceil(&bit_size, &64);
                        let native_len = native_digits.to_usize().expect("capacity overflow");
                        let mut data = vec![0u64; native_len];
                        unsafe {
                            // Generate bits in a `&mut [u32]` slice for value stability
                            let ptr = data.as_mut_ptr() as *mut u32;
                            debug_assert!(native_len * 2 >= len);
                            let data = slice::from_raw_parts_mut(ptr, len);
                            gen_bits(self, data, rem);
                        }
                        #[cfg(target_endian = "big")]
                        for digit in &mut data {
                            // swap u32 digits into u64 endianness
                            *digit = (*digit << 32) | (*digit >> 32);
                        }
                        biguint_from_vec(data)
                    }
                );

                fn gen_bigint(&mut self, bit_size: u64) -> BigInt {
                    loop {
                        // Generate a random BigUint...
                        let biguint = self.gen_biguint(bit_size);
                        // ...and then randomly assign it a Sign...
                        let sign = if biguint.is_zero() {
                            // ...except that if the BigUint is zero, we need to try
                            // again with probability 0.5. This is because otherwise,
                            // the probability of generating a zero BigInt would be
                            // double that of any other number.
                            if self.gen() {
                                continue;
                            } else {
                                NoSign
                            }
                        } else if self.gen() {
                            Plus
                        } else {
                            Minus
                        };
                        return BigInt::from_biguint(sign, biguint);
                    }
                }

                fn gen_biguint_below(&mut self, bound: &BigUint) -> BigUint {
                    assert!(!bound.is_zero());
                    let bits = bound.bits();
                    loop {
                        let n = self.gen_biguint(bits);
                        if n < *bound {
                            return n;
                        }
                    }
                }

                fn gen_biguint_range(&mut self, lbound: &BigUint, ubound: &BigUint) -> BigUint {
                    assert!(*lbound < *ubound);
                    if lbound.is_zero() {
                        self.gen_biguint_below(ubound)
                    } else {
                        lbound + self.gen_biguint_below(&(ubound - lbound))
                    }
                }

                fn gen_bigint_range(&mut self, lbound: &BigInt, ubound: &BigInt) -> BigInt {
                    assert!(*lbound < *ubound);
                    if lbound.is_zero() {
                        BigInt::from(self.gen_biguint_below(ubound.magnitude()))
                    } else if ubound.is_zero() {
                        lbound + BigInt::from(self.gen_biguint_below(lbound.magnitude()))
                    } else {
                        let delta = ubound - lbound;
                        lbound + BigInt::from(self.gen_biguint_below(delta.magnitude()))
                    }
                }
            }
            /// The back-end implementing rand's [`UniformSampler`] for [`BigUint`].
            #[derive(Clone, Debug)]
            pub struct UniformBigUint {
                base: BigUint,
                len: BigUint,
            }

            impl UniformSampler for UniformBigUint {
                type X = BigUint;

                #[inline]
                fn new<B1, B2>(low_b: B1, high_b: B2) -> Self
                where
                    B1: SampleBorrow<Self::X> + Sized,
                    B2: SampleBorrow<Self::X> + Sized,
                {
                    let low = low_b.borrow();
                    let high = high_b.borrow();
                    assert!(low < high);
                    UniformBigUint {
                        len: high - low,
                        base: low.clone(),
                    }
                }

                #[inline]
                fn new_inclusive<B1, B2>(low_b: B1, high_b: B2) -> Self
                where
                    B1: SampleBorrow<Self::X> + Sized,
                    B2: SampleBorrow<Self::X> + Sized,
                {
                    let low = low_b.borrow();
                    let high = high_b.borrow();
                    assert!(low <= high);
                    Self::new(low, high + 1u32)
                }

                #[inline]
                fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
                    &self.base + rng.gen_biguint_below(&self.len)
                }

                #[inline]
                fn sample_single<R: Rng + ?Sized, B1, B2>(low: B1, high: B2, rng: &mut R) -> Self::X
                where
                    B1: SampleBorrow<Self::X> + Sized,
                    B2: SampleBorrow<Self::X> + Sized,
                {
                    rng.gen_biguint_range(low.borrow(), high.borrow())
                }
            }

            impl SampleUniform for BigUint {
                type Sampler = UniformBigUint;
            }
            /// The back-end implementing rand's [`UniformSampler`] for [`BigInt`].
            #[derive(Clone, Debug)]
            pub struct UniformBigInt {
                base: BigInt,
                len: BigUint,
            }

            impl UniformSampler for UniformBigInt {
                type X = BigInt;

                #[inline]
                fn new<B1, B2>(low_b: B1, high_b: B2) -> Self
                where
                    B1: SampleBorrow<Self::X> + Sized,
                    B2: SampleBorrow<Self::X> + Sized,
                {
                    let low = low_b.borrow();
                    let high = high_b.borrow();
                    assert!(low < high);
                    UniformBigInt {
                        len: (high - low).into_parts().1,
                        base: low.clone(),
                    }
                }

                #[inline]
                fn new_inclusive<B1, B2>(low_b: B1, high_b: B2) -> Self
                where
                    B1: SampleBorrow<Self::X> + Sized,
                    B2: SampleBorrow<Self::X> + Sized,
                {
                    let low = low_b.borrow();
                    let high = high_b.borrow();
                    assert!(low <= high);
                    Self::new(low, high + 1u32)
                }

                #[inline]
                fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
                    &self.base + BigInt::from(rng.gen_biguint_below(&self.len))
                }

                #[inline]
                fn sample_single<R: Rng + ?Sized, B1, B2>(low: B1, high: B2, rng: &mut R) -> Self::X
                where
                    B1: SampleBorrow<Self::X> + Sized,
                    B2: SampleBorrow<Self::X> + Sized,
                {
                    rng.gen_bigint_range(low.borrow(), high.borrow())
                }
            }

            impl SampleUniform for BigInt {
                type Sampler = UniformBigInt;
            }
            /// A random distribution for [`BigUint`] and [`BigInt`] values of a particular bit size.
            ///
            /// The `rand` feature must be enabled to use this. See crate-level documentation for details.
            #[derive(Clone, Copy, Debug)]
            pub struct RandomBits {
                bits: u64,
            }

            impl RandomBits {
                #[inline]
                pub fn new(bits: u64) -> RandomBits {
                    RandomBits { bits }
                }
            }

            impl Distribution<BigUint> for RandomBits {
                #[inline]
                fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> BigUint {
                    rng.gen_biguint(self.bits)
                }
            }

            impl Distribution<BigInt> for RandomBits {
                #[inline]
                fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> BigInt {
                    rng.gen_bigint(self.bits)
                }
            }
        }
        
        pub mod uint
        {
            /*!
            */
            use ::
            {
                *,
            };
            /*
            use crate::big_digit::{self, BigDigit};

            use alloc::string::String;
            use alloc::vec::Vec;
            use core::cmp;
            use core::cmp::Ordering;
            use core::default::Default;
            use core::fmt;
            use core::hash;
            use core::mem;
            use core::str;

            use num_integer::{Integer, Roots};
            use num_traits::{ConstZero, Num, One, Pow, ToPrimitive, Unsigned, Zero};
            */
            pub mod addition
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

            pub mod division
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
            
            pub mod multiplication
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
            
            pub mod subtraction
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
            
            pub mod arbitrary
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
            
            pub mod bits
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
            
            pub mod convert
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
            
            pub mod iter
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
            
            pub mod monty
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
            
            pub mod power
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
            
            pub mod serde
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
            
            pub mod shift
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
            

            pub(crate) use self::convert::to_str_radix_reversed;
            pub use self::iter::{U32Digits, U64Digits};
            /// A big unsigned integer type.
            pub struct BigUint {
                data: Vec<BigDigit>,
            }

            // Note: derived `Clone` doesn't specialize `clone_from`,
            // but we want to keep the allocation in `data`.
            impl Clone for BigUint {
                #[inline]
                fn clone(&self) -> Self {
                    BigUint {
                        data: self.data.clone(),
                    }
                }

                #[inline]
                fn clone_from(&mut self, other: &Self) {
                    self.data.clone_from(&other.data);
                }
            }

            impl hash::Hash for BigUint {
                #[inline]
                fn hash<H: hash::Hasher>(&self, state: &mut H) {
                    debug_assert!(self.data.last() != Some(&0));
                    self.data.hash(state);
                }
            }

            impl PartialEq for BigUint {
                #[inline]
                fn eq(&self, other: &BigUint) -> bool {
                    debug_assert!(self.data.last() != Some(&0));
                    debug_assert!(other.data.last() != Some(&0));
                    self.data == other.data
                }
            }

            impl Eq for BigUint {}

            impl PartialOrd for BigUint {
                #[inline]
                fn partial_cmp(&self, other: &BigUint) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }

            impl Ord for BigUint {
                #[inline]
                fn cmp(&self, other: &BigUint) -> Ordering {
                    cmp_slice(&self.data[..], &other.data[..])
                }
            }

            #[inline] fn cmp_slice(a: &[BigDigit], b: &[BigDigit]) -> Ordering {
                debug_assert!(a.last() != Some(&0));
                debug_assert!(b.last() != Some(&0));

                match Ord::cmp(&a.len(), &b.len()) {
                    Ordering::Equal => Iterator::cmp(a.iter().rev(), b.iter().rev()),
                    other => other,
                }
            }

            impl Default for BigUint {
                #[inline]
                fn default() -> BigUint {
                    Self::ZERO
                }
            }

            impl fmt::Debug for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Display::fmt(self, f)
                }
            }

            impl fmt::Display for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(true, "", &self.to_str_radix(10))
                }
            }

            impl fmt::LowerHex for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(true, "0x", &self.to_str_radix(16))
                }
            }

            impl fmt::UpperHex for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    let mut s = self.to_str_radix(16);
                    s.make_ascii_uppercase();
                    f.pad_integral(true, "0x", &s)
                }
            }

            impl fmt::Binary for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(true, "0b", &self.to_str_radix(2))
                }
            }

            impl fmt::Octal for BigUint {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.pad_integral(true, "0o", &self.to_str_radix(8))
                }
            }

            impl Zero for BigUint {
                #[inline]
                fn zero() -> BigUint {
                    Self::ZERO
                }

                #[inline]
                fn set_zero(&mut self) {
                    self.data.clear();
                }

                #[inline]
                fn is_zero(&self) -> bool {
                    self.data.is_empty()
                }
            }

            impl ConstZero for BigUint {
                // forward to the inherent const
                const ZERO: Self = Self::ZERO; // BigUint { data: Vec::new() };
            }

            impl One for BigUint {
                #[inline]
                fn one() -> BigUint {
                    BigUint { data: vec![1] }
                }

                #[inline]
                fn set_one(&mut self) {
                    self.data.clear();
                    self.data.push(1);
                }

                #[inline]
                fn is_one(&self) -> bool {
                    self.data[..] == [1]
                }
            }

            impl Unsigned for BigUint {}

            impl Integer for BigUint {
                #[inline]
                fn div_rem(&self, other: &BigUint) -> (BigUint, BigUint) {
                    division::div_rem_ref(self, other)
                }

                #[inline]
                fn div_floor(&self, other: &BigUint) -> BigUint {
                    let (d, _) = division::div_rem_ref(self, other);
                    d
                }

                #[inline]
                fn mod_floor(&self, other: &BigUint) -> BigUint {
                    let (_, m) = division::div_rem_ref(self, other);
                    m
                }

                #[inline]
                fn div_mod_floor(&self, other: &BigUint) -> (BigUint, BigUint) {
                    division::div_rem_ref(self, other)
                }

                #[inline]
                fn div_ceil(&self, other: &BigUint) -> BigUint {
                    let (d, m) = division::div_rem_ref(self, other);
                    if m.is_zero() {
                        d
                    } else {
                        d + 1u32
                    }
                }

                /// Calculates the Greatest Common Divisor (GCD) of the number and `other`.
                ///
                /// The result is always positive.
                #[inline]
                fn gcd(&self, other: &Self) -> Self {
                    #[inline]
                    fn twos(x: &BigUint) -> u64 {
                        x.trailing_zeros().unwrap_or(0)
                    }

                    // Stein's algorithm
                    if self.is_zero() {
                        return other.clone();
                    }
                    if other.is_zero() {
                        return self.clone();
                    }
                    let mut m = self.clone();
                    let mut n = other.clone();

                    // find common factors of 2
                    let shift = cmp::min(twos(&n), twos(&m));

                    // divide m and n by 2 until odd
                    // m inside loop
                    n >>= twos(&n);

                    while !m.is_zero() {
                        m >>= twos(&m);
                        if n > m {
                            mem::swap(&mut n, &mut m)
                        }
                        m -= &n;
                    }

                    n << shift
                }

                /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
                #[inline]
                fn lcm(&self, other: &BigUint) -> BigUint {
                    if self.is_zero() && other.is_zero() {
                        Self::ZERO
                    } else {
                        self / self.gcd(other) * other
                    }
                }

                /// Calculates the Greatest Common Divisor (GCD) and
                /// Lowest Common Multiple (LCM) together.
                #[inline]
                fn gcd_lcm(&self, other: &Self) -> (Self, Self) {
                    let gcd = self.gcd(other);
                    let lcm = if gcd.is_zero() {
                        Self::ZERO
                    } else {
                        self / &gcd * other
                    };
                    (gcd, lcm)
                }

                /// Deprecated, use `is_multiple_of` instead.
                #[inline]
                fn divides(&self, other: &BigUint) -> bool {
                    self.is_multiple_of(other)
                }

                /// Returns `true` if the number is a multiple of `other`.
                #[inline]
                fn is_multiple_of(&self, other: &BigUint) -> bool {
                    if other.is_zero() {
                        return self.is_zero();
                    }
                    (self % other).is_zero()
                }

                /// Returns `true` if the number is divisible by `2`.
                #[inline]
                fn is_even(&self) -> bool {
                    // Considering only the last digit.
                    match self.data.first() {
                        Some(x) => x.is_even(),
                        None => true,
                    }
                }

                /// Returns `true` if the number is not divisible by `2`.
                #[inline]
                fn is_odd(&self) -> bool {
                    !self.is_even()
                }

                /// Rounds up to nearest multiple of argument.
                #[inline]
                fn next_multiple_of(&self, other: &Self) -> Self {
                    let m = self.mod_floor(other);
                    if m.is_zero() {
                        self.clone()
                    } else {
                        self + (other - m)
                    }
                }
                /// Rounds down to nearest multiple of argument.
                #[inline]
                fn prev_multiple_of(&self, other: &Self) -> Self {
                    self - self.mod_floor(other)
                }

                fn dec(&mut self) {
                    *self -= 1u32;
                }

                fn inc(&mut self) {
                    *self += 1u32;
                }
            }

            #[inline] fn fixpoint<F>(mut x: BigUint, max_bits: u64, f: F) -> BigUint
            where
                F: Fn(&BigUint) -> BigUint,
            {
                let mut xn = f(&x);

                // If the value increased, then the initial guess must have been low.
                // Repeat until we reverse course.
                while x < xn {
                    // Sometimes an increase will go way too far, especially with large
                    // powers, and then take a long time to walk back.  We know an upper
                    // bound based on bit size, so saturate on that.
                    x = if xn.bits() > max_bits {
                        BigUint::one() << max_bits
                    } else {
                        xn
                    };
                    xn = f(&x);
                }

                // Now keep repeating while the estimate is decreasing.
                while x > xn {
                    x = xn;
                    xn = f(&x);
                }
                x
            }

            impl Roots for BigUint {
                // nth_root, sqrt and cbrt use Newton's method to compute
                // principal root of a given degree for a given integer.

                // Reference:
                // Brent & Zimmermann, Modern Computer Arithmetic, v0.5.9, Algorithm 1.14
                fn nth_root(&self, n: u32) -> Self {
                    assert!(n > 0, "root degree n must be at least 1");

                    if self.is_zero() || self.is_one() {
                        return self.clone();
                    }

                    match n {
                        // Optimize for small n
                        1 => return self.clone(),
                        2 => return self.sqrt(),
                        3 => return self.cbrt(),
                        _ => (),
                    }

                    // The root of non-zero values less than 2ⁿ can only be 1.
                    let bits = self.bits();
                    let n64 = u64::from(n);
                    if bits <= n64 {
                        return BigUint::one();
                    }

                    // If we fit in `u64`, compute the root that way.
                    if let Some(x) = self.to_u64() {
                        return x.nth_root(n).into();
                    }

                    let max_bits = bits / n64 + 1;

                    #[cfg(feature = "std")]
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use num_traits::FromPrimitive;

                            // We fit in `f64` (lossy), so get a better initial guess from that.
                            BigUint::from_f64((f.ln() / f64::from(n)).exp()).unwrap()
                        }
                        _ => {
                            // Try to guess by scaling down such that it does fit in `f64`.
                            // With some (x * 2ⁿᵏ), its nth root ≈ (ⁿ√x * 2ᵏ)
                            let extra_bits = bits - (f64::MAX_EXP as u64 - 1);
                            let root_scale = Integer::div_ceil(&extra_bits, &n64);
                            let scale = root_scale * n64;
                            if scale < bits && bits - scale > n64 {
                                (self >> scale).nth_root(n) << root_scale
                            } else {
                                BigUint::one() << max_bits
                            }
                        }
                    };

                    #[cfg(not(feature = "std"))]
                    let guess = BigUint::one() << max_bits;

                    let n_min_1 = n - 1;
                    fixpoint(guess, max_bits, move |s| {
                        let q = self / s.pow(n_min_1);
                        let t = n_min_1 * s + q;
                        t / n
                    })
                }

                // Reference:
                // Brent & Zimmermann, Modern Computer Arithmetic, v0.5.9, Algorithm 1.13
                fn sqrt(&self) -> Self {
                    if self.is_zero() || self.is_one() {
                        return self.clone();
                    }

                    // If we fit in `u64`, compute the root that way.
                    if let Some(x) = self.to_u64() {
                        return x.sqrt().into();
                    }

                    let bits = self.bits();
                    let max_bits = bits / 2 + 1;

                    #[cfg(feature = "std")]
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use num_traits::FromPrimitive;

                            // We fit in `f64` (lossy), so get a better initial guess from that.
                            BigUint::from_f64(f.sqrt()).unwrap()
                        }
                        _ => {
                            // Try to guess by scaling down such that it does fit in `f64`.
                            // With some (x * 2²ᵏ), its sqrt ≈ (√x * 2ᵏ)
                            let extra_bits = bits - (f64::MAX_EXP as u64 - 1);
                            let root_scale = (extra_bits + 1) / 2;
                            let scale = root_scale * 2;
                            (self >> scale).sqrt() << root_scale
                        }
                    };

                    #[cfg(not(feature = "std"))]
                    let guess = BigUint::one() << max_bits;

                    fixpoint(guess, max_bits, move |s| {
                        let q = self / s;
                        let t = s + q;
                        t >> 1
                    })
                }

                fn cbrt(&self) -> Self {
                    if self.is_zero() || self.is_one() {
                        return self.clone();
                    }

                    // If we fit in `u64`, compute the root that way.
                    if let Some(x) = self.to_u64() {
                        return x.cbrt().into();
                    }

                    let bits = self.bits();
                    let max_bits = bits / 3 + 1;

                    #[cfg(feature = "std")]
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use num_traits::FromPrimitive;

                            // We fit in `f64` (lossy), so get a better initial guess from that.
                            BigUint::from_f64(f.cbrt()).unwrap()
                        }
                        _ => {
                            // Try to guess by scaling down such that it does fit in `f64`.
                            // With some (x * 2³ᵏ), its cbrt ≈ (∛x * 2ᵏ)
                            let extra_bits = bits - (f64::MAX_EXP as u64 - 1);
                            let root_scale = (extra_bits + 2) / 3;
                            let scale = root_scale * 3;
                            (self >> scale).cbrt() << root_scale
                        }
                    };

                    #[cfg(not(feature = "std"))]
                    let guess = BigUint::one() << max_bits;

                    fixpoint(guess, max_bits, move |s| {
                        let q = self / (s * s);
                        let t = (s << 1) + q;
                        t / 3u32
                    })
                }
            }
            /// A generic trait for converting a value to a [`BigUint`].
            pub trait ToBigUint {
                /// Converts the value of `self` to a [`BigUint`].
                fn to_biguint(&self) -> Option<BigUint>;
            }
            /// Creates and initializes a [`BigUint`].
            ///
            /// The digits are in little-endian base matching `BigDigit`.
            #[inline]
            pub(crate) fn biguint_from_vec(digits: Vec<BigDigit>) -> BigUint {
                BigUint { data: digits }.normalized()
            }

            impl BigUint {
                /// A constant `BigUint` with value 0, useful for static initialization.
                pub const ZERO: Self = BigUint { data: Vec::new() };

                /// Creates and initializes a [`BigUint`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline]
                pub fn new(digits: Vec<u32>) -> BigUint {
                    let mut big = Self::ZERO;

                    cfg_digit_expr!(
                        {
                            big.data = digits;
                            big.normalize();
                        },
                        big.assign_from_slice(&digits)
                    );

                    big
                }

                /// Creates and initializes a [`BigUint`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline]
                pub fn from_slice(slice: &[u32]) -> BigUint {
                    let mut big = Self::ZERO;
                    big.assign_from_slice(slice);
                    big
                }

                /// Assign a value to a [`BigUint`].
                ///
                /// The base 2<sup>32</sup> digits are ordered least significant digit first.
                #[inline]
                pub fn assign_from_slice(&mut self, slice: &[u32]) {
                    self.data.clear();

                    cfg_digit_expr!(
                        self.data.extend_from_slice(slice),
                        self.data.extend(slice.chunks(2).map(u32_chunk_to_u64))
                    );

                    self.normalize();
                }

                /// Creates and initializes a [`BigUint`].
                ///
                /// The bytes are in big-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from_bytes_be(b"A"),
                ///            BigUint::parse_bytes(b"65", 10).unwrap());
                /// assert_eq!(BigUint::from_bytes_be(b"AA"),
                ///            BigUint::parse_bytes(b"16705", 10).unwrap());
                /// assert_eq!(BigUint::from_bytes_be(b"AB"),
                ///            BigUint::parse_bytes(b"16706", 10).unwrap());
                /// assert_eq!(BigUint::from_bytes_be(b"Hello world!"),
                ///            BigUint::parse_bytes(b"22405534230753963835153736737", 10).unwrap());
                /// ```
                #[inline]
                pub fn from_bytes_be(bytes: &[u8]) -> BigUint {
                    if bytes.is_empty() {
                        Self::ZERO
                    } else {
                        let mut v = bytes.to_vec();
                        v.reverse();
                        BigUint::from_bytes_le(&v)
                    }
                }

                /// Creates and initializes a [`BigUint`].
                ///
                /// The bytes are in little-endian byte order.
                #[inline]
                pub fn from_bytes_le(bytes: &[u8]) -> BigUint {
                    if bytes.is_empty() {
                        Self::ZERO
                    } else {
                        convert::from_bitwise_digits_le(bytes, 8)
                    }
                }

                /// Creates and initializes a [`BigUint`]. The input slice must contain
                /// ascii/utf8 characters in [0-9a-zA-Z].
                /// `radix` must be in the range `2...36`.
                ///
                /// The function `from_str_radix` from the `Num` trait provides the same logic
                /// for `&str` buffers.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigUint, ToBigUint};
                ///
                /// assert_eq!(BigUint::parse_bytes(b"1234", 10), ToBigUint::to_biguint(&1234));
                /// assert_eq!(BigUint::parse_bytes(b"ABCD", 16), ToBigUint::to_biguint(&0xABCD));
                /// assert_eq!(BigUint::parse_bytes(b"G", 16), None);
                /// ```
                #[inline]
                pub fn parse_bytes(buf: &[u8], radix: u32) -> Option<BigUint> {
                    let s = str::from_utf8(buf).ok()?;
                    BigUint::from_str_radix(s, radix).ok()
                }

                /// Creates and initializes a [`BigUint`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                ///
                /// The bytes are in big-endian byte order.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigUint};
                ///
                /// let inbase190 = &[15, 33, 125, 12, 14];
                /// let a = BigUint::from_radix_be(inbase190, 190).unwrap();
                /// assert_eq!(a.to_radix_be(190), inbase190);
                /// ```
                pub fn from_radix_be(buf: &[u8], radix: u32) -> Option<BigUint> {
                    convert::from_radix_be(buf, radix)
                }

                /// Creates and initializes a [`BigUint`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                ///
                /// The bytes are in little-endian byte order.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::{BigUint};
                ///
                /// let inbase190 = &[14, 12, 125, 33, 15];
                /// let a = BigUint::from_radix_be(inbase190, 190).unwrap();
                /// assert_eq!(a.to_radix_be(190), inbase190);
                /// ```
                pub fn from_radix_le(buf: &[u8], radix: u32) -> Option<BigUint> {
                    convert::from_radix_le(buf, radix)
                }

                /// Returns the byte representation of the [`BigUint`] in big-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// let i = BigUint::parse_bytes(b"1125", 10).unwrap();
                /// assert_eq!(i.to_bytes_be(), vec![4, 101]);
                /// ```
                #[inline]
                pub fn to_bytes_be(&self) -> Vec<u8> {
                    let mut v = self.to_bytes_le();
                    v.reverse();
                    v
                }

                /// Returns the byte representation of the [`BigUint`] in little-endian byte order.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// let i = BigUint::parse_bytes(b"1125", 10).unwrap();
                /// assert_eq!(i.to_bytes_le(), vec![101, 4]);
                /// ```
                #[inline]
                pub fn to_bytes_le(&self) -> Vec<u8> {
                    if self.is_zero() {
                        vec![0]
                    } else {
                        convert::to_bitwise_digits_le(self, 8)
                    }
                }

                /// Returns the `u32` digits representation of the [`BigUint`] ordered least significant digit
                /// first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(1125u32).to_u32_digits(), vec![1125]);
                /// assert_eq!(BigUint::from(4294967295u32).to_u32_digits(), vec![4294967295]);
                /// assert_eq!(BigUint::from(4294967296u64).to_u32_digits(), vec![0, 1]);
                /// assert_eq!(BigUint::from(112500000000u64).to_u32_digits(), vec![830850304, 26]);
                /// ```
                #[inline]
                pub fn to_u32_digits(&self) -> Vec<u32> {
                    self.iter_u32_digits().collect()
                }

                /// Returns the `u64` digits representation of the [`BigUint`] ordered least significant digit
                /// first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(1125u32).to_u64_digits(), vec![1125]);
                /// assert_eq!(BigUint::from(4294967295u32).to_u64_digits(), vec![4294967295]);
                /// assert_eq!(BigUint::from(4294967296u64).to_u64_digits(), vec![4294967296]);
                /// assert_eq!(BigUint::from(112500000000u64).to_u64_digits(), vec![112500000000]);
                /// assert_eq!(BigUint::from(1u128 << 64).to_u64_digits(), vec![0, 1]);
                /// ```
                #[inline]
                pub fn to_u64_digits(&self) -> Vec<u64> {
                    self.iter_u64_digits().collect()
                }

                /// Returns an iterator of `u32` digits representation of the [`BigUint`] ordered least
                /// significant digit first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(1125u32).iter_u32_digits().collect::<Vec<u32>>(), vec![1125]);
                /// assert_eq!(BigUint::from(4294967295u32).iter_u32_digits().collect::<Vec<u32>>(), vec![4294967295]);
                /// assert_eq!(BigUint::from(4294967296u64).iter_u32_digits().collect::<Vec<u32>>(), vec![0, 1]);
                /// assert_eq!(BigUint::from(112500000000u64).iter_u32_digits().collect::<Vec<u32>>(), vec![830850304, 26]);
                /// ```
                #[inline]
                pub fn iter_u32_digits(&self) -> U32Digits<'_> {
                    U32Digits::new(self.data.as_slice())
                }

                /// Returns an iterator of `u64` digits representation of the [`BigUint`] ordered least
                /// significant digit first.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(1125u32).iter_u64_digits().collect::<Vec<u64>>(), vec![1125]);
                /// assert_eq!(BigUint::from(4294967295u32).iter_u64_digits().collect::<Vec<u64>>(), vec![4294967295]);
                /// assert_eq!(BigUint::from(4294967296u64).iter_u64_digits().collect::<Vec<u64>>(), vec![4294967296]);
                /// assert_eq!(BigUint::from(112500000000u64).iter_u64_digits().collect::<Vec<u64>>(), vec![112500000000]);
                /// assert_eq!(BigUint::from(1u128 << 64).iter_u64_digits().collect::<Vec<u64>>(), vec![0, 1]);
                /// ```
                #[inline]
                pub fn iter_u64_digits(&self) -> U64Digits<'_> {
                    U64Digits::new(self.data.as_slice())
                }

                /// Returns the integer formatted as a string in the given radix.
                /// `radix` must be in the range `2...36`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// let i = BigUint::parse_bytes(b"ff", 16).unwrap();
                /// assert_eq!(i.to_str_radix(16), "ff");
                /// ```
                #[inline]
                pub fn to_str_radix(&self, radix: u32) -> String {
                    let mut v = to_str_radix_reversed(self, radix);
                    v.reverse();
                    unsafe { String::from_utf8_unchecked(v) }
                }

                /// Returns the integer in the requested base in big-endian digit order.
                /// The output is not given in a human readable alphabet but as a zero
                /// based `u8` number.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(0xFFFFu64).to_radix_be(159),
                ///            vec![2, 94, 27]);
                /// // 0xFFFF = 65535 = 2*(159^2) + 94*159 + 27
                /// ```
                #[inline]
                pub fn to_radix_be(&self, radix: u32) -> Vec<u8> {
                    let mut v = convert::to_radix_le(self, radix);
                    v.reverse();
                    v
                }

                /// Returns the integer in the requested base in little-endian digit order.
                /// The output is not given in a human readable alphabet but as a zero
                /// based u8 number.
                /// `radix` must be in the range `2...256`.
                ///
                /// # Examples
                ///
                /// ```
                /// use num_bigint::BigUint;
                ///
                /// assert_eq!(BigUint::from(0xFFFFu64).to_radix_le(159),
                ///            vec![27, 94, 2]);
                /// // 0xFFFF = 65535 = 27 + 94*159 + 2*(159^2)
                /// ```
                #[inline]
                pub fn to_radix_le(&self, radix: u32) -> Vec<u8> {
                    convert::to_radix_le(self, radix)
                }

                /// Determines the fewest bits necessary to express the [`BigUint`].
                #[inline]
                pub fn bits(&self) -> u64 {
                    if self.is_zero() {
                        return 0;
                    }
                    let zeros: u64 = self.data.last().unwrap().leading_zeros().into();
                    self.data.len() as u64 * u64::from(big_digit::BITS) - zeros
                }

                /// Strips off trailing zero bigdigits - comparisons require the last element in the vector to
                /// be nonzero.
                #[inline]
                fn normalize(&mut self) {
                    if let Some(&0) = self.data.last() {
                        let len = self.data.iter().rposition(|&d| d != 0).map_or(0, |i| i + 1);
                        self.data.truncate(len);
                    }
                    if self.data.len() < self.data.capacity() / 4 {
                        self.data.shrink_to_fit();
                    }
                }

                /// Returns a normalized [`BigUint`].
                #[inline]
                fn normalized(mut self) -> BigUint {
                    self.normalize();
                    self
                }

                /// Returns `self ^ exponent`.
                pub fn pow(&self, exponent: u32) -> Self {
                    Pow::pow(self, exponent)
                }

                /// Returns `(self ^ exponent) % modulus`.
                ///
                /// Panics if the modulus is zero.
                pub fn modpow(&self, exponent: &Self, modulus: &Self) -> Self {
                    power::modpow(self, exponent, modulus)
                }

                /// Returns the modular multiplicative inverse if it exists, otherwise `None`.
                ///
                /// This solves for `x` in the interval `[0, modulus)` such that `self * x ≡ 1 (mod modulus)`.
                /// The solution exists if and only if `gcd(self, modulus) == 1`.
                ///
                /// ```
                /// use num_bigint::BigUint;
                /// use num_traits::{One, Zero};
                ///
                /// let m = BigUint::from(383_u32);
                ///
                /// // Trivial cases
                /// assert_eq!(BigUint::zero().modinv(&m), None);
                /// assert_eq!(BigUint::one().modinv(&m), Some(BigUint::one()));
                /// let neg1 = &m - 1u32;
                /// assert_eq!(neg1.modinv(&m), Some(neg1));
                ///
                /// let a = BigUint::from(271_u32);
                /// let x = a.modinv(&m).unwrap();
                /// assert_eq!(x, BigUint::from(106_u32));
                /// assert_eq!(x.modinv(&m).unwrap(), a);
                /// assert!((a * x % m).is_one());
                /// ```
                pub fn modinv(&self, modulus: &Self) -> Option<Self> {
                    // Based on the inverse pseudocode listed here:
                    // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
                    // TODO: consider Binary or Lehmer's GCD algorithms for optimization.

                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );
                    if modulus.is_one() {
                        return Some(Self::zero());
                    }

                    let mut r0; // = modulus.clone();
                    let mut r1 = self % modulus;
                    let mut t0; // = Self::zero();
                    let mut t1; // = Self::one();

                    // Lift and simplify the first iteration to avoid some initial allocations.
                    if r1.is_zero() {
                        return None;
                    } else if r1.is_one() {
                        return Some(r1);
                    } else {
                        let (q, r2) = modulus.div_rem(&r1);
                        if r2.is_zero() {
                            return None;
                        }
                        r0 = r1;
                        r1 = r2;
                        t0 = Self::one();
                        t1 = modulus - q;
                    }

                    while !r1.is_zero() {
                        let (q, r2) = r0.div_rem(&r1);
                        r0 = r1;
                        r1 = r2;

                        // let t2 = (t0 - q * t1) % modulus;
                        let qt1 = q * &t1 % modulus;
                        let t2 = if t0 < qt1 {
                            t0 + (modulus - qt1)
                        } else {
                            t0 - qt1
                        };
                        t0 = t1;
                        t1 = t2;
                    }

                    if r0.is_one() {
                        Some(t0)
                    } else {
                        None
                    }
                }

                /// Returns the truncated principal square root of `self` --
                /// see [Roots::sqrt](https://docs.rs/num-integer/0.1/num_integer/trait.Roots.html#method.sqrt)
                pub fn sqrt(&self) -> Self {
                    Roots::sqrt(self)
                }

                /// Returns the truncated principal cube root of `self` --
                /// see [Roots::cbrt](https://docs.rs/num-integer/0.1/num_integer/trait.Roots.html#method.cbrt).
                pub fn cbrt(&self) -> Self {
                    Roots::cbrt(self)
                }

                /// Returns the truncated principal `n`th root of `self` --
                /// see [Roots::nth_root](https://docs.rs/num-integer/0.1/num_integer/trait.Roots.html#tymethod.nth_root).
                pub fn nth_root(&self, n: u32) -> Self {
                    Roots::nth_root(self, n)
                }

                /// Returns the number of least-significant bits that are zero,
                /// or `None` if the entire number is zero.
                pub fn trailing_zeros(&self) -> Option<u64> {
                    let i = self.data.iter().position(|&digit| digit != 0)?;
                    let zeros: u64 = self.data[i].trailing_zeros().into();
                    Some(i as u64 * u64::from(big_digit::BITS) + zeros)
                }

                /// Returns the number of least-significant bits that are ones.
                pub fn trailing_ones(&self) -> u64 {
                    if let Some(i) = self.data.iter().position(|&digit| !digit != 0) {
                        let ones: u64 = self.data[i].trailing_ones().into();
                        i as u64 * u64::from(big_digit::BITS) + ones
                    } else {
                        self.data.len() as u64 * u64::from(big_digit::BITS)
                    }
                }

                /// Returns the number of one bits.
                pub fn count_ones(&self) -> u64 {
                    self.data.iter().map(|&d| u64::from(d.count_ones())).sum()
                }

                /// Returns whether the bit in the given position is set
                pub fn bit(&self, bit: u64) -> bool {
                    let bits_per_digit = u64::from(big_digit::BITS);
                    if let Some(digit_index) = (bit / bits_per_digit).to_usize() {
                        if let Some(digit) = self.data.get(digit_index) {
                            let bit_mask = (1 as BigDigit) << (bit % bits_per_digit);
                            return (digit & bit_mask) != 0;
                        }
                    }
                    false
                }

                /// Sets or clears the bit in the given position
                ///
                /// Note that setting a bit greater than the current bit length, a reallocation may be needed
                /// to store the new digits
                pub fn set_bit(&mut self, bit: u64, value: bool) {
                    // Note: we're saturating `digit_index` and `new_len` -- any such case is guaranteed to
                    // fail allocation, and that's more consistent than adding our own overflow panics.
                    let bits_per_digit = u64::from(big_digit::BITS);
                    let digit_index = (bit / bits_per_digit).to_usize().unwrap_or(usize::MAX);
                    let bit_mask = (1 as BigDigit) << (bit % bits_per_digit);
                    if value {
                        if digit_index >= self.data.len() {
                            let new_len = digit_index.saturating_add(1);
                            self.data.resize(new_len, 0);
                        }
                        self.data[digit_index] |= bit_mask;
                    } else if digit_index < self.data.len() {
                        self.data[digit_index] &= !bit_mask;
                        // the top bit may have been cleared, so normalize
                        self.normalize();
                    }
                }
            }

            impl num_traits::FromBytes for BigUint {
                type Bytes = [u8];

                fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_bytes_be(bytes)
                }

                fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_bytes_le(bytes)
                }
            }

            impl num_traits::ToBytes for BigUint {
                type Bytes = Vec<u8>;

                fn to_be_bytes(&self) -> Self::Bytes {
                    self.to_bytes_be()
                }

                fn to_le_bytes(&self) -> Self::Bytes {
                    self.to_bytes_le()
                }
            }

            pub(crate) trait IntDigits {
                fn digits(&self) -> &[BigDigit];
                fn digits_mut(&mut self) -> &mut Vec<BigDigit>;
                fn normalize(&mut self);
                fn capacity(&self) -> usize;
                fn len(&self) -> usize;
            }

            impl IntDigits for BigUint {
                #[inline]
                fn digits(&self) -> &[BigDigit] {
                    &self.data
                }
                #[inline]
                fn digits_mut(&mut self) -> &mut Vec<BigDigit> {
                    &mut self.data
                }
                #[inline]
                fn normalize(&mut self) {
                    self.normalize();
                }
                #[inline]
                fn capacity(&self) -> usize {
                    self.data.capacity()
                }
                #[inline]
                fn len(&self) -> usize {
                    self.data.len()
                }
            }
            /// Convert a `u32` chunk (len is either 1 or 2) to a single `u64` digit
            #[inline] fn u32_chunk_to_u64(chunk: &[u32]) -> u64 {
                // raw could have odd length
                let mut digit = chunk[0] as u64;
                if let Some(&hi) = chunk.get(1) {
                    digit |= (hi as u64) << 32;
                }
                digit
            }
        }
        

        #[cfg(target_pointer_width = "32")]
        type UsizePromotion = u32;
        #[cfg(target_pointer_width = "64")]
        type UsizePromotion = u64;

        #[cfg(target_pointer_width = "32")]
        type IsizePromotion = i32;
        #[cfg(target_pointer_width = "64")]
        type IsizePromotion = i64;

        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct ParseBigIntError {
            kind: BigIntErrorKind,
        }

        #[derive(Debug, Clone, PartialEq, Eq)]
        enum BigIntErrorKind {
            Empty,
            InvalidDigit,
        }

        impl ParseBigIntError {
            fn __description(&self) -> &str {
                use BigIntErrorKind::*;
                match self.kind {
                    Empty => "cannot parse integer from empty string",
                    InvalidDigit => "invalid digit found in string",
                }
            }

            fn empty() -> Self {
                ParseBigIntError {
                    kind: BigIntErrorKind::Empty,
                }
            }

            fn invalid() -> Self {
                ParseBigIntError {
                    kind: BigIntErrorKind::InvalidDigit,
                }
            }
        }

        impl fmt::Display for ParseBigIntError {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.__description().fmt(f)
            }
        }
        
        impl std::error::Error for ParseBigIntError {
            fn description(&self) -> &str {
                self.__description()
            }
        }
        /// The error type returned when a checked conversion regarding big integer fails.
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub struct TryFromBigIntError<T> {
            original: T,
        }

        impl<T> TryFromBigIntError<T> {
            fn new(original: T) -> Self {
                TryFromBigIntError { original }
            }

            fn __description(&self) -> &str {
                "out of range conversion regarding big integer attempted"
            }
            /// Extract the original value, if available. The value will be available
            /// if the type before conversion was either [`BigInt`] or [`BigUint`].
            pub fn into_original(self) -> T {
                self.original
            }
        }
        
        impl<T> std::error::Error for TryFromBigIntError<T>
        where
            T: fmt::Debug,
        {
            fn description(&self) -> &str {
                self.__description()
            }
        }

        impl<T> fmt::Display for TryFromBigIntError<T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.__description().fmt(f)
            }
        }

        pub use self::uint::BigUint;
        pub use self::uint::ToBigUint;
        pub use self::uint::U32Digits;
        pub use self::uint::U64Digits;

        pub use self::int::BigInt;
        pub use self::int::Sign;
        pub use self::int::ToBigInt;
        /*
        #[cfg(feature = "rand")]
        #[cfg_attr(docsrs, doc(cfg(feature = "rand")))]
        pub use crate::bigrand::{RandBigInt, RandomBits, UniformBigInt, UniformBigUint};
        */

        pub mod digit
        {
            // A [`BigDigit`] is a [`BigUint`]'s composing element.
            cfg_digit!(
                pub type BigDigit = u32;
                pub type BigDigit = u64;
            );

            // A [`DoubleBigDigit`] is the internal type used to do the computations.  Its
            // size is the double of the size of [`BigDigit`].
            cfg_digit!(
                pub type DoubleBigDigit = u64;
                pub type DoubleBigDigit = u128;
            );

            pub const BITS: u8 = BigDigit::BITS as u8;
            pub const HALF_BITS: u8 = BITS / 2;
            pub const HALF: BigDigit = (1 << HALF_BITS) - 1;

            pub const MAX: BigDigit = BigDigit::MAX;
            const LO_MASK: DoubleBigDigit = MAX as DoubleBigDigit;

            #[inline] fn get_hi(n: DoubleBigDigit) -> BigDigit {
                (n >> BITS) as BigDigit
            }
            #[inline] fn get_lo(n: DoubleBigDigit) -> BigDigit {
                (n & LO_MASK) as BigDigit
            }
            /// Split one [`DoubleBigDigit`] into two [`BigDigit`]s.
            #[inline] pub fn from_doublebigdigit(n: DoubleBigDigit) -> (BigDigit, BigDigit) {
                (get_hi(n), get_lo(n))
            }
            /// Join two [`BigDigit`]s into one [`DoubleBigDigit`].
            #[inline] pub fn to_doublebigdigit(hi: BigDigit, lo: BigDigit) -> DoubleBigDigit {
                DoubleBigDigit::from(lo) | (DoubleBigDigit::from(hi) << BITS)
            }
        }
    }

    pub mod integer
    {
        /*!
        Integer trait and functions. */
        use ::
        {
            *,
        };
        /*
        use core::mem;
        use core::ops::Add;

        use num_traits::{Num, Signed, Zero};

        mod roots;
        pub use crate::roots::Roots;
        pub use crate::roots::{cbrt, nth_root, sqrt};

        mod average;
        pub use crate::average::Average;
        pub use crate::average::{average_ceil, average_floor};
        */
    }
    
    pub mod traits
    {
        /*!
        Numeric traits for generic mathematics */
        use ::
        {
            *,
        };
        /*        
        use core::fmt;
        use core::num::Wrapping;
        use core::ops::{Add, Div, Mul, Rem, Sub};
        use core::ops::{AddAssign, DivAssign, MulAssign, RemAssign, SubAssign};

        pub use crate::bounds::Bounded;
        #[cfg(any(feature = "std", feature = "libm"))]
        pub use crate::float::Float;
        pub use crate::float::FloatConst;
        // pub use real::{FloatCore, Real}; // NOTE: Don't do this, it breaks `use num_traits::*;`.
        pub use crate::cast::{cast, AsPrimitive, FromPrimitive, NumCast, ToPrimitive};
        pub use crate::identities::{one, zero, ConstOne, ConstZero, One, Zero};
        pub use crate::int::PrimInt;
        pub use crate::ops::bytes::{FromBytes, ToBytes};
        pub use crate::ops::checked::{
            CheckedAdd, CheckedDiv, CheckedMul, CheckedNeg, CheckedRem, CheckedShl, CheckedShr, CheckedSub,
        };
        pub use crate::ops::euclid::{CheckedEuclid, Euclid};
        pub use crate::ops::inv::Inv;
        pub use crate::ops::mul_add::{MulAdd, MulAddAssign};
        pub use crate::ops::saturating::{Saturating, SaturatingAdd, SaturatingMul, SaturatingSub};
        pub use crate::ops::wrapping::{
            WrappingAdd, WrappingMul, WrappingNeg, WrappingShl, WrappingShr, WrappingSub,
        };
        pub use crate::pow::{checked_pow, pow, Pow};
        pub use crate::sign::{abs, abs_sub, signum, Signed, Unsigned};
        */
        pub mod bounds
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

        pub mod cast
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
        
        pub mod float
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
        
        pub mod identities
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
        
        pub mod int
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
        
        pub mod ops
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
        
        pub mod pow
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
        
        pub mod real
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
        
        pub mod sign
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
        /// The base trait for numeric types, covering `0` and `1` values,
        /// comparisons, basic numeric operations, and string conversion.
        pub trait Num: PartialEq + Zero + One + NumOps {
            type FromStrRadixErr;
            /// Convert from a string and radix (typically `2..=36`).
            ///
            /// # Examples
            ///
            /// ```rust
            /// use num_traits::Num;
            ///
            /// let result = <i32 as Num>::from_str_radix("27", 10);
            /// assert_eq!(result, Ok(27));
            ///
            /// let result = <i32 as Num>::from_str_radix("foo", 10);
            /// assert!(result.is_err());
            /// ```
            ///
            /// # Supported radices
            ///
            /// The exact range of supported radices is at the discretion of each type implementation. For
            /// primitive integers, this is implemented by the inherent `from_str_radix` methods in the
            /// standard library, which **panic** if the radix is not in the range from 2 to 36. The
            /// implementation in this crate for primitive floats is similar.
            ///
            /// For third-party types, it is suggested that implementations should follow suit and at least
            /// accept `2..=36` without panicking, but an `Err` may be returned for any unsupported radix.
            /// It's possible that a type might not even support the common radix 10, nor any, if string
            /// parsing doesn't make sense for that type.
            fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr>;
        }

        /// Generic trait for types implementing basic numeric operations
        ///
        /// This is automatically implemented for types which implement the operators.
        pub trait NumOps<Rhs = Self, Output = Self>:
            Add<Rhs, Output = Output>
            + Sub<Rhs, Output = Output>
            + Mul<Rhs, Output = Output>
            + Div<Rhs, Output = Output>
            + Rem<Rhs, Output = Output>
        {
        }

        impl<T, Rhs, Output> NumOps<Rhs, Output> for T where
            T: Add<Rhs, Output = Output>
                + Sub<Rhs, Output = Output>
                + Mul<Rhs, Output = Output>
                + Div<Rhs, Output = Output>
                + Rem<Rhs, Output = Output>
        {
        }

        /// The trait for `Num` types which also implement numeric operations taking
        /// the second operand by reference.
        ///
        /// This is automatically implemented for types which implement the operators.
        pub trait NumRef: Num + for<'r> NumOps<&'r Self> {}
        impl<T> NumRef for T where T: Num + for<'r> NumOps<&'r T> {}

        /// The trait for `Num` references which implement numeric operations, taking the
        /// second operand either by value or by reference.
        ///
        /// This is automatically implemented for all types which implement the operators. It covers
        /// every type implementing the operations though, regardless of it being a reference or
        /// related to `Num`.
        pub trait RefNum<Base>: NumOps<Base, Base> + for<'r> NumOps<&'r Base, Base> {}
        impl<T, Base> RefNum<Base> for T where T: NumOps<Base, Base> + for<'r> NumOps<&'r Base, Base> {}

        /// Generic trait for types implementing numeric assignment operators (like `+=`).
        ///
        /// This is automatically implemented for types which implement the operators.
        pub trait NumAssignOps<Rhs = Self>:
            AddAssign<Rhs> + SubAssign<Rhs> + MulAssign<Rhs> + DivAssign<Rhs> + RemAssign<Rhs>
        {
        }

        impl<T, Rhs> NumAssignOps<Rhs> for T where
            T: AddAssign<Rhs> + SubAssign<Rhs> + MulAssign<Rhs> + DivAssign<Rhs> + RemAssign<Rhs>
        {
        }

        /// The trait for `Num` types which also implement assignment operators.
        ///
        /// This is automatically implemented for types which implement the operators.
        pub trait NumAssign: Num + NumAssignOps {}
        impl<T> NumAssign for T where T: Num + NumAssignOps {}

        /// The trait for `NumAssign` types which also implement assignment operations
        /// taking the second operand by reference.
        ///
        /// This is automatically implemented for types which implement the operators.
        pub trait NumAssignRef: NumAssign + for<'r> NumAssignOps<&'r Self> {}
        impl<T> NumAssignRef for T where T: NumAssign + for<'r> NumAssignOps<&'r T> {}

        macro_rules! int_trait_impl {
            ($name:ident for $($t:ty)*) => ($(
                impl $name for $t {
                    type FromStrRadixErr = ::core::num::ParseIntError;
                    #[inline]
                    fn from_str_radix(s: &str, radix: u32)
                                    -> Result<Self, ::core::num::ParseIntError>
                    {
                        <$t>::from_str_radix(s, radix)
                    }
                }
            )*)
        }
        int_trait_impl!(Num for usize u8 u16 u32 u64 u128);
        int_trait_impl!(Num for isize i8 i16 i32 i64 i128);

        impl<T: Num> Num for Wrapping<T>
        where
            Wrapping<T>: NumOps,
        {
            type FromStrRadixErr = T::FromStrRadixErr;
            fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
                T::from_str_radix(str, radix).map(Wrapping)
            }
        }

        #[cfg(has_num_saturating)]
        impl<T: Num> Num for core::num::Saturating<T>
        where
            core::num::Saturating<T>: NumOps,
        {
            type FromStrRadixErr = T::FromStrRadixErr;
            fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
                T::from_str_radix(str, radix).map(core::num::Saturating)
            }
        }

        #[derive(Debug)]
        pub enum FloatErrorKind {
            Empty,
            Invalid,
        }
        // FIXME: core::num::ParseFloatError is stable in 1.0, but opaque to us,
        // so there's not really any way for us to reuse it.
        #[derive(Debug)]
        pub struct ParseFloatError {
            pub kind: FloatErrorKind,
        }

        impl fmt::Display for ParseFloatError {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let description = match self.kind {
                    FloatErrorKind::Empty => "cannot parse float from empty string",
                    FloatErrorKind::Invalid => "invalid float literal",
                };

                description.fmt(f)
            }
        }

        fn str_to_ascii_lower_eq_str(a: &str, b: &str) -> bool {
            a.len() == b.len()
                && a.bytes().zip(b.bytes()).all(|(a, b)| {
                    let a_to_ascii_lower = a | (((b'A' <= a && a <= b'Z') as u8) << 5);
                    a_to_ascii_lower == b
                })
        }

        // FIXME: The standard library from_str_radix on floats was deprecated, so we're stuck
        // with this implementation ourselves until we want to make a breaking change.
        // (would have to drop it from `Num` though)
        macro_rules! float_trait_impl {
            ($name:ident for $($t:ident)*) => ($(
                impl $name for $t {
                    type FromStrRadixErr = ParseFloatError;

                    fn from_str_radix(src: &str, radix: u32)
                                    -> Result<Self, Self::FromStrRadixErr>
                    {
                        use self::FloatErrorKind::*;
                        use self::ParseFloatError as PFE;

                        // Special case radix 10 to use more accurate standard library implementation
                        if radix == 10 {
                            return src.parse().map_err(|_| PFE {
                                kind: if src.is_empty() { Empty } else { Invalid },
                            });
                        }

                        // Special values
                        if str_to_ascii_lower_eq_str(src, "inf")
                            || str_to_ascii_lower_eq_str(src, "infinity")
                        {
                            return Ok(core::$t::INFINITY);
                        } else if str_to_ascii_lower_eq_str(src, "-inf")
                            || str_to_ascii_lower_eq_str(src, "-infinity")
                        {
                            return Ok(core::$t::NEG_INFINITY);
                        } else if str_to_ascii_lower_eq_str(src, "nan") {
                            return Ok(core::$t::NAN);
                        } else if str_to_ascii_lower_eq_str(src, "-nan") {
                            return Ok(-core::$t::NAN);
                        }

                        fn slice_shift_char(src: &str) -> Option<(char, &str)> {
                            let mut chars = src.chars();
                            Some((chars.next()?, chars.as_str()))
                        }

                        let (is_positive, src) =  match slice_shift_char(src) {
                            None             => return Err(PFE { kind: Empty }),
                            Some(('-', ""))  => return Err(PFE { kind: Empty }),
                            Some(('-', src)) => (false, src),
                            Some((_, _))     => (true,  src),
                        };

                        // The significand to accumulate
                        let mut sig = if is_positive { 0.0 } else { -0.0 };
                        // Necessary to detect overflow
                        let mut prev_sig = sig;
                        let mut cs = src.chars().enumerate();
                        // Exponent prefix and exponent index offset
                        let mut exp_info = None::<(char, usize)>;

                        // Parse the integer part of the significand
                        for (i, c) in cs.by_ref() {
                            match c.to_digit(radix) {
                                Some(digit) => {
                                    // shift significand one digit left
                                    sig *= radix as $t;

                                    // add/subtract current digit depending on sign
                                    if is_positive {
                                        sig += (digit as isize) as $t;
                                    } else {
                                        sig -= (digit as isize) as $t;
                                    }

                                    // Detect overflow by comparing to last value, except
                                    // if we've not seen any non-zero digits.
                                    if prev_sig != 0.0 {
                                        if is_positive && sig <= prev_sig
                                            { return Ok(core::$t::INFINITY); }
                                        if !is_positive && sig >= prev_sig
                                            { return Ok(core::$t::NEG_INFINITY); }

                                        // Detect overflow by reversing the shift-and-add process
                                        if is_positive && (prev_sig != (sig - digit as $t) / radix as $t)
                                            { return Ok(core::$t::INFINITY); }
                                        if !is_positive && (prev_sig != (sig + digit as $t) / radix as $t)
                                            { return Ok(core::$t::NEG_INFINITY); }
                                    }
                                    prev_sig = sig;
                                },
                                None => match c {
                                    'e' | 'E' | 'p' | 'P' => {
                                        exp_info = Some((c, i + 1));
                                        break;  // start of exponent
                                    },
                                    '.' => {
                                        break;  // start of fractional part
                                    },
                                    _ => {
                                        return Err(PFE { kind: Invalid });
                                    },
                                },
                            }
                        }

                        // If we are not yet at the exponent parse the fractional
                        // part of the significand
                        if exp_info.is_none() {
                            let mut power = 1.0;
                            for (i, c) in cs.by_ref() {
                                match c.to_digit(radix) {
                                    Some(digit) => {
                                        // Decrease power one order of magnitude
                                        power /= radix as $t;
                                        // add/subtract current digit depending on sign
                                        sig = if is_positive {
                                            sig + (digit as $t) * power
                                        } else {
                                            sig - (digit as $t) * power
                                        };
                                        // Detect overflow by comparing to last value
                                        if is_positive && sig < prev_sig
                                            { return Ok(core::$t::INFINITY); }
                                        if !is_positive && sig > prev_sig
                                            { return Ok(core::$t::NEG_INFINITY); }
                                        prev_sig = sig;
                                    },
                                    None => match c {
                                        'e' | 'E' | 'p' | 'P' => {
                                            exp_info = Some((c, i + 1));
                                            break; // start of exponent
                                        },
                                        _ => {
                                            return Err(PFE { kind: Invalid });
                                        },
                                    },
                                }
                            }
                        }

                        // Parse and calculate the exponent
                        let exp = match exp_info {
                            Some((c, offset)) => {
                                let base = match c {
                                    'E' | 'e' if radix == 10 => 10.0,
                                    'P' | 'p' if radix == 16 => 2.0,
                                    _ => return Err(PFE { kind: Invalid }),
                                };

                                // Parse the exponent as decimal integer
                                let src = &src[offset..];
                                let (is_positive, exp) = match slice_shift_char(src) {
                                    Some(('-', src)) => (false, src.parse::<usize>()),
                                    Some(('+', src)) => (true,  src.parse::<usize>()),
                                    Some((_, _))     => (true,  src.parse::<usize>()),
                                    None             => return Err(PFE { kind: Invalid }),
                                };

                                #[cfg(feature = "std")]
                                fn pow(base: $t, exp: usize) -> $t {
                                    Float::powi(base, exp as i32)
                                }
                                // otherwise uses the generic `pow` from the root

                                match (is_positive, exp) {
                                    (true,  Ok(exp)) => pow(base, exp),
                                    (false, Ok(exp)) => 1.0 / pow(base, exp),
                                    (_, Err(_))      => return Err(PFE { kind: Invalid }),
                                }
                            },
                            None => 1.0, // no exponent
                        };

                        Ok(sig * exp)
                    }
                }
            )*)
        }
        float_trait_impl!(Num for f32 f64);

        /// A value bounded by a minimum and a maximum
        ///
        ///  If input is less than min then this returns min.
        ///  If input is greater than max then this returns max.
        ///  Otherwise this returns input.
        ///
        /// **Panics** in debug mode if `!(min <= max)`.
        #[inline]
        pub fn clamp<T: PartialOrd>(input: T, min: T, max: T) -> T {
            debug_assert!(min <= max, "min must be less than or equal to max");
            if input < min {
                min
            } else if input > max {
                max
            } else {
                input
            }
        }

        /// A value bounded by a minimum value
        ///
        ///  If input is less than min then this returns min.
        ///  Otherwise this returns input.
        ///  `clamp_min(std::f32::NAN, 1.0)` preserves `NAN` different from `f32::min(std::f32::NAN, 1.0)`.
        ///
        /// **Panics** in debug mode if `!(min == min)`. (This occurs if `min` is `NAN`.)
        #[inline]
        #[allow(clippy::eq_op)]
        pub fn clamp_min<T: PartialOrd>(input: T, min: T) -> T {
            debug_assert!(min == min, "min must not be NAN");
            if input < min {
                min
            } else {
                input
            }
        }

        /// A value bounded by a maximum value
        ///
        ///  If input is greater than max then this returns max.
        ///  Otherwise this returns input.
        ///  `clamp_max(std::f32::NAN, 1.0)` preserves `NAN` different from `f32::max(std::f32::NAN, 1.0)`.
        ///
        /// **Panics** in debug mode if `!(max == max)`. (This occurs if `max` is `NAN`.)
        #[inline]
        #[allow(clippy::eq_op)]
        pub fn clamp_max<T: PartialOrd>(input: T, max: T) -> T {
            debug_assert!(max == max, "max must not be NAN");
            if input > max {
                max
            } else {
                input
            }
        }


    }
}

pub mod obj
{
	/*!
	Obj | A hashmap of keys to values, where values can be any type, including other objects.*/
	use ::
	{
		arr::{ Arr },
		collections::{ hash_map::{Iter, Keys, Values}, HashMap },
		error::{ OverError },
		fmt::{ self, Format },
		fs::{ write_str },
		is::{ digit },
		num::
		{
			big::BigInt,
			rational::BigRational,
			traits::Zero,
		},
		str::{ FromStr },
		sync::{ atomic::{AtomicUsize, Ordering}, Arc },
		tup::{ Tup },
		value::{ Value },
		OverResult, INDENT_STEP,
		*,
	};
	/*
	*/
	lazy_static!
	{
		static ref CUR_ID: AtomicUsize = AtomicUsize::new(0);
	}

	fn get_id() -> usize {
		CUR_ID.fetch_add(1, Ordering::Relaxed)
	}

	#[derive(Clone, Debug)]
	struct ObjInner {
		map: HashMap<String, Value>,
		parent: Option<Obj>,
		id: usize,
	}

	/// `Obj` struct.
	#[derive(Clone, Debug)]
	pub struct Obj {
		inner: Arc<ObjInner>,
	}

	macro_rules! get_fn {
		( $doc:expr, $name:tt, $type:ty ) => {
			#[doc=$doc]
			pub fn $name(&self, field: &str) -> OverResult<$type> {
				match self.get(field) {
					Some(value) => {
						match value.$name() {
							Ok(result) => Ok(result),
							e @ Err(_) => e,
						}
					}
					None => Err(OverError::FieldNotFound(field.into())),
				}
			}
		}
	}

	impl Obj {
		/// Returns a new `Obj` created from the given `HashMap`.
		///
		/// Returns an error if the map contains an invalid field name.
		/// A valid field name must start with an alphabetic character or '_' and subsequent characters
		/// must be alphabetic, numeric, or '_'.
		pub fn from_map(obj_map: HashMap<String, Value>) -> OverResult<Obj> {
			for field in obj_map.keys() {
				if !Self::is_valid_field(field) {
					return Err(OverError::InvalidFieldName((*field).clone()));
				}
			}
			let id = get_id();

			Ok(Obj {
				inner: Arc::new(ObjInner {
					map: obj_map,
					parent: None,
					id,
				}),
			})
		}

		/// Returns a new `Obj` created from the given `HashMap` with given `parent`.
		///
		/// Returns an error if the map contains an invalid field name.
		///
		/// See `from_map` for more details.
		pub fn from_map_with_parent(obj_map: HashMap<String, Value>, parent: Obj) -> OverResult<Obj> {
			for field in obj_map.keys() {
				if !Self::is_valid_field(field) {
					return Err(OverError::InvalidFieldName(field.clone()));
				}
			}
			let id = get_id();

			Ok(Obj {
				inner: Arc::new(ObjInner {
					map: obj_map,
					parent: Some(parent),
					id,
				}),
			})
		}

		/// Returns a new `Obj` created from the given `HashMap`.
		///
		/// It is faster than the safe version, `from_map`, if you know every field has a valid name.
		/// You can check ahead of time whether a field is valid with `is_valid_field`.
		///
		/// See `from_map` for more details.
		pub fn from_map_unchecked(obj_map: HashMap<String, Value>) -> Obj {
			let id = get_id();

			Obj {
				inner: Arc::new(ObjInner {
					map: obj_map,
					parent: None,
					id,
				}),
			}
		}

		/// Returns a new `Obj` created from the given `HashMap` with given `parent`.
		///
		/// It is faster than the safe version, `from_map_with_parent`, if you know every field has
		/// a valid name. You can check ahead of time whether a field is valid with `is_valid_field`.
		///
		/// See `from_map` for more details.
		pub fn from_map_with_parent_unchecked(obj_map: HashMap<String, Value>, parent: Obj) -> Obj {
			let id = get_id();

			Obj {
				inner: Arc::new(ObjInner {
					map: obj_map,
					parent: Some(parent),
					id,
				}),
			}
		}

		/// Returns the ID of this `Obj`.
		///
		/// Every `Obj` is assigned its own globally unique ID. IDs are generated incrementally,
		/// starting at 0 for the first `Obj` created.
		///
		/// # Notes
		/// The ID is ignored when testing `Obj` equality.
		pub fn id(&self) -> usize {
			self.inner.id
		}

		/// Returns a reference to the inner map of this `Obj`.
		pub fn map_ref(&self) -> &HashMap<String, Value> {
			&self.inner.map
		}

		/// Returns a new `Obj` loaded from a file.
		pub fn from_file(path: &str) -> OverResult<Obj> {
			Ok(parse::load_from_file(path)?)
		}

		/// Writes this `Obj` to given file in `.over` representation.
		///
		/// # Notes
		/// Note that the fields of the `Obj` will be output in an unpredictable order.
		/// Also note that shorthand in the original file, including variables and file includes,
		/// is not preserved when parsing the file, and will not appear when writing to another file.
		pub fn write_to_file(&self, path: &str) -> OverResult<()> {
			write_file_str(path, &self.write_str())?;
			Ok(())
		}

		/// Writes this `Obj` to a `String`.
		///
		/// # Notes
		/// See `write_to_file`.
		pub fn write_str(&self) -> String {
			self.format(false, 0)
		}

		/// Iterates over each `(String, Value)` pair in `self`, applying `f`.
		pub fn with_each<F>(&self, mut f: F)
		where
			F: FnMut(&String, &Value),
		{
			for (field, value) in &self.inner.map {
				f(field, value)
			}
		}

		/// Returns the number of fields for this `Obj` (parent fields not included).
		pub fn len(&self) -> usize {
			self.inner.map.len()
		}

		/// Returns whether this `Obj` is empty.
		pub fn is_empty(&self) -> bool {
			self.inner.map.is_empty()
		}

		/// Returns whether `self` and `other` point to the same data.
		pub fn ptr_eq(&self, other: &Self) -> bool {
			Arc::ptr_eq(&self.inner, &other.inner)
		}

		/// Returns true if this `Obj` contains `field`.
		pub fn contains(&self, field: &str) -> bool {
			self.inner.map.contains_key(field)
		}

		/// Gets the `Value` associated with `field`.
		pub fn get(&self, field: &str) -> Option<Value> {
			match self.inner.map.get(field) {
				Some(value) => Some(value.clone()),
				None => match self.inner.parent {
					Some(ref parent) => parent.get(field),
					None => None,
				},
			}
		}

		/// Gets the `Value` associated with `field` and the `Obj` where it was found (either `self` or
		/// one of its parents).
		pub fn get_with_source(&self, field: &str) -> Option<(Value, Obj)> {
			match self.inner.map.get(field) {
				Some(value) => Some((value.clone(), self.clone())),
				None => match self.inner.parent {
					Some(ref parent) => parent.get_with_source(field),
					None => None,
				},
			}
		}

		get_fn!(
			"Returns the `bool` found at `field`. \
			 Returns an error if the field was not found \
			 or if the `Value` at `field` is not `Bool`.",
			get_bool,
			bool
		);
		get_fn!(
			"Returns the `BigInt` found at `field`. \
			 Returns an error if the field was not found \
			 or if the `Value` at `field` is not `Int`.",
			get_int,
			BigInt
		);
		get_fn!(
			"Returns the `BigRational` found at `field`. \
			 Returns an error if the field was not found \
			 or if the `Value` at `field` is not `Frac`.",
			get_frac,
			BigRational
		);
		get_fn!(
			"Returns the `char` found at `field`. \
			 Returns an error if the field was not found \
			 or if the `Value` at `field` is not `Char`.",
			get_char,
			char
		);
		get_fn!(
			"Returns the `String` found at `field`. \
			 Returns an error if the field was not found \
			 or if the `Value` at `field` is not `Str`.",
			get_str,
			String
		);
		get_fn!(
			"Returns the `Arr` found at `field`. \
			 Returns an error if the field was not found \
			 or if the `Value` at `field` is not `Arr`.",
			get_arr,
			Arr
		);
		get_fn!(
			"Returns the `Tup` found at `field`. \
			 Returns an error if the field was not found \
			 or if the `Value` at `field` is not `Tup`.",
			get_tup,
			Tup
		);
		get_fn!(
			"Returns the `Obj` found at `field`. \
			 Returns an error if the field was not found \
			 or if the `Value` at `field` is not `Obj`.",
			get_obj,
			Obj
		);

		/// Returns whether this `Obj` has a parent.
		pub fn has_parent(&self) -> bool {
			self.inner.parent.is_some()
		}

		/// Returns the parent for this `Obj`.
		pub fn get_parent(&self) -> Option<Obj> {
			match self.inner.parent {
				Some(ref parent) => Some(parent.clone()),
				None => None,
			}
		}

		/// Returns true if `field` is a valid field name for an `Obj`.
		///
		/// The first character must be alphabetic or '_'. Subsequent characters are allowed to be
		/// alphabetic, digits, or '_'.
		pub fn is_valid_field(field: &str) -> bool {
			let mut first = true;

			for ch in field.chars() {
				if first {
					if !Self::is_valid_field_char(ch, true) {
						return false;
					}
					first = false;
				} else if !Self::is_valid_field_char(ch, false) {
					return false;
				}
			}

			true
		}

		/// Returns true if the given char is valid for a field, depending on whether it is the first
		/// char or not.
		///
		/// See `is_valid_field` for more details.
		pub fn is_valid_field_char(ch: char, first: bool) -> bool {
			match ch {
				ch if ch.is_alphabetic() => true,
				ch if is_digit(ch) => !first,
				'_' => true,
				'^' => first,
				_ => false,
			}
		}

		/// An iterator visiting all fields (keys) in arbitrary order.
		pub fn keys(&self) -> Keys<String, Value> {
			self.map_ref().keys()
		}

		/// An iterator visiting all values in arbitrary order.
		pub fn values(&self) -> Values<String, Value> {
			self.map_ref().values()
		}

		/// An iterator visiting all field-value pairs in arbitrary order.
		pub fn iter(&self) -> Iter<String, Value> {
			self.map_ref().iter()
		}
	}

	impl Default for Obj
    {
		fn default() -> Self {
			Self::from_map_unchecked(map! {})
		}
	}

	impl fmt::Display for Obj
    {
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			write!(f, "{}", self.format(true, INDENT_STEP))
		}
	}

	impl FromStr for Obj {
		type Err = OverError;

		fn from_str(s: &str) -> Result<Self, Self::Err> {
			Ok(parse::load_from_str(s)?)
		}
	}

	/// For two Objs to be equal, the following two checks must pass:
	/// 1. If either Obj has a parent, then both must have parents and the parents must be equal.
	/// 2. The two Objs must have all the same fields pointing to the same values.
	impl PartialEq for Obj
    {
		fn eq(&self, other: &Self) -> bool {
			let inner = &self.inner;
			let other_inner = &other.inner;

			// Check parent equality.
			if inner.parent.is_some() && other_inner.parent.is_some() {
				let parent = self.get_parent().unwrap();
				let other_parent = other.get_parent().unwrap();
				if parent != other_parent {
					return false;
				}
			} else if !(inner.parent.is_none() && other_inner.parent.is_none()) {
				return false;
			}

			// Check HashMap equality.
			inner.map == other_inner.map
		}
	}

} pub use crate::obj::Obj;

pub mod ops
{
	pub use std::ops::{ * };
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

pub mod parse
{
	/*!
	*/
	use ::
	{
		*,
	};
	/*
	*/
	pub mod over
	{
		/*!
		*/
		use ::
		{
			*,
		};
		/*
        use ::
        {
            
            arr::{self, Arr},
            collections::{ HashMap, HashSet, VecDeque },
            num::
            {
                bigint::BigInt,
                rational::BigRational,
                traits::{ToPrimitive, Zero},
            },
            obj::Obj,
            ops::{ Deref },
            path::{ Path },
            tup::Tup,
            types::Type,
            value::Value,
            *,
        };
        /*				
        use super::util::*;
        */
        use super::characters::CharStream;
        use super::error::ParseErrorKind::*;
        use super::error::{parse_err, ParseError};
        use super::{ParseResult, MAX_DEPTH};
		*/
        pub const MAX_DEPTH: usize = 64;
        pub type ParseResult<T> = Result<T, ParseError>;
        
        pub mod characters
        {
            /*!
            Character stream used for parsing. */
            use ::
            {
                *,
            };
            /*
            use std::cell::RefCell;
            use std::fs::File;
            use std::io;
            use std::io::Read;
            use std::iter::Peekable;
            use std::mem;
            use std::rc::Rc;
            use std::str::Chars; */
            #[derive(Clone, Debug)]
            struct Inner
            {
                file: Option<String>,
                contents: String,
                stream: Peekable<Chars<'static>>,
                line: usize,
                col: usize,
            }

            #[derive(Clone, Debug)]
            pub struct CharStream
            {
                inner: Rc<RefCell<Inner>>,
            }

            impl CharStream
            {
                pub fn from_file(path: &str) -> io::Result<CharStream> {
                    let mut file = File::open(path)?;

                    let len = file.metadata()?.len();
                    let mut contents = String::with_capacity(len as usize);

                    file.read_to_string(&mut contents)?;

                    Self::from_string_impl(Some(String::from(path)), contents)
                }

                pub fn from_string(contents: String) -> io::Result<CharStream> {
                    Self::from_string_impl(None, contents)
                }

                fn from_string_impl(file: Option<String>, contents: String) -> io::Result<CharStream> {
                    let chars: Chars = unsafe { mem::transmute(contents.chars()) };
                    let stream = chars.peekable();

                    Ok(CharStream {
                        inner: Rc::new(RefCell::new(Inner {
                            file,
                            contents,
                            stream,
                            line: 1,
                            col: 1,
                        })),
                    })
                }

                pub fn peek(&self) -> Option<char> {
                    let mut inner = self.inner.borrow_mut();
                    let opt = inner.stream.peek();

                    match opt {
                        Some(ch) => Some(*ch),
                        None => None,
                    }
                }

                pub fn file(&self) -> Option<String> {
                    let inner = self.inner.borrow();
                    inner.file.clone()
                }

                pub fn line(&self) -> usize {
                    let inner = self.inner.borrow();
                    inner.line
                }

                pub fn col(&self) -> usize {
                    let inner = self.inner.borrow();
                    inner.col
                }

                fn set_line(&mut self, value: usize) {
                    let mut inner = self.inner.borrow_mut();
                    inner.line = value;
                }

                fn set_col(&mut self, value: usize) {
                    let mut inner = self.inner.borrow_mut();
                    inner.col = value;
                }
            }

            impl Iterator for CharStream
            {
                type Item = char;

                fn next(&mut self) -> Option<Self::Item> {
                    let opt = {
                        let mut inner = self.inner.borrow_mut();
                        inner.stream.next()
                    };

                    match opt {
                        Some(ch) => {
                            if ch == '\n' {
                                let line = self.line();
                                self.set_line(line + 1);
                                self.set_col(1);
                            } else {
                                let col = self.col();
                                self.set_col(col + 1);
                            }
                            Some(ch)
                        }
                        None => None,
                    }
                }
            }

            pub fn format_char(ch: char) -> String
            {
                match ch {
                    '\n' => String::from("\\n"),
                    ch => format!("{}", ch),
                }
            }
        }
        
        pub mod error
        {
            /*!
            Module for parse errors. */
            use ::
            {
                error::{ * },
                num::{ big::{ * }, ParseIntError },
                types::{ * },
                *,
            };
            /*
            use crate::OverError;
            */				
            use super::misc::format_char;
            use super::ParseResult;
            use super::MAX_DEPTH;
            
            pub fn parse_err<T>(file: Option<String>, kind: ParseErrorKind) -> ParseResult<T>
            {
                Err(ParseError { file, kind })
            }
            /// Error kind.
            #[derive(Debug)]
            pub enum ParseErrorKind
            {
                BinaryOperatorError(Type, Type, char, usize, usize),
                CyclicInclude(String, usize, usize),
                DuplicateField(String, usize, usize),
                DuplicateGlobal(String, usize, usize),
                ExpectedType(Type, Type, usize, usize),
                GlobalNotFound(String, usize, usize),
                InvalidIndex(BigInt, usize, usize),
                InvalidClosingBracket(Option<char>, char, usize, usize),
                InvalidDot(Type, usize, usize),
                InvalidEscapeChar(char, usize, usize),
                InvalidFieldChar(char, usize, usize),
                InvalidFieldName(String, usize, usize),
                InvalidIncludeChar(char, usize, usize),
                InvalidIncludePath(String, usize, usize),
                InvalidIncludeToken(Type, usize, usize),
                InvalidNumeric(usize, usize),
                InvalidValue(String, usize, usize),
                InvalidValueChar(char, usize, usize),
                MaxDepth(usize, usize),
                UnaryOperatorError(Type, char, usize, usize),
                UnexpectedEnd(usize),
                VariableNotFound(String, usize, usize),
                IoError(String),
                OverError(String),
                ParseIntError(String),
            }
            /// Parse error.
            #[derive(Debug)]
            pub struct ParseError
            {
                /// The file this error occurred in.
                pub file: Option<String>,
                /// Error kind.
                pub kind: ParseErrorKind,
            }

            impl fmt::Display for ParseError
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    use self::ParseErrorKind::*;

                    if let Some(ref file) = (*self).file {
                        write!(f, "{}: ", file)?;
                    }

                    match (*self).kind {
                        BinaryOperatorError(ref expected, ref found, ref op, ref line, ref col) => write!(
                            f,
                            "Could not apply operator {} on types {} and {} at line {}, column {}",
                            op, expected, found, line, col,
                        ),
                        CyclicInclude(ref file, ref line, ref col) => write!(
                            f,
                            "Tried to cyclically include file \"{}\" at line {}, column {}",
                            file, line, col
                        ),
                        DuplicateField(ref field, ref line, ref col) => write!(
                            f,
                            "Duplicate field \"{}\" at line {}, column {}",
                            field, line, col
                        ),
                        DuplicateGlobal(ref field, ref line, ref col) => write!(
                            f,
                            "Duplicate global \"{}\" at line {}, column {}",
                            field, line, col
                        ),
                        ExpectedType(ref expected, ref found, ref line, ref col) => write!(
                            f,
                            "Expected {} at line {}, column {}; found {}",
                            expected, line, col, found
                        ),
                        GlobalNotFound(ref var, ref line, ref col) => write!(
                            f,
                            "Global \"{}\" at line {}, column {} could not be found",
                            var, line, col
                        ),
                        InvalidClosingBracket(ref expected, ref found, ref line, ref col) => write!(
                            f,
                            "Invalid closing bracket '{}' at line {}, column {}; expected {}",
                            found,
                            line,
                            col,
                            match *expected {
                                Some(ch) => format!("'{}'", ch),
                                None => String::from("none"),
                            }
                        ),
                        InvalidDot(ref t, ref line, ref col) => write!(
                            f,
                            "Invalid use of dot notation on value of type {} at line {}, column {}; \
                                value must be an Obj, Arr, or Tup.",
                            t, line, col
                        ),
                        InvalidEscapeChar(ref ch, ref line, ref col) => write!(
                            f,
                            "Invalid escape character '\\{}' at line {}, column {}. \
                                If you meant to write a backslash, use '\\\\'",
                            format_char(*ch),
                            line,
                            col
                        ),
                        InvalidFieldChar(ref ch, ref line, ref col) => write!(
                            f,
                            "Invalid character '{}' for field at line {}, column {}",
                            format_char(*ch),
                            line,
                            col
                        ),
                        InvalidFieldName(ref field, ref line, ref col) => write!(
                            f,
                            "Invalid field name \"{}\" at line {}, column {}",
                            field, line, col
                        ),
                        InvalidIncludeChar(ref found, ref line, ref col) => write!(
                            f,
                            "Invalid include token character \'{}\' at line {}, column {}",
                            found, line, col
                        ),
                        InvalidIncludePath(ref path, ref line, ref col) => write!(
                            f,
                            "Invalid include path \"{}\" at line {}, column {}",
                            path, line, col
                        ),
                        InvalidIncludeToken(ref t, ref line, ref col) => write!(
                            f,
                            "Invalid value of type \"{}\" at line {}, column {}; \
                                must be either a Str value or one of the tokens \
                                \"Obj\", \"Arr\", \"Tup\", or \"Str\"",
                            t, line, col
                        ),
                        InvalidIndex(ref index, ref line, ref col) => write!(
                            f,
                            "Invalid index {} at line {}, column {}",
                            index, line, col
                        ),
                        InvalidNumeric(ref line, ref col) => {
                            write!(f, "Invalid numeric value at line {}, column {}", line, col)
                        }
                        InvalidValue(ref value, ref line, ref col) => write!(
                            f,
                            "Invalid value \"{}\" at line {}, column {}",
                            value, line, col
                        ),
                        InvalidValueChar(ref ch, ref line, ref col) => write!(
                            f,
                            "Invalid character '{}' for value at line {}, column {}",
                            format_char(*ch),
                            line,
                            col
                        ),
                        MaxDepth(ref line, ref col) => write!(
                            f,
                            "Exceeded maximum recursion depth ({}) at line {}, column {}",
                            MAX_DEPTH, line, col
                        ),
                        UnaryOperatorError(ref found, ref op, ref line, ref col) => write!(
                            f,
                            "Could not apply operator {} on type {} at line {}, column {}",
                            op, found, line, col,
                        ),
                        UnexpectedEnd(ref line) => write!(f, "Unexpected end at line {}", line,),
                        VariableNotFound(ref var, ref line, ref col) => write!(
                            f,
                            "Variable \"{}\" at line {}, column {} could not be found",
                            var, line, col
                        ),

                        IoError(ref error) | OverError(ref error) | ParseIntError(ref error) => {
                            write!(f, "{}", error)
                        }
                    }
                }
            }

            impl Error for ParseError
            {
                fn description(&self) -> &str
                {
                    use self::ParseErrorKind::*;

                    match (*self).kind {
                        BinaryOperatorError(_, _, _, _, _) | UnaryOperatorError(_, _, _, _) => {
                            "Could not apply operator"
                        }

                        CyclicInclude(_, _, _) => "Tried to cyclically include file",
                        DuplicateField(_, _, _) => "Duplicate field",
                        DuplicateGlobal(_, _, _) => "Duplicate global",
                        ExpectedType(_, _, _, _) => "Expected different type",
                        GlobalNotFound(_, _, _) => "Global could not be found",
                        InvalidClosingBracket(_, _, _, _) => "Invalid closing bracket",
                        InvalidDot(_, _, _) => "Invalid use of dot notation",
                        InvalidEscapeChar(_, _, _) => "Invalid escape character",
                        InvalidFieldChar(_, _, _) => "Invalid character for field",
                        InvalidFieldName(_, _, _) => "Invalid field name",
                        InvalidIncludeChar(_, _, _) => "Invalid include character",
                        InvalidIncludePath(_, _, _) => "Invalid include path",
                        InvalidIncludeToken(_, _, _) => "Invalid include token",
                        InvalidIndex(_, _, _) => "Invalid index",
                        InvalidNumeric(_, _) => "Invalid numeric value",
                        InvalidValue(_, _, _) => "Invalid value",
                        InvalidValueChar(_, _, _) => "Invalid character for value",
                        MaxDepth(_, _) => "Exceeded maximum depth for a container",
                        UnexpectedEnd(_) => "Unexpected end when reading value",
                        VariableNotFound(_, _, _) => "Variable could not be found",

                        IoError(ref error) | OverError(ref error) | ParseIntError(ref error) => error,
                    }
                }
            }

            impl ParseError
            {
                /// Convert an `OverError` to a `ParseError` given line and column numbers.
                pub fn from_over(e: &OverError, file: Option<String>, line: usize, col: usize) -> Self
                {
                    ParseError {
                        file,
                        kind: ParseErrorKind::OverError(format!("{} at line {}, col {}", e, line, col)),
                    }
                }
            }

            impl From<io::Error> for ParseError
            {
                fn from(e: io::Error) -> Self {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::IoError(format!("{}", e)),
                    }
                }
            }

            impl From<ParseIntError> for ParseError
            {
                fn from(e: ParseIntError) -> Self {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::ParseIntError(format!("{}", e)),
                    }
                }
            }

            impl From<ParseBigIntError> for ParseError
            {
                fn from(e: ParseBigIntError) -> Self
                {
                    ParseError {
                        file: None,
                        kind: ParseErrorKind::ParseIntError(format!("{}", e)),
                    }
                }
            }
            
        } use self::error::ParseError;

        type ObjMap = HashMap<String, Value>;
        type GlobalMap = HashMap<String, Value>;
        type IncludedMap = (HashMap<String, Value>, HashSet<String>);

        lazy_static!
        {
            static ref OBJ_SENTINEL: Obj = Obj::from_map_unchecked(HashMap::new());
            static ref STR_SENTINEL: Obj = Obj::from_map_unchecked(HashMap::new());
            static ref ARR_SENTINEL: Obj = Obj::from_map_unchecked(HashMap::new());
            static ref TUP_SENTINEL: Obj = Obj::from_map_unchecked(HashMap::new());
        }
        /// Parses given file as an `Obj`.
        pub fn parse_obj_file(path: &str) -> ParseResult<Obj> {
            let stream = CharStream::from_file(path)?;
            parse_obj_stream(stream, &mut (HashMap::new(), HashSet::new()))
        }

        // Parses given file as an `Obj`, keeping track of already encountered includes.
        fn parse_obj_file_includes(path: &str, included: &mut IncludedMap) -> ParseResult<Obj> {
            let stream = CharStream::from_file(path)?;
            parse_obj_stream(stream, included)
        }
        /// Parses given &str as an `Obj`.
        pub fn parse_obj_str(contents: &str) -> ParseResult<Obj> {
            let contents = String::from(contents);
            let stream = CharStream::from_string(contents)?;
            parse_obj_stream(stream, &mut (HashMap::new(), HashSet::new()))
        }

        // Parses an Obj given a character stream.
        #[inline] fn parse_obj_stream(mut stream: CharStream, mut included: &mut IncludedMap) -> ParseResult<Obj> {
            let mut obj: ObjMap = HashMap::new();

            // Go to the first non-whitespace character, or return if there is none.
            if !find_char(stream.clone()) {
                return Ok(Obj::from_map_unchecked(obj));
            }

            let mut globals: GlobalMap = HashMap::new();
            let mut parent = None;

            // Parse all field/value pairs for this Obj.
            while parse_field_value_pair(
                &mut stream,
                &mut obj,
                &mut globals,
                &mut included,
                &mut parent,
                1,
                None,
            )? {}

            Ok(match parent {
                Some(parent) => Obj::from_map_with_parent_unchecked(obj, parent),
                None => Obj::from_map_unchecked(obj),
            })
        }

        // Parses a sub-Obj in a file. It *must* start with { and end with }.
        fn parse_obj(
            mut stream: &mut CharStream,
            globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
        ) -> ParseResult<Value> {
            // Check depth.
            if depth > MAX_DEPTH {
                return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
            }

            // We must already be at a '{'.
            let ch = stream.next().unwrap();
            assert_eq!(ch, '{');

            // Go to the first non-whitespace character, or error if there is none.
            if !find_char(stream.clone()) {
                return parse_err(stream.file(), UnexpectedEnd(stream.line()));
            }

            let mut obj: ObjMap = HashMap::new();
            let mut parent = None;

            // Parse field/value pairs.
            while parse_field_value_pair(
                &mut stream,
                &mut obj,
                globals,
                &mut included,
                &mut parent,
                depth,
                Some('}'),
            )? {}

            let obj = match parent {
                Some(parent) => Obj::from_map_with_parent_unchecked(obj, parent),
                None => Obj::from_map_unchecked(obj),
            };
            Ok(obj.into())
        }

        // Parses a field/value pair.
        #[inline] fn parse_field_value_pair(
            mut stream: &mut CharStream,
            obj: &mut ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            parent: &mut Option<Obj>,
            depth: usize,
            cur_brace: Option<char>,
        ) -> ParseResult<bool> {
            // Check if we're at an end delimiter instead of a field.
            let peek = stream.peek().unwrap();
            if peek == '}' && cur_brace.is_some() {
                let _ = stream.next();
                return Ok(false);
            } else if is_end_delimiter(peek) {
                return parse_err(
                    stream.file(),
                    InvalidClosingBracket(cur_brace, peek, stream.line(), stream.col()),
                );
            }

            // Get the field line/col.
            let (field_line, field_col) = (stream.line(), stream.col());

            // Parse field.
            let (field, is_global, is_parent) = parse_field(stream.clone(), field_line, field_col)?;

            if !is_global && !is_parent && obj.contains_key(&field) {
                return parse_err(stream.file(), DuplicateField(field, field_line, field_col));
            } else if is_parent && parent.is_some() {
                return parse_err(
                    stream.file(),
                    DuplicateField("^".into(), field_line, field_col),
                );
            }

            // Deal with extra whitespace between field and value.
            if !find_char(stream.clone()) {
                return parse_err(stream.file(), UnexpectedEnd(stream.line()));
            }

            // At a non-whitespace character, parse value.
            let (value_line, value_col) = (stream.line(), stream.col());
            let value = parse_value(
                &mut stream,
                obj,
                &mut globals,
                &mut included,
                value_line,
                value_col,
                depth,
                cur_brace,
                true,
            )?;

            // Add value either to the globals map or to the current Obj.
            if is_global {
                if globals.contains_key(&field) {
                    return parse_err(stream.file(), DuplicateGlobal(field, field_line, field_col));
                }
                globals.insert(field, value);
            } else if is_parent {
                let par = value
                    .get_obj()
                    .map_err(|e| ParseError::from_over(&e, stream.file(), value_line, value_col))?;
                *parent = Some(par);
            } else {
                obj.insert(field, value);
            }

            // Go to the next non-whitespace character.
            if !find_char(stream.clone()) {
                match cur_brace {
                    Some(_) => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                    None => return Ok(false),
                }
            }

            Ok(true)
        }

        // Parses an Arr given a file.
        fn parse_arr_file(path: &str, mut included: &mut IncludedMap) -> ParseResult<Arr> {
            let mut stream = CharStream::from_file(path)?;

            let obj: ObjMap = HashMap::new();
            let mut globals: GlobalMap = HashMap::new();

            let mut vec = Vec::new();
            let mut tcur = Type::Any;
            let mut has_any = true;

            loop {
                // Go to the first non-whitespace character, or error if there is none.
                if !find_char(stream.clone()) {
                    break;
                }

                // At a non-whitespace character, parse value.
                let (value_line, value_col) = (stream.line(), stream.col());
                let value = parse_value(
                    &mut stream,
                    &obj,
                    &mut globals,
                    &mut included,
                    value_line,
                    value_col,
                    1,
                    None,
                    true,
                )?;

                let tnew = value.get_type();

                if has_any {
                    match Type::most_specific(&tcur, &tnew) {
                        Some((t, any)) => {
                            tcur = t;
                            has_any = any;
                        }
                        None => {
                            return parse_err(
                                stream.file(),
                                ExpectedType(tcur, tnew, value_line, value_col),
                            );
                        }
                    }
                } else if tcur != tnew {
                    return parse_err(
                        stream.file(),
                        ExpectedType(tcur, tnew, value_line, value_col),
                    );
                }

                vec.push(value);
            }

            let arr = Arr::from_vec_unchecked(vec, tcur);

            Ok(arr)
        }

        // Parses a sub-Arr in a file. It *must* start with [ and end with ].
        fn parse_arr(
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
        ) -> ParseResult<Value> {
            // Check depth.
            if depth > MAX_DEPTH {
                return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
            }

            // We must already be at a '['.
            let ch = stream.next().unwrap();
            assert_eq!(ch, '[');

            let mut vec = Vec::new();
            let mut tcur = Type::Any;
            let mut has_any = true;

            loop {
                // Go to the first non-whitespace character, or error if there is none.
                if !find_char(stream.clone()) {
                    return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                }

                let peek = stream.peek().unwrap();
                if peek == ']' {
                    let _ = stream.next();
                    break;
                } else if is_end_delimiter(peek) {
                    return parse_err(
                        stream.file(),
                        InvalidClosingBracket(Some(']'), peek, stream.line(), stream.col()),
                    );
                }

                // At a non-whitespace character, parse value.
                let (value_line, value_col) = (stream.line(), stream.col());
                let value = parse_value(
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    value_line,
                    value_col,
                    depth,
                    Some(']'),
                    true,
                )?;

                let tnew = value.get_type();

                if has_any {
                    match Type::most_specific(&tcur, &tnew) {
                        Some((t, any)) => {
                            tcur = t;
                            has_any = any;
                        }
                        None => {
                            return parse_err(
                                stream.file(),
                                ExpectedType(tcur, tnew, value_line, value_col),
                            );
                        }
                    }
                } else if tcur != tnew {
                    return parse_err(
                        stream.file(),
                        ExpectedType(tcur, tnew, value_line, value_col),
                    );
                }

                vec.push(value);
            }

            let arr = Arr::from_vec_unchecked(vec, tcur);

            Ok(arr.into())
        }

        // Parses a Tup given a file.
        fn parse_tup_file(path: &str, mut included: &mut IncludedMap) -> ParseResult<Tup> {
            let mut stream = CharStream::from_file(path)?;

            let mut vec: Vec<Value> = Vec::new();
            let obj: ObjMap = HashMap::new();
            let mut globals: GlobalMap = HashMap::new();

            loop {
                // Go to the first non-whitespace character, or error if there is none.
                if !find_char(stream.clone()) {
                    break;
                }

                // At a non-whitespace character, parse value.
                let (value_line, value_col) = (stream.line(), stream.col());
                let value = parse_value(
                    &mut stream,
                    &obj,
                    &mut globals,
                    &mut included,
                    value_line,
                    value_col,
                    1,
                    None,
                    true,
                )?;

                vec.push(value);
            }

            Ok(vec.into())
        }

        // Parses a sub-Tup in a file. It *must* start with ( and end with ).
        fn parse_tup(
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
        ) -> ParseResult<Value> {
            // Check depth.
            if depth > MAX_DEPTH {
                return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
            }

            // We must already be at a '('.
            let ch = stream.next().unwrap();
            assert_eq!(ch, '(');

            let mut vec = Vec::new();

            loop {
                // Go to the first non-whitespace character, or error if there is none.
                if !find_char(stream.clone()) {
                    return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                }

                let peek = stream.peek().unwrap();
                if peek == ')' {
                    let _ = stream.next();
                    break;
                } else if is_end_delimiter(peek) {
                    return parse_err(
                        stream.file(),
                        InvalidClosingBracket(Some(')'), peek, stream.line(), stream.col()),
                    );
                }

                // At a non-whitespace character, parse value.
                let (value_line, value_col) = (stream.line(), stream.col());
                let value = parse_value(
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    value_line,
                    value_col,
                    depth,
                    Some(')'),
                    true,
                )?;

                vec.push(value);
            }

            let tup = Tup::from_vec(vec);

            Ok(tup.into())
        }

        // Gets the next field in the char stream.
        // Returns Option<(field_name, is_global, is_parent)>.
        fn parse_field(
            mut stream: CharStream,
            line: usize,
            col: usize,
        ) -> ParseResult<(String, bool, bool)> {
            let mut field = String::new();
            let mut first = true;
            let mut is_global = false;

            let ch = stream.peek().unwrap();
            if ch == '@' {
                let ch = stream.next().unwrap();
                is_global = true;
                field.push(ch);
            }

            while let Some(ch) = stream.next() {
                match ch {
                    ':' if !first => {
                        break;
                    }
                    ch if Obj::is_valid_field_char(ch, first) => field.push(ch),
                    ch => {
                        return parse_err(
                            stream.file(),
                            InvalidFieldChar(ch, stream.line(), stream.col() - 1),
                        );
                    }
                }

                first = false;
            }

            // Check for invalid field names.
            match field.as_str() {
                _field_str if is_reserved(_field_str) => {
                    parse_err(stream.file(), InvalidFieldName(field.clone(), line, col))
                }
                "^" => Ok((field.clone(), false, true)),
                bad if bad.starts_with('^') => {
                    parse_err(stream.file(), InvalidFieldName(field.clone(), line, col))
                }
                _ => Ok((field.clone(), is_global, false)),
            }
        }

        // Gets the next value in the char stream.
        fn parse_value(
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            line: usize,
            col: usize,
            depth: usize,
            cur_brace: Option<char>,
            is_first: bool,
        ) -> ParseResult<Value> {
            // Peek to determine what kind of value we'll be parsing.
            let res = match stream.peek().unwrap() {
                '"' => parse_str(&mut stream)?,
                '\'' => parse_char(&mut stream)?,
                '{' => parse_obj(&mut stream, &mut globals, included, depth + 1)?,
                '[' => parse_arr(&mut stream, obj, &mut globals, included, depth + 1)?,
                '(' => parse_tup(&mut stream, obj, &mut globals, included, depth + 1)?,
                '@' => parse_variable(
                    &mut stream,
                    obj,
                    globals,
                    included,
                    line,
                    col,
                    depth,
                    cur_brace,
                )?,
                '<' => parse_include(&mut stream, obj, &mut globals, &mut included, depth + 1)?,
                ch @ '+' | ch @ '-' => {
                    parse_unary_op(&mut stream, obj, globals, included, depth, cur_brace, ch)?
                }
                ch if is_numeric_char(ch) => parse_numeric(&mut stream, line, col)?,
                ch if Obj::is_valid_field_char(ch, true) => parse_variable(
                    &mut stream,
                    obj,
                    globals,
                    included,
                    line,
                    col,
                    depth,
                    cur_brace,
                )?,
                ch => {
                    return parse_err(stream.file(), InvalidValueChar(ch, line, col));
                }
            };

            // Process operations if this is the first value.
            if is_first {
                let mut val_deque: VecDeque<(Value, usize, usize)> = VecDeque::new();
                let mut op_deque: VecDeque<char> = VecDeque::new();
                val_deque.push_back((res, line, col));

                loop {
                    match stream.peek() {
                        Some(ch) if is_operator(ch) => {
                            let _ = stream.next();
                            if stream.peek().is_none() {
                                return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                            }

                            let (line2, col2) = (stream.line(), stream.col());

                            // Parse another value.
                            let val2 = parse_value(
                                &mut stream,
                                obj,
                                &mut globals,
                                &mut included,
                                line2,
                                col2,
                                depth,
                                cur_brace,
                                false,
                            )?;

                            if is_priority_operator(ch) {
                                let (val1, line1, col1) = val_deque.pop_back().unwrap();
                                let res = binary_op_on_values(stream, val1, val2, ch, line2, col2)?;
                                val_deque.push_back((res, line1, col1));
                            } else {
                                val_deque.push_back((val2, line2, col2));
                                op_deque.push_back(ch);
                            }
                        }
                        _ => break,
                    }
                }

                // Check for valid characters after the value.
                check_value_end(stream, cur_brace)?;

                let (mut val1, _, _) = val_deque.pop_front().unwrap();
                while !op_deque.is_empty() {
                    let (val2, line2, col2) = val_deque.pop_front().unwrap();
                    val1 = binary_op_on_values(
                        stream,
                        val1,
                        val2,
                        op_deque.pop_front().unwrap(),
                        line2,
                        col2,
                    )?;
                }
                Ok(val1)
            } else {
                Ok(res)
            }
        }

        fn parse_unary_op(
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
            cur_brace: Option<char>,
            ch: char,
        ) -> ParseResult<Value> {
            let _ = stream.next();
            let line = stream.line();
            let col = stream.col();

            let res = match stream.peek() {
                Some(_) => parse_value(
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    line,
                    col,
                    depth + 1,
                    cur_brace,
                    false,
                )?,
                None => return parse_err(stream.file(), UnexpectedEnd(line)),
            };
            unary_op_on_value(stream, res, ch, line, col)
        }
        
        fn parse_numeric(stream: &mut CharStream, line: usize, col: usize) -> ParseResult<Value> {
            let mut s1 = String::new();
            let mut s2 = String::new();
            let mut dec = false;
            let mut under = false;

            while let Some(ch) = stream.peek() {
                match ch {
                    ch if is_value_end_char(ch) => break,
                    ch if is_digit(ch) => {
                        if !dec {
                            s1.push(ch);
                        } else {
                            s2.push(ch);
                        }
                    }
                    '.' | ',' => {
                        if !dec {
                            dec = true;
                        } else {
                            return parse_err(
                                stream.file(),
                                InvalidValueChar(ch, stream.line(), stream.col()),
                            );
                        }
                    }
                    '_' => {
                        if !under {
                            under = true;
                        } else {
                            return parse_err(
                                stream.file(),
                                InvalidValueChar(ch, stream.line(), stream.col()),
                            );
                        }
                    }
                    _ => {
                        return parse_err(
                            stream.file(),
                            InvalidValueChar(ch, stream.line(), stream.col()),
                        );
                    }
                }

                if ch != '_' {
                    under = false;
                }

                let _ = stream.next();
            }

            if dec {
                // Parse a Frac from a number with a decimal.
                if s1.is_empty() && s2.is_empty() {
                    return parse_err(stream.file(), InvalidNumeric(line, col));
                }

                let whole: BigInt = if s1.is_empty() {
                    0u8.into()
                } else {
                    s1.parse()?
                };

                // Remove trailing zeros.
                let s2 = s2.trim_end_matches('0');

                let (decimal, dec_len): (BigInt, usize) = if s2.is_empty() {
                    (0u8.into(), 1)
                } else {
                    (s2.parse()?, s2.len())
                };

                let f = frac_from_whole_and_dec(whole, decimal, dec_len);
                Ok(f.into())
            } else {
                // Parse an Int.
                if s1.is_empty() {
                    return parse_err(stream.file(), InvalidNumeric(line, col));
                }

                let i: BigInt = s1.parse()?;
                Ok(i.into())
            }
        }
        
        fn parse_variable(
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            line: usize,
            col: usize,
            depth: usize,
            cur_brace: Option<char>,
        ) -> ParseResult<Value> {
            let mut var = String::new();
            let mut is_global = false;
            let mut dot = false;
            let mut dot_global = false;

            let ch = stream.peek().unwrap();
            if ch == '@' {
                let ch = stream.next().unwrap();
                is_global = true;
                var.push(ch);
            }

            while let Some(ch) = stream.peek() {
                match ch {
                    '.' => {
                        let _ = stream.next();
                        match stream.peek() {
                            Some('@') => dot_global = true,
                            Some(ch) if Obj::is_valid_field_char(ch, true) || is_numeric_char(ch) => (),
                            Some(ch) => {
                                return parse_err(
                                    stream.file(),
                                    InvalidValueChar(ch, stream.line(), stream.col()),
                                );
                            }
                            None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                        }

                        dot = true;
                        break;
                    }
                    ch if is_value_end_char(ch) => break,
                    ch if Obj::is_valid_field_char(ch, false) => {
                        let _ = stream.next();
                        var.push(ch);
                    }
                    ch => {
                        return parse_err(
                            stream.file(),
                            InvalidValueChar(ch, stream.line(), stream.col()),
                        );
                    }
                }
            }

            let mut value = match var.as_str() {
                "null" => Value::Null,
                "true" => Value::Bool(true),
                "false" => Value::Bool(false),

                "Obj" => Value::Obj(OBJ_SENTINEL.clone()),
                "Str" => Value::Obj(STR_SENTINEL.clone()),
                "Arr" => Value::Obj(ARR_SENTINEL.clone()),
                "Tup" => Value::Obj(TUP_SENTINEL.clone()),

                var @ "@" => return parse_err(stream.file(), InvalidValue(var.into(), line, col)),
                var if is_global => {
                    // Global variable, get value from globals map.
                    match globals.get(var) {
                        Some(value) => value.clone(),
                        None => {
                            let var = String::from(var);
                            return parse_err(stream.file(), GlobalNotFound(var, line, col));
                        }
                    }
                }
                var => {
                    // Regular variable, get value from the current Obj.
                    match obj.get(var) {
                        Some(value) => value.clone(),
                        None => {
                            let var = String::from(var);
                            return parse_err(stream.file(), VariableNotFound(var, line, col));
                        }
                    }
                }
            };

            if dot {
                value = match value {
                    Value::Arr(arr) => {
                        let (line, col) = (stream.line(), stream.col());
                        let value = parse_value(
                            &mut stream,
                            obj,
                            &mut globals,
                            &mut included,
                            line,
                            col,
                            depth + 1,
                            cur_brace,
                            false,
                        )?;

                        match value {
                            Value::Int(int) => match int.to_usize() {
                                Some(index) => arr
                                    .get(index)
                                    .map_err(|e| ParseError::from_over(&e, stream.file(), line, col))?,
                                None => return parse_err(stream.file(), InvalidIndex(int, line, col)),
                            },
                            _ => {
                                return parse_err(
                                    stream.file(),
                                    ExpectedType(Type::Int, value.get_type(), line, col),
                                );
                            }
                        }
                    }
                    Value::Tup(tup) => {
                        let (line, col) = (stream.line(), stream.col());
                        let value = parse_value(
                            &mut stream,
                            obj,
                            &mut globals,
                            &mut included,
                            line,
                            col,
                            depth + 1,
                            cur_brace,
                            false,
                        )?;

                        match value {
                            Value::Int(int) => match int.to_usize() {
                                Some(index) => tup
                                    .get(index)
                                    .map_err(|e| ParseError::from_over(&e, stream.file(), line, col))?,
                                None => return parse_err(stream.file(), InvalidIndex(int, line, col)),
                            },
                            _ => {
                                return parse_err(
                                    stream.file(),
                                    ExpectedType(Type::Int, value.get_type(), line, col),
                                );
                            }
                        }
                    }
                    Value::Obj(obj) => {
                        let (line, col) = (stream.line(), stream.col());

                        if dot_global {
                            return parse_err(stream.file(), InvalidValueChar('@', line, col));
                        }

                        parse_variable(
                            &mut stream,
                            obj.map_ref(),
                            globals,
                            included,
                            line,
                            col,
                            depth + 1,
                            cur_brace,
                        )?
                    }
                    _ => return parse_err(stream.file(), InvalidDot(value.get_type(), line, col)),
                }
            }

            Ok(value)
        }
        
        fn parse_char(stream: &mut CharStream) -> ParseResult<Value> {
            let ch = stream.next().unwrap();
            assert_eq!(ch, '\'');

            let (escape, mut ch) = match stream.next() {
                Some('\\') => (true, '\0'),
                Some(ch) if ch == '\n' || ch == '\r' || ch == '\t' => {
                    return parse_err(
                        stream.file(),
                        InvalidValueChar(ch, stream.line(), stream.col() - 1),
                    );
                }
                Some(ch) => (false, ch),
                None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
            };

            if escape {
                ch = match stream.next() {
                    Some(ch) => match get_escape_char(ch) {
                        Some(ch) => ch,
                        None => {
                            return parse_err(
                                stream.file(),
                                InvalidEscapeChar(ch, stream.line(), stream.col() - 1),
                            );
                        }
                    },
                    None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                }
            }

            match stream.next() {
                Some('\'') => (),
                Some(ch) => {
                    return parse_err(
                        stream.file(),
                        InvalidValueChar(ch, stream.line(), stream.col() - 1),
                    );
                }
                None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
            }

            Ok(ch.into())
        }

        fn parse_str_file(path: &str) -> ParseResult<String> {
            // Replace \r\n line endings with \n for consistency in internal handling.
            let s = read_file_str(path)?.replace("\r\n", "\n");

            Ok(s)
        }
        
        fn parse_str(stream: &mut CharStream) -> ParseResult<Value> {
            let ch = stream.next().unwrap();
            assert_eq!(ch, '"');

            let mut s = String::new();
            let mut escape = false;

            loop {
                match stream.next() {
                    Some(ch) => {
                        if escape {
                            match get_escape_char(ch) {
                                Some(ch) => s.push(ch),
                                None => {
                                    return parse_err(
                                        stream.file(),
                                        InvalidEscapeChar(ch, stream.line(), stream.col() - 1),
                                    );
                                }
                            }
                            escape = false;
                        } else {
                            match ch {
                                '"' => break,
                                '\\' => escape = true,
                                _ => s.push(ch),
                            }
                        }
                    }
                    None => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                }
            }

            // Replace \r\n line endings with \n for consistency in internal handling.
            let s = s.replace("\r\n", "\n");

            Ok(s.into())
        }

        fn parse_include(
            mut stream: &mut CharStream,
            obj: &ObjMap,
            mut globals: &mut GlobalMap,
            mut included: &mut IncludedMap,
            depth: usize,
        ) -> ParseResult<Value> {
            enum IncludeType {
                Obj,
                Str,
                Arr,
                Tup,
            }

            // Check depth.
            if depth > MAX_DEPTH {
                return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
            }

            let ch = stream.next().unwrap();
            assert_eq!(ch, '<');

            // Go to the next non-whitespace character, or error if there is none.
            if !find_char(stream.clone()) {
                return parse_err(stream.file(), UnexpectedEnd(stream.line()));
            }

            let (mut line, mut col) = (stream.line(), stream.col());
            let mut value = parse_value(
                &mut stream,
                obj,
                &mut globals,
                &mut included,
                line,
                col,
                depth,
                Some('>'),
                true,
            )?;

            let mut include_type = IncludeType::Obj; // Default include type if no token is present.
            let mut parse_again = true; // True if an include token was found.
            match value {
                Value::Obj(ref obj) if obj.ptr_eq(&OBJ_SENTINEL) => include_type = IncludeType::Obj,
                Value::Obj(ref obj) if obj.ptr_eq(&STR_SENTINEL) => include_type = IncludeType::Str,
                Value::Obj(ref obj) if obj.ptr_eq(&ARR_SENTINEL) => include_type = IncludeType::Arr,
                Value::Obj(ref obj) if obj.ptr_eq(&TUP_SENTINEL) => include_type = IncludeType::Tup,
                Value::Str(_) => parse_again = false,
                _ => {
                    return parse_err(
                        stream.file(),
                        InvalidIncludeToken(value.get_type(), line, col),
                    );
                }
            }

            if parse_again {
                // Go to the next non-whitespace character, or error if there is none.
                if !find_char(stream.clone()) {
                    return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                }

                line = stream.line();
                col = stream.col();
                value = parse_value(
                    &mut stream,
                    obj,
                    &mut globals,
                    &mut included,
                    line,
                    col,
                    depth,
                    Some('>'),
                    true,
                )?;
            }

            // Go to the next non-whitespace character, or error if there is none.
            if !find_char(stream.clone()) {
                return parse_err(stream.file(), UnexpectedEnd(stream.line()));
            }

            match stream.next().unwrap() {
                '>' => (),
                ch => {
                    return parse_err(
                        stream.file(),
                        InvalidClosingBracket(Some('>'), ch, stream.line(), stream.col() - 1),
                    );
                }
            }

            // Get the full path of the include file.
            let include_file = match value {
                Value::Str(s) => s,
                _ => {
                    return parse_err(
                        stream.file(),
                        ExpectedType(Type::Str, value.get_type(), line, col),
                    );
                }
            };

            let pathbuf = match stream.file().as_ref() {
                Some(file) => Path::new(file)
                    .parent()
                    .unwrap()
                    .join(Path::new(&include_file)),
                None => Path::new(&include_file).to_path_buf(),
            };
            let path = pathbuf.as_path();
            if !path.is_file() {
                return parse_err(stream.file(), InvalidIncludePath(include_file, line, col));
            }

            // Get the include file as a path relative to the current working directory.
            let path_str = match path.to_str() {
                Some(path) => path,
                None => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
            };

            // Get the include file as an absolute path.
            let path = match path.canonicalize() {
                Ok(path) => path,
                Err(_) => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
            };
            let full_path_str = match path.to_str() {
                Some(path) => path,
                None => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
            };

            // Prevent cyclic includes by temporarily storing the current file path.
            let storing = if let Some(file) = stream.file() {
                let full_file = String::from(Path::new(&file).canonicalize().unwrap().to_str().unwrap());
                included.1.insert(full_file.clone());
                Some(full_file)
            } else {
                None
            };
            if included.1.contains(full_path_str) {
                return parse_err(stream.file(), CyclicInclude(include_file, line, col));
            }

            // Get either the tracked value or parse it if it's our first time seeing the include.
            let value = if included.0.contains_key(full_path_str) {
                let value = &included.0[full_path_str];
                value.clone()
            } else {
                let value: Value = match include_type {
                    IncludeType::Obj => parse_obj_file_includes(path_str, included)?.into(),
                    IncludeType::Str => parse_str_file(path_str)?.into(),
                    IncludeType::Arr => parse_arr_file(path_str, included)?.into(),
                    IncludeType::Tup => parse_tup_file(path_str, included)?.into(),
                };
                // Use full path as included key.
                included.0.insert(full_path_str.into(), value.clone());
                value
            };

            // Remove the stored file path.
            if let Some(file) = storing {
                included.1.remove(&file);
            }

            Ok(value)
        }

        // Tries to perform a unary operation on a single value.
        fn unary_op_on_value(
            stream: &CharStream,
            val: Value,
            op: char,
            line: usize,
            col: usize,
        ) -> ParseResult<Value> {
            use crate::types::Type::*;

            let t = val.get_type();

            Ok(match op {
                '+' => match t {
                    Int | Frac => val,
                    _ => return parse_err(stream.file(), UnaryOperatorError(t, op, line, col)),
                },
                '-' => match t {
                    Int => (-val.get_int().unwrap()).into(),
                    Frac => (-val.get_frac().unwrap()).into(),
                    _ => return parse_err(stream.file(), UnaryOperatorError(t, op, line, col)),
                },
                _ => return parse_err(stream.file(), UnaryOperatorError(t, op, line, col)),
            })
        }
        
        fn binary_op_on_values(
            stream: &CharStream,
            mut val1: Value,
            mut val2: Value,
            op: char,
            line: usize,
            col: usize,
        ) -> ParseResult<Value> {
            use crate::types::Type::*;

            let (mut type1, mut type2) = (val1.get_type(), val2.get_type());

            // If one value is an Int and the other is a Frac, promote the Int.
            if type1 == Int && type2 == Frac {
                val1 = Value::Frac(BigRational::new(val1.get_int().unwrap(), 1.into()));
                type1 = Frac;
            } else if type1 == Frac && type2 == Int {
                val2 = Value::Frac(BigRational::new(val2.get_int().unwrap(), 1.into()));
                type2 = Frac;
            }

            Ok(match op {
                '+' => {
                    match type1 {
                        Int if type2 == Int => (val1.get_int().unwrap() + val2.get_int().unwrap()).into(),
                        Frac if type2 == Frac => {
                            (val1.get_frac().unwrap() + val2.get_frac().unwrap()).into()
                        }
                        Char if type2 == Char => {
                            let mut s = String::with_capacity(2);
                            s.push(val1.get_char().unwrap());
                            s.push(val2.get_char().unwrap());
                            s.into()
                        }
                        Char if type2 == Str => {
                            let str2 = val2.get_str().unwrap();
                            let mut s = String::with_capacity(1 + str2.len());
                            s.push(val1.get_char().unwrap());
                            s.push_str(&str2);
                            s.into()
                        }
                        Str if type2 == Char => {
                            let str1 = val1.get_str().unwrap();
                            let mut s = String::with_capacity(str1.len() + 1);
                            s.push_str(&str1);
                            s.push(val2.get_char().unwrap());
                            s.into()
                        }
                        Str if type2 == Str => {
                            let str1 = val1.get_str().unwrap();
                            let str2 = val2.get_str().unwrap();
                            let mut s = String::with_capacity(str1.len() + str2.len());
                            s.push_str(&str1);
                            s.push_str(&str2);
                            s.into()
                        }
                        Arr(_) => {
                            match Type::most_specific(&type1, &type2) {
                                Some((t, _)) => {
                                    let (arr1, arr2) = (val1.get_arr().unwrap(), val2.get_arr().unwrap());
                                    let (mut vec1, mut vec2) =
                                        (arr1.vec_ref().clone(), arr2.vec_ref().clone());

                                    let mut vec = Vec::with_capacity(vec1.len() + vec2.len());
                                    vec.append(&mut vec1);
                                    vec.append(&mut vec2);

                                    // Get the inner type.
                                    let arr = if let Arr(ref t) = t {
                                        // Because we know the type already, we can safely use `_unchecked`.
                                        arr::Arr::from_vec_unchecked(vec, t.deref().clone())
                                    } else {
                                        panic!("Logic error")
                                    };

                                    arr.into()
                                }
                                None => {
                                    return parse_err(
                                        stream.file(),
                                        BinaryOperatorError(type1, type2, op, line, col),
                                    );
                                }
                            }
                        }
                        _ => {
                            return parse_err(
                                stream.file(),
                                BinaryOperatorError(type1, type2, op, line, col),
                            );
                        }
                    }
                }
                '-' => match type1 {
                    Int if type2 == Int => (val1.get_int().unwrap() - val2.get_int().unwrap()).into(),
                    Frac if type2 == Frac => (val1.get_frac().unwrap() - val2.get_frac().unwrap()).into(),
                    _ => {
                        return parse_err(
                            stream.file(),
                            BinaryOperatorError(type1, type2, op, line, col),
                        );
                    }
                },
                '*' => match type1 {
                    Int if type2 == Int => (val1.get_int().unwrap() * val2.get_int().unwrap()).into(),
                    Frac if type2 == Frac => (val1.get_frac().unwrap() * val2.get_frac().unwrap()).into(),
                    _ => {
                        return parse_err(
                            stream.file(),
                            BinaryOperatorError(type1, type2, op, line, col),
                        );
                    }
                },
                '/' => match type1 {
                    Int if type2 == Int => {
                        let (int1, int2) = (val1.get_int().unwrap(), val2.get_int().unwrap());
                        if int2.is_zero() {
                            return parse_err(stream.file(), InvalidNumeric(line, col));
                        }
                        BigRational::new(int1, int2).into()
                    }
                    Frac if type2 == Frac => {
                        let (frac1, frac2) = (val1.get_frac().unwrap(), val2.get_frac().unwrap());
                        if frac2.is_zero() {
                            return parse_err(stream.file(), InvalidNumeric(line, col));
                        }
                        (frac1 / frac2).into()
                    }
                    _ => {
                        return parse_err(
                            stream.file(),
                            BinaryOperatorError(type1, type2, op, line, col),
                        );
                    }
                },
                '%' => match type1 {
                    Int if type2 == Int => {
                        let int2 = val2.get_int().unwrap();
                        if int2.is_zero() {
                            return parse_err(stream.file(), InvalidNumeric(line, col));
                        }
                        (val1.get_int().unwrap() % int2).into()
                    }
                    _ => {
                        return parse_err(
                            stream.file(),
                            BinaryOperatorError(type1, type2, op, line, col),
                        );
                    }
                },
                _ => {
                    return parse_err(
                        stream.file(),
                        BinaryOperatorError(type1, type2, op, line, col),
                    );
                }
            })
        }

        // Finds the next non-whitespace character, ignoring comments, and update stream position.
        // Returns true if such a character was found or false if we got to the end of the stream.
        fn find_char(mut stream: CharStream) -> bool {
            while let Some(ch) = stream.peek() {
                match ch {
                    '#' => {
                        // Comment found; eat the rest of the line.
                        loop {
                            let ch = stream.next();
                            if ch.is_none() {
                                return false;
                            }
                            if ch.unwrap() == '\n' {
                                break;
                            }
                        }
                    }
                    ch if ch.is_whitespace() => {
                        let _ = stream.next();
                    }
                    _ => return true,
                }
            }

            false
        }

        // Helper function to make sure values are followed by a correct end delimiter.
        fn check_value_end(stream: &CharStream, cur_brace: Option<char>) -> ParseResult<()> {
            match stream.peek() {
                Some(ch) => match ch {
                    ch if is_value_end_char(ch) => {
                        if is_end_delimiter(ch) && Some(ch) != cur_brace {
                            parse_err(
                                stream.file(),
                                InvalidClosingBracket(cur_brace, ch, stream.line(), stream.col()),
                            )
                        } else {
                            Ok(())
                        }
                    }
                    ch => parse_err(
                        stream.file(),
                        InvalidValueChar(ch, stream.line(), stream.col()),
                    ),
                },
                None => Ok(()),
            }
        }
        /// Load an `Obj` from a file.
        // pub fn load_from_file(path: &str) -> ParseResult<Obj>
        pub fn from_file(path: &str) -> ParseResult<Obj>
        {
            parser::parse_obj_file(path)
        }
        /// Load an `Obj` from a &str.
        // pub fn load_from_str(contents: &str) -> ParseResult<Obj>
        pub fn from_str(contents: &str) -> ParseResult<Obj>
        {
            parser::parse_obj_str(contents)
        }
	}
}

pub mod process
{
	pub use std::process::{ * };
}

pub mod result
{
	pub use std::result::{ * };
}

pub mod tup
{
	/*!
	Tup | A tuple container which can hold elements of different types.*/
	use ::
	{
		fmt::{ self, Format, },
		slice::{ Iter },
		sync::{ Arc },
		types::{ Type },
		value::{ Value },
		OverError, OverResult, INDENT_STEP,
		*,
	};
	/*
	*/
	#[derive(Clone, Debug)]
	struct TupInner {
		vec: Vec<Value>,
		inner_tvec: Vec<Type>,
	}

	/// `Tup` struct.
	#[derive(Clone, Debug)]
	pub struct Tup {
		inner: Arc<TupInner>,
	}

	impl Tup {
		/// Returns a new `Tup` from the given vector of `Value`s.
		pub fn from_vec(values: Vec<Value>) -> Tup {
			let tvec: Vec<Type> = values.iter().map(|val| val.get_type()).collect();

			Tup {
				inner: Arc::new(TupInner {
					vec: values,
					inner_tvec: tvec,
				}),
			}
		}

		/// Returns a reference to the inner vec of this `Tup`.
		pub fn vec_ref(&self) -> &Vec<Value> {
			&self.inner.vec
		}

		/// Iterates over each `Value` in `self`, applying `Fn` `f`.
		pub fn with_each<F>(&self, mut f: F)
		where
			F: FnMut(&Value),
		{
			for value in &self.inner.vec {
				f(value)
			}
		}

		/// Gets the value at `index`.
		/// Returns an error if `index` is out of bounds.
		pub fn get(&self, index: usize) -> OverResult<Value> {
			if index >= self.inner.vec.len() {
				Err(OverError::TupOutOfBounds(index))
			} else {
				Ok(self.inner.vec[index].clone())
			}
		}

		/// Returns the type vector of this `Tup`.
		pub fn inner_type_vec(&self) -> Vec<Type> {
			self.inner.inner_tvec.clone()
		}

		/// Returns the length of this `Tup`.
		pub fn len(&self) -> usize {
			self.inner.vec.len()
		}

		/// Returns whether this `Tup` is empty.
		pub fn is_empty(&self) -> bool {
			self.inner.vec.is_empty()
		}

		/// Returns whether `self` and `other` point to the same data.
		pub fn ptr_eq(&self, other: &Self) -> bool {
			Arc::ptr_eq(&self.inner, &other.inner)
		}

		/// Returns an iterator over the Tup.
		pub fn iter(&self) -> Iter<Value> {
			self.vec_ref().iter()
		}
	}

	impl Default for Tup
    {
		fn default() -> Self {
			Self::from_vec(vec![])
		}
	}

	impl fmt::Display for Tup
    {
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			write!(f, "{}", self.format(true, INDENT_STEP))
		}
	}

	impl From<Vec<Value>> for Tup
    {
		fn from(vec: Vec<Value>) -> Self {
			Self::from_vec(vec)
		}
	}

	impl PartialEq for Tup
    {
		fn eq(&self, other: &Self) -> bool {
			// Quickly return false if the types don't match.
			if self.inner.inner_tvec != other.inner.inner_tvec {
				return false;
			}

			self.inner.vec == other.inner.vec
		}
	}

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
	/// Enum of possible types for `Value`s.
	#[derive(Clone, Debug)]
	pub enum Type {
		/// A type used to indicate an empty Arr.
		Any,
		/// Null value.
		Null,

		/// A boolean type.
		Bool,
		/// A signed integer type.
		Int,
		/// A fractional type.
		Frac,
		/// A character type.
		Char,
		/// A string type.
		Str,

		/// An array type, containing the type of its sub-elements.
		Arr(Box<Type>),
		/// A tuple type, containing the types of its sub-elements.
		Tup(Vec<Type>),
		/// An object type.
		Obj,
	}

	impl Type {
		/// Returns true if this type is strictly the same as `other`.
		/// Usually you want to use `eq()` instead.
		pub fn is(&self, other: &Type) -> bool {
			use self::Type::*;

			match *self {
				Any => {
					if let Any = *other {
						true
					} else {
						false
					}
				}

				Null => {
					if let Null = *other {
						true
					} else {
						false
					}
				}
				Bool => {
					if let Bool = *other {
						true
					} else {
						false
					}
				}
				Int => {
					if let Int = *other {
						true
					} else {
						false
					}
				}
				Frac => {
					if let Frac = *other {
						true
					} else {
						false
					}
				}
				Char => {
					if let Char = *other {
						true
					} else {
						false
					}
				}
				Str => {
					if let Str = *other {
						true
					} else {
						false
					}
				}
				Obj => {
					if let Obj = *other {
						true
					} else {
						false
					}
				}

				Arr(ref t1) => {
					if let Arr(ref t2) = *other {
						t1.is(t2)
					} else {
						false
					}
				}

				Tup(ref tvec1) => {
					if let Tup(ref tvec2) = *other {
						if tvec1.len() != tvec2.len() {
							return false;
						}
						tvec1.iter().zip(tvec2.iter()).all(|(t1, t2)| t1.is(t2))
					} else {
						false
					}
				}
			}
		}

		/// Returns true if this `Type` contains `Any`.
		pub fn has_any(&self) -> bool {
			match *self {
				Type::Any => true,
				Type::Arr(ref t) => Self::has_any(t),
				Type::Tup(ref tvec) => tvec.iter().any(|t| Self::has_any(t)),
				_ => false,
			}
		}

		/// Returns a type with the most specificity that can be applied to the two input types as well
		/// as `true` if the returned type is not maximally specific, that is, it contains `Any`. If no
		/// single type can be applied to both input types (e.g. the types are `Str` and `Int`), returns
		/// `None`.
		///
		/// # Examples
		///
		/// ```
		/// # #[macro_use] extern crate over;
		/// # fn main() {
		///
		/// use over::types::Type;
		/// use over::types::Type::*;
		/// use over::value::Value;
		///
		/// let val1: Value = tup!(arr![], arr![2]).into();
		/// let val2: Value = tup!(arr!['c'], arr![]).into();
		///
		/// let (specific_type, has_any) =
		///     Type::most_specific(&val1.get_type(), &val2.get_type()).unwrap();
		///
		/// assert_eq!(specific_type, Tup(vec![Arr(Box::new(Char)), Arr(Box::new(Int))]));
		/// assert!(!has_any);
		///
		/// # }
		/// ```
		pub fn most_specific(type1: &Type, type2: &Type) -> Option<(Type, bool)> {
			use self::Type::*;

			if let Any = *type2 {
				return Some((type1.clone(), type1.has_any()));
			}

			match *type1 {
				Any => Some((type2.clone(), type2.has_any())),

				Arr(ref t1) => {
					if let Arr(ref t2) = *type2 {
						Self::most_specific(t1, t2).map(|(t, any)| (Arr(Box::new(t)), any))
					} else {
						None
					}
				}

				Tup(ref tvec1) => {
					if let Tup(ref tvec2) = *type2 {
						if tvec1.len() == tvec2.len() {
							let mut has_any = false;

							let tvec: Option<Vec<Type>> = tvec1
								.iter()
								.zip(tvec2.iter())
								.map(|(t1, t2)| {
									Self::most_specific(t1, t2).map(|(t, any)| {
										if !has_any && any {
											has_any = any;
										}
										t
									})
								})
								.collect();

							tvec.map(|tvec| (Tup(tvec), has_any))
						} else {
							None
						}
					} else {
						None
					}
				}

				ref t => {
					if t == type2 {
						Some((t.clone(), false))
					} else {
						None
					}
				}
			}
		}
	}

	/// Two types are considered equal if one of them is Any or they have the same variant.
	/// In the case of `Arr` and `Tup`, the inner types are recursively checked for equality.
	impl PartialEq for Type
    {
		fn eq(&self, other: &Self) -> bool {
			use self::Type::*;

			// If either is Any, always return `true`.
			if let Any = *other {
				return true;
			}

			match *self {
				Any => true,
				Arr(ref box1) => {
					if let Arr(ref box2) = *other {
						box1 == box2
					} else {
						false
					}
				}
				Tup(ref tvec1) => {
					if let Tup(ref tvec2) = *other {
						tvec1 == tvec2
					} else {
						false
					}
				}
				_ => self.is(other),
			}
		}
	}
	impl Eq for Type {}

	impl fmt::Display for Type
    {
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			use self::Type::*;

			match *self {
				Any => write!(f, "Any"),
				Null => write!(f, "Null"),
				Bool => write!(f, "Bool"),
				Int => write!(f, "Int"),
				Frac => write!(f, "Frac"),
				Char => write!(f, "Char"),
				Str => write!(f, "Str"),
				Arr(ref boxxy) => write!(f, "Arr({})", boxxy),
				Tup(ref tvec) => write!(
					f,
					"Tup({})",
					match tvec.get(0) {
						Some(t1) => tvec
							.iter()
							.skip(1)
							.fold(format!("{}", t1), |s, t| format!("{}, {}", s, t)),
						None => String::from(""),
					}
				),
				Obj => write!(f, "Obj"),
			}
		}
	}

}

pub mod value
{
	/*!
	*/
	use ::
	{
		error::{ OverError },
		fmt::{ self, Format },
		num::
		{
			big::BigInt,
			rational::BigRational,
			traits::ToPrimitive,
		},
		types::{ Type },
		OverResult, INDENT_STEP,
		*,
	};
	/*
	*/
	/// Enum of possible values and their inner types.
	#[derive(Clone, Debug, PartialEq)]
	pub enum Value {
		/// A null value.
		Null,

		// Copy values.
		/// A boolean value.
		Bool(bool),
		/// A signed integer value.
		Int(BigInt),
		/// A fractional value.
		Frac(BigRational),
		/// A character value.
		Char(char),
		/// A string value.
		Str(String),

		// Reference values.
		/// An array value.
		Arr(arr::Arr),
		/// A tuple value.
		Tup(tup::Tup),
		/// An object value.
		Obj(obj::Obj),
	}

	macro_rules! get_fn {
		( $doc:expr, $name:tt, $type:ty, $variant:ident ) => {
			#[doc=$doc]
			pub fn $name(&self) -> OverResult<$type> {
				if let Value::$variant(ref inner) = *self {
					Ok(inner.clone())
				} else {
					Err(OverError::TypeMismatch(Type::$variant, self.get_type()))
				}
			}
		}
	}

	impl Value {
		/// Returns true if this `Value` is null.
		pub fn is_null(&self) -> bool {
			if let Value::Null = *self {
				true
			} else {
				false
			}
		}

		/// Returns the `Type` of this `Value`.
		pub fn get_type(&self) -> Type {
			use self::Value::*;

			match *self {
				Null => Type::Null,
				Bool(_) => Type::Bool,
				Int(_) => Type::Int,
				Frac(_) => Type::Frac,
				Char(_) => Type::Char,
				Str(_) => Type::Str,
				Arr(ref arr) => Type::Arr(Box::new(arr.inner_type())),
				Tup(ref tup) => Type::Tup(tup.inner_type_vec()),
				Obj(_) => Type::Obj,
			}
		}

		get_fn!(
			"Returns the `bool` contained in this `Value`. \
			 Returns an error if this `Value` is not `Bool`.",
			get_bool,
			bool,
			Bool
		);
		get_fn!(
			"Returns the `BigInt` contained in this `Value`. \
			 Returns an error if this `Value` is not `Int`.",
			get_int,
			BigInt,
			Int
		);
		/// Returns the `BigRational` contained in this `Value`.
		/// Returns an error if this `Value` is not `Frac`.
		pub fn get_frac(&self) -> OverResult<BigRational> {
			match *self {
				Value::Frac(ref inner) => Ok(inner.clone()),
				Value::Int(ref inner) => Ok(frac!(inner.clone(), 1)),
				_ => Err(OverError::TypeMismatch(Type::Frac, self.get_type())),
			}
		}
		get_fn!(
			"Returns the `char` contained in this `Value`. \
			 Returns an error if this `Value` is not `Char`.",
			get_char,
			char,
			Char
		);
		get_fn!(
			"Returns the `String` contained in this `Value`. \
			 Returns an error if this `Value` is not `Str`.",
			get_str,
			String,
			Str
		);
		get_fn!(
			"Returns the `Obj` contained in this `Value`. \
			 Returns an error if this `Value` is not `Obj`.",
			get_obj,
			obj::Obj,
			Obj
		);

		/// Returns the `Arr` contained in this `Value`.
		/// Returns an error if this `Value` is not `Arr`.
		pub fn get_arr(&self) -> OverResult<arr::Arr> {
			if let Value::Arr(ref inner) = *self {
				Ok(inner.clone())
			} else {
				Err(OverError::TypeMismatch(
					Type::Arr(Box::new(Type::Any)),
					self.get_type(),
				))
			}
		}

		/// Returns the `Tup` contained in this `Value`.
		/// Returns an error if this `Value` is not `Tup`.
		pub fn get_tup(&self) -> OverResult<tup::Tup> {
			if let Value::Tup(ref inner) = *self {
				Ok(inner.clone())
			} else {
				Err(OverError::TypeMismatch(Type::Tup(vec![]), self.get_type()))
			}
		}
	}

	impl fmt::Display for Value
    {
		fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
			write!(f, "{}", self.format(true, INDENT_STEP))
		}
	}

	// impl PartialEq

	macro_rules! impl_eq {
		($valtype:ident, $type:ty) => {
			impl PartialEq<$type> for Value {
				fn eq(&self, other: &$type) -> bool {
					match *self {
						Value::$valtype(ref value) => value == other,
						_ => false,
					}
				}
			}

			impl PartialEq<Value> for $type {
				fn eq(&self, other: &Value) -> bool {
					match *other {
						Value::$valtype(ref value) => value == self,
						_ => false,
					}
				}
			}
		};
	}

	impl_eq!(Bool, bool);
	impl_eq!(Int, BigInt);
	impl_eq!(Frac, BigRational);
	impl_eq!(Char, char);
	impl_eq!(Arr, arr::Arr);
	impl_eq!(Tup, tup::Tup);
	impl_eq!(Obj, obj::Obj);

	impl<'a> PartialEq<&'a str> for Value
    {
		fn eq(&self, other: &&str) -> bool {
			match *self {
				Value::Str(ref value) => value == &other.replace("\r\n", "\n"),
				_ => false,
			}
		}
	}

	impl<'a> PartialEq<Value> for &'a str
    {
		fn eq(&self, other: &Value) -> bool {
			match *other {
				Value::Str(ref value) => value == &self.replace("\r\n", "\n"),
				_ => false,
			}
		}
	}

	impl PartialEq<String> for Value
    {
		fn eq(&self, other: &String) -> bool {
			&other.as_str() == self
		}
	}

	impl PartialEq<Value> for String
    {
		fn eq(&self, other: &Value) -> bool {
			&self.as_str() == other
		}
	}

	// PartialEq for integers

	macro_rules! impl_eq_int {
		($type:ty, $fn:tt) => {
			impl PartialEq<$type> for Value {
				fn eq(&self, other: &$type) -> bool {
					match *self {
						Value::Int(ref value) => match value.$fn() {
							Some(value) => value == *other,
							None => false,
						},
						_ => false,
					}
				}
			}

			impl PartialEq<Value> for $type {
				fn eq(&self, other: &Value) -> bool {
					match *other {
						Value::Int(ref value) => match value.$fn() {
							Some(value) => value == *self,
							None => false,
						},
						_ => false,
					}
				}
			}
		};
	}

	impl_eq_int!(usize, to_usize);
	impl_eq_int!(u8, to_u8);
	impl_eq_int!(u16, to_u16);
	impl_eq_int!(u32, to_u32);
	impl_eq_int!(u64, to_u64);
	impl_eq_int!(i8, to_i8);
	impl_eq_int!(i16, to_i16);
	impl_eq_int!(i32, to_i32);
	impl_eq_int!(i64, to_i64);

	// impl From

	macro_rules! impl_from {
		($type:ty, $fn:tt) => {
			impl From<$type> for Value {
				fn from(inner: $type) -> Self {
					Value::$fn(inner.into())
				}
			}
		};
	}

	impl_from!(bool, Bool);

	impl_from!(usize, Int);
	impl_from!(u8, Int);
	impl_from!(u16, Int);
	impl_from!(u32, Int);
	impl_from!(u64, Int);
	impl_from!(i8, Int);
	impl_from!(i16, Int);
	impl_from!(i32, Int);
	impl_from!(i64, Int);
	impl_from!(BigInt, Int);

	// This is commented because the resultant values don't pass equality checks.
	//
	// impl From<f32> for Value {
	//     fn from(inner: f32) -> Self {
	//         Value::Frac(BigRational::from_f32(inner).unwrap())
	//     }
	// }
	// impl From<f64> for Value {
	//     fn from(inner: f64) -> Self {
	//         Value::Frac(BigRational::from_f64(inner).unwrap())
	//     }
	// }
	impl_from!(BigRational, Frac);

	impl_from!(char, Char);

	impl_from!(String, Str);
	impl<'a> From<&'a str> for Value
    {
		fn from(inner: &str) -> Self {
			Value::Str(inner.into())
		}
	}

	impl_from!(arr::Arr, Arr);

	impl_from!(tup::Tup, Tup);

	impl_from!(obj::Obj, Obj);

}

pub mod slice
{
	pub use std::slice::{ * };
}

pub mod str
{
	pub use std::str::{ * };
	
	pub fn replace_all(s: &str) -> String
	{
		let mut string = String::with_capacity(s.len());

		for ch in s.chars()
		{
			if let Some(s) = get_char_map(ch) { string.push_str(s); }
			else { string.push(ch); }
		}
		
		string
	}
	/// Reads a file and returns its contents in a string.
	//pub fn read_file_str(fname: &str) -> io::Result<String>
	pub fn from_file_to_string(fname: &str) -> io::Result<String>
	{
		let mut file = File::open(fname)?;
		let mut contents = String::new();
		let _ = file.read_to_string(&mut contents)?;
		Ok(contents)
	}
}

pub mod string
{
	pub use std::string::{ * };	
	/// Returns a `String` with the given amount of spaces.
	pub fn indent(amount: usize) -> String { " ".repeat(amount) }
}

pub mod sync
{
	pub use std::sync::{ * };
	pub mod spin
	{
		/*!
		Spin-based versions of the primitives in `std::sync` and `std::lazy`. */
		use ::
		{
			sync::atomic::{ self },
			*,
		};
		/*
		*/
		pub mod barrier
		{
			/*!
			Synchronization primitive allowing multiple threads to synchronize the beginning of some computation. */
			use ::
			{
				sync::spin::{ mutex::Mutex, RelaxStrategy, Spin },
				*,
			};
			/*
			*/
			/// A primitive that synchronizes the execution of multiple threads.
			pub struct Barrier<R = Spin> {
				lock: Mutex<BarrierState, R>,
				num_threads: usize,
			}

			// The inner state of a double barrier
			struct BarrierState {
				count: usize,
				generation_id: usize,
			}

			/// A `BarrierWaitResult` is returned by [`wait`] when all threads in the [`Barrier`] have rendezvoused.
			pub struct BarrierWaitResult(bool);

			impl<R: RelaxStrategy> Barrier<R> {
				/// Blocks the current thread until all threads have rendezvoused here.
				pub fn wait(&self) -> BarrierWaitResult {
					let mut lock = self.lock.lock();
					lock.count += 1;

					if lock.count < self.num_threads {
						// not the leader
						let local_gen = lock.generation_id;

						while local_gen == lock.generation_id && lock.count < self.num_threads {
							drop(lock);
							R::relax();
							lock = self.lock.lock();
						}
						BarrierWaitResult(false)
					} else {
						lock.count = 0;
						lock.generation_id = lock.generation_id.wrapping_add(1);
						BarrierWaitResult(true)
					}
				}
			}

			impl<R> Barrier<R> {
				/// Creates a new barrier that can block a given number of threads.
				pub const fn new(n: usize) -> Self {
					Self {
						lock: Mutex::new(BarrierState {
							count: 0,
							generation_id: 0,
						}),
						num_threads: n,
					}
				}
			}

			impl BarrierWaitResult {
				/// Returns whether this thread from [`wait`] is the "leader thread".
				pub fn is_leader(&self) -> bool {
					self.0
				}
			}
		}		
		/// A primitive that synchronizes the execution of multiple threads.
		pub type Barrier = self::barrier::Barrier;
		
		pub mod lazy
		{
			/*!
			Synchronization primitives for lazy evaluation. */
			use ::
			{
				cell::{ Cell },
				ops::{ Deref },
				sync::spin::{ once::Once, RelaxStrategy, Spin },
				*,
			};
			/*
			*/
			/// A value which is initialized on the first access.
			pub struct Lazy<T, F = fn() -> T, R = Spin> {
				cell: Once<T, R>,
				init: Cell<Option<F>>,
			}

			impl<T: fmt::Debug, F, R> fmt::Debug for Lazy<T, F, R> {
				fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
					let mut d = f.debug_tuple("Lazy");
					let d = if let Some(x) = self.cell.get() {
						d.field(&x)
					} else {
						d.field(&format_args!("<uninit>"))
					};
					d.finish()
				}
			}
			
			unsafe impl<T, F: Send> Sync for Lazy<T, F> where Once<T>: Sync {}
			// auto-derived `Send` impl is OK.

			impl<T, F, R> Lazy<T, F, R> {
				/// Creates a new lazy value with the given initializing
				/// function.
				pub const fn new(f: F) -> Self {
					Self {
						cell: Once::new(),
						init: Cell::new(Some(f)),
					}
				}
				/// Retrieves a mutable pointer to the inner data.
				pub fn as_mut_ptr(&self) -> *mut T {
					self.cell.as_mut_ptr()
				}
			}

			impl<T, F: FnOnce() -> T, R: RelaxStrategy> Lazy<T, F, R> {
				/// Forces the evaluation of this lazy value and
				/// returns a reference to result. This is equivalent
				/// to the `Deref` impl, but is explicit.
				pub fn force(this: &Self) -> &T {
					this.cell.call_once(|| match this.init.take() {
						Some(f) => f(),
						None => panic!("Lazy instance has previously been poisoned"),
					})
				}
			}

			impl<T, F: FnOnce() -> T, R: RelaxStrategy> Deref for Lazy<T, F, R> {
				type Target = T;

				fn deref(&self) -> &T {
					Self::force(self)
				}
			}

			impl<T: Default, R> Default for Lazy<T, fn() -> T, R> {
				/// Creates a new lazy value using `Default` as the initializing function.
				fn default() -> Self {
					Self::new(T::default)
				}
			}

		}
		/// A value which is initialized on the first access.
		pub type Lazy<T, F = fn() -> T> = self::lazy::Lazy<T, F>;
		
		pub mod mutex
		{
			/*!
			Locks that have the same behaviour as a mutex. */
			use ::
			{
				sync::spin::{ RelaxStrategy, Spin },
				ops::{Deref, DerefMut},
				*,
			};
			/*
			*/
			pub mod spin
			{
				/*!
				A naïve spinning mutex. */
				use ::
				{
					*,
				};
				/*
				use crate::{
					atomic::{AtomicBool, Ordering},
					RelaxStrategy, Spin,
				};
				use core::{
					cell::UnsafeCell,
					fmt,
					marker::PhantomData,
					mem::ManuallyDrop,
					ops::{Deref, DerefMut},
				};
				*/
				/// A [spin lock](https://en.m.wikipedia.org/wiki/Spinlock) providing mutually exclusive access to data.
				pub struct SpinMutex<T: ?Sized, R = Spin> {
					phantom: PhantomData<R>,
					pub lock: AtomicBool,
					data: UnsafeCell<T>,
				}

				/// A guard that provides mutable data access.
				pub struct SpinMutexGuard<'a, T: ?Sized + 'a> {
					lock: &'a AtomicBool,
					data: *mut T,
				}

				// Same unsafe impls as `std::sync::Mutex`
				unsafe impl<T: ?Sized + Send, R> Sync for SpinMutex<T, R> {}
				unsafe impl<T: ?Sized + Send, R> Send for SpinMutex<T, R> {}

				unsafe impl<T: ?Sized + Sync> Sync for SpinMutexGuard<'_, T> {}
				unsafe impl<T: ?Sized + Send> Send for SpinMutexGuard<'_, T> {}

				impl<T, R> SpinMutex<T, R> {
					/// Creates a new [`SpinMutex`] wrapping the supplied data.
					#[inline(always)]
					pub const fn new(data: T) -> Self {
						SpinMutex {
							lock: AtomicBool::new(false),
							data: UnsafeCell::new(data),
							phantom: PhantomData,
						}
					}

					/// Consumes this [`SpinMutex`] and unwraps the underlying data.
					#[inline(always)]
					pub fn into_inner(self) -> T {
						// We know statically that there are no outstanding references to
						// `self` so there's no need to lock.
						let SpinMutex { data, .. } = self;
						data.into_inner()
					}

					/// Returns a mutable pointer to the underlying data.
					#[inline(always)]
					pub fn as_mut_ptr(&self) -> *mut T {
						self.data.get()
					}
				}

				impl<T: ?Sized, R: RelaxStrategy> SpinMutex<T, R> {
					/// Locks the [`SpinMutex`] and returns a guard that permits access to the inner data.
					#[inline(always)]
					pub fn lock(&self) -> SpinMutexGuard<T> {
						// Can fail to lock even if the spinlock is not locked. May be more efficient than `try_lock`
						// when called in a loop.
						loop {
							if let Some(guard) = self.try_lock_weak() {
								break guard;
							}

							while self.is_locked() {
								R::relax();
							}
						}
					}
				}

				impl<T: ?Sized, R> SpinMutex<T, R> {
					/// Returns `true` if the lock is currently held.
					#[inline(always)]
					pub fn is_locked(&self) -> bool {
						self.lock.load(Ordering::Relaxed)
					}

					/// Force unlock this [`SpinMutex`].
					#[inline(always)]
					pub unsafe fn force_unlock(&self) {
						self.lock.store(false, Ordering::Release);
					}

					/// Try to lock this [`SpinMutex`], returning a lock guard if successful.
					#[inline(always)]
					pub fn try_lock(&self) -> Option<SpinMutexGuard<T>> {
						// The reason for using a strong compare_exchange is explained here:
						// https://github.com/Amanieu/parking_lot/pull/207#issuecomment-575869107
						//
						// See also the giant comment about Ordering::Acquire in try_lock_weak below.
						if self
							.lock
							.compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
							.is_ok()
						{
							Some(SpinMutexGuard {
								lock: &self.lock,
								data: unsafe { &mut *self.data.get() },
							})
						} else {
							None
						}
					}

					/// Try to lock this [`SpinMutex`], returning a lock guard if succesful.
					#[inline(always)]
					pub fn try_lock_weak(&self) -> Option<SpinMutexGuard<T>> {
						if self
							.lock
							.compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
							.is_ok()
						{
							Some(SpinMutexGuard {
								lock: &self.lock,
								data: unsafe { &mut *self.data.get() },
							})
						} else {
							None
						}
					}

					/// Returns a mutable reference to the underlying data.
					#[inline(always)]
					pub fn get_mut(&mut self) -> &mut T {
						// We know statically that there are no other references to `self`, so
						// there's no need to lock the inner mutex.
						unsafe { &mut *self.data.get() }
					}
				}

				impl<T: ?Sized + fmt::Debug, R> fmt::Debug for SpinMutex<T, R> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						match self.try_lock() {
							Some(guard) => write!(f, "Mutex {{ data: ")
								.and_then(|()| (&*guard).fmt(f))
								.and_then(|()| write!(f, " }}")),
							None => write!(f, "Mutex {{ <locked> }}"),
						}
					}
				}

				impl<T: ?Sized + Default, R> Default for SpinMutex<T, R> {
					fn default() -> Self {
						Self::new(Default::default())
					}
				}

				impl<T, R> From<T> for SpinMutex<T, R> {
					fn from(data: T) -> Self {
						Self::new(data)
					}
				}

				impl<'a, T: ?Sized> SpinMutexGuard<'a, T> {
					/// Leak the lock guard, yielding a mutable reference to the underlying data.
					#[inline(always)]
					pub fn leak(this: Self) -> &'a mut T {
						// Use ManuallyDrop to avoid stacked-borrow invalidation
						let mut this = ManuallyDrop::new(this);
						// We know statically that only we are referencing data
						unsafe { &mut *this.data }
					}
				}

				impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for SpinMutexGuard<'a, T> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						fmt::Debug::fmt(&**self, f)
					}
				}

				impl<'a, T: ?Sized + fmt::Display> fmt::Display for SpinMutexGuard<'a, T> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						fmt::Display::fmt(&**self, f)
					}
				}

				impl<'a, T: ?Sized> Deref for SpinMutexGuard<'a, T> {
					type Target = T;
					fn deref(&self) -> &T {
						// We know statically that only we are referencing data
						unsafe { &*self.data }
					}
				}

				impl<'a, T: ?Sized> DerefMut for SpinMutexGuard<'a, T> {
					fn deref_mut(&mut self) -> &mut T {
						// We know statically that only we are referencing data
						unsafe { &mut *self.data }
					}
				}

				impl<'a, T: ?Sized> Drop for SpinMutexGuard<'a, T> {
					/// The dropping of the MutexGuard will release the lock it was created from.
					fn drop(&mut self) {
						self.lock.store(false, Ordering::Release);
					}
				}
			} pub use self::spin::{SpinMutex, SpinMutexGuard};
			
			pub mod ticket
			{
				/*!
				A ticket-based mutex. */
				use ::
				{
					*,
				};
				/*
				use crate::{
					atomic::{AtomicUsize, Ordering},
					RelaxStrategy, Spin,
				};
				use core::{
					cell::UnsafeCell,
					fmt,
					marker::PhantomData,
					ops::{Deref, DerefMut},
				};
				*/
				/// A spin-based [ticket lock](https://en.wikipedia.org/wiki/Ticket_lock) providing mutually exclusive access to data.
				pub struct TicketMutex<T: ?Sized, R = Spin> {
					phantom: PhantomData<R>,
					next_ticket: AtomicUsize,
					next_serving: AtomicUsize,
					data: UnsafeCell<T>,
				}

				/// A guard that protects some data.
				///
				/// When the guard is dropped, the next ticket will be processed.
				pub struct TicketMutexGuard<'a, T: ?Sized + 'a> {
					next_serving: &'a AtomicUsize,
					ticket: usize,
					data: &'a mut T,
				}

				unsafe impl<T: ?Sized + Send, R> Sync for TicketMutex<T, R> {}
				unsafe impl<T: ?Sized + Send, R> Send for TicketMutex<T, R> {}

				impl<T, R> TicketMutex<T, R> {
					/// Creates a new [`TicketMutex`] wrapping the supplied data.
					#[inline(always)]
					pub const fn new(data: T) -> Self {
						Self {
							phantom: PhantomData,
							next_ticket: AtomicUsize::new(0),
							next_serving: AtomicUsize::new(0),
							data: UnsafeCell::new(data),
						}
					}

					/// Consumes this [`TicketMutex`] and unwraps the underlying data.
					#[inline(always)]
					pub fn into_inner(self) -> T {
						self.data.into_inner()
					}
					/// Returns a mutable pointer to the underying data.
					#[inline(always)]
					pub fn as_mut_ptr(&self) -> *mut T {
						self.data.get()
					}
				}

				impl<T: ?Sized + fmt::Debug, R> fmt::Debug for TicketMutex<T, R> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						match self.try_lock() {
							Some(guard) => write!(f, "Mutex {{ data: ")
								.and_then(|()| (&*guard).fmt(f))
								.and_then(|()| write!(f, " }}")),
							None => write!(f, "Mutex {{ <locked> }}"),
						}
					}
				}

				impl<T: ?Sized, R: RelaxStrategy> TicketMutex<T, R> {
					/// Locks the [`TicketMutex`] and returns a guard that permits access to the inner data.
					#[inline(always)]
					pub fn lock(&self) -> TicketMutexGuard<T> {
						let ticket = self.next_ticket.fetch_add(1, Ordering::Relaxed);

						while self.next_serving.load(Ordering::Acquire) != ticket {
							R::relax();
						}

						TicketMutexGuard {
							next_serving: &self.next_serving,
							ticket,
							data: unsafe { &mut *self.data.get() },
						}
					}
				}

				impl<T: ?Sized, R> TicketMutex<T, R> {
					/// Returns `true` if the lock is currently held.
					#[inline(always)]
					pub fn is_locked(&self) -> bool {
						let ticket = self.next_ticket.load(Ordering::Relaxed);
						self.next_serving.load(Ordering::Relaxed) != ticket
					}

					/// Force unlock this [`TicketMutex`], by serving the next ticket.
					#[inline(always)]
					pub unsafe fn force_unlock(&self) {
						self.next_serving.fetch_add(1, Ordering::Release);
					}

					/// Try to lock this [`TicketMutex`], returning a lock guard if successful.
					#[inline(always)]
					pub fn try_lock(&self) -> Option<TicketMutexGuard<T>> {
						// TODO: Replace with `fetch_update` to avoid manual CAS when upgrading MSRV
						let ticket = {
							let mut prev = self.next_ticket.load(Ordering::SeqCst);
							loop {
								if self.next_serving.load(Ordering::Acquire) == prev {
									match self.next_ticket.compare_exchange_weak(
										prev,
										prev + 1,
										Ordering::SeqCst,
										Ordering::SeqCst,
									) {
										Ok(x) => break Some(x),
										Err(next_prev) => prev = next_prev,
									}
								} else {
									break None;
								}
							}
						};

						ticket.map(|ticket| TicketMutexGuard {
							next_serving: &self.next_serving,
							ticket,
							data: unsafe { &mut *self.data.get() },
						})
					}

					/// Returns a mutable reference to the underlying data.
					#[inline(always)]
					pub fn get_mut(&mut self) -> &mut T {
						unsafe { &mut *self.data.get() }
					}
				}

				impl<T: ?Sized + Default, R> Default for TicketMutex<T, R> {
					fn default() -> Self {
						Self::new(Default::default())
					}
				}

				impl<T, R> From<T> for TicketMutex<T, R> {
					fn from(data: T) -> Self {
						Self::new(data)
					}
				}

				impl<'a, T: ?Sized> TicketMutexGuard<'a, T> {
					/// Leak the lock guard, yielding a mutable reference to the underlying data.
					#[inline(always)]
					pub fn leak(this: Self) -> &'a mut T {
						let data = this.data as *mut _; // Keep it in pointer form temporarily to avoid double-aliasing
						core::mem::forget(this);
						unsafe { &mut *data }
					}
				}

				impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for TicketMutexGuard<'a, T> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						fmt::Debug::fmt(&**self, f)
					}
				}

				impl<'a, T: ?Sized + fmt::Display> fmt::Display for TicketMutexGuard<'a, T> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						fmt::Display::fmt(&**self, f)
					}
				}

				impl<'a, T: ?Sized> Deref for TicketMutexGuard<'a, T> {
					type Target = T;
					fn deref(&self) -> &T {
						self.data
					}
				}

				impl<'a, T: ?Sized> DerefMut for TicketMutexGuard<'a, T> {
					fn deref_mut(&mut self) -> &mut T {
						self.data
					}
				}

				impl<'a, T: ?Sized> Drop for TicketMutexGuard<'a, T> {
					fn drop(&mut self) {
						let new_ticket = self.ticket + 1;
						self.next_serving.store(new_ticket, Ordering::Release);
					}
				}
				
			} pub use self::ticket::{TicketMutex, TicketMutexGuard};
			
			pub mod fair
			{
				/*!
				A spinning mutex with a fairer unlock algorithm. */
				use ::
				{
					
					cell::{ UnsafeCell },
					marker::{ PhantomData },
					mem::{ ManuallyDrop },
					ops::{Deref, DerefMut},
					sync::
					{
						atomic::{AtomicUsize, Ordering},
						spin::{ RelaxStrategy, Spin },
					},
					*,
				};
				/*
				*/
				// The lowest bit of `lock` is used to indicate whether the mutex is locked or not. 
				// The rest of the bits are used to store the number of starving threads.
				const LOCKED: usize = 1;
				const STARVED: usize = 2;

				/// Number chosen by fair roll of the dice, adjust as needed.
				const STARVATION_SPINS: usize = 1024;

				/// A spin-lock providing mutually exclusive access to data, but with a fairer algorithm.
				pub struct FairMutex<T: ?Sized, R = Spin> {
					phantom: PhantomData<R>,
					pub lock: AtomicUsize,
					data: UnsafeCell<T>,
				}

				/// A guard that provides mutable data access.
				pub struct FairMutexGuard<'a, T: ?Sized + 'a> {
					lock: &'a AtomicUsize,
					data: *mut T,
				}

				/// A handle that indicates that we have been trying to acquire the lock for a while.
				pub struct Starvation<'a, T: ?Sized + 'a, R> {
					lock: &'a FairMutex<T, R>,
				}

				/// Indicates whether a lock was rejected due to the lock being held by another thread or due to starvation.
				#[derive(Debug)]
				pub enum LockRejectReason {
					/// The lock was rejected due to the lock being held by another thread.
					Locked,
					/// The lock was rejected due to starvation.
					Starved,
				}

				// Same unsafe impls as `std::sync::Mutex`
				unsafe impl<T: ?Sized + Send, R> Sync for FairMutex<T, R> {}
				unsafe impl<T: ?Sized + Send, R> Send for FairMutex<T, R> {}

				unsafe impl<T: ?Sized + Sync> Sync for FairMutexGuard<'_, T> {}
				unsafe impl<T: ?Sized + Send> Send for FairMutexGuard<'_, T> {}

				impl<T, R> FairMutex<T, R> {
					/// Creates a new [`FairMutex`] wrapping the supplied data.
					#[inline(always)]
					pub const fn new(data: T) -> Self {
						FairMutex {
							lock: AtomicUsize::new(0),
							data: UnsafeCell::new(data),
							phantom: PhantomData,
						}
					}

					/// Consumes this [`FairMutex`] and unwraps the underlying data.
					#[inline(always)]
					pub fn into_inner(self) -> T {
						// We know statically that there are no outstanding references to
						// `self` so there's no need to lock.
						let FairMutex { data, .. } = self;
						data.into_inner()
					}

					/// Returns a mutable pointer to the underlying data.
					#[inline(always)]
					pub fn as_mut_ptr(&self) -> *mut T {
						self.data.get()
					}
				}

				impl<T: ?Sized, R: RelaxStrategy> FairMutex<T, R> {
					/// Locks the [`FairMutex`] and returns a guard that permits access to the inner data.
					#[inline(always)]
					pub fn lock(&self) -> FairMutexGuard<T> {
						// Can fail to lock even if the spinlock is not locked. May be more efficient than `try_lock`
						// when called in a loop.
						let mut spins = 0;
						while self
							.lock
							.compare_exchange_weak(0, 1, Ordering::Acquire, Ordering::Relaxed)
							.is_err()
						{
							// Wait until the lock looks unlocked before retrying
							while self.is_locked() {
								R::relax();

								// If we've been spinning for a while, switch to a fairer strategy that will prevent
								// newer users from stealing our lock from us.
								if spins > STARVATION_SPINS {
									return self.starve().lock();
								}
								spins += 1;
							}
						}

						FairMutexGuard {
							lock: &self.lock,
							data: unsafe { &mut *self.data.get() },
						}
					}
				}

				impl<T: ?Sized, R> FairMutex<T, R> {
					/// Returns `true` if the lock is currently held.
					#[inline(always)]
					pub fn is_locked(&self) -> bool {
						self.lock.load(Ordering::Relaxed) & LOCKED != 0
					}

					/// Force unlock this [`FairMutex`].
					#[inline(always)]
					pub unsafe fn force_unlock(&self) {
						self.lock.fetch_and(!LOCKED, Ordering::Release);
					}

					/// Try to lock this [`FairMutex`], returning a lock guard if successful.
					#[inline(always)]
					pub fn try_lock(&self) -> Option<FairMutexGuard<T>> {
						self.try_lock_starver().ok()
					}

					/// Tries to lock this [`FairMutex`] and returns a result that indicates whether the lock was
					/// rejected due to a starver or not.
					#[inline(always)]
					pub fn try_lock_starver(&self) -> Result<FairMutexGuard<T>, LockRejectReason> {
						match self
							.lock
							.compare_exchange(0, LOCKED, Ordering::Acquire, Ordering::Relaxed)
							.unwrap_or_else(|x| x)
						{
							0 => Ok(FairMutexGuard {
								lock: &self.lock,
								data: unsafe { &mut *self.data.get() },
							}),
							LOCKED => Err(LockRejectReason::Locked),
							_ => Err(LockRejectReason::Starved),
						}
					}

					/// Indicates that the current user has been waiting for the lock for a while
					/// and that the lock should yield to this thread over a newly arriving thread.
					pub fn starve(&self) -> Starvation<'_, T, R> {
						// Add a new starver to the state.
						if self.lock.fetch_add(STARVED, Ordering::Relaxed) > (core::isize::MAX - 1) as usize {
							// In the event of a potential lock overflow, abort.
							crate::abort();
						}

						Starvation { lock: self }
					}

					/// Returns a mutable reference to the underlying data.
					#[inline(always)]
					pub fn get_mut(&mut self) -> &mut T {
						// We know statically that there are no other references to `self`, so
						// there's no need to lock the inner mutex.
						unsafe { &mut *self.data.get() }
					}
				}

				impl<T: ?Sized + fmt::Debug, R> fmt::Debug for FairMutex<T, R> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						struct LockWrapper<'a, T: ?Sized + fmt::Debug>(Option<FairMutexGuard<'a, T>>);

						impl<T: ?Sized + fmt::Debug> fmt::Debug for LockWrapper<'_, T> {
							fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
								match &self.0 {
									Some(guard) => fmt::Debug::fmt(guard, f),
									None => f.write_str("<locked>"),
								}
							}
						}

						f.debug_struct("FairMutex")
							.field("data", &LockWrapper(self.try_lock()))
							.finish()
					}
				}

				impl<T: ?Sized + Default, R> Default for FairMutex<T, R> {
					fn default() -> Self {
						Self::new(Default::default())
					}
				}

				impl<T, R> From<T> for FairMutex<T, R> {
					fn from(data: T) -> Self {
						Self::new(data)
					}
				}

				impl<'a, T: ?Sized> FairMutexGuard<'a, T> {
					/// Leak the lock guard, yielding a mutable reference to the underlying data.
					#[inline(always)]
					pub fn leak(this: Self) -> &'a mut T {
						// Use ManuallyDrop to avoid stacked-borrow invalidation
						let mut this = ManuallyDrop::new(this);
						// We know statically that only we are referencing data
						unsafe { &mut *this.data }
					}
				}

				impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for FairMutexGuard<'a, T> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						fmt::Debug::fmt(&**self, f)
					}
				}

				impl<'a, T: ?Sized + fmt::Display> fmt::Display for FairMutexGuard<'a, T> {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						fmt::Display::fmt(&**self, f)
					}
				}

				impl<'a, T: ?Sized> Deref for FairMutexGuard<'a, T> {
					type Target = T;
					fn deref(&self) -> &T {
						// We know statically that only we are referencing data
						unsafe { &*self.data }
					}
				}

				impl<'a, T: ?Sized> DerefMut for FairMutexGuard<'a, T> {
					fn deref_mut(&mut self) -> &mut T {
						// We know statically that only we are referencing data
						unsafe { &mut *self.data }
					}
				}

				impl<'a, T: ?Sized> Drop for FairMutexGuard<'a, T> {
					/// The dropping of the MutexGuard will release the lock it was created from.
					fn drop(&mut self) {
						self.lock.fetch_and(!LOCKED, Ordering::Release);
					}
				}

				impl<'a, T: ?Sized, R> Starvation<'a, T, R> {
					/// Attempts the lock the mutex if we are the only starving user.
					///
					/// This allows another user to lock the mutex if they are starving as well.
					pub fn try_lock_fair(self) -> Result<FairMutexGuard<'a, T>, Self> {
						// Try to lock the mutex.
						if self
							.lock
							.lock
							.compare_exchange(
								STARVED,
								STARVED | LOCKED,
								Ordering::Acquire,
								Ordering::Relaxed,
							)
							.is_ok()
						{
							// We are the only starving user, lock the mutex.
							Ok(FairMutexGuard {
								lock: &self.lock.lock,
								data: self.lock.data.get(),
							})
						} else {
							// Another user is starving, fail.
							Err(self)
						}
					}

					/// Attempts to lock the mutex.
					pub fn try_lock(self) -> Result<FairMutexGuard<'a, T>, Self> {
						// Try to lock the mutex.
						if self.lock.lock.fetch_or(LOCKED, Ordering::Acquire) & LOCKED == 0 {
							// We have successfully locked the mutex.
							// By dropping `self` here, we decrement the starvation count.
							Ok(FairMutexGuard {
								lock: &self.lock.lock,
								data: self.lock.data.get(),
							})
						} else {
							Err(self)
						}
					}
				}

				impl<'a, T: ?Sized, R: RelaxStrategy> Starvation<'a, T, R> {
					/// Locks the mutex.
					pub fn lock(mut self) -> FairMutexGuard<'a, T> {
						// Try to lock the mutex.
						loop {
							match self.try_lock() {
								Ok(lock) => return lock,
								Err(starve) => self = starve,
							}

							// Relax until the lock is released.
							while self.lock.is_locked() {
								R::relax();
							}
						}
					}
				}

				impl<'a, T: ?Sized, R> Drop for Starvation<'a, T, R> {
					fn drop(&mut self) {
						// As there is no longer a user being starved, we decrement the starver count.
						self.lock.lock.fetch_sub(STARVED, Ordering::Release);
					}
				}

				impl fmt::Display for LockRejectReason {
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						match self {
							LockRejectReason::Locked => write!(f, "locked"),
							LockRejectReason::Starved => write!(f, "starved"),
						}
					}
				}
				
				impl ::error::Error for LockRejectReason {}
				
			} pub use self::fair::{FairMutex, FairMutexGuard, Starvation};
			
			type InnerMutex<T, R> = self::spin::SpinMutex<T, R>;
			type InnerMutexGuard<'a, T> = self::spin::SpinMutexGuard<'a, T>;
			/*
			#[cfg(feature = "use_ticket_mutex")]
			{
				type InnerMutex<T, R> = self::ticket::TicketMutex<T, R>;
				type InnerMutexGuard<'a, T> = self::ticket::TicketMutexGuard<'a, T>;
			} */
			/// A spin-based lock providing mutually exclusive access to data.
			pub struct Mutex<T: ?Sized, R = Spin> {
				inner: InnerMutex<T, R>,
			}

			unsafe impl<T: ?Sized + Send, R> Sync for Mutex<T, R> {}
			unsafe impl<T: ?Sized + Send, R> Send for Mutex<T, R> {}

			/// A generic guard that will protect some data access and
			/// uses either a ticket lock or a normal spin mutex.
			pub struct MutexGuard<'a, T: 'a + ?Sized> {
				inner: InnerMutexGuard<'a, T>,
			}

			impl<T, R> Mutex<T, R> {
				/// Creates a new [`Mutex`] wrapping the supplied data.
				#[inline(always)]
				pub const fn new(value: T) -> Self {
					Self {
						inner: InnerMutex::new(value),
					}
				}

				/// Consumes this [`Mutex`] and unwraps the underlying data.
				#[inline(always)]
				pub fn into_inner(self) -> T {
					self.inner.into_inner()
				}
			}

			impl<T: ?Sized, R: RelaxStrategy> Mutex<T, R> {
				/// Locks the [`Mutex`] and returns a guard that permits access to the inner data.
				#[inline(always)]
				pub fn lock(&self) -> MutexGuard<T> {
					MutexGuard {
						inner: self.inner.lock(),
					}
				}
			}

			impl<T: ?Sized, R> Mutex<T, R> {
				/// Returns `true` if the lock is currently held.
				#[inline(always)]
				pub fn is_locked(&self) -> bool {
					self.inner.is_locked()
				}

				/// Force unlock this [`Mutex`].
				#[inline(always)]
				pub unsafe fn force_unlock(&self) {
					self.inner.force_unlock()
				}

				/// Try to lock this [`Mutex`], returning a lock guard if successful.
				#[inline(always)]
				pub fn try_lock(&self) -> Option<MutexGuard<T>> {
					self.inner
						.try_lock()
						.map(|guard| MutexGuard { inner: guard })
				}

				/// Returns a mutable reference to the underlying data.
				#[inline(always)]
				pub fn get_mut(&mut self) -> &mut T {
					self.inner.get_mut()
				}
			}

			impl<T: ?Sized + fmt::Debug, R> fmt::Debug for Mutex<T, R> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Debug::fmt(&self.inner, f)
				}
			}

			impl<T: ?Sized + Default, R> Default for Mutex<T, R> {
				fn default() -> Self {
					Self::new(Default::default())
				}
			}

			impl<T, R> From<T> for Mutex<T, R> {
				fn from(data: T) -> Self {
					Self::new(data)
				}
			}

			impl<'a, T: ?Sized> MutexGuard<'a, T> {
				/// Leak the lock guard, yielding a mutable reference to the underlying data.
				#[inline(always)]
				pub fn leak(this: Self) -> &'a mut T {
					InnerMutexGuard::leak(this.inner)
				}
			}

			impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for MutexGuard<'a, T> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Debug::fmt(&**self, f)
				}
			}

			impl<'a, T: ?Sized + fmt::Display> fmt::Display for MutexGuard<'a, T> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Display::fmt(&**self, f)
				}
			}

			impl<'a, T: ?Sized> Deref for MutexGuard<'a, T> {
				type Target = T;
				fn deref(&self) -> &T {
					&*self.inner
				}
			}

			impl<'a, T: ?Sized> DerefMut for MutexGuard<'a, T> {
				fn deref_mut(&mut self) -> &mut T {
					&mut *self.inner
				}
			}
		} 
		/// A primitive that synchronizes the execution of multiple threads.
		pub type Mutex<T> = self::mutex::Mutex<T>;

		pub mod once
		{
			/*!
			*/
			use ::
			{
				cell::{ UnsafeCell },
				marker::{ PhantomData },
				mem::{ PhantomData },
				sync::
				{
					atomic::{AtomicU8, Ordering},
					spin::{ RelaxStrategy, Spin, },
				},
				*,
			};
			/*
			*/
			/// A primitive that provides lazy one-time initialization.
			pub struct Once<T = (), R = Spin> {
				phantom: PhantomData<R>,
				status: AtomicStatus,
				data: UnsafeCell<MaybeUninit<T>>,
			}

			impl<T, R> Default for Once<T, R> {
				fn default() -> Self {
					Self::new()
				}
			}

			impl<T: fmt::Debug, R> fmt::Debug for Once<T, R> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					let mut d = f.debug_tuple("Once");
					let d = if let Some(x) = self.get() {
						d.field(&x)
					} else {
						d.field(&format_args!("<uninit>"))
					};
					d.finish()
				}
			}

			// Same unsafe impls as `std::sync::RwLock`, because this also allows for
			// concurrent reads.
			unsafe impl<T: Send + Sync, R> Sync for Once<T, R> {}
			unsafe impl<T: Send, R> Send for Once<T, R> {}

			mod status {
				use super::*;

				// SAFETY: This structure has an invariant, namely that the inner atomic u8 must *always* have
				// a value for which there exists a valid Status. This means that users of this API must only
				// be allowed to load and store `Status`es.
				#[repr(transparent)]
				pub struct AtomicStatus(AtomicU8);

				// Four states that a Once can be in, encoded into the lower bits of `status` in
				// the Once structure.
				#[repr(u8)]
				#[derive(Clone, Copy, Debug, PartialEq)]
				pub enum Status {
					Incomplete = 0x00,
					Running = 0x01,
					Complete = 0x02,
					Panicked = 0x03,
				}
				impl Status {
					// Construct a status from an inner u8 integer.
					//
					// # Safety
					//
					// For this to be safe, the inner number must have a valid corresponding enum variant.
					unsafe fn new_unchecked(inner: u8) -> Self {
						core::mem::transmute(inner)
					}
				}

				impl AtomicStatus {
					#[inline(always)]
					pub const fn new(status: Status) -> Self {
						// SAFETY: We got the value directly from status, so transmuting back is fine.
						Self(AtomicU8::new(status as u8))
					}
					#[inline(always)]
					pub fn load(&self, ordering: Ordering) -> Status {
						// SAFETY: We know that the inner integer must have been constructed from a Status in
						// the first place.
						unsafe { Status::new_unchecked(self.0.load(ordering)) }
					}
					#[inline(always)]
					pub fn store(&self, status: Status, ordering: Ordering) {
						// SAFETY: While not directly unsafe, this is safe because the value was retrieved from
						// a status, thus making transmutation safe.
						self.0.store(status as u8, ordering);
					}
					#[inline(always)]
					pub fn compare_exchange(
						&self,
						old: Status,
						new: Status,
						success: Ordering,
						failure: Ordering,
					) -> Result<Status, Status> {
						match self
							.0
							.compare_exchange(old as u8, new as u8, success, failure)
						{
							// SAFETY: A compare exchange will always return a value that was later stored into
							// the atomic u8, but due to the invariant that it must be a valid Status, we know
							// that both Ok(_) and Err(_) will be safely transmutable.
							Ok(ok) => Ok(unsafe { Status::new_unchecked(ok) }),
							Err(err) => Err(unsafe { Status::new_unchecked(err) }),
						}
					}
					#[inline(always)]
					pub fn get_mut(&mut self) -> &mut Status {
						// SAFETY: Since we know that the u8 inside must be a valid Status, we can safely cast
						// it to a &mut Status.
						unsafe { &mut *((self.0.get_mut() as *mut u8).cast::<Status>()) }
					}
				}
			}
			use self::status::{AtomicStatus, Status};

			impl<T, R: RelaxStrategy> Once<T, R> {
				/// Performs an initialization routine once and only once.
				pub fn call_once<F: FnOnce() -> T>(&self, f: F) -> &T {
					match self.try_call_once(|| Ok::<T, core::convert::Infallible>(f())) {
						Ok(x) => x,
						Err(void) => match void {},
					}
				}

				/// This method is similar to `call_once`, but allows the given closure to
				/// fail, and lets the `Once` in a uninitialized state if it does.
				pub fn try_call_once<F: FnOnce() -> Result<T, E>, E>(&self, f: F) -> Result<&T, E> {
					if let Some(value) = self.get() {
						Ok(value)
					} else {
						self.try_call_once_slow(f)
					}
				}

				#[cold]
				fn try_call_once_slow<F: FnOnce() -> Result<T, E>, E>(&self, f: F) -> Result<&T, E> {
					loop {
						let xchg = self.status.compare_exchange(
							Status::Incomplete,
							Status::Running,
							Ordering::Acquire,
							Ordering::Acquire,
						);

						match xchg {
							Ok(_must_be_state_incomplete) => {
								// Impl is defined after the match for readability
							}
							Err(Status::Panicked) => panic!("Once panicked"),
							Err(Status::Running) => match self.poll() {
								Some(v) => return Ok(v),
								None => continue,
							},
							Err(Status::Complete) => {
								return Ok(unsafe {
									// SAFETY: The status is Complete
									self.force_get()
								});
							}
							Err(Status::Incomplete) => {
								// The compare_exchange failed, so this shouldn't ever be reached,
								// however if we decide to switch to compare_exchange_weak it will
								// be safer to leave this here than hit an unreachable
								continue;
							}
						}

						// The compare-exchange succeeded, so we shall initialize it.

						// We use a guard (Finish) to catch panics caused by builder
						let finish = Finish {
							status: &self.status,
						};
						let val = match f() {
							Ok(val) => val,
							Err(err) => {
								// If an error occurs, clean up everything and leave.
								core::mem::forget(finish);
								self.status.store(Status::Incomplete, Ordering::Release);
								return Err(err);
							}
						};
						unsafe {
							// SAFETY:
							// `UnsafeCell`/deref: currently the only accessor, mutably
							// and immutably by cas exclusion.
							// `write`: pointer comes from `MaybeUninit`.
							(*self.data.get()).as_mut_ptr().write(val);
						};
						// If there were to be a panic with unwind enabled, the code would
						// short-circuit and never reach the point where it writes the inner data.
						// The destructor for Finish will run, and poison the Once to ensure that other
						// threads accessing it do not exhibit unwanted behavior, if there were to be
						// any inconsistency in data structures caused by the panicking thread.
						//
						// However, f() is expected in the general case not to panic. In that case, we
						// simply forget the guard, bypassing its destructor. We could theoretically
						// clear a flag instead, but this eliminates the call to the destructor at
						// compile time, and unconditionally poisons during an eventual panic, if
						// unwinding is enabled.
						core::mem::forget(finish);

						// SAFETY: Release is required here, so that all memory accesses done in the
						// closure when initializing, become visible to other threads that perform Acquire
						// loads.
						//
						// And, we also know that the changes this thread has done will not magically
						// disappear from our cache, so it does not need to be AcqRel.
						self.status.store(Status::Complete, Ordering::Release);

						// This next line is mainly an optimization.
						return unsafe { Ok(self.force_get()) };
					}
				}

				/// Spins until the [`Once`] contains a value.
				pub fn wait(&self) -> &T {
					loop {
						match self.poll() {
							Some(x) => break x,
							None => R::relax(),
						}
					}
				}

				/// Like [`Once::get`], but will spin if the [`Once`] is in the process of being
				/// initialized.
				pub fn poll(&self) -> Option<&T> {
					loop {
						// SAFETY: Acquire is safe here, because if the status is COMPLETE, then we want to make
						// sure that all memory accessed done while initializing that value, are visible when
						// we return a reference to the inner data after this load.
						match self.status.load(Ordering::Acquire) {
							Status::Incomplete => return None,
							Status::Running => R::relax(), // We spin
							Status::Complete => return Some(unsafe { self.force_get() }),
							Status::Panicked => panic!("Once previously poisoned by a panicked"),
						}
					}
				}
			}

			impl<T, R> Once<T, R> {
				/// Initialization constant of [`Once`].
				#[allow(clippy::declare_interior_mutable_const)]
				pub const INIT: Self = Self {
					phantom: PhantomData,
					status: AtomicStatus::new(Status::Incomplete),
					data: UnsafeCell::new(MaybeUninit::uninit()),
				};

				/// Creates a new [`Once`].
				pub const fn new() -> Self {
					Self::INIT
				}

				/// Creates a new initialized [`Once`].
				pub const fn initialized(data: T) -> Self {
					Self {
						phantom: PhantomData,
						status: AtomicStatus::new(Status::Complete),
						data: UnsafeCell::new(MaybeUninit::new(data)),
					}
				}

				/// Retrieve a pointer to the inner data.
				pub fn as_mut_ptr(&self) -> *mut T {
					// SAFETY:
					// * MaybeUninit<T> always has exactly the same layout as T
					self.data.get().cast::<T>()
				}

				/// Get a reference to the initialized instance. Must only be called once COMPLETE.
				unsafe fn force_get(&self) -> &T {
					// SAFETY:
					// * `UnsafeCell`/inner deref: data never changes again
					// * `MaybeUninit`/outer deref: data was initialized
					&*(*self.data.get()).as_ptr()
				}

				/// Get a reference to the initialized instance. Must only be called once COMPLETE.
				unsafe fn force_get_mut(&mut self) -> &mut T {
					// SAFETY:
					// * `UnsafeCell`/inner deref: data never changes again
					// * `MaybeUninit`/outer deref: data was initialized
					&mut *(*self.data.get()).as_mut_ptr()
				}

				/// Get a reference to the initialized instance. Must only be called once COMPLETE.
				unsafe fn force_into_inner(self) -> T {
					// SAFETY:
					// * `UnsafeCell`/inner deref: data never changes again
					// * `MaybeUninit`/outer deref: data was initialized
					(*self.data.get()).as_ptr().read()
				}

				/// Returns a reference to the inner value if the [`Once`] has been initialized.
				pub fn get(&self) -> Option<&T> {
					// SAFETY: Just as with `poll`, Acquire is safe here because we want to be able to see the
					// nonatomic stores done when initializing, once we have loaded and checked the status.
					match self.status.load(Ordering::Acquire) {
						Status::Complete => Some(unsafe { self.force_get() }),
						_ => None,
					}
				}

				/// Returns a reference to the inner value on the unchecked assumption that the  [`Once`] has been initialized.
				pub unsafe fn get_unchecked(&self) -> &T {
					debug_assert_eq!(
						self.status.load(Ordering::SeqCst),
						Status::Complete,
						"Attempted to access an uninitialized Once. If this was run without debug checks, this would be undefined behaviour. This is a serious bug and you must fix it.",
					);
					self.force_get()
				}

				/// Returns a mutable reference to the inner value if the [`Once`] has been initialized.
				pub fn get_mut(&mut self) -> Option<&mut T> {
					match *self.status.get_mut() {
						Status::Complete => Some(unsafe { self.force_get_mut() }),
						_ => None,
					}
				}

				/// Returns a mutable reference to the inner value
				pub unsafe fn get_mut_unchecked(&mut self) -> &mut T {
					debug_assert_eq!(
						self.status.load(Ordering::SeqCst),
						Status::Complete,
						"Attempted to access an unintialized Once.  If this was to run without debug checks, this would be undefined behavior.  This is a serious bug and you must fix it.",
					);
					self.force_get_mut()
				}

				/// Returns a the inner value if the [`Once`] has been initialized.
				pub fn try_into_inner(mut self) -> Option<T> {
					match *self.status.get_mut() {
						Status::Complete => Some(unsafe { self.force_into_inner() }),
						_ => None,
					}
				}

				/// Returns a the inner value if the [`Once`] has been initialized.
				pub unsafe fn into_inner_unchecked(self) -> T {
					debug_assert_eq!(
						self.status.load(Ordering::SeqCst),
						Status::Complete,
						"Attempted to access an unintialized Once.  If this was to run without debug checks, this would be undefined behavior.  This is a serious bug and you must fix it.",
					);
					self.force_into_inner()
				}

				/// Checks whether the value has been initialized.
				pub fn is_completed(&self) -> bool {
					// TODO: Add a similar variant for Relaxed?
					self.status.load(Ordering::Acquire) == Status::Complete
				}
			}

			impl<T, R> From<T> for Once<T, R> {
				fn from(data: T) -> Self {
					Self::initialized(data)
				}
			}

			impl<T, R> Drop for Once<T, R> {
				fn drop(&mut self) {
					// No need to do any atomic access here, we have &mut!
					if *self.status.get_mut() == Status::Complete {
						unsafe {
							//TODO: Use MaybeUninit::assume_init_drop once stabilised
							core::ptr::drop_in_place((*self.data.get()).as_mut_ptr());
						}
					}
				}
			}

			struct Finish<'a> {
				status: &'a AtomicStatus,
			}

			impl<'a> Drop for Finish<'a> {
				fn drop(&mut self) {
					// While using Relaxed here would most likely not be an issue, we use SeqCst anyway.
					self.status.store(Status::Panicked, Ordering::SeqCst);
				}
			}
		}
		/// A primitive that provides lazy one-time initialization.
		pub type Once<T = ()> = self::once::Once<T>;
		
		pub mod relax
		{
			/*!
			Strategies that determine the behaviour of locks when encountering contention. */
			use ::
			{
				*,
			};
			/*
			*/
			/// A trait implemented by spinning relax strategies.
			pub trait RelaxStrategy {
				/// Perform the relaxing operation during a period of contention.
				fn relax();
			}

			/// A strategy that rapidly spins while informing the CPU that it should power down non-essential components via
			/// [`core::hint::spin_loop`].
			pub struct Spin;

			impl RelaxStrategy for Spin {
				#[inline(always)]
				fn relax() {
					// Use the deprecated spin_loop_hint() to ensure that we don't get
					// a higher MSRV than we need to.
					#[allow(deprecated)]
					core::sync::atomic::spin_loop_hint();
				}
			}

			/// A strategy that yields the current time slice to the scheduler in favour of other threads or processes.
			pub struct Yield;
			
			impl RelaxStrategy for Yield {
				#[inline(always)]
				fn relax() {
					std::thread::yield_now();
				}
			}

			/// A strategy that rapidly spins, without telling the CPU to do any powering down.
			pub struct Loop;

			impl RelaxStrategy for Loop {
				#[inline(always)]
				fn relax() {}
			}

		} pub use self::relax::{RelaxStrategy, Spin, Yield};
		
		pub mod rwlock
		{
			/*!
			*/
			use ::
			{
				cell::{ UnsafeCell },
				marker::{ PhantomData },
				mem::{ self, ManuallyDrop },
				ops::{ Deref, DerefMut },
				sync::
				{
					atomic::{ AtomicUsize, Ordering },
					spin::{ RelaxStrategy, Spin },
				},
				*,
			};
			/*
			*/
			const READER: usize = 1 << 2;
			const UPGRADED: usize = 1 << 1;
			const WRITER: usize = 1;
			/// A lock that provides data access to either one writer or many readers.
			pub struct RwLock<T: ?Sized, R = Spin>
			{
				phantom: PhantomData<R>,
				lock: AtomicUsize,
				data: UnsafeCell<T>,
			}

			/// A guard that provides immutable data access.
			pub struct RwLockReadGuard<'a, T: 'a + ?Sized> {
				lock: &'a AtomicUsize,
				data: *const T,
			}

			/// A guard that provides mutable data access.
			pub struct RwLockWriteGuard<'a, T: 'a + ?Sized, R = Spin> {
				phantom: PhantomData<R>,
				inner: &'a RwLock<T, R>,
				data: *mut T,
			}

			/// A guard that provides immutable data access but can be upgraded to [`RwLockWriteGuard`].
			pub struct RwLockUpgradableGuard<'a, T: 'a + ?Sized, R = Spin> {
				phantom: PhantomData<R>,
				inner: &'a RwLock<T, R>,
				data: *const T,
			}

			// Same unsafe impls as `std::sync::RwLock`
			unsafe impl<T: ?Sized + Send, R> Send for RwLock<T, R> {}
			unsafe impl<T: ?Sized + Send + Sync, R> Sync for RwLock<T, R> {}

			unsafe impl<T: ?Sized + Send + Sync, R> Send for RwLockWriteGuard<'_, T, R> {}
			unsafe impl<T: ?Sized + Send + Sync, R> Sync for RwLockWriteGuard<'_, T, R> {}

			unsafe impl<T: ?Sized + Sync> Send for RwLockReadGuard<'_, T> {}
			unsafe impl<T: ?Sized + Sync> Sync for RwLockReadGuard<'_, T> {}

			unsafe impl<T: ?Sized + Send + Sync, R> Send for RwLockUpgradableGuard<'_, T, R> {}
			unsafe impl<T: ?Sized + Send + Sync, R> Sync for RwLockUpgradableGuard<'_, T, R> {}

			impl<T, R> RwLock<T, R> {
				/// Creates a new spinlock wrapping the supplied data.
				#[inline]
				pub const fn new(data: T) -> Self {
					RwLock {
						phantom: PhantomData,
						lock: AtomicUsize::new(0),
						data: UnsafeCell::new(data),
					}
				}

				/// Consumes this `RwLock`, returning the underlying data.
				#[inline]
				pub fn into_inner(self) -> T {
					let RwLock { data, .. } = self;
					data.into_inner()
				}
				/// Returns a mutable pointer to the underying data.
				#[inline(always)]
				pub fn as_mut_ptr(&self) -> *mut T {
					self.data.get()
				}
			}

			impl<T: ?Sized, R: RelaxStrategy> RwLock<T, R> {
				/// Locks this rwlock with shared read access, blocking the current thread
				/// until it can be acquired.
				#[inline]
				pub fn read(&self) -> RwLockReadGuard<T> {
					loop {
						match self.try_read() {
							Some(guard) => return guard,
							None => R::relax(),
						}
					}
				}

				/// Lock this rwlock with exclusive write access, blocking the current
				/// thread until it can be acquired.
				#[inline]
				pub fn write(&self) -> RwLockWriteGuard<T, R> {
					loop {
						match self.try_write_internal(false) {
							Some(guard) => return guard,
							None => R::relax(),
						}
					}
				}

				/// Obtain a readable lock guard that can later be upgraded to a writable lock guard.
				#[inline]
				pub fn upgradeable_read(&self) -> RwLockUpgradableGuard<T, R> {
					loop {
						match self.try_upgradeable_read() {
							Some(guard) => return guard,
							None => R::relax(),
						}
					}
				}
			}

			impl<T: ?Sized, R> RwLock<T, R> {
				// Acquire a read lock, returning the new lock value.
				fn acquire_reader(&self) -> usize {
					// An arbitrary cap that allows us to catch overflows long before they happen
					const MAX_READERS: usize = core::usize::MAX / READER / 2;

					let value = self.lock.fetch_add(READER, Ordering::Acquire);

					if value > MAX_READERS * READER {
						self.lock.fetch_sub(READER, Ordering::Relaxed);
						panic!("Too many lock readers, cannot safely proceed");
					} else {
						value
					}
				}

				/// Attempt to acquire this lock with shared read access.
				#[inline]
				pub fn try_read(&self) -> Option<RwLockReadGuard<T>> {
					let value = self.acquire_reader();

					// We check the UPGRADED bit here so that new readers are prevented when an UPGRADED lock is held.
					// This helps reduce writer starvation.
					if value & (WRITER | UPGRADED) != 0 {
						// Lock is taken, undo.
						self.lock.fetch_sub(READER, Ordering::Release);
						None
					} else {
						Some(RwLockReadGuard {
							lock: &self.lock,
							data: unsafe { &*self.data.get() },
						})
					}
				}

				/// Return the number of readers that currently hold the lock (including upgradable readers).
				pub fn reader_count(&self) -> usize {
					let state = self.lock.load(Ordering::Relaxed);
					state / READER + (state & UPGRADED) / UPGRADED
				}

				/// Return the number of writers that currently hold the lock.
				pub fn writer_count(&self) -> usize {
					(self.lock.load(Ordering::Relaxed) & WRITER) / WRITER
				}

				/// Force decrement the reader count.
				#[inline]
				pub unsafe fn force_read_decrement(&self) {
					debug_assert!(self.lock.load(Ordering::Relaxed) & !WRITER > 0);
					self.lock.fetch_sub(READER, Ordering::Release);
				}

				/// Force unlock exclusive write access.
				#[inline]
				pub unsafe fn force_write_unlock(&self) {
					debug_assert_eq!(self.lock.load(Ordering::Relaxed) & !(WRITER | UPGRADED), 0);
					self.lock.fetch_and(!(WRITER | UPGRADED), Ordering::Release);
				}

				#[inline(always)]
				fn try_write_internal(&self, strong: bool) -> Option<RwLockWriteGuard<T, R>> {
					if compare_exchange(
						&self.lock,
						0,
						WRITER,
						Ordering::Acquire,
						Ordering::Relaxed,
						strong,
					)
					.is_ok()
					{
						Some(RwLockWriteGuard {
							phantom: PhantomData,
							inner: self,
							data: unsafe { &mut *self.data.get() },
						})
					} else {
						None
					}
				}

				/// Attempt to lock this rwlock with exclusive write access.
				#[inline]
				pub fn try_write(&self) -> Option<RwLockWriteGuard<T, R>> {
					self.try_write_internal(true)
				}

				/// Attempt to lock this rwlock with exclusive write access.
				#[inline]
				pub fn try_write_weak(&self) -> Option<RwLockWriteGuard<T, R>> {
					self.try_write_internal(false)
				}

				/// Tries to obtain an upgradeable lock guard.
				#[inline]
				pub fn try_upgradeable_read(&self) -> Option<RwLockUpgradableGuard<T, R>> {
					if self.lock.fetch_or(UPGRADED, Ordering::Acquire) & (WRITER | UPGRADED) == 0 {
						Some(RwLockUpgradableGuard {
							phantom: PhantomData,
							inner: self,
							data: unsafe { &*self.data.get() },
						})
					} else {
						// We can't unflip the UPGRADED bit back just yet as there is another upgradeable or write lock.
						// When they unlock, they will clear the bit.
						None
					}
				}

				/// Returns a mutable reference to the underlying data.
				pub fn get_mut(&mut self) -> &mut T {
					// We know statically that there are no other references to `self`, so
					// there's no need to lock the inner lock.
					unsafe { &mut *self.data.get() }
				}
			}

			impl<T: ?Sized + fmt::Debug, R> fmt::Debug for RwLock<T, R> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					match self.try_read() {
						Some(guard) => write!(f, "RwLock {{ data: ")
							.and_then(|()| (&*guard).fmt(f))
							.and_then(|()| write!(f, " }}")),
						None => write!(f, "RwLock {{ <locked> }}"),
					}
				}
			}

			impl<T: ?Sized + Default, R> Default for RwLock<T, R> {
				fn default() -> Self {
					Self::new(Default::default())
				}
			}

			impl<T, R> From<T> for RwLock<T, R> {
				fn from(data: T) -> Self {
					Self::new(data)
				}
			}

			impl<'rwlock, T: ?Sized> RwLockReadGuard<'rwlock, T> {
				/// Leak the lock guard, yielding a reference to the underlying data.
				#[inline]
				pub fn leak(this: Self) -> &'rwlock T {
					let this = ManuallyDrop::new(this);
					// Safety: We know statically that only we are referencing data
					unsafe { &*this.data }
				}
			}

			impl<'rwlock, T: ?Sized + fmt::Debug> fmt::Debug for RwLockReadGuard<'rwlock, T> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Debug::fmt(&**self, f)
				}
			}

			impl<'rwlock, T: ?Sized + fmt::Display> fmt::Display for RwLockReadGuard<'rwlock, T> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Display::fmt(&**self, f)
				}
			}

			impl<'rwlock, T: ?Sized, R: RelaxStrategy> RwLockUpgradableGuard<'rwlock, T, R> {
				/// Upgrades an upgradeable lock guard to a writable lock guard.
				#[inline]
				pub fn upgrade(mut self) -> RwLockWriteGuard<'rwlock, T, R> {
					loop {
						self = match self.try_upgrade_internal(false) {
							Ok(guard) => return guard,
							Err(e) => e,
						};

						R::relax();
					}
				}
			}

			impl<'rwlock, T: ?Sized, R> RwLockUpgradableGuard<'rwlock, T, R> {
				#[inline(always)]
				fn try_upgrade_internal(self, strong: bool) -> Result<RwLockWriteGuard<'rwlock, T, R>, Self> {
					if compare_exchange(
						&self.inner.lock,
						UPGRADED,
						WRITER,
						Ordering::Acquire,
						Ordering::Relaxed,
						strong,
					)
					.is_ok()
					{
						let inner = self.inner;

						// Forget the old guard so its destructor doesn't run (before mutably aliasing data below)
						mem::forget(self);

						// Upgrade successful
						Ok(RwLockWriteGuard {
							phantom: PhantomData,
							inner,
							data: unsafe { &mut *inner.data.get() },
						})
					} else {
						Err(self)
					}
				}

				/// Tries to upgrade an upgradeable lock guard to a writable lock guard.
				#[inline]
				pub fn try_upgrade(self) -> Result<RwLockWriteGuard<'rwlock, T, R>, Self> {
					self.try_upgrade_internal(true)
				}

				/// Tries to upgrade an upgradeable lock guard to a writable lock guard.
				#[inline]
				pub fn try_upgrade_weak(self) -> Result<RwLockWriteGuard<'rwlock, T, R>, Self> {
					self.try_upgrade_internal(false)
				}

				#[inline]
				/// Downgrades the upgradeable lock guard to a readable, shared lock guard.
				pub fn downgrade(self) -> RwLockReadGuard<'rwlock, T> {
					// Reserve the read guard for ourselves
					self.inner.acquire_reader();

					let inner = self.inner;

					// Dropping self removes the UPGRADED bit
					mem::drop(self);

					RwLockReadGuard {
						lock: &inner.lock,
						data: unsafe { &*inner.data.get() },
					}
				}

				/// Leak the lock guard, yielding a reference to the underlying data.
				#[inline]
				pub fn leak(this: Self) -> &'rwlock T {
					let this = ManuallyDrop::new(this);
					// Safety: We know statically that only we are referencing data
					unsafe { &*this.data }
				}
			}

			impl<'rwlock, T: ?Sized + fmt::Debug, R> fmt::Debug for RwLockUpgradableGuard<'rwlock, T, R> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Debug::fmt(&**self, f)
				}
			}

			impl<'rwlock, T: ?Sized + fmt::Display, R> fmt::Display for RwLockUpgradableGuard<'rwlock, T, R> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Display::fmt(&**self, f)
				}
			}

			impl<'rwlock, T: ?Sized, R> RwLockWriteGuard<'rwlock, T, R> {
				/// Downgrades the writable lock guard to a readable, shared lock guard.
				#[inline]
				pub fn downgrade(self) -> RwLockReadGuard<'rwlock, T> {
					// Reserve the read guard for ourselves
					self.inner.acquire_reader();

					let inner = self.inner;

					// Dropping self removes the UPGRADED bit
					mem::drop(self);

					RwLockReadGuard {
						lock: &inner.lock,
						data: unsafe { &*inner.data.get() },
					}
				}

				/// Downgrades the writable lock guard to an upgradable, shared lock guard.
				#[inline]
				pub fn downgrade_to_upgradeable(self) -> RwLockUpgradableGuard<'rwlock, T, R> {
					debug_assert_eq!(
						self.inner.lock.load(Ordering::Acquire) & (WRITER | UPGRADED),
						WRITER
					);

					// Reserve the read guard for ourselves
					self.inner.lock.store(UPGRADED, Ordering::Release);

					let inner = self.inner;

					// Dropping self removes the UPGRADED bit
					mem::forget(self);

					RwLockUpgradableGuard {
						phantom: PhantomData,
						inner,
						data: unsafe { &*inner.data.get() },
					}
				}

				/// Leak the lock guard, yielding a mutable reference to the underlying data.
				#[inline]
				pub fn leak(this: Self) -> &'rwlock mut T {
					let mut this = ManuallyDrop::new(this);
					// Safety: We know statically that only we are referencing data
					unsafe { &mut *this.data }
				}
			}

			impl<'rwlock, T: ?Sized + fmt::Debug, R> fmt::Debug for RwLockWriteGuard<'rwlock, T, R> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Debug::fmt(&**self, f)
				}
			}

			impl<'rwlock, T: ?Sized + fmt::Display, R> fmt::Display for RwLockWriteGuard<'rwlock, T, R> {
				fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
					fmt::Display::fmt(&**self, f)
				}
			}

			impl<'rwlock, T: ?Sized> Deref for RwLockReadGuard<'rwlock, T> {
				type Target = T;

				fn deref(&self) -> &T {
					// Safety: We know statically that only we are referencing data
					unsafe { &*self.data }
				}
			}

			impl<'rwlock, T: ?Sized, R> Deref for RwLockUpgradableGuard<'rwlock, T, R> {
				type Target = T;

				fn deref(&self) -> &T {
					// Safety: We know statically that only we are referencing data
					unsafe { &*self.data }
				}
			}

			impl<'rwlock, T: ?Sized, R> Deref for RwLockWriteGuard<'rwlock, T, R> {
				type Target = T;

				fn deref(&self) -> &T {
					// Safety: We know statically that only we are referencing data
					unsafe { &*self.data }
				}
			}

			impl<'rwlock, T: ?Sized, R> DerefMut for RwLockWriteGuard<'rwlock, T, R> {
				fn deref_mut(&mut self) -> &mut T {
					// Safety: We know statically that only we are referencing data
					unsafe { &mut *self.data }
				}
			}

			impl<'rwlock, T: ?Sized> Drop for RwLockReadGuard<'rwlock, T> {
				fn drop(&mut self) {
					debug_assert!(self.lock.load(Ordering::Relaxed) & !(WRITER | UPGRADED) > 0);
					self.lock.fetch_sub(READER, Ordering::Release);
				}
			}

			impl<'rwlock, T: ?Sized, R> Drop for RwLockUpgradableGuard<'rwlock, T, R> {
				fn drop(&mut self) {
					debug_assert_eq!(
						self.inner.lock.load(Ordering::Relaxed) & (WRITER | UPGRADED),
						UPGRADED
					);
					self.inner.lock.fetch_sub(UPGRADED, Ordering::AcqRel);
				}
			}

			impl<'rwlock, T: ?Sized, R> Drop for RwLockWriteGuard<'rwlock, T, R> {
				fn drop(&mut self) {
					debug_assert_eq!(self.inner.lock.load(Ordering::Relaxed) & WRITER, WRITER);

					// Writer is responsible for clearing both WRITER and UPGRADED bits.
					// The UPGRADED bit may be set if an upgradeable lock attempts an upgrade while this lock is held.
					self.inner
						.lock
						.fetch_and(!(WRITER | UPGRADED), Ordering::Release);
				}
			}

			#[inline(always)]
			fn compare_exchange(
				atomic: &AtomicUsize,
				current: usize,
				new: usize,
				success: Ordering,
				failure: Ordering,
				strong: bool,
			) -> Result<usize, usize> {
				if strong {
					atomic.compare_exchange(current, new, success, failure)
				} else {
					atomic.compare_exchange_weak(current, new, success, failure)
				}
			}
			
		} pub use self::rwlock::RwLockReadGuard;
		/// A lock that provides data access to either one writer or many readers.
		pub type RwLock<T> = crate::rwlock::RwLock<T>;
		/// A guard that provides immutable data access but can be upgraded to [`RwLockWriteGuard`].
		pub type RwLockUpgradableGuard<'a, T> = self::rwlock::RwLockUpgradableGuard<'a, T>;
		/// A guard that provides mutable data access.
		pub type RwLockWriteGuard<'a, T> = self::rwlock::RwLockWriteGuard<'a, T>;		
		/// In the event of an invalid operation, it's best to abort the current process.
		fn abort() -> !
		{ 
			::process::abort();	
		}
	}
	
	pub struct Lazy<T: Sync>(Once<T>);
	
	impl<T: Sync> Lazy<T>
	{
		pub const INIT: Self = Lazy(Once::INIT);
		#[inline( always )] pub fn get<F>(&'static self, builder: F) -> &T where
		F: FnOnce() -> T,
		{ self.0.call_once(builder) }
	}

	/// Support trait for enabling a few common operations on lazy static values.
	pub trait LazyStatic
	{
		#[doc(hidden)]
		fn initialize(lazy: &Self);
	}
	/// Takes a shared reference to a lazy static and initializes it if it has not been already.
	pub fn initialize<T: LazyStatic>(lazy: &T)
	{
		LazyStatic::initialize(lazy);
	}

}
// 9767 //////////////////////////////////////////////////////////////////////////////////////////////////////////////
