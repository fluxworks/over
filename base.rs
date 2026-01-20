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
    unused_unsafe,
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
    
    #[macro_export] macro_rules! println_stderr
    {
        ($fmt:expr) =>
        (
            match writeln!(&mut ::io::stderr(), $fmt)
            {
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

    #[macro_export] macro_rules! bitflags
    {
        (
            $(#[$outer:meta])*
            $vis:vis struct $BitFlags:ident: $T:ty
            {
                $(
                    $(#[$inner:ident $($args:tt)*])*
                    const $Flag:tt = $value:expr;
                )*
            }

            $($t:tt)*
        ) =>
        {
            ::__declare_public_bitflags!
            {
                $(#[$outer])*
                $vis struct $BitFlags
            }
            
            ::__impl_public_bitflags_consts!
            {
                $BitFlags: $T
                {
                    $(
                        $(#[$inner $($args)*])*
                        const $Flag = $value;
                    )*
                }
            }
            
            const _: () =
            {
                ::__declare_internal_bitflags!
                {
                    $vis struct InternalBitFlags: $T
                }

                ::__impl_internal_bitflags!
                {
                    InternalBitFlags: $T, $BitFlags
                    {
                        $(
                            $(#[$inner $($args)*])*
                            const $Flag = $value;
                        )*
                    }
                }

                ::__impl_public_bitflags_forward!
                {
                    $BitFlags: $T, InternalBitFlags
                }

                ::__impl_public_bitflags_ops!
                {
                    $BitFlags
                }

                ::__impl_public_bitflags_iter!
                {
                    $BitFlags: $T, $BitFlags
                }
            };

            ::bitflags!
            {
                $($t)*
            }
        };

        (
            $(#[$outer:meta])*
            impl $BitFlags:ident: $T:ty
            {
                $(
                    $(#[$inner:ident $($args:tt)*])*
                    const $Flag:tt = $value:expr;
                )*
            }

            $($t:tt)*
        ) =>
        {
            ::__impl_public_bitflags_consts!
            {
                $BitFlags: $T
                {
                    $(
                        $(#[$inner $($args)*])*
                        const $Flag = $value;
                    )*
                }
            }
            
            const _: () =
            {
                ::__impl_public_bitflags!
                {
                    $(#[$outer])*
                    $BitFlags: $T, $BitFlags
                    {
                        $(
                            $(#[$inner $($args)*])*
                            const $Flag = $value;
                        )*
                    }
                }

                ::__impl_public_bitflags_ops!
                {
                    $BitFlags
                }

                ::__impl_public_bitflags_iter!
                {
                    $BitFlags: $T, $BitFlags
                }
            };

            ::bitflags!
            {
                $($t)*
            }
        };

        () => {};
    }
    
    #[macro_export] macro_rules! bitflags_match
    {
        ($operation:expr,
        {
            $($t:tt)*
        }) =>
        {
            (||
            {
                ::__bitflags_match!($operation, { $($t)* })
            })()
        };
    }

    #[macro_export] macro_rules! __impl_public_bitflags_consts
    {
        (
            $(#[$outer:meta])*
            $PublicBitFlags:ident: $T:ty
            {
                $(
                    $(#[$inner:ident $($args:tt)*])*
                    const $Flag:tt = $value:expr;
                )*
            }
        ) =>
        {
            $(#[$outer])*
            impl $PublicBitFlags
            {
                $(
                    ::__bitflags_flag!
                    ({
                        name: $Flag,
                        named:
                        {
                            $(#[$inner $($args)*])*
                            pub const $Flag: Self = Self::from_bits_retain($value);
                        },
                        unnamed: {},
                    });
                )*
            }

            $(#[$outer])*
            impl ::bits::flags::Flags for $PublicBitFlags
            {
                const FLAGS: &'static [::bits::flags::Flag<$PublicBitFlags>] = 
                &[
                    $(
                        ::__bitflags_flag!
                        ({
                            name: $Flag,
                            named:
                            {
                                ::__bitflags_expr_safe_attrs!(
                                    $(#[$inner $($args)*])*
                                    {
                                        ::bits::flags::Flag::new(stringify!($Flag), $PublicBitFlags::$Flag)
                                    }
                                )
                            },
                            unnamed:
                            {
                                ::__bitflags_expr_safe_attrs!
                                (
                                    $(#[$inner $($args)*])*
                                    {
                                        ::bits::flags::Flag::new("", $PublicBitFlags::from_bits_retain($value))
                                    }
                                )
                            },
                        }),
                    )*
                ];

                type Bits = $T;

                fn bits(&self) -> $T { $PublicBitFlags::bits(self) }

                fn from_bits_retain(bits: $T) -> $PublicBitFlags { $PublicBitFlags::from_bits_retain(bits) }
            }
        };
    }
    
    #[macro_export] macro_rules! __impl_bitflags
    {
        (
            params: $self:ident, $bits:ident, $name:ident, $other:ident, $value:ident;
            $(#[$outer:meta])*
            $PublicBitFlags:ident: $T:ty
            {
                fn empty() $empty_body:block
                fn all() $all_body:block
                fn bits(&self) $bits_body:block
                fn from_bits(bits) $from_bits_body:block
                fn from_bits_truncate(bits) $from_bits_truncate_body:block
                fn from_bits_retain(bits) $from_bits_retain_body:block
                fn from_name(name) $from_name_body:block
                fn is_empty(&self) $is_empty_body:block
                fn is_all(&self) $is_all_body:block
                fn intersects(&self, other) $intersects_body:block
                fn contains(&self, other) $contains_body:block
                fn insert(&mut self, other) $insert_body:block
                fn remove(&mut self, other) $remove_body:block
                fn toggle(&mut self, other) $toggle_body:block
                fn set(&mut self, other, value) $set_body:block
                fn intersection(self, other) $intersection_body:block
                fn union(self, other) $union_body:block
                fn difference(self, other) $difference_body:block
                fn symmetric_difference(self, other) $symmetric_difference_body:block
                fn complement(self) $complement_body:block
            }
        ) =>
        {
            $(#[$outer])*
            impl $PublicBitFlags
            {
                #[inline] pub const fn empty() -> Self
                    $empty_body
                    
                #[inline] pub const fn all() -> Self
                    $all_body
                    
                #[inline] pub const fn bits(&$self) -> $T
                    $bits_body
                    
                #[inline] pub const fn from_bits($bits: $T) -> ::option::Option<Self>
                    $from_bits_body
                    
                #[inline] pub const fn from_bits_truncate($bits: $T) -> Self
                    $from_bits_truncate_body
                    
                #[inline] pub const fn from_bits_retain($bits: $T) -> Self
                    $from_bits_retain_body
                    
                #[inline] pub fn from_name($name: &str) -> ::option::Option<Self>
                    $from_name_body
                    
                #[inline] pub const fn is_empty(&$self) -> bool
                    $is_empty_body
                    
                #[inline] pub const fn is_all(&$self) -> bool
                    $is_all_body
                    
                #[inline] pub const fn intersects(&$self, $other: Self) -> bool
                    $intersects_body
                    
                #[inline] pub const fn contains(&$self, $other: Self) -> bool
                    $contains_body
                    
                #[inline] pub fn insert(&mut $self, $other: Self)
                    $insert_body
                    
                #[inline] pub fn remove(&mut $self, $other: Self)
                    $remove_body
                    
                #[inline] pub fn toggle(&mut $self, $other: Self)
                    $toggle_body
                    
                #[inline] pub fn set(&mut $self, $other: Self, $value: bool)
                    $set_body
                    
                #[must_use] #[inline] pub const fn intersection($self, $other: Self) -> Self
                    $intersection_body
                    
                #[must_use] #[inline] pub const fn union($self, $other: Self) -> Self
                    $union_body
                    
                #[must_use] #[inline] pub const fn difference($self, $other: Self) -> Self
                    $difference_body

                #[must_use] #[inline] pub const fn symmetric_difference($self, $other: Self) -> Self
                    $symmetric_difference_body
                    
                #[must_use] #[inline] pub const fn complement($self) -> Self
                    $complement_body
            }
        };
    }

    #[macro_export] macro_rules! __declare_public_bitflags
    {
        (
            $(#[$outer:meta])*
            $vis:vis struct $PublicBitFlags:ident
        ) =>
        {
            $(#[$outer])*
            $vis struct $PublicBitFlags(<$PublicBitFlags as ::bits::flags::PublicFlags>::Internal);
        };
    }
    
    #[macro_export] macro_rules! __bitflags_match
    {
        ($operation:expr, { $pattern:expr => { $($body:tt)* } , $($t:tt)+ }) =>
        {
            ::__bitflags_match!($operation, { $pattern => { $($body)* } $($t)+ })
        };
        
        ($operation:expr, { $pattern:expr => { $($body:tt)* } $($t:tt)+ }) =>
        {
            {
                if $operation == $pattern
                {
                    return
                    {
                        $($body)*
                    };
                }

                ::__bitflags_match!($operation, { $($t)+ })
            }
        };
        
        ($operation:expr, { $pattern:expr => $body:expr , $($t:tt)+ }) =>
        {
            {
                if $operation == $pattern { return $body; }

                ::__bitflags_match!($operation, { $($t)+ })
            }
        };
        
        ($operation:expr, { _ => $default:expr $(,)? }) => { $default }
    }
    
    #[macro_export] macro_rules! __bitflags_expr_safe_attrs
    {
        (
            $(#[$inner:ident $($args:tt)*])*
            { $e:expr }
        ) =>
        {
            ::__bitflags_expr_safe_attrs!
            {
                expr: { $e },
                attrs:
                {
                    unprocessed: [$(#[$inner $($args)*])*],
                    processed: [],
                },
            }
        };
        
        (
            expr: { $e:expr },
            attrs:
            {
                unprocessed:
                [
                    #[cfg $($args:tt)*]
                    $($attrs_rest:tt)*
                ],
                processed: [$($expr:tt)*],
            },
        ) =>
        {
            ::__bitflags_expr_safe_attrs!
            {
                expr: { $e },
                attrs:
                {
                    unprocessed:
                    [
                        $($attrs_rest)*
                    ],
                    processed:
                    [
                        $($expr)*
                        #[cfg $($args)*]
                    ],
                },
            }
        };
        
        (
            expr: { $e:expr },
            attrs:
            {
                unprocessed:
                [
                    #[$other:ident $($args:tt)*]
                    $($attrs_rest:tt)*
                ],
                processed: [$($expr:tt)*],
            },
        ) =>
        {
            ::__bitflags_expr_safe_attrs!
            {
                expr: { $e },
                attrs:
                {
                    unprocessed:
                    [
                        $($attrs_rest)*
                    ],
                    processed:
                    [
                        $($expr)*
                    ],
                },
            }
        };
        
        (
            expr: { $e:expr },
            attrs:
            {
                unprocessed: [],
                processed: [$(#[$expr:ident $($exprargs:tt)*])*],
            },
        ) =>
        {
            $(#[$expr $($exprargs)*])*
            { $e }
        }
    }
    
    #[macro_export] macro_rules! __bitflags_flag
    {
        (
            {
                name: _,
                named: { $($named:tt)* },
                unnamed: { $($unnamed:tt)* },
            }
        ) =>
        {
            $($unnamed)*
        };
        (
            {
                name: $Flag:ident,
                named: { $($named:tt)* },
                unnamed: { $($unnamed:tt)* },
            }
        ) => {
            $($named)*
        };
    }

    #[macro_export] macro_rules! __declare_internal_bitflags
    {
        ( $vis:vis struct $InternalBitFlags:ident: $T:ty ) =>
        {
            #[repr(transparent)] #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            $vis struct $InternalBitFlags($T);
        };
    }
    
    #[macro_export] macro_rules! __impl_internal_bitflags
    {
        (
            $InternalBitFlags:ident: $T:ty, $PublicBitFlags:ident
            {
                $(
                    $(#[$inner:ident $($args:tt)*])*
                    const $Flag:tt = $value:expr;
                )*
            }
        ) =>
        {
            impl::bits::flags::PublicFlags for $PublicBitFlags
            {
                type Primitive = $T;
                type Internal = $InternalBitFlags;
            }

            impl ::default::Default for $InternalBitFlags
            {
                #[inline] fn default() -> Self { $InternalBitFlags::empty() }
            }

            impl ::fmt::Debug for $InternalBitFlags
            {
                fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result
                {
                    if self.is_empty() { write!(f, "{:#x}", <$T as ::bits::flags::Bits>::EMPTY) }
                    else { ::fmt::Display::fmt(self, f) }
                }
            }

            impl ::fmt::Display for $InternalBitFlags
            {
                fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result
                {
                    ::bits::flags::to_writer(&$PublicBitFlags(*self), f)
                }
            }

            impl ::str::FromStr for $InternalBitFlags
            {
                type Err = ::bits::flags::ParseError;

                fn from_str(s: &str) -> ::result::Result<Self, Self::Err> 
                { ::bits::flags::from_str::<$PublicBitFlags>(s).map(|flags| flags.0) }
            }

            impl ::convert::AsRef<$T> for $InternalBitFlags
            {
                fn as_ref(&self) -> &$T { &self.0 }
            }

            impl ::convert::From<$T> for $InternalBitFlags
            {
                fn from(bits: $T) -> Self { Self::from_bits_retain(bits) }
            }
            
            __impl_public_bitflags!
            {
                $InternalBitFlags: $T, $PublicBitFlags
                {
                    $(
                        $(#[$inner $($args)*])*
                        const $Flag = $value;
                    )*
                }
            }
            
            __impl_public_bitflags_ops!
            {
                $InternalBitFlags
            }
            
            __impl_public_bitflags_iter!
            {
                $InternalBitFlags: $T, $PublicBitFlags
            }

            impl $InternalBitFlags
            {
                #[inline] pub fn bits_mut(&mut self) -> &mut $T { &mut self.0 }
            }
        };
    }
    
    #[macro_export] macro_rules! __impl_public_bitflags_forward
    {
        (
            $(#[$outer:meta])*
            $PublicBitFlags:ident: $T:ty, $InternalBitFlags:ident
        ) =>
        {
            __impl_bitflags!
            {
                params: self, bits, name, other, value;
                $(#[$outer])*
                $PublicBitFlags: $T
                {
                    fn empty() { Self($InternalBitFlags::empty()) }

                    fn all() { Self($InternalBitFlags::all()) }

                    fn bits(&self) { self.0.bits() }

                    fn from_bits(bits)
                    {
                        match $InternalBitFlags::from_bits(bits)
                        {
                            Some(bits) => Some(Self(bits)),
                            None => None,
                        }
                    }

                    fn from_bits_truncate(bits) { Self($InternalBitFlags::from_bits_truncate(bits)) }

                    fn from_bits_retain(bits) { Self($InternalBitFlags::from_bits_retain(bits)) }

                    fn from_name(name)
                    {
                        match $InternalBitFlags::from_name(name)
                        {
                            Some(bits) => Some(Self(bits)),
                            None => None,
                        }
                    }

                    fn is_empty(&self) { self.0.is_empty() }
                    fn is_all(&self) { self.0.is_all() }
                    fn intersects(&self, other) { self.0.intersects(other.0) }
                    fn contains(&self, other) { self.0.contains(other.0) }
                    fn insert(&mut self, other) { self.0.insert(other.0) }
                    fn remove(&mut self, other) { self.0.remove(other.0) }
                    fn toggle(&mut self, other) { self.0.toggle(other.0) }
                    fn set(&mut self, other, value) { self.0.set(other.0, value) }
                    fn intersection(self, other) { Self(self.0.intersection(other.0)) }
                    fn union(self, other) { Self(self.0.union(other.0)) }
                    fn difference(self, other) { Self(self.0.difference(other.0)) }
                    fn symmetric_difference(self, other) { Self(self.0.symmetric_difference(other.0)) }
                    fn complement(self) { Self(self.0.complement()) }
                }
            }
        };
    }
    
    #[macro_export] macro_rules! __impl_public_bitflags
    {
        (
            $(#[$outer:meta])*
            $BitFlags:ident: $T:ty, $PublicBitFlags:ident
            {
                $(
                    $(#[$inner:ident $($args:tt)*])*
                    const $Flag:tt = $value:expr;
                )*
            }
        ) =>
        {
            __impl_bitflags!
            {
                params: self, bits, name, other, value;
                $(#[$outer])*
                $BitFlags: $T
                {
                    fn empty() { Self(<$T as ::bits::flags::Bits>::EMPTY) }

                    fn all()
                    {
                        let mut truncated = <$T as ::bits::flags::Bits>::EMPTY;
                        let mut i = 0;

                        $(
                            __bitflags_expr_safe_attrs!
                            (
                                $(#[$inner $($args)*])*
                                {{
                                    let flag = <$PublicBitFlags as ::bits::flags::Flags>::FLAGS[i].value().bits();

                                    truncated = truncated | flag;
                                    i += 1;
                                }}
                            );
                        )*

                        let _ = i;
                        Self(truncated)
                    }

                    fn bits(&self) { self.0 }

                    fn from_bits(bits)
                    {
                        let truncated = Self::from_bits_truncate(bits).0;

                        if truncated == bits { Some(Self(bits)) }
                        else { None }
                    }

                    fn from_bits_truncate(bits) { Self(bits & Self::all().0) }

                    fn from_bits_retain(bits) { Self(bits) }

                    fn from_name(name)
                    {
                        $(
                            __bitflags_flag!
                            ({
                                name: $Flag,
                                named:
                                {
                                    __bitflags_expr_safe_attrs!
                                    (
                                        $(#[$inner $($args)*])*
                                        {
                                            if name == stringify!($Flag) {
                                                return Some(Self($PublicBitFlags::$Flag.bits()));
                                            }
                                        }
                                    );
                                },
                                unnamed: {},
                            });
                        )*

                        let _ = name;
                        None
                    }

                    fn is_empty(&self) { self.0 == <$T as ::bits::flags::Bits>::EMPTY }
                    fn is_all(&self) { Self::all().0 | self.0 == self.0 }
                    fn intersects(&self, other) { self.0 & other.0 != <$T as ::bits::flags::Bits>::EMPTY }
                    fn contains(&self, other) { self.0 & other.0 == other.0 }
                    fn insert(&mut self, other) { *self = Self(self.0).union(other); }
                    fn remove(&mut self, other) { *self = Self(self.0).difference(other); }
                    fn toggle(&mut self, other) { *self = Self(self.0).symmetric_difference(other); }
                    fn set(&mut self, other, value)
                    {
                        if value { self.insert(other); }
                        else { self.remove(other); }
                    }
                    fn intersection(self, other) { Self(self.0 & other.0) }
                    fn union(self, other) { Self(self.0 | other.0) }
                    fn difference(self, other) { Self(self.0 & !other.0) }
                    fn symmetric_difference(self, other) { Self(self.0 ^ other.0) }
                    fn complement(self) { Self::from_bits_truncate(!self.0) }
                }
            }
        };
    }
    
    #[macro_export] macro_rules! __impl_public_bitflags_iter
    {
        (
            $(#[$outer:meta])*
            $BitFlags:ident: $T:ty, $PublicBitFlags:ident
        ) =>
        {
            $(#[$outer])*
            impl $BitFlags
            {
                
                #[inline] pub const fn iter(&self) -> ::bits::flags::Iter<$PublicBitFlags>
                {
                    ::bits::flags::Iter::__private_const_new
                    (
                        <$PublicBitFlags as ::bits::flags::Flags>::FLAGS,
                        $PublicBitFlags::from_bits_retain(self.bits()),
                        $PublicBitFlags::from_bits_retain(self.bits()),
                    )
                }
                
                #[inline] pub const fn iter_names(&self) -> ::bits::flags::IterNames<$PublicBitFlags>
                {
                    ::bits::flags::IterNames::__private_const_new
                    (
                        <$PublicBitFlags as ::bits::flags::Flags>::FLAGS,
                        $PublicBitFlags::from_bits_retain(self.bits()),
                        $PublicBitFlags::from_bits_retain(self.bits()),
                    )
                }
            }

            $(#[$outer:meta])*
            impl ::iter::IntoIterator for $BitFlags
            {
                type Item = $PublicBitFlags;
                type IntoIter = ::bits::flags::Iter<$PublicBitFlags>;
                fn into_iter(self) -> Self::IntoIter { self.iter() }
            }
        };
    }
    
    #[macro_export] macro_rules! __impl_public_bitflags_ops
    {
        (
            $(#[$outer:meta])*
            $PublicBitFlags:ident
        ) =>
        {

            $(#[$outer])*
            impl ::fmt::Binary for $PublicBitFlags
            {
                fn fmt
                (
                    &self,
                    f: &mut ::fmt::Formatter,
                ) -> ::fmt::Result
                {
                    let inner = self.0;
                    ::fmt::Binary::fmt(&inner, f)
                }
            }

            $(#[$outer])*
            impl ::fmt::Octal for $PublicBitFlags
            {
                fn fmt
                (
                    &self,
                    f: &mut ::fmt::Formatter,
                ) -> ::fmt::Result
                {
                    let inner = self.0;
                    ::fmt::Octal::fmt(&inner, f)
                }
            }

            $(#[$outer])*
            impl ::fmt::LowerHex for $PublicBitFlags
            {
                fn fmt
                (
                    &self,
                    f: &mut ::fmt::Formatter,
                ) -> ::fmt::Result
                {
                    let inner = self.0;
                    ::fmt::LowerHex::fmt(&inner, f)
                }
            }

            $(#[$outer])*
            impl ::fmt::UpperHex for $PublicBitFlags
            {
                fn fmt
                (
                    &self,
                    f: &mut ::fmt::Formatter,
                ) -> ::fmt::Result
                {
                    let inner = self.0;
                    ::fmt::UpperHex::fmt(&inner, f)
                }
            }

            $(#[$outer])*
            impl ::ops::BitOr for $PublicBitFlags
            {
                type Output = Self;
                #[inline] fn bitor(self, other: $PublicBitFlags) -> Self { self.union(other) }
            }

            $(#[$outer])*
            impl ::ops::BitOrAssign for $PublicBitFlags
            {
                #[inline] fn bitor_assign(&mut self, other: Self) { self.insert(other); }
            }

            $(#[$outer])*
            impl ops::BitXor for $PublicBitFlags
            {
                type Output = Self;                
                #[inline] fn bitxor(self, other: Self) -> Self { self.symmetric_difference(other) }
            }

            $(#[$outer])*
            impl ::ops::BitXorAssign for $PublicBitFlags
            {
                #[inline] fn bitxor_assign(&mut self, other: Self) { self.toggle(other); }
            }

            $(#[$outer])*
            impl ::ops::BitAnd for $PublicBitFlags
            {
                type Output = Self;
                #[inline] fn bitand(self, other: Self) -> Self { self.intersection(other) }
            }

            $(#[$outer])*
            impl ::ops::BitAndAssign for $PublicBitFlags
            {
                #[inline] fn bitand_assign(&mut self, other: Self)
                { *self = Self::from_bits_retain(self.bits()).intersection(other); }
            }

            $(#[$outer])*
            impl ::ops::Sub for $PublicBitFlags
            {
                type Output = Self;
                #[inline] fn sub(self, other: Self) -> Self { self.difference(other) }
            }

            $(#[$outer])*
            impl ::ops::SubAssign for $PublicBitFlags
            {
                #[inline] fn sub_assign(&mut self, other: Self) { self.remove(other); }
            }

            $(#[$outer])*
            impl ::ops::Not for $PublicBitFlags
            {
                type Output = Self;
                #[inline] fn not(self) -> Self { self.complement() }
            }

            $(#[$outer])*
            impl ::iter::Extend<$PublicBitFlags> for $PublicBitFlags
            {
                fn extend<T: ::iter::IntoIterator<Item = Self>>( &mut self, iterator: T )
                {
                    for item in iterator
                    {
                        self.insert(item)
                    }
                }
            }

            $(#[$outer])*
            impl ::iter::FromIterator<$PublicBitFlags> for $PublicBitFlags
            {
                fn from_iter<T: ::iter::IntoIterator<Item = Self>>( iterator: T ) -> Self
                {
                    use ::iter::Extend;
                    let mut result = Self::empty();
                    result.extend(iterator);
                    result
                }
            }
        };
    }
    
    #[macro_export] macro_rules! smallvec
    {
        (@one $x:expr) => (1usize);
        ($elem:expr; $n:expr) => 
        ({
            ::vec::SmallVec::from_elem($elem, $n)
        });
        
        ($($x:expr),*$(,)*) =>
        ({
            let count = 0usize $(+ smallvec!(@one $x))*;
            let mut vec = ::vec::SmallVec::new();
            if count <= vec.inline_size()
            {
                $(vec.push($x);)*
                vec
            }
            else { ::vec::SmallVec::from_vec(vec![$($x,)*]) }
        });
    }

    #[macro_export] macro_rules! err
    {
        ($code:expr $(,)?) => { ::error::sqlite::error_from_sqlite_code($code, None) };
        ($code:expr, $msg:literal $(,)?) => { ::error::sqlite::error_from_sqlite_code($code, Some(format!($msg))) };
        ($code:expr, $err:expr $(,)?) => { ::error::sqlite::error_from_sqlite_code($code, Some(format!($err))) };
        ($code:expr, $fmt:expr, $($arg:tt)*) => { ::error::sqlite::error_from_sqlite_code($code, Some(format!($fmt, $($arg)*))) };
    }
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
    pub mod flags
    {
        /*!
        */
        use ::
        {
            fmt::{ Write },
            ops::{BitAnd, BitOr, BitXor, Not},
            *,
        };
        /*
        */

        macro_rules! impl_bits
        {
            ($($u:ty, $i:ty,)*) =>
            {
                $(
                    impl Bits for $u
                    {
                        const EMPTY: $u = 0;
                        const ALL: $u = <$u>::MAX;
                    }

                    impl Bits for $i
                    {
                        const EMPTY: $i = 0;
                        const ALL: $i = <$u>::MAX as $i;
                    }

                    impl ParseHex for $u
                    {
                        fn parse_hex(input: &str) -> Result<Self, ParseError>
                        { <$u>::from_str_radix(input, 16).map_err(|_| ParseError::invalid_hex_flag(input)) }
                    }

                    impl ParseHex for $i
                    {
                        fn parse_hex(input: &str) -> Result<Self, ParseError>
                        { <$i>::from_str_radix(input, 16).map_err(|_| ParseError::invalid_hex_flag(input)) }
                    }

                    impl WriteHex for $u
                    {
                        fn write_hex<W: fmt::Write>(&self, mut writer: W) -> fmt::Result { write!(writer, "{:x}", self) }
                    }

                    impl WriteHex for $i
                    {
                        fn write_hex<W: fmt::Write>(&self, mut writer: W) -> fmt::Result { write!(writer, "{:x}", self) }
                    }

                    impl Primitive for $i {}
                    impl Primitive for $u {}
                )*
            }
        }

        #[derive(Debug)]
        pub struct Flag<B>
        {
            name: &'static str,
            value: B,
        }
        
        impl<B> Flag<B>
        {
            pub const fn new(name: &'static str, value: B) -> Self { Flag { name, value } }
            
            pub const fn name(&self) -> &'static str { self.name }
            
            pub const fn value(&self) -> &B { &self.value }
            
            pub const fn is_named(&self) -> bool { !self.name.is_empty() }
            
            pub const fn is_unnamed(&self) -> bool { self.name.is_empty() }
        }

        pub trait Flags: Sized + 'static
        {
            const FLAGS: &'static [Flag<Self>];
            type Bits: Bits;
            fn empty() -> Self { Self::from_bits_retain(Self::Bits::EMPTY) }
            
            fn all() -> Self
            {
                let mut truncated = Self::Bits::EMPTY;

                for flag in Self::FLAGS.iter()
                {
                    truncated = truncated | flag.value().bits();
                }

                Self::from_bits_retain(truncated)
            }
            
            fn contains_unknown_bits(&self) -> bool { Self::all().bits() & self.bits() != self.bits() }
            
            fn bits(&self) -> Self::Bits;
            
            fn from_bits(bits: Self::Bits) -> Option<Self>
            {
                let truncated = Self::from_bits_truncate(bits);

                if truncated.bits() == bits { Some(truncated) } else { None }
            }
            
            fn from_bits_truncate(bits: Self::Bits) -> Self { Self::from_bits_retain(bits & Self::all().bits()) }
            
            fn from_bits_retain(bits: Self::Bits) -> Self;
            
            fn from_name(name: &str) -> Option<Self>
            {
                if name.is_empty() { return None; }

                for flag in Self::FLAGS
                {
                    if flag.name() == name
                    {
                        return Some(Self::from_bits_retain(flag.value().bits()));
                    }
                }

                None
            }
            
            fn iter(&self) -> Iter<Self> { Iter::new(self) }
            
            fn iter_names(&self) -> IterNames<Self> { IterNames::new(self) }
            
            fn iter_defined_names() -> IterDefinedNames<Self> { IterDefinedNames::new() }
            
            fn is_empty(&self) -> bool { self.bits() == Self::Bits::EMPTY }
            
            fn is_all(&self) -> bool { Self::all().bits() | self.bits() == self.bits() }
            
            fn intersects(&self, other: Self) -> bool where
            Self: Sized
            { self.bits() & other.bits() != Self::Bits::EMPTY }
            
            fn contains(&self, other: Self) -> bool where
            Self: Sized
            { self.bits() & other.bits() == other.bits() }
            
            fn truncate(&mut self) where
            Self: Sized
            { *self = Self::from_bits_truncate(self.bits()); }
            
            fn insert(&mut self, other: Self) where
            Self: Sized
            { *self = Self::from_bits_retain(self.bits()).union(other); }
            
            fn remove(&mut self, other: Self) where
            Self: Sized
            { *self = Self::from_bits_retain(self.bits()).difference(other); }
            
            fn toggle(&mut self, other: Self) where
            Self: Sized
            { *self = Self::from_bits_retain(self.bits()).symmetric_difference(other); }
            
            fn set(&mut self, other: Self, value: bool) where
            Self: Sized
            {
                if value { self.insert(other); } else { self.remove(other); }
            }
            
            fn clear(&mut self) where
            Self: Sized { *self = Self::empty(); }
            
            #[must_use] fn intersection(self, other: Self) -> Self { Self::from_bits_retain(self.bits() & other.bits()) }
            
            #[must_use] fn union(self, other: Self) -> Self { Self::from_bits_retain(self.bits() | other.bits()) }
            
            #[must_use] fn difference(self, other: Self) -> Self { Self::from_bits_retain(self.bits() & !other.bits()) }
            
            #[must_use] fn symmetric_difference(self, other: Self) -> Self { Self::from_bits_retain(self.bits() ^ other.bits()) }
            
            #[must_use] fn complement(self) -> Self { Self::from_bits_truncate(!self.bits()) }
        }
        
        pub trait Bits:
        Clone
        + Copy
        + PartialEq
        + BitAnd<Output = Self>
        + BitOr<Output = Self>
        + BitXor<Output = Self>
        + Not<Output = Self>
        + Sized
        + 'static
        {
            const EMPTY: Self;
            const ALL: Self;
        }
        
        pub trait Primitive {}

        impl_bits!
        {
            u8, i8,
            u16, i16,
            u32, i32,
            u64, i64,
            u128, i128,
            usize, isize,
        }
        
        pub trait PublicFlags
        {
            type Primitive: Primitive;
            type Internal;
        }
        
        pub trait BitFlags: ImplementedByBitFlagsMacro + Flags
        {
            type Iter: Iterator<Item = Self>;
            type IterNames: Iterator<Item = (&'static str, Self)>;
        }
        
        impl<B: Flags> BitFlags for B
        {
            type Iter = Iter<Self>;
            type IterNames = IterNames<Self>;
        }

        impl<B: Flags> ImplementedByBitFlagsMacro for B {}
        
        pub trait ImplementedByBitFlagsMacro {}

        pub struct Iter<B: 'static>
        {
            inner: IterNames<B>,
            done: bool,
        }

        impl<B: Flags> Iter<B>
        {
            pub fn new(flags: &B) -> Self {
                Iter {
                    inner: IterNames::new(flags),
                    done: false,
                }
            }
        }

        impl<B: 'static> Iter<B>
        {
            pub const fn __private_const_new(flags: &'static [Flag<B>], source: B, remaining: B) -> Self
            {
                Iter
                {
                    inner: IterNames::__private_const_new(flags, source, remaining),
                    done: false,
                }
            }
        }

        impl<B: Flags> Iterator for Iter<B>
        {
            type Item = B;

            fn next(&mut self) -> Option<Self::Item>
            {
                match self.inner.next()
                {
                    Some((_, flag)) => Some(flag),
                    None if !self.done =>
                    {
                        self.done = true;

                        if !self.inner.remaining().is_empty() { Some(B::from_bits_retain(self.inner.remaining.bits())) } 
                        else { None }
                    }
                    None => None,
                }
            }
        }
        
        pub struct IterNames<B: 'static>
        {
            flags: &'static [Flag<B>],
            idx: usize,
            source: B,
            remaining: B,
        }

        impl<B: Flags> IterNames<B>
        {
            pub fn new(flags: &B) -> Self
            {
                IterNames
                {
                    flags: B::FLAGS,
                    idx: 0,
                    remaining: B::from_bits_retain(flags.bits()),
                    source: B::from_bits_retain(flags.bits()),
                }
            }
        }

        impl<B: 'static> IterNames<B>
        {
            pub const fn __private_const_new(flags: &'static [Flag<B>], source: B, remaining: B) -> Self
            {
                IterNames
                {
                    flags,
                    idx: 0,
                    remaining,
                    source,
                }
            }
            
            pub fn remaining(&self) -> &B { &self.remaining }
        }

        impl<B: Flags> Iterator for IterNames<B>
        {
            type Item = (&'static str, B);

            fn next(&mut self) -> Option<Self::Item>
            {
                while let Some(flag) = self.flags.get(self.idx)
                {
                    if self.remaining.is_empty() { return None; }

                    self.idx += 1;
                    
                    if flag.name().is_empty() { continue; }

                    let bits = flag.value().bits();
                    
                    if self.source.contains(B::from_bits_retain(bits))
                    && self.remaining.intersects(B::from_bits_retain(bits))
                    {
                        self.remaining.remove(B::from_bits_retain(bits));
                        return Some((flag.name(), B::from_bits_retain(bits)));
                    }
                }

                None
            }
        }
        
        pub struct IterDefinedNames<B: 'static>
        {
            flags: &'static [Flag<B>],
            idx: usize,
        }

        impl<B: Flags> IterDefinedNames<B>
        {
            pub fn new() -> Self
            {
                IterDefinedNames
                {
                    flags: B::FLAGS,
                    idx: 0,
                }
            }
        }

        impl<B: Flags> Iterator for IterDefinedNames<B>
        {
            type Item = (&'static str, B);

            fn next(&mut self) -> Option<Self::Item>
            {
                while let Some(flag) = self.flags.get(self.idx)
                {
                    self.idx += 1;
                    
                    if flag.is_named() { return Some((flag.name(), B::from_bits_retain(flag.value().bits()))); }
                }

                None
            }
        }
        
        pub fn to_writer<B: Flags>(flags: &B, mut writer: impl Write) -> Result<(), fmt::Error> where
        B::Bits: WriteHex
        {
            let mut first = true;
            let mut iter = flags.iter_names();
            
            for (name, _) in &mut iter
            {
                if !first { writer.write_str(" | ")?; }

                first = false;
                writer.write_str(name)?;
            }
            
            let remaining = iter.remaining().bits();
            if remaining != B::Bits::EMPTY
            {
                if !first { writer.write_str(" | ")?; }

                writer.write_str("0x")?;
                remaining.write_hex(writer)?;
            }

            fmt::Result::Ok(())
        }
        
        pub fn from_str<B: Flags>(input: &str) -> Result<B, ParseError> where
        B::Bits: ParseHex
        {
            let mut parsed_flags = B::empty();
            
            if input.trim().is_empty() { return Ok(parsed_flags); }

            for flag in input.split('|')
            {
                let flag = flag.trim();
                
                if flag.is_empty() { return Err(ParseError::empty_flag()); }
                
                let parsed_flag = if let Some(flag) = flag.strip_prefix("0x")
                {
                    let bits = <B::Bits>::parse_hex(flag).map_err(|_| ParseError::invalid_hex_flag(flag))?;
                    B::from_bits_retain(bits)
                }
                
                else { B::from_name(flag).ok_or_else(|| ParseError::invalid_named_flag(flag))? };

                parsed_flags.insert(parsed_flag);
            }

            Ok(parsed_flags)
        }
        
        pub fn to_writer_truncate<B: Flags>(flags: &B, writer: impl Write) -> Result<(), fmt::Error> where
        B::Bits: WriteHex
        { to_writer(&B::from_bits_truncate(flags.bits()), writer) }
        
        pub fn from_str_truncate<B: Flags>(input: &str) -> Result<B, ParseError> where
        B::Bits: ParseHex
        { Ok(B::from_bits_truncate(from_str::<B>(input)?.bits())) }
        
        pub fn to_writer_strict<B: Flags>(flags: &B, mut writer: impl Write) -> Result<(), fmt::Error>
        {
            let mut first = true;
            let mut iter = flags.iter_names();

            for (name, _) in &mut iter
            {
                if !first { writer.write_str(" | ")?; }

                first = false;
                writer.write_str(name)?;
            }

            fmt::Result::Ok(())
        }
        
        pub fn from_str_strict<B: Flags>(input: &str) -> Result<B, ParseError>
        {
            let mut parsed_flags = B::empty();
            
            if input.trim().is_empty() { return Ok(parsed_flags); }

            for flag in input.split('|')
            {
                let flag = flag.trim();
                
                if flag.is_empty() { return Err(ParseError::empty_flag()); }
                
                if flag.starts_with("0x") { return Err(ParseError::invalid_hex_flag("unsupported hex flag value")); }

                let parsed_flag = B::from_name(flag).ok_or_else(|| ParseError::invalid_named_flag(flag))?;

                parsed_flags.insert(parsed_flag);
            }

            Ok(parsed_flags)
        }
        
        pub trait WriteHex
        {
            fn write_hex<W: fmt::Write>(&self, writer: W) -> fmt::Result;
        }
        
        pub trait ParseHex
        {
            fn parse_hex(input: &str) -> Result<Self, ParseError> where Self: Sized;
        }
        
        #[derive(Debug)] pub struct ParseError(ParseErrorKind);

        #[derive(Debug)] enum ParseErrorKind
        {
            EmptyFlag,
            InvalidNamedFlag
            {
                got: String,
            },
            InvalidHexFlag
            {
                got: String,
            },
        }

        impl ParseError
        {
            pub fn invalid_hex_flag(flag: impl fmt::Display) -> Self
            {
                let _flag = flag;
                let got = _flag.to_string();
                ParseError(ParseErrorKind::InvalidHexFlag { got })
            }
            
            pub fn invalid_named_flag(flag: impl fmt::Display) -> Self
            {
                let _flag = flag;
                let got = _flag.to_string();
                ParseError(ParseErrorKind::InvalidNamedFlag { got })
            }
            
            pub const fn empty_flag() -> Self { ParseError(ParseErrorKind::EmptyFlag) }
        }

        impl fmt::Display for ParseError
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {
                match &self.0
                {
                    ParseErrorKind::InvalidNamedFlag { got } =>
                    {
                        let _got = got;

                        write!(f, "unrecognized named flag")?;

                        #[cfg(feature = "std")]
                        {
                            write!(f, " `{}`", _got)?;
                        }
                    }

                    ParseErrorKind::InvalidHexFlag { got } =>
                    {
                        let _got = got;
                        write!(f, "invalid hex flag")?;
                        write!(f, " `{}`", _got)?;
                    }

                    ParseErrorKind::EmptyFlag =>
                    {
                        write!(f, "encountered empty flag")?;
                    }
                }

                Ok(())
            }
        }
        
        impl std::error::Error for ParseError {}
    }
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

pub mod emit
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

pub mod history
{
    /*!
    */
    use ::
    {
        collections::{ HashMap },
        error::sqlite::Error::SqliteFailure,
        io::{ Write },
        path::{ Path },
        system::
        {
            sqlite::
            {
                Connection as Conn,
            },
        },
        *,
    };
    /*
    use lineread::terminal::DefaultTerminal;
    use lineread::Interface;
    */
    // fn init_db(hfile: &str, htable: &str)
    fn create(f:&str,t:&str) -> Result<(), ()>
    {
        let path = Path::new(f);

        if !path.exists()
        {
            let _parent = match path.parent()
            {
                Some(x) => x,
                None => 
                {
                    println_stderr!(":: history init - no parent found");
                    return Ok(());
                }
            };

            let parent = match _parent.to_str() 
            {
                Some(x) => x,
                None => 
                {
                    println_stderr!(":: parent to_str is None");
                    return Ok(());
                }
            };

            match fs::create_dir_all(parent) 
            {
                Ok(_) => {}
                Err(e) => 
                {
                    println_stderr!(":: histdir create error: {}", e);
                    return Ok(());
                }
            }

            match fs::File::create(f) 
            {
                Ok(_) => 
                {
                    println!(":: created history file: {}", f);
                }
                Err(e) => 
                {
                    println_stderr!(":: history: file create failed: {}", e);
                }
            }
        }

        let c = match Conn::open(f) 
        {
            Ok(x) => x,
            Err(e) =>
            {
                println_stderr!(":: history: open db error: {}", e);
                return Ok(());
            }
        };

        let q = format!
        (
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
            t
        );

        match c.execute(&q, []) 
        {
            Ok(_) => {}
            Err(e) => println_stderr!(":: history: query error: {}", e),
        }

        Ok(())
    }
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
            ffi::{ c_char, c_int, c_uint, c_longlong, c_ulonglong, c_void, CStr },
            *,
        };
        /*
        */
        pub mod constants
        {
            /*!
            */
            use ::
            {
                ffi::{ c_char, c_int, c_uint, c_longlong, c_ulonglong, c_void, CStr },
                *,
            };
            
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

        } pub use self::constants::{ * };
        
        pub mod types
        {
            /*!
            */
            use ::
            {
                ffi::{ c_int, c_uint, c_longlong, c_ulonglong, c_void, CStr },
                *,
            };
            
            pub type sqlite_int64 = c_longlong;
            pub type sqlite_uint64 = c_ulonglong;
            pub type sqlite3_int64 = sqlite_int64;
            pub type sqlite3_uint64 = sqlite_uint64;
        } pub use self::types::{ * };
        
        pub mod enums
        {
            /*!
            */
            use ::
            {
                ffi::{ c_int, c_uint, c_longlong, c_ulonglong, c_void, CStr },
                *,
            };
            
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
        } pub use self::enums::{ * };
        
        pub mod structs
        {
            /*!
            */
            use ::
            {
                ffi::{ c_int, c_uint, c_longlong, c_ulonglong, c_void, CStr },
                *,
            };

            use super::{ * };
            
            #[repr(C)] #[derive(Debug, Copy, Clone)]
            pub struct sqlite3
            {
                _unused: [u8; 0],
            }

            #[repr(C)] #[derive(Debug, Copy, Clone)]
            pub struct sqlite3_stmt
            {
                _unused: [u8; 0],
            }
            
            #[repr(C)] #[derive(Debug, Copy, Clone)]
            pub struct sqlite3_mutex
            {
                _unused: [u8; 0],
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
        } pub use self::structs::{ * };
        
        unsafe extern "C"
        {
            pub fn sqlite3_bind_parameter_count(arg1: *mut sqlite3_stmt) -> c_int;
            pub fn sqlite3_busy_timeout(arg1: *mut sqlite3, ms: c_int) -> c_int;
            pub fn sqlite3_changes64(arg1: *mut sqlite3) -> sqlite3_int64;
            pub fn sqlite3_close(arg1: *mut sqlite3) -> c_int;
            pub fn sqlite3_column_count(pStmt: *mut sqlite3_stmt) -> c_int;
            pub fn sqlite3_db_handle(arg1: *mut sqlite3_stmt) -> *mut sqlite3;
            pub fn sqlite3_errcode(db: *mut sqlite3) -> c_int;
            pub fn sqlite3_errstr(arg1: c_int) -> *const c_char;
            pub fn sqlite3_errmsg(arg1: *mut sqlite3) -> *const c_char;
            pub fn sqlite3_error_offset(db: *mut sqlite3) -> c_int;
            pub fn sqlite3_extended_errcode(db: *mut sqlite3) -> c_int;
            pub fn sqlite3_extended_result_codes
            (
                arg1: *mut sqlite3,
                onoff: c_int,
            ) -> c_int;
            pub fn sqlite3_libversion_number() -> c_int;
            pub fn sqlite3_open_v2
            (
                filename: *const c_char,
                ppDb: *mut *mut sqlite3,
                flags: c_int,
                zVfs: *const c_char,
            ) -> c_int;
            pub fn sqlite3_malloc64(arg1: sqlite3_uint64) -> *mut c_void;
            pub fn sqlite3_mutex_alloc(arg1: c_int) -> *mut sqlite3_mutex;
            pub fn sqlite3_mutex_free(arg1: *mut sqlite3_mutex);
            pub fn sqlite3_prepare_v3( db: *mut sqlite3, zSql: *const c_char, nByte: c_int, prepFlags: c_uint, ppStmt: *mut *mut sqlite3_stmt, pzTail: *mut *const c_char ) -> c_int;
            pub fn sqlite3_reset(pStmt: *mut sqlite3_stmt) -> c_int;
            pub fn sqlite3_set_errmsg( db: *mut sqlite3, errcode: c_int, zErrMsg: *const c_char ) -> c_int;
            pub fn sqlite3_step(arg1: *mut sqlite3_stmt) -> c_int;
            pub fn sqlite3_stmt_readonly(pStmt: *mut sqlite3_stmt) -> c_int;
            pub fn sqlite3_threadsafe() -> c_int;
            pub fn sqlite3_unlock_notify
            (
                pBlocked: *mut sqlite3,
                xNotify: Option<unsafe extern "C" fn(apArg: *mut *mut c_void, nArg: c_int), >,
                pNotifyArg: *mut c_void,
            ) -> c_int;
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
        use std::convert::TryFrom;
        use collections::BTreeMap;
        use ::
        {
            cell::{ RefCell },
            error::sqlite::{ * },
            ffi::{ c_char, c_int, c_uint, c_void, CStr, CString },
            path::{ Path },
            rc::{ Rc },
            sync::{ Arc, Condvar, Mutex },
            vec::{ SmallVec },
            *,
        };

        use super::sql;
        /*
        */
        pub type Array = Rc<Vec<Value>>;

        impl ToSql for Array
        {
            #[inline] fn to_sql(&self) -> Result<ToSqlOutput<'_>> { Ok(ToSqlOutput::Array(self.clone())) }
        }

        pub type Result<T, E = Error> = result::Result<T, E>;
        
        pub trait Params
        {
            fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()>;
        }

        impl Params for [&(dyn ToSql + Send + Sync); 0]
        {
            #[inline] fn __bind_in(self, stmt: &mut Statement<'_>) -> Result<()> { stmt.ensure_parameter_count(0) }
        }

        pub trait ToSql
        {
            fn to_sql(&self) -> Result<ToSqlOutput<'_>>;
        }

        #[derive(Copy, Clone, Debug, PartialEq)]
        pub enum ValueRef<'a>
        {
            Null,
            Integer(i64),
            Real(f64),
            Text(&'a [u8]),
            Blob(&'a [u8]),
        }

        #[derive(Clone, Debug, PartialEq)]
        pub enum Value
        {
            Null,
            Integer(i64),
            Real(f64),
            Text(String),
            Blob(Vec<u8>),
        }
        
        #[non_exhaustive] #[derive(Clone, Debug, PartialEq)]
        pub enum ToSqlOutput<'a>
        {
            Borrowed(ValueRef<'a>),
            Owned(Value),
            ZeroBlob(i32),
            Arg(usize),
            Array(Array),
        }

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

        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub struct SmallCString(SmallVec<[u8; 16]>);

        impl SmallCString
        {
            #[inline] pub fn as_bytes_without_nul(&self) -> &[u8]
            {
                &self.0[..self.len()]
            }

            #[inline] pub fn as_str(&self) -> &str
            {
                unsafe
                {
                    ::str::from_utf8_unchecked(self.as_bytes_without_nul())
                }
            }
            
            #[inline] pub fn len(&self) -> usize
            {
                debug_assert_ne!(self.0.len(), 0);
                self.0.len() - 1
            }
        }
        
        impl ::fmt::Debug for SmallCString
        {
            fn fmt(&self, f: &mut ::fmt::Formatter<'_>) -> ::fmt::Result { f.debug_tuple("SmallCString").field(&self.as_str()).finish() }
        }
        
        #[derive(Default, Clone, Debug)]
        pub struct ParamIndexCache(RefCell<BTreeMap<SmallCString, usize>>);
        
        #[derive(Debug)]
        pub struct RawStatement
        {
            ptr: *mut sql::sqlite3_stmt,
            cache: ParamIndexCache
        }
        
        impl RawStatement
        {
            #[inline] pub fn bind_parameter_count(&self) -> usize { unsafe { sql::sqlite3_bind_parameter_count(self.ptr) as usize } }
            
            #[inline] pub fn column_count(&self) -> usize { unsafe { sql::sqlite3_column_count(self.ptr) as usize } }

            #[inline] pub fn is_null(&self) -> bool { self.ptr.is_null() }

            #[inline] pub unsafe fn new(s: *mut sql::sqlite3_stmt) -> Self
            {
                Self
                {
                    ptr: s,
                    cache: ParamIndexCache::default(),
                }
            }

            #[inline] pub fn readonly(&self) -> bool { unsafe { sql::sqlite3_stmt_readonly(self.ptr) != 0 } }

            #[inline] pub fn reset(&self) -> c_int { unsafe { sql::sqlite3_reset(self.ptr) } }

            pub fn step(&self) -> c_int
            {
                unsafe
                {
                    let mut db = ptr::null_mut::<sql::sqlite3>();
                    loop
                    {                    
                        let mut rc = sql::sqlite3_step(self.ptr);
                        if (rc & 0xff) != sql::SQLITE_LOCKED {
                            break rc;
                        }
                        if db.is_null() {
                            db = sql::sqlite3_db_handle(self.ptr);
                        }
                        if !database_is_locked(db, rc) {
                            break rc;
                        }
                        rc = wait_for_unlock_notify(db);
                        if rc != sql::SQLITE_OK {
                            break rc;
                        }
                        self.reset();    
                    }
                }
            }
        }

        pub struct Statement<'conn>
        {
            pub conn: &'conn Connection,
            pub stmt: RawStatement,
        }

        impl Statement<'_>
        {
            #[inline] pub fn check_update(&self) -> Result<()>
            {
                if self.column_count() > 0 && self.stmt.readonly() { return Err(Error::ExecuteReturnedResults); }
                Ok(())
            }

            #[inline] pub fn column_count(&self) -> usize { self.stmt.column_count() }

            #[inline] pub fn ensure_parameter_count(&self, n: usize) -> Result<()>
            {
                let count = self.parameter_count();
                if count != n { Err(Error::InvalidParameterCount(n, count)) } else { Ok(()) }
            }

            #[inline] pub fn execute<P: Params>(&mut self, params: P) -> Result<usize>
            {
                params.__bind_in(self)?;
                self.execute_with_bound_parameters()
            }

            #[inline] fn execute_with_bound_parameters(&mut self) -> Result<usize>
            {
                self.check_update()?;
                let r = self.stmt.step();
                let rr = self.stmt.reset();
                match r
                {
                    sql::SQLITE_DONE => match rr
                    {
                        sql::SQLITE_OK => Ok(self.conn.changes() as usize),
                        _ => Err(self.conn.decode_result(rr).unwrap_err()),
                    },
                    sql::SQLITE_ROW => Err(Error::ExecuteReturnedResults),
                    _ => Err(self.conn.decode_result(r).unwrap_err()),
                }
            }

            #[inline] pub fn new(conn: &Connection, stmt: RawStatement) -> Statement<'_> { Statement { conn, stmt } }

            #[inline] pub fn parameter_count(&self) -> usize { self.stmt.bind_parameter_count() }
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

        impl InnerConnection
        {
            #[inline] pub fn changes(&self) -> u64 { unsafe { sql::sqlite3_changes64(self.db()) as u64 } }

            #[inline] pub fn db(&self) -> *mut sql::sqlite3 { self.db }

            #[inline] pub fn decode_result(&self, code: c_int) -> Result<()> { unsafe { decode_result_raw(self.db(), code) } }
            
            #[inline] pub unsafe fn new(db: *mut sql::sqlite3, owned: bool) -> Self
            {
                Self
                {
                    db,
                    interrupt_lock: Arc::new(Mutex::new(if owned { db } else { ptr::null_mut() })),
                    commit_hook: None,
                    rollback_hook: None,
                    update_hook: None,
                    progress_handler: None,
                    authorizer: None,
                    owned,
                }
            }

            pub fn open_with_flags( c_path: &CStr, mut flags: OpenFlags, vfs: Option<&CStr> ) -> Result<Self>
            {
                unsafe
                {
                    ensure_safe_sqlite_threading_mode()?;

                    let z_vfs = match vfs
                    {
                        Some(c_vfs) => c_vfs.as_ptr(),
                        None => ptr::null(),
                    };
                    
                    let exrescode = if version_number() >= 3_037_000
                    {
                        flags |= OpenFlags::SQLITE_OPEN_EXRESCODE;
                        true
                    }
                    else { false };


                    let mut db: *mut sql::sqlite3 = ptr::null_mut();
                    let r = sql::sqlite3_open_v2(c_path.as_ptr(), &mut db, flags.bits(), z_vfs);
                    if r != sql::SQLITE_OK {
                        let e = if db.is_null() {
                            err!(r, "{}", c_path.to_string_lossy())
                        } else {
                            let mut e = error_from_handle(db, r);
                            if let Error::SqliteFailure(
                                sql::Error {
                                    code: sql::ErrorCode::CannotOpen,
                                    ..
                                },
                                Some(msg),
                            ) = e
                            {
                                e = err!(r, "{msg}: {}", c_path.to_string_lossy());
                            }
                            sql::sqlite3_close(db);
                            e
                        };

                        return Err(e);
                    }
                    
                    if !exrescode {
                        sql::sqlite3_extended_result_codes(db, 1);
                    }

                    let r = sql::sqlite3_busy_timeout(db, 5000);
                    if r != sql::SQLITE_OK {
                        let e = error_from_handle(db, r);
                        sql::sqlite3_close(db);
                        return Err(e);
                    }

                    Ok(Self::new(db, true))
                }
            }
            
            pub fn prepare<'a>( &mut self, conn: &'a Connection, sql: &str, flags: PrepFlags ) -> Result<(Statement<'a>, usize)>
            {
                let mut c_stmt: *mut sql::sqlite3_stmt = ptr::null_mut();
                let Ok(len) = c_int::try_from(sql.len()) else { return Err(err!(sql::SQLITE_TOOBIG)); };
                let c_sql = sql.as_bytes().as_ptr().cast::<c_char>();
                let mut c_tail: *const c_char = ptr::null();
                let r = unsafe
                {
                    let mut rc;
                    loop
                    {
                        rc = sql::sqlite3_prepare_v3(
                            self.db(),
                            c_sql,
                            len,
                            flags.bits(),
                            &mut c_stmt,
                            &mut c_tail,
                        );
                        if !database_is_locked(self.db, rc) {
                            break;
                        }
                        rc = wait_for_unlock_notify(self.db);
                        if rc != sql::SQLITE_OK {
                            break;
                        }
                    }
                    rc
                };
                
                if r != sql::SQLITE_OK { return Err(unsafe { error_with_offset(self.db, r, sql) }); }
                
                let tail = if c_tail.is_null() {
                    0
                } else {
                    let n = (c_tail as isize) - (c_sql as isize);
                    if n <= 0 || n >= len as isize {
                        0
                    } else {
                        n as usize
                    }
                };
                Ok((
                    Statement::new(conn, unsafe { RawStatement::new(c_stmt) }),
                    tail,
                ))
            }
        }

        #[non_exhaustive] #[derive( Copy, Clone )]
        pub enum TransactionBehavior
        {
            Deferred,
            Immediate,
            Exclusive
        }
        
        bitflags!
        {
            #[repr(C)] #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]            
            pub struct OpenFlags:c_int
            {
                const SQLITE_OPEN_READ_ONLY = sql::SQLITE_OPEN_READONLY;
                const SQLITE_OPEN_READ_WRITE = sql::SQLITE_OPEN_READWRITE;
                const SQLITE_OPEN_CREATE = sql::SQLITE_OPEN_CREATE;
                const SQLITE_OPEN_URI = sql::SQLITE_OPEN_URI;
                const SQLITE_OPEN_MEMORY = sql::SQLITE_OPEN_MEMORY;
                const SQLITE_OPEN_NO_MUTEX = sql::SQLITE_OPEN_NOMUTEX;
                const SQLITE_OPEN_FULL_MUTEX = sql::SQLITE_OPEN_FULLMUTEX;
                const SQLITE_OPEN_SHARED_CACHE = 0x0002_0000;
                const SQLITE_OPEN_PRIVATE_CACHE = 0x0004_0000;
                const SQLITE_OPEN_NOFOLLOW = 0x0100_0000;
                const SQLITE_OPEN_EXRESCODE = 0x0200_0000;
            }
        }

        impl Default for OpenFlags
        {
            #[inline] fn default() -> Self
            {
                Self::SQLITE_OPEN_READ_WRITE
                | Self::SQLITE_OPEN_CREATE
                | Self::SQLITE_OPEN_NO_MUTEX
                | Self::SQLITE_OPEN_URI
            }
        }

        bitflags!
        {
            #[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
            #[repr(C)] pub struct PrepFlags: c_uint
            {
                const SQLITE_PREPARE_PERSISTENT = 0x01;
                const SQLITE_PREPARE_NO_VTAB = 0x04;
                const SQLITE_PREPARE_DONT_LOG = 0x10;
            }
        }
        
        pub struct Connection
        {
            db:RefCell<InnerConnection>,
            transaction_behavior:TransactionBehavior,
        }
        
        impl Connection
        {
            #[inline] pub fn changes(&self) -> u64 { self.db.borrow().changes() }

            #[inline] fn decode_result(&self, code: c_int) -> Result<()> { self.db.borrow().decode_result(code) }

            #[inline] pub fn execute<P: Params>(&self, sql: &str, params: P) -> Result<usize>
            { self.prepare(sql).and_then(|mut stmt| stmt.execute(params)) }

            #[inline] pub fn open<P: AsRef<Path>>(path: P) -> Result<Self>
            {
                let flags = OpenFlags::default();
                Self::open_with_flags(path, flags)
            }

            #[inline] pub fn open_with_flags<P: AsRef<Path>>(path: P, flags: OpenFlags) -> Result<Self>
            {
                let c_path = path_to_cstring(path.as_ref())?;
                InnerConnection::open_with_flags(&c_path, flags, None).map(|db| Self {
                    db: RefCell::new(db),
                    #[cfg(feature = "cache")]
                    cache: StatementCache::with_capacity(STATEMENT_CACHE_DEFAULT_CAPACITY),
                    transaction_behavior: TransactionBehavior::Deferred,
                })
            }

            #[inline] pub fn prepare(&self, sql: &str) -> Result<Statement<'_>> { self.prepare_with_flags(sql, PrepFlags::default()) }
            
            #[inline] pub fn prepare_with_flags(&self, sql: &str, flags: PrepFlags) -> Result<Statement<'_>>
            {
                let (stmt, tail) = self.db.borrow_mut().prepare(self, sql, flags)?;
                
                if tail != 0 && !self.prepare(&sql[tail..])?.stmt.is_null() { Err(Error::MultipleStatement) }
                else { Ok(stmt) }
            }
        }
        
        pub struct UnlockNotification
        {
            cond:Condvar,
            mutex:Mutex<bool>,
        }

        impl UnlockNotification
        {
            fn new() -> Self
            {
                Self
                {
                    cond: Condvar::new(),
                    mutex: Mutex::new(false),
                }
            }

            fn fired(&self)
            {
                let mut flag = unpoison(self.mutex.lock());
                *flag = true;
                self.cond.notify_one();
            }

            fn wait(&self)
            {
                let mut fired = unpoison(self.mutex.lock());
                
                while !*fired
                {
                    fired = unpoison(self.cond.wait(fired));
                }
            }
        }

        pub fn ensure_safe_sqlite_threading_mode() -> Result<()> 
        {
            if unsafe { sql::sqlite3_threadsafe() == 0 } { return Err(Error::SqliteSingleThreadedMode); }

            const SQLITE_SINGLETHREADED_MUTEX_MAGIC: usize = 8;
            let is_singlethreaded = unsafe
            {
                let mutex_ptr = sql::sqlite3_mutex_alloc(0);
                let is_singlethreaded = mutex_ptr as usize == SQLITE_SINGLETHREADED_MUTEX_MAGIC;
                sql::sqlite3_mutex_free(mutex_ptr);
                is_singlethreaded
            };

            if is_singlethreaded { Err(Error::SqliteSingleThreadedMode) } else { Ok(()) }
        }
        
        #[inline] fn unpoison<T>(r: Result<T, ::sync::PoisonError<T>>) -> T
        {
            r.unwrap_or_else( ::sync::PoisonError::into_inner)
        }

        pub unsafe fn database_is_locked(db: *mut sql::sqlite3, rc: c_int) -> bool
        {
            rc == sql::SQLITE_LOCKED_SHAREDCACHE
            || (rc & 0xFF) == sql::SQLITE_LOCKED
            && sql::sqlite3_extended_errcode(db) == sql::SQLITE_LOCKED_SHAREDCACHE
        }

        pub fn path_to_cstring(p: &Path) -> Result<CString>
        {
            use ::os::unix::ffi::OsStrExt;
            Ok(CString::new(p.as_os_str().as_bytes())?)
        }

        unsafe extern "C" fn unlock_notify_cb(ap_arg: *mut *mut c_void, n_arg: c_int)
        {
            use ::slice::from_raw_parts;
            let args = from_raw_parts(ap_arg as *const &UnlockNotification, n_arg as usize);
            
            for un in args
            {
                drop( ::panic::catch_unwind(::panic::AssertUnwindSafe(|| un.fired())));
            }
        }

        #[must_use] #[inline] pub fn version_number() -> i32 { unsafe { sql::sqlite3_libversion_number() } }

        pub unsafe fn wait_for_unlock_notify(db: *mut sql::sqlite3) -> c_int
        {
            let un = UnlockNotification::new();            
            let rc = sql::sqlite3_unlock_notify( db, Some(unlock_notify_cb), &un as *const UnlockNotification as *mut c_void );

            debug_assert!( rc == sql::SQLITE_LOCKED || rc == sql::SQLITE_LOCKED_SHAREDCACHE || rc == sql::SQLITE_OK );

            if rc == sql::SQLITE_OK { un.wait(); }

            rc
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
    /*!
    */
    pub use std::vec::{ * };
    use ::
    {
        alloc::{Layout, LayoutErr},
        boxed::Box,
        vec::Vec,
        borrow::{Borrow, BorrowMut},
        hash::{Hash, Hasher},
        hint::unreachable_unchecked,
        iter::{repeat, FromIterator, FusedIterator, IntoIterator},
        mem::MaybeUninit,
        ops::{self, Range, RangeBounds},
        ptr::{self, NonNull},
        slice::{self, SliceIndex},
        *,
    };
    /*
    */
    #[deprecated]
    pub trait ExtendFromSlice<T>
    {
        fn extend_from_slice(&mut self, other: &[T]);
    }

    #[allow(deprecated)]
    impl<T: Clone> ExtendFromSlice<T> for Vec<T>
    {
        fn extend_from_slice(&mut self, other: &[T]) { Vec::extend_from_slice(self, other) }
    }
    
    #[derive(Debug)] pub enum CollectionAllocErr
    {
        CapacityOverflow,
        AllocErr
        {
            layout: Layout,
        },
    }

    impl fmt::Display for CollectionAllocErr
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "Allocation error: {:?}", self) }
    }

    #[allow(deprecated)]
    impl From<LayoutErr> for CollectionAllocErr
    {
        fn from(_: LayoutErr) -> Self { CollectionAllocErr::CapacityOverflow }
    }

    fn infallible<T>(result: Result<T, CollectionAllocErr>) -> T
    {
        match result
        {
            Ok(x) => x,
            Err(CollectionAllocErr::CapacityOverflow) => panic!("capacity overflow"),
            Err(CollectionAllocErr::AllocErr { layout }) => ::alloc::handle_alloc_error(layout),
        }
    }
    
    fn layout_array<T>(n: usize) -> Result<Layout, CollectionAllocErr>
    {
        let size = mem::size_of::<T>()
        .checked_mul(n)
        .ok_or(CollectionAllocErr::CapacityOverflow)?;
        let align = mem::align_of::<T>();
        Layout::from_size_align(size, align).map_err(|_| CollectionAllocErr::CapacityOverflow)
    }

    unsafe fn deallocate<T>(ptr: *mut T, capacity: usize)
    {
        let layout = layout_array::<T>(capacity).unwrap();
        alloc::dealloc(ptr as *mut u8, layout)
    }
    
    pub struct Drain<'a, T: 'a + Array>
    {
        tail_start: usize,
        tail_len: usize,
        iter: slice::Iter<'a, T::Item>,
        vec: NonNull<SmallVec<T>>,
    }

    impl<'a, T: 'a + Array> fmt::Debug for Drain<'a, T> where
    T::Item: fmt::Debug
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { f.debug_tuple("Drain").field(&self.iter.as_slice()).finish() }
    }

    unsafe impl<'a, T: Sync + Array> Sync for Drain<'a, T> {}
    unsafe impl<'a, T: Send + Array> Send for Drain<'a, T> {}

    impl<'a, T: 'a + Array> Iterator for Drain<'a, T>
    {
        type Item = T::Item;

        #[inline] fn next(&mut self) -> Option<T::Item>
        {
            self.iter
            .next()
            .map(|reference| unsafe { ptr::read(reference) })
        }

        #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.iter.size_hint() }
    }

    impl<'a, T: 'a + Array> DoubleEndedIterator for Drain<'a, T>
    {
        #[inline] fn next_back(&mut self) -> Option<T::Item>
        {
            self.iter
            .next_back()
            .map(|reference| unsafe { ptr::read(reference) })
        }
    }

    impl<'a, T: Array> ExactSizeIterator for Drain<'a, T>
    {
        #[inline] fn len(&self) -> usize { self.iter.len() }
    }

    impl<'a, T: Array> FusedIterator for Drain<'a, T> {}

    impl<'a, T: 'a + Array> Drop for Drain<'a, T>
    {
        fn drop(&mut self)
        {
            unsafe
            {
                self.for_each(drop);
                if self.tail_len > 0
                {
                    let source_vec = self.vec.as_mut();
                    let start = source_vec.len();
                    let tail = self.tail_start;
                    if tail != start
                    {
                        let src = source_vec.as_ptr().add(tail);
                        let dst = source_vec.as_mut_ptr().add(start);
                        ptr::copy(src, dst, self.tail_len);
                    }
                    source_vec.set_len(start + self.tail_len);
                }
            }
        }
    }
    
    union SmallVecData<A: Array>
    {
        inline: ::mem::ManuallyDrop<MaybeUninit<A>>,
        heap: (*mut A::Item, usize),
    }
    
    impl<A: Array> SmallVecData<A>
    {
        #[inline] unsafe fn inline(&self) -> *const A::Item { self.inline.as_ptr() as *const A::Item }

        #[inline] unsafe fn inline_mut(&mut self) -> *mut A::Item { self.inline.as_mut_ptr() as *mut A::Item }

        #[inline] fn from_inline(inline: MaybeUninit<A>) -> SmallVecData<A>
        {
            SmallVecData
            {
                inline: ::mem::ManuallyDrop::new(inline),
            }
        }

        #[inline] unsafe fn into_inline(self) -> MaybeUninit<A> { ::mem::ManuallyDrop::into_inner(self.inline) }

        #[inline] unsafe fn heap(&self) -> (*mut A::Item, usize) { self.heap }

        #[inline] unsafe fn heap_mut(&mut self) -> &mut (*mut A::Item, usize) { &mut self.heap }

        #[inline] fn from_heap(ptr: *mut A::Item, len: usize) -> SmallVecData<A> { SmallVecData { heap: (ptr, len) } }
    }
    
    unsafe impl<A: Array + Send> Send for SmallVecData<A> {}
    unsafe impl<A: Array + Sync> Sync for SmallVecData<A> {}
    
    pub struct SmallVec<A: Array>
    {
        capacity: usize,
        data: SmallVecData<A>,
    }

    impl<A: Array> SmallVec<A>
    {
        #[inline] pub fn new() -> SmallVec<A>
        {
            assert!( mem::size_of::<A>() == A::size() * mem::size_of::<A::Item>() && mem::align_of::<A>() >= mem::align_of::<A::Item>() );

            SmallVec
            {
                capacity: 0,
                data: SmallVecData::from_inline(MaybeUninit::uninit()),
            }
        }
        
        #[inline] pub fn with_capacity(n: usize) -> Self
        {
            let mut v = SmallVec::new();
            v.reserve_exact(n);
            v
        }
        
        #[inline] pub fn from_vec(mut vec: Vec<A::Item>) -> SmallVec<A>
        {
            unsafe
            {
                if vec.capacity() <= Self::inline_capacity()
                {
                    let mut data = SmallVecData::<A>::from_inline(MaybeUninit::uninit());
                    let len = vec.len();
                    vec.set_len(0);
                    ptr::copy_nonoverlapping(vec.as_ptr(), data.inline_mut(), len);

                    SmallVec
                    {
                        capacity: len,
                        data,
                    }
                }
                
                else
                {
                    let (ptr, cap, len) = (vec.as_mut_ptr(), vec.capacity(), vec.len());
                    mem::forget(vec);

                    SmallVec
                    {
                        capacity: cap,
                        data: SmallVecData::from_heap(ptr, len),
                    }
                }
            }
        }
        
        #[inline] pub fn from_buf(buf: A) -> SmallVec<A>
        {
            SmallVec
            {
                capacity: A::size(),
                data: SmallVecData::from_inline(MaybeUninit::new(buf)),
            }
        }
        
        #[inline] pub fn from_buf_and_len(buf: A, len: usize) -> SmallVec<A>
        {
            assert!(len <= A::size());
            unsafe { SmallVec::from_buf_and_len_unchecked(MaybeUninit::new(buf), len) }
        }
        
        #[inline] pub unsafe fn from_buf_and_len_unchecked(buf: MaybeUninit<A>, len: usize) -> SmallVec<A>
        {
            SmallVec
            {
                capacity: len,
                data: SmallVecData::from_inline(buf),
            }
        }
        
        pub unsafe fn set_len(&mut self, new_len: usize)
        {
            let (_, len_ptr, _) = self.triple_mut();
            *len_ptr = new_len;
        }
        
        #[inline] fn inline_capacity() -> usize { if mem::size_of::<A::Item>() > 0 { A::size() } else { ::usize::MAX } }

        #[inline] pub fn inline_size(&self) -> usize { Self::inline_capacity() }
        
        #[inline] pub fn len(&self) -> usize { self.triple().1 }
        
        #[inline] pub fn is_empty(&self) -> bool {  self.len() == 0 }
        
        #[inline] pub fn capacity(&self) -> usize { self.triple().2 }
        
        #[inline] fn triple(&self) -> (*const A::Item, usize, usize)
        {
            unsafe
            {
                if self.spilled()
                {
                    let (ptr, len) = self.data.heap();
                    (ptr, len, self.capacity)
                }
                else { (self.data.inline(), self.capacity, Self::inline_capacity()) }
            }
        }
        
        #[inline] fn triple_mut(&mut self) -> (*mut A::Item, &mut usize, usize)
        {
            unsafe
            {
                if self.spilled()
                {
                    let &mut (ptr, ref mut len_ptr) = self.data.heap_mut();
                    (ptr, len_ptr, self.capacity)
                }
                else
                {
                    (
                        self.data.inline_mut(),
                        &mut self.capacity,
                        Self::inline_capacity(),
                    )
                }
            }
        }
        
        #[inline] pub fn spilled(&self) -> bool { self.capacity > Self::inline_capacity() }
        
        pub fn drain<R>(&mut self, range: R) -> Drain<'_, A> where
        R: RangeBounds<usize>
        {
            unsafe
            {
                use ::ops::Bound::*;

                let len = self.len();
                let start = match range.start_bound()
                {
                    Included(&n) => n,
                    Excluded(&n) => n + 1,
                    Unbounded => 0,
                };

                let end = match range.end_bound()
                {
                    Included(&n) => n + 1,
                    Excluded(&n) => n,
                    Unbounded => len,
                };

                assert!(start <= end);
                assert!(end <= len);
                self.set_len(start);

                let range_slice = slice::from_raw_parts_mut(self.as_mut_ptr().add(start), end - start);

                Drain
                {
                    tail_start: end,
                    tail_len: len - end,
                    iter: range_slice.iter(),
                    vec: NonNull::from(self),
                }
            }
        }
        
        #[inline] pub fn push(&mut self, value: A::Item)
        {
            unsafe
            {
                let (mut ptr, mut len, cap) = self.triple_mut();
                if *len == cap
                {
                    self.reserve(1);
                    let &mut (heap_ptr, ref mut heap_len) = self.data.heap_mut();
                    ptr = heap_ptr;
                    len = heap_len;
                }

                ptr::write(ptr.add(*len), value);
                *len += 1;
            }
        }
        
        #[inline] pub fn pop(&mut self) -> Option<A::Item>
        {
            unsafe
            {
                let (ptr, len_ptr, _) = self.triple_mut();

                if *len_ptr == 0 { return None; }

                let last_index = *len_ptr - 1;
                *len_ptr = last_index;
                Some(ptr::read(ptr.add(last_index)))
            }
        }
        
        pub fn append<B>(&mut self, other: &mut SmallVec<B>) where
        B: Array<Item = A::Item>
        { self.extend(other.drain(..)) }
        
        pub fn grow(&mut self, new_cap: usize) { infallible(self.try_grow(new_cap)) }
        
        pub fn try_grow(&mut self, new_cap: usize) -> Result<(), CollectionAllocErr>
        {
            unsafe
            {
                let (ptr, &mut len, cap) = self.triple_mut();
                let unspilled = !self.spilled();
                assert!(new_cap >= len);

                if new_cap <= self.inline_size()
                {
                    if unspilled { return Ok(()); }

                    self.data = SmallVecData::from_inline(MaybeUninit::uninit());
                    ptr::copy_nonoverlapping(ptr, self.data.inline_mut(), len);
                    self.capacity = len;
                    deallocate(ptr, cap);
                }

                else if new_cap != cap
                {
                    let layout = layout_array::<A::Item>(new_cap)?;
                    debug_assert!(layout.size() > 0);
                    let new_alloc;
                    if unspilled
                    {
                        new_alloc = NonNull::new(::alloc::alloc(layout))
                        .ok_or(CollectionAllocErr::AllocErr { layout })?
                        .cast()
                        .as_ptr();
                        ptr::copy_nonoverlapping(ptr, new_alloc, len);
                    }
                    
                    else
                    {
                        let old_layout = layout_array::<A::Item>(cap)?;
                        let new_ptr = ::alloc::realloc(ptr as *mut u8, old_layout, layout.size());
                        new_alloc = NonNull::new(new_ptr)
                        .ok_or(CollectionAllocErr::AllocErr { layout })?
                        .cast()
                        .as_ptr();
                    }

                    self.data = SmallVecData::from_heap(new_alloc, len);
                    self.capacity = new_cap;
                }

                Ok(())
            }
        }
        
        #[inline] pub fn reserve(&mut self, additional: usize) { infallible(self.try_reserve(additional)) }

        pub fn try_reserve(&mut self, additional: usize) -> Result<(), CollectionAllocErr>
        {
            let (_, &mut len, cap) = self.triple_mut();
            
            if cap - len >= additional { return Ok(()); }

            let new_cap = len
            .checked_add(additional)
            .and_then(usize::checked_next_power_of_two)
            .ok_or(CollectionAllocErr::CapacityOverflow)?;

            self.try_grow(new_cap)
        }

        pub fn reserve_exact(&mut self, additional: usize) { infallible(self.try_reserve_exact(additional)) }
        
        pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), CollectionAllocErr>
        {
            let (_, &mut len, cap) = self.triple_mut();

            if cap - len >= additional { return Ok(()); }

            let new_cap = len
            .checked_add(additional)
            .ok_or(CollectionAllocErr::CapacityOverflow)?;
            
            self.try_grow(new_cap)
        }
        
        pub fn shrink_to_fit(&mut self)
        {
            unsafe
            {
                if !self.spilled() { return; }

                let len = self.len();
                if self.inline_size() >= len
                {
                    let (ptr, len) = self.data.heap();
                    self.data = SmallVecData::from_inline(MaybeUninit::uninit());
                    ptr::copy_nonoverlapping(ptr, self.data.inline_mut(), len);
                    deallocate(ptr, self.capacity);
                    self.capacity = len;
                }

                else if self.capacity() > len { self.grow(len); }

            }
        }
        
        pub fn truncate(&mut self, len: usize)
        {
            unsafe
            {
                let (ptr, len_ptr, _) = self.triple_mut();
                
                while len < *len_ptr
                {
                    let last_index = *len_ptr - 1;
                    *len_ptr = last_index;
                    ptr::drop_in_place(ptr.add(last_index));
                }
            }
        }

        pub fn as_slice(&self) -> &[A::Item] { self }

        pub fn as_mut_slice(&mut self) -> &mut [A::Item] { self }

        #[inline] pub fn swap_remove(&mut self, index: usize) -> A::Item
        {
            let len = self.len();
            self.swap(len - 1, index);
            self.pop().unwrap_or_else(|| unsafe { unreachable_unchecked() })
        }
        
        #[inline] pub fn clear(&mut self) { self.truncate(0); }
        
        pub fn remove(&mut self, index: usize) -> A::Item
        {
            unsafe
            {
                let (mut ptr, len_ptr, _) = self.triple_mut();
                let len = *len_ptr;
                assert!(index < len);
                *len_ptr = len - 1;
                ptr = ptr.add(index);
                let item = ptr::read(ptr);
                ptr::copy(ptr.add(1), ptr, len - index - 1);
                item
            }
        }

        pub fn insert(&mut self, index: usize, element: A::Item)
        {
            unsafe
            {
                self.reserve(1);
                let (mut ptr, len_ptr, _) = self.triple_mut();
                let len = *len_ptr;
                assert!(index <= len);
                *len_ptr = len + 1;
                ptr = ptr.add(index);
                ptr::copy(ptr, ptr.add(1), len - index);
                ptr::write(ptr, element);
            }
        }
        
        pub fn insert_many<I: IntoIterator<Item = A::Item>>(&mut self, index: usize, iterable: I)
        {
            unsafe
            {
                let mut iter = iterable.into_iter();

                if index == self.len() { return self.extend(iter); }

                let (lower_size_bound, _) = iter.size_hint();
                assert!(lower_size_bound <= ::isize::MAX as usize);
                assert!(index + lower_size_bound >= index);

                let mut num_added = 0;
                let old_len = self.len();
                assert!(index <= old_len);
                
                self.reserve(lower_size_bound);
                let start = self.as_mut_ptr();
                let ptr = start.add(index);
                
                ptr::copy(ptr, ptr.add(lower_size_bound), old_len - index);
                
                self.set_len(0);
                let mut guard = DropOnPanic
                {
                    start,
                    skip: index..(index + lower_size_bound),
                    len: old_len + lower_size_bound,
                };

                while num_added < lower_size_bound
                {
                    let element = match iter.next()
                    {
                        Some(x) => x,
                        None => break,
                    };

                    let cur = ptr.add(num_added);
                    ptr::write(cur, element);
                    guard.skip.start += 1;
                    num_added += 1;
                }

                if num_added < lower_size_bound
                {
                    ptr::copy
                    (
                        ptr.add(lower_size_bound),
                        ptr.add(num_added),
                        old_len - index,
                    );
                }
                
                self.set_len(old_len + num_added);
                mem::forget(guard);
                
                for element in iter
                {
                    self.insert(index + num_added, element);
                    num_added += 1;
                }

                struct DropOnPanic<T>
                {
                    start: *mut T,
                    skip: Range<usize>,
                    len: usize,
                }

                impl<T> Drop for DropOnPanic<T>
                {
                    fn drop(&mut self)
                    {
                        unsafe
                        {
                            for i in 0..self.len
                            {
                                if !self.skip.contains(&i) { ptr::drop_in_place(self.start.add(i)); }
                            }
                        }
                    }
                }
            }
        }
        
        pub fn into_vec(self) -> Vec<A::Item>
        {
            unsafe
            {
                if self.spilled()
                {
                    let (ptr, len) = self.data.heap();
                    let v = Vec::from_raw_parts(ptr, len, self.capacity);
                    mem::forget(self);
                    v
                }
                else { self.into_iter().collect() }
            }
        }
        
        pub fn into_boxed_slice(self) -> Box<[A::Item]> { self.into_vec().into_boxed_slice() }
        
        pub fn into_inner(self) -> Result<A, Self>
        {
            unsafe
            {
                if self.spilled() || self.len() != A::size() { Err(self) }            
                else
                {
                    let data = ptr::read(&self.data);
                    mem::forget(self);
                    Ok(data.into_inline().assume_init())
                }
            }
        }
        
        pub fn retain<F: FnMut(&mut A::Item) -> bool>(&mut self, mut f: F)
        {
            let mut del = 0;
            let len = self.len();
            
            for i in 0..len
            {
                if !f(&mut self[i]) { del += 1; }
                else if del > 0 { self.swap(i - del, i); }
            }

            self.truncate(len - del);
        }
        
        pub fn dedup(&mut self) where
        A::Item: PartialEq<A::Item>
        { self.dedup_by(|a, b| a == b); }
        
        pub fn dedup_by<F>(&mut self, mut same_bucket: F) where
        F: FnMut(&mut A::Item, &mut A::Item) -> bool
        {
            unsafe
            {
                let len = self.len();

                if len <= 1 { return; }

                let ptr = self.as_mut_ptr();
                let mut w: usize = 1;
                
                for r in 1..len
                {
                    let p_r = ptr.add(r);
                    let p_wm1 = ptr.add(w - 1);

                    if !same_bucket(&mut *p_r, &mut *p_wm1)
                    {
                        if r != w
                        {
                            let p_w = p_wm1.add(1);
                            mem::swap(&mut *p_r, &mut *p_w);
                        }

                        w += 1;
                    }
                }

                self.truncate(w);
            }
        }
        
        pub fn dedup_by_key<F, K>(&mut self, mut key: F) where
        F: FnMut(&mut A::Item) -> K,
        K: PartialEq<K>
        { self.dedup_by(|a, b| key(a) == key(b)); }
        
        pub fn resize_with<F>(&mut self, new_len: usize, f: F) where
        F: FnMut() -> A::Item
        {
            let old_len = self.len();
            
            if old_len < new_len
            {
                let mut f = f;
                let additional = new_len - old_len;
                self.reserve(additional);
                
                for _ in 0..additional
                {
                    self.push(f());
                }
            }
            else if old_len > new_len { self.truncate(new_len); }
        }
        
        #[inline] pub unsafe fn from_raw_parts(ptr: *mut A::Item, length: usize, capacity: usize) -> SmallVec<A>
        {
            assert!(capacity > Self::inline_capacity());
            SmallVec
            {
                capacity,
                data: SmallVecData::from_heap(ptr, length),
            }
        }
        
        pub fn as_ptr(&self) -> *const A::Item { self.triple().0 }
        
        pub fn as_mut_ptr(&mut self) -> *mut A::Item { self.triple_mut().0 }
    }

    impl<A: Array> SmallVec<A> where
    A::Item: Copy
    {

        pub fn from_slice(slice: &[A::Item]) -> Self
        {
            unsafe
            {
                let len = slice.len();
                
                if len <= Self::inline_capacity()
                {
                    SmallVec
                    {
                        capacity: len,
                        data: SmallVecData::from_inline(
                        {
                            let mut data: MaybeUninit<A> = MaybeUninit::uninit();
                            ptr::copy_nonoverlapping
                            (
                                slice.as_ptr(),
                                data.as_mut_ptr() as *mut A::Item,
                                len,
                            );
                            data
                        }),
                    }
                }
                
                else
                {
                    let mut b = slice.to_vec();
                    let (ptr, cap) = (b.as_mut_ptr(), b.capacity());
                    mem::forget(b);
                    SmallVec
                    {
                        capacity: cap,
                        data: SmallVecData::from_heap(ptr, len),
                    }
                }
            }
        }
        
        pub fn insert_from_slice(&mut self, index: usize, slice: &[A::Item])
        {
            unsafe
            {
                self.reserve(slice.len());
                let len = self.len();
                assert!(index <= len);
                let slice_ptr = slice.as_ptr();
                let ptr = self.as_mut_ptr().add(index);
                ptr::copy(ptr, ptr.add(slice.len()), len - index);
                ptr::copy_nonoverlapping(slice_ptr, ptr, slice.len());
                self.set_len(len + slice.len());
            }
        }

        #[inline] pub fn extend_from_slice(&mut self, slice: &[A::Item])
        {
            let len = self.len();
            self.insert_from_slice(len, slice);
        }
    }

    impl<A: Array> SmallVec<A> where
    A::Item: Clone
    {
        pub fn resize(&mut self, len: usize, value: A::Item)
        {
            let old_len = self.len();

            if len > old_len { self.extend(repeat(value).take(len - old_len)); } else { self.truncate(len); }
        }
        
        pub fn from_elem(elem: A::Item, n: usize) -> Self
        {
            if n > Self::inline_capacity() { vec![elem; n].into() }
            else
            {
                let mut v = SmallVec::<A>::new();
                unsafe
                {
                    let (ptr, len_ptr, _) = v.triple_mut();
                    let mut local_len = SetLenOnDrop::new(len_ptr);

                    for i in 0..n 
                    {
                        ::ptr::write(ptr.add(i), elem.clone());
                        local_len.increment_len(1);
                    }
                }
                v
            }
        }
    }

    impl<A: Array> ops::Deref for SmallVec<A>
    {
        type Target = [A::Item];
        #[inline] fn deref(&self) -> &[A::Item]
        {
            unsafe
            {
                let (ptr, len, _) = self.triple();
                slice::from_raw_parts(ptr, len)
            }
        }
    }

    impl<A: Array> ops::DerefMut for SmallVec<A>
    {
        #[inline] fn deref_mut(&mut self) -> &mut [A::Item]
        {
            unsafe
            {
                let (ptr, &mut len, _) = self.triple_mut();
                slice::from_raw_parts_mut(ptr, len)
            }
        }
    }

    impl<A: Array> AsRef<[A::Item]> for SmallVec<A>
    {
        #[inline] fn as_ref(&self) -> &[A::Item] { self }
    }

    impl<A: Array> AsMut<[A::Item]> for SmallVec<A>
    {
        #[inline] fn as_mut(&mut self) -> &mut [A::Item] { self }
    }

    impl<A: Array> Borrow<[A::Item]> for SmallVec<A>
    {
        #[inline] fn borrow(&self) -> &[A::Item] { self }
    }

    impl<A: Array> BorrowMut<[A::Item]> for SmallVec<A>
    {
        #[inline] fn borrow_mut(&mut self) -> &mut [A::Item] { self }
    }
    
    impl<A: Array<Item = u8>> io::Write for SmallVec<A>
    {
        #[inline] fn write(&mut self, buf: &[u8]) -> io::Result<usize>
        {
            self.extend_from_slice(buf);
            Ok(buf.len())
        }

        #[inline] fn write_all(&mut self, buf: &[u8]) -> io::Result<()>
        {
            self.extend_from_slice(buf);
            Ok(())
        }

        #[inline] fn flush(&mut self) -> io::Result<()> { Ok(()) }
    }
    
    impl<'a, A: Array> From<&'a [A::Item]> for SmallVec<A> where
    A::Item: Clone
    {
        #[inline] fn from(slice: &'a [A::Item]) -> SmallVec<A> { slice.iter().cloned().collect() }
    }

    impl<A: Array> From<Vec<A::Item>> for SmallVec<A>
    {
        #[inline] fn from(vec: Vec<A::Item>) -> SmallVec<A> { SmallVec::from_vec(vec) }
    }

    impl<A: Array> From<A> for SmallVec<A>
    {
        #[inline] fn from(array: A) -> SmallVec<A> { SmallVec::from_buf(array) }
    }

    impl<A: Array, I: SliceIndex<[A::Item]>> ops::Index<I> for SmallVec<A>
    {
        type Output = I::Output;
        fn index(&self, index: I) -> &I::Output { &(**self)[index] }
    }

    impl<A: Array, I: SliceIndex<[A::Item]>> ops::IndexMut<I> for SmallVec<A>
    {
        fn index_mut(&mut self, index: I) -> &mut I::Output { &mut (&mut **self)[index] }
    }

    #[allow(deprecated)]
    impl<A: Array> ExtendFromSlice<A::Item> for SmallVec<A> where
    A::Item: Copy
    {
        fn extend_from_slice(&mut self, other: &[A::Item]) { SmallVec::extend_from_slice(self, other) }
    }

    impl<A: Array> FromIterator<A::Item> for SmallVec<A>
    {
        #[inline] fn from_iter<I: IntoIterator<Item = A::Item>>(iterable: I) -> SmallVec<A>
        {
            let mut v = SmallVec::new();
            v.extend(iterable);
            v
        }
    }

    impl<A: Array> Extend<A::Item> for SmallVec<A>
    {
        fn extend<I: IntoIterator<Item = A::Item>>(&mut self, iterable: I)
        {
            let mut iter = iterable.into_iter();
            let (lower_size_bound, _) = iter.size_hint();
            self.reserve(lower_size_bound);
            unsafe
            {
                let (ptr, len_ptr, cap) = self.triple_mut();
                let mut len = SetLenOnDrop::new(len_ptr);

                while len.get() < cap
                {
                    if let Some(out) = iter.next()
                    {
                        ptr::write(ptr.add(len.get()), out);
                        len.increment_len(1);
                    } else { return; }
                }
            }
            
            for elem in iter
            {
                self.push(elem);
            }
        }
    }

    impl<A: Array> fmt::Debug for SmallVec<A> where
    A::Item: fmt::Debug
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { f.debug_list().entries(self.iter()).finish() }
    }

    impl<A: Array> Default for SmallVec<A>
    {
        #[inline] fn default() -> SmallVec<A> { SmallVec::new() }
    }
    
    impl<A: Array> Drop for SmallVec<A>
    {
        fn drop(&mut self)
        {
            unsafe
            {
                if self.spilled()
                {
                    let (ptr, len) = self.data.heap();
                    Vec::from_raw_parts(ptr, len, self.capacity);
                }
                else { ptr::drop_in_place(&mut self[..]); }
            }
        }
    }

    impl<A: Array> Clone for SmallVec<A> where
    A::Item: Clone
    {
        #[inline] fn clone(&self) -> SmallVec<A> { SmallVec::from(self.as_slice()) }
    }

    impl<A: Array, B: Array> PartialEq<SmallVec<B>> for SmallVec<A> where
    A::Item: PartialEq<B::Item>
    {
        #[inline] fn eq(&self, other: &SmallVec<B>) -> bool { self[..] == other[..] }
    }

    impl<A: Array> Eq for SmallVec<A> where A::Item: Eq {}

    impl<A: Array> PartialOrd for SmallVec<A> where
    A::Item: PartialOrd
    {
        #[inline] fn partial_cmp(&self, other: &SmallVec<A>) -> Option<cmp::Ordering> { PartialOrd::partial_cmp(&**self, &**other) }
    }

    impl<A: Array> Ord for SmallVec<A> where
    A::Item: Ord
    {
        #[inline] fn cmp(&self, other: &SmallVec<A>) -> cmp::Ordering { Ord::cmp(&**self, &**other) }
    }

    impl<A: Array> Hash for SmallVec<A> where
    A::Item: Hash
    {
        fn hash<H: Hasher>(&self, state: &mut H) { (**self).hash(state) }
    }

    unsafe impl<A: Array> Send for SmallVec<A> where A::Item: Send {}

    pub struct IntoIter<A: Array>
    {
        data: SmallVec<A>,
        current: usize,
        end: usize,
    }

    impl<A: Array> fmt::Debug for IntoIter<A> where
    A::Item: fmt::Debug
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { f.debug_tuple("IntoIter").field(&self.as_slice()).finish() }
    }

    impl<A: Array + Clone> Clone for IntoIter<A> where
    A::Item: Clone
    {
        fn clone(&self) -> IntoIter<A> { SmallVec::from(self.as_slice()).into_iter() }
    }

    impl<A: Array> Drop for IntoIter<A>
    {
        fn drop(&mut self) { for _ in self {} }
    }

    impl<A: Array> Iterator for IntoIter<A>
    {
        type Item = A::Item;

        #[inline] fn next(&mut self) -> Option<A::Item>
        {
            unsafe
            {
                if self.current == self.end { None }
                else
                {
                    let current = self.current;
                    self.current += 1;
                    Some(ptr::read(self.data.as_ptr().add(current)))
                }
            }
        }

        #[inline] fn size_hint(&self) -> (usize, Option<usize>)
        {
            let size = self.end - self.current;
            (size, Some(size))
        }
    }

    impl<A: Array> DoubleEndedIterator for IntoIter<A>
    {
        #[inline] fn next_back(&mut self) -> Option<A::Item>
        {
            unsafe
            {
                if self.current == self.end { None }
                else
                {
                    self.end -= 1;
                    Some(ptr::read(self.data.as_ptr().add(self.end)))
                }
            }
        }
    }

    impl<A: Array> ExactSizeIterator for IntoIter<A> {}
    impl<A: Array> FusedIterator for IntoIter<A> {}

    impl<A: Array> IntoIter<A>
    {
        pub fn as_slice(&self) -> &[A::Item]
        {unsafe
            { 
                let len = self.end - self.current;
                ::slice::from_raw_parts(self.data.as_ptr().add(self.current), len)
            }
        }
        
        pub fn as_mut_slice(&mut self) -> &mut [A::Item]
        {
            unsafe
            {
                let len = self.end - self.current;
                ::slice::from_raw_parts_mut(self.data.as_mut_ptr().add(self.current), len)
            }
        }
    }

    impl<A: Array> IntoIterator for SmallVec<A>
    {
        type IntoIter = IntoIter<A>;
        type Item = A::Item;
        fn into_iter(mut self) -> Self::IntoIter
        {
            unsafe
            {
                let len = self.len();
                self.set_len(0);
                IntoIter
                {
                    data: self,
                    current: 0,
                    end: len,
                }
            }
        }
    }

    impl<'a, A: Array> IntoIterator for &'a SmallVec<A>
    {
        type IntoIter = slice::Iter<'a, A::Item>;
        type Item = &'a A::Item;
        fn into_iter(self) -> Self::IntoIter { self.iter() }
    }

    impl<'a, A: Array> IntoIterator for &'a mut SmallVec<A>
    {
        type IntoIter = slice::IterMut<'a, A::Item>;
        type Item = &'a mut A::Item;
        fn into_iter(self) -> Self::IntoIter { self.iter_mut() }
    }
    
    pub unsafe trait Array
    {
        type Item;
        fn size() -> usize;
    }

    struct SetLenOnDrop<'a>
    {
        len: &'a mut usize,
        local_len: usize,
    }

    impl<'a> SetLenOnDrop<'a>
    {
        #[inline] fn new(len: &'a mut usize) -> Self
        {
            SetLenOnDrop
            {
                local_len: *len,
                len,
            }
        }

        #[inline] fn get(&self) -> usize { self.local_len }

        #[inline] fn increment_len(&mut self, increment: usize) { self.local_len += increment; }
    }

    impl<'a> Drop for SetLenOnDrop<'a>
    {
        #[inline] fn drop(&mut self) { *self.len = self.local_len; }
    }
    
    unsafe impl<T, const N: usize> Array for [T; N]
    {
        type Item = T;
        fn size() -> usize { N }
    }
    
    pub trait ToSmallVec<A: Array>
    {
        fn to_smallvec(&self) -> SmallVec<A>;
    }

    impl<A: Array> ToSmallVec<A> for [A::Item] where
    A::Item: Copy
    {
        #[inline] fn to_smallvec(&self) -> SmallVec<A> { SmallVec::from_slice(self) }
    }
}

pub fn main() -> Result<(), ()>
{
    unsafe
    {
        Ok( () )
    }
}
// 5525 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
