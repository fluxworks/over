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

pub mod error;
pub mod obj;
pub mod tup;
pub mod types;
pub mod value;
pub mod parse;
*/
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate rand;

#[macro_use] pub mod macros
{
    /// Performs variable interpolation against the input and produces it as [`process::macros::TokenStream`].
    #[macro_export] macro_rules! quote
    {
        () =>
        {
            ::__private::TokenStream::new()
        };
       
        ($tt:tt) => 
        {{
            let mut _s = ::__private::TokenStream::new();
            ::quote_token!{$tt _s}
            _s
        }};
       
        
        (# $var:ident) => 
        {{
            let mut _s = ::__private::TokenStream::new();
            ::ToTokens::to_tokens(&$var, &mut _s);
            _s
        }};
        
        ($tt1:tt $tt2:tt) =>
        {{
            let mut _s = ::__private::TokenStream::new();
            ::quote_token!{$tt1 _s}
            ::quote_token!{$tt2 _s}
            _s
        }};
       
        ($($tt:tt)*) => {{
            let mut _s = ::__private::TokenStream::new();
            ::quote_each_token!{_s $($tt)*}
            _s
        }};
    }
    /// Same as `quote!`, but applies a given span to all tokens originating within the macro invocation.
    #[macro_export] macro_rules! quote_spanned
    {
        ($span:expr=>) => {{
            let _: ::__private::Span = ::__private::get_span($span).__into_span();
            ::__private::TokenStream::new()
        }};
       
        ($span:expr=> $tt:tt) => {{
            let mut _s = ::__private::TokenStream::new();
            let _span: ::__private::Span = ::__private::get_span($span).__into_span();
            ::quote_token_spanned!{$tt _s _span}
            _s
        }};
       
        ($span:expr=> # $var:ident) => {{
            let mut _s = ::__private::TokenStream::new();
            let _: ::__private::Span = ::__private::get_span($span).__into_span();
            ::ToTokens::to_tokens(&$var, &mut _s);
            _s
        }};
        ($span:expr=> $tt1:tt $tt2:tt) => {{
            let mut _s = ::__private::TokenStream::new();
            let _span: ::__private::Span = ::__private::get_span($span).__into_span();
            ::quote_token_spanned!{$tt1 _s _span}
            ::quote_token_spanned!{$tt2 _s _span}
            _s
        }};
       
        ($span:expr=> $($tt:tt)*) => {{
            let mut _s = ::__private::TokenStream::new();
            let _span: ::__private::Span = ::__private::get_span($span).__into_span();
            ::quote_each_token_spanned!{_s _span $($tt)*}
            _s
        }};
    }
   
    #[macro_export] macro_rules! pounded_var_names
    {
        ($call:ident! $extra:tt $($tts:tt)*) => {
            ::pounded_var_names_with_context!{$call! $extra
                (@ $($tts)*)
                ($($tts)* @)
            }
        };
    }

    #[macro_export] macro_rules! pounded_var_names_with_context
    {
        ($call:ident! $extra:tt ($($b1:tt)*) ($($curr:tt)*)) => {
            $(
                ::pounded_var_with_context!{$call! $extra $b1 $curr}
            )*
        };
    }

    #[macro_export] macro_rules! pounded_var_with_context
    {
        ($call:ident! $extra:tt $b1:tt ( $($inner:tt)* )) => {
            ::pounded_var_names!{$call! $extra $($inner)*}
        };

        ($call:ident! $extra:tt $b1:tt [ $($inner:tt)* ]) => {
            ::pounded_var_names!{$call! $extra $($inner)*}
        };

        ($call:ident! $extra:tt $b1:tt { $($inner:tt)* }) => {
            ::pounded_var_names!{$call! $extra $($inner)*}
        };

        ($call:ident!($($extra:tt)*) # $var:ident) => {
            ::$call!($($extra)* $var);
        };

        ($call:ident! $extra:tt $b1:tt $curr:tt) => {};
    }

    #[macro_export] macro_rules! quote_bind_into_iter 
    {
        ($has_iter:ident $var:ident) => {
           
            #[allow(unused_mut)]
            let (mut $var, i) = $var.quote_into_iter();
            let $has_iter = $has_iter | i;
        };
    }

    #[macro_export] macro_rules! quote_bind_next_or_break 
    {
        ($var:ident) => {
            let $var = match $var.next() {
                Some(_x) => ::__private::RepInterp(_x),
                None => break,
            };
        };
    }
    
    #[macro_export] macro_rules! quote_each_token 
    {
        ($tokens:ident $($tts:tt)*) => {
            ::quote_tokens_with_context!{$tokens
                (@ @ @ @ @ @ $($tts)*)
                (@ @ @ @ @ $($tts)* @)
                (@ @ @ @ $($tts)* @ @)
                (@ @ @ $(($tts))* @ @ @)
                (@ @ $($tts)* @ @ @ @)
                (@ $($tts)* @ @ @ @ @)
                ($($tts)* @ @ @ @ @ @)
            }
        };
    }

    #[macro_export] macro_rules! quote_each_token_spanned 
    {
        ($tokens:ident $span:ident $($tts:tt)*) => {
            ::quote_tokens_with_context_spanned!{$tokens $span
                (@ @ @ @ @ @ $($tts)*)
                (@ @ @ @ @ $($tts)* @)
                (@ @ @ @ $($tts)* @ @)
                (@ @ @ $(($tts))* @ @ @)
                (@ @ $($tts)* @ @ @ @)
                (@ $($tts)* @ @ @ @ @)
                ($($tts)* @ @ @ @ @ @)
            }
        };
    }

    #[macro_export] macro_rules! quote_tokens_with_context
    {
        ($tokens:ident
            ($($b3:tt)*) ($($b2:tt)*) ($($b1:tt)*)
            ($($curr:tt)*)
            ($($a1:tt)*) ($($a2:tt)*) ($($a3:tt)*)
        ) => {
            $(
                ::quote_token_with_context!{$tokens $b3 $b2 $b1 $curr $a1 $a2 $a3}
            )*
        };
    }

    #[macro_export] macro_rules! quote_tokens_with_context_spanned
    {
        ($tokens:ident $span:ident
            ($($b3:tt)*) ($($b2:tt)*) ($($b1:tt)*)
            ($($curr:tt)*)
            ($($a1:tt)*) ($($a2:tt)*) ($($a3:tt)*)
        ) => {
            $(
                ::quote_token_with_context_spanned!{$tokens $span $b3 $b2 $b1 $curr $a1 $a2 $a3}
            )*
        };
    }

    #[macro_export] macro_rules! quote_token_with_context
    {  
        ($tokens:ident $b3:tt $b2:tt $b1:tt @ $a1:tt $a2:tt $a3:tt) => {};
       
        ($tokens:ident $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) * $a3:tt) => 
        {{
            use ::__private::ext::*;
            let has_iter = ::__private::ThereIsNoIteratorInRepetition;
            ::pounded_var_names!{quote_bind_into_iter!(has_iter) () $($inner)*}
            let _: ::__private::HasIterator = has_iter;
            
            while true 
            {
                ::pounded_var_names!{quote_bind_next_or_break!() () $($inner)*}
                ::quote_each_token!{$tokens $($inner)*}
            }
        }};
       
        ($tokens:ident $b3:tt $b2:tt # (( $($inner:tt)* )) * $a2:tt $a3:tt) => {};
       
        ($tokens:ident $b3:tt # ( $($inner:tt)* ) (*) $a1:tt $a2:tt $a3:tt) => {};
       
        ($tokens:ident $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) $sep:tt *) => 
        {{
            use ::__private::ext::*;
            let mut _i = 0usize;
            let has_iter = ::__private::ThereIsNoIteratorInRepetition;
            ::pounded_var_names!{quote_bind_into_iter!(has_iter) () $($inner)*}
            let _: ::__private::HasIterator = has_iter;
            while true {
                ::pounded_var_names!{quote_bind_next_or_break!() () $($inner)*}
                if _i > 0 {
                    ::quote_token!{$sep $tokens}
                }
                _i += 1;
                ::quote_each_token!{$tokens $($inner)*}
            }
        }};
       
        ($tokens:ident $b3:tt $b2:tt # (( $($inner:tt)* )) $sep:tt * $a3:tt) => {};
       
        ($tokens:ident $b3:tt # ( $($inner:tt)* ) ($sep:tt) * $a2:tt $a3:tt) => {};
       
       
        ($tokens:ident # ( $($inner:tt)* ) * (*) $a1:tt $a2:tt $a3:tt) => {
            ::quote_token!{* $tokens}
        };
       
        ($tokens:ident # ( $($inner:tt)* ) $sep:tt (*) $a1:tt $a2:tt $a3:tt) => {};
       
        ($tokens:ident $b3:tt $b2:tt $b1:tt (#) $var:ident $a2:tt $a3:tt) => {
            ::ToTokens::to_tokens(&$var, &mut $tokens);
        };
       
        ($tokens:ident $b3:tt $b2:tt # ($var:ident) $a1:tt $a2:tt $a3:tt) => {};
       
        ($tokens:ident $b3:tt $b2:tt $b1:tt ($curr:tt) $a1:tt $a2:tt $a3:tt) => {
            ::quote_token!{$curr $tokens}
        };
    }
    
    #[macro_export] macro_rules! quote_token_with_context_spanned 
    {
        ($tokens:ident $span:ident $b3:tt $b2:tt $b1:tt @ $a1:tt $a2:tt $a3:tt) => {};

        ($tokens:ident $span:ident $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) * $a3:tt) => {{
            use ::__private::ext::*;
            let has_iter = ::__private::ThereIsNoIteratorInRepetition;
            ::pounded_var_names!{quote_bind_into_iter!(has_iter) () $($inner)*}
            let _: ::__private::HasIterator = has_iter;
            while true {
                ::pounded_var_names!{quote_bind_next_or_break!() () $($inner)*}
                ::quote_each_token_spanned!{$tokens $span $($inner)*}
            }
        }};
        ($tokens:ident $span:ident $b3:tt $b2:tt # (( $($inner:tt)* )) * $a2:tt $a3:tt) => {};
        ($tokens:ident $span:ident $b3:tt # ( $($inner:tt)* ) (*) $a1:tt $a2:tt $a3:tt) => {};

        ($tokens:ident $span:ident $b3:tt $b2:tt $b1:tt (#) ( $($inner:tt)* ) $sep:tt *) => {{
            use ::__private::ext::*;
            let mut _i = 0usize;
            let has_iter = ::__private::ThereIsNoIteratorInRepetition;
            ::pounded_var_names!{quote_bind_into_iter!(has_iter) () $($inner)*}
            let _: ::__private::HasIterator = has_iter;
            while true {
                ::pounded_var_names!{quote_bind_next_or_break!() () $($inner)*}
                if _i > 0 {
                    ::quote_token_spanned!{$sep $tokens $span}
                }
                _i += 1;
                ::quote_each_token_spanned!{$tokens $span $($inner)*}
            }
        }};
        ($tokens:ident $span:ident $b3:tt $b2:tt # (( $($inner:tt)* )) $sep:tt * $a3:tt) => {};
        ($tokens:ident $span:ident $b3:tt # ( $($inner:tt)* ) ($sep:tt) * $a2:tt $a3:tt) => {};
        ($tokens:ident $span:ident # ( $($inner:tt)* ) * (*) $a1:tt $a2:tt $a3:tt) => {
            ::quote_token_spanned!{* $tokens $span}
        };
        ($tokens:ident $span:ident # ( $($inner:tt)* ) $sep:tt (*) $a1:tt $a2:tt $a3:tt) => {};

        ($tokens:ident $span:ident $b3:tt $b2:tt $b1:tt (#) $var:ident $a2:tt $a3:tt) => {
            ::ToTokens::to_tokens(&$var, &mut $tokens);
        };
        ($tokens:ident $span:ident $b3:tt $b2:tt # ($var:ident) $a1:tt $a2:tt $a3:tt) => {};

        ($tokens:ident $span:ident $b3:tt $b2:tt $b1:tt ($curr:tt) $a1:tt $a2:tt $a3:tt) => {
            ::quote_token_spanned!{$curr $tokens $span}
        };
    }
    
    #[macro_export] macro_rules! quote_token
    {
        ($ident:ident $tokens:ident) =>
        {
            ::__private::push_ident(&mut $tokens, stringify!($ident));
        };

        (:: $tokens:ident) =>
        {
            ::__private::push_colon2(&mut $tokens);
        };

        (( $($inner:tt)* ) $tokens:ident) =>
        {
            ::__private::push_group
            (
                &mut $tokens,
                ::__private::Delimiter::Parenthesis,
                ::quote!($($inner)*),
            );
        };

        ([ $($inner:tt)* ] $tokens:ident) =>
        {
            ::__private::push_group
            (
                &mut $tokens,
                ::__private::Delimiter::Bracket,
                ::quote!($($inner)*),
            );
        };

        ({ $($inner:tt)* } $tokens:ident) =>
        {
            ::__private::push_group
            (
                &mut $tokens,
                ::__private::Delimiter::Brace,
                ::quote!($($inner)*),
            );
        };

        (# $tokens:ident) =>
        {
            ::__private::push_pound(&mut $tokens);
        };

        (, $tokens:ident) =>
        {
            ::__private::push_comma(&mut $tokens);
        };

        (. $tokens:ident) =>
        {
            ::__private::push_dot(&mut $tokens);
        };

        (; $tokens:ident) =>
        {
            ::__private::push_semi(&mut $tokens);
        };

        (: $tokens:ident) =>
        {
            ::__private::push_colon(&mut $tokens);
        };

        (+ $tokens:ident) =>
        {
            ::__private::push_add(&mut $tokens);
        };

        (+= $tokens:ident) =>
        {
            ::__private::push_add_eq(&mut $tokens);
        };

        (& $tokens:ident) =>
        {
            ::__private::push_and(&mut $tokens);
        };

        (&& $tokens:ident) =>
        {
            ::__private::push_and_and(&mut $tokens);
        };

        (&= $tokens:ident) =>
        {
            ::__private::push_and_eq(&mut $tokens);
        };

        (@ $tokens:ident) =>
        {
            ::__private::push_at(&mut $tokens);
        };

        (! $tokens:ident) =>
        {
            ::__private::push_bang(&mut $tokens);
        };

        (^ $tokens:ident) =>
        {
            ::__private::push_caret(&mut $tokens);
        };

        (^= $tokens:ident) =>
        {
            ::__private::push_caret_eq(&mut $tokens);
        };

        (/ $tokens:ident) =>
        {
            ::__private::push_div(&mut $tokens);
        };

        (/= $tokens:ident) =>
        {
            ::__private::push_div_eq(&mut $tokens);
        };

        (.. $tokens:ident) =>
        {
            ::__private::push_dot2(&mut $tokens);
        };

        (... $tokens:ident) =>
        {
            ::__private::push_dot3(&mut $tokens);
        };

        (..= $tokens:ident) =>
        {
            ::__private::push_dot_dot_eq(&mut $tokens);
        };

        (= $tokens:ident) =>
        {
            ::__private::push_eq(&mut $tokens);
        };

        (== $tokens:ident) =>
        {
            ::__private::push_eq_eq(&mut $tokens);
        };

        (>= $tokens:ident) =>
        {
            ::__private::push_ge(&mut $tokens);
        };

        (> $tokens:ident) =>
        {
            ::__private::push_gt(&mut $tokens);
        };

        (<= $tokens:ident) =>
        {
            ::__private::push_le(&mut $tokens);
        };

        (< $tokens:ident) =>
        {
            ::__private::push_lt(&mut $tokens);
        };

        (*= $tokens:ident) =>
        {
            ::__private::push_mul_eq(&mut $tokens);
        };

        (!= $tokens:ident) =>
        {
            ::__private::push_ne(&mut $tokens);
        };

        (| $tokens:ident) =>
        {
            ::__private::push_or(&mut $tokens);
        };

        (|= $tokens:ident) =>
        {
            ::__private::push_or_eq(&mut $tokens);
        };

        (|| $tokens:ident) =>
        {
            ::__private::push_or_or(&mut $tokens);
        };

        (? $tokens:ident) =>
        {
            ::__private::push_question(&mut $tokens);
        };

        (-> $tokens:ident) =>
        {
            ::__private::push_rarrow(&mut $tokens);
        };

        (<- $tokens:ident) =>
        {
            ::__private::push_larrow(&mut $tokens);
        };

        (% $tokens:ident) =>
        {
            ::__private::push_rem(&mut $tokens);
        };

        (%= $tokens:ident) =>
        {
            ::__private::push_rem_eq(&mut $tokens);
        };

        (=> $tokens:ident) =>
        {
            ::__private::push_fat_arrow(&mut $tokens);
        };

        (<< $tokens:ident) =>
        {
            ::__private::push_shl(&mut $tokens);
        };

        (<<= $tokens:ident) =>
        {
            ::__private::push_shl_eq(&mut $tokens);
        };

        (>> $tokens:ident) =>
        {
            ::__private::push_shr(&mut $tokens);
        };

        (>>= $tokens:ident) =>
        {
            ::__private::push_shr_eq(&mut $tokens);
        };

        (* $tokens:ident) =>
        {
            ::__private::push_star(&mut $tokens);
        };

        (- $tokens:ident) =>
        {
            ::__private::push_sub(&mut $tokens);
        };

        (-= $tokens:ident) =>
        {
            ::__private::push_sub_eq(&mut $tokens);
        };

        ($lifetime:lifetime $tokens:ident) =>
        {
            ::__private::push_lifetime(&mut $tokens, stringify!($lifetime));
        };

        (_ $tokens:ident) =>
        {
            ::__private::push_underscore(&mut $tokens);
        };

        ($other:tt $tokens:ident) =>
        {
            ::__private::parse(&mut $tokens, stringify!($other));
        };
    }

    #[macro_export] macro_rules! quote_token_spanned
    {
        ($ident:ident $tokens:ident $span:ident) =>
        {
            ::__private::push_ident_spanned(&mut $tokens, $span, stringify!($ident));
        };

        (:: $tokens:ident $span:ident) =>
        {
            ::__private::push_colon2_spanned(&mut $tokens, $span);
        };

        (( $($inner:tt)* ) $tokens:ident $span:ident) =>
        {
            ::__private::push_group_spanned            
            (
                &mut $tokens,
                $span,
                ::__private::Delimiter::Parenthesis,
                ::quote_spanned!($span=> $($inner)*),
            );
        };

        ([ $($inner:tt)* ] $tokens:ident $span:ident) =>
        {
            ::__private::push_group_spanned
            (
                &mut $tokens,
                $span,
                ::__private::Delimiter::Bracket,
                ::quote_spanned!($span=> $($inner)*),
            );
        };

        ({ $($inner:tt)* } $tokens:ident $span:ident) =>
        {
            ::__private::push_group_spanned
            
            (
                &mut $tokens,
                $span,
                ::__private::Delimiter::Brace,
                ::quote_spanned!($span=> $($inner)*),
            );
        };

        (# $tokens:ident $span:ident) =>
        {
            ::__private::push_pound_spanned(&mut $tokens, $span);
        };

        (, $tokens:ident $span:ident) =>
        {
            ::__private::push_comma_spanned(&mut $tokens, $span);
        };

        (. $tokens:ident $span:ident) =>
        {
            ::__private::push_dot_spanned(&mut $tokens, $span);
        };

        (; $tokens:ident $span:ident) =>
        {
            ::__private::push_semi_spanned(&mut $tokens, $span);
        };

        (: $tokens:ident $span:ident) =>
        {
            ::__private::push_colon_spanned(&mut $tokens, $span);
        };

        (+ $tokens:ident $span:ident) =>
        {
            ::__private::push_add_spanned(&mut $tokens, $span);
        };

        (+= $tokens:ident $span:ident) =>
        {
            ::__private::push_add_eq_spanned(&mut $tokens, $span);
        };

        (& $tokens:ident $span:ident) =>
        {
            ::__private::push_and_spanned(&mut $tokens, $span);
        };

        (&& $tokens:ident $span:ident) =>
        {
            ::__private::push_and_and_spanned(&mut $tokens, $span);
        };

        (&= $tokens:ident $span:ident) =>
        {
            ::__private::push_and_eq_spanned(&mut $tokens, $span);
        };

        (@ $tokens:ident $span:ident) =>
        {
            ::__private::push_at_spanned(&mut $tokens, $span);
        };

        (! $tokens:ident $span:ident) =>
        {
            ::__private::push_bang_spanned(&mut $tokens, $span);
        };

        (^ $tokens:ident $span:ident) =>
        {
            ::__private::push_caret_spanned(&mut $tokens, $span);
        };

        (^= $tokens:ident $span:ident) =>
        {
            ::__private::push_caret_eq_spanned(&mut $tokens, $span);
        };

        (/ $tokens:ident $span:ident) =>
        {
            ::__private::push_div_spanned(&mut $tokens, $span);
        };

        (/= $tokens:ident $span:ident) =>
        {
            ::__private::push_div_eq_spanned(&mut $tokens, $span);
        };

        (.. $tokens:ident $span:ident) =>
        {
            ::__private::push_dot2_spanned(&mut $tokens, $span);
        };

        (... $tokens:ident $span:ident) =>
        {
            ::__private::push_dot3_spanned(&mut $tokens, $span);
        };

        (..= $tokens:ident $span:ident) =>
        {
            ::__private::push_dot_dot_eq_spanned(&mut $tokens, $span);
        };

        (= $tokens:ident $span:ident) =>
        {
            ::__private::push_eq_spanned(&mut $tokens, $span);
        };

        (== $tokens:ident $span:ident) =>
        {
            ::__private::push_eq_eq_spanned(&mut $tokens, $span);
        };

        (>= $tokens:ident $span:ident) =>
        {
            ::__private::push_ge_spanned(&mut $tokens, $span);
        };

        (> $tokens:ident $span:ident) =>
        {
            ::__private::push_gt_spanned(&mut $tokens, $span);
        };

        (<= $tokens:ident $span:ident) =>
        {
            ::__private::push_le_spanned(&mut $tokens, $span);
        };

        (< $tokens:ident $span:ident) =>
        {
            ::__private::push_lt_spanned(&mut $tokens, $span);
        };

        (*= $tokens:ident $span:ident) =>
        {
            ::__private::push_mul_eq_spanned(&mut $tokens, $span);
        };

        (!= $tokens:ident $span:ident) =>
        {
            ::__private::push_ne_spanned(&mut $tokens, $span);
        };

        (| $tokens:ident $span:ident) =>
        {
            ::__private::push_or_spanned(&mut $tokens, $span);
        };

        (|= $tokens:ident $span:ident) =>
        {
            ::__private::push_or_eq_spanned(&mut $tokens, $span);
        };

        (|| $tokens:ident $span:ident) =>
        {
            ::__private::push_or_or_spanned(&mut $tokens, $span);
        };

        (? $tokens:ident $span:ident) =>
        {
            ::__private::push_question_spanned(&mut $tokens, $span);
        };

        (-> $tokens:ident $span:ident) =>
        {
            ::__private::push_rarrow_spanned(&mut $tokens, $span);
        };

        (<- $tokens:ident $span:ident) =>
        {
            ::__private::push_larrow_spanned(&mut $tokens, $span);
        };

        (% $tokens:ident $span:ident) =>
        {
            ::__private::push_rem_spanned(&mut $tokens, $span);
        };

        (%= $tokens:ident $span:ident) =>
        {
            ::__private::push_rem_eq_spanned(&mut $tokens, $span);
        };

        (=> $tokens:ident $span:ident) =>
        {
            ::__private::push_fat_arrow_spanned(&mut $tokens, $span);
        };

        (<< $tokens:ident $span:ident) =>
        {
            ::__private::push_shl_spanned(&mut $tokens, $span);
        };

        (<<= $tokens:ident $span:ident) =>
        {
            ::__private::push_shl_eq_spanned(&mut $tokens, $span);
        };

        (>> $tokens:ident $span:ident) =>
        {
            ::__private::push_shr_spanned(&mut $tokens, $span);
        };

        (>>= $tokens:ident $span:ident) =>
        {
            ::__private::push_shr_eq_spanned(&mut $tokens, $span);
        };

        (* $tokens:ident $span:ident) =>
        {
            ::__private::push_star_spanned(&mut $tokens, $span);
        };

        (- $tokens:ident $span:ident) =>
        {
            ::__private::push_sub_spanned(&mut $tokens, $span);
        };

        (-= $tokens:ident $span:ident) =>
        {
            ::__private::push_sub_eq_spanned(&mut $tokens, $span);
        };

        ($lifetime:lifetime $tokens:ident $span:ident) =>
        {
            ::__private::push_lifetime_spanned(&mut $tokens, $span, stringify!($lifetime));
        };

        (_ $tokens:ident $span:ident) =>
        {
            ::__private::push_underscore_spanned(&mut $tokens, $span);
        };

        ($other:tt $tokens:ident $span:ident) =>
        {
            ::__private::parse_spanned(&mut $tokens, $span, stringify!($other));
        };
    }

    #[macro_export] macro_rules! format_ident
    {
        ($fmt:expr) => {
            format_ident_impl!([
                ::quote::__private::Option::None,
                $fmt
            ])
        };

        ($fmt:expr, $($rest:tt)*) => {
            format_ident_impl!([
                ::quote::__private::Option::None,
                $fmt
            ] $($rest)*)
        };
    }

    #[macro_export] macro_rules! format_ident_impl
    {
       
        ([$span:expr, $($fmt:tt)*]) => {
            ::quote::__private::mk_ident(
                &::quote::__private::format!($($fmt)*),
                $span,
            )
        };

       
        ([$old:expr, $($fmt:tt)*] span = $span:expr) => {
            format_ident_impl!([$old, $($fmt)*] span = $span,)
        };
        ([$old:expr, $($fmt:tt)*] span = $span:expr, $($rest:tt)*) => {
            format_ident_impl!([
                ::quote::__private::Option::Some::<::quote::__private::Span>($span),
                $($fmt)*
            ] $($rest)*)
        };

       
        ([$span:expr, $($fmt:tt)*] $name:ident = $arg:expr) => {
            format_ident_impl!([$span, $($fmt)*] $name = $arg,)
        };
        ([$span:expr, $($fmt:tt)*] $name:ident = $arg:expr, $($rest:tt)*) => {
            match ::quote::__private::IdentFragmentAdapter(&$arg) {
                arg => format_ident_impl!([$span.or(arg.span()), $($fmt)*, $name = arg] $($rest)*),
            }
        };

       
        ([$span:expr, $($fmt:tt)*] $arg:expr) => {
            format_ident_impl!([$span, $($fmt)*] $arg,)
        };
        ([$span:expr, $($fmt:tt)*] $arg:expr, $($rest:tt)*) => {
            match ::quote::__private::IdentFragmentAdapter(&$arg) {
                arg => format_ident_impl!([$span.or(arg.span()), $($fmt)*, arg] $($rest)*),
            }
        };
    }

    #[macro_export] macro_rules! ast_struct 
    {
        (
            $(#[$attr:meta])*
            $pub:ident $struct:ident $name:ident #full $body:tt
        ) =>
        {
            check_keyword_matches!(pub $pub);
            check_keyword_matches!(struct $struct);
            $(#[$attr])* $pub $struct $name $body
        };

        (
            $(#[$attr:meta])*
            $pub:ident $struct:ident $name:ident $body:tt
        ) => {
            check_keyword_matches!(pub $pub);
            check_keyword_matches!(struct $struct);

            $(#[$attr])* $pub $struct $name $body
        };
    }
    
    #[macro_export] macro_rules! ast_enum
    {
        (
            $(#[$enum_attr:meta])*
            $pub:ident $enum:ident $name:ident $body:tt
        ) => {
            check_keyword_matches!(pub $pub);
            check_keyword_matches!(enum $enum);

            $(#[$enum_attr])* $pub $enum $name $body
        };
    }

    #[macro_export] macro_rules! ast_enum_of_structs
    {
        (
            $(#[$enum_attr:meta])*
            $pub:ident $enum:ident $name:ident $body:tt
        ) => {
            check_keyword_matches!(pub $pub);
            check_keyword_matches!(enum $enum);

            $(#[$enum_attr])* $pub $enum $name $body

            ast_enum_of_structs_impl!($name $body);

                generate_to_tokens!(() tokens $name $body);
        };
    }

    #[macro_export] macro_rules! ast_enum_of_structs_impl
    {
        (
            $name:ident {
                $(
                    $(#[cfg $cfg_attr:tt])*
                    $(#[doc $($doc_attr:tt)*])*
                    $variant:ident $( ($member:ident) )*,
                )*
            }
        ) => {
            $($(
                ast_enum_from_struct!($name::$variant, $member);
            )*)*
        };
    }

    #[macro_export] macro_rules! ast_enum_from_struct
    {
        ($name:ident::Verbatim, $member:ident) => {};

        ($name:ident::$variant:ident, $member:ident) =>
        {
            impl From<$member> for $name
            {
                fn from(e: $member) -> $name {
                    $name::$variant(e)
                }
            }
        };
    }
    
    #[macro_export] macro_rules! generate_to_tokens
    {
        (
            ($($arms:tt)*) $tokens:ident $name:ident {
                $(#[cfg $cfg_attr:tt])*
                $(#[doc $($doc_attr:tt)*])*
                $variant:ident,
                $($next:tt)*
            }
        ) => {
            generate_to_tokens!(
                ($($arms)* $(#[cfg $cfg_attr])* $name::$variant => {})
                $tokens $name { $($next)* }
            );
        };

        (
            ($($arms:tt)*) $tokens:ident $name:ident {
                $(#[cfg $cfg_attr:tt])*
                $(#[doc $($doc_attr:tt)*])*
                $variant:ident($member:ident),
                $($next:tt)*
            }
        ) => {
            generate_to_tokens!(
                ($($arms)* $(#[cfg $cfg_attr])* $name::$variant(_e) => _e.to_tokens($tokens),)
                $tokens $name { $($next)* }
            );
        };

        (($($arms:tt)*) $tokens:ident $name:ident {}) => {
            impl ::quote::ToTokens for $name {
                fn to_tokens(&self, $tokens:&mut ::process::macros::TokenStream )
                {
                    match self {
                        $($arms)*
                    }
                }
            }
        };
    }
    
    #[macro_export] macro_rules! pub_if_not_doc
    {
        ($(#[$m:meta])* $pub:ident $($item:tt)*) => {
            check_keyword_matches!(pub $pub);

            $(#[$m])*
            $pub $($item)*
        };
    }

    #[macro_export] macro_rules! check_keyword_matches 
    {
        (enum enum) => {};
        (pub pub) => {};
        (struct struct) => {};
    }

    #[macro_export] macro_rules! return_impl_trait
    {
        (
            $(#[$attr:meta])*
            $vis:vis fn $name:ident $args:tt -> $impl_trait:ty [$concrete:ty] $body:block
        ) => {
            #[cfg(not(docsrs))]
            $(#[$attr])*
            $vis fn $name $args -> $concrete $body

            #[cfg(docsrs)]
            $(#[$attr])*
            $vis fn $name $args -> $impl_trait $body
        };
    }
    /// Parse a set of parentheses and expose their content to subsequent parsers.
    #[macro_export] macro_rules! parenthesized
    {
        ($content:ident in $cursor:expr) => {
            match ::syntax::__private::parse_parens(&$cursor) {
                ::syntax::__private::Ok(parens) => {
                    $content = parens.content;
                    parens.token
                }
                ::syntax::__private::Err(error) => {
                    return ::syntax::__private::Err(error);
                }
            }
        };
    }
    /// Parse a set of curly braces and expose their content to subsequent parsers.
    #[macro_export] macro_rules! braced
    {
        ($content:ident in $cursor:expr) => 
        {
            match ::syntax::__private::parse_braces(&$cursor) {
                ::syntax::__private::Ok(braces) => {
                    $content = braces.content;
                    braces.token
                }
                ::syntax::__private::Err(error) => {
                    return ::syntax::__private::Err(error);
                }
            }
        };
    }
    /// Parse a set of square brackets and expose their content to subsequent parsers.
    #[macro_export] macro_rules! bracketed
    {
        ($content:ident in $cursor:expr) =>
        {
            match ::syntax::__private::parse_brackets(&$cursor)
            {
                ::syntax::__private::Ok(brackets) => {
                    $content = brackets.content;
                    brackets.token
                }
                ::syntax::__private::Err(error) => {
                    return ::syntax::__private::Err(error);
                }
            }
        };
    }

    #[macro_export] macro_rules! parse_quote
    {
        ($($tt:tt)*) =>
        {
            ::syntax::__private::parse_quote(::syntax::__private::quote::quote!($($tt)*))
        };
    }
    /// This macro is [`parse_quote!`] + [`quote_spanned!`][quote::quote_spanned].
    #[macro_export] macro_rules! parse_quote_spanned
    {
        ($span:expr=> $($tt:tt)*) =>
        {
            ::syntax::__private::parse_quote(::syntax::__private::quote::quote_spanned!($span=> $($tt)*))
        };
    }
    
    #[macro_export] macro_rules! parse_macro_input
    {
        ($tokenstream:ident as $ty:ty) => {
            match ::syntax::parse::<$ty>($tokenstream) {
                ::syntax::__private::Ok(data) => data,
                ::syntax::__private::Err(err) => {
                    return ::syntax::__private::TokenStream::from(err.to_compile_error());
                }
            }
        };
        ($tokenstream:ident with $parser:path) => {
            match ::syntax::parse::Parser::parse($parser, $tokenstream) {
                ::syntax::__private::Ok(data) => data,
                ::syntax::__private::Err(err) => {
                    return ::syntax::__private::TokenStream::from(err.to_compile_error());
                }
            }
        };
        ($tokenstream:ident) => {
            ::syntax::parse_macro_input!($tokenstream as _)
        };
    }

    #[macro_export] macro_rules! custom_punctuation
    {
        ($ident:ident, $($tt:tt)+) =>
        {
            pub struct $ident {
                #[allow(dead_code)]
                pub spans: ::syntax::custom_punctuation_repr!($($tt)+),
            }
                #[allow(dead_code, non_snake_case)]
            pub fn $ident<__S: ::syntax::__private::IntoSpans<::syntax::custom_punctuation_repr!($($tt)+)>>(
                spans: __S,
            ) -> $ident {
                let _validate_len = 0 $(+ ::syntax::custom_punctuation_len!(strict, $tt))*;
                $ident {
                    spans: ::syntax::__private::IntoSpans::into_spans(spans)
                }
            }
            const _: () = {
                impl ::syntax::__private::Default for $ident {
                    fn default() -> Self {
                        $ident(::syntax::__private::Span::call_site())
                    }
                }
                ::syntax::impl_parse_for_custom_punctuation!($ident, $($tt)+);
                ::syntax::impl_to_tokens_for_custom_punctuation!($ident, $($tt)+);
                ::syntax::impl_clone_for_custom_punctuation!($ident, $($tt)+);
                ::syntax::impl_extra_traits_for_custom_punctuation!($ident, $($tt)+);
            };
        };
    }
    
    #[macro_export] macro_rules! impl_parse_for_custom_punctuation
    {
        ($ident:ident, $($tt:tt)+) => {
            impl ::syntax::__private::CustomToken for $ident {
                fn peek(cursor: ::syntax::buffer::Cursor) -> ::syntax::__private::bool {
                    ::syntax::__private::peek_punct(cursor, ::syntax::stringify_punct!($($tt)+))
                }
                fn display() -> &'static ::syntax::__private::str {
                    ::syntax::__private::concat!("`", ::syntax::stringify_punct!($($tt)+), "`")
                }
            }
            
            impl ::syntax::parse::Parse for $ident
            {
                fn parse(input: ::syntax::parse::ParseStream) -> ::syntax::parse::Result<$ident> {
                    let spans: ::syntax::custom_punctuation_repr!($($tt)+) =
                        ::syntax::__private::parse_punct(input, ::syntax::stringify_punct!($($tt)+))?;
                    Ok($ident(spans))
                }
            }
        };
    }
    
    #[macro_export] macro_rules! impl_to_tokens_for_custom_punctuation
    {
        ($ident:ident, $($tt:tt)+) => {
            impl ::syntax::__private::ToTokens for $ident {
                fn to_tokens(&self, tokens: &mut ::syntax::__private::TokenStream2) {
                    ::syntax::__private::print_punct(::syntax::stringify_punct!($($tt)+), &self.spans, tokens)
                }
            }
        };
    }
        
    #[macro_export] macro_rules! impl_clone_for_custom_punctuation
    {
        ($ident:ident, $($tt:tt)+) => {
            impl ::syntax::__private::Copy for $ident {}
            #[allow(clippy::expl_impl_clone_on_copy)]
            impl ::syntax::__private::Clone for $ident {
                fn clone(&self) -> Self {
                    *self
                }
            }
        };
    }
    
    #[macro_export] macro_rules! impl_extra_traits_for_custom_punctuation
    {
        ($ident:ident, $($tt:tt)+) => {
            impl ::syntax::__private::Debug for $ident {
                fn fmt(&self, f: &mut ::syntax::__private::Formatter) -> ::syntax::__private::FmtResult {
                    ::syntax::__private::Formatter::write_str(f, ::syntax::__private::stringify!($ident))
                }
            }
            
            impl ::syntax::__private::Eq for $ident {}
            
            impl ::syntax::__private::PartialEq for $ident {
                fn eq(&self, _other: &Self) -> ::syntax::__private::bool {
                    true
                }
            }
            
            impl ::syntax::__private::Hash for $ident {
                fn hash<__H: ::syntax::__private::Hasher>(&self, _state: &mut __H) {}
            }
        };
    }
        
    #[macro_export] macro_rules! custom_punctuation_repr
    {
        ($($tt:tt)+) => {
            [::syntax::__private::Span; 0 $(+ ::syntax::custom_punctuation_len!(lenient, $tt))+]
        };
    }
    
    #[macro_export] macro_rules! custom_punctuation_len
    {
        ($mode:ident, &)     => { 1 };
        ($mode:ident, &&)    => { 2 };
        ($mode:ident, &=)    => { 2 };
        ($mode:ident, @)     => { 1 };
        ($mode:ident, ^)     => { 1 };
        ($mode:ident, ^=)    => { 2 };
        ($mode:ident, :)     => { 1 };
        ($mode:ident, ,)     => { 1 };
        ($mode:ident, $)     => { 1 };
        ($mode:ident, .)     => { 1 };
        ($mode:ident, ..)    => { 2 };
        ($mode:ident, ...)   => { 3 };
        ($mode:ident, ..=)   => { 3 };
        ($mode:ident, =)     => { 1 };
        ($mode:ident, ==)    => { 2 };
        ($mode:ident, =>)    => { 2 };
        ($mode:ident, >=)    => { 2 };
        ($mode:ident, >)     => { 1 };
        ($mode:ident, <-)    => { 2 };
        ($mode:ident, <=)    => { 2 };
        ($mode:ident, <)     => { 1 };
        ($mode:ident, -)     => { 1 };
        ($mode:ident, -=)    => { 2 };
        ($mode:ident, !=)    => { 2 };
        ($mode:ident, !)     => { 1 };
        ($mode:ident, |)     => { 1 };
        ($mode:ident, |=)    => { 2 };
        ($mode:ident, ||)    => { 2 };
        ($mode:ident, ::)    => { 2 };
        ($mode:ident, %)     => { 1 };
        ($mode:ident, %=)    => { 2 };
        ($mode:ident, +)     => { 1 };
        ($mode:ident, +=)    => { 2 };
        ($mode:ident, #)     => { 1 };
        ($mode:ident, ?)     => { 1 };
        ($mode:ident, ->)    => { 2 };
        ($mode:ident, ;)     => { 1 };
        ($mode:ident, <<)    => { 2 };
        ($mode:ident, <<=)   => { 3 };
        ($mode:ident, >>)    => { 2 };
        ($mode:ident, >>=)   => { 3 };
        ($mode:ident, /)     => { 1 };
        ($mode:ident, /=)    => { 2 };
        ($mode:ident, *)     => { 1 };
        ($mode:ident, *=)    => { 2 };
        ($mode:ident, ~)     => { 1 };
        (lenient, $tt:tt)    => { 0 };
        (strict, $tt:tt)     => {{ ::syntax::custom_punctuation_unexpected!($tt); 0 }};
    }
    
    #[macro_export] macro_rules! custom_punctuation_unexpected
    {
        () => {};
    }
    
    #[macro_export] macro_rules! stringify_punct
    {
        ($($tt:tt)+) => {
            ::syntax::__private::concat!($(::syntax::__private::stringify!($tt)),+)
        };
    }

    #[macro_export] macro_rules! custom_keyword
    {
        ($ident:ident) => {
            #[allow(non_camel_case_types)]
            pub struct $ident {
                #[allow(dead_code)]
                pub span: ::syntax::__private::Span,
            }
                #[allow(dead_code, non_snake_case)]
            pub fn $ident<__S: ::syntax::__private::IntoSpans<::syntax::__private::Span>>(
                span: __S,
            ) -> $ident {
                $ident {
                    span: ::syntax::__private::IntoSpans::into_spans(span),
                }
            }
            const _: () = {
                impl ::syntax::__private::Default for $ident {
                    fn default() -> Self {
                        $ident {
                            span: ::syntax::__private::Span::call_site(),
                        }
                    }
                }
                ::syntax::impl_parse_for_custom_keyword!($ident);
                ::syntax::impl_to_tokens_for_custom_keyword!($ident);
                ::syntax::impl_clone_for_custom_keyword!($ident);
                ::syntax::impl_extra_traits_for_custom_keyword!($ident);
            };
        };
    }
    
    #[macro_export] macro_rules! impl_parse_for_custom_keyword
    {
        ($ident:ident) => {
           
            impl ::syntax::__private::CustomToken for $ident {
                fn peek(cursor: ::syntax::buffer::Cursor) -> ::syntax::__private::bool {
                    if let ::syntax::__private::Some((ident, _rest)) = cursor.ident() {
                        ident == ::syntax::__private::stringify!($ident)
                    } else {
                        false
                    }
                }
                fn display() -> &'static ::syntax::__private::str {
                    ::syntax::__private::concat!("`", ::syntax::__private::stringify!($ident), "`")
                }
            }
            
            impl ::syntax::parse::Parse for $ident
            {
                fn parse(input: ::syntax::parse::ParseStream) -> ::syntax::parse::Result<$ident> {
                    input.step(|cursor| {
                        if let ::syntax::__private::Some((ident, rest)) = cursor.ident() {
                            if ident == ::syntax::__private::stringify!($ident) {
                                return ::syntax::__private::Ok(($ident { span: ident.span() }, rest));
                            }
                        }
                        ::syntax::__private::Err(cursor.error(::syntax::__private::concat!(
                            "expected `",
                            ::syntax::__private::stringify!($ident),
                            "`",
                        )))
                    })
                }
            }
        };
    }
    
    #[macro_export] macro_rules! impl_to_tokens_for_custom_keyword
    {
        ($ident:ident) => {
            impl ::syntax::__private::ToTokens for $ident {
                fn to_tokens(&self, tokens: &mut ::syntax::__private::TokenStream2) {
                    let ident = ::syntax::Ident::new(::syntax::__private::stringify!($ident), self.span);
                    ::syntax::__private::TokenStreamExt::append(tokens, ident);
                }
            }
        };
    }
    
    #[macro_export] macro_rules! impl_clone_for_custom_keyword
    {
        ($ident:ident) => {
            impl ::syntax::__private::Copy for $ident {}
            #[allow(clippy::expl_impl_clone_on_copy)]
            impl ::syntax::__private::Clone for $ident {
                fn clone(&self) -> Self {
                    *self
                }
            }
        };
    }
    
    #[macro_export] macro_rules! impl_extra_traits_for_custom_keyword 
    {
        ($ident:ident) => {
            impl ::syntax::__private::Debug for $ident {
                fn fmt(&self, f: &mut ::syntax::__private::Formatter) -> ::syntax::__private::FmtResult {
                    ::syntax::__private::Formatter::write_str(
                        f,
                        ::syntax::__private::concat!(
                            "Keyword [",
                            ::syntax::__private::stringify!($ident),
                            "]",
                        ),
                    )
                }
            }
            
            impl ::syntax::__private::Eq for $ident {}
            
            impl ::syntax::__private::PartialEq for $ident {
                fn eq(&self, _other: &Self) -> ::syntax::__private::bool {
                    true
                }
            }
            
            impl ::syntax::__private::Hash for $ident {
                fn hash<__H: ::syntax::__private::Hasher>(&self, _state: &mut __H) {}
            }
        };
    }
    /// Forward a method to an inherent method or a base trait method.
    #[macro_export] macro_rules! forward
    {
        ($( Self :: $method:ident ( self $( , $arg:ident : $ty:ty )* ) -> $ret:ty ; )*)
            => {$(
                #[inline] fn $method(self $( , $arg : $ty )* ) -> $ret {
                    Self::$method(self $( , $arg )* )
                }
            )*};
        ($( $base:ident :: $method:ident ( self $( , $arg:ident : $ty:ty )* ) -> $ret:ty ; )*)
            => {$(
                #[inline] fn $method(self $( , $arg : $ty )* ) -> $ret {
                    <Self as $base>::$method(self $( , $arg )* )
                }
            )*};
        ($( $base:ident :: $method:ident ( $( $arg:ident : $ty:ty ),* ) -> $ret:ty ; )*)
            => {$(
                #[inline] fn $method( $( $arg : $ty ),* ) -> $ret {
                    <Self as $base>::$method( $( $arg ),* )
                }
            )*};
        ($( $imp:path as $method:ident ( self $( , $arg:ident : $ty:ty )* ) -> $ret:ty ; )*)
            => {$(
                #[inline] fn $method(self $( , $arg : $ty )* ) -> $ret {
                    $imp(self $( , $arg )* )
                }
            )*};
    }

    #[macro_export] macro_rules! constant
    {
        ($( $method:ident () -> $ret:expr ; )*)
            => {$(
                #[inline] fn $method() -> Self {
                    $ret
                }
            )*};
    }
    
    #[macro_export] macro_rules! forward_ref_ref_binop
    {
        (impl $imp:ident, $method:ident) =>
        {
            impl<'a, 'b, T: Clone + Integer> $imp<&'b Ratio<T>> for &'a Ratio<T> {
                type Output = Ratio<T>;

                #[inline] fn $method(self, other: &'b Ratio<T>) -> Ratio<T> {
                    self.clone().$method(other.clone())
                }
            }
            
            impl<'a, 'b, T: Clone + Integer> $imp<&'b T> for &'a Ratio<T> {
                type Output = Ratio<T>;

                #[inline] fn $method(self, other: &'b T) -> Ratio<T> {
                    self.clone().$method(other.clone())
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_ref_ref_binop_big
    {
        (impl $imp:ident for $res:ty, $method:ident) =>
        {
            impl $imp<&$res> for &$res
            {
                type Output = $res;
                #[inline] fn $method(self, other: &$res) -> $res
                {
                    $imp::$method(self.clone(), other)
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_ref_val_binop
    {
        (impl $imp:ident, $method:ident) =>
        {
            impl<'a, T> $imp<Ratio<T>> for &'a Ratio<T> where
            T: Clone + Integer
            {
                type Output = Ratio<T>;
                #[inline] fn $method(self, other: Ratio<T>) -> Ratio<T> { self.clone().$method(other) }
            }
            
            impl<'a, T> $imp<T> for &'a Ratio<T> where
            T: Clone + Integer,
            {
                type Output = Ratio<T>;
                #[inline] fn $method(self, other: T) -> Ratio<T> { self.clone().$method(other) }
            }
        };
    }

    #[macro_export] macro_rules! forward_ref_val_binop_big
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            impl $imp<$res> for &$res {
                type Output = $res;

                #[inline] fn $method(self, other: $res) -> $res {
                   
                    $imp::$method(self, &other)
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_val_ref_binop
    {
        (impl $imp:ident, $method:ident) => {
            impl<'a, T> $imp<&'a Ratio<T>> for Ratio<T> where
                T: Clone + Integer,
            {
                type Output = Ratio<T>;

                #[inline] fn $method(self, other: &Ratio<T>) -> Ratio<T> {
                    self.$method(other.clone())
                }
            }
            
            impl<'a, T> $imp<&'a T> for Ratio<T> where
                T: Clone + Integer,
            {
                type Output = Ratio<T>;

                #[inline] fn $method(self, other: &T) -> Ratio<T> {
                    self.$method(other.clone())
                }
            }
        };
    }

    
    #[macro_export] macro_rules! forward_val_ref_binop_big
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            impl $imp<&$res> for $res {
                type Output = $res;

                #[inline] fn $method(self, other: &$res) -> $res {
                   
                    $imp::$method(&self, other)
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_all_binop
    {
        (impl $imp:ident, $method:ident) =>
        {
            forward_ref_ref_binop!(impl $imp, $method);
            forward_ref_val_binop!(impl $imp, $method);
            forward_val_ref_binop!(impl $imp, $method);
        };
    }

    #[macro_export] macro_rules! cfg_32
    {
        ($($any:tt)+) => {
            #[cfg(not(target_pointer_width = "64"))] $($any)+
        }
    }

    #[macro_export] macro_rules! cfg_32_or_test 
    {
        ($($any:tt)+) => {
            #[cfg(any(not(target_pointer_width = "64"), test))] $($any)+
        }
    }

    #[macro_export] macro_rules! cfg_64
    {
        ($($any:tt)+) => {
            #[cfg(target_pointer_width = "64")] $($any)+
        }
    }

    #[macro_export] macro_rules! cfg_digit
    {
        ($item32:item $item64:item) => {
            cfg_32!($item32);
            cfg_64!($item64);
        };
    }

    #[macro_export] macro_rules! cfg_digit_expr
    {
        ($expr32:expr, $expr64:expr) => {
            cfg_32!($expr32);
            cfg_64!($expr64);
        };
    }

    #[macro_export] macro_rules! forward_val_val_binop
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            impl $imp<$res> for $res {
                type Output = $res;

                #[inline] fn $method(self, other: $res) -> $res {
                   
                    $imp::$method(self, &other)
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_val_val_binop_commutative
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            impl $imp<$res> for $res {
                type Output = $res;

                #[inline] fn $method(self, other: $res) -> $res {
                   
                    if self.capacity() >= other.capacity() {
                        $imp::$method(self, &other)
                    } else {
                        $imp::$method(other, &self)
                    }
                }
            }
        };
    }
    /*
     */

    #[macro_export] macro_rules! forward_ref_val_binop_commutative
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            impl $imp<$res> for &$res {
                type Output = $res;

                #[inline] fn $method(self, other: $res) -> $res {
                   
                    $imp::$method(other, self)
                }
            }
        };
    }
    /*

     */

    #[macro_export] macro_rules! forward_ref_ref_binop_commutative
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            impl $imp<&$res> for &$res {
                type Output = $res;

                #[inline] fn $method(self, other: &$res) -> $res {
                   
                    if self.len() >= other.len() {
                        $imp::$method(self.clone(), other)
                    } else {
                        $imp::$method(other.clone(), self)
                    }
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_val_assign
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            impl $imp<$res> for $res {
                #[inline] fn $method(&mut self, other: $res) {
                    self.$method(&other);
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_val_assign_scalar
    {
        (impl $imp:ident for $res:ty, $scalar:ty, $method:ident) => {
            impl $imp<$res> for $scalar {
                #[inline] fn $method(&mut self, other: $res) {
                    self.$method(&other);
                }
            }
        };
    }
    
    #[macro_export] macro_rules! forward_scalar_val_val_binop_commutative
    {
        (impl $imp:ident < $scalar:ty > for $res:ty, $method:ident) => {
            impl $imp<$res> for $scalar {
                type Output = $res;

                #[inline] fn $method(self, other: $res) -> $res {
                    $imp::$method(other, self)
                }
            }
        };
    }
    
    #[macro_export] macro_rules! forward_scalar_val_val_binop_to_ref_val
    {
        (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
            impl $imp<$scalar> for $res {
                type Output = $res;

                #[inline] fn $method(self, other: $scalar) -> $res {
                    $imp::$method(&self, other)
                }
            }
            
            impl $imp<$res> for $scalar {
                type Output = $res;

                #[inline] fn $method(self, other: $res) -> $res {
                    $imp::$method(self, &other)
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_scalar_ref_ref_binop_to_ref_val
    {
        (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
            impl $imp<&$scalar> for &$res {
                type Output = $res;

                #[inline] fn $method(self, other: &$scalar) -> $res {
                    $imp::$method(self, *other)
                }
            }
            
            impl $imp<&$res> for &$scalar {
                type Output = $res;

                #[inline] fn $method(self, other: &$res) -> $res {
                    $imp::$method(*self, other)
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_scalar_val_ref_binop_to_ref_val 
    {
        (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
            impl $imp<&$scalar> for $res {
                type Output = $res;

                #[inline] fn $method(self, other: &$scalar) -> $res {
                    $imp::$method(&self, *other)
                }
            }
            
            impl $imp<$res> for &$scalar {
                type Output = $res;

                #[inline] fn $method(self, other: $res) -> $res {
                    $imp::$method(*self, &other)
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_scalar_val_ref_binop_to_val_val 
    {
        (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
            impl $imp<&$scalar> for $res {
                type Output = $res;

                #[inline] fn $method(self, other: &$scalar) -> $res {
                    $imp::$method(self, *other)
                }
            }
            
            impl $imp<$res> for &$scalar {
                type Output = $res;

                #[inline] fn $method(self, other: $res) -> $res {
                    $imp::$method(*self, other)
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_scalar_ref_val_binop_to_val_val 
    {
        (impl $imp:ident < $scalar:ty > for $res:ty, $method:ident) => {
            impl $imp<$scalar> for &$res {
                type Output = $res;

                #[inline] fn $method(self, other: $scalar) -> $res {
                    $imp::$method(self.clone(), other)
                }
            }
            
            impl $imp<&$res> for $scalar {
                type Output = $res;

                #[inline] fn $method(self, other: &$res) -> $res {
                    $imp::$method(self, other.clone())
                }
            }
        };
    }

    #[macro_export] macro_rules! forward_scalar_ref_ref_binop_to_val_val 
    {
        (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
            impl $imp<&$scalar> for &$res {
                type Output = $res;

                #[inline] fn $method(self, other: &$scalar) -> $res {
                    $imp::$method(self.clone(), *other)
                }
            }
            
            impl $imp<&$res> for &$scalar {
                type Output = $res;

                #[inline] fn $method(self, other: &$res) -> $res {
                    $imp::$method(*self, other.clone())
                }
            }
        };
    }

    #[macro_export] macro_rules! promote_scalars 
    {
        (impl $imp:ident<$promo:ty> for $res:ty, $method:ident, $( $scalar:ty ),*) => {
            $(
                forward_all_scalar_binop_to_val_val!(impl $imp<$scalar> for $res, $method);

                impl $imp<$scalar> for $res {
                    type Output = $res;

                    #[allow(clippy::cast_lossless)]
                    #[inline] fn $method(self, other: $scalar) -> $res {
                        $imp::$method(self, other as $promo)
                    }
                }
                impl $imp<$res> for $scalar {
                    type Output = $res;

                    #[allow(clippy::cast_lossless)]
                    #[inline] fn $method(self, other: $res) -> $res {
                        $imp::$method(self as $promo, other)
                    }
                }
            )*
        }
    }

    #[macro_export] macro_rules! promote_scalars_assign 
    {
        (impl $imp:ident<$promo:ty> for $res:ty, $method:ident, $( $scalar:ty ),*) => {
            $(
                impl $imp<$scalar> for $res {
                    #[allow(clippy::cast_lossless)]
                    #[inline] fn $method(&mut self, other: $scalar) {
                        self.$method(other as $promo);
                    }
                }
            )*
        }
    }

    #[macro_export] macro_rules! promote_unsigned_scalars 
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            promote_scalars!(impl $imp<u32> for $res, $method, u8, u16);
            promote_scalars!(impl $imp<::num::big::UsizePromotion> for $res, $method, usize);
        }
    }

    #[macro_export] macro_rules! promote_unsigned_scalars_assign 
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            promote_scalars_assign!(impl $imp<u32> for $res, $method, u8, u16);
            promote_scalars_assign!(impl $imp<::num::big::UsizePromotion> for $res, $method, usize);
        }
    }

    #[macro_export] macro_rules! promote_signed_scalars 
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            promote_scalars!(impl $imp<i32> for $res, $method, i8, i16);
            promote_scalars!(impl $imp<::num::big::IsizePromotion> for $res, $method, isize);
        }
    }

    #[macro_export] macro_rules! promote_signed_scalars_assign 
    {
        (impl $imp:ident for $res:ty, $method:ident) =>
        {
            promote_scalars_assign!(impl $imp<i32> for $res, $method, i8, i16);
            promote_scalars_assign!(impl $imp<::num::big::IsizePromotion> for $res, $method, isize);
        }
    }

    #[macro_export] macro_rules! forward_all_binop_to_ref_ref
    {
        (impl $imp:ident for $res:ty, $method:ident) =>
        {
            forward_val_val_binop!(impl $imp for $res, $method);
            forward_val_ref_binop_big!(impl $imp for $res, $method);
            forward_ref_val_binop_big!(impl $imp for $res, $method);
        };
    }
    
    #[macro_export] macro_rules! forward_all_binop_to_val_ref
    {
        (impl $imp:ident for $res:ty, $method:ident) =>
        {
            forward_val_val_binop!(impl $imp for $res, $method);
            forward_ref_val_binop!(impl $imp for $res, $method);
            forward_ref_ref_binop!(impl $imp for $res, $method);
        };
    }
    
    #[macro_export] macro_rules! forward_all_binop_to_val_ref_commutative
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            forward_val_val_binop_commutative!(impl $imp for $res, $method);
            forward_ref_val_binop_commutative!(impl $imp for $res, $method);
            forward_ref_ref_binop_commutative!(impl $imp for $res, $method);
        };
    }

    #[macro_export] macro_rules! forward_all_scalar_binop_to_ref_val
    {
        (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
            forward_scalar_val_val_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
            forward_scalar_val_ref_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
            forward_scalar_ref_ref_binop_to_ref_val!(impl $imp<$scalar> for $res, $method);
        }
    }

    #[macro_export] macro_rules! forward_all_scalar_binop_to_val_val
    {
        (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
            forward_scalar_val_ref_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
            forward_scalar_ref_val_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
            forward_scalar_ref_ref_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
        }
    }

    #[macro_export] macro_rules! forward_all_scalar_binop_to_val_val_commutative
    {
        (impl $imp:ident<$scalar:ty> for $res:ty, $method:ident) => {
            forward_scalar_val_val_binop_commutative!(impl $imp<$scalar> for $res, $method);
            forward_all_scalar_binop_to_val_val!(impl $imp<$scalar> for $res, $method);
        }
    }

    #[macro_export] macro_rules! promote_all_scalars
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            promote_unsigned_scalars!(impl $imp for $res, $method);
            promote_signed_scalars!(impl $imp for $res, $method);
        }
    }

    #[macro_export] macro_rules! promote_all_scalars_assign
    {
        (impl $imp:ident for $res:ty, $method:ident) => {
            promote_unsigned_scalars_assign!(impl $imp for $res, $method);
            promote_signed_scalars_assign!(impl $imp for $res, $method);
        }
    }

    #[macro_export] macro_rules! impl_sum_iter_type
    {
        ($res:ty) => {
            impl<T> Sum<T> for $res
            where
                $res: Add<T, Output = $res>,
            {
                fn sum<I>(iter: I) -> Self
                where
                    I: Iterator<Item = T>,
                {
                    iter.fold(Self::ZERO, <$res>::add)
                }
            }
        };
    }

    #[macro_export] macro_rules! impl_product_iter_type
    {
        ($res:ty) => {
            impl<T> Product<T> for $res
            where
                $res: Mul<T, Output = $res>,
            {
                fn product<I>(iter: I) -> Self
                where
                    I: Iterator<Item = T>,
                {
                    iter.fold(One::one(), <$res>::mul)
                }
            }
        };
    }

    #[macro_export] macro_rules! map
    {
        { } => { ::collections::HashMap::new() };
        { $( $key:expr => $value:expr ),+ , } =>
        {
           
            map!{ $( $key => $value),+ }
        };
        { $( $key:expr => $value:expr ),* } =>
        {
            {
                let mut _map = ::collections::HashMap::new();

                $(
                    let _ = _map.insert($key, $value);
                )*

                _map
            }
        }
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
            ::num::rational::BigRational::new($int1.into(), $int2.into())
        }};
    }
    /// With a list of items, converts each item to a `Value` and returns an `Arr` containing a vector of the values.
    #[macro_export] macro_rules! arr 
    {
        [] => 
        {
            $crate::arrays::Arr::from_vec(vec![]).unwrap()
        };

        [ $( $elem:expr ),+ , ] => 
        {
           
            try_arr![ $( $elem ),+ ].unwrap()
        };

        [ $( $elem:expr ),+ ] => 
        {
            try_arr![ $( $elem ),+ ].unwrap()
        };
    }
    /// With a list of items, converts each item to a `Value` and returns an `Arr` containing a vector of the values.
    #[macro_export] macro_rules! try_arr 
    {
        [ $( $elem:expr ),+ , ] => 
        {
           
            try_arr![ $( $elem ),+ ]
        };

        [ $( $elem:expr ),+ ] => 
        {{
                $crate::arrays::Arr::from_vec(vec![ $( $elem.into() ),+ ])
        }};
    }
    /// With a list of items, converts each item to `Value`s and returns a `Tup` containing a vector of the values.
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
    /// With a list of field/value pairs, returns an `Obj` containing each pair.
    #[macro_export] macro_rules! obj 
    {
        {} => {
            $crate::obj::Obj::from_map_unchecked(::std::collections::HashMap::new())
        };
        { $( $field:expr => $inner:expr ),+ , } => {
           
            try_obj!{ $( $field => $inner ),+ }.unwrap()
        };
        { $( $field:expr => $inner:expr ),+ } => {
            try_obj!{ $( $field => $inner ),+ }.unwrap()
        };
    }
    /// With a list of field to `Value` pairs, returns an `Obj` with the fields and values.
    #[macro_export] macro_rules! try_obj
    {
        { $( $field:expr => $inner:expr ),+ , } =>
        {
           
            try_obj!{ $( $field => $inner ),* };
        };
        
        { $( $field:expr => $inner:expr ),+ } =>
        {{
            use ::objects::Obj;
            let mut _map = ::collections::HashMap::new();
            let mut _parent: Option<::values::Value> = None;

            $(
                if $field == "^" 
                {
                    _parent = Some($inner.into());
                } 
                
                else 
                {
                    _map.insert($field.into(), $inner.into());
                }
            )*

            match _parent 
            {
                Some(parent) => match parent.get_obj() {
                    Ok(parent) => Obj::from_map_with_parent(_map, parent),
                    e @ Err(_) => e,
                }
                None => Obj::from_map(_map),
            }
        }};
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
    use ::
    {
        error::{ OverError },
        fmt::{ Format },
        result::{ OverResult },
        slice::{ Iter },
        sync::{ Arc },
        types::{ Type },
        values::{ Value },
        *,
    };
    /*
    */
    #[derive(Clone, Debug)]
    struct ArrInner 
    {
        vec: Vec<Value>,
        inner_t: Type,
    }
    /// `Arr` struct.
    #[derive(Clone, Debug)]
    pub struct Arr 
    {
        inner: Arc<ArrInner>,
    }

    impl Arr 
    {
        /// Returns a new `Arr` from the given vector of `Value`s.
        pub fn from_vec(vec: Vec<Value>) -> OverResult<Arr> 
        {
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
        /// Returns a new `Arr` from the given vector of `Value`s without checking 
        /// whether every value in `vec` is the same type.
        pub fn from_vec_unchecked(vec: Vec<Value>, inner_t: Type) -> Arr 
        {
            Arr {
                inner: Arc::new(ArrInner { vec, inner_t }),
            }
        }
        /// Returns a reference to the inner vec of this `Arr`.
        pub fn vec_ref(&self) -> &Vec<Value> 
        {
            &self.inner.vec
        }
        /// Iterates over each `Value` in `self`, applying `Fn` `f`.
        pub fn with_each<F>(&self, mut f: F) where
            F: FnMut(&Value),
        {
            for value in &self.inner.vec {
                f(value)
            }
        }
        /// Gets the value at `index`.
        pub fn get(&self, index: usize) -> OverResult<Value> 
        {
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
        pub fn ptr_eq(&self, other: &Self) -> bool 
        {
            Arc::ptr_eq(&self.inner, &other.inner)
        }
        /// Returns an iterator over the Arr.
        pub fn iter(&self) -> Iter<Value> 
        {
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
           
            if self.inner.inner_t != other.inner.inner_t {
                return false;
            }
            self.inner.vec == other.inner.vec
        }
    }
}

pub mod ascii
{
    pub use std::ascii::{ * };
    
    const T: bool = true;
    const F: bool = false;

    #[repr(C, align(8))]
    pub struct Align8<T>(pub T);
    #[repr(C, align(64))]
    pub struct Align64<T>(pub T);

    pub static ASCII_START: Align64<[bool; 128]> = Align64
    ([
        F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
        F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, T, T, T, T, T, T, T, T, T,
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
        T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F,
    ]);

    pub static ASCII_CONTINUE: Align64<[bool; 128]> = Align64
    ([
        F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, 
        F, F, F, F, F, F, F, F, F, F, F, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F, F, F, T, T, T, T, T, T, T, T, T, 
        T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, T, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, 
        T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F,
    ]);

    pub const CHUNK: usize = 64;

    pub static TRIE_START: Align8<[u8; 411]> = Align8
    ([
        0x04, 0x0B, 0x0F, 0x13, 0x17, 0x1B, 0x1F, 0x23, 0x27, 0x2D, 0x31, 0x34, 0x38, 0x3C, 0x40, 0x02, 0x45, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x49, 0x00, 0x4D, 0x00, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x06, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x51, 0x54, 0x58, 0x5C, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x09, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x60, 
        0x64, 0x66, 0x6A, 0x6E, 0x72, 0x28, 0x76, 0x78, 0x7C, 0x80, 0x84, 0x88, 0x8C, 0x90, 0x94, 0x98, 0x9C, 0xA0, 
        0x05, 0x2B, 0xA4, 0x00, 0x00, 0x00, 0x00, 0xA6, 0x05, 0x05, 0xA8, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x32, 
        0x05, 0xAD, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xAE, 0x00, 0x00, 0x00, 
        0x05, 0xB2, 0xB6, 0xBA, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0xBE, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 
        0x43, 0xC2, 0x00, 0x00, 0x00, 0x00, 0xC5, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0xD1, 0xD3, 0x00, 0x00, 0x00, 0xC9, 0xD9, 0xDD, 0xE1, 0xE5, 0xE9, 0x00, 0x00, 0xED, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xEF, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xF1, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xF3, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x52, 0x05, 0xF5, 0x00, 0x00, 
        0x00, 0x00, 0x05, 0xAF, 0x00, 0x00, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xA9, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xF7,
    ]);

    pub static TRIE_CONTINUE: Align8<[u8; 1793]> = Align8
    ([
        0x08, 0x0D, 0x11, 0x15, 0x19, 0x1D, 0x21, 0x25, 0x2A, 0x2F, 0x31, 0x36, 0x3A, 0x3E, 0x42, 0x02,0x47, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x4B, 0x00, 0x4F, 0x00, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x06, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x51, 0x56, 0x5A, 0x5E, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x09, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x62, 
        0x64, 0x68,0x6C, 0x70, 0x74, 0x28, 0x76, 0x7A, 0x7E, 0x82, 0x86, 0x8A, 0x8E, 0x92, 0x96, 0x9A, 0x9E, 0xA2,
        0x05, 0x2B, 0xA4, 0x00, 0x00, 0x00, 0x00, 0xA6, 0x05, 0x05, 0xAB, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x32, 
        0x05, 0xAD, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0xB0, 0x00, 0x00, 0x00, 
        0x05, 0xB4, 0xB8, 0xBC, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0xBE, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 
        0x43, 0xC2, 0x00, 0x00, 0x00, 0x00, 0xC8, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xCB, 0xC3, 0xC6, 0xCE, 
        0xD1, 0xD5, 0x00, 0xD7, 0x00, 0xC9,0xDB, 0xDF, 0xE3, 0xE7, 0xEB, 0x00, 0x00, 0xED, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0xCC, 0x00, 0x00,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0xEF, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xF1, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xF3, 
        0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 0x05, 0x52, 0x05, 0xF5, 0x00, 0x00, 
        0x00, 0x00, 0x05, 0xAF, 0x00, 0x00,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xA9, 0x05, 0x05, 
        0x05, 0x05, 0x05, 0x05,0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0xF7, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,0xCF,
    ]);

    pub static LEAF: Align64<[u8; 7968]> = Align64
    ([
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x3F, 0xFF, 0xAA, 
        0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xDF, 0x5F, 0xDC, 0x1F, 0xCF, 0x0F, 0xFF, 0x1F, 
        0xDC, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x20, 0x04, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0xA0, 0x04, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00,
        0xFF, 0xFF, 0x7F, 0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xC3, 0xFF, 
        0x03, 0x00, 0x1F, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0x00, 0x00, 0xDF, 0xB8, 0x40, 0xD7, 0xFF, 0xFF, 0xFB, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xBF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xC3, 0xFF, 0x03, 0x00, 0x1F, 0x50, 0x00, 0x00, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xDF, 0xB8, 0xC0, 0xD7, 0xFF, 0xFF,
        0xFB, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0xFF, 0xFF, 0xFF,
        0x7F, 0x02, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
        0xFF, 0x87, 0x07, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFB, 0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0xFF, 0xFF, 0xFF, 0x7F, 0x02, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01,
        0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0xB6, 0x00, 0xFF, 0xFF, 0xFF, 0x87, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x00, 0xC0, 0xFE, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x2F, 0x00, 0x60, 0xC0, 0x00, 0x9C,
        0x00, 0x00, 0xFD, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xE0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x02, 0x00, 0x00, 0xFC, 0xFF, 0xFF, 0xFF, 0x07, 0x30, 0x04,
        0x00, 0x00, 0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xC3, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xEF, 0x9F, 0xFF, 0xFD, 0xFF, 0x9F,
        0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xE7, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x24,
        0xFF, 0xFF, 0x3F, 0x04, 0x10, 0x01, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x01, 0xFF, 0x07, 0xFF, 0xFF,
        0xFF, 0xFE, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xF0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x23, 0x00, 0x00, 0x01, 0xFF, 0x03, 0x00, 0xFE, 0xFF,
        0xE1, 0x9F, 0xF9, 0xFF, 0xFF, 0xFD, 0xC5, 0x23, 0x00, 0x40, 0x00, 0xB0, 0x03, 0x00, 0x03, 0x10,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0x07, 0xFF, 0xFF,
        0xFF, 0xFE, 0x80, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFB, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCF, 0xFF, 0xFE, 0xFF,
        0xEF, 0x9F, 0xF9, 0xFF, 0xFF, 0xFD, 0xC5, 0xF3, 0x9F, 0x79, 0x80, 0xB0, 0xCF, 0xFF, 0x03, 0x50,
        0xE0, 0x87, 0xF9, 0xFF, 0xFF, 0xFD, 0x6D, 0x03, 0x00, 0x00, 0x00, 0x5E, 0x00, 0x00, 0x1C, 0x00,
        0xE0, 0xBF, 0xFB, 0xFF, 0xFF, 0xFD, 0xED, 0x23, 0x00, 0x00, 0x01, 0x00, 0x03, 0x00, 0x00, 0x02,
        0xE0, 0x9F, 0xF9, 0xFF, 0xFF, 0xFD, 0xED, 0x23, 0x00, 0x00, 0x00, 0xB0, 0x03, 0x00, 0x02, 0x00,
        0xE8, 0xC7, 0x3D, 0xD6, 0x18, 0xC7, 0xFF, 0x03, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xEE, 0x87, 0xF9, 0xFF, 0xFF, 0xFD, 0x6D, 0xD3, 0x87, 0x39, 0x02, 0x5E, 0xC0, 0xFF, 0x3F, 0x00,
        0xEE, 0xBF, 0xFB, 0xFF, 0xFF, 0xFD, 0xED, 0xF3, 0xBF, 0x3B, 0x01, 0x00, 0xCF, 0xFF, 0x00, 0xFE,
        0xEE, 0x9F, 0xF9, 0xFF, 0xFF, 0xFD, 0xED, 0xF3, 0x9F, 0x39, 0xE0, 0xB0, 0xCF, 0xFF, 0x02, 0x00,
        0xEC, 0xC7, 0x3D, 0xD6, 0x18, 0xC7, 0xFF, 0xC3, 0xC7, 0x3D, 0x81, 0x00, 0xC0, 0xFF, 0x00, 0x00,
        0xE0, 0xDF, 0xFD, 0xFF, 0xFF, 0xFD, 0xFF, 0x23, 0x00, 0x00, 0x00, 0x37, 0x03, 0x00, 0x00, 0x00,
        0xE1, 0xDF, 0xFD, 0xFF, 0xFF, 0xFD, 0xEF, 0x23, 0x00, 0x00, 0x00, 0x70, 0x03, 0x00, 0x06, 0x00,
        0xF0, 0xDF, 0xFD, 0xFF, 0xFF, 0xFF, 0xFF, 0x27, 0x00, 0x40, 0x70, 0x80, 0x03, 0x00, 0x00, 0xFC,
        0xE0, 0xFF, 0x7F, 0xFC, 0xFF, 0xFF, 0xFB, 0x2F, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xDF, 0xFD, 0xFF, 0xFF, 0xFD, 0xFF, 0xF3, 0xDF, 0x3D, 0x60, 0x37, 0xCF, 0xFF, 0x00, 0x00,
        0xEF, 0xDF, 0xFD, 0xFF, 0xFF, 0xFD, 0xEF, 0xF3, 0xDF, 0x3D, 0x60, 0x70, 0xCF, 0xFF, 0x0E, 0x00,
        0xFF, 0xDF, 0xFD, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xDF, 0x7D, 0xF0, 0x80, 0xCF, 0xFF, 0x00, 0xFC,
        0xEE, 0xFF, 0x7F, 0xFC, 0xFF, 0xFF, 0xFB, 0x2F, 0x7F, 0x84, 0x5F, 0xFF, 0xC0, 0xFF, 0x0C, 0x00,
        0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x05, 0x00, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xD6, 0xF7, 0xFF, 0xFF, 0xAF, 0xFF, 0x05, 0x20, 0x5F, 0x00, 0x00, 0xF0, 0x00, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFE, 0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x00,
        0x00, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0x7F, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00,
        0xD6, 0xF7, 0xFF, 0xFF, 0xAF, 0xFF, 0xFF, 0x3F, 0x5F, 0x7F, 0xFF, 0xF3, 0x00, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x03, 0xFF, 0x03, 0xA0, 0xC2, 0xFF, 0xFE, 0xFF, 0xFF, 0xFF, 0x1F, 0xFE, 0xFF,
        0xDF, 0xFF, 0xFF, 0xFE, 0xFF, 0xFF, 0xFF, 0x1F, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x80, 0x00, 0x00, 0x3F, 0x3C, 0x62, 0xC0, 0xE1, 0xFF,
        0x03, 0x40, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0x20, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xF7,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0x00, 0x00, 0x00,
        0xBF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFD, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0x20, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xF7,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3D, 0x7F, 0x3D, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x3D, 0xFF, 0xFF, 0xFF, 0xFF, 0x3D, 0x7F, 0x3D, 0xFF, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0x3D, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x3F,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3D, 0x7F, 0x3D, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x3D, 0xFF, 0xFF, 0xFF, 0xFF, 0x3D, 0x7F, 0x3D, 0xFF, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0x3D, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xE7, 0x00, 0xFE, 0x03, 0x00,
        0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x3F,
        0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x9F, 0xFF, 0xFF,
        0xFE, 0xFF, 0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xC7, 0xFF, 0x01,
        0xFF, 0xFF, 0x03, 0x80, 0xFF, 0xFF, 0x03, 0x00, 0xFF, 0xFF, 0x03, 0x00, 0xFF, 0xDF, 0x01, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x80, 0x10, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x9F, 0xFF, 0xFF,
        0xFE, 0xFF, 0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xC7, 0xFF, 0x01,
        0xFF, 0xFF, 0x3F, 0x80, 0xFF, 0xFF, 0x1F, 0x00, 0xFF, 0xFF, 0x0F, 0x00, 0xFF, 0xDF, 0x0D, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x8F, 0x30, 0xFF, 0x03, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x05, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00,
        0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x3F, 0x1F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0xB8, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00,
        0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0x0F, 0xFF, 0x0F, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x1F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF, 0xFF, 0x03, 0xFF, 0x07, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0x7F, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xE0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0xE0, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xF8, 0xFF, 0xFF, 0xFF, 0x01, 0xC0, 0x00, 0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0xFF, 0x9F,
        0xFF, 0x03, 0xFF, 0x03, 0x80, 0x00, 0xFF, 0xBF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0x0F, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0xFF, 0x03, 0x00, 0xF8, 0x0F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0xE0, 0x00, 0xFC, 0xFF, 0xFF, 0xFF, 0x3F,
        0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xE7, 0x00, 0x00, 0x00, 0x00, 0x00, 0xDE, 0x6F, 0x04,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xE3, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F,
        0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xE7, 0x00, 0x00, 0xF7, 0xFF, 0xFF, 0xFF, 0xFF, 0x07,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x04, 0x00, 0x00, 0x00, 0x27, 0x00, 0xF0, 0x00, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x80,
        0x00, 0x00, 0xFF, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x84, 0xFC, 0x2F, 0x3F, 0x50, 0xFD, 0xFF, 0xF3, 0xE0, 0x43, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x10, 0x00, 0x00, 0x00, 0x02, 0x80,
        0x00, 0x00, 0xFF, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x1F, 0xE2, 0xFF, 0x01, 0x00,
        0x84, 0xFC, 0x2F, 0x3F, 0x50, 0xFD, 0xFF, 0xF3, 0xE0, 0x43, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x78, 0x0C, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0x20, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x80, 0x00, 0x00,
        0xFF, 0xFF, 0x7F, 0x00, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0xF8, 0x0F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0x20, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x80, 0x00, 0x80,
        0xFF, 0xFF, 0x7F, 0x00, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF,
        0xE0, 0x00, 0x00, 0x00, 0xFE, 0x03, 0x3E, 0x1F, 0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0x7F, 0xE0, 0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xF7,
        0xE0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x7F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
        0xE0, 0x00, 0x00, 0x00, 0xFE, 0xFF, 0x3E, 0x1F, 0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0x7F, 0xE6, 0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xE0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x7F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0xFF, 0xFF,
        0xFF, 0x1F, 0xFF, 0xFF, 0x00, 0x0C, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x80,
        0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00,
        0x00, 0x00, 0x80, 0xFF, 0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xF9, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0xFE, 0xFF,
        0xFF, 0x1F, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xF0, 0xBF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00,
        0x00, 0x00, 0x80, 0xFF, 0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xF9, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0xFE, 0xFF,
        0xBB, 0xF7, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00,
        0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFC, 0x68,
        0x00, 0xFC, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x1F,
        0xF0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x80, 0x00, 0x00, 0xDF, 0xFF, 0x00, 0x7C,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x10, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0xE8,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0xFF, 0xFF, 0xFF, 0x1F,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x80, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0x7F,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x00, 0xF7, 0x0F, 0x00, 0x00, 0xFF, 0xFF, 0x7F, 0xC4,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x62, 0x3E, 0x05, 0x00, 0x00, 0x38, 0xFF, 0x07, 0x1C, 0x00,
        0x7E, 0x7E, 0x7E, 0x00, 0x7F, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xF7, 0xFF, 0x03, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0xFF, 0x3F, 0xFF, 0x03, 0xFF, 0xFF, 0x7F, 0xFC,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x38, 0xFF, 0xFF, 0x7C, 0x00,
        0x7E, 0x7E, 0x7E, 0x00, 0x7F, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xF7, 0xFF, 0x03, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x37, 0xFF, 0x03,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00,
        0x7F, 0x00, 0xF8, 0xA0, 0xFF, 0xFD, 0x7F, 0x5F, 0xDB, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00,
        0x7F, 0x00, 0xF8, 0xE0, 0xFF, 0xFD, 0x7F, 0x5F, 0xDB, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xF0, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x03,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x8A, 0xAA,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F,
        0x00, 0x00, 0x00, 0x00, 0xFE, 0xFF, 0xFF, 0x07, 0xFE, 0xFF, 0xFF, 0x07, 0xC0, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0x7F, 0xFC, 0xFC, 0xFC, 0x1C, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0x18, 0x00, 0x00, 0xE0, 0x00, 0x00, 0x00, 0x00, 0x8A, 0xAA,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F,
        0x00, 0x00, 0xFF, 0x03, 0xFE, 0xFF, 0xFF, 0x87, 0xFE, 0xFF, 0xFF, 0x07, 0xE0, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0xFC, 0xFC, 0xFC, 0x1C, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xEF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0xB7, 0xFF, 0x3F, 0xFF, 0x3F, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xEF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0xB7, 0xFF, 0x3F, 0xFF, 0x3F, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0x1F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xE0, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00,
        0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0x3E, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0x1F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xE0, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07,
        0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0x3E, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0xFF, 0xF7,
        0xFF, 0xF7, 0xB7, 0xFF, 0xFB, 0xFF, 0xFB, 0x1B, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0xFF, 0xF7,
        0xFF, 0xF7, 0xB7, 0xFF, 0xFB, 0xFF, 0xFB, 0x1B, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00,
        0x3F, 0xFD, 0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0x91, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0xFF, 0x7F, 0x00,
        0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x37, 0x00,
        0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0xFF, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x01, 0x00, 0xEF, 0xFE, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x1F,
        0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFE, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0xFF, 0x07, 0x00,
        0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x6F, 0xF0, 0xEF, 0xFE, 0xFF, 0xFF, 0x3F, 0x87, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x1F,
        0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFE, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0xFF, 0x07, 0x00,
        0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0xFC, 0xFF, 0xFF, 0x3F, 0x80, 0xFF, 0xFF,
        0x3F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xBE, 0xFF, 0xFF,
        0x3F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x03, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0x1F, 0x80, 0x00, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
        0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x7F, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1B, 0x03, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFC,
        0xFF, 0xFF, 0xFF, 0x1F, 0x80, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x00, 0x00, 0xFF, 0xFF,
        0x3F, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x7F, 0x00,
        0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x26, 0x00,
        0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x00,
        0xF8, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x90, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x47, 0x00,
        0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x1E, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0xC0, 0xFF, 0x3F, 0x80,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x04, 0x00, 0xFF, 0xFF, 0xFF, 0x01, 0xFF, 0x03,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xDF, 0xFF, 0xF0, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x4F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0xDE, 0xFF, 0x17, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFB, 0xFF, 0xFF, 0x0F, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x7F, 0xBD, 0xFF, 0xBF, 0xFF, 0x01, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00,
        0xE0, 0x9F, 0xF9, 0xFF, 0xFF, 0xFD, 0xED, 0x23, 0x00, 0x00, 0x01, 0xE0, 0x03, 0x00, 0x00, 0x00,
        0xFF, 0x4B, 0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0x00, 0x00, 0x00, 0x0A, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFB, 0xFF, 0xFF, 0xFF, 0xFF, 0xC0, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x7F, 0xBD, 0xFF, 0xBF, 0xFF, 0x01, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0x03,
        0xEF, 0x9F, 0xF9, 0xFF, 0xFF, 0xFD, 0xED, 0xFB, 0x9F, 0x39, 0x81, 0xE0, 0xCF, 0x1F, 0x1F, 0x00,
        0xFF, 0x4B, 0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0xFF, 0xA5, 0xF7, 0x0F, 0x00, 0x06, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x80, 0x07, 0x00, 0x80, 0x03, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xB0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0xC3, 0x03, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xBF, 0x00, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0x01, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x00, 0x00, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x11, 0x00, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0xFF, 0x03, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xE7, 0xFF, 0x0F, 0xFF, 0x03, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x80,
        0x7F, 0xF2, 0x6F, 0xFF, 0xFF, 0xFF, 0x00, 0x80, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0xFF, 0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x0A, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x80,
        0x7F, 0xF2, 0x6F, 0xFF, 0xFF, 0xFF, 0xBF, 0xF9, 0x0F, 0x00, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0xFF, 0xFC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFC, 0x1B, 0x00, 0x00, 0x00,
        0x01, 0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x04, 0x00, 0x00, 0x01, 0xF0, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x03, 0x00, 0x20, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x80, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0x23, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0xFF, 0x03,
        0xFF, 0xFD, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFC, 0xFF,
        0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x7F, 0xFB, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00, 0x40, 0x00, 0x00, 0x00, 0xBF, 0xFD, 0xFF, 0xFF,
        0xFF, 0x03, 0x00, 0x01, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFD, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0x01, 0x00, 0xFF, 0x03, 0x00, 0x00, 0xFC, 0xFF,
        0xFF, 0xFF, 0xFC, 0xFF, 0xFF, 0xFE, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x7F, 0xFB, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0xB4, 0xFF, 0x00, 0xFF, 0x03, 0xBF, 0xFD, 0xFF, 0xFF,
        0xFF, 0x7F, 0xFB, 0x01, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0x03, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x07, 0x00,
        0xF4, 0xFF, 0xFD, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x7F, 0x00,
        0xFF, 0xFF, 0xFD, 0xFF, 0xFF, 0xFF, 0xFF, 0xC7, 0x07, 0x00, 0xFF, 0x07, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x7E, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0x3F, 0x00, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0xF8, 0xFF, 0xFF, 0xE0,
        0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0x03, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0x3F, 0x1F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x0F, 0x00, 0xFF, 0x03, 0xF8, 0xFF, 0xFF, 0xE0,
        0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0xFF, 0x03,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xF9, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xF8, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0B, 0x00, 0x7C, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xF9, 0xFF, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x87, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x80, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1B, 0x00, 0x7F, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0x00, 0x00, 0x80,
        0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xEF, 0x6F,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0x1F,
        0xFF, 0x01, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xE0, 0xE3, 0x07, 0xF8,
        0xE7, 0x0F, 0x00, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07, 0xFF, 0x1F,
        0xFF, 0x01, 0xFF, 0x63, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0x7F, 0xE0, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x03,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x03,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xDF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xDF, 0x64, 0xDE, 0xFF, 0xEB, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xBF, 0xE7, 0xDF, 0xDF, 0xFF, 0xFF, 0xFF, 0x7B, 0x5F, 0xFC, 0xFD, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFD, 0xFF, 0xFF, 0xF7, 0xFF, 0xFF, 0xFF, 0xF7,
        0xFF, 0xFF, 0xDF, 0xFF, 0xFF, 0xFF, 0xDF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF,
        0xFF, 0xFD, 0xFF, 0xFF, 0xFF, 0xFD, 0xFF, 0xFF, 0xF7, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFD, 0xFF, 0xFF, 0xF7, 0xFF, 0xFF, 0xFF, 0xF7,
        0xFF, 0xFF, 0xDF, 0xFF, 0xFF, 0xFF, 0xDF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF,
        0xFF, 0xFD, 0xFF, 0xFF, 0xFF, 0xFD, 0xFF, 0xFF, 0xF7, 0xCF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F, 0xF8, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x20, 0x00,
        0x10, 0x00, 0x00, 0xF8, 0xFE, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x80, 0x3F, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x7F, 0xFF, 0xFF, 0xF9, 0xDB, 0x07, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00,
        0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0xFF, 0x3F, 0xFF, 0x43, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x0F, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x3F, 0x01, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x07,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x7F, 0xB7, 0x3F, 0x1F, 0xC0,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7F, 0x6F, 0xFF, 0x7F,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x7F, 0xFF, 0xFF, 0x3F, 0xC0,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7F, 0x6F, 0xFF, 0x7F,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1F, 0x00, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0F, 0xFF, 0x03, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xEF, 0xFF, 0xFF, 0xFF, 0x96, 0xFE, 0xF7, 0x0A, 0x84, 0xEA, 0x96, 0xAA, 0x96, 0xF7, 0xF7, 0x5E,
        0xFF, 0xFB, 0xFF, 0x0F, 0xEE, 0xFB, 0xFF, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x3F, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x03,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ]);
}

pub mod backtrace
{
    pub use std::backtrace::{ * };
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
    use ::
    {
        cell::{ RefCell },
        fs::{ File },
        io::{ self, Read },
        iter::{ Peekable },
        rc::{ Rc },
        str::{ Chars },
        *,
    };
    /*
    */
    #[allow( dead_code )]
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
        pub fn from_file(path: &str) -> io::Result<CharStream> 
        {
            let mut file = File::open(path)?;

            let len = file.metadata()?.len();
            let mut contents = String::with_capacity(len as usize);

            file.read_to_string(&mut contents)?;

            Self::from_string_impl(Some(String::from(path)), contents)
        }

        fn from_string_impl(file: Option<String>, contents: String) -> io::Result<CharStream> 
        {
            let chars: Chars = unsafe { mem::transmute(contents.chars()) };
            let stream = chars.peekable();
            Ok(CharStream 
            {
                inner: Rc::new(RefCell::new(Inner 
                {
                    file,
                    contents,
                    stream,
                    line: 1,
                    col: 1,
                })),
            })
        }

        pub fn from_string(contents: String) -> io::Result<CharStream> 
        {
            Self::from_string_impl(None, contents)
        }

        pub fn peek(&self) -> Option<char> 
        {
            let mut inner = self.inner.borrow_mut();
            let opt = inner.stream.peek();

            match opt {
                Some(ch) => Some(*ch),
                None => None,
            }
        }

        pub fn file(&self) -> Option<String> 
        {
            let inner = self.inner.borrow();
            inner.file.clone()
        }

        pub fn line(&self) -> usize 
        {
            let inner = self.inner.borrow();
            inner.line
        }

        pub fn col(&self) -> usize 
        {
            let inner = self.inner.borrow();
            inner.col
        }

        fn set_line(&mut self, value: usize) 
        {
            let mut inner = self.inner.borrow_mut();
            inner.line = value;
        }

        fn set_col(&mut self, value: usize) 
        {
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

    pub fn format(ch: char) -> String 
    {
        match ch {
            '\n' => String::from("\\n"),
            ch => format!("{}", ch),
        }
    }
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

pub mod error
{
    /*!
    Error Handling */
    pub use std::error::{ * };

    use ::
    {
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
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
        {
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
        fn description(&self) -> &str
        {
            use self::OverError::*;

            match *self 
            {
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
        fn from(e: io::Error) -> Self { OverError::IoError(format!("{}", e)) }
    }

    impl From<ParseError> for OverError
    {
        fn from(e: ParseError) -> Self
        {
            OverError::ParseError(format!("{}", e))
        }
    }

    pub mod parse
    {
        /*!
        */
        use ::
        {
            char::{ format },
            error::{ Error, OverError },
            num::
            {
                big::{ BigInt, ParseBigIntError },
                ParseIntError
            },
            objects::{ Obj },
            parses::{ MAX_DEPTH, object_file, object_from_str },
            result::{ ParseResult },
            types::{ Type },
            *,
        };

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

                if let Some(ref file) = (*self).file
                {
                    write!(f, "{}: ", file)?;
                }
                match (*self).kind
                {
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
                        format(*ch),
                        line,
                        col
                    ),
                    InvalidFieldChar(ref ch, ref line, ref col) => write!(
                        f,
                        "Invalid character '{}' for field at line {}, column {}",
                        format(*ch),
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
                        format(*ch),
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
            fn description(&self) -> &str {
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
            pub fn from_over(e: &OverError, file: Option<String>, line: usize, col: usize) -> Self {
                ParseError {
                    file,
                    kind: ParseErrorKind::OverError(format!("{} at line {}, col {}", e, line, col)),
                }
            }
        }

        impl From<()> for ParseError 
        {
            fn from(e:()) -> Self {
                ParseError {
                    file: None,
                    kind: ParseErrorKind::IoError(format!("()")),
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
            fn from(e: ParseBigIntError) -> Self {
                ParseError {
                    file: None,
                    kind: ParseErrorKind::ParseIntError(format!("{}", e)),
                }
            }
        }
    } pub use self::parse::{ ParseError };
}

pub mod get
{
    /*!
    Getter Setter Functionality*/
    use ::
    {
        *,
    };
    /*
    */
    /// If `ch` preceded by a backslash together form an escape character, then return this char. Otherwise, return None.
   
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
    Module containing functions for formatting output of objects. */
    pub use std::fmt::{ * };    
    use ::
    {
        arrays::{ Arr },
        num::
        {
            big::{ BigInt },
            rational::{ BigRational },
            traits::{ One },
        },
        objects::{ Obj },
        tuples::{ Tup },
        values::{ Value },
        *,
    };
    /*
    use crate::INDENT_STEP;
    */
   
    fn indent(amount: usize) -> String 
    {
        " ".repeat(amount)
    }

    fn get_char_map(ch: char) -> Option<&'static str> 
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

    fn replace_all(s: &str) -> String 
    {
        let mut string = String::with_capacity(s.len());

        for ch in s.chars() 
        {
            if let Some(s) = get_char_map( ch ) { string.push_str(s); }            
            else { string.push( ch ); }
        }

        string
    }
    /// Trait for formatting a .over representation of an object.
    pub trait Format 
    {
        fn format(&self, full: bool, indent_amt: usize) -> String;
    }

    impl Format for BigRational 
    {
        fn format(&self, _full: bool, _indent_amt: usize) -> String {
            let frac_fmt = format!("{}", *self);

            if *self.denom() == BigInt::one() {
                format!("{}.0", frac_fmt)
            } else {
                frac_fmt
            }
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

pub mod fs
{
    pub use std::fs::{ * };
}

pub mod hash
{
    pub use std::hash::{ * };
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
    Identity Signature*/
    use ::
    {
        *,
    };
    /*
    */
    /// Returns true if this character signifies the legal end of a value.
   
    pub fn value_end_char(ch: char) -> bool 
    {
        whitespace(ch) || end_delimiter(ch) || operator(ch)
    }
    /// Returns true if the character is either whitespace or '#' (start of a comment).   
    pub fn whitespace(ch: char) -> bool 
    {
        ch.is_whitespace() || ch == '#'
    }
   
    pub fn end_delimiter(ch: char) -> bool 
    {
        match ch {
            ')' | ']' | '}' | '>' => true,
            _ => false,
        }
    }
   
    pub fn numeric_char(ch: char) -> bool 
    {
        match ch {
            _ch if digit(_ch) => true,
            '.' | ',' => true,
            _ => false,
        }
    }
   
    pub fn priority_operator(ch: char) -> bool 
    {
        match ch {
            '*' | '/' | '%' => true,
            _ => false,
        }
    }
   
    pub fn operator(ch: char) -> bool 
    {
        match ch {
            '+' | '-' | '*' | '/' | '%' => true,
            _ => false,
        }
    }
    /// Returns true if `ch` is an ASCII decimal digit.
    //pub fn is_digit(ch: char) -> bool {
    pub fn digit(ch: char) -> bool 
    {
        match ch {
            '0'..='9' => true,
            _ => false,
        }
    }
   
    pub fn reserved(field: &str) -> bool 
    {
        match field {
            "@" | "null" | "true" | "false" | "Obj" | "Str" | "Arr" | "Tup" => true,
            _ => false,
        }
    }
    /// Whether the character has the Unicode property XID\_Start.
    /*
    pub fn is_xid_start( ... ) -> bool*/
    pub fn xid_start( ch:char ) -> bool
    {
        if ch.is_ascii() { return ::ascii::ASCII_START.0[ch as usize]; }
        let chunk = * ::ascii::TRIE_START.0.get(ch as usize / 8 / ::ascii::CHUNK).unwrap_or(&0);
        let offset = chunk as usize *  ::ascii::CHUNK / 2 + ch as usize / 8 %  ::ascii::CHUNK;
        unsafe {  ::ascii::LEAF.0.get_unchecked(offset) }.wrapping_shr(ch as u32 % 8) & 1 != 0
    }
    /// Whether the character has the Unicode property XID\_Continue.
    /*
    pub fn is_xid_continue(ch: char) -> bool */
    pub fn xid_continue(ch: char) -> bool
    {
        if ch.is_ascii() { return ::ascii::ASCII_CONTINUE.0[ch as usize]; }
        let chunk = *::ascii::TRIE_CONTINUE.0.get(ch as usize / 8 / ::ascii::CHUNK).unwrap_or(&0);
        let offset = chunk as usize * ::ascii::CHUNK / 2 + ch as usize / 8 % ::ascii::CHUNK;
        unsafe { ::ascii::LEAF.0.get_unchecked(offset) }.wrapping_shr(ch as u32 % 8) & 1 != 0
    }
    /*
    pub fn is_ident_start(c: char) -> bool*/
    pub fn ident_start(c: char) -> bool
    {
        c == '_' || xid_start(c)
    }
    /*
    pub fn is_ident_continue(c: char) -> bool */
    pub fn ident_continue(c: char) -> bool
    {
        xid_continue(c)
    }
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
    use ::
    {
        rc::{ Rc },
        panic::{ RefUnwindSafe, UnwindSafe },
        *,
    };
    /*
    */
    pub const MARKER: ProcMacroAutoTraits = ProcMacroAutoTraits(PhantomData);
    /// Zero sized marker with the correct set of autotrait impls we want all proc macro types to have.
    #[derive(Copy, Clone, PartialEq, Eq)]
    pub struct ProcMacroAutoTraits( pub PhantomData<Rc<()>> );
    impl UnwindSafe for ProcMacroAutoTraits {}
    impl RefUnwindSafe for ProcMacroAutoTraits {}
}

pub mod num
{
    pub use std::num::{ * };
    use ::
    {
        *,
    };
    /*
    */
    pub mod traits
    {
        //! Numeric traits for generic mathematics
        use ::
        {
            num::{ Wrapping },
            ops::{ Add, Div, Mul, Rem, Sub, AddAssign, DivAssign, MulAssign, RemAssign, SubAssign },
            *,
        };
        /*
        */
        pub mod bounds
        {
            use ::
            {
                num::
                {
                    NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize, NonZeroU128,
                    NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize, Wrapping,
                },
                *,
            };
            /*
            */
            /// Numbers which have upper and lower bounds
            pub trait Bounded
            {
               
                /// Returns the smallest finite number this type can represent
                fn min_value() -> Self;
                /// Returns the largest finite number this type can represent
                fn max_value() -> Self;
            }
            /// Numbers which have lower bounds
            pub trait LowerBounded
            {
                /// Returns the smallest finite number this type can represent
                fn min_value() -> Self;
            }
           
            impl<T: Bounded> LowerBounded for T
            {
                fn min_value() -> T {
                    Bounded::min_value()
                }
            }
            /// Numbers which have upper bounds
            pub trait UpperBounded
            {
                /// Returns the largest finite number this type can represent
                fn max_value() -> Self;
            }
           
            impl<T: Bounded> UpperBounded for T
            {
                fn max_value() -> T {
                    Bounded::max_value()
                }
            }
            macro_rules! bounded_impl
            {
                ($t:ty, $min:expr, $max:expr) =>
                {
                    impl Bounded for $t {
                        #[inline]
                        fn min_value() -> $t {
                            $min
                        }
                        #[inline]
                        fn max_value() -> $t {
                            $max
                        }
                    }
                };
            }
            bounded_impl!(usize, usize::MIN, usize::MAX);
            bounded_impl!(u8, u8::MIN, u8::MAX);
            bounded_impl!(u16, u16::MIN, u16::MAX);
            bounded_impl!(u32, u32::MIN, u32::MAX);
            bounded_impl!(u64, u64::MIN, u64::MAX);
            bounded_impl!(u128, u128::MIN, u128::MAX);

            bounded_impl!(isize, isize::MIN, isize::MAX);
            bounded_impl!(i8, i8::MIN, i8::MAX);
            bounded_impl!(i16, i16::MIN, i16::MAX);
            bounded_impl!(i32, i32::MIN, i32::MAX);
            bounded_impl!(i64, i64::MIN, i64::MAX);
            bounded_impl!(i128, i128::MIN, i128::MAX);

            macro_rules! bounded_impl_nonzero_const 
            {
                ($t:ty, $v:expr, $i:ident) => {
                    const $i: $t = match <$t>::new($v) {
                        Some(nz) => nz,
                        None => panic!("bad nonzero bound!"),
                    };
                };
            }
            macro_rules! bounded_impl_nonzero 
            {
                ($t:ty, $min:expr, $max:expr) => {
                    impl Bounded for $t {
                        #[inline]
                        fn min_value() -> $t {
                           
                            bounded_impl_nonzero_const!($t, $min, MIN);
                            MIN
                        }
                        #[inline]
                        fn max_value() -> $t {
                           
                            bounded_impl_nonzero_const!($t, $max, MAX);
                            MAX
                        }
                    }
                };
            }
            bounded_impl_nonzero!(NonZeroUsize, 1, usize::MAX);
            bounded_impl_nonzero!(NonZeroU8, 1, u8::MAX);
            bounded_impl_nonzero!(NonZeroU16, 1, u16::MAX);
            bounded_impl_nonzero!(NonZeroU32, 1, u32::MAX);
            bounded_impl_nonzero!(NonZeroU64, 1, u64::MAX);
            bounded_impl_nonzero!(NonZeroU128, 1, u128::MAX);

            bounded_impl_nonzero!(NonZeroIsize, isize::MIN, isize::MAX);
            bounded_impl_nonzero!(NonZeroI8, i8::MIN, i8::MAX);
            bounded_impl_nonzero!(NonZeroI16, i16::MIN, i16::MAX);
            bounded_impl_nonzero!(NonZeroI32, i32::MIN, i32::MAX);
            bounded_impl_nonzero!(NonZeroI64, i64::MIN, i64::MAX);
            bounded_impl_nonzero!(NonZeroI128, i128::MIN, i128::MAX);

            impl<T: Bounded> Bounded for Wrapping<T> 
            {
                fn min_value() -> Self {
                    Wrapping(T::min_value())
                }
                fn max_value() -> Self {
                    Wrapping(T::max_value())
                }
            }
            bounded_impl!(f32, f32::MIN, f32::MAX);

            macro_rules! for_each_tuple_ 
            {
                ( $m:ident !! ) => (
                    $m! { }
                );
                ( $m:ident !! $h:ident, $($t:ident,)* ) => (
                    $m! { $h $($t)* }
                    for_each_tuple_! { $m !! $($t,)* }
                );
            }
            macro_rules! for_each_tuple 
            {
                ($m:ident) => {
                    for_each_tuple_! { $m !! A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, }
                };
            }
            macro_rules! bounded_tuple 
            {
                ( $($name:ident)* ) => (
                    impl<$($name: Bounded,)*> Bounded for ($($name,)*) {
                        #[inline]
                        fn min_value() -> Self {
                            ($($name::min_value(),)*)
                        }
                        #[inline]
                        fn max_value() -> Self {
                            ($($name::max_value(),)*)
                        }
                    }
                );
            }
            for_each_tuple!(bounded_tuple);
            bounded_impl!(f64, f64::MIN, f64::MAX);
        } pub use self::bounds::Bounded;

        pub mod cast
        {
            use ::
            {
                mem::{ size_of },
                num::
                {
                    NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize, NonZeroU128,
                    NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize, Wrapping,
                },
                *,
            };
            /*
            */
            /// A generic trait for converting a value to a number.
            pub trait ToPrimitive {
                /// Converts the value of `self` to an `isize`. If the value cannot be
                /// represented by an `isize`, then `None` is returned.
                #[inline] fn to_isize(&self) -> Option<isize> {
                    self.to_i64().as_ref().and_then(ToPrimitive::to_isize)
                }
                /// Converts the value of `self` to an `i8`. If the value cannot be
                /// represented by an `i8`, then `None` is returned.
                #[inline] fn to_i8(&self) -> Option<i8> {
                    self.to_i64().as_ref().and_then(ToPrimitive::to_i8)
                }
                /// Converts the value of `self` to an `i16`. If the value cannot be
                /// represented by an `i16`, then `None` is returned.
                #[inline] fn to_i16(&self) -> Option<i16> {
                    self.to_i64().as_ref().and_then(ToPrimitive::to_i16)
                }
                /// Converts the value of `self` to an `i32`. If the value cannot be
                /// represented by an `i32`, then `None` is returned.
                #[inline] fn to_i32(&self) -> Option<i32> {
                    self.to_i64().as_ref().and_then(ToPrimitive::to_i32)
                }
                /// Converts the value of `self` to an `i64`. If the value cannot be
                /// represented by an `i64`, then `None` is returned.
                fn to_i64(&self) -> Option<i64>;
                /// Converts the value of `self` to an `i128`. If the value cannot be
                /// represented by an `i128` (`i64` under the default implementation), then
                /// `None` is returned.
                #[inline] fn to_i128(&self) -> Option<i128> {
                    self.to_i64().map(From::from)
                }
                /// Converts the value of `self` to a `usize`. If the value cannot be
                /// represented by a `usize`, then `None` is returned.
                #[inline] fn to_usize(&self) -> Option<usize> {
                    self.to_u64().as_ref().and_then(ToPrimitive::to_usize)
                }
                /// Converts the value of `self` to a `u8`. If the value cannot be
                /// represented by a `u8`, then `None` is returned.
                #[inline] fn to_u8(&self) -> Option<u8> {
                    self.to_u64().as_ref().and_then(ToPrimitive::to_u8)
                }
                /// Converts the value of `self` to a `u16`. If the value cannot be
                /// represented by a `u16`, then `None` is returned.
                #[inline] fn to_u16(&self) -> Option<u16> {
                    self.to_u64().as_ref().and_then(ToPrimitive::to_u16)
                }
                /// Converts the value of `self` to a `u32`. If the value cannot be
                /// represented by a `u32`, then `None` is returned.
                #[inline] fn to_u32(&self) -> Option<u32> {
                    self.to_u64().as_ref().and_then(ToPrimitive::to_u32)
                }
                /// Converts the value of `self` to a `u64`. If the value cannot be
                /// represented by a `u64`, then `None` is returned.
                fn to_u64(&self) -> Option<u64>;
                /// Converts the value of `self` to a `u128`. If the value cannot be
                /// represented by a `u128` (`u64` under the default implementation), then
                /// `None` is returned.
                #[inline] fn to_u128(&self) -> Option<u128> {
                    self.to_u64().map(From::from)
                }
                /// Converts the value of `self` to an `f32`. Overflows may map to positive
                /// or negative inifinity, otherwise `None` is returned if the value cannot
                /// be represented by an `f32`.
                #[inline] fn to_f32(&self) -> Option<f32> {
                    self.to_f64().as_ref().and_then(ToPrimitive::to_f32)
                }
                /// Converts the value of `self` to an `f64`. Overflows may map to positive
                /// or negative inifinity, otherwise `None` is returned if the value cannot
                /// be represented by an `f64`.
                /// override this method if they can represent a greater range.
                #[inline] fn to_f64(&self) -> Option<f64> {
                    match self.to_i64() {
                        Some(i) => i.to_f64(),
                        None => self.to_u64().as_ref().and_then(ToPrimitive::to_f64),
                    }
                }
            }
            macro_rules! impl_to_primitive_int_to_int 
            {
                ($SrcT:ident : $( fn $method:ident -> $DstT:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$DstT> {
                        let min = $DstT::MIN as $SrcT;
                        let max = $DstT::MAX as $SrcT;
                        if size_of::<$SrcT>() <= size_of::<$DstT>() || (min <= *self && *self <= max) {
                            Some(*self as $DstT)
                        } else {
                            None
                        }
                    }
                )*}
            }
            macro_rules! impl_to_primitive_int_to_uint 
            {
                ($SrcT:ident : $( fn $method:ident -> $DstT:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$DstT> {
                        let max = $DstT::MAX as $SrcT;
                        if 0 <= *self && (size_of::<$SrcT>() <= size_of::<$DstT>() || *self <= max) {
                            Some(*self as $DstT)
                        } else {
                            None
                        }
                    }
                )*}
            }
            macro_rules! impl_to_primitive_int 
            {
                ($T:ident) => {
                    impl ToPrimitive for $T {
                        impl_to_primitive_int_to_int! { $T:
                            fn to_isize -> isize;
                            fn to_i8 -> i8;
                            fn to_i16 -> i16;
                            fn to_i32 -> i32;
                            fn to_i64 -> i64;
                            fn to_i128 -> i128;
                        }
                        impl_to_primitive_int_to_uint! { $T:
                            fn to_usize -> usize;
                            fn to_u8 -> u8;
                            fn to_u16 -> u16;
                            fn to_u32 -> u32;
                            fn to_u64 -> u64;
                            fn to_u128 -> u128;
                        }
                        #[inline]
                        fn to_f32(&self) -> Option<f32> {
                            Some(*self as f32)
                        }
                        #[inline]
                        fn to_f64(&self) -> Option<f64> {
                            Some(*self as f64)
                        }
                    }
                };
            }
            impl_to_primitive_int!(isize);
            impl_to_primitive_int!(i8);
            impl_to_primitive_int!(i16);
            impl_to_primitive_int!(i32);
            impl_to_primitive_int!(i64);
            impl_to_primitive_int!(i128);

            macro_rules! impl_to_primitive_uint_to_int 
            {
                ($SrcT:ident : $( fn $method:ident -> $DstT:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$DstT> {
                        let max = $DstT::MAX as $SrcT;
                        if size_of::<$SrcT>() < size_of::<$DstT>() || *self <= max {
                            Some(*self as $DstT)
                        } else {
                            None
                        }
                    }
                )*}
            }
            macro_rules! impl_to_primitive_uint_to_uint 
            {
                ($SrcT:ident : $( fn $method:ident -> $DstT:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$DstT> {
                        let max = $DstT::MAX as $SrcT;
                        if size_of::<$SrcT>() <= size_of::<$DstT>() || *self <= max {
                            Some(*self as $DstT)
                        } else {
                            None
                        }
                    }
                )*}
            }
            macro_rules! impl_to_primitive_uint 
            {
                ($T:ident) => {
                    impl ToPrimitive for $T {
                        impl_to_primitive_uint_to_int! { $T:
                            fn to_isize -> isize;
                            fn to_i8 -> i8;
                            fn to_i16 -> i16;
                            fn to_i32 -> i32;
                            fn to_i64 -> i64;
                            fn to_i128 -> i128;
                        }
                        impl_to_primitive_uint_to_uint! { $T:
                            fn to_usize -> usize;
                            fn to_u8 -> u8;
                            fn to_u16 -> u16;
                            fn to_u32 -> u32;
                            fn to_u64 -> u64;
                            fn to_u128 -> u128;
                        }
                        #[inline]
                        fn to_f32(&self) -> Option<f32> {
                            Some(*self as f32)
                        }
                        #[inline]
                        fn to_f64(&self) -> Option<f64> {
                            Some(*self as f64)
                        }
                    }
                };
            }
            impl_to_primitive_uint!(usize);
            impl_to_primitive_uint!(u8);
            impl_to_primitive_uint!(u16);
            impl_to_primitive_uint!(u32);
            impl_to_primitive_uint!(u64);
            impl_to_primitive_uint!(u128);

            macro_rules! impl_to_primitive_nonzero_to_method 
            {
                ($SrcT:ident : $( fn $method:ident -> $DstT:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$DstT> {
                        self.get().$method()
                    }
                )*}
            }
            macro_rules! impl_to_primitive_nonzero 
            {
                ($T:ident) => {
                    impl ToPrimitive for $T {
                        impl_to_primitive_nonzero_to_method! { $T:
                            fn to_isize -> isize;
                            fn to_i8 -> i8;
                            fn to_i16 -> i16;
                            fn to_i32 -> i32;
                            fn to_i64 -> i64;
                            fn to_i128 -> i128;

                            fn to_usize -> usize;
                            fn to_u8 -> u8;
                            fn to_u16 -> u16;
                            fn to_u32 -> u32;
                            fn to_u64 -> u64;
                            fn to_u128 -> u128;

                            fn to_f32 -> f32;
                            fn to_f64 -> f64;
                        }
                    }
                };
            }
            impl_to_primitive_nonzero!(NonZeroUsize);
            impl_to_primitive_nonzero!(NonZeroU8);
            impl_to_primitive_nonzero!(NonZeroU16);
            impl_to_primitive_nonzero!(NonZeroU32);
            impl_to_primitive_nonzero!(NonZeroU64);
            impl_to_primitive_nonzero!(NonZeroU128);

            impl_to_primitive_nonzero!(NonZeroIsize);
            impl_to_primitive_nonzero!(NonZeroI8);
            impl_to_primitive_nonzero!(NonZeroI16);
            impl_to_primitive_nonzero!(NonZeroI32);
            impl_to_primitive_nonzero!(NonZeroI64);
            impl_to_primitive_nonzero!(NonZeroI128);

            macro_rules! impl_to_primitive_float_to_float 
            {
                ($SrcT:ident : $( fn $method:ident -> $DstT:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$DstT> {
                       
                       
                        Some(*self as $DstT)
                    }
                )*}
            }
            macro_rules! float_to_int_unchecked 
            {
               
               
                ($float:expr => $int:ty) => {
                    unsafe { $float.to_int_unchecked::<$int>() }
                };
            }
            macro_rules! impl_to_primitive_float_to_signed_int 
            {
                ($f:ident : $( fn $method:ident -> $i:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$i> {
                       
                       
                        if size_of::<$f>() > size_of::<$i>() {
                           
                            const MIN_M1: $f = $i::MIN as $f - 1.0;
                            const MAX_P1: $f = $i::MAX as $f + 1.0;
                            if *self > MIN_M1 && *self < MAX_P1 {
                                return Some(float_to_int_unchecked!(*self => $i));
                            }
                        } else {
                           
                           
                            const MIN: $f = $i::MIN as $f;
                           
                           
                            const MAX_P1: $f = $i::MAX as $f;
                            if *self >= MIN && *self < MAX_P1 {
                                return Some(float_to_int_unchecked!(*self => $i));
                            }
                        }
                        None
                    }
                )*}
            }
            macro_rules! impl_to_primitive_float_to_unsigned_int 
            {
                ($f:ident : $( fn $method:ident -> $u:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$u> {
                       
                       
                        if size_of::<$f>() > size_of::<$u>() {
                           
                            const MAX_P1: $f = $u::MAX as $f + 1.0;
                            if *self > -1.0 && *self < MAX_P1 {
                                return Some(float_to_int_unchecked!(*self => $u));
                            }
                        } else {
                           
                           
                           
                            const MAX_P1: $f = $u::MAX as $f;
                            if *self > -1.0 && *self < MAX_P1 {
                                return Some(float_to_int_unchecked!(*self => $u));
                            }
                        }
                        None
                    }
                )*}
            }
            macro_rules! impl_to_primitive_float 
            {
                ($T:ident) => {
                    impl ToPrimitive for $T {
                        impl_to_primitive_float_to_signed_int! { $T:
                            fn to_isize -> isize;
                            fn to_i8 -> i8;
                            fn to_i16 -> i16;
                            fn to_i32 -> i32;
                            fn to_i64 -> i64;
                            fn to_i128 -> i128;
                        }
                        impl_to_primitive_float_to_unsigned_int! { $T:
                            fn to_usize -> usize;
                            fn to_u8 -> u8;
                            fn to_u16 -> u16;
                            fn to_u32 -> u32;
                            fn to_u64 -> u64;
                            fn to_u128 -> u128;
                        }
                        impl_to_primitive_float_to_float! { $T:
                            fn to_f32 -> f32;
                            fn to_f64 -> f64;
                        }
                    }
                };
            }
            impl_to_primitive_float!(f32);
            impl_to_primitive_float!(f64);
            /// A generic trait for converting a number to a value.
            pub trait FromPrimitive: Sized
            {
                /// Converts an `isize` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_isize(n: isize) -> Option<Self> {
                    n.to_i64().and_then(FromPrimitive::from_i64)
                }
                /// Converts an `i8` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_i8(n: i8) -> Option<Self> {
                    FromPrimitive::from_i64(From::from(n))
                }
                /// Converts an `i16` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_i16(n: i16) -> Option<Self> {
                    FromPrimitive::from_i64(From::from(n))
                }
                /// Converts an `i32` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_i32(n: i32) -> Option<Self> {
                    FromPrimitive::from_i64(From::from(n))
                }
                /// Converts an `i64` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                fn from_i64(n: i64) -> Option<Self>;
                /// Converts an `i128` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_i128(n: i128) -> Option<Self> {
                    n.to_i64().and_then(FromPrimitive::from_i64)
                }
                /// Converts a `usize` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_usize(n: usize) -> Option<Self> {
                    n.to_u64().and_then(FromPrimitive::from_u64)
                }
                /// Converts an `u8` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_u8(n: u8) -> Option<Self> {
                    FromPrimitive::from_u64(From::from(n))
                }
                /// Converts an `u16` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_u16(n: u16) -> Option<Self> {
                    FromPrimitive::from_u64(From::from(n))
                }
                /// Converts an `u32` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_u32(n: u32) -> Option<Self> {
                    FromPrimitive::from_u64(From::from(n))
                }
                /// Converts an `u64` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                fn from_u64(n: u64) -> Option<Self>;
                /// Converts an `u128` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_u128(n: u128) -> Option<Self> {
                    n.to_u64().and_then(FromPrimitive::from_u64)
                }
                /// Converts a `f32` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                #[inline] fn from_f32(n: f32) -> Option<Self> {
                    FromPrimitive::from_f64(From::from(n))
                }
                /// Converts a `f64` to return an optional value of this type. If the
                /// value cannot be represented by this type, then `None` is returned.
                /// override this method if they can represent a greater range.
                #[inline] fn from_f64(n: f64) -> Option<Self> {
                    match n.to_i64() {
                        Some(i) => FromPrimitive::from_i64(i),
                        None => n.to_u64().and_then(FromPrimitive::from_u64),
                    }
                }
            }
            macro_rules! impl_from_primitive
            {
                ($T:ty, $to_ty:ident) => {
                    impl FromPrimitive for $T {
                        #[inline]
                        fn from_isize(n: isize) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_i8(n: i8) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_i16(n: i16) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_i32(n: i32) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_i64(n: i64) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_i128(n: i128) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_usize(n: usize) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_u8(n: u8) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_u16(n: u16) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_u32(n: u32) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_u64(n: u64) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_u128(n: u128) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_f32(n: f32) -> Option<$T> {
                            n.$to_ty()
                        }
                        #[inline]
                        fn from_f64(n: f64) -> Option<$T> {
                            n.$to_ty()
                        }
                    }
                };
            }
            impl_from_primitive!(isize, to_isize);
            impl_from_primitive!(i8, to_i8);
            impl_from_primitive!(i16, to_i16);
            impl_from_primitive!(i32, to_i32);
            impl_from_primitive!(i64, to_i64);
            impl_from_primitive!(i128, to_i128);
            impl_from_primitive!(usize, to_usize);
            impl_from_primitive!(u8, to_u8);
            impl_from_primitive!(u16, to_u16);
            impl_from_primitive!(u32, to_u32);
            impl_from_primitive!(u64, to_u64);
            impl_from_primitive!(u128, to_u128);
            impl_from_primitive!(f32, to_f32);
            impl_from_primitive!(f64, to_f64);

            macro_rules! impl_from_primitive_nonzero
            {
                ($T:ty, $to_ty:ident) => {
                    impl FromPrimitive for $T {
                        #[inline]
                        fn from_isize(n: isize) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_i8(n: i8) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_i16(n: i16) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_i32(n: i32) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_i64(n: i64) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_i128(n: i128) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_usize(n: usize) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_u8(n: u8) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_u16(n: u16) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_u32(n: u32) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_u64(n: u64) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_u128(n: u128) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_f32(n: f32) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                        #[inline]
                        fn from_f64(n: f64) -> Option<$T> {
                            n.$to_ty().and_then(Self::new)
                        }
                    }
                };
            }
            impl_from_primitive_nonzero!(NonZeroIsize, to_isize);
            impl_from_primitive_nonzero!(NonZeroI8, to_i8);
            impl_from_primitive_nonzero!(NonZeroI16, to_i16);
            impl_from_primitive_nonzero!(NonZeroI32, to_i32);
            impl_from_primitive_nonzero!(NonZeroI64, to_i64);
            impl_from_primitive_nonzero!(NonZeroI128, to_i128);
            impl_from_primitive_nonzero!(NonZeroUsize, to_usize);
            impl_from_primitive_nonzero!(NonZeroU8, to_u8);
            impl_from_primitive_nonzero!(NonZeroU16, to_u16);
            impl_from_primitive_nonzero!(NonZeroU32, to_u32);
            impl_from_primitive_nonzero!(NonZeroU64, to_u64);
            impl_from_primitive_nonzero!(NonZeroU128, to_u128);

            macro_rules! impl_to_primitive_wrapping 
            {
                ($( fn $method:ident -> $i:ident ; )*) => {$(
                    #[inline] fn $method(&self) -> Option<$i> {
                        (self.0).$method()
                    }
                )*}
            }
            
            impl<T: ToPrimitive> ToPrimitive for Wrapping<T> 
            {
                impl_to_primitive_wrapping! {
                    fn to_isize -> isize;
                    fn to_i8 -> i8;
                    fn to_i16 -> i16;
                    fn to_i32 -> i32;
                    fn to_i64 -> i64;
                    fn to_i128 -> i128;

                    fn to_usize -> usize;
                    fn to_u8 -> u8;
                    fn to_u16 -> u16;
                    fn to_u32 -> u32;
                    fn to_u64 -> u64;
                    fn to_u128 -> u128;

                    fn to_f32 -> f32;
                    fn to_f64 -> f64;
                }
            }
            macro_rules! impl_from_primitive_wrapping 
            {
                ($( fn $method:ident ( $i:ident ); )*) => {$(
                    #[inline] fn $method(n: $i) -> Option<Self> {
                        T::$method(n).map(Wrapping)
                    }
                )*}
            }
            
            impl<T: FromPrimitive> FromPrimitive for Wrapping<T> 
            {
                impl_from_primitive_wrapping! {
                    fn from_isize(isize);
                    fn from_i8(i8);
                    fn from_i16(i16);
                    fn from_i32(i32);
                    fn from_i64(i64);
                    fn from_i128(i128);

                    fn from_usize(usize);
                    fn from_u8(u8);
                    fn from_u16(u16);
                    fn from_u32(u32);
                    fn from_u64(u64);
                    fn from_u128(u128);

                    fn from_f32(f32);
                    fn from_f64(f64);
                }
            }
            /// Cast from one machine scalar to another.
            #[inline] pub fn cast<T: NumCast, U: NumCast>(n: T) -> Option<U>
            {
                NumCast::from(n)
            }
            /// An interface for casting between machine scalars.
            pub trait NumCast: Sized + ToPrimitive
            {
                /// Creates a number from another value that can be converted into
                /// a primitive via the `ToPrimitive` trait. If the source value cannot be
                /// represented by the target type, then `None` is returned.
                fn from<T: ToPrimitive>(n: T) -> Option<Self>;
            }
            macro_rules! impl_num_cast
            {
                ($T:ty, $conv:ident) => {
                    impl NumCast for $T {
                        #[inline]
                        fn from<N: ToPrimitive>(n: N) -> Option<$T> {
                            n.$conv()
                        }
                    }
                };
            }
            impl_num_cast!(u8, to_u8);
            impl_num_cast!(u16, to_u16);
            impl_num_cast!(u32, to_u32);
            impl_num_cast!(u64, to_u64);
            impl_num_cast!(u128, to_u128);
            impl_num_cast!(usize, to_usize);
            impl_num_cast!(i8, to_i8);
            impl_num_cast!(i16, to_i16);
            impl_num_cast!(i32, to_i32);
            impl_num_cast!(i64, to_i64);
            impl_num_cast!(i128, to_i128);
            impl_num_cast!(isize, to_isize);
            impl_num_cast!(f32, to_f32);
            impl_num_cast!(f64, to_f64);

            macro_rules! impl_num_cast_nonzero {
                ($T:ty, $conv:ident) => {
                    impl NumCast for $T {
                        #[inline]
                        fn from<N: ToPrimitive>(n: N) -> Option<$T> {
                            n.$conv().and_then(Self::new)
                        }
                    }
                };
            }
            impl_num_cast_nonzero!(NonZeroUsize, to_usize);
            impl_num_cast_nonzero!(NonZeroU8, to_u8);
            impl_num_cast_nonzero!(NonZeroU16, to_u16);
            impl_num_cast_nonzero!(NonZeroU32, to_u32);
            impl_num_cast_nonzero!(NonZeroU64, to_u64);
            impl_num_cast_nonzero!(NonZeroU128, to_u128);

            impl_num_cast_nonzero!(NonZeroIsize, to_isize);
            impl_num_cast_nonzero!(NonZeroI8, to_i8);
            impl_num_cast_nonzero!(NonZeroI16, to_i16);
            impl_num_cast_nonzero!(NonZeroI32, to_i32);
            impl_num_cast_nonzero!(NonZeroI64, to_i64);
            impl_num_cast_nonzero!(NonZeroI128, to_i128);

            impl<T: NumCast> NumCast for Wrapping<T>
            {
                fn from<U: ToPrimitive>(n: U) -> Option<Self> {
                    T::from(n).map(Wrapping)
                }
            }
            /// Generic interface for casting between machine scalars with the `as` operator, 
            /// which admits narrowing and precision loss.
            pub trait AsPrimitive<T>: 'static + Copy where
            T: 'static + Copy,
            {
                /// Convert a value to another, using the `as` operator.
                fn as_(self) -> T;
            }
            macro_rules! impl_as_primitive
            {
                (@ $T: ty =>  impl $U: ty ) => {
                    impl AsPrimitive<$U> for $T {
                        #[inline] fn as_(self) -> $U { self as $U }
                    }
                };
                (@ $T: ty => { $( $U: ty ),* } ) => {$(
                    impl_as_primitive!(@ $T => impl $U);
                )*};
                ($T: ty => { $( $U: ty ),* } ) => {
                    impl_as_primitive!(@ $T => { $( $U ),* });
                    impl_as_primitive!(@ $T => { u8, u16, u32, u64, u128, usize });
                    impl_as_primitive!(@ $T => { i8, i16, i32, i64, i128, isize });
                };
            }
            impl_as_primitive!(u8 => { char, f32, f64 });
            impl_as_primitive!(i8 => { f32, f64 });
            impl_as_primitive!(u16 => { f32, f64 });
            impl_as_primitive!(i16 => { f32, f64 });
            impl_as_primitive!(u32 => { f32, f64 });
            impl_as_primitive!(i32 => { f32, f64 });
            impl_as_primitive!(u64 => { f32, f64 });
            impl_as_primitive!(i64 => { f32, f64 });
            impl_as_primitive!(u128 => { f32, f64 });
            impl_as_primitive!(i128 => { f32, f64 });
            impl_as_primitive!(usize => { f32, f64 });
            impl_as_primitive!(isize => { f32, f64 });
            impl_as_primitive!(f32 => { f32, f64 });
            impl_as_primitive!(f64 => { f32, f64 });
            impl_as_primitive!(char => { char });
            impl_as_primitive!(bool => {});

        } pub use self::cast::{cast, AsPrimitive, FromPrimitive, NumCast, ToPrimitive};

        pub mod float
        {
            use ::
            {
                cmp::{ Ordering },
                num::
                {
                    traits::{ Num, NumCast, ToPrimitive },
                    FpCategory
                },
                ops::{ Add, Div, Neg },
                *,
            };
            /*
            */
            /// Generic trait for floating point numbers that works with `no_std`.
            pub trait FloatCore: Num + NumCast + Neg<Output = Self> + PartialOrd + Copy
            {
                /// Returns positive infinity.
                fn infinity() -> Self;
                /// Returns negative infinity.
                fn neg_infinity() -> Self;
                /// Returns NaN.
                fn nan() -> Self;
                /// Returns `-0.0`.
                fn neg_zero() -> Self;
                /// Returns the smallest finite value that this type can represent.
                fn min_value() -> Self;
                /// Returns the smallest positive, normalized value that this type can represent.
                fn min_positive_value() -> Self;
                /// Returns epsilon, a small positive value.
                fn epsilon() -> Self;
                /// Returns the largest finite value that this type can represent.
                fn max_value() -> Self;
                /// Returns `true` if the number is NaN.
                #[inline] fn is_nan(self) -> bool {
                    self != self
                }
                /// Returns `true` if the number is infinite.
                #[inline] fn is_infinite(self) -> bool {
                    self == Self::infinity() || self == Self::neg_infinity()
                }
                /// Returns `true` if the number is neither infinite or NaN.
                #[inline] fn is_finite(self) -> bool {
                    !(self.is_nan() || self.is_infinite())
                }
                /// Returns `true` if the number is neither zero, infinite, subnormal or NaN.
                #[inline] fn is_normal(self) -> bool {
                    self.classify() == FpCategory::Normal
                }
                /// Returns `true` if the number is [subnormal].
                #[inline] fn is_subnormal(self) -> bool {
                    self.classify() == FpCategory::Subnormal
                }
                /// Returns the floating point category of the number.
                fn classify(self) -> FpCategory;
                /// Returns the largest integer less than or equal to a number.
                #[inline] fn floor(self) -> Self {
                    let f = self.fract();
                    if f.is_nan() || f.is_zero() {
                        self
                    } else if self < Self::zero() {
                        self - f - Self::one()
                    } else {
                        self - f
                    }
                }
                /// Returns the smallest integer greater than or equal to a number.
                #[inline] fn ceil(self) -> Self {
                    let f = self.fract();
                    if f.is_nan() || f.is_zero() {
                        self
                    } else if self > Self::zero() {
                        self - f + Self::one()
                    } else {
                        self - f
                    }
                }
                /// Returns the nearest integer to a number. Round half-way cases away from `0.0`.
                #[inline] fn round(self) -> Self {
                    let one = Self::one();
                    let h = Self::from(0.5).expect("Unable to cast from 0.5");
                    let f = self.fract();
                    if f.is_nan() || f.is_zero() {
                        self
                    } else if self > Self::zero() {
                        if f < h {
                            self - f
                        } else {
                            self - f + one
                        }
                    } else if -f < h {
                        self - f
                    } else {
                        self - f - one
                    }
                }
                /// Return the integer part of a number.
                #[inline] fn trunc(self) -> Self {
                    let f = self.fract();
                    if f.is_nan() {
                        self
                    } else {
                        self - f
                    }
                }
                /// Returns the fractional part of a number.
                #[inline] fn fract(self) -> Self {
                    if self.is_zero() {
                        Self::zero()
                    } else {
                        self % Self::one()
                    }
                }
                /// Computes the absolute value of `self`. Returns `Float::nan()` if the number is `Float::nan()`.
                #[inline] fn abs(self) -> Self {
                    if self.is_sign_positive() {
                        return self;
                    }
                    if self.is_sign_negative() {
                        return -self;
                    }
                    Self::nan()
                }
                /// Returns a number that represents the sign of `self`.
                #[inline] fn signum(self) -> Self {
                    if self.is_nan() {
                        Self::nan()
                    } else if self.is_sign_negative() {
                        -Self::one()
                    } else {
                        Self::one()
                    }
                }
                /// Returns `true` if `self` is positive, including `+0.0`, `Float::infinity()`, and `Float::nan()`.
                #[inline] fn is_sign_positive(self) -> bool {
                    !self.is_sign_negative()
                }
                /// Returns `true` if `self` is negative, including `-0.0`, `Float::neg_infinity()`, and `-Float::nan()`.
                #[inline] fn is_sign_negative(self) -> bool {
                    let (_, _, sign) = self.integer_decode();
                    sign < 0
                }
                /// Returns the minimum of the two numbers.
                #[inline] fn min(self, other: Self) -> Self {
                    if self.is_nan() {
                        return other;
                    }
                    if other.is_nan() {
                        return self;
                    }
                    if self < other {
                        self
                    } else {
                        other
                    }
                }
                /// Returns the maximum of the two numbers.
                #[inline] fn max(self, other: Self) -> Self {
                    if self.is_nan() {
                        return other;
                    }
                    if other.is_nan() {
                        return self;
                    }
                    if self > other {
                        self
                    } else {
                        other
                    }
                }
                /// A value bounded by a minimum and a maximum.
                fn clamp(self, min: Self, max: Self) -> Self {
                    ::num::traits::clamp(self, min, max)
                }
                /// Returns the reciprocal (multiplicative inverse) of the number.
                #[inline] fn recip(self) -> Self {
                    Self::one() / self
                }
                /// Raise a number to an integer power.
                #[inline] fn powi(mut self, mut exp: i32) -> Self {
                    if exp < 0 {
                        exp = exp.wrapping_neg();
                        self = self.recip();
                    }
                   
                   
                   
                    super::pow(self, (exp as u32).to_usize().unwrap())
                }
                /// Converts to degrees, assuming the number is in radians.
                fn to_degrees(self) -> Self;
                /// Converts to radians, assuming the number is in degrees.
                fn to_radians(self) -> Self;
                /// Returns the mantissa, base 2 exponent, and sign as integers, respectively.
                fn integer_decode(self) -> (u64, i16, i8);
            }
            
            impl FloatCore for f32
            {
                constant!
                {
                    infinity() -> f32::INFINITY;
                    neg_infinity() -> f32::NEG_INFINITY;
                    nan() -> f32::NAN;
                    neg_zero() -> -0.0;
                    min_value() -> f32::MIN;
                    min_positive_value() -> f32::MIN_POSITIVE;
                    epsilon() -> f32::EPSILON;
                    max_value() -> f32::MAX;
                }
                #[inline] fn integer_decode(self) -> (u64, i16, i8)
                {
                    integer_decode_f32(self)
                }
                forward!
                {
                    Self::is_nan(self) -> bool;
                    Self::is_infinite(self) -> bool;
                    Self::is_finite(self) -> bool;
                    Self::is_normal(self) -> bool;
                    Self::is_subnormal(self) -> bool;
                    Self::clamp(self, min: Self, max: Self) -> Self;
                    Self::classify(self) -> FpCategory;
                    Self::is_sign_positive(self) -> bool;
                    Self::is_sign_negative(self) -> bool;
                    Self::min(self, other: Self) -> Self;
                    Self::max(self, other: Self) -> Self;
                    Self::recip(self) -> Self;
                    Self::to_degrees(self) -> Self;
                    Self::to_radians(self) -> Self;
                }
                
                forward!
                {
                    Self::floor(self) -> Self;
                    Self::ceil(self) -> Self;
                    Self::round(self) -> Self;
                    Self::trunc(self) -> Self;
                    Self::fract(self) -> Self;
                    Self::abs(self) -> Self;
                    Self::signum(self) -> Self;
                    Self::powi(self, n: i32) -> Self;
                }
            }
            
            impl FloatCore for f64
            {
                constant!
                {
                    infinity() -> f64::INFINITY;
                    neg_infinity() -> f64::NEG_INFINITY;
                    nan() -> f64::NAN;
                    neg_zero() -> -0.0;
                    min_value() -> f64::MIN;
                    min_positive_value() -> f64::MIN_POSITIVE;
                    epsilon() -> f64::EPSILON;
                    max_value() -> f64::MAX;
                }
                #[inline] fn integer_decode(self) -> (u64, i16, i8)
                {
                    integer_decode_f64(self)
                }
                forward!
                {
                    Self::is_nan(self) -> bool;
                    Self::is_infinite(self) -> bool;
                    Self::is_finite(self) -> bool;
                    Self::is_normal(self) -> bool;
                    Self::is_subnormal(self) -> bool;
                    Self::clamp(self, min: Self, max: Self) -> Self;
                    Self::classify(self) -> FpCategory;
                    Self::is_sign_positive(self) -> bool;
                    Self::is_sign_negative(self) -> bool;
                    Self::min(self, other: Self) -> Self;
                    Self::max(self, other: Self) -> Self;
                    Self::recip(self) -> Self;
                    Self::to_degrees(self) -> Self;
                    Self::to_radians(self) -> Self;
                }
                
                forward!
                {
                    Self::floor(self) -> Self;
                    Self::ceil(self) -> Self;
                    Self::round(self) -> Self;
                    Self::trunc(self) -> Self;
                    Self::fract(self) -> Self;
                    Self::abs(self) -> Self;
                    Self::signum(self) -> Self;
                    Self::powi(self, n: i32) -> Self;
                }
            }
            /// Generic trait for floating point numbers
            pub trait Float: Num + Copy + NumCast + PartialOrd + Neg<Output = Self>
            {
                /// Returns the `NaN` value.
                fn nan() -> Self;
                /// Returns the infinite value.
                fn infinity() -> Self;
                /// Returns the negative infinite value.
                fn neg_infinity() -> Self;
                /// Returns `-0.0`.
                fn neg_zero() -> Self;
                /// Returns the smallest finite value that this type can represent.
                fn min_value() -> Self;
                /// Returns the smallest positive, normalized value that this type can represent.
                fn min_positive_value() -> Self;
                /// Returns epsilon, a small positive value.
                fn epsilon() -> Self {
                    Self::from(f32::EPSILON).expect("Unable to cast from f32::EPSILON")
                }
                /// Returns the largest finite value that this type can represent.
                fn max_value() -> Self;
                /// Returns `true` if this value is `NaN` and false otherwise.
                fn is_nan(self) -> bool;
                /// Returns `true` if this value is positive infinity or negative infinity and false otherwise.
                fn is_infinite(self) -> bool;
                /// Returns `true` if this number is neither infinite nor `NaN`.
                fn is_finite(self) -> bool;
                /// Returns `true` if the number is neither zero, infinite, subnormal, or `NaN`.
                fn is_normal(self) -> bool;
                /// Returns `true` if the number is [subnormal].
                #[inline] fn is_subnormal(self) -> bool {
                    self.classify() == FpCategory::Subnormal
                }
                /// Returns the floating point category of the number.
                fn classify(self) -> FpCategory;
                /// Returns the largest integer less than or equal to a number.
                fn floor(self) -> Self;
                /// Returns the smallest integer greater than or equal to a number.
                fn ceil(self) -> Self;
                /// Returns the nearest integer to a number. Round half-way cases away from `0.0`.
                fn round(self) -> Self;
                /// Return the integer part of a number.
                fn trunc(self) -> Self;
                /// Returns the fractional part of a number.
                fn fract(self) -> Self;
                /// Computes the absolute value of `self`. Returns `Float::nan()` if the number is `Float::nan()`.
                fn abs(self) -> Self;
                /// Returns a number that represents the sign of `self`.
                fn signum(self) -> Self;
                /// Returns `true` if `self` is positive, including `+0.0`, `Float::infinity()`, `Float::nan()`.
                fn is_sign_positive(self) -> bool;
                /// Returns `true` if `self` is negative, including `-0.0`, `Float::neg_infinity()`, `-Float::nan()`.
                fn is_sign_negative(self) -> bool;
                /// Fused multiply-add.
                fn mul_add(self, a: Self, b: Self) -> Self;
                /// Take the reciprocal (inverse) of a number, `1/x`.
                fn recip(self) -> Self;
                /// Raise a number to an integer power.
                fn powi(self, n: i32) -> Self;
                /// Raise a number to a floating point power.
                fn powf(self, n: Self) -> Self;
                /// Take the square root of a number.
                fn sqrt(self) -> Self;
                /// Returns `e^(self)`, (the exponential function).
                fn exp(self) -> Self;
                /// Returns `2^(self)`.
                fn exp2(self) -> Self;
                /// Returns the natural logarithm of the number.
                fn ln(self) -> Self;
                /// Returns the logarithm of the number with respect to an arbitrary base.
                fn log(self, base: Self) -> Self;
                /// Returns the base 2 logarithm of the number.
                fn log2(self) -> Self;
                /// Returns the base 10 logarithm of the number.
                fn log10(self) -> Self;
                /// Converts radians to degrees.
                #[inline] fn to_degrees(self) -> Self {
                    let halfpi = Self::zero().acos();
                    let ninety = Self::from(90u8).unwrap();
                    self * ninety / halfpi
                }
                /// Converts degrees to radians.
                #[inline] fn to_radians(self) -> Self {
                    let halfpi = Self::zero().acos();
                    let ninety = Self::from(90u8).unwrap();
                    self * halfpi / ninety
                }
                /// Returns the maximum of the two numbers.
                fn max(self, other: Self) -> Self;
                /// Returns the minimum of the two numbers.
                fn min(self, other: Self) -> Self;
                /// Clamps a value between a min and max.
                fn clamp(self, min: Self, max: Self) -> Self {
                    num::traits::clamp(self, min, max)
                }
                /// The positive difference of two numbers.
                fn abs_sub(self, other: Self) -> Self;
                /// Take the cubic root of a number.
                fn cbrt(self) -> Self;
                /// Calculate the length of the hypotenuse of a right-angle triangle, legs of length `x` and `y`.
                fn hypot(self, other: Self) -> Self;
                /// Computes the sine of a number (in radians).
                fn sin(self) -> Self;
                /// Computes the cosine of a number (in radians).
                fn cos(self) -> Self;
                /// Computes the tangent of a number (in radians).
                fn tan(self) -> Self;
                /// Computes the arcsine of a number.
                fn asin(self) -> Self;
                /// Computes the arccosine of a number.
                fn acos(self) -> Self;
                /// Computes the arctangent of a number.
                fn atan(self) -> Self;
                /// Computes the four quadrant arctangent of `self` (`y`) and `other` (`x`).
                fn atan2(self, other: Self) -> Self;
                /// Simultaneously computes the sine and cosine of the number, `x`.
                fn sin_cos(self) -> (Self, Self);
                /// Returns `e^(self) - 1` in a way that is accurate even if the number is close to zero.
                fn exp_m1(self) -> Self;
                /// Returns `ln(1+n)` more accurately than if the operations were performed separately.
                fn ln_1p(self) -> Self;
                /// Hyperbolic sine function.
                fn sinh(self) -> Self;
                /// Hyperbolic cosine function.
                fn cosh(self) -> Self;
                /// Hyperbolic tangent function.
                fn tanh(self) -> Self;
                /// Inverse hyperbolic sine function.
                fn asinh(self) -> Self;
                /// Inverse hyperbolic cosine function.
                fn acosh(self) -> Self;
                /// Inverse hyperbolic tangent function.
                fn atanh(self) -> Self;
                /// Returns the mantissa, base 2 exponent, and sign as integers, respectively.
                fn integer_decode(self) -> (u64, i16, i8);
                /// Returns a number composed of the magnitude of `self` and the sign of `sign`.
                fn copysign(self, sign: Self) -> Self {
                    if self.is_sign_negative() == sign.is_sign_negative() {
                        self
                    } else {
                        self.neg()
                    }
                }
            }
            macro_rules! float_impl_std
            {
                ($T:ident $decode:ident) => {
                    impl Float for $T {
                        constant! {
                            nan() -> $T::NAN;
                            infinity() -> $T::INFINITY;
                            neg_infinity() -> $T::NEG_INFINITY;
                            neg_zero() -> -0.0;
                            min_value() -> $T::MIN;
                            min_positive_value() -> $T::MIN_POSITIVE;
                            epsilon() -> $T::EPSILON;
                            max_value() -> $T::MAX;
                        }
                        #[inline]
                        #[allow(deprecated)]
                        fn abs_sub(self, other: Self) -> Self {
                            <$T>::abs_sub(self, other)
                        }
                        #[inline]
                        fn integer_decode(self) -> (u64, i16, i8) {
                            $decode(self)
                        }
                        forward! {
                            Self::is_nan(self) -> bool;
                            Self::is_infinite(self) -> bool;
                            Self::is_finite(self) -> bool;
                            Self::is_normal(self) -> bool;
                            Self::is_subnormal(self) -> bool;
                            Self::classify(self) -> FpCategory;
                            Self::clamp(self, min: Self, max: Self) -> Self;
                            Self::floor(self) -> Self;
                            Self::ceil(self) -> Self;
                            Self::round(self) -> Self;
                            Self::trunc(self) -> Self;
                            Self::fract(self) -> Self;
                            Self::abs(self) -> Self;
                            Self::signum(self) -> Self;
                            Self::is_sign_positive(self) -> bool;
                            Self::is_sign_negative(self) -> bool;
                            Self::mul_add(self, a: Self, b: Self) -> Self;
                            Self::recip(self) -> Self;
                            Self::powi(self, n: i32) -> Self;
                            Self::powf(self, n: Self) -> Self;
                            Self::sqrt(self) -> Self;
                            Self::exp(self) -> Self;
                            Self::exp2(self) -> Self;
                            Self::ln(self) -> Self;
                            Self::log(self, base: Self) -> Self;
                            Self::log2(self) -> Self;
                            Self::log10(self) -> Self;
                            Self::to_degrees(self) -> Self;
                            Self::to_radians(self) -> Self;
                            Self::max(self, other: Self) -> Self;
                            Self::min(self, other: Self) -> Self;
                            Self::cbrt(self) -> Self;
                            Self::hypot(self, other: Self) -> Self;
                            Self::sin(self) -> Self;
                            Self::cos(self) -> Self;
                            Self::tan(self) -> Self;
                            Self::asin(self) -> Self;
                            Self::acos(self) -> Self;
                            Self::atan(self) -> Self;
                            Self::atan2(self, other: Self) -> Self;
                            Self::sin_cos(self) -> (Self, Self);
                            Self::exp_m1(self) -> Self;
                            Self::ln_1p(self) -> Self;
                            Self::sinh(self) -> Self;
                            Self::cosh(self) -> Self;
                            Self::tanh(self) -> Self;
                            Self::asinh(self) -> Self;
                            Self::acosh(self) -> Self;
                            Self::atanh(self) -> Self;
                            Self::copysign(self, sign: Self) -> Self;
                        }
                    }
                };
            }
            
            fn integer_decode_f32(f: f32) -> (u64, i16, i8)
            {
                let bits: u32 = f.to_bits();
                let sign: i8 = if bits >> 31 == 0 { 1 } else { -1 };
                let mut exponent: i16 = ((bits >> 23) & 0xff) as i16;
                let mantissa = if exponent == 0 {
                    (bits & 0x7fffff) << 1
                } else {
                    (bits & 0x7fffff) | 0x800000
                };
               
                exponent -= 127 + 23;
                (mantissa as u64, exponent, sign)
            }
            fn integer_decode_f64(f: f64) -> (u64, i16, i8)
            {
                let bits: u64 = f.to_bits();
                let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
                let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
                let mantissa = if exponent == 0 {
                    (bits & 0xfffffffffffff) << 1
                } else {
                    (bits & 0xfffffffffffff) | 0x10000000000000
                };
               
                exponent -= 1023 + 52;
                (mantissa, exponent, sign)
            }
            float_impl_std!(f32 integer_decode_f32);
            float_impl_std!(f64 integer_decode_f64);
            
            macro_rules! float_const_impl
            {
                ($(#[$doc:meta] $constant:ident,)+) => (
                    #[allow(non_snake_case)]
                    pub trait FloatConst {
                        $(#[$doc] fn $constant() -> Self;)+
                        #[doc = "Return the full circle constant `τ`."]
                        #[inline]
                        fn TAU() -> Self where Self: Sized + Add<Self, Output = Self> {
                            Self::PI() + Self::PI()
                        }
                        #[doc = "Return `log10(2.0)`."]
                        #[inline]
                        fn LOG10_2() -> Self where Self: Sized + Div<Self, Output = Self> {
                            Self::LN_2() / Self::LN_10()
                        }
                        #[doc = "Return `log2(10.0)`."]
                        #[inline]
                        fn LOG2_10() -> Self where Self: Sized + Div<Self, Output = Self> {
                            Self::LN_10() / Self::LN_2()
                        }
                    }
                    float_const_impl! { @float f32, $($constant,)+ }
                    float_const_impl! { @float f64, $($constant,)+ }
                );
                (@float $T:ident, $($constant:ident,)+) => (
                    impl FloatConst for $T {
                        constant! {
                            $( $constant() -> $T::consts::$constant; )+
                            TAU() -> 6.28318530717958647692528676655900577;
                            LOG10_2() -> 0.301029995663981195213738894724493027;
                            LOG2_10() -> 3.32192809488736234787031942948939018;
                        }
                    }
                );
            }
            float_const_impl!
            {
                #[doc = "Return Euler’s number."]
                E,
                #[doc = "Return `1.0 / π`."]
                FRAC_1_PI,
                #[doc = "Return `1.0 / sqrt(2.0)`."]
                FRAC_1_SQRT_2,
                #[doc = "Return `2.0 / π`."]
                FRAC_2_PI,
                #[doc = "Return `2.0 / sqrt(π)`."]
                FRAC_2_SQRT_PI,
                #[doc = "Return `π / 2.0`."]
                FRAC_PI_2,
                #[doc = "Return `π / 3.0`."]
                FRAC_PI_3,
                #[doc = "Return `π / 4.0`."]
                FRAC_PI_4,
                #[doc = "Return `π / 6.0`."]
                FRAC_PI_6,
                #[doc = "Return `π / 8.0`."]
                FRAC_PI_8,
                #[doc = "Return `ln(10.0)`."]
                LN_10,
                #[doc = "Return `ln(2.0)`."]
                LN_2,
                #[doc = "Return `log10(e)`."]
                LOG10_E,
                #[doc = "Return `log2(e)`."]
                LOG2_E,
                #[doc = "Return Archimedes’ constant `π`."]
                PI,
                #[doc = "Return `sqrt(2.0)`."]
                SQRT_2,
            }
            /// Trait for floating point numbers that provide an implementation of the `totalOrder` predicate.
            pub trait TotalOrder
            {
                /// Return the ordering between `self` and `other`.
                fn total_cmp(&self, other: &Self) -> Ordering;
            }
            macro_rules! totalorder_impl
            {
                ($T:ident, $I:ident, $U:ident, $bits:expr) => {
                    impl TotalOrder for $T {
                        #[inline]
                        #[cfg(has_total_cmp)]
                        fn total_cmp(&self, other: &Self) -> Ordering {
                           
                            Self::total_cmp(&self, other)
                        }
                        #[inline]
                        #[cfg(not(has_total_cmp))]
                        fn total_cmp(&self, other: &Self) -> Ordering {
                           
                            let mut left = self.to_bits() as $I;
                            let mut right = other.to_bits() as $I;

                            left ^= (((left >> ($bits - 1)) as $U) >> 1) as $I;
                            right ^= (((right >> ($bits - 1)) as $U) >> 1) as $I;

                            left.cmp(&right)
                        }
                    }
                };
            }
            totalorder_impl!(f64, i64, u64, 64);
            totalorder_impl!(f32, i32, u32, 32);
        } pub use self::float::{ Float, FloatConst };

        pub mod identities
        {
            use ::
            {
                num::{ Saturating, Wrapping },
                ops::{ Add, Mul },
                *,
            };
            /*
            */
            /// Defines an additive identity element for `Self`.
            pub trait Zero: Sized + Add<Self, Output = Self>
            {
                /// Returns the additive identity element of `Self`, `0`.
               
                fn zero() -> Self;
                /// Sets `self` to the additive identity element of `Self`, `0`.
                fn set_zero(&mut self) {
                    *self = Zero::zero();
                }
                /// Returns `true` if `self` is equal to the additive identity.
                fn is_zero(&self) -> bool;
            }
            /// Defines an associated constant representing the additive identity element for `Self`.
            pub trait ConstZero: Zero
            {
                /// The additive identity element of `Self`, `0`.
                const ZERO: Self;
            }
            macro_rules! zero_impl
            {
                ($t:ty, $v:expr) => {
                    impl Zero for $t {
                        #[inline]
                        fn zero() -> $t {
                            $v
                        }
                        #[inline]
                        fn is_zero(&self) -> bool {
                            *self == $v
                        }
                    }
                    impl ConstZero for $t {
                        const ZERO: Self = $v;
                    }
                };
            }
            zero_impl!(usize, 0);
            zero_impl!(u8, 0);
            zero_impl!(u16, 0);
            zero_impl!(u32, 0);
            zero_impl!(u64, 0);
            zero_impl!(u128, 0);

            zero_impl!(isize, 0);
            zero_impl!(i8, 0);
            zero_impl!(i16, 0);
            zero_impl!(i32, 0);
            zero_impl!(i64, 0);
            zero_impl!(i128, 0);

            zero_impl!(f32, 0.0);
            zero_impl!(f64, 0.0);

            impl<T: Zero> Zero for Wrapping<T> where
                Wrapping<T>: Add<Output = Wrapping<T>>,
            {
                fn is_zero(&self) -> bool {
                    self.0.is_zero()
                }
                fn set_zero(&mut self) {
                    self.0.set_zero();
                }
                fn zero() -> Self {
                    Wrapping(T::zero())
                }
            }
            
            impl<T: ConstZero> ConstZero for Wrapping<T> where
                Wrapping<T>: Add<Output = Wrapping<T>>,
            {
                const ZERO: Self = Wrapping(T::ZERO);
            }
            #[cfg(has_num_saturating)]
            impl<T: Zero> Zero for Saturating<T> where
                Saturating<T>: Add<Output = Saturating<T>>,
            {
                fn is_zero(&self) -> bool {
                    self.0.is_zero()
                }
                fn set_zero(&mut self) {
                    self.0.set_zero();
                }
                fn zero() -> Self {
                    Saturating(T::zero())
                }
            }
            #[cfg(has_num_saturating)]
            impl<T: ConstZero> ConstZero for Saturating<T> where
                Saturating<T>: Add<Output = Saturating<T>>,
            {
                const ZERO: Self = Saturating(T::ZERO);
            }
            /// Defines a multiplicative identity element for `Self`.
            /// a * 1 = a       ∀ a ∈ Self
            /// 1 * a = a       ∀ a ∈ Self
            /// ```
            pub trait One: Sized + Mul<Self, Output = Self> {
                /// Returns the multiplicative identity element of `Self`, `1`.
               
                fn one() -> Self;
                /// Sets `self` to the multiplicative identity element of `Self`, `1`.
                fn set_one(&mut self) {
                    *self = One::one();
                }
                /// Returns `true` if `self` is equal to the multiplicative identity.
                #[inline] fn is_one(&self) -> bool
                where
                    Self: PartialEq,
                {
                    *self == Self::one()
                }
            }
            /// Defines an associated constant representing the multiplicative identity
            /// element for `Self`.
            pub trait ConstOne: One {
                /// The multiplicative identity element of `Self`, `1`.
                const ONE: Self;
            }
            macro_rules! one_impl {
                ($t:ty, $v:expr) => {
                    impl One for $t {
                        #[inline]
                        fn one() -> $t {
                            $v
                        }
                        #[inline]
                        fn is_one(&self) -> bool {
                            *self == $v
                        }
                    }
                    impl ConstOne for $t {
                        const ONE: Self = $v;
                    }
                };
            }
            one_impl!(usize, 1);
            one_impl!(u8, 1);
            one_impl!(u16, 1);
            one_impl!(u32, 1);
            one_impl!(u64, 1);
            one_impl!(u128, 1);

            one_impl!(isize, 1);
            one_impl!(i8, 1);
            one_impl!(i16, 1);
            one_impl!(i32, 1);
            one_impl!(i64, 1);
            one_impl!(i128, 1);

            one_impl!(f32, 1.0);
            one_impl!(f64, 1.0);

            impl<T: One> One for Wrapping<T> where
                Wrapping<T>: Mul<Output = Wrapping<T>>,
            {
                fn set_one(&mut self) {
                    self.0.set_one();
                }
                fn one() -> Self {
                    Wrapping(T::one())
                }
            }
            
            impl<T: ConstOne> ConstOne for Wrapping<T> where
                Wrapping<T>: Mul<Output = Wrapping<T>>,
            {
                const ONE: Self = Wrapping(T::ONE);
            }
            #[cfg(has_num_saturating)]
            impl<T: One> One for Saturating<T> where
                Saturating<T>: Mul<Output = Saturating<T>>,
            {
                fn set_one(&mut self) {
                    self.0.set_one();
                }
                fn one() -> Self {
                    Saturating(T::one())
                }
            }
            #[cfg(has_num_saturating)]
            impl<T: ConstOne> ConstOne for Saturating<T> where
                Saturating<T>: Mul<Output = Saturating<T>>,
            {
                const ONE: Self = Saturating(T::ONE);
            }
           

            /// Returns the additive identity, `0`.
            #[inline( always )] pub fn zero<T: Zero>() -> T {
                Zero::zero()
            }
            /// Returns the multiplicative identity, `1`.
            #[inline( always )] pub fn one<T: One>() -> T {
                One::one()
            }
        } pub use self::identities::{one, zero, ConstOne, ConstZero, One, Zero};

        pub mod int
        {
            use ::
            {
                num::
                {
                    traits::
                    {
                        bounds::Bounded,
                        ops::
                        {
                            checked::*,
                            saturating::Saturating,
                        },
                        Num, NumCast,
                    },
                },
                ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr},
                *,
            };
            /*
            */
            /// Generic trait for primitive integers.
            pub trait PrimInt:
                Sized
                + Copy
                + Num
                + NumCast
                + Bounded
                + PartialOrd
                + Ord
                + Eq
                + Not<Output = Self>
                + BitAnd<Output = Self>
                + BitOr<Output = Self>
                + BitXor<Output = Self>
                + Shl<usize, Output = Self>
                + Shr<usize, Output = Self>
                + CheckedAdd<Output = Self>
                + CheckedSub<Output = Self>
                + CheckedMul<Output = Self>
                + CheckedDiv<Output = Self>
                + Saturating
            {
                /// Returns the number of ones in the binary representation of `self`.
                fn count_ones(self) -> u32;
                /// Returns the number of zeros in the binary representation of `self`.
                fn count_zeros(self) -> u32;
                /// Returns the number of leading ones in the binary representation
                /// of `self`.
                fn leading_ones(self) -> u32 {
                    (!self).leading_zeros()
                }
                /// Returns the number of leading zeros in the binary representation
                /// of `self`.
                fn leading_zeros(self) -> u32;
                /// Returns the number of trailing ones in the binary representation
                /// of `self`.
                fn trailing_ones(self) -> u32 {
                    (!self).trailing_zeros()
                }
                /// Returns the number of trailing zeros in the binary representation
                /// of `self`.
                fn trailing_zeros(self) -> u32;
                /// Shifts the bits to the left by a specified amount, `n`, wrapping
                /// the truncated bits to the end of the resulting integer.
                fn rotate_left(self, n: u32) -> Self;
                /// Shifts the bits to the right by a specified amount, `n`, wrapping
                /// the truncated bits to the beginning of the resulting integer.
                fn rotate_right(self, n: u32) -> Self;
                /// Shifts the bits to the left by a specified amount, `n`, filling
                /// zeros in the least significant bits.
                fn signed_shl(self, n: u32) -> Self;
                /// Shifts the bits to the right by a specified amount, `n`, copying
                /// the "sign bit" in the most significant bits even for unsigned types.
                fn signed_shr(self, n: u32) -> Self;
                /// Shifts the bits to the left by a specified amount, `n`, filling
                /// zeros in the least significant bits.
                fn unsigned_shl(self, n: u32) -> Self;
                /// Shifts the bits to the right by a specified amount, `n`, filling
                /// zeros in the most significant bits.
                fn unsigned_shr(self, n: u32) -> Self;
                /// Reverses the byte order of the integer.
                fn swap_bytes(self) -> Self;
                /// Reverses the order of bits in the integer.
                fn reverse_bits(self) -> Self {
                    reverse_bits_fallback(self)
                }
                /// Convert an integer from big endian to the target's endianness.
                fn from_be(x: Self) -> Self;
                /// Convert an integer from little endian to the target's endianness.
                fn from_le(x: Self) -> Self;
                /// Convert `self` to big endian from the target's endianness.
                fn to_be(self) -> Self;
                /// Convert `self` to little endian from the target's endianness.
                fn to_le(self) -> Self;
                /// Raises self to the power of `exp`, using exponentiation by squaring.
                fn pow(self, exp: u32) -> Self;
            }
            fn one_per_byte<P: PrimInt>() -> P {
               
               
               
               
                let mut ret = P::one();
                let mut shift = 8;
                let mut b = ret.count_zeros() >> 3;
                while b != 0 {
                    ret = (ret << shift) | ret;
                    shift <<= 1;
                    b >>= 1;
                }
                ret
            }
            fn reverse_bits_fallback<P: PrimInt>(i: P) -> P {
                let rep_01: P = one_per_byte();
                let rep_03 = (rep_01 << 1) | rep_01;
                let rep_05 = (rep_01 << 2) | rep_01;
                let rep_0f = (rep_03 << 2) | rep_03;
                let rep_33 = (rep_03 << 4) | rep_03;
                let rep_55 = (rep_05 << 4) | rep_05;

               
               
                let mut ret = i.swap_bytes();
                ret = ((ret & rep_0f) << 4) | ((ret >> 4) & rep_0f);
                ret = ((ret & rep_33) << 2) | ((ret >> 2) & rep_33);
                ret = ((ret & rep_55) << 1) | ((ret >> 1) & rep_55);
                ret
            }
            macro_rules! prim_int_impl {
                ($T:ty, $S:ty, $U:ty) => {
                    impl PrimInt for $T {
                        #[inline]
                        fn count_ones(self) -> u32 {
                            <$T>::count_ones(self)
                        }
                        #[inline]
                        fn count_zeros(self) -> u32 {
                            <$T>::count_zeros(self)
                        }
                        #[inline]
                        fn leading_ones(self) -> u32 {
                            <$T>::leading_ones(self)
                        }
                        #[inline]
                        fn leading_zeros(self) -> u32 {
                            <$T>::leading_zeros(self)
                        }
                        #[inline]
                        fn trailing_ones(self) -> u32 {
                            <$T>::trailing_ones(self)
                        }
                        #[inline]
                        fn trailing_zeros(self) -> u32 {
                            <$T>::trailing_zeros(self)
                        }
                        #[inline]
                        fn rotate_left(self, n: u32) -> Self {
                            <$T>::rotate_left(self, n)
                        }
                        #[inline]
                        fn rotate_right(self, n: u32) -> Self {
                            <$T>::rotate_right(self, n)
                        }
                        #[inline]
                        fn signed_shl(self, n: u32) -> Self {
                            ((self as $S) << n) as $T
                        }
                        #[inline]
                        fn signed_shr(self, n: u32) -> Self {
                            ((self as $S) >> n) as $T
                        }
                        #[inline]
                        fn unsigned_shl(self, n: u32) -> Self {
                            ((self as $U) << n) as $T
                        }
                        #[inline]
                        fn unsigned_shr(self, n: u32) -> Self {
                            ((self as $U) >> n) as $T
                        }
                        #[inline]
                        fn swap_bytes(self) -> Self {
                            <$T>::swap_bytes(self)
                        }
                        #[inline]
                        fn reverse_bits(self) -> Self {
                            <$T>::reverse_bits(self)
                        }
                        #[inline]
                        fn from_be(x: Self) -> Self {
                            <$T>::from_be(x)
                        }
                        #[inline]
                        fn from_le(x: Self) -> Self {
                            <$T>::from_le(x)
                        }
                        #[inline]
                        fn to_be(self) -> Self {
                            <$T>::to_be(self)
                        }
                        #[inline]
                        fn to_le(self) -> Self {
                            <$T>::to_le(self)
                        }
                        #[inline]
                        fn pow(self, exp: u32) -> Self {
                            <$T>::pow(self, exp)
                        }
                    }
                };
            }
            
            prim_int_impl!(u8, i8, u8);
            prim_int_impl!(u16, i16, u16);
            prim_int_impl!(u32, i32, u32);
            prim_int_impl!(u64, i64, u64);
            prim_int_impl!(u128, i128, u128);
            prim_int_impl!(usize, isize, usize);
            prim_int_impl!(i8, i8, u8);
            prim_int_impl!(i16, i16, u16);
            prim_int_impl!(i32, i32, u32);
            prim_int_impl!(i64, i64, u64);
            prim_int_impl!(i128, i128, u128);
            prim_int_impl!(isize, isize, usize);
        } pub use self::int::PrimInt;

        pub mod ops
        {
            use ::
            {
                *,
            };
            /*
            */
            pub mod bytes
            {
                /*!
                */
                use ::
                {
                    borrow::{ Borrow, BorrowMut },
                    cmp::{ Eq, Ord, PartialEq, PartialOrd },
                    fmt::{ Debug },
                    hash::{ Hash },
                    *,
                };
                /*
                */
                pub trait NumBytes:
                    Debug
                    + AsRef<[u8]>
                    + AsMut<[u8]>
                    + PartialEq
                    + Eq
                    + PartialOrd
                    + Ord
                    + Hash
                    + Borrow<[u8]>
                    + BorrowMut<[u8]>
                {
                }
                impl<T> NumBytes for T where
                    T: Debug
                        + AsRef<[u8]>
                        + AsMut<[u8]>
                        + PartialEq
                        + Eq
                        + PartialOrd
                        + Ord
                        + Hash
                        + Borrow<[u8]>
                        + BorrowMut<[u8]>
                        + ?Sized
                {
                }
                pub trait ToBytes {
                    type Bytes: NumBytes;

                    /// Return the memory representation of this number as a byte array in big-endian byte order.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::ToBytes;
                    ///
                    /// let bytes = ToBytes::to_be_bytes(&0x12345678u32);
                    /// assert_eq!(bytes, [0x12, 0x34, 0x56, 0x78]);
                    /// ```
                    fn to_be_bytes(&self) -> Self::Bytes;

                    /// Return the memory representation of this number as a byte array in little-endian byte order.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::ToBytes;
                    ///
                    /// let bytes = ToBytes::to_le_bytes(&0x12345678u32);
                    /// assert_eq!(bytes, [0x78, 0x56, 0x34, 0x12]);
                    /// ```
                    fn to_le_bytes(&self) -> Self::Bytes;

                    /// Return the memory representation of this number as a byte array in native byte order.
                    ///
                    /// As the target platform's native endianness is used,
                    /// portable code should use [`to_be_bytes`] or [`to_le_bytes`], as appropriate, instead.
                    ///
                    /// [`to_be_bytes`]: #method.to_be_bytes
                    /// [`to_le_bytes`]: #method.to_le_bytes
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::ToBytes;
                    ///
                    /// #[cfg(target_endian = "big")]
                    /// let expected = [0x12, 0x34, 0x56, 0x78];
                    ///
                    /// #[cfg(target_endian = "little")]
                    /// let expected = [0x78, 0x56, 0x34, 0x12];
                    ///
                    /// let bytes = ToBytes::to_ne_bytes(&0x12345678u32);
                    /// assert_eq!(bytes, expected)
                    /// ```
                    fn to_ne_bytes(&self) -> Self::Bytes {
                        #[cfg(target_endian = "big")]
                        let bytes = self.to_be_bytes();
                        #[cfg(target_endian = "little")]
                        let bytes = self.to_le_bytes();
                        bytes
                    }
                }
                pub trait FromBytes: Sized {
                    type Bytes: NumBytes + ?Sized;

                    /// Create a number from its representation as a byte array in big endian.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::FromBytes;
                    ///
                    /// let value: u32 = FromBytes::from_be_bytes(&[0x12, 0x34, 0x56, 0x78]);
                    /// assert_eq!(value, 0x12345678);
                    /// ```
                    fn from_be_bytes(bytes: &Self::Bytes) -> Self;

                    /// Create a number from its representation as a byte array in little endian.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::FromBytes;
                    ///
                    /// let value: u32 = FromBytes::from_le_bytes(&[0x78, 0x56, 0x34, 0x12]);
                    /// assert_eq!(value, 0x12345678);
                    /// ```
                    fn from_le_bytes(bytes: &Self::Bytes) -> Self;

                    /// Create a number from its memory representation as a byte array in native endianness.
                    ///
                    /// As the target platform's native endianness is used,
                    /// portable code likely wants to use [`from_be_bytes`] or [`from_le_bytes`], as appropriate instead.
                    ///
                    /// [`from_be_bytes`]: #method.from_be_bytes
                    /// [`from_le_bytes`]: #method.from_le_bytes
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::FromBytes;
                    ///
                    /// #[cfg(target_endian = "big")]
                    /// let bytes = [0x12, 0x34, 0x56, 0x78];
                    ///
                    /// #[cfg(target_endian = "little")]
                    /// let bytes = [0x78, 0x56, 0x34, 0x12];
                    ///
                    /// let value: u32 = FromBytes::from_ne_bytes(&bytes);
                    /// assert_eq!(value, 0x12345678)
                    /// ```
                    fn from_ne_bytes(bytes: &Self::Bytes) -> Self {
                        #[cfg(target_endian = "big")]
                        let this = Self::from_be_bytes(bytes);
                        #[cfg(target_endian = "little")]
                        let this = Self::from_le_bytes(bytes);
                        this
                    }
                }
                macro_rules! float_to_from_bytes_impl {
                    ($T:ty, $L:expr) => {
                        impl ToBytes for $T {
                            type Bytes = [u8; $L];

                            #[inline]
                            fn to_be_bytes(&self) -> Self::Bytes {
                                <$T>::to_be_bytes(*self)
                            }
                            #[inline]
                            fn to_le_bytes(&self) -> Self::Bytes {
                                <$T>::to_le_bytes(*self)
                            }
                            #[inline]
                            fn to_ne_bytes(&self) -> Self::Bytes {
                                <$T>::to_ne_bytes(*self)
                            }
                        }
                        impl FromBytes for $T {
                            type Bytes = [u8; $L];

                            #[inline]
                            fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                                <$T>::from_be_bytes(*bytes)
                            }
                            #[inline]
                            fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                                <$T>::from_le_bytes(*bytes)
                            }
                            #[inline]
                            fn from_ne_bytes(bytes: &Self::Bytes) -> Self {
                                <$T>::from_ne_bytes(*bytes)
                            }
                        }
                    };
                }
                macro_rules! int_to_from_bytes_impl {
                    ($T:ty, $L:expr) => {
                        impl ToBytes for $T {
                            type Bytes = [u8; $L];

                            #[inline]
                            fn to_be_bytes(&self) -> Self::Bytes {
                                <$T>::to_be_bytes(*self)
                            }
                            #[inline]
                            fn to_le_bytes(&self) -> Self::Bytes {
                                <$T>::to_le_bytes(*self)
                            }
                            #[inline]
                            fn to_ne_bytes(&self) -> Self::Bytes {
                                <$T>::to_ne_bytes(*self)
                            }
                        }
                        impl FromBytes for $T {
                            type Bytes = [u8; $L];

                            #[inline]
                            fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                                <$T>::from_be_bytes(*bytes)
                            }
                            #[inline]
                            fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                                <$T>::from_le_bytes(*bytes)
                            }
                            #[inline]
                            fn from_ne_bytes(bytes: &Self::Bytes) -> Self {
                                <$T>::from_ne_bytes(*bytes)
                            }
                        }
                    };
                }
                int_to_from_bytes_impl!(u8, 1);
                int_to_from_bytes_impl!(u16, 2);
                int_to_from_bytes_impl!(u32, 4);
                int_to_from_bytes_impl!(u64, 8);
                int_to_from_bytes_impl!(u128, 16);
                #[cfg(target_pointer_width = "64")]
                int_to_from_bytes_impl!(usize, 8);
                #[cfg(target_pointer_width = "32")]
                int_to_from_bytes_impl!(usize, 4);

                int_to_from_bytes_impl!(i8, 1);
                int_to_from_bytes_impl!(i16, 2);
                int_to_from_bytes_impl!(i32, 4);
                int_to_from_bytes_impl!(i64, 8);
                int_to_from_bytes_impl!(i128, 16);
                #[cfg(target_pointer_width = "64")]
                int_to_from_bytes_impl!(isize, 8);
                #[cfg(target_pointer_width = "32")]
                int_to_from_bytes_impl!(isize, 4);

                float_to_from_bytes_impl!(f32, 4);
                float_to_from_bytes_impl!(f64, 8);
            }
            
            pub mod checked
            {
                /*!
                */
                use ::
                {
                    ops::{Add, Div, Mul, Rem, Shl, Shr, Sub},
                    *,
                };
                /*
                */
                /// Performs addition, returning `None` if overflow occurred.
                pub trait CheckedAdd: Sized + Add<Self, Output = Self> {
                    /// Adds two numbers, checking for overflow. If overflow happens, `None` is
                    /// returned.
                    fn checked_add(&self, v: &Self) -> Option<Self>;
                }
                macro_rules! checked_impl {
                    ($trait_name:ident, $method:ident, $t:ty) => {
                        impl $trait_name for $t {
                            #[inline]
                            fn $method(&self, v: &$t) -> Option<$t> {
                                <$t>::$method(*self, *v)
                            }
                        }
                    };
                }
                checked_impl!(CheckedAdd, checked_add, u8);
                checked_impl!(CheckedAdd, checked_add, u16);
                checked_impl!(CheckedAdd, checked_add, u32);
                checked_impl!(CheckedAdd, checked_add, u64);
                checked_impl!(CheckedAdd, checked_add, usize);
                checked_impl!(CheckedAdd, checked_add, u128);

                checked_impl!(CheckedAdd, checked_add, i8);
                checked_impl!(CheckedAdd, checked_add, i16);
                checked_impl!(CheckedAdd, checked_add, i32);
                checked_impl!(CheckedAdd, checked_add, i64);
                checked_impl!(CheckedAdd, checked_add, isize);
                checked_impl!(CheckedAdd, checked_add, i128);
                /// Performs subtraction, returning `None` if overflow occurred.
                pub trait CheckedSub: Sized + Sub<Self, Output = Self> {
                    /// Subtracts two numbers, checking for overflow. If overflow happens,
                    /// `None` is returned.
                    fn checked_sub(&self, v: &Self) -> Option<Self>;
                }
                checked_impl!(CheckedSub, checked_sub, u8);
                checked_impl!(CheckedSub, checked_sub, u16);
                checked_impl!(CheckedSub, checked_sub, u32);
                checked_impl!(CheckedSub, checked_sub, u64);
                checked_impl!(CheckedSub, checked_sub, usize);
                checked_impl!(CheckedSub, checked_sub, u128);

                checked_impl!(CheckedSub, checked_sub, i8);
                checked_impl!(CheckedSub, checked_sub, i16);
                checked_impl!(CheckedSub, checked_sub, i32);
                checked_impl!(CheckedSub, checked_sub, i64);
                checked_impl!(CheckedSub, checked_sub, isize);
                checked_impl!(CheckedSub, checked_sub, i128);
                /// Performs multiplication, returning `None` if overflow occurred.
                pub trait CheckedMul: Sized + Mul<Self, Output = Self> {
                    /// Multiplies two numbers, checking for overflow. If overflow happens,
                    /// `None` is returned.
                    fn checked_mul(&self, v: &Self) -> Option<Self>;
                }
                checked_impl!(CheckedMul, checked_mul, u8);
                checked_impl!(CheckedMul, checked_mul, u16);
                checked_impl!(CheckedMul, checked_mul, u32);
                checked_impl!(CheckedMul, checked_mul, u64);
                checked_impl!(CheckedMul, checked_mul, usize);
                checked_impl!(CheckedMul, checked_mul, u128);

                checked_impl!(CheckedMul, checked_mul, i8);
                checked_impl!(CheckedMul, checked_mul, i16);
                checked_impl!(CheckedMul, checked_mul, i32);
                checked_impl!(CheckedMul, checked_mul, i64);
                checked_impl!(CheckedMul, checked_mul, isize);
                checked_impl!(CheckedMul, checked_mul, i128);
                /// Performs division, returning `None` on division by zero or if overflow
                /// occurred.
                pub trait CheckedDiv: Sized + Div<Self, Output = Self> {
                    /// Divides two numbers, checking for overflow and division by
                    /// zero. If any of that happens, `None` is returned.
                    fn checked_div(&self, v: &Self) -> Option<Self>;
                }
                checked_impl!(CheckedDiv, checked_div, u8);
                checked_impl!(CheckedDiv, checked_div, u16);
                checked_impl!(CheckedDiv, checked_div, u32);
                checked_impl!(CheckedDiv, checked_div, u64);
                checked_impl!(CheckedDiv, checked_div, usize);
                checked_impl!(CheckedDiv, checked_div, u128);

                checked_impl!(CheckedDiv, checked_div, i8);
                checked_impl!(CheckedDiv, checked_div, i16);
                checked_impl!(CheckedDiv, checked_div, i32);
                checked_impl!(CheckedDiv, checked_div, i64);
                checked_impl!(CheckedDiv, checked_div, isize);
                checked_impl!(CheckedDiv, checked_div, i128);
                /// Performs integral remainder, returning `None` on division by zero or if
                /// overflow occurred.
                pub trait CheckedRem: Sized + Rem<Self, Output = Self> {
                    /// Finds the remainder of dividing two numbers, checking for overflow and
                    /// division by zero. If any of that happens, `None` is returned.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::CheckedRem;
                    /// use ::i32::MIN;
                    ///
                    /// assert_eq!(CheckedRem::checked_rem(&10, &7), Some(3));
                    /// assert_eq!(CheckedRem::checked_rem(&10, &-7), Some(3));
                    /// assert_eq!(CheckedRem::checked_rem(&-10, &7), Some(-3));
                    /// assert_eq!(CheckedRem::checked_rem(&-10, &-7), Some(-3));
                    ///
                    /// assert_eq!(CheckedRem::checked_rem(&10, &0), None);
                    ///
                    /// assert_eq!(CheckedRem::checked_rem(&MIN, &1), Some(0));
                    /// assert_eq!(CheckedRem::checked_rem(&MIN, &-1), None);
                    /// ```
                    fn checked_rem(&self, v: &Self) -> Option<Self>;
                }
                checked_impl!(CheckedRem, checked_rem, u8);
                checked_impl!(CheckedRem, checked_rem, u16);
                checked_impl!(CheckedRem, checked_rem, u32);
                checked_impl!(CheckedRem, checked_rem, u64);
                checked_impl!(CheckedRem, checked_rem, usize);
                checked_impl!(CheckedRem, checked_rem, u128);

                checked_impl!(CheckedRem, checked_rem, i8);
                checked_impl!(CheckedRem, checked_rem, i16);
                checked_impl!(CheckedRem, checked_rem, i32);
                checked_impl!(CheckedRem, checked_rem, i64);
                checked_impl!(CheckedRem, checked_rem, isize);
                checked_impl!(CheckedRem, checked_rem, i128);

                macro_rules! checked_impl_unary {
                    ($trait_name:ident, $method:ident, $t:ty) => {
                        impl $trait_name for $t {
                            #[inline]
                            fn $method(&self) -> Option<$t> {
                                <$t>::$method(*self)
                            }
                        }
                    };
                }
                /// Performs negation, returning `None` if the result can't be represented.
                pub trait CheckedNeg: Sized {
                    /// Negates a number, returning `None` for results that can't be represented, like signed `MIN`
                    /// values that can't be positive, or non-zero unsigned values that can't be negative.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::CheckedNeg;
                    /// use ::i32::MIN;
                    ///
                    /// assert_eq!(CheckedNeg::checked_neg(&1_i32), Some(-1));
                    /// assert_eq!(CheckedNeg::checked_neg(&-1_i32), Some(1));
                    /// assert_eq!(CheckedNeg::checked_neg(&MIN), None);
                    ///
                    /// assert_eq!(CheckedNeg::checked_neg(&0_u32), Some(0));
                    /// assert_eq!(CheckedNeg::checked_neg(&1_u32), None);
                    /// ```
                    fn checked_neg(&self) -> Option<Self>;
                }
                checked_impl_unary!(CheckedNeg, checked_neg, u8);
                checked_impl_unary!(CheckedNeg, checked_neg, u16);
                checked_impl_unary!(CheckedNeg, checked_neg, u32);
                checked_impl_unary!(CheckedNeg, checked_neg, u64);
                checked_impl_unary!(CheckedNeg, checked_neg, usize);
                checked_impl_unary!(CheckedNeg, checked_neg, u128);

                checked_impl_unary!(CheckedNeg, checked_neg, i8);
                checked_impl_unary!(CheckedNeg, checked_neg, i16);
                checked_impl_unary!(CheckedNeg, checked_neg, i32);
                checked_impl_unary!(CheckedNeg, checked_neg, i64);
                checked_impl_unary!(CheckedNeg, checked_neg, isize);
                checked_impl_unary!(CheckedNeg, checked_neg, i128);
                /// Performs shift left, returning `None` on shifts larger than or equal to
                /// the type width.
                pub trait CheckedShl: Sized + Shl<u32, Output = Self> {
                    /// Checked shift left. Computes `self << rhs`, returning `None`
                    /// if `rhs` is larger than or equal to the number of bits in `self`.
                    ///
                    /// ```
                    /// use ::num::traits::CheckedShl;
                    ///
                    /// let x: u16 = 0x0001;
                    ///
                    /// assert_eq!(CheckedShl::checked_shl(&x, 0),  Some(0x0001));
                    /// assert_eq!(CheckedShl::checked_shl(&x, 1),  Some(0x0002));
                    /// assert_eq!(CheckedShl::checked_shl(&x, 15), Some(0x8000));
                    /// assert_eq!(CheckedShl::checked_shl(&x, 16), None);
                    /// ```
                    fn checked_shl(&self, rhs: u32) -> Option<Self>;
                }
                macro_rules! checked_shift_impl {
                    ($trait_name:ident, $method:ident, $t:ty) => {
                        impl $trait_name for $t {
                            #[inline]
                            fn $method(&self, rhs: u32) -> Option<$t> {
                                <$t>::$method(*self, rhs)
                            }
                        }
                    };
                }
                checked_shift_impl!(CheckedShl, checked_shl, u8);
                checked_shift_impl!(CheckedShl, checked_shl, u16);
                checked_shift_impl!(CheckedShl, checked_shl, u32);
                checked_shift_impl!(CheckedShl, checked_shl, u64);
                checked_shift_impl!(CheckedShl, checked_shl, usize);
                checked_shift_impl!(CheckedShl, checked_shl, u128);

                checked_shift_impl!(CheckedShl, checked_shl, i8);
                checked_shift_impl!(CheckedShl, checked_shl, i16);
                checked_shift_impl!(CheckedShl, checked_shl, i32);
                checked_shift_impl!(CheckedShl, checked_shl, i64);
                checked_shift_impl!(CheckedShl, checked_shl, isize);
                checked_shift_impl!(CheckedShl, checked_shl, i128);
                /// Performs shift right, returning `None` on shifts larger than or equal to
                /// the type width.
                pub trait CheckedShr: Sized + Shr<u32, Output = Self> {
                    /// Checked shift right. Computes `self >> rhs`, returning `None`
                    /// if `rhs` is larger than or equal to the number of bits in `self`.
                    ///
                    /// ```
                    /// use ::num::traits::CheckedShr;
                    ///
                    /// let x: u16 = 0x8000;
                    ///
                    /// assert_eq!(CheckedShr::checked_shr(&x, 0),  Some(0x8000));
                    /// assert_eq!(CheckedShr::checked_shr(&x, 1),  Some(0x4000));
                    /// assert_eq!(CheckedShr::checked_shr(&x, 15), Some(0x0001));
                    /// assert_eq!(CheckedShr::checked_shr(&x, 16), None);
                    /// ```
                    fn checked_shr(&self, rhs: u32) -> Option<Self>;
                }
                checked_shift_impl!(CheckedShr, checked_shr, u8);
                checked_shift_impl!(CheckedShr, checked_shr, u16);
                checked_shift_impl!(CheckedShr, checked_shr, u32);
                checked_shift_impl!(CheckedShr, checked_shr, u64);
                checked_shift_impl!(CheckedShr, checked_shr, usize);
                checked_shift_impl!(CheckedShr, checked_shr, u128);

                checked_shift_impl!(CheckedShr, checked_shr, i8);
                checked_shift_impl!(CheckedShr, checked_shr, i16);
                checked_shift_impl!(CheckedShr, checked_shr, i32);
                checked_shift_impl!(CheckedShr, checked_shr, i64);
                checked_shift_impl!(CheckedShr, checked_shr, isize);
                checked_shift_impl!(CheckedShr, checked_shr, i128);

            }
            
            pub mod euclid
            {
                /*!
                */
                use ::
                {
                    ops::{Div, Rem},
                    *,
                };
                /*
                */
                pub trait Euclid: Sized + Div<Self, Output = Self> + Rem<Self, Output = Self> {
                    /// Calculates Euclidean division, the matching method for `rem_euclid`.
                    ///
                    /// This computes the integer `n` such that
                    /// `self = n * v + self.rem_euclid(v)`.
                    /// In other words, the result is `self / v` rounded to the integer `n`
                    /// such that `self >= n * v`.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::Euclid;
                    ///
                    /// let a: i32 = 7;
                    /// let b: i32 = 4;
                    /// assert_eq!(Euclid::div_euclid(&a, &b), 1);
                    /// assert_eq!(Euclid::div_euclid(&-a, &b), -2);
                    /// assert_eq!(Euclid::div_euclid(&a, &-b), -1);
                    /// assert_eq!(Euclid::div_euclid(&-a, &-b), 2);
                    /// ```
                    fn div_euclid(&self, v: &Self) -> Self;

                    /// Calculates the least nonnegative remainder of `self (mod v)`.
                    ///
                    /// In particular, the return value `r` satisfies `0.0 <= r < v.abs()` in
                    /// most cases. However, due to a floating point round-off error it can
                    /// result in `r == v.abs()`, violating the mathematical definition, if
                    /// `self` is much smaller than `v.abs()` in magnitude and `self < 0.0`.
                    /// This result is not an element of the function's codomain, but it is the
                    /// closest floating point number in the real numbers and thus fulfills the
                    /// property `self == self.div_euclid(v) * v + self.rem_euclid(v)`
                    /// approximatively.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::num::traits::Euclid;
                    ///
                    /// let a: i32 = 7;
                    /// let b: i32 = 4;
                    /// assert_eq!(Euclid::rem_euclid(&a, &b), 3);
                    /// assert_eq!(Euclid::rem_euclid(&-a, &b), 1);
                    /// assert_eq!(Euclid::rem_euclid(&a, &-b), 3);
                    /// assert_eq!(Euclid::rem_euclid(&-a, &-b), 1);
                    /// ```
                    fn rem_euclid(&self, v: &Self) -> Self;

                    /// Returns both the quotient and remainder from Euclidean division.
                    ///
                    /// By default, it internally calls both `Euclid::div_euclid` and `Euclid::rem_euclid`,
                    /// but it can be overridden in order to implement some optimization.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// # use ::num::traits::Euclid;
                    /// let x = 5u8;
                    /// let y = 3u8;
                    ///
                    /// let div = Euclid::div_euclid(&x, &y);
                    /// let rem = Euclid::rem_euclid(&x, &y);
                    ///
                    /// assert_eq!((div, rem), Euclid::div_rem_euclid(&x, &y));
                    /// ```
                    fn div_rem_euclid(&self, v: &Self) -> (Self, Self) {
                        (self.div_euclid(v), self.rem_euclid(v))
                    }
                }
                macro_rules! euclid_forward_impl 
                {
                    ($($t:ty)*) => {$(
                        impl Euclid for $t {
                            #[inline]
                            fn div_euclid(&self, v: &$t) -> Self {
                                <$t>::div_euclid(*self, *v)
                            }
                            #[inline]
                            fn rem_euclid(&self, v: &$t) -> Self {
                                <$t>::rem_euclid(*self, *v)
                            }
                        }
                    )*}
                }
                euclid_forward_impl!(isize i8 i16 i32 i64 i128);
                euclid_forward_impl!(usize u8 u16 u32 u64 u128);
                euclid_forward_impl!(f32 f64);

                pub trait CheckedEuclid: Euclid
                {
                    /// Performs euclid division, returning `None` on division by zero or if
                    /// overflow occurred.
                    fn checked_div_euclid(&self, v: &Self) -> Option<Self>;

                    /// Finds the euclid remainder of dividing two numbers, returning `None` on
                    /// division by zero or if overflow occurred.
                    fn checked_rem_euclid(&self, v: &Self) -> Option<Self>;

                    /// Returns both the quotient and remainder from checked Euclidean division,
                    /// returning `None` on division by zero or if overflow occurred.
                    ///
                    /// By default, it internally calls both `CheckedEuclid::checked_div_euclid` and `CheckedEuclid::checked_rem_euclid`,
                    /// but it can be overridden in order to implement some optimization.
                    /// # Examples
                    ///
                    /// ```
                    /// # use ::num::traits::CheckedEuclid;
                    /// let x = 5u8;
                    /// let y = 3u8;
                    ///
                    /// let div = CheckedEuclid::checked_div_euclid(&x, &y);
                    /// let rem = CheckedEuclid::checked_rem_euclid(&x, &y);
                    ///
                    /// assert_eq!(Some((div.unwrap(), rem.unwrap())), CheckedEuclid::checked_div_rem_euclid(&x, &y));
                    /// ```
                    fn checked_div_rem_euclid(&self, v: &Self) -> Option<(Self, Self)> {
                        Some((self.checked_div_euclid(v)?, self.checked_rem_euclid(v)?))
                    }
                }
                macro_rules! checked_euclid_forward_impl 
                {
                    ($($t:ty)*) => {$(
                        impl CheckedEuclid for $t {
                            #[inline]
                            fn checked_div_euclid(&self, v: &$t) -> Option<Self> {
                                <$t>::checked_div_euclid(*self, *v)
                            }
                            #[inline]
                            fn checked_rem_euclid(&self, v: &$t) -> Option<Self> {
                                <$t>::checked_rem_euclid(*self, *v)
                            }
                        }
                    )*}
                }
                checked_euclid_forward_impl!(isize i8 i16 i32 i64 i128);
                checked_euclid_forward_impl!(usize u8 u16 u32 u64 u128);
            }
            
            pub mod inv
            {
                /*!
                */
                use ::
                {
                    *,
                };
                /*
                */
                /// Unary operator for retrieving the multiplicative inverse, or reciprocal, of a value.
                pub trait Inv {
                    /// The result after applying the operator.
                    type Output;

                    /// Returns the multiplicative inverse of `self`.
                    ///
                    /// # Examples
                    ///
                    /// ```
                    /// use ::f64::INFINITY;
                    /// use ::num::traits::Inv;
                    ///
                    /// assert_eq!(7.0.inv() * 7.0, 1.0);
                    /// assert_eq!((-0.0).inv(), -INFINITY);
                    /// ```
                    fn inv(self) -> Self::Output;
                }
                impl Inv for f32 {
                    type Output = f32;
                    #[inline] fn inv(self) -> f32 {
                        1.0 / self
                    }
                }
                impl Inv for f64 {
                    type Output = f64;
                    #[inline] fn inv(self) -> f64 {
                        1.0 / self
                    }
                }
                impl<'a> Inv for &'a f32 {
                    type Output = f32;
                    #[inline] fn inv(self) -> f32 {
                        1.0 / *self
                    }
                }
                impl<'a> Inv for &'a f64 {
                    type Output = f64;
                    #[inline] fn inv(self) -> f64 {
                        1.0 / *self
                    }
                }
            }
            
            pub mod mul_add
            {
                /*!
                */
                use ::
                {
                    *,
                };
                /*
                */
                /// Fused multiply-add.
                pub trait MulAdd<A = Self, B = Self> 
                {
                    /// The resulting type after applying the fused multiply-add.
                    type Output;

                    /// Performs the fused multiply-add operation `(self * a) + b`
                    fn mul_add(self, a: A, b: B) -> Self::Output;
                }
                /// The fused multiply-add assignment operation `*self = (*self * a) + b`
                pub trait MulAddAssign<A = Self, B = Self> 
                {
                    /// Performs the fused multiply-add assignment operation `*self = (*self * a) + b`
                    fn mul_add_assign(&mut self, a: A, b: B);
                }
                
                impl MulAdd<f32, f32> for f32                    
                {
                    type Output = Self;

                    #[inline] fn mul_add(self, a: Self, b: Self) -> Self::Output {
                        <Self as ::num::traits::Float>::mul_add(self, a, b)
                    }
                }
                
                impl MulAdd<f64, f64> for f64
                {
                    type Output = Self;

                    #[inline] fn mul_add(self, a: Self, b: Self) -> Self::Output {
                        <Self as ::num::traits::Float>::mul_add(self, a, b)
                    }
                }
                macro_rules! mul_add_impl 
                {
                    ($trait_name:ident for $($t:ty)*) => {$(
                        impl $trait_name for $t {
                            type Output = Self;

                            #[inline]
                            fn mul_add(self, a: Self, b: Self) -> Self::Output {
                                (self * a) + b
                            }
                        }
                    )*}
                }
                mul_add_impl!(MulAdd for isize i8 i16 i32 i64 i128);
                mul_add_impl!(MulAdd for usize u8 u16 u32 u64 u128);
                
                impl MulAddAssign<f32, f32> for f32
                {
                    #[inline] fn mul_add_assign(&mut self, a: Self, b: Self) {
                        *self = <Self as ::num::traits::Float>::mul_add(*self, a, b)
                    }
                }
                
                impl MulAddAssign<f64, f64> for f64
                {
                    #[inline] fn mul_add_assign(&mut self, a: Self, b: Self) {
                        *self = <Self as ::num::traits::Float>::mul_add(*self, a, b)
                    }
                }
                macro_rules! mul_add_assign_impl
                {
                    ($trait_name:ident for $($t:ty)*) => {$(
                        impl $trait_name for $t {
                            #[inline]
                            fn mul_add_assign(&mut self, a: Self, b: Self) {
                                *self = (*self * a) + b
                            }
                        }
                    )*}
                }
                mul_add_assign_impl!(MulAddAssign for isize i8 i16 i32 i64 i128);
                mul_add_assign_impl!(MulAddAssign for usize u8 u16 u32 u64 u128);
            }
            
            pub mod overflowing
            {
                /*!
                */
                use ::
                {
                    ops::{Add, Mul, Sub},
                    *,
                };
                /*
                */
                macro_rules! overflowing_impl {
                    ($trait_name:ident, $method:ident, $t:ty) => {
                        impl $trait_name for $t {
                            #[inline]
                            fn $method(&self, v: &Self) -> (Self, bool) {
                                <$t>::$method(*self, *v)
                            }
                        }
                    };
                }
                /// Performs addition with a flag for overflow.
                pub trait OverflowingAdd: Sized + Add<Self, Output = Self> {
                    /// Returns a tuple of the sum along with a boolean indicating whether an arithmetic overflow would occur.
                    /// If an overflow would have occurred then the wrapped value is returned.
                    fn overflowing_add(&self, v: &Self) -> (Self, bool);
                }
                overflowing_impl!(OverflowingAdd, overflowing_add, u8);
                overflowing_impl!(OverflowingAdd, overflowing_add, u16);
                overflowing_impl!(OverflowingAdd, overflowing_add, u32);
                overflowing_impl!(OverflowingAdd, overflowing_add, u64);
                overflowing_impl!(OverflowingAdd, overflowing_add, usize);
                overflowing_impl!(OverflowingAdd, overflowing_add, u128);

                overflowing_impl!(OverflowingAdd, overflowing_add, i8);
                overflowing_impl!(OverflowingAdd, overflowing_add, i16);
                overflowing_impl!(OverflowingAdd, overflowing_add, i32);
                overflowing_impl!(OverflowingAdd, overflowing_add, i64);
                overflowing_impl!(OverflowingAdd, overflowing_add, isize);
                overflowing_impl!(OverflowingAdd, overflowing_add, i128);
                /// Performs substraction with a flag for overflow.
                pub trait OverflowingSub: Sized + Sub<Self, Output = Self> {
                    /// Returns a tuple of the difference along with a boolean indicating whether an arithmetic overflow would occur.
                    /// If an overflow would have occurred then the wrapped value is returned.
                    fn overflowing_sub(&self, v: &Self) -> (Self, bool);
                }
                overflowing_impl!(OverflowingSub, overflowing_sub, u8);
                overflowing_impl!(OverflowingSub, overflowing_sub, u16);
                overflowing_impl!(OverflowingSub, overflowing_sub, u32);
                overflowing_impl!(OverflowingSub, overflowing_sub, u64);
                overflowing_impl!(OverflowingSub, overflowing_sub, usize);
                overflowing_impl!(OverflowingSub, overflowing_sub, u128);

                overflowing_impl!(OverflowingSub, overflowing_sub, i8);
                overflowing_impl!(OverflowingSub, overflowing_sub, i16);
                overflowing_impl!(OverflowingSub, overflowing_sub, i32);
                overflowing_impl!(OverflowingSub, overflowing_sub, i64);
                overflowing_impl!(OverflowingSub, overflowing_sub, isize);
                overflowing_impl!(OverflowingSub, overflowing_sub, i128);
                /// Performs multiplication with a flag for overflow.
                pub trait OverflowingMul: Sized + Mul<Self, Output = Self> {
                    /// Returns a tuple of the product along with a boolean indicating whether an arithmetic overflow would occur.
                    /// If an overflow would have occurred then the wrapped value is returned.
                    fn overflowing_mul(&self, v: &Self) -> (Self, bool);
                }
                overflowing_impl!(OverflowingMul, overflowing_mul, u8);
                overflowing_impl!(OverflowingMul, overflowing_mul, u16);
                overflowing_impl!(OverflowingMul, overflowing_mul, u32);
                overflowing_impl!(OverflowingMul, overflowing_mul, u64);
                overflowing_impl!(OverflowingMul, overflowing_mul, usize);
                overflowing_impl!(OverflowingMul, overflowing_mul, u128);

                overflowing_impl!(OverflowingMul, overflowing_mul, i8);
                overflowing_impl!(OverflowingMul, overflowing_mul, i16);
                overflowing_impl!(OverflowingMul, overflowing_mul, i32);
                overflowing_impl!(OverflowingMul, overflowing_mul, i64);
                overflowing_impl!(OverflowingMul, overflowing_mul, isize);
                overflowing_impl!(OverflowingMul, overflowing_mul, i128);
            }
            
            pub mod saturating
            {
                /*!
                */
                use ::
                {
                    ops::{Add, Mul, Sub},
                    *,
                };
                /*
                */
                /// Saturating math operations. Deprecated.
                pub trait Saturating
                {
                    /// Saturating addition operator.
                    /// Returns a+b, saturating at the numeric bounds instead of overflowing.
                    fn saturating_add(self, v: Self) -> Self;

                    /// Saturating subtraction operator.
                    /// Returns a-b, saturating at the numeric bounds instead of overflowing.
                    fn saturating_sub(self, v: Self) -> Self;
                }
                macro_rules! deprecated_saturating_impl
                {
                    ($trait_name:ident for $($t:ty)*) => {$(
                        impl $trait_name for $t {
                            #[inline]
                            fn saturating_add(self, v: Self) -> Self {
                                Self::saturating_add(self, v)
                            }
                            #[inline]
                            fn saturating_sub(self, v: Self) -> Self {
                                Self::saturating_sub(self, v)
                            }
                        }
                    )*}
                }
                deprecated_saturating_impl!(Saturating for isize i8 i16 i32 i64 i128);
                deprecated_saturating_impl!(Saturating for usize u8 u16 u32 u64 u128);

                macro_rules! saturating_impl
                {
                    ($trait_name:ident, $method:ident, $t:ty) => {
                        impl $trait_name for $t {
                            #[inline]
                            fn $method(&self, v: &Self) -> Self {
                                <$t>::$method(*self, *v)
                            }
                        }
                    };
                }
                /// Performs addition that saturates at the numeric bounds instead of overflowing.
                pub trait SaturatingAdd: Sized + Add<Self, Output = Self>
                {
                    /// Saturating addition. Computes `self + other`, saturating at the relevant high or low boundary of
                    /// the type.
                    fn saturating_add(&self, v: &Self) -> Self;
                }
                saturating_impl!(SaturatingAdd, saturating_add, u8);
                saturating_impl!(SaturatingAdd, saturating_add, u16);
                saturating_impl!(SaturatingAdd, saturating_add, u32);
                saturating_impl!(SaturatingAdd, saturating_add, u64);
                saturating_impl!(SaturatingAdd, saturating_add, usize);
                saturating_impl!(SaturatingAdd, saturating_add, u128);

                saturating_impl!(SaturatingAdd, saturating_add, i8);
                saturating_impl!(SaturatingAdd, saturating_add, i16);
                saturating_impl!(SaturatingAdd, saturating_add, i32);
                saturating_impl!(SaturatingAdd, saturating_add, i64);
                saturating_impl!(SaturatingAdd, saturating_add, isize);
                saturating_impl!(SaturatingAdd, saturating_add, i128);
                /// Performs subtraction that saturates at the numeric bounds instead of overflowing.
                pub trait SaturatingSub: Sized + Sub<Self, Output = Self> 
                {
                    /// Saturating subtraction. Computes `self - other`, saturating at the relevant high or low boundary of
                    /// the type.
                    fn saturating_sub(&self, v: &Self) -> Self;
                }
                saturating_impl!(SaturatingSub, saturating_sub, u8);
                saturating_impl!(SaturatingSub, saturating_sub, u16);
                saturating_impl!(SaturatingSub, saturating_sub, u32);
                saturating_impl!(SaturatingSub, saturating_sub, u64);
                saturating_impl!(SaturatingSub, saturating_sub, usize);
                saturating_impl!(SaturatingSub, saturating_sub, u128);

                saturating_impl!(SaturatingSub, saturating_sub, i8);
                saturating_impl!(SaturatingSub, saturating_sub, i16);
                saturating_impl!(SaturatingSub, saturating_sub, i32);
                saturating_impl!(SaturatingSub, saturating_sub, i64);
                saturating_impl!(SaturatingSub, saturating_sub, isize);
                saturating_impl!(SaturatingSub, saturating_sub, i128);
                /// Performs multiplication that saturates at the numeric bounds instead of overflowing.
                pub trait SaturatingMul: Sized + Mul<Self, Output = Self> 
                {
                    /// Saturating multiplication. Computes `self * other`, saturating at the relevant high or low boundary of
                    /// the type.
                    fn saturating_mul(&self, v: &Self) -> Self;
                }
                saturating_impl!(SaturatingMul, saturating_mul, u8);
                saturating_impl!(SaturatingMul, saturating_mul, u16);
                saturating_impl!(SaturatingMul, saturating_mul, u32);
                saturating_impl!(SaturatingMul, saturating_mul, u64);
                saturating_impl!(SaturatingMul, saturating_mul, usize);
                saturating_impl!(SaturatingMul, saturating_mul, u128);

                saturating_impl!(SaturatingMul, saturating_mul, i8);
                saturating_impl!(SaturatingMul, saturating_mul, i16);
                saturating_impl!(SaturatingMul, saturating_mul, i32);
                saturating_impl!(SaturatingMul, saturating_mul, i64);
                saturating_impl!(SaturatingMul, saturating_mul, isize);
                saturating_impl!(SaturatingMul, saturating_mul, i128);
            }
            
            pub mod wrapping
            {
                /*!
                */
                use ::
                {
                    num::{ Wrapping },
                    ops::{ Add, Mul, Neg, Shl, Shr, Sub },
                    *,
                };
                /*
                */
                macro_rules! wrapping_impl
                {
                    ($trait_name:ident, $method:ident, $t:ty) => {
                        impl $trait_name for $t {
                            #[inline]
                            fn $method(&self, v: &Self) -> Self {
                                <$t>::$method(*self, *v)
                            }
                        }
                    };
                    ($trait_name:ident, $method:ident, $t:ty, $rhs:ty) => {
                        impl $trait_name<$rhs> for $t {
                            #[inline]
                            fn $method(&self, v: &$rhs) -> Self {
                                <$t>::$method(*self, *v)
                            }
                        }
                    };
                }
                /// Performs addition that wraps around on overflow.
                pub trait WrappingAdd: Sized + Add<Self, Output = Self>
                {
                    /// Wrapping (modular) addition. Computes `self + other`, wrapping around at the boundary of
                    /// the type.
                    fn wrapping_add(&self, v: &Self) -> Self;
                }
                wrapping_impl!(WrappingAdd, wrapping_add, u8);
                wrapping_impl!(WrappingAdd, wrapping_add, u16);
                wrapping_impl!(WrappingAdd, wrapping_add, u32);
                wrapping_impl!(WrappingAdd, wrapping_add, u64);
                wrapping_impl!(WrappingAdd, wrapping_add, usize);
                wrapping_impl!(WrappingAdd, wrapping_add, u128);

                wrapping_impl!(WrappingAdd, wrapping_add, i8);
                wrapping_impl!(WrappingAdd, wrapping_add, i16);
                wrapping_impl!(WrappingAdd, wrapping_add, i32);
                wrapping_impl!(WrappingAdd, wrapping_add, i64);
                wrapping_impl!(WrappingAdd, wrapping_add, isize);
                wrapping_impl!(WrappingAdd, wrapping_add, i128);
                /// Performs subtraction that wraps around on overflow.
                pub trait WrappingSub: Sized + Sub<Self, Output = Self>
                {
                    /// Wrapping (modular) subtraction. Computes `self - other`, wrapping around at the boundary
                    /// of the type.
                    fn wrapping_sub(&self, v: &Self) -> Self;
                }
                wrapping_impl!(WrappingSub, wrapping_sub, u8);
                wrapping_impl!(WrappingSub, wrapping_sub, u16);
                wrapping_impl!(WrappingSub, wrapping_sub, u32);
                wrapping_impl!(WrappingSub, wrapping_sub, u64);
                wrapping_impl!(WrappingSub, wrapping_sub, usize);
                wrapping_impl!(WrappingSub, wrapping_sub, u128);

                wrapping_impl!(WrappingSub, wrapping_sub, i8);
                wrapping_impl!(WrappingSub, wrapping_sub, i16);
                wrapping_impl!(WrappingSub, wrapping_sub, i32);
                wrapping_impl!(WrappingSub, wrapping_sub, i64);
                wrapping_impl!(WrappingSub, wrapping_sub, isize);
                wrapping_impl!(WrappingSub, wrapping_sub, i128);
                /// Performs multiplication that wraps around on overflow.
                pub trait WrappingMul: Sized + Mul<Self, Output = Self>
                {
                    /// Wrapping (modular) multiplication. Computes `self * other`, wrapping around at the boundary
                    /// of the type.
                    fn wrapping_mul(&self, v: &Self) -> Self;
                }
                wrapping_impl!(WrappingMul, wrapping_mul, u8);
                wrapping_impl!(WrappingMul, wrapping_mul, u16);
                wrapping_impl!(WrappingMul, wrapping_mul, u32);
                wrapping_impl!(WrappingMul, wrapping_mul, u64);
                wrapping_impl!(WrappingMul, wrapping_mul, usize);
                wrapping_impl!(WrappingMul, wrapping_mul, u128);

                wrapping_impl!(WrappingMul, wrapping_mul, i8);
                wrapping_impl!(WrappingMul, wrapping_mul, i16);
                wrapping_impl!(WrappingMul, wrapping_mul, i32);
                wrapping_impl!(WrappingMul, wrapping_mul, i64);
                wrapping_impl!(WrappingMul, wrapping_mul, isize);
                wrapping_impl!(WrappingMul, wrapping_mul, i128);

                macro_rules! wrapping_unary_impl 
                {
                    ($trait_name:ident, $method:ident, $t:ty) => {
                        impl $trait_name for $t {
                            #[inline]
                            fn $method(&self) -> $t {
                                <$t>::$method(*self)
                            }
                        }
                    };
                }
                /// Performs a negation that does not panic.
                pub trait WrappingNeg: Sized 
                {
                    /// Wrapping (modular) negation. Computes `-self`,
                    /// wrapping around at the boundary of the type.
                    ///
                    /// Since unsigned types do not have negative equivalents
                    /// all applications of this function will wrap (except for `-0`).
                    /// For values smaller than the corresponding signed type's maximum
                    /// the result is the same as casting the corresponding signed value.
                    /// Any larger values are equivalent to `MAX + 1 - (val - MAX - 1)` where
                    /// `MAX` is the corresponding signed type's maximum.
                    ///
                    /// ```
                    /// use ::num::traits::WrappingNeg;
                    ///
                    /// assert_eq!(100i8.wrapping_neg(), -100);
                    /// assert_eq!((-100i8).wrapping_neg(), 100);
                    /// assert_eq!((-128i8).wrapping_neg(), -128);
                    /// ```
                    fn wrapping_neg(&self) -> Self;
                }
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, u8);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, u16);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, u32);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, u64);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, usize);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, u128);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, i8);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, i16);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, i32);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, i64);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, isize);
                wrapping_unary_impl!(WrappingNeg, wrapping_neg, i128);

                macro_rules! wrapping_shift_impl 
                {
                    ($trait_name:ident, $method:ident, $t:ty) => {
                        impl $trait_name for $t {
                            #[inline]
                            fn $method(&self, rhs: u32) -> $t {
                                <$t>::$method(*self, rhs)
                            }
                        }
                    };
                }
                /// Performs a left shift that does not panic.
                pub trait WrappingShl: Sized + Shl<usize, Output = Self> 
                {
                    /// Panic-free bitwise shift-left; yields `self << mask(rhs)`,
                    /// where `mask` removes any high order bits of `rhs` that would
                    /// cause the shift to exceed the bitwidth of the type.
                    ///
                    /// ```
                    /// use ::num::traits::WrappingShl;
                    ///
                    /// let x: u16 = 0x0001;
                    ///
                    /// assert_eq!(WrappingShl::wrapping_shl(&x, 0),  0x0001);
                    /// assert_eq!(WrappingShl::wrapping_shl(&x, 1),  0x0002);
                    /// assert_eq!(WrappingShl::wrapping_shl(&x, 15), 0x8000);
                    /// assert_eq!(WrappingShl::wrapping_shl(&x, 16), 0x0001);
                    /// ```
                    fn wrapping_shl(&self, rhs: u32) -> Self;
                }
                wrapping_shift_impl!(WrappingShl, wrapping_shl, u8);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, u16);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, u32);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, u64);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, usize);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, u128);

                wrapping_shift_impl!(WrappingShl, wrapping_shl, i8);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, i16);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, i32);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, i64);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, isize);
                wrapping_shift_impl!(WrappingShl, wrapping_shl, i128);
                /// Performs a right shift that does not panic.
                pub trait WrappingShr: Sized + Shr<usize, Output = Self> 
                {
                    /// Panic-free bitwise shift-right; yields `self >> mask(rhs)`,
                    /// where `mask` removes any high order bits of `rhs` that would
                    /// cause the shift to exceed the bitwidth of the type.
                    ///
                    /// ```
                    /// use ::num::traits::WrappingShr;
                    ///
                    /// let x: u16 = 0x8000;
                    ///
                    /// assert_eq!(WrappingShr::wrapping_shr(&x, 0),  0x8000);
                    /// assert_eq!(WrappingShr::wrapping_shr(&x, 1),  0x4000);
                    /// assert_eq!(WrappingShr::wrapping_shr(&x, 15), 0x0001);
                    /// assert_eq!(WrappingShr::wrapping_shr(&x, 16), 0x8000);
                    /// ```
                    fn wrapping_shr(&self, rhs: u32) -> Self;
                }
                wrapping_shift_impl!(WrappingShr, wrapping_shr, u8);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, u16);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, u32);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, u64);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, usize);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, u128);

                wrapping_shift_impl!(WrappingShr, wrapping_shr, i8);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, i16);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, i32);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, i64);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, isize);
                wrapping_shift_impl!(WrappingShr, wrapping_shr, i128);
               
                impl<T: WrappingAdd> WrappingAdd for Wrapping<T> where
                    Wrapping<T>: Add<Output = Wrapping<T>>,
                {
                    fn wrapping_add(&self, v: &Self) -> Self {
                        Wrapping(self.0.wrapping_add(&v.0))
                    }
                }
                
                impl<T: WrappingSub> WrappingSub for Wrapping<T> where
                    Wrapping<T>: Sub<Output = Wrapping<T>>,
                {
                    fn wrapping_sub(&self, v: &Self) -> Self {
                        Wrapping(self.0.wrapping_sub(&v.0))
                    }
                }
                impl<T: WrappingMul> WrappingMul for Wrapping<T> where
                    Wrapping<T>: Mul<Output = Wrapping<T>>,
                {
                    fn wrapping_mul(&self, v: &Self) -> Self {
                        Wrapping(self.0.wrapping_mul(&v.0))
                    }
                }
                impl<T: WrappingNeg> WrappingNeg for Wrapping<T> where
                    Wrapping<T>: Neg<Output = Wrapping<T>>,
                {
                    fn wrapping_neg(&self) -> Self {
                        Wrapping(self.0.wrapping_neg())
                    }
                }
                impl<T: WrappingShl> WrappingShl for Wrapping<T> where
                    Wrapping<T>: Shl<usize, Output = Wrapping<T>>,
                {
                    fn wrapping_shl(&self, rhs: u32) -> Self {
                        Wrapping(self.0.wrapping_shl(rhs))
                    }
                }
                impl<T: WrappingShr> WrappingShr for Wrapping<T> where
                    Wrapping<T>: Shr<usize, Output = Wrapping<T>>,
                {
                    fn wrapping_shr(&self, rhs: u32) -> Self {
                        Wrapping(self.0.wrapping_shr(rhs))
                    }
                }
            }
        } pub use self::ops::
        {
            bytes::{ FromBytes, ToBytes },
            checked::{ CheckedAdd, CheckedDiv, CheckedMul, CheckedNeg, CheckedRem, CheckedShl, CheckedShr, CheckedSub },
            euclid::{ CheckedEuclid, Euclid },
            inv::{ Inv },
            mul_add::{ MulAdd, MulAddAssign },
            saturating::{ Saturating, SaturatingAdd, SaturatingMul, SaturatingSub },
            wrapping::{ WrappingAdd, WrappingMul, WrappingNeg, WrappingShl, WrappingShr, WrappingSub },
        };

        pub mod pow
        {
            use ::
            {
                num::
                {
                    traits::{ CheckedMul, One, Float }, Wrapping
                },
                ops::{ Mul },
                *,
            };
            /*
            */
            /// Binary operator for raising a value to a power.
            pub trait Pow<RHS>
            {
                /// The result after applying the operator.
                type Output;
                /// Returns `self` to the power `rhs`.
                fn pow(self, rhs: RHS) -> Self::Output;
            }
            macro_rules! pow_impl {
                ($t:ty) =>
                {
                    pow_impl!($t, u8);
                    pow_impl!($t, usize);
                };
                ($t:ty, $rhs:ty) => {
                    pow_impl!($t, $rhs, usize, pow);
                };
                ($t:ty, $rhs:ty, $desired_rhs:ty, $method:expr) => {
                    impl Pow<$rhs> for $t {
                        type Output = $t;
                        #[inline]
                        fn pow(self, rhs: $rhs) -> $t {
                            ($method)(self, <$desired_rhs>::from(rhs))
                        }
                    }
                    impl<'a> Pow<&'a $rhs> for $t {
                        type Output = $t;
                        #[inline]
                        fn pow(self, rhs: &'a $rhs) -> $t {
                            ($method)(self, <$desired_rhs>::from(*rhs))
                        }
                    }
                    impl<'a> Pow<$rhs> for &'a $t {
                        type Output = $t;
                        #[inline]
                        fn pow(self, rhs: $rhs) -> $t {
                            ($method)(*self, <$desired_rhs>::from(rhs))
                        }
                    }
                    impl<'a, 'b> Pow<&'a $rhs> for &'b $t {
                        type Output = $t;
                        #[inline]
                        fn pow(self, rhs: &'a $rhs) -> $t {
                            ($method)(*self, <$desired_rhs>::from(*rhs))
                        }
                    }
                };
            }
            pow_impl!(u8, u8, u32, u8::pow);
            pow_impl!(u8, u16, u32, u8::pow);
            pow_impl!(u8, u32, u32, u8::pow);
            pow_impl!(u8, usize);
            pow_impl!(i8, u8, u32, i8::pow);
            pow_impl!(i8, u16, u32, i8::pow);
            pow_impl!(i8, u32, u32, i8::pow);
            pow_impl!(i8, usize);
            pow_impl!(u16, u8, u32, u16::pow);
            pow_impl!(u16, u16, u32, u16::pow);
            pow_impl!(u16, u32, u32, u16::pow);
            pow_impl!(u16, usize);
            pow_impl!(i16, u8, u32, i16::pow);
            pow_impl!(i16, u16, u32, i16::pow);
            pow_impl!(i16, u32, u32, i16::pow);
            pow_impl!(i16, usize);
            pow_impl!(u32, u8, u32, u32::pow);
            pow_impl!(u32, u16, u32, u32::pow);
            pow_impl!(u32, u32, u32, u32::pow);
            pow_impl!(u32, usize);
            pow_impl!(i32, u8, u32, i32::pow);
            pow_impl!(i32, u16, u32, i32::pow);
            pow_impl!(i32, u32, u32, i32::pow);
            pow_impl!(i32, usize);
            pow_impl!(u64, u8, u32, u64::pow);
            pow_impl!(u64, u16, u32, u64::pow);
            pow_impl!(u64, u32, u32, u64::pow);
            pow_impl!(u64, usize);
            pow_impl!(i64, u8, u32, i64::pow);
            pow_impl!(i64, u16, u32, i64::pow);
            pow_impl!(i64, u32, u32, i64::pow);
            pow_impl!(i64, usize);

            pow_impl!(u128, u8, u32, u128::pow);
            pow_impl!(u128, u16, u32, u128::pow);
            pow_impl!(u128, u32, u32, u128::pow);
            pow_impl!(u128, usize);

            pow_impl!(i128, u8, u32, i128::pow);
            pow_impl!(i128, u16, u32, i128::pow);
            pow_impl!(i128, u32, u32, i128::pow);
            pow_impl!(i128, usize);

            pow_impl!(usize, u8, u32, usize::pow);
            pow_impl!(usize, u16, u32, usize::pow);
            pow_impl!(usize, u32, u32, usize::pow);
            pow_impl!(usize, usize);
            pow_impl!(isize, u8, u32, isize::pow);
            pow_impl!(isize, u16, u32, isize::pow);
            pow_impl!(isize, u32, u32, isize::pow);
            pow_impl!(isize, usize);
            pow_impl!(Wrapping<u8>);
            pow_impl!(Wrapping<i8>);
            pow_impl!(Wrapping<u16>);
            pow_impl!(Wrapping<i16>);
            pow_impl!(Wrapping<u32>);
            pow_impl!(Wrapping<i32>);
            pow_impl!(Wrapping<u64>);
            pow_impl!(Wrapping<i64>);
            pow_impl!(Wrapping<u128>);
            pow_impl!(Wrapping<i128>);
            pow_impl!(Wrapping<usize>);
            pow_impl!(Wrapping<isize>);

            pow_impl!(f32, i8, i32, <f32 as Float>::powi);
            pow_impl!(f32, u8, i32, <f32 as Float>::powi);
            pow_impl!(f32, i16, i32, <f32 as Float>::powi);
            pow_impl!(f32, u16, i32, <f32 as Float>::powi);
            pow_impl!(f32, i32, i32, <f32 as Float>::powi);
            pow_impl!(f64, i8, i32, <f64 as Float>::powi);
            pow_impl!(f64, u8, i32, <f64 as Float>::powi);
            pow_impl!(f64, i16, i32, <f64 as Float>::powi);
            pow_impl!(f64, u16, i32, <f64 as Float>::powi);
            pow_impl!(f64, i32, i32, <f64 as Float>::powi);
            pow_impl!(f32, f32, f32, <f32 as Float>::powf);
            pow_impl!(f64, f32, f64, <f64 as Float>::powf);
            pow_impl!(f64, f64, f64, <f64 as Float>::powf);            
            /// Raises a value to the power of exp, using exponentiation by squaring.
            #[inline] pub fn pow<T: Clone + One + Mul<T, Output = T>>(mut base: T, mut exp: usize) -> T 
            {
                if exp == 0 {
                    return T::one();
                }
                while exp & 1 == 0 {
                    base = base.clone() * base;
                    exp >>= 1;
                }
                if exp == 1 {
                    return base;
                }
                let mut acc = base.clone();
                while exp > 1 {
                    exp >>= 1;
                    base = base.clone() * base;
                    if exp & 1 == 1 {
                        acc = acc * base.clone();
                    }
                }
                acc
            }
            /// Raises a value to the power of exp, returning `None` if an overflow occurred.
            #[inline] pub fn checked_pow<T: Clone + One + CheckedMul>(mut base: T, mut exp: usize) -> Option<T> 
            {
                if exp == 0 {
                    return Some(T::one());
                }
                while exp & 1 == 0 {
                    base = base.checked_mul(&base)?;
                    exp >>= 1;
                }
                if exp == 1 {
                    return Some(base);
                }
                let mut acc = base.clone();
                while exp > 1 {
                    exp >>= 1;
                    base = base.checked_mul(&base)?;
                    if exp & 1 == 1 {
                        acc = acc.checked_mul(&base)?;
                    }
                }
                Some(acc)
            }

        } pub use self::pow::{checked_pow, pow, Pow};

        pub mod real
        {
            use ::
            {
                num::traits::{Float, Num, NumCast},
                ops::{ Neg },
                *,
            };
            /*
            */
            /// A trait for real number types that do not necessarily have
            /// floating-point-specific characteristics such as NaN and infinity.
            pub trait Real: Num + Copy + NumCast + PartialOrd + Neg<Output = Self>
            {
                /// Returns the smallest finite value that this type can represent.
                fn min_value() -> Self;
                /// Returns the smallest positive, normalized value that this type can represent.
                fn min_positive_value() -> Self;
                /// Returns epsilon, a small positive value.
                fn epsilon() -> Self;
                /// Returns the largest finite value that this type can represent.
                fn max_value() -> Self;
                /// Returns the largest integer less than or equal to a number.
                fn floor(self) -> Self;
                /// Returns the smallest integer greater than or equal to a number.
                fn ceil(self) -> Self;
                /// Returns the nearest integer to a number.
                fn round(self) -> Self;
                /// Return the integer part of a number.
                fn trunc(self) -> Self;
                /// Returns the fractional part of a number.
                fn fract(self) -> Self;
                /// Computes the absolute value of `self`.
                fn abs(self) -> Self;
                /// Returns a number that represents the sign of `self`.
                fn signum(self) -> Self;
                /// Returns `true` if `self` is positive, including `+0.0`,
                /// `Float::infinity()`, and with newer versions of Rust `f64::NAN`.
                fn is_sign_positive(self) -> bool;
                /// Returns `true` if `self` is negative, including `-0.0`,
                /// `Float::neg_infinity()`, and with newer versions of Rust `-f64::NAN`.
                fn is_sign_negative(self) -> bool;
                /// Fused multiply-add.
                fn mul_add(self, a: Self, b: Self) -> Self;
                /// Take the reciprocal (inverse) of a number, `1/x`.
                fn recip(self) -> Self;
                /// Raise a number to an integer power.
                fn powi(self, n: i32) -> Self;
                /// Raise a number to a real number power.
                fn powf(self, n: Self) -> Self;
                /// Take the square root of a number.
                fn sqrt(self) -> Self;
                /// Returns `e^(self)`, (the exponential function).
                fn exp(self) -> Self;
                /// Returns `2^(self)`.
                fn exp2(self) -> Self;
                /// Returns the natural logarithm of the number.
                fn ln(self) -> Self;
                /// Returns the logarithm of the number with respect to an arbitrary base.
                fn log(self, base: Self) -> Self;
                /// Returns the base 2 logarithm of the number.
                fn log2(self) -> Self;
                /// Returns the base 10 logarithm of the number.
                fn log10(self) -> Self;
                /// Converts radians to degrees.
                fn to_degrees(self) -> Self;
                /// Converts degrees to radians.
                fn to_radians(self) -> Self;
                /// Returns the maximum of the two numbers.
                fn max(self, other: Self) -> Self;
                /// Returns the minimum of the two numbers.
                fn min(self, other: Self) -> Self;
                /// The positive difference of two numbers.
                fn abs_sub(self, other: Self) -> Self;
                /// Take the cubic root of a number.
                fn cbrt(self) -> Self;
                /// Calculate the length of the hypotenuse of a right-angle triangle given legs of length `x` and `y`.
                fn hypot(self, other: Self) -> Self;
                /// Computes the sine of a number (in radians).
                fn sin(self) -> Self;
                /// Computes the cosine of a number (in radians).
                fn cos(self) -> Self;
                /// Computes the tangent of a number (in radians).
                fn tan(self) -> Self;
                /// Computes the arcsine of a number.
                fn asin(self) -> Self;
                /// Computes the arccosine of a number.
                fn acos(self) -> Self;
                /// Computes the arctangent of a number.
                fn atan(self) -> Self;
                /// Computes the four quadrant arctangent of `self` (`y`) and `other` (`x`).
                fn atan2(self, other: Self) -> Self;
                /// Simultaneously computes the sine and cosine of the number, `x`.
                fn sin_cos(self) -> (Self, Self);
                /// Returns `e^(self) - 1` in a way that is accurate even if the number is close to zero.
                fn exp_m1(self) -> Self;
                /// Returns `ln(1+n)` more accurately than if the operations were performed separately.
                fn ln_1p(self) -> Self;
                /// Hyperbolic sine function.
                fn sinh(self) -> Self;
                /// Hyperbolic cosine function.
                fn cosh(self) -> Self;
                /// Hyperbolic tangent function.
                fn tanh(self) -> Self;
                /// Inverse hyperbolic sine function.
                fn asinh(self) -> Self;
                /// Inverse hyperbolic cosine function.
                fn acosh(self) -> Self;
                /// Inverse hyperbolic tangent function.
                fn atanh(self) -> Self;
            }
            
            impl<T: Float> Real for T
            {
                forward! {
                    Float::min_value() -> Self;
                    Float::min_positive_value() -> Self;
                    Float::epsilon() -> Self;
                    Float::max_value() -> Self;
                }
                forward! {
                    Float::floor(self) -> Self;
                    Float::ceil(self) -> Self;
                    Float::round(self) -> Self;
                    Float::trunc(self) -> Self;
                    Float::fract(self) -> Self;
                    Float::abs(self) -> Self;
                    Float::signum(self) -> Self;
                    Float::is_sign_positive(self) -> bool;
                    Float::is_sign_negative(self) -> bool;
                    Float::mul_add(self, a: Self, b: Self) -> Self;
                    Float::recip(self) -> Self;
                    Float::powi(self, n: i32) -> Self;
                    Float::powf(self, n: Self) -> Self;
                    Float::sqrt(self) -> Self;
                    Float::exp(self) -> Self;
                    Float::exp2(self) -> Self;
                    Float::ln(self) -> Self;
                    Float::log(self, base: Self) -> Self;
                    Float::log2(self) -> Self;
                    Float::log10(self) -> Self;
                    Float::to_degrees(self) -> Self;
                    Float::to_radians(self) -> Self;
                    Float::max(self, other: Self) -> Self;
                    Float::min(self, other: Self) -> Self;
                    Float::abs_sub(self, other: Self) -> Self;
                    Float::cbrt(self) -> Self;
                    Float::hypot(self, other: Self) -> Self;
                    Float::sin(self) -> Self;
                    Float::cos(self) -> Self;
                    Float::tan(self) -> Self;
                    Float::asin(self) -> Self;
                    Float::acos(self) -> Self;
                    Float::atan(self) -> Self;
                    Float::atan2(self, other: Self) -> Self;
                    Float::sin_cos(self) -> (Self, Self);
                    Float::exp_m1(self) -> Self;
                    Float::ln_1p(self) -> Self;
                    Float::sinh(self) -> Self;
                    Float::cosh(self) -> Self;
                    Float::tanh(self) -> Self;
                    Float::asinh(self) -> Self;
                    Float::acosh(self) -> Self;
                    Float::atanh(self) -> Self;
                }
            }
        }

        pub mod sign
        {
            use ::
            {
                num::
                {
                    traits::{ float::{ Float, FloatCore }, Num }, Wrapping
                },
                ops::{ Neg },
                *,
            };
            /*
            */
            /// Useful functions for signed numbers (i.e. numbers that can be negative).
            pub trait Signed: Sized + Num + Neg<Output = Self>
            {
                /// Computes the absolute value.
                fn abs(&self) -> Self;
                /// The positive difference of two numbers.
                fn abs_sub(&self, other: &Self) -> Self;
                /// Returns the sign of the number.
                /// * `-1` if the number is negative
                fn signum(&self) -> Self;
                /// Returns true if the number is positive and false if the number is zero or negative.
                fn is_positive(&self) -> bool;
                /// Returns true if the number is negative and false if the number is zero or positive.
                fn is_negative(&self) -> bool;
            }
            macro_rules! signed_impl {
                ($($t:ty)*) => ($(
                    impl Signed for $t {
                        #[inline]
                        fn abs(&self) -> $t {
                            if self.is_negative() { -*self } else { *self }
                        }
                        #[inline]
                        fn abs_sub(&self, other: &$t) -> $t {
                            if *self <= *other { 0 } else { *self - *other }
                        }
                        #[inline]
                        fn signum(&self) -> $t {
                            match *self {
                                n if n > 0 => 1,
                                0 => 0,
                                _ => -1,
                            }
                        }
                        #[inline]
                        fn is_positive(&self) -> bool { *self > 0 }
                        #[inline]
                        fn is_negative(&self) -> bool { *self < 0 }
                    }
                )*)
            }
            signed_impl!(isize i8 i16 i32 i64 i128);

            impl<T: Signed> Signed for Wrapping<T> where
                Wrapping<T>: Num + Neg<Output = Wrapping<T>>,
            {
                #[inline] fn abs(&self) -> Self {
                    Wrapping(self.0.abs())
                }
                #[inline] fn abs_sub(&self, other: &Self) -> Self {
                    Wrapping(self.0.abs_sub(&other.0))
                }
                #[inline] fn signum(&self) -> Self {
                    Wrapping(self.0.signum())
                }
                #[inline] fn is_positive(&self) -> bool {
                    self.0.is_positive()
                }
                #[inline] fn is_negative(&self) -> bool {
                    self.0.is_negative()
                }
            }
            macro_rules! signed_float_impl {
                ($t:ty) => {
                    impl Signed for $t {
                        /// Computes the absolute value. Returns `NAN` if the number is `NAN`.
                        #[inline]
                        fn abs(&self) -> $t {
                            Float::abs(*self)
                        }
                        /// The positive difference of two numbers. Returns `0.0` if the number is
                        /// less than or equal to `other`, otherwise the difference between`self`
                        /// and `other` is returned.
                        #[inline]
                        fn abs_sub(&self, other: &$t) -> $t {
                            if *self <= *other {
                                0.
                            } else {
                                *self - *other
                            }
                        }
                        /// # Returns
                        ///
                        /// - `1.0` if the number is positive, `+0.0` or `INFINITY`
                        /// - `-1.0` if the number is negative, `-0.0` or `NEG_INFINITY`
                        /// - `NAN` if the number is NaN
                        #[inline]
                        fn signum(&self) -> $t {
                            Float::signum(*self)
                        }
                        /// Returns `true` if the number is positive, including `+0.0` and `INFINITY`
                        #[inline]
                        fn is_positive(&self) -> bool {
                            Float::is_sign_positive(*self)
                        }
                        /// Returns `true` if the number is negative, including `-0.0` and `NEG_INFINITY`
                        #[inline]
                        fn is_negative(&self) -> bool {
                            Float::is_sign_negative(*self)
                        }
                    }
                };
            }
            signed_float_impl!(f32);
            signed_float_impl!(f64);
            /// Computes the absolute value.
            #[inline( always )] pub fn abs<T: Signed>(value: T) -> T {
                value.abs()
            }
            /// The positive difference of two numbers.
            #[inline( always )] pub fn abs_sub<T: Signed>(x: T, y: T) -> T {
                x.abs_sub(&y)
            }
            /// Returns the sign of the number.
            #[inline( always )] pub fn signum<T: Signed>(value: T) -> T {
                value.signum()
            }
            /// A trait for values which cannot be negative
            pub trait Unsigned: Num {}
            macro_rules! empty_trait_impl
            {
                ($name:ident for $($t:ty)*) => ($(
                    impl $name for $t {}
                )*)
            }
            empty_trait_impl!(Unsigned for usize u8 u16 u32 u64 u128);

            impl<T: Unsigned> Unsigned for Wrapping<T> where Wrapping<T>: Num {}
        } pub use self::sign::{abs, abs_sub, signum, Signed, Unsigned};
        /// The base trait for numeric types, covering `0` and `1` values,
        /// comparisons, basic numeric operations, and string conversion.
        pub trait Num: PartialEq + Zero + One + NumOps
        {
            type FromStrRadixErr;

            /// Convert from a string and radix (typically `2..=36`).
            /// use ::num::traits::Num;
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
            /// It's possible that a type might not even support the common radix 10, nor any, if string
            /// parsing doesn't make sense for that type.
            fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr>;
        }
        /// Generic trait for types implementing basic numeric operations.
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
        /// The trait for `Num` types which also implement numeric operations taking the second operand by reference.
        pub trait NumRef: Num + for<'r> NumOps<&'r Self> {}
        impl<T> NumRef for T where T: Num + for<'r> NumOps<&'r T> {}
        /// Trait for `Num` references which implement numeric operations, taking the second operand either by value or by reference.
        pub trait RefNum<Base>: NumOps<Base, Base> + for<'r> NumOps<&'r Base, Base> {}
        impl<T, Base> RefNum<Base> for T where T: NumOps<Base, Base> + for<'r> NumOps<&'r Base, Base> {}
        /// Generic trait for types implementing numeric assignment operators (like `+=`).
        pub trait NumAssignOps<Rhs = Self>:
        AddAssign<Rhs> + SubAssign<Rhs> + MulAssign<Rhs> + DivAssign<Rhs> + RemAssign<Rhs>
        {
        }

        impl<T, Rhs> NumAssignOps<Rhs> for T where
            T: AddAssign<Rhs> + SubAssign<Rhs> + MulAssign<Rhs> + DivAssign<Rhs> + RemAssign<Rhs>
        {
        }
        /// The trait for `Num` types which also implement assignment operators.
        pub trait NumAssign: Num + NumAssignOps {}
        impl<T> NumAssign for T where T: Num + NumAssignOps {}
        /// Trait for `NumAssign` types which also implement assignment operations taking the second operand by reference.
        pub trait NumAssignRef: NumAssign + for<'r> NumAssignOps<&'r Self> {}
        impl<T> NumAssignRef for T where T: NumAssign + for<'r> NumAssignOps<&'r T> {}

        macro_rules! int_trait_impl
        {
            ($name:ident for $($t:ty)*) => ($(
                impl $name for $t {
                    type FromStrRadixErr = ::num::ParseIntError;
                    #[inline] fn from_str_radix(s: &str, radix: u32)
                                    -> Result<Self, ::num::ParseIntError>
                    {
                        <$t>::from_str_radix(s, radix)
                    }
                }
            )*)
        }

        int_trait_impl!(Num for usize u8 u16 u32 u64 u128);
        int_trait_impl!(Num for isize i8 i16 i32 i64 i128);

        impl<T: Num> Num for Wrapping<T> where
            Wrapping<T>: NumOps,
        {
            type FromStrRadixErr = T::FromStrRadixErr;
            fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
                T::from_str_radix(str, radix).map(Wrapping)
            }
        }
        
        #[derive(Debug)]
        pub enum FloatErrorKind
        {
            Empty,
            Invalid,
        }
        
        #[derive(Debug)]
        pub struct ParseFloatError
        {
            pub kind: FloatErrorKind,
        }

        impl fmt::Display for ParseFloatError
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {let description = match self.kind {
                    FloatErrorKind::Empty => "cannot parse float from empty string",
                    FloatErrorKind::Invalid => "invalid float literal",
                };

                description.fmt(f)
            }
        }

        fn str_to_ascii_lower_eq_str(a: &str, b: &str) -> bool
        {
            a.len() == b.len()
                && a.bytes().zip(b.bytes()).all(|(a, b)| {
                    let a_to_ascii_lower = a | (((b'A' <= a && a <= b'Z') as u8) << 5);
                    a_to_ascii_lower == b
                })
        }
        
        macro_rules! float_trait_impl
        {
            ($name:ident for $($t:ident)*) => ($(
                impl $name for $t {
                    type FromStrRadixErr = ParseFloatError;

                    fn from_str_radix(src: &str, radix: u32)
                                    -> Result<Self, Self::FromStrRadixErr>
                    {
                        use self::FloatErrorKind::*;
                        use self::ParseFloatError as PFE;

                       
                        if radix == 10 {
                            return src.parse().map_err(|_| PFE {
                                kind: if src.is_empty() { Empty } else { Invalid },
                            });
                        }
                       
                        if str_to_ascii_lower_eq_str(src, "inf")
                            || str_to_ascii_lower_eq_str(src, "infinity")
                        {
                            return Ok(::$t::INFINITY);
                        } else if str_to_ascii_lower_eq_str(src, "-inf")
                            || str_to_ascii_lower_eq_str(src, "-infinity")
                        {
                            return Ok(::$t::NEG_INFINITY);
                        } else if str_to_ascii_lower_eq_str(src, "nan") {
                            return Ok(::$t::NAN);
                        } else if str_to_ascii_lower_eq_str(src, "-nan") {
                            return Ok(-::$t::NAN);
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

                       
                        let mut sig = if is_positive { 0.0 } else { -0.0 };
                       
                        let mut prev_sig = sig;
                        let mut cs = src.chars().enumerate();
                       
                        let mut exp_info = None::<(char, usize)>;

                       
                        for (i, c) in cs.by_ref() {
                            match c.to_digit(radix) {
                                Some(digit) => {
                                   
                                    sig *= radix as $t;

                                   
                                    if is_positive {
                                        sig += (digit as isize) as $t;
                                    } else {
                                        sig -= (digit as isize) as $t;
                                    }
                                   
                                   
                                    if prev_sig != 0.0 {
                                        if is_positive && sig <= prev_sig
                                            { return Ok(::$t::INFINITY); }
                                        if !is_positive && sig >= prev_sig
                                            { return Ok(::$t::NEG_INFINITY); }
                                       
                                        if is_positive && (prev_sig != (sig - digit as $t) / radix as $t)
                                            { return Ok(::$t::INFINITY); }
                                        if !is_positive && (prev_sig != (sig + digit as $t) / radix as $t)
                                            { return Ok(::$t::NEG_INFINITY); }
                                    }
                                    prev_sig = sig;
                                },
                                None => match c {
                                    'e' | 'E' | 'p' | 'P' => {
                                        exp_info = Some((c, i + 1));
                                        break; 
                                    },
                                    '.' => {
                                        break; 
                                    },
                                    _ => {
                                        return Err(PFE { kind: Invalid });
                                    },
                                },
                            }
                        }
                       
                       
                        if exp_info.is_none() {
                            let mut power = 1.0;
                            for (i, c) in cs.by_ref() {
                                match c.to_digit(radix) {
                                    Some(digit) => {
                                       
                                        power /= radix as $t;
                                       
                                        sig = if is_positive {
                                            sig + (digit as $t) * power
                                        } else {
                                            sig - (digit as $t) * power
                                        };
                                       
                                        if is_positive && sig < prev_sig
                                            { return Ok(::$t::INFINITY); }
                                        if !is_positive && sig > prev_sig
                                            { return Ok(::$t::NEG_INFINITY); }
                                        prev_sig = sig;
                                    },
                                    None => match c {
                                        'e' | 'E' | 'p' | 'P' => {
                                            exp_info = Some((c, i + 1));
                                            break;
                                        },
                                        _ => {
                                            return Err(PFE { kind: Invalid });
                                        },
                                    },
                                }
                            }
                        }
                       
                        let exp = match exp_info {
                            Some((c, offset)) => {
                                let base = match c {
                                    'E' | 'e' if radix == 10 => 10.0,
                                    'P' | 'p' if radix == 16 => 2.0,
                                    _ => return Err(PFE { kind: Invalid }),
                                };

                               
                                let src = &src[offset..];
                                let (is_positive, exp) = match slice_shift_char(src) {
                                    Some(('-', src)) => (false, src.parse::<usize>()),
                                    Some(('+', src)) => (true,  src.parse::<usize>()),
                                    Some((_, _))     => (true,  src.parse::<usize>()),
                                    None             => return Err(PFE { kind: Invalid }),
                                };

                                                    fn pow(base: $t, exp: usize) -> $t {
                                    Float::powi(base, exp as i32)
                                }
                               

                                match (is_positive, exp) {
                                    (true,  Ok(exp)) => pow(base, exp),
                                    (false, Ok(exp)) => 1.0 / pow(base, exp),
                                    (_, Err(_))      => return Err(PFE { kind: Invalid }),
                                }
                            },
                            None => 1.0,
                        };

                        Ok(sig * exp)
                    }
                }
            )*)
        }
        
        float_trait_impl!(Num for f32 f64);
        /// A value bounded by a minimum and a maximum
        #[inline] pub fn clamp<T: PartialOrd>(input: T, min: T, max: T) -> T
        {
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
        #[inline] pub fn clamp_min<T: PartialOrd>(input: T, min: T) -> T
        {
            debug_assert!(min == min, "min must not be NAN");
            if input < min {
                min
            } else {
                input
            }
        }
        /// A value bounded by a maximum value
        #[inline] pub fn clamp_max<T: PartialOrd>(input: T, max: T) -> T
        {
            debug_assert!(max == max, "max must not be NAN");
            if input > max {
                max
            } else {
                input
            }
        }
    }
    /*
    */
    pub mod integers
    {
        //! Integer trait and functions.
        use ::
        {
            num::{ traits::{ Num, Signed, Zero } },
            ops::{ Add },
            *,
        };
        /*
        */
        pub mod average
        {
            /*!
            */
            use ::
            {
                num::integers::{ Integer },
                ops::{BitAnd, BitOr, BitXor, Shr},
                *,
            };
            /*
            */
            /// Provides methods to compute the average of two integers, without overflows.
            pub trait Average: Integer {
                /// Returns the ceiling value of the average of `self` and `other`. assert_eq!(u8::max_value().average_ceil(&2), 129);
                /// assert_eq!(i8::min_value().average_ceil(&-1), -64);
                /// assert_eq!(i8::min_value().average_ceil(&i8::max_value()), 0);
                /// ```
                ///
                fn average_ceil(&self, other: &Self) -> Self;
                /// Returns the floor value of the average of `self` and `other`. assert_eq!(u8::max_value().average_floor(&2), 128);
                /// assert_eq!(i8::min_value().average_floor(&-1), -65);
                /// assert_eq!(i8::min_value().average_floor(&i8::max_value()), -1);
                /// ```
                ///
                fn average_floor(&self, other: &Self) -> Self;
            }
            
            impl<I> Average for I
            where
                I: Integer + Shr<usize, Output = I>,
                for<'a, 'b> &'a I:
                    BitAnd<&'b I, Output = I> + BitOr<&'b I, Output = I> + BitXor<&'b I, Output = I>,
            {
               
               

                /// Returns the floor value of the average of `self` and `other`.
                #[inline] fn average_floor(&self, other: &I) -> I {
                    (self & other) + ((self ^ other) >> 1)
                }
                /// Returns the ceil value of the average of `self` and `other`.
                #[inline] fn average_ceil(&self, other: &I) -> I {
                    (self | other) - ((self ^ other) >> 1)
                }
            }
            /// Returns the floor value of the average of `x` and `y` --
            /// see [Average::average_floor](trait.Average.html#tymethod.average_floor).
            #[inline] pub fn average_floor<T: Average>(x: T, y: T) -> T {
                x.average_floor(&y)
            }
            /// Returns the ceiling value of the average of `x` and `y` --
            /// see [Average::average_ceil](trait.Average.html#tymethod.average_ceil).
            #[inline] pub fn average_ceil<T: Average>(x: T, y: T) -> T {
                x.average_ceil(&y)
            }
        } pub use self::average::{average_ceil, average_floor, Average};

        pub mod roots
        {
            /*!
            */
            use ::
            {
                num::
                {
                    traits::{checked_pow, PrimInt},
                    integers::{ Integer },
                },
                *,
            };
            /*
            */
            /// Provides methods to compute an integer's square root, cube root,
            /// and arbitrary `n`th root.
            pub trait Roots: Integer {
                /// Returns the truncated principal `n`th root of an integer
                /// -- `if x >= 0 { ⌊ⁿ√x⌋ } else { ⌈ⁿ√x⌉ }`
                ///
                /// This is solving for `r` in `rⁿ = x`, rounding toward zero.
                ///
                /// ```
                /// use ::num::integers::Roots;
                ///
                /// let x: i32 = 12345;
                /// assert_eq!(x.nth_root(1), x);
                /// assert_eq!(x.nth_root(2), x.sqrt());
                /// assert_eq!(x.nth_root(3), x.cbrt());
                /// assert_eq!(x.nth_root(4), 10);
                /// assert_eq!(x.nth_root(13), 2);
                /// assert_eq!(x.nth_root(14), 1);
                /// assert_eq!(x.nth_root(::u32::MAX), 1);
                ///
                /// assert_eq!(::i32::MAX.nth_root(30), 2);
                /// assert_eq!(::i32::MAX.nth_root(31), 1);
                /// assert_eq!(::i32::MIN.nth_root(31), -2);
                /// assert_eq!((::i32::MIN + 1).nth_root(31), -1);
                ///
                /// assert_eq!(::u32::MAX.nth_root(31), 2);
                /// assert_eq!(::u32::MAX.nth_root(32), 1);
                /// ```
                fn nth_root(&self, n: u32) -> Self;
                /// Returns the truncated principal square root of an integer -- `⌊√x⌋`
                ///
                /// This is solving for `r` in `r² = x`, rounding toward zero.
                #[inline] fn sqrt(&self) -> Self {
                    self.nth_root(2)
                }
                /// Returns the truncated principal cube root of an integer --
                /// `if x >= 0 { ⌊∛x⌋ } else { ⌈∛x⌉ }`
                ///
                /// This is solving for `r` in `r³ = x`, rounding toward zero.
                #[inline] fn cbrt(&self) -> Self {
                    self.nth_root(3)
                }
            }
            /// Returns the truncated principal square root of an integer --
            /// see [Roots::sqrt](trait.Roots.html#method.sqrt).
            #[inline] pub fn sqrt<T: Roots>(x: T) -> T {
                x.sqrt()
            }
            /// Returns the truncated principal cube root of an integer --
            /// see [Roots::cbrt](trait.Roots.html#method.cbrt).
            #[inline] pub fn cbrt<T: Roots>(x: T) -> T {
                x.cbrt()
            }
            /// Returns the truncated principal `n`th root of an integer --
            /// see [Roots::nth_root](trait.Roots.html#tymethod.nth_root).
            #[inline] pub fn nth_root<T: Roots>(x: T, n: u32) -> T {
                x.nth_root(n)
            }
            macro_rules! signed_roots {
                ($T:ty, $U:ty) => {
                    impl Roots for $T {
                        #[inline]
                        fn nth_root(&self, n: u32) -> Self {
                            if *self >= 0 {
                                (*self as $U).nth_root(n) as Self
                            } else {
                                assert!(n.is_odd(), "even roots of a negative are imaginary");
                                -((self.wrapping_neg() as $U).nth_root(n) as Self)
                            }
                        }
                        #[inline]
                        fn sqrt(&self) -> Self {
                            assert!(*self >= 0, "the square root of a negative is imaginary");
                            (*self as $U).sqrt() as Self
                        }
                        #[inline]
                        fn cbrt(&self) -> Self {
                            if *self >= 0 {
                                (*self as $U).cbrt() as Self
                            } else {
                                -((self.wrapping_neg() as $U).cbrt() as Self)
                            }
                        }
                    }
                };
            }
            signed_roots!(i8, u8);
            signed_roots!(i16, u16);
            signed_roots!(i32, u32);
            signed_roots!(i64, u64);
            signed_roots!(i128, u128);
            signed_roots!(isize, usize);

            #[inline] fn fixpoint<T, F>(mut x: T, f: F) -> T
            where
                T: Integer + Copy,
                F: Fn(T) -> T,
            {
                let mut xn = f(x);
                while x < xn {
                    x = xn;
                    xn = f(x);
                }
                while x > xn {
                    x = xn;
                    xn = f(x);
                }
                x
            }
            #[inline] fn bits<T>() -> u32 {
                8 * mem::size_of::<T>() as u32
            }
            #[inline] fn log2<T: PrimInt>(x: T) -> u32
            {
                debug_assert!(x > T::zero());
                bits::<T>() - 1 - x.leading_zeros()
            }
            macro_rules! unsigned_roots {
                ($T:ident) => {
                    impl Roots for $T {
                        #[inline]
                        fn nth_root(&self, n: u32) -> Self {
                            fn go(a: $T, n: u32) -> $T {
                               
                                match n {
                                    0 => panic!("can't find a root of degree 0!"),
                                    1 => return a,
                                    2 => return a.sqrt(),
                                    3 => return a.cbrt(),
                                    _ => (),
                                }
                               
                                if bits::<$T>() <= n || a < (1 << n) {
                                    return (a > 0) as $T;
                                }
                                if bits::<$T>() > 64 {
                                   
                                    return if a <= ::u64::MAX as $T {
                                        (a as u64).nth_root(n) as $T
                                    } else {
                                        let lo = (a >> n).nth_root(n) << 1;
                                        let hi = lo + 1;
                                       
                                       
                                        if hi.next_power_of_two().trailing_zeros() * n >= bits::<$T>() {
                                            match checked_pow(hi, n as usize) {
                                                Some(x) if x <= a => hi,
                                                _ => lo,
                                            }
                                        } else {
                                            if hi.pow(n) <= a {
                                                hi
                                            } else {
                                                lo
                                            }
                                        }
                                    };
                                }
                                
                                #[inline] fn guess(x: $T, n: u32) -> $T {
                                   
                                    if bits::<$T>() <= 32 || x <= ::u32::MAX as $T {
                                        1 << ((log2(x) + n - 1) / n)
                                    } else {
                                        ((x as f64).ln() / f64::from(n)).exp() as $T
                                    }
                                }
                                
                                let n1 = n - 1;
                                let next = |x: $T| {
                                    let y = match checked_pow(x, n1 as usize) {
                                        Some(ax) => a / ax,
                                        None => 0,
                                    };
                                    (y + x * n1 as $T) / n as $T
                                };
                                fixpoint(guess(a, n), next)
                            }
                            go(*self, n)
                        }
                        #[inline]
                        fn sqrt(&self) -> Self {
                            fn go(a: $T) -> $T {
                                if bits::<$T>() > 64 {
                                   
                                    return if a <= ::u64::MAX as $T {
                                        (a as u64).sqrt() as $T
                                    } else {
                                        let lo = (a >> 2u32).sqrt() << 1;
                                        let hi = lo + 1;
                                        if hi * hi <= a {
                                            hi
                                        } else {
                                            lo
                                        }
                                    };
                                }
                                if a < 4 {
                                    return (a > 0) as $T;
                                }
                                                    #[inline]
                                fn guess(x: $T) -> $T {
                                    (x as f64).sqrt() as $T
                                }
                                
                                let next = |x: $T| (a / x + x) >> 1;
                                fixpoint(guess(a), next)
                            }
                            go(*self)
                        }
                        #[inline]
                        fn cbrt(&self) -> Self {
                            fn go(a: $T) -> $T {
                                if bits::<$T>() > 64 {
                                   
                                    return if a <= ::u64::MAX as $T {
                                        (a as u64).cbrt() as $T
                                    } else {
                                        let lo = (a >> 3u32).cbrt() << 1;
                                        let hi = lo + 1;
                                        if hi * hi * hi <= a {
                                            hi
                                        } else {
                                            lo
                                        }
                                    };
                                }
                                if bits::<$T>() <= 32 {
                                   
                                    let mut x = a;
                                    let mut y2 = 0;
                                    let mut y = 0;
                                    let smax = bits::<$T>() / 3;
                                    for s in (0..smax + 1).rev() {
                                        let s = s * 3;
                                        y2 *= 4;
                                        y *= 2;
                                        let b = 3 * (y2 + y) + 1;
                                        if x >> s >= b {
                                            x -= b << s;
                                            y2 += 2 * y + 1;
                                            y += 1;
                                        }
                                    }
                                    return y;
                                }
                                if a < 8 {
                                    return (a > 0) as $T;
                                }
                                if a <= ::u32::MAX as $T {
                                    return (a as u32).cbrt() as $T;
                                }
                                                    #[inline]
                                fn guess(x: $T) -> $T {
                                    (x as f64).cbrt() as $T
                                }
                                
                                let next = |x: $T| (a / (x * x) + x * 2) / 3;
                                fixpoint(guess(a), next)
                            }
                            go(*self)
                        }
                    }
                };
            }
            unsigned_roots!(u8);
            unsigned_roots!(u16);
            unsigned_roots!(u32);
            unsigned_roots!(u64);
            unsigned_roots!(u128);
            unsigned_roots!(usize);
        } pub use self::roots::{cbrt, nth_root, sqrt, Roots};

        pub trait Integer: Sized + Num + PartialOrd + Ord + Eq 
        {
            /// Floored integer division.
            /// assert!((-8).div_floor(&-3) ==  2);
            ///
            /// assert!(( 1).div_floor(& 2) ==  0);
            /// assert!(( 1).div_floor(&-2) == -1);
            /// assert!((-1).div_floor(& 2) == -1);
            /// assert!((-1).div_floor(&-2) ==  0);
            /// ~~~
            fn div_floor(&self, other: &Self) -> Self;

            /// Floored integer modulo, satisfying:
            ///
            /// ~~~
            /// # use ::num::integers::Integer;
            /// # let n = 1; let d = 1;
            /// assert!(n.div_floor(&d) * d + n.mod_floor(&d) == n)
            /// ~~~
            ///
            /// # Examples
            ///
            /// ~~~
            /// # use ::num::integers::Integer;
            /// assert!(( 8).mod_floor(& 3) ==  2);
            /// assert!(( 8).mod_floor(&-3) == -1);
            /// assert!((-8).mod_floor(& 3) ==  1);
            /// assert!((-8).mod_floor(&-3) == -2);
            ///
            /// assert!(( 1).mod_floor(& 2) ==  1);
            /// assert!(( 1).mod_floor(&-2) == -1);
            /// assert!((-1).mod_floor(& 2) ==  1);
            /// assert!((-1).mod_floor(&-2) == -1);
            /// ~~~
            fn mod_floor(&self, other: &Self) -> Self;

            /// Ceiled integer division.
            /// assert_eq!((-8).div_ceil(&-3),  3);
            ///
            /// assert_eq!(( 1).div_ceil( &2), 1);
            /// assert_eq!(( 1).div_ceil(&-2), 0);
            /// assert_eq!((-1).div_ceil( &2), 0);
            /// assert_eq!((-1).div_ceil(&-2), 1);
            /// ~~~
            fn div_ceil(&self, other: &Self) -> Self {
                let (q, r) = self.div_mod_floor(other);
                if r.is_zero() {
                    q
                } else {
                    q + Self::one()
                }
            }
            /// Greatest Common Divisor (GCD).
            fn gcd(&self, other: &Self) -> Self;

            /// Lowest Common Multiple (LCM).
            /// ~~~
            fn lcm(&self, other: &Self) -> Self;

            /// Greatest Common Divisor (GCD) and
            /// Lowest Common Multiple (LCM) together.
            #[inline] fn gcd_lcm(&self, other: &Self) -> (Self, Self) {
                (self.gcd(other), self.lcm(other))
            }
            /// Greatest common divisor and Bézout coefficients.
            ///     let ExtendedGcd { gcd, x, y, .. } = a.extended_gcd(&b);
            ///     gcd == x * a + y * b
            /// }
            /// assert!(check(10isize, 4isize));
            /// assert!(check(8isize,  9isize));
            /// # }
            /// ~~~
            #[inline] fn extended_gcd(&self, other: &Self) -> ExtendedGcd<Self> where
                Self: Clone,
            {
                let mut s = (Self::zero(), Self::one());
                let mut t = (Self::one(), Self::zero());
                let mut r = (other.clone(), self.clone());

                while !r.0.is_zero() {
                    let q = r.1.clone() / r.0.clone();
                    let f = |mut r: (Self, Self)| {
                        mem::swap(&mut r.0, &mut r.1);
                        r.0 = r.0 - q.clone() * r.1.clone();
                        r
                    };
                    r = f(r);
                    s = f(s);
                    t = f(t);
                }
                if r.1 >= Self::zero() {
                    ExtendedGcd {
                        gcd: r.1,
                        x: s.1,
                        y: t.1,
                    }
                } else {
                    ExtendedGcd {
                        gcd: Self::zero() - r.1,
                        x: Self::zero() - s.1,
                        y: Self::zero() - t.1,
                    }
                }
            }
            /// Greatest common divisor, least common multiple, and Bézout coefficients.
            #[inline] fn extended_gcd_lcm(&self, other: &Self) -> (ExtendedGcd<Self>, Self)
            where
                Self: Clone + Signed,
            {
                (self.extended_gcd(other), self.lcm(other))
            }
            /// Deprecated, use `is_multiple_of` instead.
            #[deprecated(note = "Please use is_multiple_of instead")]
            #[inline] fn divides(&self, other: &Self) -> bool {
                self.is_multiple_of(other)
            }
            /// Returns `true` if `self` is a multiple of `other`.
            fn is_multiple_of(&self, other: &Self) -> bool;

            /// Returns `true` if the number is even.
            fn is_even(&self) -> bool;

            /// Returns `true` if the number is odd.
            fn is_odd(&self) -> bool;

            /// Simultaneous truncated integer division and modulus.
            /// assert_eq!((-8).div_rem( &3), (-2, -2));
            /// assert_eq!((-8).div_rem(&-3), ( 2, -2));
            ///
            /// assert_eq!(( 1).div_rem( &2), ( 0,  1));
            /// assert_eq!(( 1).div_rem(&-2), ( 0,  1));
            /// assert_eq!((-1).div_rem( &2), ( 0, -1));
            /// assert_eq!((-1).div_rem(&-2), ( 0, -1));
            /// ~~~
            fn div_rem(&self, other: &Self) -> (Self, Self);
            /// Simultaneous floored integer division and modulus.
            /// assert_eq!((-8).div_mod_floor( &3), (-3,  1));
            /// assert_eq!((-8).div_mod_floor(&-3), ( 2, -2));
            ///
            /// assert_eq!(( 1).div_mod_floor( &2), ( 0,  1));
            /// assert_eq!(( 1).div_mod_floor(&-2), (-1, -1));
            /// assert_eq!((-1).div_mod_floor( &2), (-1,  1));
            /// assert_eq!((-1).div_mod_floor(&-2), ( 0, -1));
            /// ~~~
            fn div_mod_floor(&self, other: &Self) -> (Self, Self) {
                (self.div_floor(other), self.mod_floor(other))
            }
            /// Rounds up to nearest multiple of argument.
            ///
            /// ~~~
            /// # use ::num::integers::Integer;
            /// assert_eq!(( 16).next_multiple_of(& 8),  16);
            /// assert_eq!(( 23).next_multiple_of(& 8),  24);
            /// assert_eq!(( 16).next_multiple_of(&-8),  16);
            /// assert_eq!(( 23).next_multiple_of(&-8),  16);
            /// assert_eq!((-16).next_multiple_of(& 8), -16);
            /// assert_eq!((-23).next_multiple_of(& 8), -16);
            /// assert_eq!((-16).next_multiple_of(&-8), -16);
            /// assert_eq!((-23).next_multiple_of(&-8), -24);
            /// ~~~
            #[inline] fn next_multiple_of(&self, other: &Self) -> Self
            where
                Self: Clone,
            {
                let m = self.mod_floor(other);
                self.clone()
                    + if m.is_zero() {
                        Self::zero()
                    } else {
                        other.clone() - m
                    }
            }
            /// Rounds down to nearest multiple of argument.
            ///
            /// ~~~
            /// # use ::num::integers::Integer;
            /// assert_eq!(( 16).prev_multiple_of(& 8),  16);
            /// assert_eq!(( 23).prev_multiple_of(& 8),  16);
            /// assert_eq!(( 16).prev_multiple_of(&-8),  16);
            /// assert_eq!(( 23).prev_multiple_of(&-8),  24);
            /// assert_eq!((-16).prev_multiple_of(& 8), -16);
            /// assert_eq!((-23).prev_multiple_of(& 8), -24);
            /// assert_eq!((-16).prev_multiple_of(&-8), -16);
            /// assert_eq!((-23).prev_multiple_of(&-8), -16);
            /// ~~~
            #[inline] fn prev_multiple_of(&self, other: &Self) -> Self
            where
                Self: Clone,
            {
                self.clone() - self.mod_floor(other)
            }
            /// Decrements self by one.
            /// ~~~
            fn dec(&mut self)
            where
                Self: Clone,
            {
                *self = self.clone() - Self::one()
            }
            /// Increments self by one.
            /// ~~~
            fn inc(&mut self)
            where
                Self: Clone,
            {
                *self = self.clone() + Self::one()
            }
        }
        /// Greatest common divisor and Bézout coefficients.
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct ExtendedGcd<A> 
        {
            pub gcd: A,
            pub x: A,
            pub y: A,
        }
        /// Simultaneous integer division and modulus
        #[inline] pub fn div_rem<T: Integer>(x: T, y: T) -> (T, T) 
        {
            x.div_rem(&y)
        }
        /// Floored integer division
        #[inline] pub fn div_floor<T: Integer>(x: T, y: T) -> T 
        {
            x.div_floor(&y)
        }
        /// Floored integer modulus
        #[inline] pub fn mod_floor<T: Integer>(x: T, y: T) -> T 
        {
            x.mod_floor(&y)
        }
        /// Simultaneous floored integer division and modulus
        #[inline] pub fn div_mod_floor<T: Integer>(x: T, y: T) -> (T, T) 
        {
            x.div_mod_floor(&y)
        }
        /// Ceiled integer division
        #[inline] pub fn div_ceil<T: Integer>(x: T, y: T) -> T 
        {
            x.div_ceil(&y)
        }
        /// Calculates the Greatest Common Divisor (GCD) of the number and `other`.
        #[inline(always)] pub fn gcd<T: Integer>(x: T, y: T) -> T 
        {
            x.gcd(&y)
        }
        /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
        #[inline(always)] pub fn lcm<T: Integer>(x: T, y: T) -> T 
        {
            x.lcm(&y)
        }
        /// Calculates the Greatest Common Divisor (GCD) and Lowest Common Multiple (LCM) of the number and `other`.
        #[inline(always)] pub fn gcd_lcm<T: Integer>(x: T, y: T) -> (T, T) 
        {
            x.gcd_lcm(&y)
        }

        macro_rules! impl_integer_for_isize 
        {
            ($T:ty, $test_mod:ident) => {
                impl Integer for $T {
                    /// Floored integer division
                    #[inline] fn div_floor(&self, other: &Self) -> Self {
                       
                       
                        let (d, r) = self.div_rem(other);
                        if (r > 0 && *other < 0) || (r < 0 && *other > 0) {
                            d - 1
                        } else {
                            d
                        }
                    }
                    /// Floored integer modulo
                    #[inline] fn mod_floor(&self, other: &Self) -> Self {
                       
                       
                        let r = *self % *other;
                        if (r > 0 && *other < 0) || (r < 0 && *other > 0) {
                            r + *other
                        } else {
                            r
                        }
                    }
                    /// Calculates `div_floor` and `mod_floor` simultaneously
                    #[inline] fn div_mod_floor(&self, other: &Self) -> (Self, Self) {
                       
                       
                        let (d, r) = self.div_rem(other);
                        if (r > 0 && *other < 0) || (r < 0 && *other > 0) {
                            (d - 1, r + *other)
                        } else {
                            (d, r)
                        }
                    }
                    #[inline] fn div_ceil(&self, other: &Self) -> Self {
                        let (d, r) = self.div_rem(other);
                        if (r > 0 && *other > 0) || (r < 0 && *other < 0) {
                            d + 1
                        } else {
                            d
                        }
                    }
                    /// Calculates the Greatest Common Divisor (GCD) of the number and
                    /// `other`. The result is always non-negative.
                    #[inline] fn gcd(&self, other: &Self) -> Self {
                       
                        let mut m = *self;
                        let mut n = *other;
                        if m == 0 || n == 0 {
                            return (m | n).abs();
                        }
                       
                        let shift = (m | n).trailing_zeros();

                       
                       
                       
                       

                       
                       
                       
                        if m == Self::min_value() || n == Self::min_value() {
                            return (1 << shift).abs();
                        }
                       
                        m = m.abs();
                        n = n.abs();

                       
                        m >>= m.trailing_zeros();
                        n >>= n.trailing_zeros();

                        while m != n {
                            if m > n {
                                m -= n;
                                m >>= m.trailing_zeros();
                            } else {
                                n -= m;
                                n >>= n.trailing_zeros();
                            }
                        }
                        m << shift
                    }
                    #[inline] fn extended_gcd_lcm(&self, other: &Self) -> (ExtendedGcd<Self>, Self) {
                        let egcd = self.extended_gcd(other);
                       
                        let lcm = if egcd.gcd.is_zero() {
                            Self::zero()
                        } else {
                            (*self * (*other / egcd.gcd)).abs()
                        };
                        (egcd, lcm)
                    }
                    /// Calculates the Lowest Common Multiple (LCM) of the number and
                    /// `other`.
                    #[inline] fn lcm(&self, other: &Self) -> Self {
                        self.gcd_lcm(other).1
                    }
                    /// Calculates the Greatest Common Divisor (GCD) and
                    /// Lowest Common Multiple (LCM) of the number and `other`.
                    #[inline] fn gcd_lcm(&self, other: &Self) -> (Self, Self) {
                        if self.is_zero() && other.is_zero() {
                            return (Self::zero(), Self::zero());
                        }
                        let gcd = self.gcd(other);
                       
                        let lcm = (*self * (*other / gcd)).abs();
                        (gcd, lcm)
                    }
                    /// Returns `true` if the number is a multiple of `other`.
                    #[inline] fn is_multiple_of(&self, other: &Self) -> bool {
                        if other.is_zero() {
                            return self.is_zero();
                        }
                        *self % *other == 0
                    }
                    /// Returns `true` if the number is divisible by `2`
                    #[inline] fn is_even(&self) -> bool {
                        (*self) & 1 == 0
                    }
                    /// Returns `true` if the number is not divisible by `2`
                    #[inline] fn is_odd(&self) -> bool {
                        !self.is_even()
                    }
                    /// Simultaneous truncated integer division and modulus.
                    #[inline] fn div_rem(&self, other: &Self) -> (Self, Self) {
                        (*self / *other, *self % *other)
                    }
                    /// Rounds up to nearest multiple of argument.
                    #[inline] fn next_multiple_of(&self, other: &Self) -> Self {
                       
                        if *other == -1 {
                            return *self;
                        }
                        let m = Integer::mod_floor(self, other);
                        *self + if m == 0 { 0 } else { other - m }
                    }
                    /// Rounds down to nearest multiple of argument.
                    #[inline] fn prev_multiple_of(&self, other: &Self) -> Self {
                       
                        if *other == -1 {
                            return *self;
                        }
                        *self - Integer::mod_floor(self, other)
                    }
                }
            };
        }

        impl_integer_for_isize!(i8, test_integer_i8);
        impl_integer_for_isize!(i16, test_integer_i16);
        impl_integer_for_isize!(i32, test_integer_i32);
        impl_integer_for_isize!(i64, test_integer_i64);
        impl_integer_for_isize!(i128, test_integer_i128);
        impl_integer_for_isize!(isize, test_integer_isize);

        macro_rules! impl_integer_for_usize 
        {
            ($T:ty, $test_mod:ident) => {
                impl Integer for $T {
                    /// Unsigned integer division. Returns the same result as `div` (`/`).
                    #[inline] fn div_floor(&self, other: &Self) -> Self {
                        *self / *other
                    }
                    /// Unsigned integer modulo operation. Returns the same result as `rem` (`%`).
                    #[inline] fn mod_floor(&self, other: &Self) -> Self {
                        *self % *other
                    }
                    #[inline] fn div_ceil(&self, other: &Self) -> Self {
                        *self / *other + (0 != *self % *other) as Self
                    }
                    /// Calculates the Greatest Common Divisor (GCD) of the number and `other`
                    #[inline] fn gcd(&self, other: &Self) -> Self {
                       
                        let mut m = *self;
                        let mut n = *other;
                        if m == 0 || n == 0 {
                            return m | n;
                        }
                       
                        let shift = (m | n).trailing_zeros();

                       
                        m >>= m.trailing_zeros();
                        n >>= n.trailing_zeros();

                        while m != n {
                            if m > n {
                                m -= n;
                                m >>= m.trailing_zeros();
                            } else {
                                n -= m;
                                n >>= n.trailing_zeros();
                            }
                        }
                        m << shift
                    }
                    #[inline] fn extended_gcd_lcm(&self, other: &Self) -> (ExtendedGcd<Self>, Self) {
                        let egcd = self.extended_gcd(other);
                       
                        let lcm = if egcd.gcd.is_zero() {
                            Self::zero()
                        } else {
                            *self * (*other / egcd.gcd)
                        };
                        (egcd, lcm)
                    }
                    /// Calculates the Lowest Common Multiple (LCM) of the number and `other`.
                    #[inline] fn lcm(&self, other: &Self) -> Self {
                        self.gcd_lcm(other).1
                    }
                    /// Calculates the Greatest Common Divisor (GCD) and
                    /// Lowest Common Multiple (LCM) of the number and `other`.
                    #[inline] fn gcd_lcm(&self, other: &Self) -> (Self, Self) {
                        if self.is_zero() && other.is_zero() {
                            return (Self::zero(), Self::zero());
                        }
                        let gcd = self.gcd(other);
                        let lcm = *self * (*other / gcd);
                        (gcd, lcm)
                    }
                    /// Returns `true` if the number is a multiple of `other`.
                    #[inline] fn is_multiple_of(&self, other: &Self) -> bool {
                        if other.is_zero() {
                            return self.is_zero();
                        }
                        *self % *other == 0
                    }
                    /// Returns `true` if the number is divisible by `2`.
                    #[inline] fn is_even(&self) -> bool {
                        *self % 2 == 0
                    }
                    /// Returns `true` if the number is not divisible by `2`.
                    #[inline] fn is_odd(&self) -> bool {
                        !self.is_even()
                    }
                    /// Simultaneous truncated integer division and modulus.
                    #[inline] fn div_rem(&self, other: &Self) -> (Self, Self) {
                        (*self / *other, *self % *other)
                    }
                }
            };
        }

        impl_integer_for_usize!(u8, test_integer_u8);
        impl_integer_for_usize!(u16, test_integer_u16);
        impl_integer_for_usize!(u32, test_integer_u32);
        impl_integer_for_usize!(u64, test_integer_u64);
        impl_integer_for_usize!(u128, test_integer_u128);
        impl_integer_for_usize!(usize, test_integer_usize);
        /// An iterator over binomial coefficients.
        pub struct IterBinomial<T> 
        {
            a: T,
            n: T,
            k: T,
        }

        impl<T> IterBinomial<T> where
            T: Integer,
        {
            /// For a given n, iterate over all binomial coefficients binomial(n, k), for k=0...n.
            /// be no overflow:
            ///
            /// type | n
            /// -----|---
            /// u8   | 10
            /// i8   |  9
            /// u16  | 18
            /// i16  | 17
            /// u32  | 34
            /// i32  | 33
            /// u64  | 67
            /// i64  | 66
            ///
            /// For larger n, `T` should be a bigint type.
            pub fn new(n: T) -> IterBinomial<T> {
                IterBinomial {
                    k: T::zero(),
                    a: T::one(),
                    n,
                }
            }
        }

        impl<T> Iterator for IterBinomial<T> where
        T: Integer + Clone,
        {
            type Item = T;

            fn next(&mut self) -> Option<T> {
                if self.k > self.n {
                    return None;
                }
                self.a = if !self.k.is_zero() {
                    multiply_and_divide(
                        self.a.clone(),
                        self.n.clone() - self.k.clone() + T::one(),
                        self.k.clone(),
                    )
                } else {
                    T::one()
                };
                self.k = self.k.clone() + T::one();
                Some(self.a.clone())
            }
        }
        /// Calculate r * a / b, avoiding overflows and fractions.
        fn multiply_and_divide<T: Integer + Clone>(r: T, a: T, b: T) -> T
        {
           
            let g = gcd(r.clone(), b.clone());
            r / g.clone() * (a / (b / g))
        }
        /// Calculate the binomial coefficient.
        pub fn binomial<T: Integer + Clone>(mut n: T, k: T) -> T
        {
           
            if k > n {
                return T::zero();
            }
            if k > n.clone() - k.clone() {
                return binomial(n.clone(), n - k);
            }
            let mut r = T::one();
            let mut d = T::one();
            loop {
                if d > k {
                    break;
                }
                r = multiply_and_divide(r, n.clone(), d.clone());
                n = n - T::one();
                d = d + T::one();
            }
            r
        }
        /// Calculate the multinomial coefficient.
        pub fn multinomial<T: Integer + Clone>(k: &[T]) -> T where
            for<'a> T: Add<&'a T, Output = T>,
        {
            let mut r = T::one();
            let mut p = T::zero();
            for i in k {
                p = p + i;
                r = r * binomial(p.clone(), i.clone());
            }
            r
        }
    }
    /*
    */
    pub mod big
    {
        //! Big Integer Types for Rust
        use ::
        {
            *,
        };
        /*
        */
        pub mod int
        {
            use ::
            {
                cmp::{ Ordering::{self, Equal} },
                default::{ Default },
                num::
                {
                    traits::{ ConstZero, Num, One, Pow, Signed, Zero },
                    integers::{Integer, Roots},
                    big::
                    {
                        digit::BigDigit,
                        uint::{BigUint, IntDigits, to_str_radix_reversed, U32Digits, U64Digits},
                    },
                },
                ops::{ Neg, Not },
                string::{ String },
                vec::{ Vec },
                *,
            };
            /*
            */
            use self::Sign::{Minus, NoSign, Plus};

            pub mod addition
            {
                use ::
                {
                    cmp::Ordering::{ Equal, Greater, Less },
                    iter::{ Sum },
                    num::
                    {
                        big::{ IsizePromotion, UsizePromotion },
                        traits::{ CheckedAdd },
                    },
                    ops::{ Add, AddAssign },
                    *,
                };
                use super::CheckedUnsignedAbs::{ Negative, Positive };
                use super::Sign::{ Minus, NoSign, Plus };
                use super::{ BigInt, UnsignedAbs };
                /*
                */
                macro_rules! bigint_add 
                {
                    ($a:expr, $a_owned:expr, $a_data:expr, $b:expr, $b_owned:expr, $b_data:expr) => {
                        match ($a.sign, $b.sign) {
                            (_, NoSign) => $a_owned,
                            (NoSign, _) => $b_owned,
                           
                            (Plus, Plus) | (Minus, Minus) => BigInt::from_biguint($a.sign, $a_data + $b_data),
                           
                            (Plus, Minus) | (Minus, Plus) => match $a.data.cmp(&$b.data) {
                                Less => BigInt::from_biguint($b.sign, $b_data - $a_data),
                                Greater => BigInt::from_biguint($a.sign, $a_data - $b_data),
                                Equal => BigInt::ZERO,
                            },
                        }
                    };
                }
                impl Add<&BigInt> for &BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: &BigInt) -> BigInt {
                        bigint_add!(
                            self,
                            self.clone(),
                            &self.data,
                            other,
                            other.clone(),
                            &other.data
                        )
                    }
                }
                impl Add<BigInt> for &BigInt 
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: BigInt) -> BigInt {
                        bigint_add!(self, self.clone(), &self.data, other, other, other.data)
                    }
                }
                impl Add<&BigInt> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: &BigInt) -> BigInt {
                        bigint_add!(self, self, self.data, other, other.clone(), &other.data)
                    }
                }
                impl Add<BigInt> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: BigInt) -> BigInt {
                        bigint_add!(self, self, self.data, other, other, other.data)
                    }
                }
                impl AddAssign<&BigInt> for BigInt 
                {
                    #[inline] fn add_assign(&mut self, other: &BigInt) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n + other;
                    }
                }
                forward_val_assign!(impl AddAssign for BigInt, add_assign);

                promote_all_scalars!(impl Add for BigInt, add);
                promote_all_scalars_assign!(impl AddAssign for BigInt, add_assign);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u32> for BigInt, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u64> for BigInt, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u128> for BigInt, add);

                impl Add<u32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: u32) -> BigInt {
                        match self.sign {
                            NoSign => From::from(other),
                            Plus => BigInt::from(self.data + other),
                            Minus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Less => BigInt::from(other - self.data),
                                Greater => -BigInt::from(self.data - other),
                            },
                        }
                    }
                }
                impl AddAssign<u32> for BigInt
                {
                    #[inline] fn add_assign(&mut self, other: u32) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n + other;
                    }
                }
                impl Add<u64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: u64) -> BigInt {
                        match self.sign {
                            NoSign => From::from(other),
                            Plus => BigInt::from(self.data + other),
                            Minus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Less => BigInt::from(other - self.data),
                                Greater => -BigInt::from(self.data - other),
                            },
                        }
                    }
                }
                impl AddAssign<u64> for BigInt
                {
                    #[inline] fn add_assign(&mut self, other: u64) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n + other;
                    }
                }
                impl Add<u128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: u128) -> BigInt {
                        match self.sign {
                            NoSign => BigInt::from(other),
                            Plus => BigInt::from(self.data + other),
                            Minus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Less => BigInt::from(other - self.data),
                                Greater => -BigInt::from(self.data - other),
                            },
                        }
                    }
                }
                impl AddAssign<u128> for BigInt
                {
                    #[inline] fn add_assign(&mut self, other: u128) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n + other;
                    }
                }
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<i32> for BigInt, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<i64> for BigInt, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<i128> for BigInt, add);

                impl Add<i32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: i32) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self + u,
                            Negative(u) => self - u,
                        }
                    }
                }
                impl AddAssign<i32> for BigInt
                {
                    #[inline] fn add_assign(&mut self, other: i32) {
                        match other.checked_uabs() {
                            Positive(u) => *self += u,
                            Negative(u) => *self -= u,
                        }
                    }
                }
                impl Add<i64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: i64) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self + u,
                            Negative(u) => self - u,
                        }
                    }
                }
                impl AddAssign<i64> for BigInt
                {
                    #[inline] fn add_assign(&mut self, other: i64) {
                        match other.checked_uabs() {
                            Positive(u) => *self += u,
                            Negative(u) => *self -= u,
                        }
                    }
                }
                impl Add<i128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn add(self, other: i128) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self + u,
                            Negative(u) => self - u,
                        }
                    }
                }
                impl AddAssign<i128> for BigInt
                {
                    #[inline] fn add_assign(&mut self, other: i128) {
                        match other.checked_uabs() {
                            Positive(u) => *self += u,
                            Negative(u) => *self -= u,
                        }
                    }
                }
                impl CheckedAdd for BigInt
                {
                    #[inline] fn checked_add(&self, v: &BigInt) -> Option<BigInt> {
                        Some(self.add(v))
                    }
                }
                impl_sum_iter_type!(BigInt);
            }
            pub mod division
            {
                use ::
                {
                    num::
                    {
                        big::{IsizePromotion, UsizePromotion},
                        integers::{ Integer },
                        traits::{ CheckedDiv, CheckedEuclid, Euclid, Signed, ToPrimitive, Zero },
                    },
                    ops::{ Div, DivAssign, Rem, RemAssign },
                    *,
                };
                use super::CheckedUnsignedAbs::{Negative, Positive};
                use super::Sign::NoSign;
                use super::{BigInt, UnsignedAbs};
                /*
                */
                forward_all_binop_to_ref_ref!(impl Div for BigInt, div);

                impl Div<&BigInt> for &BigInt
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: &BigInt) -> BigInt {
                        let (q, _) = self.div_rem(other);
                        q
                    }
                }
                impl DivAssign<&BigInt> for BigInt
                {
                    #[inline] fn div_assign(&mut self, other: &BigInt) {
                        *self = &*self / other;
                    }
                }
                forward_val_assign!(impl DivAssign for BigInt, div_assign);

                promote_all_scalars!(impl Div for BigInt, div);
                promote_all_scalars_assign!(impl DivAssign for BigInt, div_assign);
                forward_all_scalar_binop_to_val_val!(impl Div<u32> for BigInt, div);
                forward_all_scalar_binop_to_val_val!(impl Div<u64> for BigInt, div);
                forward_all_scalar_binop_to_val_val!(impl Div<u128> for BigInt, div);

                impl Div<u32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: u32) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data / other)
                    }
                }
                impl DivAssign<u32> for BigInt
                {
                    #[inline] fn div_assign(&mut self, other: u32) {
                        self.data /= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                impl Div<BigInt> for u32 {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        BigInt::from_biguint(other.sign, self / other.data)
                    }
                }
                impl Div<u64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: u64) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data / other)
                    }
                }
                impl DivAssign<u64> for BigInt
                {
                    #[inline] fn div_assign(&mut self, other: u64) {
                        self.data /= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                impl Div<BigInt> for u64 {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        BigInt::from_biguint(other.sign, self / other.data)
                    }
                }
                impl Div<u128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: u128) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data / other)
                    }
                }
                impl DivAssign<u128> for BigInt
                {
                    #[inline] fn div_assign(&mut self, other: u128) {
                        self.data /= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                impl Div<BigInt> for u128 {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        BigInt::from_biguint(other.sign, self / other.data)
                    }
                }
                forward_all_scalar_binop_to_val_val!(impl Div<i32> for BigInt, div);
                forward_all_scalar_binop_to_val_val!(impl Div<i64> for BigInt, div);
                forward_all_scalar_binop_to_val_val!(impl Div<i128> for BigInt, div);

                impl Div<i32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: i32) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self / u,
                            Negative(u) => -self / u,
                        }
                    }
                }
                impl DivAssign<i32> for BigInt
                {
                    #[inline] fn div_assign(&mut self, other: i32) {
                        match other.checked_uabs() {
                            Positive(u) => *self /= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                *self /= u;
                            }
                        }
                    }
                }
                impl Div<BigInt> for i32 {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u / other,
                            Negative(u) => u / -other,
                        }
                    }
                }
                impl Div<i64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: i64) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self / u,
                            Negative(u) => -self / u,
                        }
                    }
                }
                impl DivAssign<i64> for BigInt
                {
                    #[inline] fn div_assign(&mut self, other: i64) {
                        match other.checked_uabs() {
                            Positive(u) => *self /= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                *self /= u;
                            }
                        }
                    }
                }
                impl Div<BigInt> for i64 {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u / other,
                            Negative(u) => u / -other,
                        }
                    }
                }
                impl Div<i128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn div(self, other: i128) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self / u,
                            Negative(u) => -self / u,
                        }
                    }
                }
                impl DivAssign<i128> for BigInt
                {
                    #[inline] fn div_assign(&mut self, other: i128) {
                        match other.checked_uabs() {
                            Positive(u) => *self /= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                *self /= u;
                            }
                        }
                    }
                }
                impl Div<BigInt> for i128 {
                    type Output = BigInt;

                    #[inline] fn div(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u / other,
                            Negative(u) => u / -other,
                        }
                    }
                }
                forward_all_binop_to_ref_ref!(impl Rem for BigInt, rem);

                impl Rem<&BigInt> for &BigInt
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: &BigInt) -> BigInt {
                        if let Some(other) = other.to_u32() {
                            self % other
                        } else if let Some(other) = other.to_i32() {
                            self % other
                        } else {
                            let (_, r) = self.div_rem(other);
                            r
                        }
                    }
                }
                impl RemAssign<&BigInt> for BigInt
                {
                    #[inline] fn rem_assign(&mut self, other: &BigInt) {
                        *self = &*self % other;
                    }
                }
                forward_val_assign!(impl RemAssign for BigInt, rem_assign);

                promote_all_scalars!(impl Rem for BigInt, rem);
                promote_all_scalars_assign!(impl RemAssign for BigInt, rem_assign);
                forward_all_scalar_binop_to_val_val!(impl Rem<u32> for BigInt, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<u64> for BigInt, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<u128> for BigInt, rem);

                impl Rem<u32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: u32) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data % other)
                    }
                }
                impl RemAssign<u32> for BigInt
                {
                    #[inline] fn rem_assign(&mut self, other: u32) {
                        self.data %= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                impl Rem<BigInt> for u32 {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        BigInt::from(self % other.data)
                    }
                }
                impl Rem<u64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: u64) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data % other)
                    }
                }
                impl RemAssign<u64> for BigInt
                {
                    #[inline] fn rem_assign(&mut self, other: u64) {
                        self.data %= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                impl Rem<BigInt> for u64 {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        BigInt::from(self % other.data)
                    }
                }
                impl Rem<u128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: u128) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data % other)
                    }
                }
                impl RemAssign<u128> for BigInt
                {
                    #[inline] fn rem_assign(&mut self, other: u128) {
                        self.data %= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                impl Rem<BigInt> for u128 {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        BigInt::from(self % other.data)
                    }
                }
                forward_all_scalar_binop_to_val_val!(impl Rem<i32> for BigInt, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<i64> for BigInt, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<i128> for BigInt, rem);

                impl Rem<i32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: i32) -> BigInt {
                        self % other.unsigned_abs()
                    }
                }
                impl RemAssign<i32> for BigInt
                {
                    #[inline] fn rem_assign(&mut self, other: i32) {
                        *self %= other.unsigned_abs();
                    }
                }
                impl Rem<BigInt> for i32 {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u % other,
                            Negative(u) => -(u % other),
                        }
                    }
                }
                impl Rem<i64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: i64) -> BigInt {
                        self % other.unsigned_abs()
                    }
                }
                impl RemAssign<i64> for BigInt
                {
                    #[inline] fn rem_assign(&mut self, other: i64) {
                        *self %= other.unsigned_abs();
                    }
                }
                impl Rem<BigInt> for i64 {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u % other,
                            Negative(u) => -(u % other),
                        }
                    }
                }
                impl Rem<i128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: i128) -> BigInt {
                        self % other.unsigned_abs()
                    }
                }
                impl RemAssign<i128> for BigInt
                {
                    #[inline] fn rem_assign(&mut self, other: i128) {
                        *self %= other.unsigned_abs();
                    }
                }
                impl Rem<BigInt> for i128 {
                    type Output = BigInt;

                    #[inline] fn rem(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u % other,
                            Negative(u) => -(u % other),
                        }
                    }
                }
                impl CheckedDiv for BigInt
                {
                    #[inline] fn checked_div(&self, v: &BigInt) -> Option<BigInt> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.div(v))
                    }
                }
                impl CheckedEuclid for BigInt
                {
                    #[inline] fn checked_div_euclid(&self, v: &BigInt) -> Option<BigInt> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.div_euclid(v))
                    }
                    #[inline] fn checked_rem_euclid(&self, v: &BigInt) -> Option<BigInt> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.rem_euclid(v))
                    }
                    fn checked_div_rem_euclid(&self, v: &Self) -> Option<(Self, Self)> {
                        Some(self.div_rem_euclid(v))
                    }
                }
                impl Euclid for BigInt
                {
                    #[inline] fn div_euclid(&self, v: &BigInt) -> BigInt {
                        let (q, r) = self.div_rem(v);
                        if r.is_negative() {
                            if v.is_positive() {
                                q - 1
                            } else {
                                q + 1
                            }
                        } else {
                            q
                        }
                    }
                    #[inline] fn rem_euclid(&self, v: &BigInt) -> BigInt {
                        let r = self % v;
                        if r.is_negative() {
                            if v.is_positive() {
                                r + v
                            } else {
                                r - v
                            }
                        } else {
                            r
                        }
                    }
                    fn div_rem_euclid(&self, v: &Self) -> (Self, Self) {
                        let (q, r) = self.div_rem(v);
                        if r.is_negative() {
                            if v.is_positive() {
                                (q - 1, r + v)
                            } else {
                                (q + 1, r - v)
                            }
                        } else {
                            (q, r)
                        }
                    }
                }
            }
            
            pub mod multiplication
            {
                use ::
                {
                    iter::{ Product },
                    num::
                    {
                        big::{IsizePromotion, UsizePromotion},
                        traits::{CheckedMul, One, Zero},
                    },
                    ops::{ Mul, MulAssign },
                    *,
                };
                use super::CheckedUnsignedAbs::{Negative, Positive};
                use super::Sign::{self, Minus, NoSign, Plus};
                use super::{BigInt, UnsignedAbs};
                /*
                */
                impl Mul<Sign> for Sign {
                    type Output = Sign;

                    #[inline] fn mul(self, other: Sign) -> Sign {
                        match (self, other) {
                            (NoSign, _) | (_, NoSign) => NoSign,
                            (Plus, Plus) | (Minus, Minus) => Plus,
                            (Plus, Minus) | (Minus, Plus) => Minus,
                        }
                    }
                }
                macro_rules! impl_mul {
                    ($(impl Mul<$Other:ty> for $Self:ty;)*) => {$(
                        impl Mul<$Other> for $Self {
                            type Output = BigInt;

                            #[inline]
                            fn mul(self, other: $Other) -> BigInt {
                               
                                let BigInt { data: x, .. } = self;
                                let BigInt { data: y, .. } = other;
                                BigInt::from_biguint(self.sign * other.sign, x * y)
                            }
                        }
                    )*}
                }
                impl_mul! {
                    impl Mul<BigInt> for BigInt;
                    impl Mul<BigInt> for &BigInt;
                    impl Mul<&BigInt> for BigInt;
                    impl Mul<&BigInt> for &BigInt;
                }
                macro_rules! impl_mul_assign {
                    ($(impl MulAssign<$Other:ty> for BigInt;)*) => {$(
                        impl MulAssign<$Other> for BigInt {
                            #[inline]
                            fn mul_assign(&mut self, other: $Other) {
                               
                                let BigInt { data: y, .. } = other;
                                self.data *= y;
                                if self.data.is_zero() {
                                    self.sign = NoSign;
                                } else {
                                    self.sign = self.sign * other.sign;
                                }
                            }
                        }
                    )*}
                }
                impl_mul_assign! {
                    impl MulAssign<BigInt> for BigInt;
                    impl MulAssign<&BigInt> for BigInt;
                }
                promote_all_scalars!(impl Mul for BigInt, mul);
                promote_all_scalars_assign!(impl MulAssign for BigInt, mul_assign);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u32> for BigInt, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u64> for BigInt, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u128> for BigInt, mul);

                impl Mul<u32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: u32) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data * other)
                    }
                }
                impl MulAssign<u32> for BigInt
                {
                    #[inline] fn mul_assign(&mut self, other: u32) {
                        self.data *= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                impl Mul<u64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: u64) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data * other)
                    }
                }
                impl MulAssign<u64> for BigInt
                {
                    #[inline] fn mul_assign(&mut self, other: u64) {
                        self.data *= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                impl Mul<u128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: u128) -> BigInt {
                        BigInt::from_biguint(self.sign, self.data * other)
                    }
                }
                impl MulAssign<u128> for BigInt
                {
                    #[inline] fn mul_assign(&mut self, other: u128) {
                        self.data *= other;
                        if self.data.is_zero() {
                            self.sign = NoSign;
                        }
                    }
                }
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<i32> for BigInt, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<i64> for BigInt, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<i128> for BigInt, mul);

                impl Mul<i32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: i32) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self * u,
                            Negative(u) => -self * u,
                        }
                    }
                }
                impl MulAssign<i32> for BigInt
                {
                    #[inline] fn mul_assign(&mut self, other: i32) {
                        match other.checked_uabs() {
                            Positive(u) => *self *= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                self.data *= u;
                            }
                        }
                    }
                }
                impl Mul<i64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: i64) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self * u,
                            Negative(u) => -self * u,
                        }
                    }
                }
                impl MulAssign<i64> for BigInt
                {
                    #[inline] fn mul_assign(&mut self, other: i64) {
                        match other.checked_uabs() {
                            Positive(u) => *self *= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                self.data *= u;
                            }
                        }
                    }
                }
                impl Mul<i128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn mul(self, other: i128) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self * u,
                            Negative(u) => -self * u,
                        }
                    }
                }
                impl MulAssign<i128> for BigInt
                {
                    #[inline] fn mul_assign(&mut self, other: i128) {
                        match other.checked_uabs() {
                            Positive(u) => *self *= u,
                            Negative(u) => {
                                self.sign = -self.sign;
                                self.data *= u;
                            }
                        }
                    }
                }
                impl CheckedMul for BigInt
                {
                    #[inline] fn checked_mul(&self, v: &BigInt) -> Option<BigInt> {
                        Some(self.mul(v))
                    }
                }
                impl_product_iter_type!(BigInt);

            }
            pub mod subtraction
            {
                use ::
                {
                    cmp::{ Ordering::{ Equal, Greater, Less } },
                    num::{ traits::CheckedSub },
                    ops::{ Sub, SubAssign },
                    *,
                };
                use super::CheckedUnsignedAbs::{Negative, Positive};
                use super::Sign::{Minus, NoSign, Plus};
                use super::{BigInt, UnsignedAbs};
                /*
                */
                macro_rules! bigint_sub
                {
                    ($a:expr, $a_owned:expr, $a_data:expr, $b:expr, $b_owned:expr, $b_data:expr) => {
                        match ($a.sign, $b.sign) {
                            (_, NoSign) => $a_owned,
                            (NoSign, _) => -$b_owned,
                           
                            (Plus, Minus) | (Minus, Plus) => BigInt::from_biguint($a.sign, $a_data + $b_data),
                           
                            (Plus, Plus) | (Minus, Minus) => match $a.data.cmp(&$b.data) {
                                Less => BigInt::from_biguint(-$a.sign, $b_data - $a_data),
                                Greater => BigInt::from_biguint($a.sign, $a_data - $b_data),
                                Equal => BigInt::ZERO,
                            },
                        }
                    };
                }
                impl Sub<&BigInt> for &BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: &BigInt) -> BigInt {
                        bigint_sub!(
                            self,
                            self.clone(),
                            &self.data,
                            other,
                            other.clone(),
                            &other.data
                        )
                    }
                }
                impl Sub<BigInt> for &BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        bigint_sub!(self, self.clone(), &self.data, other, other, other.data)
                    }
                }
                impl Sub<&BigInt> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: &BigInt) -> BigInt {
                        bigint_sub!(self, self, self.data, other, other.clone(), &other.data)
                    }
                }
                impl Sub<BigInt> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        bigint_sub!(self, self, self.data, other, other, other.data)
                    }
                }
                impl SubAssign<&BigInt> for BigInt
                {
                    #[inline] fn sub_assign(&mut self, other: &BigInt) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n - other;
                    }
                }
                forward_val_assign!(impl SubAssign for BigInt, sub_assign);

                promote_all_scalars!(impl Sub for BigInt, sub);
                promote_all_scalars_assign!(impl SubAssign for BigInt, sub_assign);
                forward_all_scalar_binop_to_val_val!(impl Sub<u32> for BigInt, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<u64> for BigInt, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<u128> for BigInt, sub);

                impl Sub<u32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: u32) -> BigInt {
                        match self.sign {
                            NoSign => -BigInt::from(other),
                            Minus => -BigInt::from(self.data + other),
                            Plus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Greater => BigInt::from(self.data - other),
                                Less => -BigInt::from(other - self.data),
                            },
                        }
                    }
                }
                impl SubAssign<u32> for BigInt
                {
                    #[inline] fn sub_assign(&mut self, other: u32) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n - other;
                    }
                }
                impl Sub<BigInt> for u32 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        -(other - self)
                    }
                }
                impl Sub<BigInt> for u64 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        -(other - self)
                    }
                }
                impl Sub<BigInt> for u128 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        -(other - self)
                    }
                }
                impl Sub<u64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: u64) -> BigInt {
                        match self.sign {
                            NoSign => -BigInt::from(other),
                            Minus => -BigInt::from(self.data + other),
                            Plus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Greater => BigInt::from(self.data - other),
                                Less => -BigInt::from(other - self.data),
                            },
                        }
                    }
                }
                impl SubAssign<u64> for BigInt
                {
                    #[inline] fn sub_assign(&mut self, other: u64) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n - other;
                    }
                }
                impl Sub<u128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: u128) -> BigInt {
                        match self.sign {
                            NoSign => -BigInt::from(other),
                            Minus => -BigInt::from(self.data + other),
                            Plus => match self.data.cmp(&From::from(other)) {
                                Equal => Self::ZERO,
                                Greater => BigInt::from(self.data - other),
                                Less => -BigInt::from(other - self.data),
                            },
                        }
                    }
                }
                impl SubAssign<u128> for BigInt
                {
                    #[inline] fn sub_assign(&mut self, other: u128) {
                        let n = mem::replace(self, Self::ZERO);
                        *self = n - other;
                    }
                }
                forward_all_scalar_binop_to_val_val!(impl Sub<i32> for BigInt, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<i64> for BigInt, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<i128> for BigInt, sub);

                impl Sub<i32> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: i32) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self - u,
                            Negative(u) => self + u,
                        }
                    }
                }
                impl SubAssign<i32> for BigInt
                {
                    #[inline] fn sub_assign(&mut self, other: i32) {
                        match other.checked_uabs() {
                            Positive(u) => *self -= u,
                            Negative(u) => *self += u,
                        }
                    }
                }
                impl Sub<BigInt> for i32 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u - other,
                            Negative(u) => -other - u,
                        }
                    }
                }
                impl Sub<i64> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: i64) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self - u,
                            Negative(u) => self + u,
                        }
                    }
                }
                impl SubAssign<i64> for BigInt
                {
                    #[inline] fn sub_assign(&mut self, other: i64) {
                        match other.checked_uabs() {
                            Positive(u) => *self -= u,
                            Negative(u) => *self += u,
                        }
                    }
                }
                impl Sub<BigInt> for i64 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u - other,
                            Negative(u) => -other - u,
                        }
                    }
                }
                impl Sub<i128> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: i128) -> BigInt {
                        match other.checked_uabs() {
                            Positive(u) => self - u,
                            Negative(u) => self + u,
                        }
                    }
                }
                impl SubAssign<i128> for BigInt
                {
                    #[inline] fn sub_assign(&mut self, other: i128) {
                        match other.checked_uabs() {
                            Positive(u) => *self -= u,
                            Negative(u) => *self += u,
                        }
                    }
                }
                impl Sub<BigInt> for i128 {
                    type Output = BigInt;

                    #[inline] fn sub(self, other: BigInt) -> BigInt {
                        match self.checked_uabs() {
                            Positive(u) => u - other,
                            Negative(u) => -other - u,
                        }
                    }
                }
                impl CheckedSub for BigInt
                {
                    #[inline] fn checked_sub(&self, v: &BigInt) -> Option<BigInt> {
                        Some(self.sub(v))
                    }
                }
            }
            pub mod bits
            {
                use ::
                {
                    cmp::{ Ordering::{ Equal, Greater, Less } },
                    num::
                    {
                        big::
                        {
                            uint::{ IntDigits },
                            digit::{ BigDigit, DoubleBigDigit },
                        },
                        traits::{ToPrimitive, Zero},
                    },
                    ops::{ BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign },
                    vec::{ Vec },
                    *,
                };
                use super::BigInt;
                use super::Sign::{Minus, NoSign, Plus};
                /*
                */
                #[inline] fn negate_carry(a: BigDigit, acc: &mut DoubleBigDigit) -> BigDigit
                {
                    *acc += DoubleBigDigit::from(!a);
                    let lo = *acc as BigDigit;
                    *acc >>= ::num::big::digit::BITS;
                    lo
                }
                
                fn bitand_pos_neg(a: &mut [BigDigit], b: &[BigDigit]) 
                {
                    let mut carry_b = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai &= twos_b;
                    }
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                }
                fn bitand_neg_pos(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
                {
                    let mut carry_a = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        *ai = twos_a & bi;
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => a.truncate(b.len()),
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().cloned());
                        }
                    }
                }
                fn bitand_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
                {
                    let mut carry_a = 1;
                    let mut carry_b = 1;
                    let mut carry_and = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai = negate_carry(twos_a & twos_b, &mut carry_and);
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            for ai in a[b.len()..].iter_mut() {
                                let twos_a = negate_carry(*ai, &mut carry_a);
                                *ai = negate_carry(twos_a, &mut carry_and);
                            }
                            debug_assert!(carry_a == 0);
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_b = negate_carry(bi, &mut carry_b);
                                negate_carry(twos_b, &mut carry_and)
                            }));
                            debug_assert!(carry_b == 0);
                        }
                    }
                    if carry_and != 0 {
                        a.push(1);
                    }
                }
                forward_val_val_binop!(impl BitAnd for BigInt, bitand);
                forward_ref_val_binop_big!(impl BitAnd for BigInt, bitand);
                
                impl BitAnd<&BigInt> for &BigInt
                {
                    type Output = BigInt;

                    #[inline] fn bitand(self, other: &BigInt) -> BigInt {
                        match (self.sign, other.sign) {
                            (NoSign, _) | (_, NoSign) => BigInt::ZERO,
                            (Plus, Plus) => BigInt::from(&self.data & &other.data),
                            (Plus, Minus) => self.clone() & other,
                            (Minus, Plus) => other.clone() & self,
                            (Minus, Minus) => {
                               
                                if self.len() >= other.len() {
                                    self.clone() & other
                                } else {
                                    other.clone() & self
                                }
                            }
                        }
                    }
                }
                impl BitAnd<&BigInt> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn bitand(mut self, other: &BigInt) -> BigInt {
                        self &= other;
                        self
                    }
                }
                forward_val_assign!(impl BitAndAssign for BigInt, bitand_assign);

                impl BitAndAssign<&BigInt> for BigInt {
                    fn bitand_assign(&mut self, other: &BigInt) {
                        match (self.sign, other.sign) {
                            (NoSign, _) => {}
                            (_, NoSign) => self.set_zero(),
                            (Plus, Plus) => {
                                self.data &= &other.data;
                                if self.data.is_zero() {
                                    self.sign = NoSign;
                                }
                            }
                            (Plus, Minus) => {
                                bitand_pos_neg(self.digits_mut(), other.digits());
                                self.normalize();
                            }
                            (Minus, Plus) => {
                                bitand_neg_pos(self.digits_mut(), other.digits());
                                self.sign = Plus;
                                self.normalize();
                            }
                            (Minus, Minus) => {
                                bitand_neg_neg(self.digits_mut(), other.digits());
                                self.normalize();
                            }
                        }
                    }
                }
                fn bitor_pos_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
                {
                    let mut carry_b = 1;
                    let mut carry_or = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai = negate_carry(*ai | twos_b, &mut carry_or);
                    }
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            a.truncate(b.len());
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_b = negate_carry(bi, &mut carry_b);
                                negate_carry(twos_b, &mut carry_or)
                            }));
                            debug_assert!(carry_b == 0);
                        }
                    }
                   
                    debug_assert!(carry_or == 0);
                }
                fn bitor_neg_pos(a: &mut [BigDigit], b: &[BigDigit]) 
                {
                    let mut carry_a = 1;
                    let mut carry_or = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        *ai = negate_carry(twos_a | bi, &mut carry_or);
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    if a.len() > b.len() {
                        for ai in a[b.len()..].iter_mut() {
                            let twos_a = negate_carry(*ai, &mut carry_a);
                            *ai = negate_carry(twos_a, &mut carry_or);
                        }
                        debug_assert!(carry_a == 0);
                    }
                   
                    debug_assert!(carry_or == 0);
                }
                fn bitor_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
                {
                    let mut carry_a = 1;
                    let mut carry_b = 1;
                    let mut carry_or = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai = negate_carry(twos_a | twos_b, &mut carry_or);
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                    if a.len() > b.len() {
                        a.truncate(b.len());
                    }
                   
                    debug_assert!(carry_or == 0);
                }
                forward_val_val_binop!(impl BitOr for BigInt, bitor);
                forward_ref_val_binop_big!(impl BitOr for BigInt, bitor);               
               
                impl BitOr<&BigInt> for &BigInt
                {
                    type Output = BigInt;

                    #[inline] fn bitor(self, other: &BigInt) -> BigInt {
                        match (self.sign, other.sign) {
                            (NoSign, _) => other.clone(),
                            (_, NoSign) => self.clone(),
                            (Plus, Plus) => BigInt::from(&self.data | &other.data),
                            (Plus, Minus) => other.clone() | self,
                            (Minus, Plus) => self.clone() | other,
                            (Minus, Minus) => {
                               
                                if self.len() <= other.len() {
                                    self.clone() | other
                                } else {
                                    other.clone() | self
                                }
                            }
                        }
                    }
                }
                impl BitOr<&BigInt> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn bitor(mut self, other: &BigInt) -> BigInt {
                        self |= other;
                        self
                    }
                }
                forward_val_assign!(impl BitOrAssign for BigInt, bitor_assign);

                impl BitOrAssign<&BigInt> for BigInt {
                    fn bitor_assign(&mut self, other: &BigInt) {
                        match (self.sign, other.sign) {
                            (_, NoSign) => {}
                            (NoSign, _) => self.clone_from(other),
                            (Plus, Plus) => self.data |= &other.data,
                            (Plus, Minus) => {
                                bitor_pos_neg(self.digits_mut(), other.digits());
                                self.sign = Minus;
                                self.normalize();
                            }
                            (Minus, Plus) => {
                                bitor_neg_pos(self.digits_mut(), other.digits());
                                self.normalize();
                            }
                            (Minus, Minus) => {
                                bitor_neg_neg(self.digits_mut(), other.digits());
                                self.normalize();
                            }
                        }
                    }
                }
                fn bitxor_pos_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
                {
                    let mut carry_b = 1;
                    let mut carry_xor = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai = negate_carry(*ai ^ twos_b, &mut carry_xor);
                    }
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            for ai in a[b.len()..].iter_mut() {
                                let twos_b = !0;
                                *ai = negate_carry(*ai ^ twos_b, &mut carry_xor);
                            }
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_b = negate_carry(bi, &mut carry_b);
                                negate_carry(twos_b, &mut carry_xor)
                            }));
                            debug_assert!(carry_b == 0);
                        }
                    }
                    if carry_xor != 0 {
                        a.push(1);
                    }
                }
                fn bitxor_neg_pos(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
                {
                    let mut carry_a = 1;
                    let mut carry_xor = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        *ai = negate_carry(twos_a ^ bi, &mut carry_xor);
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            for ai in a[b.len()..].iter_mut() {
                                let twos_a = negate_carry(*ai, &mut carry_a);
                                *ai = negate_carry(twos_a, &mut carry_xor);
                            }
                            debug_assert!(carry_a == 0);
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_a = !0;
                                negate_carry(twos_a ^ bi, &mut carry_xor)
                            }));
                        }
                    }
                    if carry_xor != 0 {
                        a.push(1);
                    }
                }
                fn bitxor_neg_neg(a: &mut Vec<BigDigit>, b: &[BigDigit]) 
                {
                    let mut carry_a = 1;
                    let mut carry_b = 1;
                    for (ai, &bi) in a.iter_mut().zip(b.iter()) {
                        let twos_a = negate_carry(*ai, &mut carry_a);
                        let twos_b = negate_carry(bi, &mut carry_b);
                        *ai = twos_a ^ twos_b;
                    }
                    debug_assert!(a.len() > b.len() || carry_a == 0);
                    debug_assert!(b.len() > a.len() || carry_b == 0);
                    match Ord::cmp(&a.len(), &b.len()) {
                        Greater => {
                            for ai in a[b.len()..].iter_mut() {
                                let twos_a = negate_carry(*ai, &mut carry_a);
                                let twos_b = !0;
                                *ai = twos_a ^ twos_b;
                            }
                            debug_assert!(carry_a == 0);
                        }
                        Equal => {}
                        Less => {
                            let extra = &b[a.len()..];
                            a.extend(extra.iter().map(|&bi| {
                                let twos_a = !0;
                                let twos_b = negate_carry(bi, &mut carry_b);
                                twos_a ^ twos_b
                            }));
                            debug_assert!(carry_b == 0);
                        }
                    }
                }
                forward_all_binop_to_val_ref_commutative!(impl BitXor for BigInt, bitxor);

                impl BitXor<&BigInt> for BigInt
                {
                    type Output = BigInt;

                    #[inline] fn bitxor(mut self, other: &BigInt) -> BigInt {
                        self ^= other;
                        self
                    }
                }
                forward_val_assign!(impl BitXorAssign for BigInt, bitxor_assign);

                impl BitXorAssign<&BigInt> for BigInt
                {
                    fn bitxor_assign(&mut self, other: &BigInt) {
                        match (self.sign, other.sign) {
                            (_, NoSign) => {}
                            (NoSign, _) => self.clone_from(other),
                            (Plus, Plus) => {
                                self.data ^= &other.data;
                                if self.data.is_zero() {
                                    self.sign = NoSign;
                                }
                            }
                            (Plus, Minus) => {
                                bitxor_pos_neg(self.digits_mut(), other.digits());
                                self.sign = Minus;
                                self.normalize();
                            }
                            (Minus, Plus) => {
                                bitxor_neg_pos(self.digits_mut(), other.digits());
                                self.normalize();
                            }
                            (Minus, Minus) => {
                                bitxor_neg_neg(self.digits_mut(), other.digits());
                                self.sign = Plus;
                                self.normalize();
                            }
                        }
                    }
                }
                pub fn set_negative_bit(x: &mut BigInt, bit: u64, value: bool)
                {
                    debug_assert_eq!(x.sign, Minus);
                    let data = &mut x.data;

                    let bits_per_digit = u64::from(::num::big::digit::BITS);
                    if bit >= bits_per_digit * data.len() as u64 {
                        if !value {
                            data.set_bit(bit, true);
                        }
                    } else {
                       
                       
                       
                       
                       
                       
                        let trailing_zeros = data.trailing_zeros().unwrap();
                        if bit > trailing_zeros {
                            data.set_bit(bit, !value);
                        } else if bit == trailing_zeros && !value {
                           
                           
                           
                           
                           
                            let bit_index = (bit / bits_per_digit).to_usize().unwrap();
                            let bit_mask = (1 as BigDigit) << (bit % bits_per_digit);
                            let mut digit_iter = data.digits_mut().iter_mut().skip(bit_index);
                            let mut carry_in = 1;
                            let mut carry_out = 1;

                            let digit = digit_iter.next().unwrap();
                            let twos_in = negate_carry(*digit, &mut carry_in);
                            let twos_out = twos_in & !bit_mask;
                            *digit = negate_carry(twos_out, &mut carry_out);

                            for digit in digit_iter {
                                if carry_in == 0 && carry_out == 0 {
                                   
                                    break;
                                }
                                let twos = negate_carry(*digit, &mut carry_in);
                                *digit = negate_carry(twos, &mut carry_out);
                            }
                            if carry_out != 0 {
                               
                                debug_assert_eq!(carry_in, 0);
                                data.digits_mut().push(1);
                            }
                        } else if bit < trailing_zeros && value {
                           
                           
                           
                           
                           
                           
                            let index_lo = (bit / bits_per_digit).to_usize().unwrap();
                            let index_hi = (trailing_zeros / bits_per_digit).to_usize().unwrap();
                            let bit_mask_lo = ::num::big::digit::MAX << (bit % bits_per_digit);
                            let bit_mask_hi =
                                ::num::big::digit::MAX >> (bits_per_digit - 1 - (trailing_zeros % bits_per_digit));
                            let digits = data.digits_mut();

                            if index_lo == index_hi {
                                digits[index_lo] ^= bit_mask_lo & bit_mask_hi;
                            } else {
                                digits[index_lo] = bit_mask_lo;
                                for digit in &mut digits[index_lo + 1..index_hi] {
                                    *digit = ::num::big::digit::MAX;
                                }
                                digits[index_hi] ^= bit_mask_hi;
                            }
                        } else {
                           
                           
                           
                        }
                    }
                }
            }
            pub mod convert
            {
                use ::
                {
                    cmp::{ Ordering::{ Equal, Greater, Less } },
                    convert::{ TryFrom },
                    num::
                    {
                        big::{ BigUint, ParseBigIntError, ToBigUint, TryFromBigIntError },
                        traits::{ FromPrimitive, Num, One, ToPrimitive, Zero },
                    },
                    str::{ self, FromStr },
                    vec::{ Vec },
                    *,
                };
                use super::Sign::{self, Minus, NoSign, Plus};
                use super::{BigInt, ToBigInt};
                /*
                */
                impl FromStr for BigInt
                {
                    type Err = ParseBigIntError;

                    #[inline] fn from_str(s: &str) -> Result<BigInt, ParseBigIntError> {
                        BigInt::from_str_radix(s, 10)
                    }
                }
                impl Num for BigInt
                {
                    type FromStrRadixErr = ParseBigIntError;

                    /// Creates and initializes a [`BigInt`].
                    #[inline] fn from_str_radix(mut s: &str, radix: u32) -> Result<BigInt, ParseBigIntError> {
                        let sign = if let Some(tail) = s.strip_prefix('-') {
                            if !tail.starts_with('+') {
                                s = tail
                            }
                            Minus
                        } else {
                            Plus
                        };
                        let bu = BigUint::from_str_radix(s, radix)?;
                        Ok(BigInt::from_biguint(sign, bu))
                    }
                }
                impl ToPrimitive for BigInt
                {
                    #[inline] fn to_i64(&self) -> Option<i64> {
                        match self.sign {
                            Plus => self.data.to_i64(),
                            NoSign => Some(0),
                            Minus => {
                                let n = self.data.to_u64()?;
                                let m: u64 = 1 << 63;
                                match n.cmp(&m) {
                                    Less => Some(-(n as i64)),
                                    Equal => Some(i64::MIN),
                                    Greater => None,
                                }
                            }
                        }
                    }
                    #[inline] fn to_i128(&self) -> Option<i128> {
                        match self.sign {
                            Plus => self.data.to_i128(),
                            NoSign => Some(0),
                            Minus => {
                                let n = self.data.to_u128()?;
                                let m: u128 = 1 << 127;
                                match n.cmp(&m) {
                                    Less => Some(-(n as i128)),
                                    Equal => Some(i128::MIN),
                                    Greater => None,
                                }
                            }
                        }
                    }
                    #[inline] fn to_u64(&self) -> Option<u64> {
                        match self.sign {
                            Plus => self.data.to_u64(),
                            NoSign => Some(0),
                            Minus => None,
                        }
                    }
                    #[inline] fn to_u128(&self) -> Option<u128> {
                        match self.sign {
                            Plus => self.data.to_u128(),
                            NoSign => Some(0),
                            Minus => None,
                        }
                    }
                    #[inline] fn to_f32(&self) -> Option<f32> {
                        let n = self.data.to_f32()?;
                        Some(if self.sign == Minus { -n } else { n })
                    }
                    #[inline] fn to_f64(&self) -> Option<f64> {
                        let n = self.data.to_f64()?;
                        Some(if self.sign == Minus { -n } else { n })
                    }
                }
                macro_rules! impl_try_from_bigint 
                {
                    ($T:ty, $to_ty:path) => {
                        impl TryFrom<&BigInt> for $T {
                            type Error = TryFromBigIntError<()>;

                            #[inline]
                            fn try_from(value: &BigInt) -> Result<$T, TryFromBigIntError<()>> {
                                $to_ty(value).ok_or(TryFromBigIntError::new(()))
                            }
                        }
                        impl TryFrom<BigInt> for $T {
                            type Error = TryFromBigIntError<BigInt>;

                            #[inline]
                            fn try_from(value: BigInt) -> Result<$T, TryFromBigIntError<BigInt>> {
                                <$T>::try_from(&value).map_err(|_| TryFromBigIntError::new(value))
                            }
                        }
                    };
                }
                impl_try_from_bigint!(u8, ToPrimitive::to_u8);
                impl_try_from_bigint!(u16, ToPrimitive::to_u16);
                impl_try_from_bigint!(u32, ToPrimitive::to_u32);
                impl_try_from_bigint!(u64, ToPrimitive::to_u64);
                impl_try_from_bigint!(usize, ToPrimitive::to_usize);
                impl_try_from_bigint!(u128, ToPrimitive::to_u128);

                impl_try_from_bigint!(i8, ToPrimitive::to_i8);
                impl_try_from_bigint!(i16, ToPrimitive::to_i16);
                impl_try_from_bigint!(i32, ToPrimitive::to_i32);
                impl_try_from_bigint!(i64, ToPrimitive::to_i64);
                impl_try_from_bigint!(isize, ToPrimitive::to_isize);
                impl_try_from_bigint!(i128, ToPrimitive::to_i128);

                impl FromPrimitive for BigInt
                {
                    #[inline] fn from_i64(n: i64) -> Option<BigInt> {
                        Some(BigInt::from(n))
                    }
                    #[inline] fn from_i128(n: i128) -> Option<BigInt> {
                        Some(BigInt::from(n))
                    }
                    #[inline] fn from_u64(n: u64) -> Option<BigInt> {
                        Some(BigInt::from(n))
                    }
                    #[inline] fn from_u128(n: u128) -> Option<BigInt> {
                        Some(BigInt::from(n))
                    }
                    #[inline] fn from_f64(n: f64) -> Option<BigInt> {
                        if n >= 0.0 {
                            BigUint::from_f64(n).map(BigInt::from)
                        } else {
                            let x = BigUint::from_f64(-n)?;
                            Some(-BigInt::from(x))
                        }
                    }
                }
                impl From<i64> for BigInt
                {
                    #[inline] fn from(n: i64) -> Self {
                        if n >= 0 {
                            BigInt::from(n as u64)
                        } else {
                            let u = u64::MAX - (n as u64) + 1;
                            BigInt {
                                sign: Minus,
                                data: BigUint::from(u),
                            }
                        }
                    }
                }
                impl From<i128> for BigInt
                {
                    #[inline] fn from(n: i128) -> Self {
                        if n >= 0 {
                            BigInt::from(n as u128)
                        } else {
                            let u = u128::MAX - (n as u128) + 1;
                            BigInt {
                                sign: Minus,
                                data: BigUint::from(u),
                            }
                        }
                    }
                }
                macro_rules! impl_bigint_from_int 
                {
                    ($T:ty) => {
                        impl From<$T> for BigInt {
                            #[inline]
                            fn from(n: $T) -> Self {
                                BigInt::from(n as i64)
                            }
                        }
                    };
                }
                impl_bigint_from_int!(i8);
                impl_bigint_from_int!(i16);
                impl_bigint_from_int!(i32);
                impl_bigint_from_int!(isize);

                impl From<u64> for BigInt
                {
                    #[inline] fn from(n: u64) -> Self {
                        if n > 0 {
                            BigInt {
                                sign: Plus,
                                data: BigUint::from(n),
                            }
                        } else {
                            Self::ZERO
                        }
                    }
                }
                impl From<u128> for BigInt
                {
                    #[inline] fn from(n: u128) -> Self {
                        if n > 0 {
                            BigInt {
                                sign: Plus,
                                data: BigUint::from(n),
                            }
                        } else {
                            Self::ZERO
                        }
                    }
                }
                macro_rules! impl_bigint_from_uint 
                {
                    ($T:ty) => {
                        impl From<$T> for BigInt {
                            #[inline]
                            fn from(n: $T) -> Self {
                                BigInt::from(n as u64)
                            }
                        }
                    };
                }
                impl_bigint_from_uint!(u8);
                impl_bigint_from_uint!(u16);
                impl_bigint_from_uint!(u32);
                impl_bigint_from_uint!(usize);

                impl From<BigUint> for BigInt
                {
                    #[inline] fn from(n: BigUint) -> Self {
                        if n.is_zero() {
                            Self::ZERO
                        } else {
                            BigInt {
                                sign: Plus,
                                data: n,
                            }
                        }
                    }
                }
                impl ToBigInt for BigInt
                {
                    #[inline] fn to_bigint(&self) -> Option<BigInt> {
                        Some(self.clone())
                    }
                }
                impl ToBigInt for BigUint
                {
                    #[inline] fn to_bigint(&self) -> Option<BigInt> {
                        if self.is_zero() {
                            Some(BigInt::ZERO)
                        } else {
                            Some(BigInt {
                                sign: Plus,
                                data: self.clone(),
                            })
                        }
                    }
                }
                impl ToBigUint for BigInt
                {
                    #[inline] fn to_biguint(&self) -> Option<BigUint> {
                        match self.sign() {
                            Plus => Some(self.data.clone()),
                            NoSign => Some(BigUint::ZERO),
                            Minus => None,
                        }
                    }
                }
                impl TryFrom<&BigInt> for BigUint
                {
                    type Error = TryFromBigIntError<()>;

                    #[inline] fn try_from(value: &BigInt) -> Result<BigUint, TryFromBigIntError<()>> {
                        value
                            .to_biguint()
                            .ok_or_else(|| TryFromBigIntError::new(()))
                    }
                }
                impl TryFrom<BigInt> for BigUint
                {
                    type Error = TryFromBigIntError<BigInt>;

                    #[inline] fn try_from(value: BigInt) -> Result<BigUint, TryFromBigIntError<BigInt>> {
                        if value.sign() == Sign::Minus {
                            Err(TryFromBigIntError::new(value))
                        } else {
                            Ok(value.data)
                        }
                    }
                }
                macro_rules! impl_to_bigint
                {
                    ($T:ty, $from_ty:path) => {
                        impl ToBigInt for $T {
                            #[inline]
                            fn to_bigint(&self) -> Option<BigInt> {
                                $from_ty(*self)
                            }
                        }
                    };
                }
                impl_to_bigint!(isize, FromPrimitive::from_isize);
                impl_to_bigint!(i8, FromPrimitive::from_i8);
                impl_to_bigint!(i16, FromPrimitive::from_i16);
                impl_to_bigint!(i32, FromPrimitive::from_i32);
                impl_to_bigint!(i64, FromPrimitive::from_i64);
                impl_to_bigint!(i128, FromPrimitive::from_i128);

                impl_to_bigint!(usize, FromPrimitive::from_usize);
                impl_to_bigint!(u8, FromPrimitive::from_u8);
                impl_to_bigint!(u16, FromPrimitive::from_u16);
                impl_to_bigint!(u32, FromPrimitive::from_u32);
                impl_to_bigint!(u64, FromPrimitive::from_u64);
                impl_to_bigint!(u128, FromPrimitive::from_u128);

                impl_to_bigint!(f32, FromPrimitive::from_f32);
                impl_to_bigint!(f64, FromPrimitive::from_f64);

                impl From<bool> for BigInt 
                {
                    fn from(x: bool) -> Self {
                        if x {
                            One::one()
                        } else {
                            Self::ZERO
                        }
                    }
                }
                #[inline] pub fn from_signed_bytes_be(digits: &[u8]) -> BigInt 
                {
                    let sign = match digits.first() {
                        Some(v) if *v > 0x7f => Sign::Minus,
                        Some(_) => Sign::Plus,
                        None => return BigInt::ZERO,
                    };

                    if sign == Sign::Minus {
                       
                        let mut digits = Vec::from(digits);
                        twos_complement_be(&mut digits);
                        BigInt::from_biguint(sign, BigUint::from_bytes_be(&digits))
                    } else {
                        BigInt::from_biguint(sign, BigUint::from_bytes_be(digits))
                    }
                }
                #[inline] pub fn from_signed_bytes_le(digits: &[u8]) -> BigInt 
                {
                    let sign = match digits.last() {
                        Some(v) if *v > 0x7f => Sign::Minus,
                        Some(_) => Sign::Plus,
                        None => return BigInt::ZERO,
                    };

                    if sign == Sign::Minus {
                       
                        let mut digits = Vec::from(digits);
                        twos_complement_le(&mut digits);
                        BigInt::from_biguint(sign, BigUint::from_bytes_le(&digits))
                    } else {
                        BigInt::from_biguint(sign, BigUint::from_bytes_le(digits))
                    }
                }
                #[inline] pub fn to_signed_bytes_be(x: &BigInt) -> Vec<u8> 
                {
                    let mut bytes = x.data.to_bytes_be();
                    let first_byte = bytes.first().cloned().unwrap_or(0);
                    if first_byte > 0x7f
                        && !(first_byte == 0x80 && bytes.iter().skip(1).all(Zero::is_zero) && x.sign == Sign::Minus)
                    {
                       
                        bytes.insert(0, 0);
                    }
                    if x.sign == Sign::Minus {
                        twos_complement_be(&mut bytes);
                    }
                    bytes
                }
                #[inline] pub fn to_signed_bytes_le(x: &BigInt) -> Vec<u8> 
                {
                    let mut bytes = x.data.to_bytes_le();
                    let last_byte = bytes.last().cloned().unwrap_or(0);
                    if last_byte > 0x7f
                        && !(last_byte == 0x80
                            && bytes.iter().rev().skip(1).all(Zero::is_zero)
                            && x.sign == Sign::Minus)
                    {
                       
                        bytes.push(0);
                    }
                    if x.sign == Sign::Minus {
                        twos_complement_le(&mut bytes);
                    }
                    bytes
                }
                /// Perform in-place two's complement of the given binary representation, in little-endian byte order.
                #[inline] fn twos_complement_le(digits: &mut [u8])
                {
                    twos_complement(digits)
                }
                /// Perform in-place two's complement of the given binary representation in big-endian byte order.
                #[inline] fn twos_complement_be(digits: &mut [u8])
                {
                    twos_complement(digits.iter_mut().rev())
                }
                /// Perform in-place two's complement of the given digit iterator starting from the least significant byte.
                #[inline] fn twos_complement<'a, I>(digits: I) where
                I: IntoIterator<Item = &'a mut u8>
                {
                    let mut carry = true;
                    for d in digits {
                        *d = !*d;
                        if carry {
                            *d = d.wrapping_add(1);
                            carry = d.is_zero();
                        }
                    }
                }
            }
            pub mod power
            {
                use ::
                {
                    num::
                    {
                        big::{ BigUint }, 
                        integers::Integer,
                        traits::{Pow, Signed, Zero},
                    },
                    *,
                };
                use super::BigInt;
                use super::Sign::{self, Minus, Plus};
                /*
                */
                /// Help function for pow
                ///
                /// Computes the effect of the exponent on the sign.
                #[inline] fn powsign<T: Integer>(sign: Sign, other: &T) -> Sign {
                    if other.is_zero() {
                        Plus
                    } else if sign != Minus || other.is_odd() {
                        sign
                    } else {
                        -sign
                    }
                }
                macro_rules! pow_impl {
                    ($T:ty) => {
                        impl Pow<$T> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn pow(self, rhs: $T) -> BigInt {
                                BigInt::from_biguint(powsign(self.sign, &rhs), self.data.pow(rhs))
                            }
                        }
                        impl Pow<&$T> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn pow(self, rhs: &$T) -> BigInt {
                                BigInt::from_biguint(powsign(self.sign, rhs), self.data.pow(rhs))
                            }
                        }
                        impl Pow<$T> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn pow(self, rhs: $T) -> BigInt {
                                BigInt::from_biguint(powsign(self.sign, &rhs), Pow::pow(&self.data, rhs))
                            }
                        }
                        impl Pow<&$T> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn pow(self, rhs: &$T) -> BigInt {
                                BigInt::from_biguint(powsign(self.sign, rhs), Pow::pow(&self.data, rhs))
                            }
                        }
                    };
                }
                pow_impl!(u8);
                pow_impl!(u16);
                pow_impl!(u32);
                pow_impl!(u64);
                pow_impl!(usize);
                pow_impl!(u128);
                pow_impl!(BigUint);

                pub(super) fn modpow(x: &BigInt, exponent: &BigInt, modulus: &BigInt) -> BigInt {
                    assert!(
                        !exponent.is_negative(),
                        "negative exponentiation is not supported!"
                    );
                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );

                    let result = x.data.modpow(&exponent.data, &modulus.data);
                    if result.is_zero() {
                        return BigInt::ZERO;
                    }
                   
                    let (sign, mag) = match (x.is_negative() && exponent.is_odd(), modulus.is_negative()) {
                        (false, false) => (Plus, result),
                        (true, false) => (Plus, &modulus.data - result),
                        (false, true) => (Minus, &modulus.data - result),
                        (true, true) => (Minus, result),
                    };
                    BigInt::from_biguint(sign, mag)
                }
            }
            pub mod shift
            {
                use ::
                {
                    num::traits::{ PrimInt, Signed, Zero },
                    ops::{ Shl, ShlAssign, Shr, ShrAssign },
                    *,
                };
                use super::BigInt;
                use super::Sign::NoSign;
                /*
                */
                macro_rules! impl_shift
                {
                    (@ref $Shx:ident :: $shx:ident, $ShxAssign:ident :: $shx_assign:ident, $rhs:ty) => {
                        impl $Shx<&$rhs> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn $shx(self, rhs: &$rhs) -> BigInt {
                                $Shx::$shx(self, *rhs)
                            }
                        }
                        impl $Shx<&$rhs> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn $shx(self, rhs: &$rhs) -> BigInt {
                                $Shx::$shx(self, *rhs)
                            }
                        }
                        impl $ShxAssign<&$rhs> for BigInt {
                            #[inline]
                            fn $shx_assign(&mut self, rhs: &$rhs) {
                                $ShxAssign::$shx_assign(self, *rhs);
                            }
                        }
                    };
                    ($($rhs:ty),+) => {$(
                        impl Shl<$rhs> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn shl(self, rhs: $rhs) -> BigInt {
                                BigInt::from_biguint(self.sign, self.data << rhs)
                            }
                        }
                        impl Shl<$rhs> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn shl(self, rhs: $rhs) -> BigInt {
                                BigInt::from_biguint(self.sign, &self.data << rhs)
                            }
                        }
                        impl ShlAssign<$rhs> for BigInt {
                            #[inline]
                            fn shl_assign(&mut self, rhs: $rhs) {
                                self.data <<= rhs
                            }
                        }
                        impl_shift! { @ref Shl::shl, ShlAssign::shl_assign, $rhs }
                        impl Shr<$rhs> for BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn shr(self, rhs: $rhs) -> BigInt {
                                let round_down = shr_round_down(&self, rhs);
                                let data = self.data >> rhs;
                                let data = if round_down { data + 1u8 } else { data };
                                BigInt::from_biguint(self.sign, data)
                            }
                        }
                        impl Shr<$rhs> for &BigInt {
                            type Output = BigInt;

                            #[inline]
                            fn shr(self, rhs: $rhs) -> BigInt {
                                let round_down = shr_round_down(self, rhs);
                                let data = &self.data >> rhs;
                                let data = if round_down { data + 1u8 } else { data };
                                BigInt::from_biguint(self.sign, data)
                            }
                        }
                        impl ShrAssign<$rhs> for BigInt {
                            #[inline]
                            fn shr_assign(&mut self, rhs: $rhs) {
                                let round_down = shr_round_down(self, rhs);
                                self.data >>= rhs;
                                if round_down {
                                    self.data += 1u8;
                                } else if self.data.is_zero() {
                                    self.sign = NoSign;
                                }
                            }
                        }
                        impl_shift! { @ref Shr::shr, ShrAssign::shr_assign, $rhs }
                    )*};
                }
                impl_shift! { u8, u16, u32, u64, u128, usize }
                impl_shift! { i8, i16, i32, i64, i128, isize }
                
                fn shr_round_down<T: PrimInt>(i: &BigInt, shift: T) -> bool
                {
                    if i.is_negative() {
                        let zeros = i.trailing_zeros().expect("negative values are non-zero");
                        shift > T::zero() && shift.to_u64().map(|shift| zeros < shift).unwrap_or(true)
                    } else {
                        false
                    }
                }
            }
            /// A `Sign` is a [`BigInt`]'s composing element.
            #[derive(PartialEq, PartialOrd, Eq, Ord, Copy, Clone, Debug, Hash)]
            pub enum Sign 
            {
                Minus,
                NoSign,
                Plus,
            }
            
            impl Neg for Sign 
            {
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
            pub struct BigInt 
            {
                sign: Sign,
                data: BigUint,
            }
                       
            impl Clone for BigInt
            {
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
            
            impl hash::Hash for BigInt
            {
                #[inline] fn hash<H: hash::Hasher>(&self, state: &mut H) {
                    debug_assert!((self.sign != NoSign) ^ self.data.is_zero());
                    self.sign.hash(state);
                    if self.sign != NoSign {
                        self.data.hash(state);
                    }
                }
            }
            
            impl PartialEq for BigInt
            {
                #[inline] fn eq(&self, other: &BigInt) -> bool {
                    debug_assert!((self.sign != NoSign) ^ self.data.is_zero());
                    debug_assert!((other.sign != NoSign) ^ other.data.is_zero());
                    self.sign == other.sign && (self.sign == NoSign || self.data == other.data)
                }
            }
            
            impl Eq for BigInt {}
            
            impl PartialOrd for BigInt
            {
                #[inline] fn partial_cmp(&self, other: &BigInt) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }
            
            impl Ord for BigInt
            {
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
            
            impl Default for BigInt
            {
                #[inline] fn default() -> BigInt {
                    Self::ZERO
                }
            }
            
            impl fmt::Debug for BigInt
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {    fmt::Display::fmt(self, f)
                }
            }
            
            impl fmt::Display for BigInt
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {    f.pad_integral(!self.is_negative(), "", &self.data.to_str_radix(10))
                }
            }
            
            impl fmt::Binary for BigInt
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {    f.pad_integral(!self.is_negative(), "0b", &self.data.to_str_radix(2))
                }
            }
            
            impl fmt::Octal for BigInt
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {    f.pad_integral(!self.is_negative(), "0o", &self.data.to_str_radix(8))
                }
            }
            
            impl fmt::LowerHex for BigInt
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {    f.pad_integral(!self.is_negative(), "0x", &self.data.to_str_radix(16))
                }
            }
            
            impl fmt::UpperHex for BigInt
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {    let mut s = self.data.to_str_radix(16);
                    s.make_ascii_uppercase();
                    f.pad_integral(!self.is_negative(), "0x", &s)
                }
            }
            
            impl Not for BigInt
            {
                type  Output = BigInt;

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
            
            impl Not for &BigInt
            {
                type  Output = BigInt;

                fn not(self) -> BigInt {
                    match self.sign {
                        NoSign => -BigInt::one(),
                        Plus => -BigInt::from(&self.data + 1u32),
                        Minus => BigInt::from(&self.data - 1u32),
                    }
                }
            }
            
            impl Zero for BigInt
            {
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
            
            impl ConstZero for BigInt 
            {
               
                const ZERO: Self = Self::ZERO;
            }
            
            impl One for BigInt
            {
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
            
            impl Signed for BigInt
            {
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
            trait UnsignedAbs 
            {
                type Unsigned;

                fn checked_uabs(self) -> CheckedUnsignedAbs<Self::Unsigned>;
            }
            enum CheckedUnsignedAbs<T> 
            {
                Positive(T),
                Negative(T),
            } use self::CheckedUnsignedAbs::{Negative, Positive};

            macro_rules! impl_unsigned_abs
            {
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

            impl Neg for BigInt
            {
                type  Output = BigInt;

                #[inline] fn neg(mut self) -> BigInt {
                    self.sign = -self.sign;
                    self
                }
            }
            
            impl Neg for &BigInt
            {
                type Output = BigInt;

                #[inline] fn neg(self) -> BigInt 
                {
                    -self.clone()
                }
            }
            
            impl Integer for BigInt
            {
                #[inline] fn div_rem(&self, other: &BigInt) -> (BigInt, BigInt) {
                   
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
                #[inline] fn extended_gcd_lcm(&self, other: &BigInt) -> (::num::integers::ExtendedGcd<BigInt>, BigInt) {
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
            
            impl Roots for BigInt
            {
                fn nth_root(&self, n: u32) -> Self
                {
                    assert!(
                        !(self.is_negative() && n.is_even()),
                        "root of degree {} is imaginary",
                        n
                    );

                    BigInt::from_biguint(self.sign, self.data.nth_root(n))
                }
                fn sqrt(&self) -> Self
                {
                    assert!(!self.is_negative(), "square root is imaginary");

                    BigInt::from_biguint(self.sign, self.data.sqrt())
                }
                fn cbrt(&self) -> Self
                {
                    BigInt::from_biguint(self.sign, self.data.cbrt())
                }
            }
            
            impl IntDigits for BigInt
            {
                #[inline] fn digits(&self) -> &[BigDigit] { self.data.digits() }
                #[inline] fn digits_mut(&mut self) -> &mut Vec<BigDigit> { self.data.digits_mut() }
                #[inline] fn normalize(&mut self)
                {
                    self.data.normalize();
                    if self.data.is_zero() { self.sign = NoSign; }
                }
                #[inline] fn capacity(&self) -> usize { self.data.capacity() }
                #[inline] fn len(&self) -> usize { self.data.len() }
            }
            /// A generic trait for converting a value to a [`BigInt`]. This may return
            /// `None` when converting from `f32` or `f64`, and will always succeed
            /// when converting from any integer or unsigned primitive, or [`BigUint`].
            pub trait ToBigInt
            {
                /// Converts the value of `self` to a [`BigInt`].
                fn to_bigint(&self) -> Option<BigInt>;
            }
            
            impl BigInt
            {
                /// A constant `BigInt` with value 0, useful for static initialization.
                pub const ZERO: Self = BigInt {
                    sign: NoSign,
                    data: BigUint::ZERO,
                };
                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn new(sign: Sign, digits: Vec<u32>) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::new(digits))
                }
                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn from_biguint(mut sign: Sign, mut data: BigUint) -> BigInt {
                    if sign == NoSign {
                        data.assign_from_slice(&[]);
                    } else if data.is_zero() {
                        sign = NoSign;
                    }
                    BigInt { sign, data }
                }
                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn from_slice(sign: Sign, slice: &[u32]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_slice(slice))
                }
                /// Reinitializes a [`BigInt`].
                #[inline] pub fn assign_from_slice(&mut self, sign: Sign, slice: &[u32]) {
                    if sign == NoSign {
                        self.set_zero();
                    } else {
                        self.data.assign_from_slice(slice);
                        self.sign = if self.data.is_zero() { NoSign } else { sign };
                    }
                }
                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn from_bytes_be(sign: Sign, bytes: &[u8]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_bytes_be(bytes))
                }
                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn from_bytes_le(sign: Sign, bytes: &[u8]) -> BigInt {
                    BigInt::from_biguint(sign, BigUint::from_bytes_le(bytes))
                }
                /// Creates and initializes a [`BigInt`] from an array of bytes in
                /// two's complement binary representation.
                #[inline] pub fn from_signed_bytes_be(digits: &[u8]) -> BigInt {
                    convert::from_signed_bytes_be(digits)
                }
                /// Creates and initializes a [`BigInt`] from an array of bytes in two's complement.
                #[inline] pub fn from_signed_bytes_le(digits: &[u8]) -> BigInt {
                    convert::from_signed_bytes_le(digits)
                }
                /// Creates and initializes a [`BigInt`].
                #[inline] pub fn parse_bytes(buf: &[u8], radix: u32) -> Option<BigInt> {
                    let s = str::from_utf8(buf).ok()?;
                    BigInt::from_str_radix(s, radix).ok()
                }
                /// Creates and initializes a [`BigInt`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                pub fn from_radix_be(sign: Sign, buf: &[u8], radix: u32) -> Option<BigInt> {
                    let u = BigUint::from_radix_be(buf, radix)?;
                    Some(BigInt::from_biguint(sign, u))
                }
                /// Creates and initializes a [`BigInt`]. Each `u8` of the input slice is
                /// interpreted as one digit of the number
                /// and must therefore be less than `radix`.
                pub fn from_radix_le(sign: Sign, buf: &[u8], radix: u32) -> Option<BigInt> {
                    let u = BigUint::from_radix_le(buf, radix)?;
                    Some(BigInt::from_biguint(sign, u))
                }
                /// Returns the sign and the byte representation of the [`BigInt`] in big-endian byte order.
                #[inline] pub fn to_bytes_be(&self) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_bytes_be())
                }
                /// Returns the sign and the byte representation of the [`BigInt`] in little-endian byte order.
                #[inline] pub fn to_bytes_le(&self) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_bytes_le())
                }
                /// Returns the sign and the `u32` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                #[inline] pub fn to_u32_digits(&self) -> (Sign, Vec<u32>) {
                    (self.sign, self.data.to_u32_digits())
                }
                /// Returns the sign and the `u64` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                #[inline] pub fn to_u64_digits(&self) -> (Sign, Vec<u64>) {
                    (self.sign, self.data.to_u64_digits())
                }
                /// Returns an iterator of `u32` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                #[inline] pub fn iter_u32_digits(&self) -> U32Digits<'_> {
                    self.data.iter_u32_digits()
                }
                /// Returns an iterator of `u64` digits representation of the [`BigInt`] ordered least
                /// significant digit first.
                #[inline] pub fn iter_u64_digits(&self) -> U64Digits<'_> {
                    self.data.iter_u64_digits()
                }
                /// Returns the two's-complement byte representation of the [`BigInt`] in big-endian byte order.
                #[inline] pub fn to_signed_bytes_be(&self) -> Vec<u8> {
                    convert::to_signed_bytes_be(self)
                }
                /// Returns the two's-complement byte representation of the [`BigInt`] in little-endian byte order.
                #[inline] pub fn to_signed_bytes_le(&self) -> Vec<u8> {
                    convert::to_signed_bytes_le(self)
                }
                /// Returns the integer formatted as a string in the given radix.
                #[inline] pub fn to_str_radix(&self, radix: u32) -> String {
                    let mut v = to_str_radix_reversed(&self.data, radix);

                    if self.is_negative() {
                        v.push(b'-');
                    }
                    v.reverse();
                    unsafe { String::from_utf8_unchecked(v) }
                }
                /// Returns the integer in the requested base in big-endian digit order.
                #[inline] pub fn to_radix_be(&self, radix: u32) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_radix_be(radix))
                }
                /// Returns the integer in the requested base in little-endian digit order.
                #[inline] pub fn to_radix_le(&self, radix: u32) -> (Sign, Vec<u8>) {
                    (self.sign, self.data.to_radix_le(radix))
                }
                /// Returns the sign of the [`BigInt`] as a [`Sign`].
                #[inline] pub fn sign(&self) -> Sign {
                    self.sign
                }
                /// Returns the magnitude of the [`BigInt`] as a [`BigUint`].
                #[inline] pub fn magnitude(&self) -> &BigUint {
                    &self.data
                }
                /// Convert this [`BigInt`] into its [`Sign`] and [`BigUint`] magnitude,
                /// the reverse of [`BigInt::from_biguint()`].
                #[inline] pub fn into_parts(self) -> (Sign, BigUint) {
                    (self.sign, self.data)
                }
                /// Determines the fewest bits necessary to express the [`BigInt`],
                /// not including the sign.
                #[inline] pub fn bits(&self) -> u64 {
                    self.data.bits()
                }
                /// Converts this [`BigInt`] into a [`BigUint`], if it's not negative.
                #[inline] pub fn to_biguint(&self) -> Option<BigUint> {
                    match self.sign {
                        Plus => Some(self.data.clone()),
                        NoSign => Some(BigUint::ZERO),
                        Minus => None,
                    }
                }
                #[inline] pub fn checked_add(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self + v)
                }
                #[inline] pub fn checked_sub(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self - v)
                }
                #[inline] pub fn checked_mul(&self, v: &BigInt) -> Option<BigInt> {
                    Some(self * v)
                }
                #[inline] pub fn checked_div(&self, v: &BigInt) -> Option<BigInt> {
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
                pub fn modpow(&self, exponent: &Self, modulus: &Self) -> Self {
                    power::modpow(self, exponent, modulus)
                }
                /// Returns the modular multiplicative inverse if it exists, otherwise `None`.
                /// assert_eq!(BigInt::one().modinv(&m), Some(BigInt::one()));
                /// let neg1 = &m - 1u32;
                /// assert_eq!(neg1.modinv(&m), Some(neg1));
                ///
                ///
                /// let a = BigInt::from(271);
                /// let x = a.modinv(&m).unwrap();
                /// assert_eq!(x, BigInt::from(106));
                /// assert_eq!(x.modinv(&m).unwrap(), a);
                /// assert_eq!((&a * x).mod_floor(&m), BigInt::one());
                ///
                ///
                /// let b = -&a;
                /// let x = b.modinv(&m).unwrap();
                /// assert_eq!(x, BigInt::from(277));
                /// assert_eq!((&b * x).mod_floor(&m), BigInt::one());
                ///
                ///
                /// let n = -&m;
                /// let x = a.modinv(&n).unwrap();
                /// assert_eq!(x, BigInt::from(-277));
                /// assert_eq!((&a * x).mod_floor(&n), &n + 1);
                ///
                ///
                /// let x = b.modinv(&n).unwrap();
                /// assert_eq!(x, BigInt::from(-106));
                /// assert_eq!((&b * x).mod_floor(&n), &n + 1);
                /// ```
                pub fn modinv(&self, modulus: &Self) -> Option<Self> {
                    let result = self.data.modinv(&modulus.data)?;
                   
                    let (sign, mag) = match (self.is_negative(), modulus.is_negative()) {
                        (false, false) => (Plus, result),
                        (true, false) => (Plus, &modulus.data - result),
                        (false, true) => (Minus, &modulus.data - result),
                        (true, true) => (Minus, result),
                    };
                    Some(BigInt::from_biguint(sign, mag))
                }
                /// Returns the truncated principal square root of `self` --
                /// see [`::num::integers::Roots::sqrt()`].
                pub fn sqrt(&self) -> Self {
                    Roots::sqrt(self)
                }
                /// Returns the truncated principal cube root of `self` --
                /// see [`::num::integers::Roots::cbrt()`].
                pub fn cbrt(&self) -> Self {
                    Roots::cbrt(self)
                }
                /// Returns the truncated principal `n`th root of `self` --
                /// See [`::num::integers::Roots::nth_root()`].
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
                pub fn bit(&self, bit: u64) -> bool
                {
                    if self.is_negative()
                    {
                        if bit >= u64::from(::num::big::digit::BITS) * self.len() as u64
                        {
                            true
                        }
                        
                        else
                        {
                            let trailing_zeros = self.data.trailing_zeros().unwrap();
                            match Ord::cmp(&bit, &trailing_zeros) {
                                Ordering::Less => false,
                                Ordering::Equal => true,
                                Ordering::Greater => !self.data.bit(bit),
                            }
                        }
                    }
                    
                    else
                    {
                        self.data.bit(bit)
                    }
                }
                /// Sets or clears the bit in the given position, using the two's complement for negative numbers.
                pub fn set_bit(&mut self, bit: u64, value: bool)
                {
                    match self.sign {
                        Sign::Plus => self.data.set_bit(bit, value),
                        Sign::Minus => bits::set_negative_bit(self, bit, value),
                        Sign::NoSign => {
                            if value {
                                self.data.set_bit(bit, true);
                                self.sign = Sign::Plus;
                            } else {
                               
                            }
                        }
                    }
                   
                    self.normalize();
                }
            }
            
            impl ::num::traits::FromBytes for BigInt
            {
                type  Bytes = [u8];

                fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_signed_bytes_be(bytes)
                }
                fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_signed_bytes_le(bytes)
                }
            }
            
            impl ::num::traits::ToBytes for BigInt
            {
                type  Bytes = Vec<u8>;

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
            //! Randomization of big integers
            use ::
            {
                num::
                {
                    big::{ uint::biguint_from_vec, BigInt, BigUint, Sign::* },
                    integers::{ Integer },
                    traits::{ ToPrimitive, Zero },
                },
                rand::
                {
                    Rng,
                    distributions::uniform::{SampleBorrow, SampleUniform, UniformSampler},
                    prelude::Distribution,
                },
                *,
            };
            /*
            */
            /// A trait for sampling random big integers.
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
            fn gen_bits<R: Rng + ?Sized>( rng: &mut R, data: &mut [u32], rem: u64 ) 
            {
               
                rng.fill(data);
                if rem > 0 {
                    let last = data.len() - 1;
                    data[last] >>= 32 - rem;
                }
            }
            
            impl<R: Rng + ?Sized> RandBigInt for R 
            {
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
                        use ::slice;

                        let (digits, rem) = bit_size.div_rem(&32);
                        let len = (digits + (rem > 0) as u64)
                            .to_usize()
                            .expect("capacity overflow");
                        let native_digits = Integer::div_ceil(&bit_size, &64);
                        let native_len = native_digits.to_usize().expect("capacity overflow");
                        let mut data = vec![0u64; native_len];
                        unsafe {
                           
                            let ptr = data.as_mut_ptr() as *mut u32;
                            debug_assert!(native_len * 2 >= len);
                            let data = slice::from_raw_parts_mut(ptr, len);
                            gen_bits(self, data, rem);
                        }
                        #[cfg(target_endian = "big")]
                        for digit in &mut data {
                           
                            *digit = (*digit << 32) | (*digit >> 32);
                        }
                        biguint_from_vec(data)
                    }
                );

                fn gen_bigint(&mut self, bit_size: u64) -> BigInt {
                    loop {
                       
                        let biguint = self.gen_biguint(bit_size);
                       
                        let sign = if biguint.is_zero() {
                           
                           
                           
                           
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
            pub struct UniformBigUint 
            {
                base: BigUint,
                len: BigUint,
            }
            
            impl UniformSampler for UniformBigUint 
            {
                type X = BigUint;

                #[inline] fn new<B1, B2>(low_b: B1, high_b: B2) -> Self
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
                #[inline] fn new_inclusive<B1, B2>(low_b: B1, high_b: B2) -> Self
                where
                    B1: SampleBorrow<Self::X> + Sized,
                    B2: SampleBorrow<Self::X> + Sized,
                {
                    let low = low_b.borrow();
                    let high = high_b.borrow();
                    assert!(low <= high);
                    Self::new(low, high + 1u32)
                }
                #[inline] fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X {
                    &self.base + rng.gen_biguint_below(&self.len)
                }
                #[inline] fn sample_single<R: Rng + ?Sized, B1, B2>(low: B1, high: B2, rng: &mut R) -> Self::X
                where
                    B1: SampleBorrow<Self::X> + Sized,
                    B2: SampleBorrow<Self::X> + Sized,
                {
                    rng.gen_biguint_range(low.borrow(), high.borrow())
                }
            }
            
            impl SampleUniform for BigUint 
            {
                type Sampler = UniformBigUint;
            }
            /// The back-end implementing rand's [`UniformSampler`] for [`BigInt`].
            #[derive(Clone, Debug)]
            pub struct UniformBigInt 
            {
                base: BigInt,
                len: BigUint,
            }
            
            impl UniformSampler for UniformBigInt 
            {
                type X = BigInt;

                #[inline] fn new<B1, B2>(low_b: B1, high_b: B2) -> Self where
                B1: SampleBorrow<Self::X> + Sized,
                B2: SampleBorrow<Self::X> + Sized
                {
                    let low = low_b.borrow();
                    let high = high_b.borrow();
                    assert!(low < high);
                    UniformBigInt {
                        len: (high - low).into_parts().1,
                        base: low.clone(),
                    }
                }
                
                #[inline] fn new_inclusive<B1, B2>(low_b: B1, high_b: B2) -> Self where
                B1: SampleBorrow<Self::X> + Sized,
                B2: SampleBorrow<Self::X> + Sized
                {
                    let low = low_b.borrow();
                    let high = high_b.borrow();
                    assert!(low <= high);
                    Self::new(low, high + 1u32)
                }

                #[inline] fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Self::X 
                {
                    &self.base + BigInt::from(rng.gen_biguint_below(&self.len))
                }

                #[inline] fn sample_single<R: Rng + ?Sized, B1, B2>(low: B1, high: B2, rng: &mut R) -> Self::X where
                B1: SampleBorrow<Self::X> + Sized,
                B2: SampleBorrow<Self::X> + Sized
                {
                    rng.gen_bigint_range(low.borrow(), high.borrow())
                }
            }
            
            impl SampleUniform for BigInt 
            {
                type Sampler = UniformBigInt;
            }
            /// A random distribution for [`BigUint`] and [`BigInt`] values of a particular bit size.
            #[derive(Clone, Copy, Debug)]
            pub struct RandomBits 
            {
                bits: u64,
            }
            
            impl RandomBits 
            {
                #[inline] pub fn new(bits: u64) -> RandomBits {
                    RandomBits { bits }
                }
            }
            
            impl Distribution<BigUint> for RandomBits 
            {
                #[inline] fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> BigUint {
                    rng.gen_biguint(self.bits)
                }
            }
            
            impl Distribution<BigInt> for RandomBits 
            {
                #[inline] fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> BigInt {
                    rng.gen_bigint(self.bits)
                }
            }
        }

        pub mod uint
        {
            use ::
            {
                cmp::{ self, Ordering },
                default::{ Default },
                num::
                {
                    big::digit::{ self, BigDigit },
                    integers::{ Integer, Roots },
                    traits::{ ConstZero, Num, One, Pow, ToPrimitive, Unsigned, Zero },
                },
                string::{ String },
                vec::{ Vec },
                *,
            };
            /*
            */            
            pub mod addition
            {
                /*!
                */
                use ::
                {
                    iter::{ Sum },
                    num::
                    {
                        big::
                        {
                            digit::{self, BigDigit}, UsizePromotion
                        },
                        traits::CheckedAdd,
                    },
                    ops::{ Add, AddAssign },
                    *,
                };

                use super::{BigUint, IntDigits};
                /*
                */
                use ::arch::x86_64 as arch;
                
                cfg_64!(
                    #[inline] fn adc(carry: u8, a: u64, b: u64, out: &mut u64) -> u8 {
                       
                       
                        unsafe { arch::_addcarry_u64(carry, a, b, out) }
                    }
                );
                /// Two argument addition of raw slices, `a += b`, returning the carry.
                #[inline] pub fn __add2(a: &mut [BigDigit], b: &[BigDigit]) -> BigDigit {
                    debug_assert!(a.len() >= b.len());

                    let mut carry = 0;
                    let (a_lo, a_hi) = a.split_at_mut(b.len());

                    for (a, b) in a_lo.iter_mut().zip(b) {
                        carry = adc(carry, *a, *b, a);
                    }
                    if carry != 0 {
                        for a in a_hi {
                            carry = adc(carry, *a, 0, a);
                            if carry == 0 {
                                break;
                            }
                        }
                    }
                    carry as BigDigit
                }
                /// Two argument addition of raw slices:
                /// a += b
                ///
                /// The caller _must_ ensure that a is big enough to store the result - typically this means
                /// resizing a to max(a.len(), b.len()) + 1, to fit a possible carry.
                pub(super) fn add2(a: &mut [BigDigit], b: &[BigDigit]) {
                    let carry = __add2(a, b);

                    debug_assert!(carry == 0);
                }
                forward_all_binop_to_val_ref_commutative!(impl Add for BigUint, add);
                forward_val_assign!(impl AddAssign for BigUint, add_assign);

                impl Add<&BigUint> for BigUint
                {
                    type Output = BigUint;

                    fn add(mut self, other: &BigUint) -> BigUint {
                        self += other;
                        self
                    }
                }
                impl AddAssign<&BigUint> for BigUint
                {
                    #[inline] fn add_assign(&mut self, other: &BigUint) {
                        let self_len = self.data.len();
                        let carry = if self_len < other.data.len() {
                            let lo_carry = __add2(&mut self.data[..], &other.data[..self_len]);
                            self.data.extend_from_slice(&other.data[self_len..]);
                            __add2(&mut self.data[self_len..], &[lo_carry])
                        } else {
                            __add2(&mut self.data[..], &other.data[..])
                        };
                        if carry != 0 {
                            self.data.push(carry);
                        }
                    }
                }
                promote_unsigned_scalars!(impl Add for BigUint, add);
                promote_unsigned_scalars_assign!(impl AddAssign for BigUint, add_assign);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u32> for BigUint, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u64> for BigUint, add);
                forward_all_scalar_binop_to_val_val_commutative!(impl Add<u128> for BigUint, add);

                impl Add<u32> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn add(mut self, other: u32) -> BigUint {
                        self += other;
                        self
                    }
                }
                impl AddAssign<u32> for BigUint
                {
                    #[inline] fn add_assign(&mut self, other: u32) {
                        if other != 0 {
                            if self.data.is_empty() {
                                self.data.push(0);
                            }
                            let carry = __add2(&mut self.data, &[other as BigDigit]);
                            if carry != 0 {
                                self.data.push(carry);
                            }
                        }
                    }
                }
                impl Add<u64> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn add(mut self, other: u64) -> BigUint {
                        self += other;
                        self
                    }
                }
                impl AddAssign<u64> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn add_assign(&mut self, other: u64) {
                            let (hi, lo) = ::num::big::digit::from_doublebigdigit(other);
                            if hi == 0 {
                                *self += lo;
                            } else {
                                while self.data.len() < 2 {
                                    self.data.push(0);
                                }
                                let carry = __add2(&mut self.data, &[lo, hi]);
                                if carry != 0 {
                                    self.data.push(carry);
                                }
                            }
                        }
                        #[inline]
                        fn add_assign(&mut self, other: u64) {
                            if other != 0 {
                                if self.data.is_empty() {
                                    self.data.push(0);
                                }
                                let carry = __add2(&mut self.data, &[other as BigDigit]);
                                if carry != 0 {
                                    self.data.push(carry);
                                }
                            }
                        }
                    );
                }
                impl Add<u128> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn add(mut self, other: u128) -> BigUint {
                        self += other;
                        self
                    }
                }
                impl AddAssign<u128> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn add_assign(&mut self, other: u128) {
                            if other <= u128::from(u64::MAX) {
                                *self += other as u64
                            } else {
                                let (a, b, c, d) = super::u32_from_u128(other);
                                let carry = if a > 0 {
                                    while self.data.len() < 4 {
                                        self.data.push(0);
                                    }
                                    __add2(&mut self.data, &[d, c, b, a])
                                } else {
                                    debug_assert!(b > 0);
                                    while self.data.len() < 3 {
                                        self.data.push(0);
                                    }
                                    __add2(&mut self.data, &[d, c, b])
                                };

                                if carry != 0 {
                                    self.data.push(carry);
                                }
                            }
                        }
                        #[inline]
                        fn add_assign(&mut self, other: u128) {
                            let (hi, lo) = ::num::big::digit::from_doublebigdigit(other);
                            if hi == 0 {
                                *self += lo;
                            } else {
                                while self.data.len() < 2 {
                                    self.data.push(0);
                                }
                                let carry = __add2(&mut self.data, &[lo, hi]);
                                if carry != 0 {
                                    self.data.push(carry);
                                }
                            }
                        }
                    );
                }
                impl CheckedAdd for BigUint
                {
                    #[inline] fn checked_add(&self, v: &BigUint) -> Option<BigUint> {
                        Some(self.add(v))
                    }
                }
                impl_sum_iter_type!(BigUint);

            }
            pub mod division
            {
                /*!
                */
                use ::
                {
                    cmp::Ordering::{Equal, Greater, Less},
                    num::
                    {
                        big::
                        {
                            digit::{self, BigDigit, DoubleBigDigit}, UsizePromotion
                        },
                        integers::Integer,
                        traits::{CheckedDiv, CheckedEuclid, Euclid, One, ToPrimitive, Zero},
                    },
                    ops::{Div, DivAssign, Rem, RemAssign},
                    *,
                };
                
                use super::addition::__add2;
                use super::{cmp_slice, BigUint};
                /*
                */
                pub const FAST_DIV_WIDE: bool = true;
                /// Divide a two digit numerator by a one digit divisor, returns quotient and remainder.
                #[inline] fn div_wide(hi: BigDigit, lo: BigDigit, divisor: BigDigit) -> (BigDigit, BigDigit)
                {
                   
                   
                   
                    debug_assert!(hi < divisor);

                   
                   
                   
                   
                    unsafe {
                        let (div, rem);

                        cfg_digit!(
                            macro_rules! div {
                                () => {
                                    "div {0:e}"
                                };
                            }
                            macro_rules! div {
                                () => {
                                    "div {0:r}"
                                };
                            }
                        );

                        ::arch::asm!(
                            div!(),
                            in(reg) divisor,
                            inout("dx") hi => rem,
                            inout("ax") lo => div,
                            options(pure, nomem, nostack),
                        );

                        (div, rem)
                    }
                }
                /// For small divisors, we can divide without promoting to `DoubleBigDigit`
                /// by using half-size pieces of digit, like long-division.
                #[inline] fn div_half(rem: BigDigit, digit: BigDigit, divisor: BigDigit) -> (BigDigit, BigDigit)
                {
                    use ::num::big::digit::{HALF, HALF_BITS};
                    debug_assert!(rem < divisor && divisor <= HALF);
                    let (hi, rem) = ((rem << HALF_BITS) | (digit >> HALF_BITS)).div_rem(&divisor);
                    let (lo, rem) = ((rem << HALF_BITS) | (digit & HALF)).div_rem(&divisor);
                    ((hi << HALF_BITS) | lo, rem)
                }
                #[inline] pub fn div_rem_digit(mut a: BigUint, b: BigDigit) -> (BigUint, BigDigit)
                {
                    if b == 0 {
                        panic!("attempt to divide by zero")
                    }
                    let mut rem = 0;

                    if !FAST_DIV_WIDE && b <= ::num::big::digit::HALF {
                        for d in a.data.iter_mut().rev() {
                            let (q, r) = div_half(rem, *d, b);
                            *d = q;
                            rem = r;
                        }
                    } else {
                        for d in a.data.iter_mut().rev() {
                            let (q, r) = div_wide(rem, *d, b);
                            *d = q;
                            rem = r;
                        }
                    }
                    (a.normalized(), rem)
                }
                #[inline] fn rem_digit(a: &BigUint, b: BigDigit) -> BigDigit
                {
                    if b == 0 {
                        panic!("attempt to divide by zero")
                    }
                    let mut rem = 0;

                    if !FAST_DIV_WIDE && b <= ::num::big::digit::HALF {
                        for &digit in a.data.iter().rev() {
                            let (_, r) = div_half(rem, digit, b);
                            rem = r;
                        }
                    } else {
                        for &digit in a.data.iter().rev() {
                            let (_, r) = div_wide(rem, digit, b);
                            rem = r;
                        }
                    }
                    rem
                }
                /// Subtract a multiple.
                fn sub_mul_digit_same_len(a: &mut [BigDigit], b: &[BigDigit], c: BigDigit) -> BigDigit
                {
                    debug_assert!(a.len() == b.len());

                   
                   
                    let mut offset_carry = ::num::big::digit::MAX;

                    for (x, y) in a.iter_mut().zip(b) {
                       
                       
                       
                       
                        let offset_sum = ::num::big::digit::to_doublebigdigit(::num::big::digit::MAX, *x)
                            - ::num::big::digit::MAX as DoubleBigDigit
                            + offset_carry as DoubleBigDigit
                            - *y as DoubleBigDigit * c as DoubleBigDigit;

                        let (new_offset_carry, new_x) = ::num::big::digit::from_doublebigdigit(offset_sum);
                        offset_carry = new_offset_carry;
                        *x = new_x;
                    }
                   
                    ::num::big::digit::MAX - offset_carry
                }
                fn div_rem(mut u: BigUint, mut d: BigUint) -> (BigUint, BigUint)
                {
                    if d.is_zero() {
                        panic!("attempt to divide by zero")
                    }
                    if u.is_zero() {
                        return (BigUint::ZERO, BigUint::ZERO);
                    }
                    if d.data.len() == 1 {
                        if d.data == [1] {
                            return (u, BigUint::ZERO);
                        }
                        let (div, rem) = div_rem_digit(u, d.data[0]);
                       
                        d.data.clear();
                        d += rem;
                        return (div, d);
                    }
                   
                    match u.cmp(&d) {
                        Less => return (BigUint::ZERO, u),
                        Equal => {
                            u.set_one();
                            return (u, BigUint::ZERO);
                        }
                        Greater => {}
                    }
                   
                    //
                   
                   
                   
                    //
                    let shift = d.data.last().unwrap().leading_zeros() as usize;

                    if shift == 0 {
                       
                        div_rem_core(u, &d.data)
                    } else {
                        let (q, r) = div_rem_core(u << shift, &(d << shift).data);
                       
                        (q, r >> shift)
                    }
                }
                pub(super) fn div_rem_ref(u: &BigUint, d: &BigUint) -> (BigUint, BigUint)
                {
                    if d.is_zero() {
                        panic!("attempt to divide by zero")
                    }
                    if u.is_zero() {
                        return (BigUint::ZERO, BigUint::ZERO);
                    }
                    if d.data.len() == 1 {
                        if d.data == [1] {
                            return (u.clone(), BigUint::ZERO);
                        }
                        let (div, rem) = div_rem_digit(u.clone(), d.data[0]);
                        return (div, rem.into());
                    }
                   
                    match u.cmp(d) {
                        Less => return (BigUint::ZERO, u.clone()),
                        Equal => return (One::one(), BigUint::ZERO),
                        Greater => {}
                    }
                   
                    //
                   
                   
                   
                    //
                    let shift = d.data.last().unwrap().leading_zeros() as usize;

                    if shift == 0 {
                       
                        div_rem_core(u.clone(), &d.data)
                    } else {
                        let (q, r) = div_rem_core(u << shift, &(d << shift).data);
                       
                        (q, r >> shift)
                    }
                }
                /// An implementation of the base division algorithm.
                fn div_rem_core(mut a: BigUint, b: &[BigDigit]) -> (BigUint, BigUint)
                {
                    debug_assert!(a.data.len() >= b.len() && b.len() > 1);
                    debug_assert!(b.last().unwrap().leading_zeros() == 0);

                   
                   
                    //
                   
                   
                    //
                   
                    //
                   
                   
                   
                   
                    //
                   
                   
                   

                   
                    let mut a0 = 0;

                   
                    let b0 = b[b.len() - 1];
                    let b1 = b[b.len() - 2];

                    let q_len = a.data.len() - b.len() + 1;
                    let mut q = BigUint {
                        data: vec![0; q_len],
                    };

                    for j in (0..q_len).rev() {
                        debug_assert!(a.data.len() == b.len() + j);

                        let a1 = *a.data.last().unwrap();
                        let a2 = a.data[a.data.len() - 2];

                       
                       
                        let (mut q0, mut r) = if a0 < b0 {
                            let (q0, r) = div_wide(a0, a1, b0);
                            (q0, r as DoubleBigDigit)
                        } else {
                            debug_assert!(a0 == b0);
                           
                           
                            (::num::big::digit::MAX, a0 as DoubleBigDigit + a1 as DoubleBigDigit)
                        };

                       
                        //
                       
                       
                        //
                       
                       
                       
                        while r <= ::num::big::digit::MAX as DoubleBigDigit
                            && ::num::big::digit::to_doublebigdigit(r as BigDigit, a2)
                                < q0 as DoubleBigDigit * b1 as DoubleBigDigit
                        {
                            q0 -= 1;
                            r += b0 as DoubleBigDigit;
                        }
                       
                       

                        let mut borrow = sub_mul_digit_same_len(&mut a.data[j..], b, q0);
                        if borrow > a0 {
                           
                            q0 -= 1;
                            borrow -= __add2(&mut a.data[j..], b);
                        }
                       
                        debug_assert!(borrow == a0);

                        q.data[j] = q0;

                       
                        a0 = a.data.pop().unwrap();
                    }
                    a.data.push(a0);
                    a.normalize();

                    debug_assert_eq!(cmp_slice(&a.data, b), Less);

                    (q.normalized(), a)
                }
                forward_val_ref_binop_big!(impl Div for BigUint, div);
                forward_ref_val_binop_big!(impl Div for BigUint, div);
                forward_val_assign!(impl DivAssign for BigUint, div_assign);

                impl Div<BigUint> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn div(self, other: BigUint) -> BigUint {
                        let (q, _) = div_rem(self, other);
                        q
                    }
                }
                impl Div<&BigUint> for &BigUint
                {
                    type Output = BigUint;

                    #[inline] fn div(self, other: &BigUint) -> BigUint {
                        let (q, _) = self.div_rem(other);
                        q
                    }
                }
                impl DivAssign<&BigUint> for BigUint
                {
                    #[inline] fn div_assign(&mut self, other: &BigUint) {
                        *self = &*self / other;
                    }
                }
                promote_unsigned_scalars!(impl Div for BigUint, div);
                promote_unsigned_scalars_assign!(impl DivAssign for BigUint, div_assign);
                forward_all_scalar_binop_to_val_val!(impl Div<u32> for BigUint, div);
                forward_all_scalar_binop_to_val_val!(impl Div<u64> for BigUint, div);
                forward_all_scalar_binop_to_val_val!(impl Div<u128> for BigUint, div);

                impl Div<u32> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn div(self, other: u32) -> BigUint {
                        let (q, _) = div_rem_digit(self, other as BigDigit);
                        q
                    }
                }
                impl DivAssign<u32> for BigUint
                {
                    #[inline] fn div_assign(&mut self, other: u32) {
                        *self = &*self / other;
                    }
                }
                impl Div<BigUint> for u32 {
                    type Output = BigUint;

                    #[inline] fn div(self, other: BigUint) -> BigUint {
                        match other.data.len() {
                            0 => panic!("attempt to divide by zero"),
                            1 => From::from(self as BigDigit / other.data[0]),
                            _ => BigUint::ZERO,
                        }
                    }
                }
                impl Div<u64> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn div(self, other: u64) -> BigUint {
                        let (q, _) = div_rem(self, From::from(other));
                        q
                    }
                }
                impl DivAssign<u64> for BigUint
                {
                    #[inline] fn div_assign(&mut self, other: u64) {
                       
                        let temp = mem::replace(self, Self::ZERO);
                        *self = temp / other;
                    }
                }
                impl Div<BigUint> for u64 {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn div(self, other: BigUint) -> BigUint {
                            match other.data.len() {
                                0 => panic!("attempt to divide by zero"),
                                1 => From::from(self / u64::from(other.data[0])),
                                2 => From::from(self / ::num::big::digit::to_doublebigdigit(other.data[1], other.data[0])),
                                _ => BigUint::ZERO,
                            }
                        }
                        #[inline]
                        fn div(self, other: BigUint) -> BigUint {
                            match other.data.len() {
                                0 => panic!("attempt to divide by zero"),
                                1 => From::from(self / other.data[0]),
                                _ => BigUint::ZERO,
                            }
                        }
                    );
                }
                impl Div<u128> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn div(self, other: u128) -> BigUint {
                        let (q, _) = div_rem(self, From::from(other));
                        q
                    }
                }
                impl DivAssign<u128> for BigUint
                {
                    #[inline] fn div_assign(&mut self, other: u128) {
                        *self = &*self / other;
                    }
                }
                impl Div<BigUint> for u128 {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn div(self, other: BigUint) -> BigUint {
                            use super::u32_to_u128;
                            match other.data.len() {
                                0 => panic!("attempt to divide by zero"),
                                1 => From::from(self / u128::from(other.data[0])),
                                2 => From::from(
                                    self / u128::from(::num::big::digit::to_doublebigdigit(other.data[1], other.data[0])),
                                ),
                                3 => From::from(self / u32_to_u128(0, other.data[2], other.data[1], other.data[0])),
                                4 => From::from(
                                    self / u32_to_u128(other.data[3], other.data[2], other.data[1], other.data[0]),
                                ),
                                _ => BigUint::ZERO,
                            }
                        }
                        #[inline]
                        fn div(self, other: BigUint) -> BigUint {
                            match other.data.len() {
                                0 => panic!("attempt to divide by zero"),
                                1 => From::from(self / other.data[0] as u128),
                                2 => From::from(self / ::num::big::digit::to_doublebigdigit(other.data[1], other.data[0])),
                                _ => BigUint::ZERO,
                            }
                        }
                    );
                }
                forward_val_ref_binop_big!(impl Rem for BigUint, rem);
                forward_ref_val_binop_big!(impl Rem for BigUint, rem);
                forward_val_assign!(impl RemAssign for BigUint, rem_assign);

                impl Rem<BigUint> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: BigUint) -> BigUint {
                        if let Some(other) = other.to_u32() {
                            &self % other
                        } else {
                            let (_, r) = div_rem(self, other);
                            r
                        }
                    }
                }
                impl Rem<&BigUint> for &BigUint
                {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: &BigUint) -> BigUint {
                        if let Some(other) = other.to_u32() {
                            self % other
                        } else {
                            let (_, r) = self.div_rem(other);
                            r
                        }
                    }
                }
                impl RemAssign<&BigUint> for BigUint
                {
                    #[inline] fn rem_assign(&mut self, other: &BigUint) {
                        *self = &*self % other;
                    }
                }
                promote_unsigned_scalars!(impl Rem for BigUint, rem);
                promote_unsigned_scalars_assign!(impl RemAssign for BigUint, rem_assign);
                forward_all_scalar_binop_to_ref_val!(impl Rem<u32> for BigUint, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<u64> for BigUint, rem);
                forward_all_scalar_binop_to_val_val!(impl Rem<u128> for BigUint, rem);

                impl Rem<u32> for &BigUint
                {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: u32) -> BigUint {
                        rem_digit(self, other as BigDigit).into()
                    }
                }
                impl RemAssign<u32> for BigUint
                {
                    #[inline] fn rem_assign(&mut self, other: u32) {
                        *self = &*self % other;
                    }
                }
                impl Rem<&BigUint> for u32 {
                    type Output = BigUint;

                    #[inline] fn rem(mut self, other: &BigUint) -> BigUint {
                        self %= other;
                        From::from(self)
                    }
                }
                macro_rules! impl_rem_assign_scalar {
                    ($scalar:ty, $to_scalar:ident) => {
                        forward_val_assign_scalar!(impl RemAssign for BigUint, $scalar, rem_assign);
                        impl RemAssign<&BigUint> for $scalar {
                            #[inline]
                            fn rem_assign(&mut self, other: &BigUint) {
                                *self = match other.$to_scalar() {
                                    None => *self,
                                    Some(0) => panic!("attempt to divide by zero"),
                                    Some(v) => *self % v
                                };
                            }
                        }
                    }
                }
               
                impl_rem_assign_scalar!(u128, to_u128);
                impl_rem_assign_scalar!(usize, to_usize);
                impl_rem_assign_scalar!(u64, to_u64);
                impl_rem_assign_scalar!(u32, to_u32);
                impl_rem_assign_scalar!(u16, to_u16);
                impl_rem_assign_scalar!(u8, to_u8);
                impl_rem_assign_scalar!(i128, to_i128);
                impl_rem_assign_scalar!(isize, to_isize);
                impl_rem_assign_scalar!(i64, to_i64);
                impl_rem_assign_scalar!(i32, to_i32);
                impl_rem_assign_scalar!(i16, to_i16);
                impl_rem_assign_scalar!(i8, to_i8);

                impl Rem<u64> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: u64) -> BigUint {
                        let (_, r) = div_rem(self, From::from(other));
                        r
                    }
                }
                impl RemAssign<u64> for BigUint
                {
                    #[inline] fn rem_assign(&mut self, other: u64) {
                        *self = &*self % other;
                    }
                }
                impl Rem<BigUint> for u64 {
                    type Output = BigUint;

                    #[inline] fn rem(mut self, other: BigUint) -> BigUint {
                        self %= other;
                        From::from(self)
                    }
                }
                impl Rem<u128> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn rem(self, other: u128) -> BigUint {
                        let (_, r) = div_rem(self, From::from(other));
                        r
                    }
                }
                impl RemAssign<u128> for BigUint
                {
                    #[inline] fn rem_assign(&mut self, other: u128) {
                        *self = &*self % other;
                    }
                }
                impl Rem<BigUint> for u128 {
                    type Output = BigUint;

                    #[inline] fn rem(mut self, other: BigUint) -> BigUint {
                        self %= other;
                        From::from(self)
                    }
                }
                impl CheckedDiv for BigUint
                {
                    #[inline] fn checked_div(&self, v: &BigUint) -> Option<BigUint> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.div(v))
                    }
                }
                impl CheckedEuclid for BigUint
                {
                    #[inline] fn checked_div_euclid(&self, v: &BigUint) -> Option<BigUint> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.div_euclid(v))
                    }
                    #[inline] fn checked_rem_euclid(&self, v: &BigUint) -> Option<BigUint> {
                        if v.is_zero() {
                            return None;
                        }
                        Some(self.rem_euclid(v))
                    }
                    fn checked_div_rem_euclid(&self, v: &Self) -> Option<(Self, Self)> {
                        Some(self.div_rem_euclid(v))
                    }
                }
                impl Euclid for BigUint
                {
                    #[inline] fn div_euclid(&self, v: &BigUint) -> BigUint {
                       
                        self / v
                    }
                    #[inline] fn rem_euclid(&self, v: &BigUint) -> BigUint {
                       
                        self % v
                    }
                    fn div_rem_euclid(&self, v: &Self) -> (Self, Self) {
                       
                        self.div_rem(v)
                    }
                }
            }
            pub mod multiplication
            {
                /*!
                */
                use ::
                {
                    cmp::{ Ordering },
                    iter::{ Product },
                    num::
                    {
                        big::
                        {
                            digit::{self, BigDigit, DoubleBigDigit},
                            Sign::{self, Minus, NoSign, Plus},
                            {BigInt, UsizePromotion},
                        },
                        traits::{ CheckedMul, FromPrimitive, One, Zero },
                    },
                    ops::{Mul, MulAssign},
                    *,
                };
                use super::addition::{__add2, add2};
                use super::subtraction::sub2;
                use super::{biguint_from_vec, cmp_slice, BigUint, IntDigits};
                /*
                */
                #[inline] pub fn mac_with_carry(
                    a: BigDigit,
                    b: BigDigit,
                    c: BigDigit,
                    acc: &mut DoubleBigDigit,
                ) -> BigDigit {
                    *acc += DoubleBigDigit::from(a);
                    *acc += DoubleBigDigit::from(b) * DoubleBigDigit::from(c);
                    let lo = *acc as BigDigit;
                    *acc >>= ::num::big::digit::BITS;
                    lo
                }
                #[inline] fn mul_with_carry(a: BigDigit, b: BigDigit, acc: &mut DoubleBigDigit) -> BigDigit {
                    *acc += DoubleBigDigit::from(a) * DoubleBigDigit::from(b);
                    let lo = *acc as BigDigit;
                    *acc >>= ::num::big::digit::BITS;
                    lo
                }
                /// Three argument multiply accumulate:
                /// acc += b * c
                fn mac_digit(acc: &mut [BigDigit], b: &[BigDigit], c: BigDigit) {
                    if c == 0 {
                        return;
                    }
                    let mut carry = 0;
                    let (a_lo, a_hi) = acc.split_at_mut(b.len());

                    for (a, &b) in a_lo.iter_mut().zip(b) {
                        *a = mac_with_carry(*a, b, c, &mut carry);
                    }
                    let (carry_hi, carry_lo) = ::num::big::digit::from_doublebigdigit(carry);

                    let final_carry = if carry_hi == 0 {
                        __add2(a_hi, &[carry_lo])
                    } else {
                        __add2(a_hi, &[carry_hi, carry_lo])
                    };
                    assert_eq!(final_carry, 0, "carry overflow during multiplication!");
                }
                fn bigint_from_slice(slice: &[BigDigit]) -> BigInt {
                    BigInt::from(biguint_from_vec(slice.to_vec()))
                }
                /// Three argument multiply accumulate:
                /// acc += b * c
                #[allow(clippy::many_single_char_names)]
                fn mac3(mut acc: &mut [BigDigit], mut b: &[BigDigit], mut c: &[BigDigit]) {
                   
                    if let Some(&0) = b.first() {
                        if let Some(nz) = b.iter().position(|&d| d != 0) {
                            b = &b[nz..];
                            acc = &mut acc[nz..];
                        } else {
                            return;
                        }
                    }
                    if let Some(&0) = c.first() {
                        if let Some(nz) = c.iter().position(|&d| d != 0) {
                            c = &c[nz..];
                            acc = &mut acc[nz..];
                        } else {
                            return;
                        }
                    }
                    let acc = acc;
                    let (x, y) = if b.len() < c.len() { (b, c) } else { (c, b) };

                   
                    //
                   
                   
                   
                   
                   
                   
                    //
                   
                   

                    if x.len() <= 32 {
                       
                        for (i, xi) in x.iter().enumerate() {
                            mac_digit(&mut acc[i..], y, *xi);
                        }
                    } else if x.len() * 2 <= y.len() {
                       
                        //
                       
                       
                       
                        //
                       
                       
                       
                        //
                       
                        //
                       
                       
                       
                       
                        //
                       
                        //
                       
                       
                       
                        //
                       
                        //
                       
                        //
                       
                        //
                       
                       
                        //
                       
                        //
                       
                       
                        //
                       
                       
                       
                        //
                       
                       
                       
                        //
                       
                        //
                       
                       
                       
                       
                       
                        let m2 = y.len() / 2;
                        let (low2, high2) = y.split_at(m2);

                       
                        mac3(acc, x, low2);
                        mac3(&mut acc[m2..], x, high2);
                    } else if x.len() <= 256 {
                       
                        //
                       
                       
                        //
                       
                       
                        //
                       
                       
                        //
                       
                        //
                       
                       
                       
                       
                        //
                       
                        //
                       
                       
                       
                        //
                       
                        //
                       
                        //
                       
                        //
                       
                        //
                       
                        //
                       
                        //
                       
                        //
                       
                        //
                       
                       
                       
                        //
                       
                        //
                       
                       
                       
                        //
                       
                       
                       
                        //
                       
                       
                       
                        //
                       
                       

                       
                       
                        let b = x.len() / 2;
                        let (x0, x1) = x.split_at(b);
                        let (y0, y1) = y.split_at(b);

                       
                       
                        let len = x1.len() + y1.len() + 1;
                        let mut p = BigUint { data: vec![0; len] };

                       
                        mac3(&mut p.data, x1, y1);

                       
                        p.normalize();

                        add2(&mut acc[b..], &p.data);
                        add2(&mut acc[b * 2..], &p.data);

                       
                        p.data.truncate(0);
                        p.data.resize(len, 0);

                       
                        mac3(&mut p.data, x0, y0);
                        p.normalize();

                        add2(acc, &p.data);
                        add2(&mut acc[b..], &p.data);

                       
                       
                        let (j0_sign, j0) = sub_sign(x1, x0);
                        let (j1_sign, j1) = sub_sign(y1, y0);

                        match j0_sign * j1_sign {
                            Plus => {
                                p.data.truncate(0);
                                p.data.resize(len, 0);

                                mac3(&mut p.data, &j0.data, &j1.data);
                                p.normalize();

                                sub2(&mut acc[b..], &p.data);
                            }
                            Minus => {
                                mac3(&mut acc[b..], &j0.data, &j1.data);
                            }
                            NoSign => (),
                        }
                    } else {
                       
                        //
                       
                       
                        //
                       
                       
                       
                        let i = y.len() / 3 + 1;

                        let x0_len = Ord::min(x.len(), i);
                        let x1_len = Ord::min(x.len() - x0_len, i);

                        let y0_len = i;
                        let y1_len = Ord::min(y.len() - y0_len, i);

                       
                       
                       
                        //
                       
                        let x0 = bigint_from_slice(&x[..x0_len]);
                        let x1 = bigint_from_slice(&x[x0_len..x0_len + x1_len]);
                        let x2 = bigint_from_slice(&x[x0_len + x1_len..]);

                       
                        let y0 = bigint_from_slice(&y[..y0_len]);
                        let y1 = bigint_from_slice(&y[y0_len..y0_len + y1_len]);
                        let y2 = bigint_from_slice(&y[y0_len + y1_len..]);

                       
                        //
                       
                        //
                       
                        //
                       
                       
                       
                       
                        //
                       
                       
                        //
                       
                        //
                       
                        //
                       
                       
                       
                       
                       

                       
                        let p = &x0 + &x2;

                       
                        let q = &y0 + &y2;

                       
                        let p2 = &p - &x1;

                       
                        let q2 = &q - &y1;

                       
                        let r0 = &x0 * &y0;

                       
                        let r4 = &x2 * &y2;

                       
                        let r1 = (p + x1) * (q + y1);

                       
                        let r2 = &p2 * &q2;

                       
                        let r3 = ((p2 + x2) * 2 - x0) * ((q2 + y2) * 2 - y0);

                       
                        //
                       
                       
                       
                       
                       
                        //
                       
                       
                        //
                       
                       
                       
                       
                       
                        //
                       
                       
                        let mut comp3: BigInt = (r3 - &r1) / 3u32;
                        let mut comp1: BigInt = (r1 - &r2) >> 1;
                        let mut comp2: BigInt = r2 - &r0;
                        comp3 = ((&comp2 - comp3) >> 1) + (&r4 << 1);
                        comp2 += &comp1 - &r4;
                        comp1 -= &comp3;

                       
                        //
                       
                        //
                       
                       
                       
                       
                       
                       
                       
                       
                        //
                       
                        for (j, result) in [&r0, &comp1, &comp2, &comp3, &r4].iter().enumerate().rev() {
                            match result.sign() {
                                Plus => add2(&mut acc[i * j..], result.digits()),
                                Minus => sub2(&mut acc[i * j..], result.digits()),
                                NoSign => {}
                            }
                        }
                    }
                }
                fn mul3(x: &[BigDigit], y: &[BigDigit]) -> BigUint {
                    let len = x.len() + y.len() + 1;
                    let mut prod = BigUint { data: vec![0; len] };

                    mac3(&mut prod.data, x, y);
                    prod.normalized()
                }
                fn scalar_mul(a: &mut BigUint, b: BigDigit) {
                    match b {
                        0 => a.set_zero(),
                        1 => {}
                        _ => {
                            if b.is_power_of_two() {
                                *a <<= b.trailing_zeros();
                            } else {
                                let mut carry = 0;
                                for a in a.data.iter_mut() {
                                    *a = mul_with_carry(*a, b, &mut carry);
                                }
                                if carry != 0 {
                                    a.data.push(carry as BigDigit);
                                }
                            }
                        }
                    }
                }
                fn sub_sign(mut a: &[BigDigit], mut b: &[BigDigit]) -> (Sign, BigUint) {
                   
                    if let Some(&0) = a.last() {
                        a = &a[..a.iter().rposition(|&x| x != 0).map_or(0, |i| i + 1)];
                    }
                    if let Some(&0) = b.last() {
                        b = &b[..b.iter().rposition(|&x| x != 0).map_or(0, |i| i + 1)];
                    }
                    match cmp_slice(a, b) {
                        Ordering::Greater => {
                            let mut a = a.to_vec();
                            sub2(&mut a, b);
                            (Plus, biguint_from_vec(a))
                        }
                        Ordering::Less => {
                            let mut b = b.to_vec();
                            sub2(&mut b, a);
                            (Minus, biguint_from_vec(b))
                        }
                        Ordering::Equal => (NoSign, BigUint::ZERO),
                    }
                }
                macro_rules! impl_mul {
                    ($(impl Mul<$Other:ty> for $Self:ty;)*) => {$(
                        impl Mul<$Other> for $Self {
                            type Output = BigUint;

                            #[inline]
                            fn mul(self, other: $Other) -> BigUint {
                                match (&*self.data, &*other.data) {
                                   
                                    (&[], _) | (_, &[]) => BigUint::ZERO,
                                   
                                    (_, &[digit]) => self * digit,
                                    (&[digit], _) => other * digit,
                                   
                                    (x, y) => mul3(x, y),
                                }
                            }
                        }
                    )*}
                }
                impl_mul! {
                    impl Mul<BigUint> for BigUint;
                    impl Mul<BigUint> for &BigUint;
                    impl Mul<&BigUint> for BigUint;
                    impl Mul<&BigUint> for &BigUint;
                }
                macro_rules! impl_mul_assign {
                    ($(impl MulAssign<$Other:ty> for BigUint;)*) => {$(
                        impl MulAssign<$Other> for BigUint {
                            #[inline]
                            fn mul_assign(&mut self, other: $Other) {
                                match (&*self.data, &*other.data) {
                                   
                                    (&[], _) => {},
                                    (_, &[]) => self.set_zero(),
                                   
                                    (_, &[digit]) => *self *= digit,
                                    (&[digit], _) => *self = other * digit,
                                   
                                    (x, y) => *self = mul3(x, y),
                                }
                            }
                        }
                    )*}
                }
                impl_mul_assign! {
                    impl MulAssign<BigUint> for BigUint;
                    impl MulAssign<&BigUint> for BigUint;
                }
                promote_unsigned_scalars!(impl Mul for BigUint, mul);
                promote_unsigned_scalars_assign!(impl MulAssign for BigUint, mul_assign);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u32> for BigUint, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u64> for BigUint, mul);
                forward_all_scalar_binop_to_val_val_commutative!(impl Mul<u128> for BigUint, mul);

                impl Mul<u32> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn mul(mut self, other: u32) -> BigUint {
                        self *= other;
                        self
                    }
                }
                impl MulAssign<u32> for BigUint
                {
                    #[inline] fn mul_assign(&mut self, other: u32) {
                        scalar_mul(self, other as BigDigit);
                    }
                }
                impl Mul<u64> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn mul(mut self, other: u64) -> BigUint {
                        self *= other;
                        self
                    }
                }
                impl MulAssign<u64> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn mul_assign(&mut self, other: u64) {
                            if let Some(other) = BigDigit::from_u64(other) {
                                scalar_mul(self, other);
                            } else {
                                let (hi, lo) = ::num::big::digit::from_doublebigdigit(other);
                                *self = mul3(&self.data, &[lo, hi]);
                            }
                        }
                        #[inline]
                        fn mul_assign(&mut self, other: u64) {
                            scalar_mul(self, other);
                        }
                    );
                }
                impl Mul<u128> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn mul(mut self, other: u128) -> BigUint {
                        self *= other;
                        self
                    }
                }
                impl MulAssign<u128> for BigUint {
                    cfg_digit!(
                        #[inline]
                        fn mul_assign(&mut self, other: u128) {
                            if let Some(other) = BigDigit::from_u128(other) {
                                scalar_mul(self, other);
                            } else {
                                *self = match super::u32_from_u128(other) {
                                    (0, 0, c, d) => mul3(&self.data, &[d, c]),
                                    (0, b, c, d) => mul3(&self.data, &[d, c, b]),
                                    (a, b, c, d) => mul3(&self.data, &[d, c, b, a]),
                                };
                            }
                        }
                        #[inline]
                        fn mul_assign(&mut self, other: u128) {
                            if let Some(other) = BigDigit::from_u128(other) {
                                scalar_mul(self, other);
                            } else {
                                let (hi, lo) = ::num::big::digit::from_doublebigdigit(other);
                                *self = mul3(&self.data, &[lo, hi]);
                            }
                        }
                    );
                }
                impl CheckedMul for BigUint
                {
                    #[inline] fn checked_mul(&self, v: &BigUint) -> Option<BigUint> {
                        Some(self.mul(v))
                    }
                }
                impl_product_iter_type!(BigUint);
            }
            pub mod subtraction
            {
                /*!
                */
                use ::
                {
                    cmp::Ordering::{ Equal, Greater, Less },
                    num::
                    {
                        big::
                        {
                            digit::{self, BigDigit}, UsizePromotion
                        },
                        traits::{ CheckedSub },
                    },
                    ops::{ Sub, SubAssign },
                    *,
                };
                
                use super::BigUint;
                /*
                */
                use ::arch::x86_64 as arch;
                
                cfg_64!(
                    #[inline] fn sbb(borrow: u8, a: u64, b: u64, out: &mut u64) -> u8 {
                       
                       
                        unsafe { arch::_subborrow_u64(borrow, a, b, out) }
                    }
                );
                
                cfg_32!(
                    #[inline] fn sbb(borrow: u8, a: u32, b: u32, out: &mut u32) -> u8 {
                       
                       
                        unsafe { arch::_subborrow_u32(borrow, a, b, out) }
                    }
                );
                
                pub(super) fn sub2(a: &mut [BigDigit], b: &[BigDigit]) {
                    let mut borrow = 0;

                    let len = Ord::min(a.len(), b.len());
                    let (a_lo, a_hi) = a.split_at_mut(len);
                    let (b_lo, b_hi) = b.split_at(len);

                    for (a, b) in a_lo.iter_mut().zip(b_lo) {
                        borrow = sbb(borrow, *a, *b, a);
                    }
                    if borrow != 0 {
                        for a in a_hi {
                            borrow = sbb(borrow, *a, 0, a);
                            if borrow == 0 {
                                break;
                            }
                        }
                    }
                   
                    assert!(
                        borrow == 0 && b_hi.iter().all(|x| *x == 0),
                        "Cannot subtract b from a because b is larger than a."
                    );
                }
               
                #[inline] fn __sub2rev(a: &[BigDigit], b: &mut [BigDigit]) -> u8 {
                    debug_assert!(b.len() == a.len());

                    let mut borrow = 0;

                    for (ai, bi) in a.iter().zip(b) {
                        borrow = sbb(borrow, *ai, *bi, bi);
                    }
                    borrow
                }
                fn sub2rev(a: &[BigDigit], b: &mut [BigDigit]) {
                    debug_assert!(b.len() >= a.len());

                    let len = Ord::min(a.len(), b.len());
                    let (a_lo, a_hi) = a.split_at(len);
                    let (b_lo, b_hi) = b.split_at_mut(len);

                    let borrow = __sub2rev(a_lo, b_lo);

                    assert!(a_hi.is_empty());

                   
                    assert!(
                        borrow == 0 && b_hi.iter().all(|x| *x == 0),
                        "Cannot subtract b from a because b is larger than a."
                    );
                }
                forward_val_val_binop!(impl Sub for BigUint, sub);
                forward_ref_ref_binop_big!(impl Sub for BigUint, sub);
                forward_val_assign!(impl SubAssign for BigUint, sub_assign);

                impl Sub<&BigUint> for BigUint
                {
                    type Output = BigUint;

                    fn sub(mut self, other: &BigUint) -> BigUint {
                        self -= other;
                        self
                    }
                }
                impl SubAssign<&BigUint> for BigUint
                {
                    fn sub_assign(&mut self, other: &BigUint) {
                        sub2(&mut self.data[..], &other.data[..]);
                        self.normalize();
                    }
                }
                impl Sub<BigUint> for &BigUint
                {
                    type Output = BigUint;

                    fn sub(self, mut other: BigUint) -> BigUint {
                        let other_len = other.data.len();
                        if other_len < self.data.len() {
                            let lo_borrow = __sub2rev(&self.data[..other_len], &mut other.data);
                            other.data.extend_from_slice(&self.data[other_len..]);
                            if lo_borrow != 0 {
                                sub2(&mut other.data[other_len..], &[1])
                            }
                        } else {
                            sub2rev(&self.data[..], &mut other.data[..]);
                        }
                        other.normalized()
                    }
                }
                promote_unsigned_scalars!(impl Sub for BigUint, sub);
                promote_unsigned_scalars_assign!(impl SubAssign for BigUint, sub_assign);
                forward_all_scalar_binop_to_val_val!(impl Sub<u32> for BigUint, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<u64> for BigUint, sub);
                forward_all_scalar_binop_to_val_val!(impl Sub<u128> for BigUint, sub);

                impl Sub<u32> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn sub(mut self, other: u32) -> BigUint {
                        self -= other;
                        self
                    }
                }
                impl SubAssign<u32> for BigUint 
                {
                    fn sub_assign(&mut self, other: u32) {
                        sub2(&mut self.data[..], &[other as BigDigit]);
                        self.normalize();
                    }
                }
                impl Sub<BigUint> for u32 
                {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            if other.data.len() == 0 {
                                other.data.push(self);
                            } else {
                                sub2rev(&[self], &mut other.data[..]);
                            }
                            other.normalized()
                        }
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            if other.data.is_empty() {
                                other.data.push(self as BigDigit);
                            } else {
                                sub2rev(&[self as BigDigit], &mut other.data[..]);
                            }
                            other.normalized()
                        }
                    );
                }
                impl Sub<u64> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn sub(mut self, other: u64) -> BigUint {
                        self -= other;
                        self
                    }
                }
                impl SubAssign<u64> for BigUint 
                {
                    cfg_digit!(
                        #[inline]
                        fn sub_assign(&mut self, other: u64) {
                            let (hi, lo) = ::num::big::digit::from_doublebigdigit(other);
                            sub2(&mut self.data[..], &[lo, hi]);
                            self.normalize();
                        }
                        #[inline]
                        fn sub_assign(&mut self, other: u64) {
                            sub2(&mut self.data[..], &[other as BigDigit]);
                            self.normalize();
                        }
                    );
                }
                impl Sub<BigUint> for u64 
                {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            while other.data.len() < 2 {
                                other.data.push(0);
                            }
                            let (hi, lo) = ::num::big::digit::from_doublebigdigit(self);
                            sub2rev(&[lo, hi], &mut other.data[..]);
                            other.normalized()
                        }
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            if other.data.is_empty() {
                                other.data.push(self);
                            } else {
                                sub2rev(&[self], &mut other.data[..]);
                            }
                            other.normalized()
                        }
                    );
                }
                impl Sub<u128> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn sub(mut self, other: u128) -> BigUint {
                        self -= other;
                        self
                    }
                }
                impl SubAssign<u128> for BigUint 
                {
                    cfg_digit!(
                        #[inline]
                        fn sub_assign(&mut self, other: u128) {
                            let (a, b, c, d) = super::u32_from_u128(other);
                            sub2(&mut self.data[..], &[d, c, b, a]);
                            self.normalize();
                        }
                        #[inline]
                        fn sub_assign(&mut self, other: u128) {
                            let (hi, lo) = ::num::big::digit::from_doublebigdigit(other);
                            sub2(&mut self.data[..], &[lo, hi]);
                            self.normalize();
                        }
                    );
                }
                impl Sub<BigUint> for u128 
                {
                    type Output = BigUint;

                    cfg_digit!(
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            while other.data.len() < 4 {
                                other.data.push(0);
                            }
                            let (a, b, c, d) = super::u32_from_u128(self);
                            sub2rev(&[d, c, b, a], &mut other.data[..]);
                            other.normalized()
                        }
                        #[inline]
                        fn sub(self, mut other: BigUint) -> BigUint {
                            while other.data.len() < 2 {
                                other.data.push(0);
                            }
                            let (hi, lo) = ::num::big::digit::from_doublebigdigit(self);
                            sub2rev(&[lo, hi], &mut other.data[..]);
                            other.normalized()
                        }
                    );
                }
                impl CheckedSub for BigUint
                {
                    #[inline] fn checked_sub(&self, v: &BigUint) -> Option<BigUint> {
                        match self.cmp(v) {
                            Less => None,
                            Equal => Some(Self::ZERO),
                            Greater => Some(self.sub(v)),
                        }
                    }
                }
            }
            pub mod bits
            {
                /*!
                */
                use ::
                {
                    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign},
                    *,
                };
                use super::{BigUint, IntDigits};
                /*
                */
                forward_val_val_binop!(impl BitAnd for BigUint, bitand);
                forward_ref_val_binop_big!(impl BitAnd for BigUint, bitand);
               
                impl BitAnd<&BigUint> for &BigUint
                {
                    type Output = BigUint;

                    #[inline] fn bitand(self, other: &BigUint) -> BigUint {
                       
                        if self.data.len() <= other.data.len() {
                            self.clone() & other
                        } else {
                            other.clone() & self
                        }
                    }
                }
                forward_val_assign!(impl BitAndAssign for BigUint, bitand_assign);

                impl BitAnd<&BigUint> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn bitand(mut self, other: &BigUint) -> BigUint {
                        self &= other;
                        self
                    }
                }
                impl BitAndAssign<&BigUint> for BigUint
                {
                    #[inline] fn bitand_assign(&mut self, other: &BigUint) {
                        for (ai, &bi) in self.data.iter_mut().zip(other.data.iter()) {
                            *ai &= bi;
                        }
                        self.data.truncate(other.data.len());
                        self.normalize();
                    }
                }
                forward_all_binop_to_val_ref_commutative!(impl BitOr for BigUint, bitor);
                forward_val_assign!(impl BitOrAssign for BigUint, bitor_assign);

                impl BitOr<&BigUint> for BigUint
                {
                    type Output = BigUint;

                    fn bitor(mut self, other: &BigUint) -> BigUint {
                        self |= other;
                        self
                    }
                }
                impl BitOrAssign<&BigUint> for BigUint
                {
                    #[inline] fn bitor_assign(&mut self, other: &BigUint) {
                        for (ai, &bi) in self.data.iter_mut().zip(other.data.iter()) {
                            *ai |= bi;
                        }
                        if other.data.len() > self.data.len() {
                            let extra = &other.data[self.data.len()..];
                            self.data.extend(extra.iter().cloned());
                        }
                    }
                }
                forward_all_binop_to_val_ref_commutative!(impl BitXor for BigUint, bitxor);
                forward_val_assign!(impl BitXorAssign for BigUint, bitxor_assign);

                impl BitXor<&BigUint> for BigUint
                {
                    type Output = BigUint;

                    fn bitxor(mut self, other: &BigUint) -> BigUint {
                        self ^= other;
                        self
                    }
                }
                impl BitXorAssign<&BigUint> for BigUint
                {
                    #[inline] fn bitxor_assign(&mut self, other: &BigUint) {
                        for (ai, &bi) in self.data.iter_mut().zip(other.data.iter()) {
                            *ai ^= bi;
                        }
                        if other.data.len() > self.data.len() {
                            let extra = &other.data[self.data.len()..];
                            self.data.extend(extra.iter().cloned());
                        }
                        self.normalize();
                    }
                }
            }
            pub mod convert
            {
                /*!
                */
                use ::
                {
                    vec::{ Vec },
                    cmp::Ordering::{ Equal, Greater, Less },
                    convert::{ TryFrom },
                    num::
                    {
                        big::
                        {
                            digit::{self, BigDigit},
                            ParseBigIntError, TryFromBigIntError,
                        },
                        integers::{ Integer, Roots },
                        traits::
                        {
                            float::{ Float, FloatCore }, FromPrimitive, Num, One, PrimInt, ToPrimitive, Zero,
                        },
                    },
                    str::{ FromStr },
                    *,
                };
                use super::{biguint_from_vec, BigUint, ToBigUint};
                use super::addition::add2;
                use super::division::{div_rem_digit, FAST_DIV_WIDE};
                use super::multiplication::mac_with_carry;
                /*
                */
                /// Find last set bit
                /// fls(0) == 0, fls(u32::MAX) == 32
                fn fls<T: PrimInt>(v: T) -> u8 {
                    mem::size_of::<T>() as u8 * 8 - v.leading_zeros() as u8
                }
                fn ilog2<T: PrimInt>(v: T) -> u8 {
                    fls(v) - 1
                }
                impl FromStr for BigUint
                {
                    type Err = ParseBigIntError;

                    #[inline] fn from_str(s: &str) -> Result<BigUint, ParseBigIntError> {
                        BigUint::from_str_radix(s, 10)
                    }
                }
                
                pub(super) fn from_bitwise_digits_le(v: &[u8], bits: u8) -> BigUint {
                    debug_assert!(!v.is_empty() && bits <= 8 && ::num::big::digit::BITS % bits == 0);
                    debug_assert!(v.iter().all(|&c| BigDigit::from(c) < (1 << bits)));

                    let digits_per_big_digit = ::num::big::digit::BITS / bits;

                    let data = v
                        .chunks(digits_per_big_digit.into())
                        .map(|chunk| {
                            chunk
                                .iter()
                                .rev()
                                .fold(0, |acc, &c| (acc << bits) | BigDigit::from(c))
                        })
                        .collect();

                    biguint_from_vec(data)
                }
               
               
                fn from_inexact_bitwise_digits_le(v: &[u8], bits: u8) -> BigUint {
                    debug_assert!(!v.is_empty() && bits <= 8 && ::num::big::digit::BITS % bits != 0);
                    debug_assert!(v.iter().all(|&c| BigDigit::from(c) < (1 << bits)));

                    let total_bits = (v.len() as u64).saturating_mul(bits.into());
                    let big_digits = Integer::div_ceil(&total_bits, &::num::big::digit::BITS.into())
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    let mut data = Vec::with_capacity(big_digits);

                    let mut d = 0;
                    let mut dbits = 0;

                   
                   
                    for &c in v {
                        d |= BigDigit::from(c) << dbits;
                        dbits += bits;

                        if dbits >= ::num::big::digit::BITS {
                            data.push(d);
                            dbits -= ::num::big::digit::BITS;
                           
                           
                            d = BigDigit::from(c) >> (bits - dbits);
                        }
                    }
                    if dbits > 0 {
                        debug_assert!(dbits < ::num::big::digit::BITS);
                        data.push(d as BigDigit);
                    }
                    biguint_from_vec(data)
                }
               
                fn from_radix_digits_be(v: &[u8], radix: u32) -> BigUint {
                    debug_assert!(!v.is_empty() && !radix.is_power_of_two());
                    debug_assert!(v.iter().all(|&c| u32::from(c) < radix));

                   
                            let big_digits = {
                        let radix_log2 = f64::from(radix).log2();
                        let bits = radix_log2 * v.len() as f64;
                        (bits / ::num::big::digit::BITS as f64).ceil()
                    };
                    
                    let mut data = Vec::with_capacity(big_digits.to_usize().unwrap_or(0));

                    let (base, power) = get_radix_base(radix);
                    let radix = radix as BigDigit;

                    let r = v.len() % power;
                    let i = if r == 0 { power } else { r };
                    let (head, tail) = v.split_at(i);

                    let first = head
                        .iter()
                        .fold(0, |acc, &d| acc * radix + BigDigit::from(d));
                    data.push(first);

                    debug_assert!(tail.len() % power == 0);
                    for chunk in tail.chunks(power) {
                        if data.last() != Some(&0) {
                            data.push(0);
                        }
                        let mut carry = 0;
                        for d in data.iter_mut() {
                            *d = mac_with_carry(0, *d, base, &mut carry);
                        }
                        debug_assert!(carry == 0);

                        let n = chunk
                            .iter()
                            .fold(0, |acc, &d| acc * radix + BigDigit::from(d));
                        add2(&mut data, &[n]);
                    }
                    biguint_from_vec(data)
                }
                pub(super) fn from_radix_be(buf: &[u8], radix: u32) -> Option<BigUint> {
                    assert!(
                        2 <= radix && radix <= 256,
                        "The radix must be within 2...256"
                    );

                    if buf.is_empty() {
                        return Some(BigUint::ZERO);
                    }
                    if radix != 256 && buf.iter().any(|&b| b >= radix as u8) {
                        return None;
                    }
                    let res = if radix.is_power_of_two() {
                       
                        let bits = ilog2(radix);
                        let mut v = Vec::from(buf);
                        v.reverse();
                        if ::num::big::digit::BITS % bits == 0 {
                            from_bitwise_digits_le(&v, bits)
                        } else {
                            from_inexact_bitwise_digits_le(&v, bits)
                        }
                    } else {
                        from_radix_digits_be(buf, radix)
                    };

                    Some(res)
                }
                pub(super) fn from_radix_le(buf: &[u8], radix: u32) -> Option<BigUint> {
                    assert!(
                        2 <= radix && radix <= 256,
                        "The radix must be within 2...256"
                    );

                    if buf.is_empty() {
                        return Some(BigUint::ZERO);
                    }
                    if radix != 256 && buf.iter().any(|&b| b >= radix as u8) {
                        return None;
                    }
                    let res = if radix.is_power_of_two() {
                       
                        let bits = ilog2(radix);
                        if ::num::big::digit::BITS % bits == 0 {
                            from_bitwise_digits_le(buf, bits)
                        } else {
                            from_inexact_bitwise_digits_le(buf, bits)
                        }
                    } else {
                        let mut v = Vec::from(buf);
                        v.reverse();
                        from_radix_digits_be(&v, radix)
                    };

                    Some(res)
                }
                impl Num for BigUint
                {
                    type FromStrRadixErr = ParseBigIntError;

                    /// Creates and initializes a `BigUint`.
                    fn from_str_radix(s: &str, radix: u32) -> Result<BigUint, ParseBigIntError> {
                        assert!(2 <= radix && radix <= 36, "The radix must be within 2...36");
                        let mut s = s;
                        if let Some(tail) = s.strip_prefix('+') {
                            if !tail.starts_with('+') {
                                s = tail
                            }
                        }
                        if s.is_empty() {
                            return Err(ParseBigIntError::empty());
                        }
                        if s.starts_with('_') {
                           
                            return Err(ParseBigIntError::invalid());
                        }
                       
                        let mut v = Vec::with_capacity(s.len());
                        for b in s.bytes() {
                            let d = match b {
                                b'0'..=b'9' => b - b'0',
                                b'a'..=b'z' => b - b'a' + 10,
                                b'A'..=b'Z' => b - b'A' + 10,
                                b'_' => continue,
                                _ => u8::MAX,
                            };
                            if d < radix as u8 {
                                v.push(d);
                            } else {
                                return Err(ParseBigIntError::invalid());
                            }
                        }
                        let res = if radix.is_power_of_two() {
                           
                            let bits = ilog2(radix);
                            v.reverse();
                            if ::num::big::digit::BITS % bits == 0 {
                                from_bitwise_digits_le(&v, bits)
                            } else {
                                from_inexact_bitwise_digits_le(&v, bits)
                            }
                        } else {
                            from_radix_digits_be(&v, radix)
                        };
                        Ok(res)
                    }
                }
                fn high_bits_to_u64(v: &BigUint) -> u64 {
                    match v.data.len() {
                        0 => 0,
                        1 => {
                           
                            #[allow(clippy::useless_conversion)]
                            let v0 = u64::from(v.data[0]);
                            v0
                        }
                        _ => {
                            let mut bits = v.bits();
                            let mut ret = 0u64;
                            let mut ret_bits = 0;

                            for d in v.data.iter().rev() {
                                let digit_bits = (bits - 1) % u64::from(::num::big::digit::BITS) + 1;
                                let bits_want = Ord::min(64 - ret_bits, digit_bits);

                                if bits_want != 0 {
                                    if bits_want != 64 {
                                        ret <<= bits_want;
                                    }
                                   
                                    #[allow(clippy::useless_conversion)]
                                    let d0 = u64::from(*d) >> (digit_bits - bits_want);
                                    ret |= d0;
                                }
                               
                               
                               
                                //
                               

                                if digit_bits - bits_want != 0 {
                                   
                                    #[allow(clippy::useless_conversion)]
                                    let masked = u64::from(*d) << (64 - (digit_bits - bits_want) as u32);
                                    ret |= (masked != 0) as u64;
                                }
                                ret_bits += bits_want;
                                bits -= bits_want;
                            }
                            ret
                        }
                    }
                }
                impl ToPrimitive for BigUint
                {
                    #[inline] fn to_i64(&self) -> Option<i64> {
                        self.to_u64().as_ref().and_then(u64::to_i64)
                    }
                    #[inline] fn to_i128(&self) -> Option<i128> {
                        self.to_u128().as_ref().and_then(u128::to_i128)
                    }
                    #[allow(clippy::useless_conversion)]
                    #[inline] fn to_u64(&self) -> Option<u64> {
                        let mut ret: u64 = 0;
                        let mut bits = 0;

                        for i in self.data.iter() {
                            if bits >= 64 {
                                return None;
                            }
                           
                            ret += u64::from(*i) << bits;
                            bits += ::num::big::digit::BITS;
                        }
                        Some(ret)
                    }
                    #[inline] fn to_u128(&self) -> Option<u128> {
                        let mut ret: u128 = 0;
                        let mut bits = 0;

                        for i in self.data.iter() {
                            if bits >= 128 {
                                return None;
                            }
                            ret |= u128::from(*i) << bits;
                            bits += ::num::big::digit::BITS;
                        }
                        Some(ret)
                    }
                    #[inline] fn to_f32(&self) -> Option<f32> {
                        let mantissa = high_bits_to_u64(self);
                        let exponent = self.bits() - u64::from(fls(mantissa));

                        if exponent > f32::MAX_EXP as u64 {
                            Some(f32::INFINITY)
                        } else {
                            Some((mantissa as f32) * 2.0f32.powi(exponent as i32))
                        }
                    }
                    #[inline] fn to_f64(&self) -> Option<f64> {
                        let mantissa = high_bits_to_u64(self);
                        let exponent = self.bits() - u64::from(fls(mantissa));

                        if exponent > f64::MAX_EXP as u64 {
                            Some(f64::INFINITY)
                        } else {
                            Some((mantissa as f64) * 2.0f64.powi(exponent as i32))
                        }
                    }
                }
                macro_rules! impl_try_from_biguint {
                    ($T:ty, $to_ty:path) => {
                        impl TryFrom<&BigUint> for $T {
                            type Error = TryFromBigIntError<()>;

                            #[inline]
                            fn try_from(value: &BigUint) -> Result<$T, TryFromBigIntError<()>> {
                                $to_ty(value).ok_or(TryFromBigIntError::new(()))
                            }
                        }
                        impl TryFrom<BigUint> for $T {
                            type Error = TryFromBigIntError<BigUint>;

                            #[inline]
                            fn try_from(value: BigUint) -> Result<$T, TryFromBigIntError<BigUint>> {
                                <$T>::try_from(&value).map_err(|_| TryFromBigIntError::new(value))
                            }
                        }
                    };
                }
                impl_try_from_biguint!(u8, ToPrimitive::to_u8);
                impl_try_from_biguint!(u16, ToPrimitive::to_u16);
                impl_try_from_biguint!(u32, ToPrimitive::to_u32);
                impl_try_from_biguint!(u64, ToPrimitive::to_u64);
                impl_try_from_biguint!(usize, ToPrimitive::to_usize);
                impl_try_from_biguint!(u128, ToPrimitive::to_u128);

                impl_try_from_biguint!(i8, ToPrimitive::to_i8);
                impl_try_from_biguint!(i16, ToPrimitive::to_i16);
                impl_try_from_biguint!(i32, ToPrimitive::to_i32);
                impl_try_from_biguint!(i64, ToPrimitive::to_i64);
                impl_try_from_biguint!(isize, ToPrimitive::to_isize);
                impl_try_from_biguint!(i128, ToPrimitive::to_i128);

                impl FromPrimitive for BigUint
                {
                    #[inline] fn from_i64(n: i64) -> Option<BigUint> {
                        if n >= 0 {
                            Some(BigUint::from(n as u64))
                        } else {
                            None
                        }
                    }
                    #[inline] fn from_i128(n: i128) -> Option<BigUint> {
                        if n >= 0 {
                            Some(BigUint::from(n as u128))
                        } else {
                            None
                        }
                    }
                    #[inline] fn from_u64(n: u64) -> Option<BigUint> {
                        Some(BigUint::from(n))
                    }
                    #[inline] fn from_u128(n: u128) -> Option<BigUint> {
                        Some(BigUint::from(n))
                    }
                    #[inline] fn from_f64(mut n: f64) -> Option<BigUint> {
                       
                        if !n.is_finite() {
                            return None;
                        }
                       
                        n = n.trunc();

                       
                        if n.is_zero() {
                            return Some(Self::ZERO);
                        }
                        let (mantissa, exponent, sign) = Float::integer_decode(n);

                        if sign == -1 {
                            return None;
                        }
                        let mut ret = BigUint::from(mantissa);
                        match exponent.cmp(&0) {
                            Greater => ret <<= exponent as usize,
                            Equal => {}
                            Less => ret >>= (-exponent) as usize,
                        }
                        Some(ret)
                    }
                }
                impl From<u64> for BigUint
                {
                    #[inline] fn from(mut n: u64) -> Self {
                        let mut ret: BigUint = Self::ZERO;

                        while n != 0 {
                            ret.data.push(n as BigDigit);
                           
                            n = (n >> 1) >> (::num::big::digit::BITS - 1);
                        }
                        ret
                    }
                }
                impl From<u128> for BigUint
                {
                    #[inline] fn from(mut n: u128) -> Self {
                        let mut ret: BigUint = Self::ZERO;

                        while n != 0 {
                            ret.data.push(n as BigDigit);
                            n >>= ::num::big::digit::BITS;
                        }
                        ret
                    }
                }
                macro_rules! impl_biguint_from_uint {
                    ($T:ty) => {
                        impl From<$T> for BigUint {
                            #[inline]
                            fn from(n: $T) -> Self {
                                BigUint::from(n as u64)
                            }
                        }
                    };
                }
                impl_biguint_from_uint!(u8);
                impl_biguint_from_uint!(u16);
                impl_biguint_from_uint!(u32);
                impl_biguint_from_uint!(usize);

                macro_rules! impl_biguint_try_from_int {
                    ($T:ty, $from_ty:path) => {
                        impl TryFrom<$T> for BigUint {
                            type Error = TryFromBigIntError<()>;

                            #[inline]
                            fn try_from(value: $T) -> Result<BigUint, TryFromBigIntError<()>> {
                                $from_ty(value).ok_or(TryFromBigIntError::new(()))
                            }
                        }
                    };
                }
                impl_biguint_try_from_int!(i8, FromPrimitive::from_i8);
                impl_biguint_try_from_int!(i16, FromPrimitive::from_i16);
                impl_biguint_try_from_int!(i32, FromPrimitive::from_i32);
                impl_biguint_try_from_int!(i64, FromPrimitive::from_i64);
                impl_biguint_try_from_int!(isize, FromPrimitive::from_isize);
                impl_biguint_try_from_int!(i128, FromPrimitive::from_i128);

                impl ToBigUint for BigUint
                {
                    #[inline] fn to_biguint(&self) -> Option<BigUint> {
                        Some(self.clone())
                    }
                }
                macro_rules! impl_to_biguint {
                    ($T:ty, $from_ty:path) => {
                        impl ToBigUint for $T {
                            #[inline]
                            fn to_biguint(&self) -> Option<BigUint> {
                                $from_ty(*self)
                            }
                        }
                    };
                }
                impl_to_biguint!(isize, FromPrimitive::from_isize);
                impl_to_biguint!(i8, FromPrimitive::from_i8);
                impl_to_biguint!(i16, FromPrimitive::from_i16);
                impl_to_biguint!(i32, FromPrimitive::from_i32);
                impl_to_biguint!(i64, FromPrimitive::from_i64);
                impl_to_biguint!(i128, FromPrimitive::from_i128);

                impl_to_biguint!(usize, FromPrimitive::from_usize);
                impl_to_biguint!(u8, FromPrimitive::from_u8);
                impl_to_biguint!(u16, FromPrimitive::from_u16);
                impl_to_biguint!(u32, FromPrimitive::from_u32);
                impl_to_biguint!(u64, FromPrimitive::from_u64);
                impl_to_biguint!(u128, FromPrimitive::from_u128);

                impl_to_biguint!(f32, FromPrimitive::from_f32);
                impl_to_biguint!(f64, FromPrimitive::from_f64);

                impl From<bool> for BigUint {
                    fn from(x: bool) -> Self {
                        if x {
                            One::one()
                        } else {
                            Self::ZERO
                        }
                    }
                }
               
                pub(super) fn to_bitwise_digits_le(u: &BigUint, bits: u8) -> Vec<u8> {
                    debug_assert!(!u.is_zero() && bits <= 8 && ::num::big::digit::BITS % bits == 0);

                    let last_i = u.data.len() - 1;
                    let mask: BigDigit = (1 << bits) - 1;
                    let digits_per_big_digit = ::num::big::digit::BITS / bits;
                    let digits = Integer::div_ceil(&u.bits(), &u64::from(bits))
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    let mut res = Vec::with_capacity(digits);

                    for mut r in u.data[..last_i].iter().cloned() {
                        for _ in 0..digits_per_big_digit {
                            res.push((r & mask) as u8);
                            r >>= bits;
                        }
                    }
                    let mut r = u.data[last_i];
                    while r != 0 {
                        res.push((r & mask) as u8);
                        r >>= bits;
                    }
                    res
                }
               
                fn to_inexact_bitwise_digits_le(u: &BigUint, bits: u8) -> Vec<u8> {
                    debug_assert!(!u.is_zero() && bits <= 8 && ::num::big::digit::BITS % bits != 0);

                    let mask: BigDigit = (1 << bits) - 1;
                    let digits = Integer::div_ceil(&u.bits(), &u64::from(bits))
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    let mut res = Vec::with_capacity(digits);

                    let mut r = 0;
                    let mut rbits = 0;

                    for c in &u.data {
                        r |= *c << rbits;
                        rbits += ::num::big::digit::BITS;

                        while rbits >= bits {
                            res.push((r & mask) as u8);
                            r >>= bits;

                           
                            if rbits > ::num::big::digit::BITS {
                                r = *c >> (::num::big::digit::BITS - (rbits - bits));
                            }
                            rbits -= bits;
                        }
                    }
                    if rbits != 0 {
                        res.push(r as u8);
                    }
                    while let Some(&0) = res.last() {
                        res.pop();
                    }
                    res
                }
               
                #[inline(always)]
                pub(super) fn to_radix_digits_le(u: &BigUint, radix: u32) -> Vec<u8> {
                    debug_assert!(!u.is_zero() && !radix.is_power_of_two());

                            let radix_digits = {
                        let radix_log2 = f64::from(radix).log2();
                        ((u.bits() as f64) / radix_log2).ceil()
                    };
                    
                    let mut res = Vec::with_capacity(radix_digits.to_usize().unwrap_or(0));

                    let mut digits = u.clone();

                   
                   
                    let (base, power) = if FAST_DIV_WIDE {
                        get_radix_base(radix)
                    } else {
                        get_half_radix_base(radix)
                    };
                    let radix = radix as BigDigit;

                   
                   
                   
                   
                    if digits.data.len() >= 64 {
                        let mut big_base = BigUint::from(base);
                        let mut big_power = 1usize;

                       
                        let target_len = digits.data.len().sqrt();
                        while big_base.data.len() < target_len {
                            big_base = &big_base * &big_base;
                            big_power *= 2;
                        }
                       
                        while digits > big_base {
                           
                            let (q, mut big_r) = digits.div_rem(&big_base);
                            digits = q;

                           
                            for _ in 0..big_power {
                                let (q, mut r) = div_rem_digit(big_r, base);
                                big_r = q;
                                for _ in 0..power {
                                    res.push((r % radix) as u8);
                                    r /= radix;
                                }
                            }
                        }
                    }
                    while digits.data.len() > 1 {
                        let (q, mut r) = div_rem_digit(digits, base);
                        for _ in 0..power {
                            res.push((r % radix) as u8);
                            r /= radix;
                        }
                        digits = q;
                    }
                    let mut r = digits.data[0];
                    while r != 0 {
                        res.push((r % radix) as u8);
                        r /= radix;
                    }
                    res
                }
                pub(super) fn to_radix_le(u: &BigUint, radix: u32) -> Vec<u8> {
                    if u.is_zero() {
                        vec![0]
                    } else if radix.is_power_of_two() {
                       
                        let bits = ilog2(radix);
                        if ::num::big::digit::BITS % bits == 0 {
                            to_bitwise_digits_le(u, bits)
                        } else {
                            to_inexact_bitwise_digits_le(u, bits)
                        }
                    } else if radix == 10 {
                       
                       
                        to_radix_digits_le(u, 10)
                    } else {
                        to_radix_digits_le(u, radix)
                    }
                }
                pub fn to_str_radix_reversed(u: &BigUint, radix: u32) -> Vec<u8> {
                    assert!(2 <= radix && radix <= 36, "The radix must be within 2...36");

                    if u.is_zero() {
                        return vec![b'0'];
                    }
                    let mut res = to_radix_le(u, radix);

                   
                    for r in &mut res {
                        debug_assert!(u32::from(*r) < radix);
                        if *r < 10 {
                            *r += b'0';
                        } else {
                            *r += b'a' - 10;
                        }
                    }
                    res
                }
                /// Returns the greatest power of the radix for the `BigDigit` bit size
                #[inline] fn get_radix_base(radix: u32) -> (BigDigit, usize) {
                    static BASES: [(BigDigit, usize); 257] = generate_radix_bases(::num::big::digit::MAX);
                    debug_assert!(!radix.is_power_of_two());
                    debug_assert!((3..256).contains(&radix));
                    BASES[radix as usize]
                }
                /// Returns the greatest power of the radix for half the `BigDigit` bit size
                #[inline] fn get_half_radix_base(radix: u32) -> (BigDigit, usize) {
                    static BASES: [(BigDigit, usize); 257] = generate_radix_bases(::num::big::digit::HALF);
                    debug_assert!(!radix.is_power_of_two());
                    debug_assert!((3..256).contains(&radix));
                    BASES[radix as usize]
                }
                /// Generate tables of the greatest power of each radix that is less that the given maximum. These
                /// are returned from `get_radix_base` to batch the multiplication/division of radix conversions on
                /// full `BigUint` values, operating on primitive integers as much as possible. Powers of two are not included, just zeroed, as they're implemented with shifts.
                const fn generate_radix_bases(max: BigDigit) -> [(BigDigit, usize); 257] {
                    let mut bases = [(0, 0); 257];

                    let mut radix: BigDigit = 3;
                    while radix < 256 {
                        if !radix.is_power_of_two() {
                            let mut power = 1;
                            let mut base = radix;

                            while let Some(b) = base.checked_mul(radix) {
                                if b > max {
                                    break;
                                }
                                base = b;
                                power += 1;
                            }
                            bases[radix as usize] = (base, power)
                        }
                        radix += 1;
                    }
                    bases
                }
            } pub use self::convert::to_str_radix_reversed;

            pub mod iter
            {
                /*!
                */
                use ::
                {
                    iter::{ FusedIterator },
                    *,
                };
                /*
                    use ::iter::FusedIterator;
                */
                cfg_digit!
                (
                    /// An iterator of `u32` digits representation of a `BigUint` or `BigInt`,
                    /// ordered least significant digit first.
                    pub struct U32Digits<'a> {
                        it: ::slice::Iter<'a, u32>,
                    }
                    /// An iterator of `u32` digits representation of a `BigUint` or `BigInt`,
                    /// ordered least significant digit first.
                    pub struct U32Digits<'a> {
                        data: &'a [u64],
                        next_is_lo: bool,
                        last_hi_is_zero: bool,
                    }
                );

                cfg_digit!
                (
                    const _: () = {
                        impl<'a> U32Digits<'a> {
                            #[inline]
                            pub(super) fn new(data: &'a [u32]) -> Self {
                                Self { it: data.iter() }
                            }
                        }
                        impl Iterator for U32Digits<'_> {
                            type Item = u32;
                            #[inline]
                            fn next(&mut self) -> Option<u32> {
                                self.it.next().cloned()
                            }
                            #[inline]
                            fn size_hint(&self) -> (usize, Option<usize>) {
                                self.it.size_hint()
                            }
                            #[inline]
                            fn nth(&mut self, n: usize) -> Option<u32> {
                                self.it.nth(n).cloned()
                            }
                            #[inline]
                            fn last(self) -> Option<u32> {
                                self.it.last().cloned()
                            }
                            #[inline]
                            fn count(self) -> usize {
                                self.it.count()
                            }
                        }
                        impl DoubleEndedIterator for U32Digits<'_> {
                            fn next_back(&mut self) -> Option<Self::Item> {
                                self.it.next_back().cloned()
                            }
                        }
                        impl ExactSizeIterator for U32Digits<'_> {
                            #[inline]
                            fn len(&self) -> usize {
                                self.it.len()
                            }
                        }
                    };

                    const _: () = {
                        impl<'a> U32Digits<'a> {
                            #[inline]
                            pub(super) fn new(data: &'a [u64]) -> Self {
                                let last_hi_is_zero = data
                                    .last()
                                    .map(|&last| {
                                        let last_hi = (last >> 32) as u32;
                                        last_hi == 0
                                    })
                                    .unwrap_or(false);
                                U32Digits {
                                    data,
                                    next_is_lo: true,
                                    last_hi_is_zero,
                                }
                            }
                        }
                        impl Iterator for U32Digits<'_> {
                            type Item = u32;
                            #[inline]
                            fn next(&mut self) -> Option<u32> {
                                match self.data.split_first() {
                                    Some((&first, data)) => {
                                        let next_is_lo = self.next_is_lo;
                                        self.next_is_lo = !next_is_lo;
                                        if next_is_lo {
                                            Some(first as u32)
                                        } else {
                                            self.data = data;
                                            if data.is_empty() && self.last_hi_is_zero {
                                                self.last_hi_is_zero = false;
                                                None
                                            } else {
                                                Some((first >> 32) as u32)
                                            }
                                        }
                                    }
                                    None => None,
                                }
                            }
                            #[inline]
                            fn size_hint(&self) -> (usize, Option<usize>) {
                                let len = self.len();
                                (len, Some(len))
                            }
                            #[inline]
                            fn last(self) -> Option<u32> {
                                self.data.last().map(|&last| {
                                    if self.last_hi_is_zero {
                                        last as u32
                                    } else {
                                        (last >> 32) as u32
                                    }
                                })
                            }
                            #[inline]
                            fn count(self) -> usize {
                                self.len()
                            }
                        }
                        impl DoubleEndedIterator for U32Digits<'_> {
                            fn next_back(&mut self) -> Option<Self::Item> {
                                match self.data.split_last() {
                                    Some((&last, data)) => {
                                        let last_is_lo = self.last_hi_is_zero;
                                        self.last_hi_is_zero = !last_is_lo;
                                        if last_is_lo {
                                            self.data = data;
                                            if data.is_empty() && !self.next_is_lo {
                                                self.next_is_lo = true;
                                                None
                                            } else {
                                                Some(last as u32)
                                            }
                                        } else {
                                            Some((last >> 32) as u32)
                                        }
                                    }
                                    None => None,
                                }
                            }
                        }
                        impl ExactSizeIterator for U32Digits<'_> {
                            #[inline]
                            fn len(&self) -> usize {
                                self.data.len() * 2
                                    - usize::from(self.last_hi_is_zero)
                                    - usize::from(!self.next_is_lo)
                            }
                        }
                    };
                );

                impl FusedIterator for U32Digits<'_> {}
                cfg_digit!
                (
                    /// An iterator of `u64` digits representation of a `BigUint` or `BigInt`,
                    /// ordered least significant digit first.
                    pub struct U64Digits<'a> {
                        it: ::slice::Chunks<'a, u32>,
                    }
                    /// An iterator of `u64` digits representation of a `BigUint` or `BigInt`,
                    /// ordered least significant digit first.
                    pub struct U64Digits<'a> {
                        it: ::slice::Iter<'a, u64>,
                    }
                );

                cfg_digit!
                (
                    const _: () = {
                        impl<'a> U64Digits<'a> {
                            #[inline]
                            pub(super) fn new(data: &'a [u32]) -> Self {
                                U64Digits { it: data.chunks(2) }
                            }
                        }
                        impl Iterator for U64Digits<'_> {
                            type Item = u64;
                            #[inline]
                            fn next(&mut self) -> Option<u64> {
                                self.it.next().map(super::u32_chunk_to_u64)
                            }
                            #[inline]
                            fn size_hint(&self) -> (usize, Option<usize>) {
                                let len = self.len();
                                (len, Some(len))
                            }
                            #[inline]
                            fn last(self) -> Option<u64> {
                                self.it.last().map(super::u32_chunk_to_u64)
                            }
                            #[inline]
                            fn count(self) -> usize {
                                self.len()
                            }
                        }
                        impl DoubleEndedIterator for U64Digits<'_> {
                            fn next_back(&mut self) -> Option<Self::Item> {
                                self.it.next_back().map(super::u32_chunk_to_u64)
                            }
                        }
                    };

                    const _: () = {
                        impl<'a> U64Digits<'a> {
                            #[inline]
                            pub(super) fn new(data: &'a [u64]) -> Self {
                                Self { it: data.iter() }
                            }
                        }
                        impl Iterator for U64Digits<'_> {
                            type Item = u64;
                            #[inline]
                            fn next(&mut self) -> Option<u64> {
                                self.it.next().cloned()
                            }
                            #[inline]
                            fn size_hint(&self) -> (usize, Option<usize>) {
                                self.it.size_hint()
                            }
                            #[inline]
                            fn nth(&mut self, n: usize) -> Option<u64> {
                                self.it.nth(n).cloned()
                            }
                            #[inline]
                            fn last(self) -> Option<u64> {
                                self.it.last().cloned()
                            }
                            #[inline]
                            fn count(self) -> usize {
                                self.it.count()
                            }
                        }
                        impl DoubleEndedIterator for U64Digits<'_> {
                            fn next_back(&mut self) -> Option<Self::Item> {
                                self.it.next_back().cloned()
                            }
                        }
                    };
                );

                impl ExactSizeIterator for U64Digits<'_>
                {
                    #[inline] fn len(&self) -> usize {
                        self.it.len()
                    }
                }
                impl FusedIterator for U64Digits<'_> {}
            }
            pub mod monty
            {
                /*!
                */
                use ::
                {
                    vec::{ Vec },
                    ops::{ Shl },
                    num::
                    {
                        big::
                        {
                            digit::{self, BigDigit, DoubleBigDigit},
                            uint::BigUint,
                        },
                        traits::One,
                    },
                    *,
                };
                /*
                */
                struct MontyReducer {
                    n0inv: BigDigit,
                }
               
               
                fn inv_mod_alt(b: BigDigit) -> BigDigit {
                    assert_ne!(b & 1, 0);

                    let mut k0 = BigDigit::wrapping_sub(2, b);
                    let mut t = b - 1;
                    let mut i = 1;
                    while i < ::num::big::digit::BITS {
                        t = t.wrapping_mul(t);
                        k0 = k0.wrapping_mul(t + 1);

                        i <<= 1;
                    }
                    debug_assert_eq!(k0.wrapping_mul(b), 1);
                    k0.wrapping_neg()
                }
                impl MontyReducer {
                    fn new(n: &BigUint) -> Self {
                        let n0inv = inv_mod_alt(n.data[0]);
                        MontyReducer { n0inv }
                    }
                }
                /// Computes z mod m = x * y * 2 ** (-n*_W) mod m
                /// assuming k = -1/m mod 2**_W
                /// See Gueron, "Efficient Software Implementations of Modular Exponentiation".
                #[allow(clippy::many_single_char_names)]
                fn montgomery(x: &BigUint, y: &BigUint, m: &BigUint, k: BigDigit, n: usize) -> BigUint {
                   
                   
                   
                   
                    assert!(
                        x.data.len() == n && y.data.len() == n && m.data.len() == n,
                        "{:?} {:?} {:?} {}",
                        x,
                        y,
                        m,
                        n
                    );

                    let mut z = BigUint::ZERO;
                    z.data.resize(n * 2, 0);

                    let mut c: BigDigit = 0;
                    for i in 0..n {
                        let c2 = add_mul_vvw(&mut z.data[i..n + i], &x.data, y.data[i]);
                        let t = z.data[i].wrapping_mul(k);
                        let c3 = add_mul_vvw(&mut z.data[i..n + i], &m.data, t);
                        let cx = c.wrapping_add(c2);
                        let cy = cx.wrapping_add(c3);
                        z.data[n + i] = cy;
                        if cx < c2 || cy < c3 {
                            c = 1;
                        } else {
                            c = 0;
                        }
                    }
                    if c == 0 {
                        z.data = z.data[n..].to_vec();
                    } else {
                        {
                            let (first, second) = z.data.split_at_mut(n);
                            sub_vv(first, second, &m.data);
                        }
                        z.data = z.data[..n].to_vec();
                    }
                    z
                }
                #[inline( always )] fn add_mul_vvw(z: &mut [BigDigit], x: &[BigDigit], y: BigDigit) -> BigDigit {
                    let mut c = 0;
                    for (zi, xi) in z.iter_mut().zip(x.iter()) {
                        let (z1, z0) = mul_add_www(*xi, y, *zi);
                        let (c_, zi_) = add_ww(z0, c, 0);
                        *zi = zi_;
                        c = c_ + z1;
                    }
                    c
                }
                /// The resulting carry c is either 0 or 1.
                #[inline( always )] fn sub_vv(z: &mut [BigDigit], x: &[BigDigit], y: &[BigDigit]) -> BigDigit {
                    let mut c = 0;
                    for (i, (xi, yi)) in x.iter().zip(y.iter()).enumerate().take(z.len()) {
                        let zi = xi.wrapping_sub(*yi).wrapping_sub(c);
                        z[i] = zi;
                       
                        c = ((yi & !xi) | ((yi | !xi) & zi)) >> (::num::big::digit::BITS - 1)
                    }
                    c
                }
                /// z1<<_W + z0 = x+y+c, with c == 0 or 1
                #[inline( always )] fn add_ww(x: BigDigit, y: BigDigit, c: BigDigit) -> (BigDigit, BigDigit) {
                    let yc = y.wrapping_add(c);
                    let z0 = x.wrapping_add(yc);
                    let z1 = if z0 < x || yc < y { 1 } else { 0 };

                    (z1, z0)
                }
                /// z1 << _W + z0 = x * y + c
                #[inline( always )] fn mul_add_www(x: BigDigit, y: BigDigit, c: BigDigit) -> (BigDigit, BigDigit) {
                    let z = x as DoubleBigDigit * y as DoubleBigDigit + c as DoubleBigDigit;
                    ((z >> ::num::big::digit::BITS) as BigDigit, z as BigDigit)
                }
                /// Calculates x ** y mod m using a fixed, 4-bit window.
                #[allow(clippy::many_single_char_names)]
                pub(super) fn monty_modpow(x: &BigUint, y: &BigUint, m: &BigUint) -> BigUint {
                    assert!(m.data[0] & 1 == 1);
                    let mr = MontyReducer::new(m);
                    let num_words = m.data.len();

                    let mut x = x.clone();

                   
                   
                    if x.data.len() > num_words {
                        x %= m;
                       
                    }
                    if x.data.len() < num_words {
                        x.data.resize(num_words, 0);
                    }
                   
                    let mut rr = BigUint::one();
                    rr = (rr.shl(2 * num_words as u64 * u64::from(::num::big::digit::BITS))) % m;
                    if rr.data.len() < num_words {
                        rr.data.resize(num_words, 0);
                    }
                   
                    let mut one = BigUint::one();
                    one.data.resize(num_words, 0);

                    let n = 4;
                   
                    let mut powers = Vec::with_capacity(1 << n);
                    powers.push(montgomery(&one, &rr, m, mr.n0inv, num_words));
                    powers.push(montgomery(&x, &rr, m, mr.n0inv, num_words));
                    for i in 2..1 << n {
                        let r = montgomery(&powers[i - 1], &powers[1], m, mr.n0inv, num_words);
                        powers.push(r);
                    }
                   
                    let mut z = powers[0].clone();
                    z.data.resize(num_words, 0);
                    let mut zz = BigUint::ZERO;
                    zz.data.resize(num_words, 0);

                   
                    for i in (0..y.data.len()).rev() {
                        let mut yi = y.data[i];
                        let mut j = 0;
                        while j < ::num::big::digit::BITS {
                            if i != y.data.len() - 1 || j != 0 {
                                zz = montgomery(&z, &z, m, mr.n0inv, num_words);
                                z = montgomery(&zz, &zz, m, mr.n0inv, num_words);
                                zz = montgomery(&z, &z, m, mr.n0inv, num_words);
                                z = montgomery(&zz, &zz, m, mr.n0inv, num_words);
                            }
                            zz = montgomery(
                                &z,
                                &powers[(yi >> (::num::big::digit::BITS - n)) as usize],
                                m,
                                mr.n0inv,
                                num_words,
                            );
                            mem::swap(&mut z, &mut zz);
                            yi <<= n;
                            j += n;
                        }
                    }
                   
                    zz = montgomery(&z, &one, m, mr.n0inv, num_words);

                    zz.normalize();
                   
                   
                    if zz >= *m {
                       
                       
                       
                       
                       
                       
                       
                        zz -= m;
                        if zz >= *m {
                            zz %= m;
                        }
                    }
                    zz.normalize();
                    zz
                }
            } pub use self::iter::{U32Digits, U64Digits};

            pub mod power
            {
                /*!
                */
                use ::
                {
                    num::
                    {
                        big::digit::{self, BigDigit},
                        integers::Integer,
                        traits::{One, Pow, ToPrimitive, Zero},
                    },
                    *,
                };
                use super::monty::monty_modpow;
                use super::BigUint;
                /*
                */
                impl Pow<&BigUint> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn pow(self, exp: &BigUint) -> BigUint {
                        if self.is_one() || exp.is_zero() {
                            BigUint::one()
                        } else if self.is_zero() {
                            Self::ZERO
                        } else if let Some(exp) = exp.to_u64() {
                            self.pow(exp)
                        } else if let Some(exp) = exp.to_u128() {
                            self.pow(exp)
                        } else {
                           
                           
                            panic!("memory overflow")
                        }
                    }
                }
                impl Pow<BigUint> for BigUint
                {
                    type Output = BigUint;

                    #[inline] fn pow(self, exp: BigUint) -> BigUint {
                        Pow::pow(self, &exp)
                    }
                }
                impl Pow<&BigUint> for &BigUint
                {
                    type Output = BigUint;

                    #[inline] fn pow(self, exp: &BigUint) -> BigUint {
                        if self.is_one() || exp.is_zero() {
                            BigUint::one()
                        } else if self.is_zero() {
                            BigUint::ZERO
                        } else {
                            self.clone().pow(exp)
                        }
                    }
                }
                impl Pow<BigUint> for &BigUint
                {
                    type Output = BigUint;

                    #[inline] fn pow(self, exp: BigUint) -> BigUint {
                        Pow::pow(self, &exp)
                    }
                }
                macro_rules! pow_impl 
                {
                    ($T:ty) => {
                        impl Pow<$T> for BigUint {
                            type Output = BigUint;

                            fn pow(self, mut exp: $T) -> BigUint {
                                if exp == 0 {
                                    return BigUint::one();
                                }
                                let mut base = self;

                                while exp & 1 == 0 {
                                    base = &base * &base;
                                    exp >>= 1;
                                }
                                if exp == 1 {
                                    return base;
                                }
                                let mut acc = base.clone();
                                while exp > 1 {
                                    exp >>= 1;
                                    base = &base * &base;
                                    if exp & 1 == 1 {
                                        acc *= &base;
                                    }
                                }
                                acc
                            }
                        }
                        impl Pow<&$T> for BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn pow(self, exp: &$T) -> BigUint {
                                Pow::pow(self, *exp)
                            }
                        }
                        impl Pow<$T> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn pow(self, exp: $T) -> BigUint {
                                if exp == 0 {
                                    return BigUint::one();
                                }
                                Pow::pow(self.clone(), exp)
                            }
                        }
                        impl Pow<&$T> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn pow(self, exp: &$T) -> BigUint {
                                Pow::pow(self, *exp)
                            }
                        }
                    };
                }
                pow_impl!(u8);
                pow_impl!(u16);
                pow_impl!(u32);
                pow_impl!(u64);
                pow_impl!(usize);
                pow_impl!(u128);

                pub fn modpow(x: &BigUint, exponent: &BigUint, modulus: &BigUint) -> BigUint 
                {
                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );

                    if modulus.is_odd() {
                       
                        monty_modpow(x, exponent, modulus)
                    } else {
                       
                        plain_modpow(x, &exponent.data, modulus)
                    }
                }
                pub fn plain_modpow(base: &BigUint, exp_data: &[BigDigit], modulus: &BigUint) -> BigUint 
                {
                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );

                    let i = match exp_data.iter().position(|&r| r != 0) {
                        None => return BigUint::one(),
                        Some(i) => i,
                    };

                    let mut base = base % modulus;
                    for _ in 0..i {
                        for _ in 0..::num::big::digit::BITS {
                            base = &base * &base % modulus;
                        }
                    }
                    let mut r = exp_data[i];
                    let mut b = 0u8;
                    while r.is_even() {
                        base = &base * &base % modulus;
                        r >>= 1;
                        b += 1;
                    }
                    let mut exp_iter = exp_data[i + 1..].iter();
                    if exp_iter.len() == 0 && r.is_one() {
                        return base;
                    }
                    let mut acc = base.clone();
                    r >>= 1;
                    b += 1;

                    {
                        let mut unit = |exp_is_odd| {
                            base = &base * &base % modulus;
                            if exp_is_odd {
                                acc *= &base;
                                acc %= modulus;
                            }
                        };

                        if let Some(&last) = exp_iter.next_back() {
                           
                            for _ in b..::num::big::digit::BITS {
                                unit(r.is_odd());
                                r >>= 1;
                            }
                           
                            for &r in exp_iter {
                                let mut r = r;
                                for _ in 0..::num::big::digit::BITS {
                                    unit(r.is_odd());
                                    r >>= 1;
                                }
                            }
                            r = last;
                        }
                        debug_assert_ne!(r, 0);
                        while !r.is_zero() {
                            unit(r.is_odd());
                            r >>= 1;
                        }
                    }
                    acc
                }
            }
            pub mod shift
            {
                /*!
                */
                use ::
                {
                    borrow::{ Cow },
                    num::
                    {
                        big::{ digit },
                        traits::{PrimInt, Zero},
                    },
                    ops::{Shl, ShlAssign, Shr, ShrAssign},
                    vec::{ Vec },
                    *,
                };

                use super::{biguint_from_vec, BigUint};
                /*
                */
                #[inline] fn biguint_shl<T: PrimInt>(n: Cow<'_, BigUint>, shift: T) -> BigUint {
                    if shift < T::zero() {
                        panic!("attempt to shift left with negative");
                    }
                    if n.is_zero() {
                        return n.into_owned();
                    }
                    let bits = T::from(::num::big::digit::BITS).unwrap();
                    let digits = (shift / bits).to_usize().expect("capacity overflow");
                    let shift = (shift % bits).to_u8().unwrap();
                    biguint_shl2(n, digits, shift)
                }
                fn biguint_shl2(n: Cow<'_, BigUint>, digits: usize, shift: u8) -> BigUint {
                    let mut data = match digits {
                        0 => n.into_owned().data,
                        _ => {
                            let len = digits.saturating_add(n.data.len() + 1);
                            let mut data = Vec::with_capacity(len);
                            data.resize(digits, 0);
                            data.extend(n.data.iter());
                            data
                        }
                    };

                    if shift > 0 {
                        let mut carry = 0;
                        let carry_shift = ::num::big::digit::BITS - shift;
                        for elem in data[digits..].iter_mut() {
                            let new_carry = *elem >> carry_shift;
                            *elem = (*elem << shift) | carry;
                            carry = new_carry;
                        }
                        if carry != 0 {
                            data.push(carry);
                        }
                    }
                    biguint_from_vec(data)
                }
                #[inline] fn biguint_shr<T: PrimInt>(n: Cow<'_, BigUint>, shift: T) -> BigUint {
                    if shift < T::zero() {
                        panic!("attempt to shift right with negative");
                    }
                    if n.is_zero() {
                        return n.into_owned();
                    }
                    let bits = T::from(::num::big::digit::BITS).unwrap();
                    let digits = (shift / bits).to_usize().unwrap_or(usize::MAX);
                    let shift = (shift % bits).to_u8().unwrap();
                    biguint_shr2(n, digits, shift)
                }
                fn biguint_shr2(n: Cow<'_, BigUint>, digits: usize, shift: u8) -> BigUint {
                    if digits >= n.data.len() {
                        let mut n = n.into_owned();
                        n.set_zero();
                        return n;
                    }
                    let mut data = match n {
                        Cow::Borrowed(n) => n.data[digits..].to_vec(),
                        Cow::Owned(mut n) => {
                            n.data.drain(..digits);
                            n.data
                        }
                    };

                    if shift > 0 {
                        let mut borrow = 0;
                        let borrow_shift = ::num::big::digit::BITS - shift;
                        for elem in data.iter_mut().rev() {
                            let new_borrow = *elem << borrow_shift;
                            *elem = (*elem >> shift) | borrow;
                            borrow = new_borrow;
                        }
                    }
                    biguint_from_vec(data)
                }
                macro_rules! impl_shift {
                    (@ref $Shx:ident :: $shx:ident, $ShxAssign:ident :: $shx_assign:ident, $rhs:ty) => {
                        impl $Shx<&$rhs> for BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn $shx(self, rhs: &$rhs) -> BigUint {
                                $Shx::$shx(self, *rhs)
                            }
                        }
                        impl $Shx<&$rhs> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn $shx(self, rhs: &$rhs) -> BigUint {
                                $Shx::$shx(self, *rhs)
                            }
                        }
                        impl $ShxAssign<&$rhs> for BigUint {
                            #[inline]
                            fn $shx_assign(&mut self, rhs: &$rhs) {
                                $ShxAssign::$shx_assign(self, *rhs);
                            }
                        }
                    };
                    ($($rhs:ty),+) => {$(
                        impl Shl<$rhs> for BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn shl(self, rhs: $rhs) -> BigUint {
                                biguint_shl(Cow::Owned(self), rhs)
                            }
                        }
                        impl Shl<$rhs> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn shl(self, rhs: $rhs) -> BigUint {
                                biguint_shl(Cow::Borrowed(self), rhs)
                            }
                        }
                        impl ShlAssign<$rhs> for BigUint {
                            #[inline]
                            fn shl_assign(&mut self, rhs: $rhs) {
                                let n = mem::replace(self, Self::ZERO);
                                *self = n << rhs;
                            }
                        }
                        impl_shift! { @ref Shl::shl, ShlAssign::shl_assign, $rhs }
                        impl Shr<$rhs> for BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn shr(self, rhs: $rhs) -> BigUint {
                                biguint_shr(Cow::Owned(self), rhs)
                            }
                        }
                        impl Shr<$rhs> for &BigUint {
                            type Output = BigUint;

                            #[inline]
                            fn shr(self, rhs: $rhs) -> BigUint {
                                biguint_shr(Cow::Borrowed(self), rhs)
                            }
                        }
                        impl ShrAssign<$rhs> for BigUint {
                            #[inline]
                            fn shr_assign(&mut self, rhs: $rhs) {
                                let n = mem::replace(self, Self::ZERO);
                                *self = n >> rhs;
                            }
                        }
                        impl_shift! { @ref Shr::shr, ShrAssign::shr_assign, $rhs }
                    )*};
                }
                impl_shift! { u8, u16, u32, u64, u128, usize }
                impl_shift! { i8, i16, i32, i64, i128, isize }
            }
            /// A big unsigned integer type.
            pub struct BigUint
            {
                data: Vec<BigDigit>,
            }
            
            impl Clone for BigUint
            {
                #[inline] fn clone(&self) -> Self {
                    BigUint {
                        data: self.data.clone(),
                    }
                }
                #[inline] fn clone_from(&mut self, other: &Self) {
                    self.data.clone_from(&other.data);
                }
            }
            
            impl hash::Hash for BigUint
            {
                #[inline] fn hash<H: hash::Hasher>(&self, state: &mut H) {
                    debug_assert!(self.data.last() != Some(&0));
                    self.data.hash(state);
                }
            }
            
            impl PartialEq for BigUint
            {
                #[inline] fn eq(&self, other: &BigUint) -> bool {
                    debug_assert!(self.data.last() != Some(&0));
                    debug_assert!(other.data.last() != Some(&0));
                    self.data == other.data
                }
            }
            
            impl Eq for BigUint {}
            
            impl PartialOrd for BigUint
            {
                #[inline] fn partial_cmp(&self, other: &BigUint) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }
            
            impl Ord for BigUint
            {
                #[inline] fn cmp(&self, other: &BigUint) -> Ordering {
                    cmp_slice(&self.data[..], &other.data[..])
                }
            }
            #[inline] fn cmp_slice(a: &[BigDigit], b: &[BigDigit]) -> Ordering
            {
                debug_assert!(a.last() != Some(&0));
                debug_assert!(b.last() != Some(&0));

                match Ord::cmp(&a.len(), &b.len()) {
                    Ordering::Equal => Iterator::cmp(a.iter().rev(), b.iter().rev()),
                    other => other,
                }
            }
            
            impl Default for BigUint
            {
                #[inline] fn default() -> BigUint {
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
            
            impl Zero for BigUint
            {
                #[inline] fn zero() -> BigUint {
                    Self::ZERO
                }
                #[inline] fn set_zero(&mut self) {
                    self.data.clear();
                }
                #[inline] fn is_zero(&self) -> bool {
                    self.data.is_empty()
                }
            }
            
            impl ConstZero for BigUint {
               
                const ZERO: Self = Self::ZERO;
            }
            
            impl One for BigUint
            {
                #[inline] fn one() -> BigUint {
                    BigUint { data: vec![1] }
                }
                #[inline] fn set_one(&mut self) {
                    self.data.clear();
                    self.data.push(1);
                }
                #[inline] fn is_one(&self) -> bool {
                    self.data[..] == [1]
                }
            }
            
            impl Unsigned for BigUint {}
            
            impl Integer for BigUint
            {
                #[inline] fn div_rem(&self, other: &BigUint) -> (BigUint, BigUint) {
                    division::div_rem_ref(self, other)
                }
                #[inline] fn div_floor(&self, other: &BigUint) -> BigUint {
                    let (d, _) = division::div_rem_ref(self, other);
                    d
                }
                #[inline] fn mod_floor(&self, other: &BigUint) -> BigUint {
                    let (_, m) = division::div_rem_ref(self, other);
                    m
                }
                #[inline] fn div_mod_floor(&self, other: &BigUint) -> (BigUint, BigUint) {
                    division::div_rem_ref(self, other)
                }
                #[inline] fn div_ceil(&self, other: &BigUint) -> BigUint {
                    let (d, m) = division::div_rem_ref(self, other);
                    if m.is_zero() {
                        d
                    } else {
                        d + 1u32
                    }
                }
                /// Calculates the Greatest Common Divisor (GCD) of the number and `other`.
                #[inline] fn gcd(&self, other: &Self) -> Self
                {
                    #[inline] fn twos(x: &BigUint) -> u64 {
                        x.trailing_zeros().unwrap_or(0)
                    }
                   
                    if self.is_zero() {
                        return other.clone();
                    }
                    if other.is_zero() {
                        return self.clone();
                    }
                    let mut m = self.clone();
                    let mut n = other.clone();

                   
                    let shift = cmp::min(twos(&n), twos(&m));

                   
                   
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
                #[inline] fn lcm(&self, other: &BigUint) -> BigUint
                {
                    if self.is_zero() && other.is_zero() {
                        Self::ZERO
                    } else {
                        self / self.gcd(other) * other
                    }
                }
                /// Calculates the Greatest Common Divisor (GCD) and Lowest Common Multiple (LCM) together.
                #[inline] fn gcd_lcm(&self, other: &Self) -> (Self, Self)
                {
                    let gcd = self.gcd(other);
                    let lcm = if gcd.is_zero() {
                        Self::ZERO
                    } else {
                        self / &gcd * other
                    };
                    (gcd, lcm)
                }
                /// Deprecated, use `is_multiple_of` instead.
                #[inline] fn divides(&self, other: &BigUint) -> bool {
                    self.is_multiple_of(other)
                }
                /// Returns `true` if the number is a multiple of `other`.
                #[inline] fn is_multiple_of(&self, other: &BigUint) -> bool {
                    if other.is_zero() {
                        return self.is_zero();
                    }
                    (self % other).is_zero()
                }
                /// Returns `true` if the number is divisible by `2`.
                #[inline] fn is_even(&self) -> bool
                {                   
                    match self.data.first() {
                        Some(x) => x.is_even(),
                        None => true,
                    }
                }
                /// Returns `true` if the number is not divisible by `2`.
                #[inline] fn is_odd(&self) -> bool {
                    !self.is_even()
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
            #[inline] fn fixpoint<F>(mut x: BigUint, max_bits: u64, f: F) -> BigUint where
                F: Fn(&BigUint) -> BigUint,
            {
                let mut xn = f(&x);

               
               
                while x < xn {
                   
                   
                   
                    x = if xn.bits() > max_bits {
                        BigUint::one() << max_bits
                    } else {
                        xn
                    };
                    xn = f(&x);
                }
               
                while x > xn {
                    x = xn;
                    xn = f(&x);
                }
                x
            }
            
            impl Roots for BigUint
            {  
                fn nth_root(&self, n: u32) -> Self {
                    assert!(n > 0, "root degree n must be at least 1");

                    if self.is_zero() || self.is_one() {
                        return self.clone();
                    }
                    match n {
                       
                        1 => return self.clone(),
                        2 => return self.sqrt(),
                        3 => return self.cbrt(),
                        _ => (),
                    }
                   
                    let bits = self.bits();
                    let n64 = u64::from(n);
                    if bits <= n64 {
                        return BigUint::one();
                    }
                   
                    if let Some(x) = self.to_u64() {
                        return x.nth_root(n).into();
                    }
                    let max_bits = bits / n64 + 1;

                    #[cfg(feature = "std")]
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use ::num::traits::FromPrimitive;

                           
                            BigUint::from_f64((f.ln() / f64::from(n)).exp()).unwrap()
                        }
                        _ => {
                           
                           
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
               
               
                fn sqrt(&self) -> Self {
                    if self.is_zero() || self.is_one() {
                        return self.clone();
                    }
                   
                    if let Some(x) = self.to_u64() {
                        return x.sqrt().into();
                    }
                    let bits = self.bits();
                    let max_bits = bits / 2 + 1;

                    #[cfg(feature = "std")]
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use ::num::traits::FromPrimitive;

                           
                            BigUint::from_f64(f.sqrt()).unwrap()
                        }
                        _ => {
                           
                           
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
                   
                    if let Some(x) = self.to_u64() {
                        return x.cbrt().into();
                    }
                    let bits = self.bits();
                    let max_bits = bits / 3 + 1;

                    #[cfg(feature = "std")]
                    let guess = match self.to_f64() {
                        Some(f) if f.is_finite() => {
                            use ::num::traits::FromPrimitive;

                           
                            BigUint::from_f64(f.cbrt()).unwrap()
                        }
                        _ => {
                           
                           
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
            pub trait ToBigUint
            {
                /// Converts the value of `self` to a [`BigUint`].
                fn to_biguint(&self) -> Option<BigUint>;
            }
            /// Creates and initializes a [`BigUint`].
            #[inline] pub fn biguint_from_vec(digits: Vec<BigDigit>) -> BigUint
            {
                BigUint { data: digits }.normalized()
            }
            
            impl BigUint
            {
                /// A constant `BigUint` with value 0, useful for static initialization.
                pub const ZERO: Self = BigUint { data: Vec::new() };
                /// Creates and initializes a [`BigUint`].
                #[inline] pub fn new(digits: Vec<u32>) -> BigUint
                {
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
                #[inline] pub fn from_slice(slice: &[u32]) -> BigUint
                {
                    let mut big = Self::ZERO;
                    big.assign_from_slice(slice);
                    big
                }
                /// Assign a value to a [`BigUint`].
                #[inline] pub fn assign_from_slice(&mut self, slice: &[u32])
                {
                    self.data.clear();

                    cfg_digit_expr!(
                        self.data.extend_from_slice(slice),
                        self.data.extend(slice.chunks(2).map(u32_chunk_to_u64))
                    );

                    self.normalize();
                }
                /// Creates and initializes a [`BigUint`].
                #[inline] pub fn from_bytes_be(bytes: &[u8]) -> BigUint
                {
                    if bytes.is_empty() {
                        Self::ZERO
                    } else {
                        let mut v = bytes.to_vec();
                        v.reverse();
                        BigUint::from_bytes_le(&v)
                    }
                }
                /// Creates and initializes a [`BigUint`].
                #[inline] pub fn from_bytes_le(bytes: &[u8]) -> BigUint
                {
                    if bytes.is_empty() {
                        Self::ZERO
                    } else {
                        convert::from_bitwise_digits_le(bytes, 8)
                    }
                }
                /// Creates and initializes a [`BigUint`].
                #[inline] pub fn parse_bytes(buf: &[u8], radix: u32) -> Option<BigUint>
                {
                    let s = str::from_utf8(buf).ok()?;
                    BigUint::from_str_radix(s, radix).ok()
                }
                /// Creates and initializes a [`BigUint`].
                pub fn from_radix_be(buf: &[u8], radix: u32) -> Option<BigUint>
                {
                    convert::from_radix_be(buf, radix)
                }
                /// Creates and initializes a [`BigUint`].
                pub fn from_radix_le(buf: &[u8], radix: u32) -> Option<BigUint>
                {
                    convert::from_radix_le(buf, radix)
                }
                /// Returns the byte representation of the [`BigUint`] in big-endian byte order.
                #[inline] pub fn to_bytes_be(&self) -> Vec<u8> {
                    let mut v = self.to_bytes_le();
                    v.reverse();
                    v
                }
                /// Returns the byte representation of the [`BigUint`] in little-endian byte order.
                #[inline] pub fn to_bytes_le(&self) -> Vec<u8> {
                    if self.is_zero() {
                        vec![0]
                    } else {
                        convert::to_bitwise_digits_le(self, 8)
                    }
                }
                /// Returns the `u32` digits representation of the [`BigUint`] ordered least significant digit first.
                #[inline] pub fn to_u32_digits(&self) -> Vec<u32> {
                    self.iter_u32_digits().collect()
                }
                /// Returns the `u64` digits representation of the [`BigUint`] ordered least significant digit first.
                #[inline] pub fn to_u64_digits(&self) -> Vec<u64> {
                    self.iter_u64_digits().collect()
                }
                /// Returns an iterator of `u32` digits representation of the 
                /// [`BigUint`] ordered least significant digit first.
                #[inline] pub fn iter_u32_digits(&self) -> U32Digits<'_> {
                    U32Digits::new(self.data.as_slice())
                }
                /// Returns an iterator of `u64` digits representation of the
                /// [`BigUint`] ordered least significant digit first.
                #[inline] pub fn iter_u64_digits(&self) -> U64Digits<'_> {
                    U64Digits::new(self.data.as_slice())
                }
                /// Returns the integer formatted as a string in the given radix.
                #[inline] pub fn to_str_radix(&self, radix: u32) -> String {
                    let mut v = to_str_radix_reversed(self, radix);
                    v.reverse();
                    unsafe { String::from_utf8_unchecked(v) }
                }
                /// Returns the integer in the requested base in big-endian digit order.
                #[inline] pub fn to_radix_be(&self, radix: u32) -> Vec<u8> {
                    let mut v = convert::to_radix_le(self, radix);
                    v.reverse();
                    v
                }
                /// Returns the integer in the requested base in little-endian digit order.
                #[inline] pub fn to_radix_le(&self, radix: u32) -> Vec<u8> {
                    convert::to_radix_le(self, radix)
                }
                /// Determines the fewest bits necessary to express the [`BigUint`].
                #[inline] pub fn bits(&self) -> u64 {
                    if self.is_zero() {
                        return 0;
                    }
                    let zeros: u64 = self.data.last().unwrap().leading_zeros().into();
                    self.data.len() as u64 * u64::from(::num::big::digit::BITS) - zeros
                }
                /// Strips off trailing zero bigdigits.
                #[inline] fn normalize(&mut self) {
                    if let Some(&0) = self.data.last() {
                        let len = self.data.iter().rposition(|&d| d != 0).map_or(0, |i| i + 1);
                        self.data.truncate(len);
                    }
                    if self.data.len() < self.data.capacity() / 4 {
                        self.data.shrink_to_fit();
                    }
                }
                /// Returns a normalized [`BigUint`].
                #[inline] fn normalized(mut self) -> BigUint {
                    self.normalize();
                    self
                }
                /// Returns `self ^ exponent`.
                pub fn pow(&self, exponent: u32) -> Self {
                    Pow::pow(self, exponent)
                }
                /// Returns `(self ^ exponent) % modulus`.
                pub fn modpow(&self, exponent: &Self, modulus: &Self) -> Self {
                    power::modpow(self, exponent, modulus)
                }
                /// Returns the modular multiplicative inverse if it exists, otherwise `None`.
                pub fn modinv(&self, modulus: &Self) -> Option<Self> {
                   
                   
                   

                    assert!(
                        !modulus.is_zero(),
                        "attempt to calculate with zero modulus!"
                    );
                    if modulus.is_one() {
                        return Some(Self::zero());
                    }
                    let mut r0;
                    let mut r1 = self % modulus;
                    let mut t0;
                    let mut t1;

                   
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
                /// Returns the truncated principal square root of `self`
                pub fn sqrt(&self) -> Self {
                    Roots::sqrt(self)
                }
                /// Returns the truncated principal cube root of `self`
                pub fn cbrt(&self) -> Self {
                    Roots::cbrt(self)
                }
                /// Returns the truncated principal `n`th root of `self`
                pub fn nth_root(&self, n: u32) -> Self {
                    Roots::nth_root(self, n)
                }
                /// Returns the number of least-significant bits that are zero, 
                /// or `None` if the entire number is zero.
                pub fn trailing_zeros(&self) -> Option<u64> {
                    let i = self.data.iter().position(|&digit| digit != 0)?;
                    let zeros: u64 = self.data[i].trailing_zeros().into();
                    Some(i as u64 * u64::from(::num::big::digit::BITS) + zeros)
                }
                /// Returns the number of least-significant bits that are ones.
                pub fn trailing_ones(&self) -> u64 {
                    if let Some(i) = self.data.iter().position(|&digit| !digit != 0) {
                        let ones: u64 = self.data[i].trailing_ones().into();
                        i as u64 * u64::from(::num::big::digit::BITS) + ones
                    } else {
                        self.data.len() as u64 * u64::from(::num::big::digit::BITS)
                    }
                }
                /// Returns the number of one bits.
                pub fn count_ones(&self) -> u64 {
                    self.data.iter().map(|&d| u64::from(d.count_ones())).sum()
                }
                /// Returns whether the bit in the given position is set
                pub fn bit(&self, bit: u64) -> bool
                {
                    let bits_per_digit = u64::from(::num::big::digit::BITS);
                    if let Some(digit_index) = (bit / bits_per_digit).to_usize() {
                        if let Some(digit) = self.data.get(digit_index) {
                            let bit_mask = (1 as BigDigit) << (bit % bits_per_digit);
                            return (digit & bit_mask) != 0;
                        }
                    }
                    false
                }
                /// Sets or clears the bit in the given position.
                pub fn set_bit(&mut self, bit: u64, value: bool)
                {
                    let bits_per_digit = u64::from(::num::big::digit::BITS);
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
                       
                        self.normalize();
                    }
                }
            }
            
            impl ::num::traits::FromBytes for BigUint {
                type Bytes = [u8];

                fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_bytes_be(bytes)
                }
                fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                    Self::from_bytes_le(bytes)
                }
            }
            
            impl ::num::traits::ToBytes for BigUint {
                type Bytes = Vec<u8>;

                fn to_be_bytes(&self) -> Self::Bytes {
                    self.to_bytes_be()
                }
                fn to_le_bytes(&self) -> Self::Bytes {
                    self.to_bytes_le()
                }
            }
            pub trait IntDigits {
                fn digits(&self) -> &[BigDigit];
                fn digits_mut(&mut self) -> &mut Vec<BigDigit>;
                fn normalize(&mut self);
                fn capacity(&self) -> usize;
                fn len(&self) -> usize;
            }
            
            impl IntDigits for BigUint
            {
                #[inline] fn digits(&self) -> &[BigDigit] {
                    &self.data
                }
                #[inline] fn digits_mut(&mut self) -> &mut Vec<BigDigit> {
                    &mut self.data
                }
                #[inline] fn normalize(&mut self) {
                    self.normalize();
                }
                #[inline] fn capacity(&self) -> usize {
                    self.data.capacity()
                }
                #[inline] fn len(&self) -> usize {
                    self.data.len()
                }
            }
            /// Convert a `u32` chunk (len is either 1 or 2) to a single `u64` digit
            #[inline] fn u32_chunk_to_u64(chunk: &[u32]) -> u64 {
               
                let mut digit = chunk[0] as u64;
                if let Some(&hi) = chunk.get(1) {
                    digit |= (hi as u64) << 32;
                }
                digit
            }
            cfg_32_or_test!(
                /// Combine four `u32`s into a single `u128`.
                #[inline] fn u32_to_u128(a: u32, b: u32, c: u32, d: u32) -> u128 {
                    u128::from(d) | (u128::from(c) << 32) | (u128::from(b) << 64) | (u128::from(a) << 96)
                }
            );

            cfg_32_or_test!(
                /// Split a single `u128` into four `u32`.
                #[inline] fn u32_from_u128(n: u128) -> (u32, u32, u32, u32) {
                    (
                        (n >> 96) as u32,
                        (n >> 64) as u32,
                        (n >> 32) as u32,
                        n as u32,
                    )
                }
            );
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
        pub struct ParseBigIntError
        {
            kind: BigIntErrorKind,
        }

        #[derive(Debug, Clone, PartialEq, Eq)]
        enum BigIntErrorKind
        {
            Empty,
            InvalidDigit,
        }

        impl ParseBigIntError
        {
            fn __description(&self) -> &str {
                use ::num::big::BigIntErrorKind::*;
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

        impl fmt::Display for ParseBigIntError
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {self.__description().fmt(f)
            }
        }
        
        impl ::error::Error for ParseBigIntError
        {
            fn description(&self) -> &str {
                self.__description()
            }
        }
        /// The error type returned when a checked conversion regarding big integer fails.
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub struct TryFromBigIntError<T>
        {
            original: T,
        }

        impl<T> TryFromBigIntError<T>
        {
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
        
        impl<T> ::error::Error for TryFromBigIntError<T> where
        T: fmt::Debug,
        {
            fn description(&self) -> &str {
                self.__description()
            }
        }

        impl<T> fmt::Display for TryFromBigIntError<T>
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.__description().fmt(f) }
        }

        pub use self::uint::BigUint;
        pub use self::uint::ToBigUint;
        pub use self::uint::U32Digits;
        pub use self::uint::U64Digits;

        pub use self::int::BigInt;
        pub use self::int::Sign;
        pub use self::int::ToBigInt;
        
        pub use self::rand::{ RandBigInt, RandomBits, UniformBigInt, UniformBigUint };

        mod digit 
        {
           
            cfg_digit!
            (
                pub type BigDigit = u32;
                pub type BigDigit = u64;
            );
            
            cfg_digit!
            (
                pub type DoubleBigDigit = u64;
                pub type DoubleBigDigit = u128;
            );

            pub const BITS: u8 = BigDigit::BITS as u8;
            pub const HALF_BITS: u8 = BITS / 2;
            pub const HALF: BigDigit = (1 << HALF_BITS) - 1;
            pub const MAX: BigDigit = BigDigit::MAX;
            const LO_MASK: DoubleBigDigit = MAX as DoubleBigDigit;

            #[inline] fn get_hi(n: DoubleBigDigit) -> BigDigit 
            {
                (n >> BITS) as BigDigit
            }
            #[inline] fn get_lo(n: DoubleBigDigit) -> BigDigit 
            {
                (n & LO_MASK) as BigDigit
            }
            /// Split one [`DoubleBigDigit`] into two [`BigDigit`]s.
            #[inline] pub fn from_doublebigdigit(n: DoubleBigDigit) -> (BigDigit, BigDigit) 
            {
                (get_hi(n), get_lo(n))
            }
            /// Join two [`BigDigit`]s into one [`DoubleBigDigit`].
            #[inline] pub fn to_doublebigdigit(hi: BigDigit, lo: BigDigit) -> DoubleBigDigit 
            {
                DoubleBigDigit::from(lo) | (DoubleBigDigit::from(hi) << BITS)
            }
        }
    }
    /*
    */
    pub mod rational
    {
        //! Rational numbers
        use ::
        {
            error::{ Error },
            fmt::{ Binary, Display, Formatter, LowerExp, LowerHex, Octal, UpperExp, UpperHex },
            hash::{Hash, Hasher},
            num::
            {
                big::{ BigInt, BigUint, Sign, ToBigInt },
                integers::{ Integer },
                traits::
                {
                    float::FloatCore,
                    Bounded, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, ConstOne, ConstZero, FromPrimitive, Inv,
                    Num, NumCast, One, Pow, Signed, ToPrimitive, Unsigned, Zero
                },
            },
            ops::{Add, Div, Mul, Neg, Rem, ShlAssign, Sub},
            str::{ FromStr },
            *,
        };
        /*
        */
        pub mod pow
        {
            use ::
            {
                num::
                {
                    integers::{ Integer },
                    rational::{ Ratio },
                    traits::{ One, Pow },
                },
                *,
            };
            /*
            */
            macro_rules! pow_unsigned_impl {
                (@ $exp:ty) => {
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
            pow_unsigned_impl!(u8);
            pow_unsigned_impl!(u16);
            pow_unsigned_impl!(u32);
            pow_unsigned_impl!(u64);
            pow_unsigned_impl!(u128);
            pow_unsigned_impl!(usize);

            macro_rules! pow_signed_impl {
                (@ &'b BigInt, BigUint) => {
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
                (@ $exp:ty, $unsigned:ty) => {
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
            pow_signed_impl!(i8, u8);
            pow_signed_impl!(i16, u16);
            pow_signed_impl!(i32, u32);
            pow_signed_impl!(i64, u64);
            pow_signed_impl!(i128, u128);
            pow_signed_impl!(isize, usize);

            #[cfg(feature = "num-bigint")]
            mod bigint {
                use super::*;
                use num_bigint::{BigInt, BigUint, Sign};

                impl<T: Clone + Integer + for<'b> Pow<&'b BigUint, Output = T>> Pow<BigUint> for Ratio<T> {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigUint) -> Ratio<T> {
                        Pow::pow(self, &expon)
                    }
                }
                impl<'a, T: Clone + Integer> Pow<BigUint> for &'a Ratio<T> where
                    &'a T: for<'b> Pow<&'b BigUint, Output = T>,
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigUint) -> Ratio<T> {
                        Pow::pow(self, &expon)
                    }
                }
                impl<'b, T: Clone + Integer + Pow<&'b BigUint, Output = T>> Pow<&'b BigUint> for Ratio<T> {
                    pow_unsigned_impl!(@ &'b BigUint);
                }
                impl<'a, 'b, T: Clone + Integer> Pow<&'b BigUint> for &'a Ratio<T> where
                    &'a T: Pow<&'b BigUint, Output = T>,
                {
                    pow_unsigned_impl!(@ &'b BigUint);
                }
                impl<T: Clone + Integer + for<'b> Pow<&'b BigUint, Output = T>> Pow<BigInt> for Ratio<T> {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigInt) -> Ratio<T> {
                        Pow::pow(self, &expon)
                    }
                }
                impl<'a, T: Clone + Integer> Pow<BigInt> for &'a Ratio<T> where
                    &'a T: for<'b> Pow<&'b BigUint, Output = T>,
                {
                    type Output = Ratio<T>;
                    #[inline] fn pow(self, expon: BigInt) -> Ratio<T> {
                        Pow::pow(self, &expon)
                    }
                }
                impl<'b, T: Clone + Integer + Pow<&'b BigUint, Output = T>> Pow<&'b BigInt> for Ratio<T> {
                    pow_signed_impl!(@ &'b BigInt, BigUint);
                }
                impl<'a, 'b, T: Clone + Integer> Pow<&'b BigInt> for &'a Ratio<T> where
                    &'a T: Pow<&'b BigUint, Output = T>,
                {
                    pow_signed_impl!(@ &'b BigInt, BigUint);
                }
            }

        }
        /// Represents the ratio between two numbers.
        #[derive(Copy, Clone, Debug)]
        #[allow(missing_docs)]
        pub struct Ratio<T> {
            /// Numerator.
            numer: T,
            /// Denominator.
            denom: T,
        }
        /// Alias for a `Ratio` of machine-sized integers.
        #[deprecated(
            since = "0.4.0",
            note = "it's better to use a specific size, like `Rational32` or `Rational64`"
        )]
        pub type Rational = Ratio<isize>;
        /// Alias for a `Ratio` of 32-bit-sized integers.
        pub type Rational32 = Ratio<i32>;
        /// Alias for a `Ratio` of 64-bit-sized integers.
        pub type Rational64 = Ratio<i64>;
        /// Alias for arbitrary precision rationals.
        pub type BigRational = Ratio<BigInt>;
        /// These method are `const`.
        impl<T> Ratio<T>
        {
            /// Creates a `Ratio` without checking for `denom == 0` or reducing.
            #[inline] pub const fn new_raw(numer: T, denom: T) -> Ratio<T> {
                Ratio { numer, denom }
            }
            /// Deconstructs a `Ratio` into its numerator and denominator.
            #[inline] pub fn into_raw(self) -> (T, T) {
                (self.numer, self.denom)
            }
            /// Gets an immutable reference to the numerator.
            #[inline] pub const fn numer(&self) -> &T {
                &self.numer
            }
            /// Gets an immutable reference to the denominator.
            #[inline] pub const fn denom(&self) -> &T {
                &self.denom
            }
        }

        impl<T: Clone + Integer> Ratio<T>
        {
            /// Creates a new `Ratio`.
            #[inline] pub fn new(numer: T, denom: T) -> Ratio<T> {
                let mut ret = Ratio::new_raw(numer, denom);
                ret.reduce();
                ret
            }
            /// Creates a `Ratio` representing the integer `t`.
            #[inline] pub fn from_integer(t: T) -> Ratio<T> {
                Ratio::new_raw(t, One::one())
            }
            /// Converts to an integer, rounding towards zero.
            #[inline] pub fn to_integer(&self) -> T {
                self.trunc().numer
            }
            /// Returns true if the rational number is an integer (denominator is 1).
            #[inline] pub fn is_integer(&self) -> bool {
                self.denom.is_one()
            }
            /// Puts self into lowest terms, with `denom` > 0.
            fn reduce(&mut self) {
                if self.denom.is_zero() {
                    panic!("denominator == 0");
                }
                if self.numer.is_zero() {
                    self.denom.set_one();
                    return;
                }
                if self.numer == self.denom {
                    self.set_one();
                    return;
                }
                let g: T = self.numer.gcd(&self.denom);

               
               

                #[inline] fn replace_with<T: Zero>(x: &mut T, f: impl FnOnce(T) -> T) {
                    let y = ::mem::replace(x, T::zero());
                    *x = f(y);
                }
               
                replace_with(&mut self.numer, |x| x / g.clone());

               
                replace_with(&mut self.denom, |x| x / g);

               
                if self.denom < T::zero() {
                    replace_with(&mut self.numer, |x| T::zero() - x);
                    replace_with(&mut self.denom, |x| T::zero() - x);
                }
            }
            /// Returns a reduced copy of self.
            pub fn reduced(&self) -> Ratio<T> {
                let mut ret = self.clone();
                ret.reduce();
                ret
            }
            /// Returns the reciprocal.
            #[inline] pub fn recip(&self) -> Ratio<T> {
                self.clone().into_recip()
            }
            #[inline] fn into_recip(self) -> Ratio<T> {
                match self.numer.cmp(&T::zero()) {
                    cmp::Ordering::Equal => panic!("division by zero"),
                    cmp::Ordering::Greater => Ratio::new_raw(self.denom, self.numer),
                    cmp::Ordering::Less => Ratio::new_raw(T::zero() - self.denom, T::zero() - self.numer),
                }
            }
            /// Rounds towards minus infinity.
            #[inline] pub fn floor(&self) -> Ratio<T> {
                if *self < Zero::zero() {
                    let one: T = One::one();
                    Ratio::from_integer(
                        (self.numer.clone() - self.denom.clone() + one) / self.denom.clone(),
                    )
                } else {
                    Ratio::from_integer(self.numer.clone() / self.denom.clone())
                }
            }
            /// Rounds towards plus infinity.
            #[inline] pub fn ceil(&self) -> Ratio<T> {
                if *self < Zero::zero() {
                    Ratio::from_integer(self.numer.clone() / self.denom.clone())
                } else {
                    let one: T = One::one();
                    Ratio::from_integer(
                        (self.numer.clone() + self.denom.clone() - one) / self.denom.clone(),
                    )
                }
            }
            /// Rounds to the nearest integer. Rounds half-way cases away from zero.
            #[inline] pub fn round(&self) -> Ratio<T> {
                let zero: Ratio<T> = Zero::zero();
                let one: T = One::one();
                let two: T = one.clone() + one.clone();

               
                let mut fractional = self.fract();
                if fractional < zero {
                    fractional = zero - fractional
                };

               
               
               
                let half_or_larger = if fractional.denom.is_even() {
                    fractional.numer >= fractional.denom / two
                } else {
                    fractional.numer >= (fractional.denom / two) + one
                };

                if half_or_larger {
                    let one: Ratio<T> = One::one();
                    if *self >= Zero::zero() {
                        self.trunc() + one
                    } else {
                        self.trunc() - one
                    }
                } else {
                    self.trunc()
                }
            }
            /// Rounds towards zero.
            #[inline] pub fn trunc(&self) -> Ratio<T> {
                Ratio::from_integer(self.numer.clone() / self.denom.clone())
            }
            /// Returns the fractional part of a number, with division rounded towards zero.
            #[inline] pub fn fract(&self) -> Ratio<T> {
                Ratio::new_raw(self.numer.clone() % self.denom.clone(), self.denom.clone())
            }
            /// Raises the `Ratio` to the power of an exponent.
            #[inline] pub fn pow(&self, expon: i32) -> Ratio<T> where
                for<'a> &'a T: Pow<u32, Output = T>,
            {
                Pow::pow(self, expon)
            }
        }
        
        impl Ratio<BigInt>
        {
            /// Converts a float into a rational number.
            pub fn from_float<T: FloatCore>(f: T) -> Option<BigRational> {
                if !f.is_finite() {
                    return None;
                }
                let (mantissa, exponent, sign) = f.integer_decode();
                let bigint_sign = if sign == 1 { Sign::Plus } else { Sign::Minus };
                if exponent < 0 {
                    let one: BigInt = One::one();
                    let denom: BigInt = one << ((-exponent) as usize);
                    let numer: BigUint = FromPrimitive::from_u64(mantissa).unwrap();
                    Some(Ratio::new(BigInt::from_biguint(bigint_sign, numer), denom))
                } else {
                    let mut numer: BigUint = FromPrimitive::from_u64(mantissa).unwrap();
                    numer <<= exponent as usize;
                    Some(Ratio::from_integer(BigInt::from_biguint(
                        bigint_sign,
                        numer,
                    )))
                }
            }
        }

        impl<T: Clone + Integer> Default for Ratio<T>
        {
            /// Returns zero
            fn default() -> Self {
                Ratio::zero()
            }
        }
        
        impl<T> From<T> for Ratio<T> where
            T: Clone + Integer,
        {
            fn from(x: T) -> Ratio<T> {
                Ratio::from_integer(x)
            }
        }
        
        impl<T> From<(T, T)> for Ratio<T> where
            T: Clone + Integer,
        {
            fn from(pair: (T, T)) -> Ratio<T> {
                Ratio::new(pair.0, pair.1)
            }
        }
        
        impl<T: Clone + Integer> Ord for Ratio<T> 
        {
            #[inline] fn cmp(&self, other: &Self) -> cmp::Ordering {
               
                if self.denom == other.denom {
                    let ord = self.numer.cmp(&other.numer);
                    return if self.denom < T::zero() {
                        ord.reverse()
                    } else {
                        ord
                    };
                }
               
                if self.numer == other.numer {
                    if self.numer.is_zero() {
                        return cmp::Ordering::Equal;
                    }
                    let ord = self.denom.cmp(&other.denom);
                    return if self.numer < T::zero() {
                        ord
                    } else {
                        ord.reverse()
                    };
                }
               
               
               

               
                let (self_int, self_rem) = self.numer.div_mod_floor(&self.denom);
                let (other_int, other_rem) = other.numer.div_mod_floor(&other.denom);
                match self_int.cmp(&other_int) {
                    cmp::Ordering::Greater => cmp::Ordering::Greater,
                    cmp::Ordering::Less => cmp::Ordering::Less,
                    cmp::Ordering::Equal => {
                        match (self_rem.is_zero(), other_rem.is_zero()) {
                            (true, true) => cmp::Ordering::Equal,
                            (true, false) => cmp::Ordering::Less,
                            (false, true) => cmp::Ordering::Greater,
                            (false, false) => {
                               
                                let self_recip = Ratio::new_raw(self.denom.clone(), self_rem);
                                let other_recip = Ratio::new_raw(other.denom.clone(), other_rem);
                                self_recip.cmp(&other_recip).reverse()
                            }
                        }
                    }
                }
            }
        }

        impl<T: Clone + Integer> PartialOrd for Ratio<T>
        {
            #[inline] fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl<T: Clone + Integer> PartialEq for Ratio<T> 
        {
            #[inline] fn eq(&self, other: &Self) -> bool {
                self.cmp(other) == cmp::Ordering::Equal
            }
        }

        impl<T: Clone + Integer> Eq for Ratio<T> {}       
       
        impl<T: Clone + Integer + Hash> Hash for Ratio<T> 
        {
            fn hash<H: Hasher>(&self, state: &mut H) {
                recurse(&self.numer, &self.denom, state);

                fn recurse<T: Integer + Hash, H: Hasher>(numer: &T, denom: &T, state: &mut H) {
                    if !denom.is_zero() {
                        let (int, rem) = numer.div_mod_floor(denom);
                        int.hash(state);
                        recurse(denom, &rem, state);
                    } else {
                        denom.hash(state);
                    }
                }
            }
        }

        mod iter_sum_product
        {
            use ::
            {
                iter::{Product, Sum},
                num::
                {
                    integers::{ Integer },
                    rational::{ Ratio },
                    traits::{ One, Zero },
                },
                *,
            };

            impl<T: Integer + Clone> Sum for Ratio<T>
            {
                fn sum<I>(iter: I) -> Self
                where
                    I: Iterator<Item = Ratio<T>>,
                {
                    iter.fold(Self::zero(), |sum, num| sum + num)
                }
            }
            
            impl<'a, T: Integer + Clone> Sum<&'a Ratio<T>> for Ratio<T>
            {
                fn sum<I>(iter: I) -> Self
                where
                    I: Iterator<Item = &'a Ratio<T>>,
                {
                    iter.fold(Self::zero(), |sum, num| sum + num)
                }
            }
            
            impl<T: Integer + Clone> Product for Ratio<T>
            {
                fn product<I>(iter: I) -> Self
                where
                    I: Iterator<Item = Ratio<T>>,
                {
                    iter.fold(Self::one(), |prod, num| prod * num)
                }
            }
            
            impl<'a, T: Integer + Clone> Product<&'a Ratio<T>> for Ratio<T>
            {
                fn product<I>(iter: I) -> Self
                where
                    I: Iterator<Item = &'a Ratio<T>>,
                {
                    iter.fold(Self::one(), |prod, num| prod * num)
                }
            }
        }

        mod opassign
        {
            use ::
            {
                ops::{AddAssign, DivAssign, MulAssign, RemAssign, SubAssign},
                num::
                {
                    integers::Integer,
                    rational::{ Ratio },
                    traits::NumAssign,
                },
                *,
            };

            impl<T: Clone + Integer + NumAssign> AddAssign for Ratio<T>
            {
                fn add_assign(&mut self, other: Ratio<T>) {
                    if self.denom == other.denom {
                        self.numer += other.numer
                    } else {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer + rhs_numer;
                        self.denom = lcm;
                    }
                    self.reduce();
                }
            }
           
            impl<T: Clone + Integer + NumAssign> DivAssign for Ratio<T>
            {
                fn div_assign(&mut self, other: Ratio<T>) {
                    let gcd_ac = self.numer.gcd(&other.numer);
                    let gcd_bd = self.denom.gcd(&other.denom);
                    self.numer /= gcd_ac.clone();
                    self.numer *= other.denom / gcd_bd.clone();
                    self.denom /= gcd_bd;
                    self.denom *= other.numer / gcd_ac;
                    self.reduce();
                }
            }
           
            impl<T: Clone + Integer + NumAssign> MulAssign for Ratio<T>
            {
                fn mul_assign(&mut self, other: Ratio<T>) {
                    let gcd_ad = self.numer.gcd(&other.denom);
                    let gcd_bc = self.denom.gcd(&other.numer);
                    self.numer /= gcd_ad.clone();
                    self.numer *= other.numer / gcd_bc.clone();
                    self.denom /= gcd_bc;
                    self.denom *= other.denom / gcd_ad;
                    self.reduce();
                }
            }
            
            impl<T: Clone + Integer + NumAssign> RemAssign for Ratio<T>
            {
                fn rem_assign(&mut self, other: Ratio<T>) {
                    if self.denom == other.denom {
                        self.numer %= other.numer
                    } else {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer % rhs_numer;
                        self.denom = lcm;
                    }
                    self.reduce();
                }
            }
            
            impl<T: Clone + Integer + NumAssign> SubAssign for Ratio<T>
            {
                fn sub_assign(&mut self, other: Ratio<T>) {
                    if self.denom == other.denom {
                        self.numer -= other.numer
                    } else {
                        let lcm = self.denom.lcm(&other.denom);
                        let lhs_numer = self.numer.clone() * (lcm.clone() / self.denom.clone());
                        let rhs_numer = other.numer * (lcm.clone() / other.denom);
                        self.numer = lhs_numer - rhs_numer;
                        self.denom = lcm;
                    }
                    self.reduce();
                }
            }
           
            impl<T: Clone + Integer + NumAssign> AddAssign<T> for Ratio<T>
            {
                fn add_assign(&mut self, other: T) {
                    self.numer += self.denom.clone() * other;
                    self.reduce();
                }
            }
            
            impl<T: Clone + Integer + NumAssign> DivAssign<T> for Ratio<T>
            {
                fn div_assign(&mut self, other: T) {
                    let gcd = self.numer.gcd(&other);
                    self.numer /= gcd.clone();
                    self.denom *= other / gcd;
                    self.reduce();
                }
            }
            
            impl<T: Clone + Integer + NumAssign> MulAssign<T> for Ratio<T>
            {
                fn mul_assign(&mut self, other: T) {
                    let gcd = self.denom.gcd(&other);
                    self.denom /= gcd.clone();
                    self.numer *= other / gcd;
                    self.reduce();
                }
            }
           
            impl<T: Clone + Integer + NumAssign> RemAssign<T> for Ratio<T>
            {
                fn rem_assign(&mut self, other: T) {
                    self.numer %= self.denom.clone() * other;
                    self.reduce();
                }
            }
           
            impl<T: Clone + Integer + NumAssign> SubAssign<T> for Ratio<T>
            {
                fn sub_assign(&mut self, other: T) {
                    self.numer -= self.denom.clone() * other;
                    self.reduce();
                }
            }
            macro_rules! forward_op_assign {
                (impl $imp:ident, $method:ident) => {
                    impl<'a, T: Clone + Integer + NumAssign> $imp<&'a Ratio<T>> for Ratio<T> {
                        #[inline]
                        fn $method(&mut self, other: &Ratio<T>) {
                            self.$method(other.clone())
                        }
                    }
                    impl<'a, T: Clone + Integer + NumAssign> $imp<&'a T> for Ratio<T> {
                        #[inline]
                        fn $method(&mut self, other: &T) {
                            self.$method(other.clone())
                        }
                    }
                };
            }
            forward_op_assign!(impl AddAssign, add_assign);
            forward_op_assign!(impl DivAssign, div_assign);
            forward_op_assign!(impl MulAssign, mul_assign);
            forward_op_assign!(impl RemAssign, rem_assign);
            forward_op_assign!(impl SubAssign, sub_assign);
        }

        forward_all_binop!(impl Mul, mul);
       
        impl<T> Mul<Ratio<T>> for Ratio<T> where
            T: Clone + Integer,
        {
            type Output = Ratio<T>;
            #[inline] fn mul(self, rhs: Ratio<T>) -> Ratio<T> {
                let gcd_ad = self.numer.gcd(&rhs.denom);
                let gcd_bc = self.denom.gcd(&rhs.numer);
                Ratio::new(
                    self.numer / gcd_ad.clone() * (rhs.numer / gcd_bc.clone()),
                    self.denom / gcd_bc * (rhs.denom / gcd_ad),
                )
            }
        }
       
        impl<T> Mul<T> for Ratio<T> where
            T: Clone + Integer,
        {
            type Output = Ratio<T>;
            #[inline] fn mul(self, rhs: T) -> Ratio<T> {
                let gcd = self.denom.gcd(&rhs);
                Ratio::new(self.numer * (rhs / gcd.clone()), self.denom / gcd)
            }
        }

        forward_all_binop!(impl Div, div);
       
        impl<T> Div<Ratio<T>> for Ratio<T> where
            T: Clone + Integer,
        {
            type Output = Ratio<T>;

            #[inline] fn div(self, rhs: Ratio<T>) -> Ratio<T> {
                let gcd_ac = self.numer.gcd(&rhs.numer);
                let gcd_bd = self.denom.gcd(&rhs.denom);
                Ratio::new(
                    self.numer / gcd_ac.clone() * (rhs.denom / gcd_bd.clone()),
                    self.denom / gcd_bd * (rhs.numer / gcd_ac),
                )
            }
        }
       
        impl<T> Div<T> for Ratio<T> where
            T: Clone + Integer,
        {
            type Output = Ratio<T>;

            #[inline] fn div(self, rhs: T) -> Ratio<T> {
                let gcd = self.numer.gcd(&rhs);
                Ratio::new(self.numer / gcd.clone(), self.denom * (rhs / gcd))
            }
        }

        macro_rules! arith_impl
        {
            (impl $imp:ident, $method:ident) => {
                forward_all_binop!(impl $imp, $method);
               
                impl<T: Clone + Integer> $imp<Ratio<T>> for Ratio<T> {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, rhs: Ratio<T>) -> Ratio<T> {
                        if self.denom == rhs.denom {
                            return Ratio::new(self.numer.$method(rhs.numer), rhs.denom);
                        }
                        let lcm = self.denom.lcm(&rhs.denom);
                        let lhs_numer = self.numer * (lcm.clone() / self.denom);
                        let rhs_numer = rhs.numer * (lcm.clone() / rhs.denom);
                        Ratio::new(lhs_numer.$method(rhs_numer), lcm)
                    }
                }
               
                impl<T: Clone + Integer> $imp<T> for Ratio<T> {
                    type Output = Ratio<T>;
                    #[inline] fn $method(self, rhs: T) -> Ratio<T> {
                        Ratio::new(self.numer.$method(self.denom.clone() * rhs), self.denom)
                    }
                }
            };
        }

        arith_impl!(impl Add, add);
        arith_impl!(impl Sub, sub);
        arith_impl!(impl Rem, rem);
       
        impl<T> CheckedMul for Ratio<T> where
            T: Clone + Integer + CheckedMul,
        {
            #[inline] fn checked_mul(&self, rhs: &Ratio<T>) -> Option<Ratio<T>> {
                let gcd_ad = self.numer.gcd(&rhs.denom);
                let gcd_bc = self.denom.gcd(&rhs.numer);
                Some(Ratio::new(
                    (self.numer.clone() / gcd_ad.clone())
                        .checked_mul(&(rhs.numer.clone() / gcd_bc.clone()))?,
                    (self.denom.clone() / gcd_bc).checked_mul(&(rhs.denom.clone() / gcd_ad))?,
                ))
            }
        }

       
        impl<T> CheckedDiv for Ratio<T> where
            T: Clone + Integer + CheckedMul,
        {
            #[inline] fn checked_div(&self, rhs: &Ratio<T>) -> Option<Ratio<T>> {
                if rhs.is_zero() {
                    return None;
                }
                let (numer, denom) = if self.denom == rhs.denom {
                    (self.numer.clone(), rhs.numer.clone())
                } else if self.numer == rhs.numer {
                    (rhs.denom.clone(), self.denom.clone())
                } else {
                    let gcd_ac = self.numer.gcd(&rhs.numer);
                    let gcd_bd = self.denom.gcd(&rhs.denom);
                    (
                        (self.numer.clone() / gcd_ac.clone())
                            .checked_mul(&(rhs.denom.clone() / gcd_bd.clone()))?,
                        (self.denom.clone() / gcd_bd).checked_mul(&(rhs.numer.clone() / gcd_ac))?,
                    )
                };
               
                if denom.is_zero() {
                    None
                } else if numer.is_zero() {
                    Some(Self::zero())
                } else if numer == denom {
                    Some(Self::one())
                } else {
                    let g = numer.gcd(&denom);
                    let numer = numer / g.clone();
                    let denom = denom / g;
                    let raw = if denom < T::zero() {
                       
                       
                        let n1 = T::zero() - T::one();
                        Ratio::new_raw(numer.checked_mul(&n1)?, denom.checked_mul(&n1)?)
                    } else {
                        Ratio::new_raw(numer, denom)
                    };
                    Some(raw)
                }
            }
        }
        
        macro_rules! checked_arith_impl
        {
            (impl $imp:ident, $method:ident) => {
                impl<T: Clone + Integer + CheckedMul + $imp> $imp for Ratio<T> {
                    #[inline] fn $method(&self, rhs: &Ratio<T>) -> Option<Ratio<T>> {
                        let gcd = self.denom.clone().gcd(&rhs.denom);
                        let lcm = (self.denom.clone() / gcd.clone()).checked_mul(&rhs.denom)?;
                        let lhs_numer = (lcm.clone() / self.denom.clone()).checked_mul(&self.numer)?;
                        let rhs_numer = (lcm.clone() / rhs.denom.clone()).checked_mul(&rhs.numer)?;
                        Some(Ratio::new(lhs_numer.$method(&rhs_numer)?, lcm))
                    }
                }
            };
        }
        
        checked_arith_impl!(impl CheckedAdd, checked_add);
        checked_arith_impl!(impl CheckedSub, checked_sub);

        impl<T> Neg for Ratio<T> where
            T: Clone + Integer + Neg<Output = T>,
        {
            type Output = Ratio<T>;

            #[inline] fn neg(self) -> Ratio<T> {
                Ratio::new_raw(-self.numer, self.denom)
            }
        }

        impl<'a, T> Neg for &'a Ratio<T> where
            T: Clone + Integer + Neg<Output = T>,
        {
            type Output = Ratio<T>;

            #[inline] fn neg(self) -> Ratio<T> {
                -self.clone()
            }
        }

        impl<T> Inv for Ratio<T> where
            T: Clone + Integer,
        {
            type Output = Ratio<T>;

            #[inline] fn inv(self) -> Ratio<T> {
                self.recip()
            }
        }

        impl<'a, T> Inv for &'a Ratio<T> where
            T: Clone + Integer,
        {
            type Output = Ratio<T>;

            #[inline] fn inv(self) -> Ratio<T> {
                self.recip()
            }
        }
        
        impl<T: ConstZero + ConstOne> Ratio<T>
        {
            /// A constant `Ratio` 0/1.
            pub const ZERO: Self = Self::new_raw(T::ZERO, T::ONE);
        }

        impl<T: Clone + Integer + ConstZero + ConstOne> ConstZero for Ratio<T>
        {
            const ZERO: Self = Self::ZERO;
        }

        impl<T: Clone + Integer> Zero for Ratio<T> 
        {
            #[inline] fn zero() -> Ratio<T> {
                Ratio::new_raw(Zero::zero(), One::one())
            }
            #[inline] fn is_zero(&self) -> bool {
                self.numer.is_zero()
            }
            #[inline] fn set_zero(&mut self) {
                self.numer.set_zero();
                self.denom.set_one();
            }
        }

        impl<T: ConstOne> Ratio<T> 
        {
            /// A constant `Ratio` 1/1.
            pub const ONE: Self = Self::new_raw(T::ONE, T::ONE);
        }

        impl<T: Clone + Integer + ConstOne> ConstOne for Ratio<T> 
        {
            const ONE: Self = Self::ONE;
        }

        impl<T: Clone + Integer> One for Ratio<T>
        {
            #[inline] fn one() -> Ratio<T> {
                Ratio::new_raw(One::one(), One::one())
            }
            #[inline] fn is_one(&self) -> bool {
                self.numer == self.denom
            }
            #[inline] fn set_one(&mut self) {
                self.numer.set_one();
                self.denom.set_one();
            }
        }

        impl<T: Clone + Integer> Num for Ratio<T>
        {
            type FromStrRadixErr = ParseRatioError;

            /// Parses `numer/denom` where the numbers are in base `radix`.
            fn from_str_radix(s: &str, radix: u32) -> Result<Ratio<T>, ParseRatioError> {
                if s.splitn(2, '/').count() == 2 {
                    let mut parts = s.splitn(2, '/').map(|ss| {
                        T::from_str_radix(ss, radix).map_err(|_| ParseRatioError {
                            kind: RatioErrorKind::ParseError,
                        })
                    });
                    let numer: T = parts.next().unwrap()?;
                    let denom: T = parts.next().unwrap()?;
                    if denom.is_zero() {
                        Err(ParseRatioError {
                            kind: RatioErrorKind::ZeroDenominator,
                        })
                    } else {
                        Ok(Ratio::new(numer, denom))
                    }
                } else {
                    Err(ParseRatioError {
                        kind: RatioErrorKind::ParseError,
                    })
                }
            }
        }

        impl<T: Clone + Integer + Signed> Signed for Ratio<T>
        {
            #[inline] fn abs(&self) -> Ratio<T> {
                if self.is_negative() {
                    -self.clone()
                } else {
                    self.clone()
                }
            }
            #[inline] fn abs_sub(&self, other: &Ratio<T>) -> Ratio<T> {
                if *self <= *other {
                    Zero::zero()
                } else {
                    self - other
                }
            }
            #[inline] fn signum(&self) -> Ratio<T> {
                if self.is_positive() {
                    Self::one()
                } else if self.is_zero() {
                    Self::zero()
                } else {
                    -Self::one()
                }
            }
            #[inline] fn is_positive(&self) -> bool {
                (self.numer.is_positive() && self.denom.is_positive())
                    || (self.numer.is_negative() && self.denom.is_negative())
            }
            #[inline] fn is_negative(&self) -> bool {
                (self.numer.is_negative() && self.denom.is_positive())
                    || (self.numer.is_positive() && self.denom.is_negative())
            }
        }
        
        macro_rules! impl_formatting
        {
            ($fmt_trait:ident, $prefix:expr, $fmt_str:expr, $fmt_alt:expr) => {
                impl<T: $fmt_trait + Clone + Integer> $fmt_trait for Ratio<T> {
                            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result
                {        let pre_pad = if self.denom.is_one() {
                            format!($fmt_str, self.numer)
                        } else {
                            if f.alternate() {
                                format!(concat!($fmt_str, "/", $fmt_alt), self.numer, self.denom)
                            } else {
                                format!(concat!($fmt_str, "/", $fmt_str), self.numer, self.denom)
                            }
                        };
                        if let Some(pre_pad) = pre_pad.strip_prefix("-") {
                            f.pad_integral(false, $prefix, pre_pad)
                        } else {
                            f.pad_integral(true, $prefix, &pre_pad)
                        }
                    }
                }
            };
        }

        impl_formatting!(Display, "", "{}", "{:#}");
        impl_formatting!(Octal, "0o", "{:o}", "{:#o}");
        impl_formatting!(Binary, "0b", "{:b}", "{:#b}");
        impl_formatting!(LowerHex, "0x", "{:x}", "{:#x}");
        impl_formatting!(UpperHex, "0x", "{:X}", "{:#X}");
        impl_formatting!(LowerExp, "", "{:e}", "{:#e}");
        impl_formatting!(UpperExp, "", "{:E}", "{:#E}");

        impl<T: FromStr + Clone + Integer> FromStr for Ratio<T>
        {
            type Err = ParseRatioError;

            /// Parses `numer/denom` or just `numer`.
            fn from_str(s: &str) -> Result<Ratio<T>, ParseRatioError> {
                let mut split = s.splitn(2, '/');

                let n = split.next().ok_or(ParseRatioError {
                    kind: RatioErrorKind::ParseError,
                })?;
                let num = FromStr::from_str(n).map_err(|_| ParseRatioError {
                    kind: RatioErrorKind::ParseError,
                })?;

                let d = split.next().unwrap_or("1");
                let den = FromStr::from_str(d).map_err(|_| ParseRatioError {
                    kind: RatioErrorKind::ParseError,
                })?;

                if Zero::is_zero(&den) {
                    Err(ParseRatioError {
                        kind: RatioErrorKind::ZeroDenominator,
                    })
                } else {
                    Ok(Ratio::new(num, den))
                }
            }
        }

        impl<T> From<Ratio<T>> for (T, T)
        {
            fn from(val: Ratio<T>) -> Self {
                (val.numer, val.denom)
            }
        }
                
        #[derive(Copy, Clone, Debug, PartialEq)]
        pub struct ParseRatioError
        {
            kind: RatioErrorKind,
        }

        #[derive(Copy, Clone, Debug, PartialEq)]
        enum RatioErrorKind
        {
            ParseError,
            ZeroDenominator,
        }

        impl fmt::Display for ParseRatioError
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
                {self.kind.description().fmt(f)
            }
        }
        
        impl Error for ParseRatioError
        {
            #[allow(deprecated)]
            fn description(&self) -> &str {
                self.kind.description()
            }
        }

        impl RatioErrorKind
        {
            fn description(&self) -> &'static str {
                match *self {
                    RatioErrorKind::ParseError => "failed to parse integer",
                    RatioErrorKind::ZeroDenominator => "zero value denominator",
                }
            }
        }
        
        impl FromPrimitive for Ratio<BigInt>
        {
            fn from_i64(n: i64) -> Option<Self> {
                Some(Ratio::from_integer(n.into()))
            }
            fn from_i128(n: i128) -> Option<Self> {
                Some(Ratio::from_integer(n.into()))
            }
            fn from_u64(n: u64) -> Option<Self> {
                Some(Ratio::from_integer(n.into()))
            }
            fn from_u128(n: u128) -> Option<Self> {
                Some(Ratio::from_integer(n.into()))
            }
            fn from_f32(n: f32) -> Option<Self> {
                Ratio::from_float(n)
            }
            fn from_f64(n: f64) -> Option<Self> {
                Ratio::from_float(n)
            }
        }

        macro_rules! from_primitive_integer
        {
            ($typ:ty, $approx:ident) => {
                impl FromPrimitive for Ratio<$typ> {
                    fn from_i64(n: i64) -> Option<Self> {
                        <$typ as FromPrimitive>::from_i64(n).map(Ratio::from_integer)
                    }
                    fn from_i128(n: i128) -> Option<Self> {
                        <$typ as FromPrimitive>::from_i128(n).map(Ratio::from_integer)
                    }
                    fn from_u64(n: u64) -> Option<Self> {
                        <$typ as FromPrimitive>::from_u64(n).map(Ratio::from_integer)
                    }
                    fn from_u128(n: u128) -> Option<Self> {
                        <$typ as FromPrimitive>::from_u128(n).map(Ratio::from_integer)
                    }
                    fn from_f32(n: f32) -> Option<Self> {
                        $approx(n, 10e-20, 30)
                    }
                    fn from_f64(n: f64) -> Option<Self> {
                        $approx(n, 10e-20, 30)
                    }
                }
            };
        }

        from_primitive_integer!(i8, approximate_float);
        from_primitive_integer!(i16, approximate_float);
        from_primitive_integer!(i32, approximate_float);
        from_primitive_integer!(i64, approximate_float);
        from_primitive_integer!(i128, approximate_float);
        from_primitive_integer!(isize, approximate_float);

        from_primitive_integer!(u8, approximate_float_unsigned);
        from_primitive_integer!(u16, approximate_float_unsigned);
        from_primitive_integer!(u32, approximate_float_unsigned);
        from_primitive_integer!(u64, approximate_float_unsigned);
        from_primitive_integer!(u128, approximate_float_unsigned);
        from_primitive_integer!(usize, approximate_float_unsigned);

        impl<T: Integer + Signed + Bounded + NumCast + Clone> Ratio<T>
        {
            pub fn approximate_float<F: FloatCore + NumCast>(f: F) -> Option<Ratio<T>> {
               
               
               
                let epsilon = <F as NumCast>::from(10e-20).expect("Can't convert 10e-20");
                approximate_float(f, epsilon, 30)
            }
        }

        impl<T: Integer + Unsigned + Bounded + NumCast + Clone> Ratio<T>
        {
            pub fn approximate_float_unsigned<F: FloatCore + NumCast>(f: F) -> Option<Ratio<T>> {
               
               
               
                let epsilon = <F as NumCast>::from(10e-20).expect("Can't convert 10e-20");
                approximate_float_unsigned(f, epsilon, 30)
            }
        }

        fn approximate_float<T, F>(val: F, max_error: F, max_iterations: usize) -> Option<Ratio<T>> where
            T: Integer + Signed + Bounded + NumCast + Clone,
            F: FloatCore + NumCast,
        {
            let negative = val.is_sign_negative();
            let abs_val = val.abs();

            let r = approximate_float_unsigned(abs_val, max_error, max_iterations)?;

           
            Some(if negative { r.neg() } else { r })
        }
        
        fn approximate_float_unsigned<T, F>(val: F, max_error: F, max_iterations: usize) -> Option<Ratio<T>> where
            T: Integer + Bounded + NumCast + Clone,
            F: FloatCore + NumCast,
        {
           
           

            if val < F::zero() || val.is_nan() {
                return None;
            }
            let mut q = val;
            let mut n0 = T::zero();
            let mut d0 = T::one();
            let mut n1 = T::one();
            let mut d1 = T::zero();

            let t_max = T::max_value();
            let t_max_f = <F as NumCast>::from(t_max.clone())?;

           
            let epsilon = t_max_f.recip();

           
            if q > t_max_f {
                return None;
            }
            for _ in 0..max_iterations {
                let a = match <T as NumCast>::from(q) {
                    None => break,
                    Some(a) => a,
                };

                let a_f = match <F as NumCast>::from(a.clone()) {
                    None => break,
                    Some(a_f) => a_f,
                };
                let f = q - a_f;

               
                if !a.is_zero()
                    && (n1 > t_max.clone() / a.clone()
                        || d1 > t_max.clone() / a.clone()
                        || a.clone() * n1.clone() > t_max.clone() - n0.clone()
                        || a.clone() * d1.clone() > t_max.clone() - d0.clone())
                {
                    break;
                }
                let n = a.clone() * n1.clone() + n0.clone();
                let d = a.clone() * d1.clone() + d0.clone();

                n0 = n1;
                d0 = d1;
                n1 = n.clone();
                d1 = d.clone();

               
               
                let g = Integer::gcd(&n1, &d1);
                if !g.is_zero() {
                    n1 = n1 / g.clone();
                    d1 = d1 / g.clone();
                }
               
                let (n_f, d_f) = match (<F as NumCast>::from(n), <F as NumCast>::from(d)) {
                    (Some(n_f), Some(d_f)) => (n_f, d_f),
                    _ => break,
                };
                if (n_f / d_f - val).abs() < max_error {
                    break;
                }
               
                if f < epsilon {
                    break;
                }
                q = f.recip();
            }
           
            if d1.is_zero() {
                return None;
            }
            Some(Ratio::new(n1, d1))
        }
        
        impl<T: Clone + Integer + ToPrimitive + ToBigInt> ToPrimitive for Ratio<T>
        {
            fn to_i64(&self) -> Option<i64> {
                self.to_integer().to_i64()
            }
            fn to_i128(&self) -> Option<i128> {
                self.to_integer().to_i128()
            }
            fn to_u64(&self) -> Option<u64> {
                self.to_integer().to_u64()
            }
            fn to_u128(&self) -> Option<u128> {
                self.to_integer().to_u128()
            }
            fn to_f64(&self) -> Option<f64> {
                let float = match (self.numer.to_i64(), self.denom.to_i64()) {
                    (Some(numer), Some(denom)) => ratio_to_f64(
                        <i128 as From<_>>::from(numer),
                        <i128 as From<_>>::from(denom),
                    ),
                    _ => {
                        let numer: BigInt = self.numer.to_bigint()?;
                        let denom: BigInt = self.denom.to_bigint()?;
                        ratio_to_f64(numer, denom)
                    }
                };
                if float.is_nan() {
                    None
                } else {
                    Some(float)
                }
            }
        }

        trait Bits
        {
            fn bits(&self) -> u64;
        }
        
        impl Bits for BigInt
        {
            fn bits(&self) -> u64 {
                self.bits()
            }
        }

        impl Bits for i128
        {
            fn bits(&self) -> u64 {
                (128 - self.wrapping_abs().leading_zeros()).into()
            }
        }
        /// Converts a ratio of `T` to an f64.
        fn ratio_to_f64<T: Bits + Clone + Integer + Signed + ShlAssign<usize> + ToPrimitive>(
            numer: T,
            denom: T,
        ) -> f64 {
            use ::f64::{INFINITY, MANTISSA_DIGITS, MAX_EXP, MIN_EXP, RADIX};

            assert_eq!(
                RADIX, 2,
                "only floating point implementations with radix 2 are supported"
            );

           
            const MAX_EXACT_INT: i64 = 1i64 << MANTISSA_DIGITS;
            const MIN_EXACT_INT: i64 = -MAX_EXACT_INT;

            let flo_sign = numer.signum().to_f64().unwrap() / denom.signum().to_f64().unwrap();
            if !flo_sign.is_normal() {
                return flo_sign;
            }
           
           
           
            if let (Some(n), Some(d)) = (numer.to_i64(), denom.to_i64()) {
                let exact = MIN_EXACT_INT..=MAX_EXACT_INT;
                if exact.contains(&n) && exact.contains(&d) {
                    return n.to_f64().unwrap() / d.to_f64().unwrap();
                }
            }
           
           
           
           
            let mut numer = numer.abs();
            let mut denom = denom.abs();
            let (is_diff_positive, absolute_diff) = match numer.bits().checked_sub(denom.bits()) {
                Some(diff) => (true, diff),
                None => (false, denom.bits() - numer.bits()),
            };

           
           
            if is_diff_positive && absolute_diff > MAX_EXP as u64 {
                return INFINITY * flo_sign;
            }
            if !is_diff_positive && absolute_diff > -MIN_EXP as u64 + MANTISSA_DIGITS as u64 + 1 {
                return 0.0 * flo_sign;
            }
            let diff = if is_diff_positive {
                absolute_diff.to_isize().unwrap()
            } else {
                -absolute_diff.to_isize().unwrap()
            };

           
           
            let shift: isize = diff.max(MIN_EXP as isize) - MANTISSA_DIGITS as isize - 2;
            if shift >= 0 {
                denom <<= shift as usize
            } else {
                numer <<= -shift as usize
            };

            let (quotient, remainder) = numer.div_rem(&denom);

           
            let mut quotient = quotient.to_u64().unwrap();
            let n_rounding_bits = {
                let quotient_bits = 64 - quotient.leading_zeros() as isize;
                let subnormal_bits = MIN_EXP as isize - shift;
                quotient_bits.max(subnormal_bits) - MANTISSA_DIGITS as isize
            } as usize;
            debug_assert!(n_rounding_bits == 2 || n_rounding_bits == 3);
            let rounding_bit_mask = (1u64 << n_rounding_bits) - 1;

           
           
            let ls_bit = quotient & (1u64 << n_rounding_bits) != 0;
            let ms_rounding_bit = quotient & (1u64 << (n_rounding_bits - 1)) != 0;
            let ls_rounding_bits = quotient & (rounding_bit_mask >> 1) != 0;
            if ms_rounding_bit && (ls_bit || ls_rounding_bits || !remainder.is_zero()) {
                quotient += 1u64 << n_rounding_bits;
            }
            quotient &= !rounding_bit_mask;

           
           
            let q_float = quotient as f64 * flo_sign;
            ldexp(q_float, shift as i32)
        }
        /// Multiply `x` by 2 to the power of `exp`. Returns an accurate result even if `2^exp` is not representable.
        fn ldexp(x: f64, exp: i32) -> f64
        {
            use ::f64::{INFINITY, MANTISSA_DIGITS, MAX_EXP, RADIX};

            assert_eq!(
                RADIX, 2,
                "only floating point implementations with radix 2 are supported"
            );

            const EXPONENT_MASK: u64 = 0x7ff << 52;
            const MAX_UNSIGNED_EXPONENT: i32 = 0x7fe;
            const MIN_SUBNORMAL_POWER: i32 = MANTISSA_DIGITS as i32;

            if x.is_zero() || x.is_infinite() || x.is_nan() {
                return x;
            }
           
            if exp > 3 * MAX_EXP {
                return INFINITY * x.signum();
            } else if exp < -3 * MAX_EXP {
                return 0.0 * x.signum();
            }
           
            let (bits, curr_exp) = if !x.is_normal() {
               
               
                let normal_x = x * 2f64.powi(MIN_SUBNORMAL_POWER);
                let bits = normal_x.to_bits();
               
                (
                    bits,
                    ((bits & EXPONENT_MASK) >> 52) as i32 - MIN_SUBNORMAL_POWER,
                )
            } else {
                let bits = x.to_bits();
                let curr_exp = (bits & EXPONENT_MASK) >> 52;
               
                (bits, curr_exp as i32)
            };

           
           
            let new_exp = curr_exp + exp;

            if new_exp > MAX_UNSIGNED_EXPONENT {
                INFINITY * x.signum()
            } else if new_exp > 0 {
               
                let new_bits = (bits & !EXPONENT_MASK) | ((new_exp as u64) << 52);
                f64::from_bits(new_bits)
            } else if new_exp >= -(MANTISSA_DIGITS as i32) {
               
               
               
               
                let new_exp = new_exp + MIN_SUBNORMAL_POWER;
                debug_assert!(new_exp >= 0);
                let new_bits = (bits & !EXPONENT_MASK) | ((new_exp as u64) << 52);
                f64::from_bits(new_bits) * 2f64.powi(-MIN_SUBNORMAL_POWER)
            } else {
               
                return 0.0 * x.signum();
            }
        }
        
        pub fn frac_from_whole_and_dec(whole: BigInt, decimal: BigInt, dec_len: usize) -> BigRational
        {
            let denom = ::num::traits::pow(BigInt::from_u8(10).unwrap(), dec_len);
            BigRational::new(whole, 1.into()) + BigRational::new(decimal, denom)
        }
    }
}

pub mod mem
{
    pub use std::mem::{ * };
}

pub mod objects
{
    /*!
    Object | A hashmap of keys to values, where values can be any type, including other objects. */
    use ::
    {
        arrays::{ Arr },
        collections::{ hash_map::{Iter, Keys, Values}, HashMap },
        error::{ OverError },
        fmt::{ Format },
        num::
        {
            big::{ BigInt },
            rational::{ BigRational },
            traits::{ Zero },
        },
        result::{ OverResult },
        str::{ FromStr },
        sync::{ Arc, atomic::{ AtomicUsize, Ordering } },
        tuples::{ Tup },
        types::{ Type },
        values::{ Value },
        *,
    };
    /*
    */
    lazy_static! 
    {
        static ref CUR_ID: AtomicUsize = AtomicUsize::new(0);
    }

    fn get_id() -> usize 
    {
        CUR_ID.fetch_add(1, Ordering::Relaxed)
    }

    #[derive(Clone, Debug)]
    struct ObjInner 
    {
        map: HashMap<String, Value>,
        parent: Option<Obj>,
        id: usize,
    }
    /// `Obj` struct.
    #[derive(Clone, Debug)]
    pub struct Obj 
    {
        inner: Arc<ObjInner>,
    }

    macro_rules! get_fn 
    {
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

    impl Obj 
    {
        /// Gets the `Value` associated with `field`.
        pub fn read(&self, field: &str) -> Result<Value, ()> 
        {
            match self.inner.map.get(field)
            {
                Some(value) => Ok( value.clone() ),
                None => match self.inner.parent
                {
                    Some( ref parent ) => Ok( parent.get( field ).unwrap_or( Value::Null ) ),
                    None => Err( () ),
                },
            }
        }

        /// Returns a new `Obj` created from the given `HashMap`.
        pub fn from_map(obj_map: HashMap<String, Value>) -> OverResult<Obj> 
        {
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
        pub fn from_map_with_parent(obj_map: HashMap<String, Value>, parent: Obj) -> OverResult<Obj> 
        {
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
        pub fn from_map_unchecked(obj_map: HashMap<String, Value>) -> Obj 
        {
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
        pub fn from_map_with_parent_unchecked(obj_map: HashMap<String, Value>, parent: Obj) -> Obj 
        {
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
        pub fn id(&self) -> usize 
        {
            self.inner.id
        }
        /// Returns a reference to the inner map of this `Obj`.
        pub fn map_ref(&self) -> &HashMap<String, Value> 
        {
            &self.inner.map
        }
        /// Returns a new `Obj` loaded from a file.
        pub fn from_file( path:&str ) -> OverResult<Obj> 
        {
            Ok( parses::load_from_file(path)? )
        }
        /// Writes this `Obj` to given file in `.over` representation.
        pub fn write_to_file(&self, path: &str) -> OverResult<()>
        {
            str::write_file_from(path, &self.write_str())?;
            Ok(())
        }
        /// Writes this `Obj` to a `String`.
        pub fn write_str(&self) -> String 
        {
            self.format(false, 0)
        }
        /// Iterates over each `(String, Value)` pair in `self`, applying `f`.
        pub fn with_each<F>(&self, mut f: F) where
        F: FnMut(&String, &Value),
        {
            for (field, value) in &self.inner.map {
                f(field, value)
            }
        }
        /// Returns the number of fields for this `Obj` (parent fields not included).
        pub fn len(&self) -> usize 
        {
            self.inner.map.len()
        }
        /// Returns whether this `Obj` is empty.
        pub fn is_empty(&self) -> bool 
        {
            self.inner.map.is_empty()
        }
        /// Returns whether `self` and `other` point to the same data.
        pub fn ptr_eq(&self, other: &Self) -> bool 
        {
            Arc::ptr_eq(&self.inner, &other.inner)
        }
        /// Returns true if this `Obj` contains `field`.
        pub fn contains(&self, field: &str) -> bool 
        {
            self.inner.map.contains_key(field)
        }
        /// Gets the `Value` associated with `field`.
        pub fn get(&self, field: &str) -> Option<Value> 
        {
            match self.inner.map.get(field) {
                Some(value) => Some(value.clone()),
                None => match self.inner.parent {
                    Some(ref parent) => parent.get(field),
                    None => None,
                },
            }
        }
        /// Gets the `Value` associated with `field` and the `Obj` where it was found.
        pub fn get_with_source(&self, field: &str) -> Option<(Value, Obj)> 
        {
            match self.inner.map.get(field) {
                Some(value) => Some((value.clone(), self.clone())),
                None => match self.inner.parent {
                    Some(ref parent) => parent.get_with_source(field),
                    None => None,
                },
            }
        }

        get_fn!
        (
            "Returns the `bool` found at `field`. \
            Returns an error if the field was not found \
            or if the `Value` at `field` is not `Bool`.",
            get_bool,
            bool
        );

        get_fn!
        (
            "Returns the `BigInt` found at `field`. \
            Returns an error if the field was not found \
            or if the `Value` at `field` is not `Int`.",
            get_int,
            BigInt
        );

        get_fn!
        (
            "Returns the `BigRational` found at `field`. \
            Returns an error if the field was not found \
            or if the `Value` at `field` is not `Frac`.",
            get_frac,
            BigRational
        );

        get_fn!
        (
            "Returns the `char` found at `field`. \
            Returns an error if the field was not found \
            or if the `Value` at `field` is not `Char`.",
            get_char,
            char
        );

        get_fn!
        (
            "Returns the `String` found at `field`. \
            Returns an error if the field was not found \
            or if the `Value` at `field` is not `Str`.",
            get_str,
            String
        );

        get_fn!
        (
            "Returns the `Arr` found at `field`. \
            Returns an error if the field was not found \
            or if the `Value` at `field` is not `Arr`.",
            get_arr,
            Arr
        );

        get_fn!
        (
            "Returns the `Tup` found at `field`. \
            Returns an error if the field was not found \
            or if the `Value` at `field` is not `Tup`.",
            get_tup,
            Tup
        );

        get_fn!
        (
            "Returns the `Obj` found at `field`. \
            Returns an error if the field was not found \
            or if the `Value` at `field` is not `Obj`.",
            get_obj,
            Obj
        );

        /// Returns whether this `Obj` has a parent.
        pub fn has_parent(&self) -> bool 
        {
            self.inner.parent.is_some()
        }
        /// Returns the parent for this `Obj`.
        pub fn get_parent(&self) -> Option<Obj> 
        {
            match self.inner.parent {
                Some(ref parent) => Some(parent.clone()),
                None => None,
            }
        }
        /// Returns true if `field` is a valid field name for an `Obj`.
        pub fn is_valid_field(field: &str) -> bool 
        {
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
        /// Returns true if the given char is valid for a field, depending on whether it is the first char or not.
        pub fn is_valid_field_char(ch: char, first: bool) -> bool 
        {
            match ch 
            {
                ch if ch.is_alphabetic() => true,
                ch if is::digit(ch) => !first,
                '_' => true,
                '^' => first,
                _ => false,
            }
        }
        /// An iterator visiting all fields (keys) in arbitrary order.
        pub fn keys(&self) -> Keys<String, Value> 
        {
            self.map_ref().keys()
        }
        /// An iterator visiting all values in arbitrary order.
        pub fn values(&self) -> Values<String, Value> 
        {
            self.map_ref().values()
        }
        /// An iterator visiting all field-value pairs in arbitrary order.
        pub fn iter(&self) -> Iter<String, Value> 
        {
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

    impl FromStr for Obj 
    {
        type Err = OverError;

        fn from_str(s: &str) -> Result<Self, Self::Err> 
        {
            Ok(parses::load_from_str(s)?)
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

           
            if inner.parent.is_some() && other_inner.parent.is_some() {
                let parent = self.get_parent().unwrap();
                let other_parent = other.get_parent().unwrap();
                if parent != other_parent {
                    return false;
                }
            } else if !(inner.parent.is_none() && other_inner.parent.is_none()) {
                return false;
            }
           
            inner.map == other_inner.map
        }
    }
}

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

pub mod parses
{
    /*!
    */
    use ::
    {
        arrays::{ self, Arr },
        char::{ CharStream },
        collections::{ HashMap, HashSet, VecDeque },
        error::{ parse::{ ParseErrorKind::{ * }, ParseError, parse_err } },
        num::
        {
            big::BigInt,
            rational::{ BigRational, frac_from_whole_and_dec },
            traits::{ ToPrimitive, Zero },
        },
        objects::{ Obj },
        ops::{ Deref },
        path::{ Path },
        result::{ ParseResult },
        tuples::{ Tup },
        types::{ Type },
        values::{ Value },
        *,
    };
    /*
    */
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
    pub const MAX_DEPTH: usize = 64;
    /// Load an `Obj` from a file.
    pub fn load_from_file( path:&str ) -> ParseResult<Obj>
    {
        object_file( path )
    }
    /// Load an `Obj` from a &str.
    pub fn load_from_str(contents: &str) -> ParseResult<Obj>
    {
        object_from_str(contents)
    }
    /// Parses given file as an `Obj`.
   
    pub fn object_file(path: &str) -> ParseResult<Obj>
    {
        let stream = CharStream::from_file(path)?;
        parse_obj_stream(stream, &mut (HashMap::new(), HashSet::new()))
    }
   
    fn parse_obj_file_includes(path: &str, included: &mut IncludedMap) -> ParseResult<Obj> {
        let stream = CharStream::from_file(path)?;
        parse_obj_stream(stream, included)
    }
    /// Parses given &str as an `Obj`.
   
    pub fn object_from_str(contents: &str) -> ParseResult<Obj>
    {
        let contents = String::from(contents);
        let stream = CharStream::from_string(contents)?;
        parse_obj_stream(stream, &mut (HashMap::new(), HashSet::new()))
    }
    /// Parses an Obj given a character stream.
    #[inline] fn parse_obj_stream(mut stream: CharStream, mut included: &mut IncludedMap) -> ParseResult<Obj> {
        let mut obj: ObjMap = HashMap::new();
       
        if !find_char(stream.clone()) {
            return Ok(Obj::from_map_unchecked(obj));
        }

        let mut globals: GlobalMap = HashMap::new();
        let mut parent = None;

       
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
   
    fn parse_obj
    (
        mut stream: &mut CharStream,
        globals: &mut GlobalMap,
        mut included: &mut IncludedMap,
        depth: usize,
    ) -> ParseResult<Value> 
    {
       
        if depth > MAX_DEPTH {
            return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
        }

       
        let ch = stream.next().unwrap();
        assert_eq!(ch, '{');
       
        if !find_char(stream.clone()) {
            return parse_err(stream.file(), UnexpectedEnd(stream.line()));
        }

        let mut obj: ObjMap = HashMap::new();
        let mut parent = None;

       
        while parse_field_value_pair
        (
            &mut stream,
            &mut obj,
            globals,
            &mut included,
            &mut parent,
            depth,
            Some('}'),
        )? {}

        let obj = match parent 
        {
            Some(parent) => Obj::from_map_with_parent_unchecked(obj, parent),
            None => Obj::from_map_unchecked(obj),
        };
        Ok(obj.into())
    }
    /// Parses a field/value pair.
    #[inline] pub fn parse_field_value_pair
    (
        mut stream: &mut CharStream,
        obj: &mut ObjMap,
        mut globals: &mut GlobalMap,
        mut included: &mut IncludedMap,
        parent: &mut Option<Obj>,
        depth: usize,
        cur_brace: Option<char>,
    ) -> ParseResult<bool> 
    {
       
        let peek = stream.peek().unwrap();
        
        if peek == '}' && cur_brace.is_some() 
        {
            let _ = stream.next();
            return Ok(false);
        }
        
        else if is::end_delimiter(peek) 
        {
            return parse_err(
                stream.file(),
                InvalidClosingBracket(cur_brace, peek, stream.line(), stream.col()),
            );
        }

       
        let (field_line, field_col) = (stream.line(), stream.col());

       
        let (field, is_global, is_parent) = parse_field(stream.clone(), field_line, field_col)?;

        if !is_global && !is_parent && obj.contains_key(&field) {
            return parse_err(stream.file(), DuplicateField(field, field_line, field_col));
        } else if is_parent && parent.is_some() {
            return parse_err(
                stream.file(),
                DuplicateField("^".into(), field_line, field_col),
            );
        }

       
        if !find_char(stream.clone()) {
            return parse_err(stream.file(), UnexpectedEnd(stream.line()));
        }

       
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

       
        if !find_char(stream.clone()) {
            match cur_brace {
                Some(_) => return parse_err(stream.file(), UnexpectedEnd(stream.line())),
                None => return Ok(false),
            }
        }

        Ok(true)
    }
   
    fn parse_arr_file(path: &str, mut included: &mut IncludedMap) -> ParseResult<Arr> {
        let mut stream = CharStream::from_file(path)?;

        let obj: ObjMap = HashMap::new();
        let mut globals: GlobalMap = HashMap::new();

        let mut vec = Vec::new();
        let mut tcur = Type::Any;
        let mut has_any = true;

        loop {
           
            if !find_char(stream.clone()) {
                break;
            }
           
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
   
    fn parse_arr
    (
        mut stream: &mut CharStream,
        obj: &ObjMap,
        mut globals: &mut GlobalMap,
        mut included: &mut IncludedMap,
        depth: usize,
    ) -> ParseResult<Value> 
    {
       
        if depth > MAX_DEPTH 
        {
            return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
        }

       
        let ch = stream.next().unwrap();
        assert_eq!(ch, '[');

        let mut vec = Vec::new();
        let mut tcur = Type::Any;
        let mut has_any = true;

        loop 
        {
           
            if !find_char(stream.clone()) 
            {
                return parse_err(stream.file(), UnexpectedEnd(stream.line()));
            }
            let peek = stream.peek().unwrap();
            if peek == ']' 
            {
                let _ = stream.next();
                break;
            } 
            
            else if is::end_delimiter(peek) 
            {
                return parse_err(
                    stream.file(),
                    InvalidClosingBracket(Some(']'), peek, stream.line(), stream.col()),
                );
            }
           
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
   
    fn parse_tup_file(path: &str, mut included: &mut IncludedMap) -> ParseResult<Tup> {
        let mut stream = CharStream::from_file(path)?;

        let mut vec: Vec<Value> = Vec::new();
        let obj: ObjMap = HashMap::new();
        let mut globals: GlobalMap = HashMap::new();

        loop {
           
            if !find_char(stream.clone()) {
                break;
            }
           
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
   
    fn parse_tup
    (
        mut stream: &mut CharStream,
        obj: &ObjMap,
        mut globals: &mut GlobalMap,
        mut included: &mut IncludedMap,
        depth: usize,
    ) -> ParseResult<Value> 
    {        
        if depth > MAX_DEPTH 
        {
            return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
        }
        
        let ch = stream.next().unwrap();
        assert_eq!(ch, '(');

        let mut vec = Vec::new();

        loop 
        {
            if !find_char(stream.clone()) 
            {
                return parse_err(stream.file(), UnexpectedEnd(stream.line()));
            }
            let peek = stream.peek().unwrap();
            
            if peek == ')' 
            {
                let _ = stream.next();
                break;
            } 
            
            else if is::end_delimiter(peek)
            {
                return parse_err(
                    stream.file(),
                    InvalidClosingBracket(Some(')'), peek, stream.line(), stream.col()),
                );
            }
           
            let (value_line, value_col) = (stream.line(), stream.col());
            let value = parse_value
            (
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
   
    fn parse_field
    (
        mut stream: CharStream,
        line: usize,
        col: usize,
    ) -> ParseResult<(String, bool, bool)> 
    {
        let mut field = String::new();
        let mut first = true;
        let mut is_global = false;

        let ch = stream.peek().unwrap();
        
        if ch == '@' {
            let ch = stream.next().unwrap();
            is_global = true;
            field.push(ch);
        }

        while let Some(ch) = stream.next() 
        {
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
       
        match field.as_str() 
        {
            _field_str if is::reserved(_field_str) => 
            {
                parse_err(stream.file(), InvalidFieldName(field.clone(), line, col))
            }
            "^" => Ok((field.clone(), false, true)),
            bad if bad.starts_with('^') => 
            {
                parse_err(stream.file(), InvalidFieldName(field.clone(), line, col))
            }
            _ => Ok((field.clone(), is_global, false)),
        }
    }
   
    fn parse_value
    (
        mut stream: &mut CharStream,
        obj: &ObjMap,
        mut globals: &mut GlobalMap,
        mut included: &mut IncludedMap,
        line: usize,
        col: usize,
        depth: usize,
        cur_brace: Option<char>,
        is_first: bool,
    ) -> ParseResult<Value> 
    {
       
        let res = match stream.peek().unwrap() 
        {
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
            ch if is::numeric_char(ch) => parse_numeric(&mut stream, line, col)?,
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
       
        if is_first 
        {
            let mut val_deque: VecDeque<(Value, usize, usize)> = VecDeque::new();
            let mut op_deque: VecDeque<char> = VecDeque::new();
            val_deque.push_back((res, line, col));

            loop 
            {
                match stream.peek()
                {
                    Some(ch) if is::operator(ch) => 
                    {
                        let _ = stream.next();
                        if stream.peek().is_none() {
                            return parse_err(stream.file(), UnexpectedEnd(stream.line()));
                        }
                        let (line2, col2) = (stream.line(), stream.col());

                       
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

                        if is::priority_operator(ch) 
                        {
                            let (val1, line1, col1) = val_deque.pop_back().unwrap();
                            let res = binary_op_on_values(stream, val1, val2, ch, line2, col2)?;
                            val_deque.push_back((res, line1, col1));
                        }
                        
                        else
                        {
                            val_deque.push_back((val2, line2, col2));
                            op_deque.push_back(ch);
                        }
                    }
                    _ => break,
                }
            }
           
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
        } 

        else 
        {
            Ok(res)
        }
    }

    fn parse_unary_op
    (
        mut stream: &mut CharStream,
        obj: &ObjMap,
        mut globals: &mut GlobalMap,
        mut included: &mut IncludedMap,
        depth: usize,
        cur_brace: Option<char>,
        ch: char,
    ) -> ParseResult<Value> 
    {
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
   
    fn parse_numeric(stream: &mut CharStream, line: usize, col: usize) -> ParseResult<Value> 
    {
        let mut s1 = String::new();
        let mut s2 = String::new();
        let mut dec = false;
        let mut under = false;

        while let Some(ch) = stream.peek() {
            match ch {
                ch if is::value_end_char(ch) => break,
                ch if is::digit(ch) => {
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
           
            if s1.is_empty() && s2.is_empty() {
                return parse_err(stream.file(), InvalidNumeric(line, col));
            }
            let whole: BigInt = if s1.is_empty() {
                0u8.into()
            } else {
                s1.parse()?
            };

           
            let s2 = s2.trim_end_matches('0');

            let (decimal, dec_len): (BigInt, usize) = if s2.is_empty() {
                (0u8.into(), 1)
            } else {
                (s2.parse()?, s2.len())
            };

            let f = frac_from_whole_and_dec(whole, decimal, dec_len);
            Ok(f.into())
        } else {
           
            if s1.is_empty() {
                return parse_err(stream.file(), InvalidNumeric(line, col));
            }
            let i: BigInt = s1.parse()?;
            Ok(i.into())
        }
    }
   
    fn parse_variable
    (
        mut stream: &mut CharStream,
        obj: &ObjMap,
        mut globals: &mut GlobalMap,
        mut included: &mut IncludedMap,
        line: usize,
        col: usize,
        depth: usize,
        cur_brace: Option<char>,
    ) -> ParseResult<Value> 
    {
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

        while let Some(ch) = stream.peek() 
        {
            match ch 
            {
                '.' => {
                    let _ = stream.next();
                    match stream.peek() {
                        Some('@') => dot_global = true,
                        Some(ch) if Obj::is_valid_field_char(ch, true) || is::numeric_char(ch) => (),
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
                ch if is::value_end_char(ch) => break,
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

        let mut value = match var.as_str() 
        {
            "null" => Value::Null,
            "true" => Value::Bool(true),
            "false" => Value::Bool(false),

            "Obj" => Value::Obj(OBJ_SENTINEL.clone()),
            "Str" => Value::Obj(STR_SENTINEL.clone()),
            "Arr" => Value::Obj(ARR_SENTINEL.clone()),
            "Tup" => Value::Obj(TUP_SENTINEL.clone()),

            var @ "@" => return parse_err(stream.file(), InvalidValue(var.into(), line, col)),
            
            var if is_global => 
            {
               
                match globals.get(var) {
                    Some(value) => value.clone(),
                    None => {
                        let var = String::from(var);
                        return parse_err(stream.file(), GlobalNotFound(var, line, col));
                    }
                }
            }
            
            var => 
            {
               
                match obj.get(var) {
                    Some(value) => value.clone(),
                    None => {
                        let var = String::from(var);
                        return parse_err(stream.file(), VariableNotFound(var, line, col));
                    }
                }
            }
        };

        if dot 
        {
            value = match value 
            {
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
   
    fn parse_char(stream: &mut CharStream) -> ParseResult<Value> 
    {
        let ch = stream.next().unwrap();
        assert_eq!(ch, '\'');

        let (escape, mut ch) = match stream.next() 
        {
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

        if escape 
        {
            ch = match stream.next() 
            {
                Some(ch) => match get::escaped_character(ch)
                {
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

        match stream.next() 
        {
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

    fn parse_str_file(path: &str) -> ParseResult<String> 
    {
       
        let s = str::read_from_file(path)?.replace("\r\n", "\n");

        Ok(s)
    }
   
    fn parse_str(stream: &mut CharStream) -> ParseResult<Value> 
    {
        let ch = stream.next().unwrap();
        assert_eq!(ch, '"');

        let mut s = String::new();
        let mut escape = false;

        loop 
        {
            match stream.next() {
                Some(ch) => {
                    if escape {
                        match get::escaped_character(ch) {
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

       
        let s = s.replace("\r\n", "\n");

        Ok(s.into())
    }

    fn parse_include
    (
        mut stream: &mut CharStream,
        obj: &ObjMap,
        mut globals: &mut GlobalMap,
        mut included: &mut IncludedMap,
        depth: usize,
    ) -> ParseResult<Value> 
    {
        enum IncludeType {
            Obj,
            Str,
            Arr,
            Tup,
        }

       
        if depth > MAX_DEPTH {
            return parse_err(stream.file(), MaxDepth(stream.line(), stream.col()));
        }

        let ch = stream.next().unwrap();
        assert_eq!(ch, '<');
       
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

        let mut include_type = IncludeType::Obj;
        let mut parse_again = true;
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

       
        let path_str = match path.to_str() {
            Some(path) => path,
            None => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
        };

       
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(_) => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
        };
        let full_path_str = match path.to_str() {
            Some(path) => path,
            None => return parse_err(stream.file(), InvalidIncludePath(include_file, line, col)),
        };

       
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
           
            included.0.insert(full_path_str.into(), value.clone());
            value
        };
       
        if let Some(file) = storing {
            included.1.remove(&file);
        }

        Ok(value)
    }
   
    fn unary_op_on_value
    (
        stream: &CharStream,
        val: Value,
        op: char,
        line: usize,
        col: usize,
    ) -> ParseResult<Value> 
    {
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
   
    fn binary_op_on_values
    (
        stream: &CharStream,
        mut val1: Value,
        mut val2: Value,
        op: char,
        line: usize,
        col: usize,
    ) -> ParseResult<Value> 
    {
        use crate::types::Type::*;

        let (mut type1, mut type2) = (val1.get_type(), val2.get_type());
       
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

                               
                                let arr = if let Arr(ref t) = t {
                                   
                                    arrays::Arr::from_vec_unchecked(vec, t.deref().clone())
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
   
    fn find_char(mut stream: CharStream) -> bool 
    {
        while let Some(ch) = stream.peek() {
            match ch {
                '#' => {
                   
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
   
    fn check_value_end(stream: &CharStream, cur_brace: Option<char>) -> ParseResult<()> 
    {
        match stream.peek()
        {
            Some(ch) => match ch
            {
                ch if is::value_end_char(ch) =>
                {
                    if is::end_delimiter(ch) && Some(ch) != cur_brace
                    {
                        parse_err
                        (
                            stream.file(),
                            InvalidClosingBracket(cur_brace, ch, stream.line(), stream.col()),
                        )
                    }
                    
                    else
                    {
                        Ok(())
                    }
                }
                ch => parse_err
                (
                    stream.file(),
                    InvalidValueChar(ch, stream.line(), stream.col()),
                ),
            },
            None => Ok(()),
        }
    }
}

pub mod path
{
    pub use std::path::{ * };
}

pub mod primitive
{
    pub use std::primitive::{ * };
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
    use ::
    {
        error::{ OverError, ParseError },
        *,
    };
    /// Result type for this crate.
    pub type OverResult<T> = Result<T, OverError>;    
    /*
    */
    pub type ParseResult<T> = Result<T, ParseError>;
}

pub mod slice
{
    pub use std::slice::{ * };
}

pub mod str
{
    pub use std::str::{ * };
    use ::
    {
        fs::{ File },
        io::{ Read, Write },
        *,
    };
    /// Reads a file and returns its contents in a string.
   
    pub fn read_from_file(fname: &str) -> io::Result<String>
    {
        let mut file = File::open(fname)?;

        let mut contents = String::new();
        let _ = file.read_to_string(&mut contents)?;

        Ok(contents)
    }
    /// Writes a string to a file.
    //pub fn write_file_str(fname: &str, contents: &str) -> io::Result<()>
    pub fn write_file_from(fname: &str, contents: &str) -> io::Result<()>
    {
       
        let mut file = File::create(fname)?;

        file.write_all(contents.as_bytes())?;

        Ok(())
    }
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

pub mod tuples
{
    /*!
    Tuple | A container which can hold elements of different types.*/
    use ::
    {
        error::{ OverError },
        fmt::{ Format },
        result::{ OverResult },
        slice::{ Iter },
        sync::{ Arc },
        types::{ Type },
        values::{ Value },
        *,
    };
    /*
    use crate::{OverError, OverResult, INDENT_STEP};
    */
    #[derive(Clone, Debug)]
    struct TupInner 
    {
        vec: Vec<Value>,
        inner_tvec: Vec<Type>,
    }
    /// `Tup` struct.
    #[derive(Clone, Debug)]
    pub struct Tup 
    {
        inner: Arc<TupInner>,
    }

    impl Tup 
    {
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
        pub fn with_each<F>(&self, mut f: F) where
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
        fn default() -> Self { Self::from_vec( Vec::new() ) }
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
    pub enum Type 
    {
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
        pub fn is(&self, other: &Type) -> bool 
        {
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
        pub fn has_any(&self) -> bool 
        {
            match *self {
                Type::Any => true,
                Type::Arr(ref t) => Self::has_any(t),
                Type::Tup(ref tvec) => tvec.iter().any(|t| Self::has_any(t)),
                _ => false,
            }
        }
        /// Returns a type with the most specificity that can be applied to the two input types as well
        /// as `true` if the returned type is not maximally specific, that is, it contains `Any`.
        pub fn most_specific(type1: &Type, type2: &Type) -> Option<(Type, bool)> 
        {
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
    impl PartialEq for Type {
        fn eq(&self, other: &Self) -> bool {
            use self::Type::*;

           
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

    impl fmt::Display for Type {
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

pub mod usize
{
    pub use std::usize::{ * };
}

pub mod u128
{
    pub use std::u128::{ * };
}

pub mod values
{
    /*!
    */
    use ::
    {
        arrays::{ self, Arr },
        error::{ OverError },
        fmt::{ Format },
        num::
        {
            big::BigInt,
            rational::BigRational,
            traits::ToPrimitive,
        },
        objects::{ self, Obj },
        result::{ OverResult },
        tuples::{ self, Tup },
        types::{ Type },
        *,
    };
    /*
    
    use crate::{OverResult, INDENT_STEP};
    */
    macro_rules! get_fn
    {
        ( $doc:expr, $name:tt, $type:ty, $variant:ident ) =>
        {
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

    macro_rules! impl_eq
    {
        ($valtype:ident, $type:ty) =>
        {
            impl PartialEq<$type> for Value
            {
                fn eq(&self, other: &$type) -> bool
                {
                    match *self
                    {
                        Value::$valtype(ref value) => value == other,
                        _ => false,
                    }
                }
            }
            
            impl PartialEq<Value> for $type
            {
                fn eq(&self, other: &Value) -> bool
                {
                    match *other
                    {
                        Value::$valtype(ref value) => value == self,
                        _ => false,
                    }
                }
            }
        };
    }
    
    macro_rules! impl_eq_int
    {
        ($type:ty, $fn:tt) =>
        {
            impl PartialEq<$type> for Value
            {
                fn eq(&self, other: &$type) -> bool
                {
                    match *self
                    {
                        Value::Int(ref value) => match value.$fn()
                        {
                            Some(value) => value == *other,
                            None => false,
                        },
                        _ => false,
                    }
                }
            }
            
            impl PartialEq<Value> for $type
            {
                fn eq(&self, other: &Value) -> bool
                {
                    match *other
                    {
                        Value::Int(ref value) => match value.$fn()
                        {
                            Some(value) => value == *self,
                            None => false,
                        },
                        _ => false,
                    }
                }
            }
        };
    }

    macro_rules! impl_from
    {
        ($type:ty, $fn:tt) =>
        {
            impl From<$type> for Value
            {
                fn from(inner: $type) -> Self { Value::$fn(inner.into()) }
            }
        };
    }
    /// Enum of possible values and their inner types.
    #[derive(Clone, Debug, PartialEq)]
    pub enum Value
    {
        /// A null value.
        Null,
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
        /// An array value.
        Arr( Arr),
        /// A tuple value.
        Tup( Tup),
        /// An object value.
        Obj( Obj),
    }

    impl Value
    {
        /// Returns true if this `Value` is null.
        pub fn is_null(&self) -> bool {
            if let Value::Null = *self {
                true
            } else {
                false
            }
        }
        /// Returns the `Type` of this `Value`.
        pub fn get_type(&self) -> Type
        {
            use self::Value::*;

            match *self
            {
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

        get_fn!( r#"Returns the `bool` contained in this `Value`."#, get_bool, bool, Bool );        
        get_fn! ( "Returns the `BigInt` contained in this `Value`.", get_int, BigInt, Int );
        /// Returns the `BigRational` contained in this `Value`.
        pub fn get_frac( &self ) -> OverResult<BigRational>
        {
            match *self 
            {
                Value::Frac(ref inner) => Ok(inner.clone()),
                Value::Int(ref inner) => Ok(frac!(inner.clone(), 1)),
                _ => Err(OverError::TypeMismatch(Type::Frac, self.get_type())),
            }
        }

        get_fn!( "Returns the `char` contained in this `Value`.", get_char, char, Char );        
        get_fn!( r#"Returns the `String` contained in this `Value`."#, get_str, String, Str );        
        get_fn!( "Returns the `Obj` contained in this `Value`.", get_obj, objects::Obj, Obj );

        /// Returns the `Arr` contained in this `Value`.
        pub fn get_arr(&self) -> OverResult<arrays::Arr>
        {
            if let Value::Arr(ref inner) = *self { Ok(inner.clone()) }
            else
            {
                Err
                (
                    OverError::TypeMismatch
                    (
                        Type::Arr(Box::new(Type::Any)),
                        self.get_type(),
                    )
                )
            }
        }
        /// Returns the `Tup` contained in this `Value`.
        pub fn get_tup(&self) -> OverResult<tuples::Tup>
        {
            if let Value::Tup(ref inner) = *self { Ok(inner.clone()) }
            else { Err(OverError::TypeMismatch(Type::Tup(vec![]), self.get_type())) }
        }
    }

    impl fmt::Display for Value
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
        {
            write!(f, "{}", self.format(true, INDENT_STEP))
        }
    }

    impl_eq!(Bool, bool);
    impl_eq!(Int, BigInt);
    impl_eq!(Frac, BigRational);
    impl_eq!(Char, char);
    impl_eq!(Arr, arrays::Arr);
    impl_eq!(Tup, tuples::Tup);
    impl_eq!(Obj, objects::Obj);

    impl<'a> PartialEq<&'a str> for Value
    {
        fn eq(&self, other: &&str) -> bool
        {
            match *self
            {
                Value::Str(ref value) => value == &other.replace("\r\n", "\n"),
                _ => false,
            }
        }
    }

    impl<'a> PartialEq<Value> for &'a str
    {
        fn eq(&self, other: &Value) -> bool
        {
            match *other
            {
                Value::Str(ref value) => value == &self.replace("\r\n", "\n"),
                _ => false,
            }
        }
    }

    impl PartialEq<String> for Value
    {
        fn eq(&self, other: &String) -> bool
        {
            &other.as_str() == self
        }
    }

    impl PartialEq<Value> for String
    {
        fn eq(&self, other: &Value) -> bool
        {
            &self.as_str() == other
        }
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
    
    impl<'a> From<&'a str> for Value 
    {
        fn from(inner: &str) -> Self 
        {
            Value::Str(inner.into())
        }
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
    impl_from!(BigRational, Frac);
    impl_from!(char, Char);
    impl_from!(String, Str);
    impl_from!( arrays::Arr, Arr );
    impl_from!( tuples::Tup, Tup );
    impl_from!( objects::Obj, Obj );
}

pub mod vec
{
    pub use std::vec::{ * };
}
/// Indent step in .over files.
pub const INDENT_STEP: usize = 4;

pub fn main() -> Result<(), error::parse::ParseError>
{
    let document = ::parses::load_from_str
    (
        r#"receipt: "Oz-Ware Purchase Invoice"
            date:    "2012-08-06"
            customer: {
                first_name:  "Dorothy"
                family_name: "Gale"
            }

            items: [
                    {
                    part_no:  "A4786"
                    descrip:  "Water Bucket (Filled)"
                    price:    01.47
                    quantity: 4
                    }
                    {
                    part_no:  "E1628"
                    descrip:  "High Heeled \"Ruby\" Slippers"
                    size:     8
                    price:    133.70
                    quantity: 1
                    }
                ]

            bill_to: {
                street:
                # A multi-line string. Can also be written as "123 Tornado Alley\nSuite16"
            "123 Tornado Alley
            Suite 16"
                city:  "East Centerville"
                state: "KS"
            }

            ship_to: bill_to

            specialDelivery:
            "Follow the Yellow Brick Road to the Emerald City. Pay no attention to the man behind the curtain."
    "#)?;
    
    assert_eq!( document.read( "receipt" )?, "Oz-Ware Purchase Invoice");
    assert_eq!( document.read( "date"    )?, "2012-08-06"              );
    assert_eq!
    (
        document.read("customer")?,
        obj!
        { 
            "first_name"  => "Dorothy", 
            "family_name" => "Gale"
        }
    );

    assert_eq!
    (
        document.read("items")?,
        arr!
        [
            obj!
            {
                "part_no" => "A4786",
                "descrip" => "Water Bucket (Filled)",
                "price" => frac!(147,100),
                "quantity" => 4
            },

            obj!
            {
                "part_no" => "E1628",
                "descrip" => "High Heeled \"Ruby\" Slippers",
                "size" => 8,
                "price" => frac!(1337,10),
                "quantity" => 1
            },
        ]
    );

    assert_eq!
    (
        document.read("bill_to")?,
        obj!
        {
            "street" => "123 Tornado Alley\nSuite 16",
            "city" => "East Centerville",
            "state" => "KS",
        }
    );

    assert_eq!
    (
        document.read("ship_to")?,
        document.read("bill_to")?,
    );

    assert_eq!
    (
        document.read("specialDelivery")?,
        r#"Follow the Yellow Brick Road to the Emerald City.
        Pay no attention to the man behind the curtain."#
    );
    /*
    */
    Ok(())
}
// 21326 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
