//! Implementation of Unicode Standard Annex #31 for 
//! determining which `char` values are valid in programming language identifiers.
#![feature
(
    
)]

#![allow
(
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
    use ::
    {
        *,
    };
    /*
    */
}
*/
extern crate proc_macro;

#[macro_use] pub mod macros
{
    /// Performs variable interpolation against the input and produces it as [`proc_macro2::TokenStream`].
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
       
        (# $var:ident) => {{
            let mut _s = ::__private::TokenStream::new();
            ::ToTokens::to_tokens(&$var, &mut _s);
            _s
        }};
        ($tt1:tt $tt2:tt) => {{
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

pub mod cell
{
    pub use std::cell::{ * };
}

pub mod char
{
    pub use std::char::{ * };
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

pub mod error
{
    pub use std::error::{ * };
}

pub mod ffi
{
    pub use std::ffi::{ * };
}

pub mod fmt
{
    pub use std::fmt::{ * };
}

pub mod hash
{
    pub use std::hash::{ * };
}

pub mod is
{
    use ::
    {
        *,
    };
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
    fn is_whitespace(ch: char) -> bool*/
    pub fn whitespace(ch: char) -> bool
    {
        ch.is_whitespace() || ch == '\u{200e}' || ch == '\u{200f}'
    }
    /*
    pub fn is_ident_start(c: char) -> bool*/
    pub fn ident_start(c: char) -> bool
    {
        c == '_' || ::is::xid_start(c)
    }
    /*
    pub fn is_ident_continue(c: char) -> bool */
    pub fn ident_continue(c: char) -> bool
    {
        xid_continue(c)
    }
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
    use alloc::rc::Rc;
    use core::marker::PhantomData;
    use core::panic::{RefUnwindSafe, UnwindSafe};
    */
    pub const MARKER: ProcMacroAutoTraits = ProcMacroAutoTraits(PhantomData);
    /// Zero sized marker with the correct set of autotrait impls we want all proc macro types to have.
    #[derive(Copy, Clone, PartialEq, Eq)]
    pub struct ProcMacroAutoTraits( pub PhantomData<Rc<()>> );
    impl UnwindSafe for ProcMacroAutoTraits {}
    impl RefUnwindSafe for ProcMacroAutoTraits {}
}

pub mod mem
{
    pub use std::mem::{ * };
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

pub mod path
{
    pub use std::path::{ * };
}

pub mod process
{
    pub use std::process::{ * };

    pub mod macros
    {
        use ::
        {
            cmp::{ Ordering },
            error::{ Error },
            fmt::{ self, Debug, Display },
            hash::{ Hash, Hasher },
            ops::{ Range, RangeBounds },
            path::{ PathBuf },
            *,
        };
        /*
            use ::process::macros::extra::DelimSpan;
            use ::process::macros::marker::{ProcMacroAutoTraits, MARKER};
            use core::cmp::Ordering;
            use core::fmt::{self, Debug, Display};
            use core::hash::{Hash, Hasher};
            use core::ops::Range;
            use core::ops::RangeBounds;
            use core::str::FromStr;
            use std::error::Error;
            use std::ffi::CStr;
            use std::path::PathBuf;
            pub use ::process::macros::location::LineColumn;
        */        
        pub mod detection
        {
            use ::
            {
                sync::
                {
                    atomic::{ AtomicUsize, Ordering },
                    Once
                },
                *,
            };
            /*
            */
            static WORKS: AtomicUsize = AtomicUsize::new(0);
            static INIT: Once = Once::new();

            pub fn inside_proc_macro() -> bool
            {
                match WORKS.load(Ordering::Relaxed)
                {
                    1 => return false,
                    2 => return true,
                    _ =>
                    {}
                }

                INIT.call_once(initialize);
                inside_proc_macro()
            }

            pub fn force_fallback() { WORKS.store(1, Ordering::Relaxed); }

            pub fn unforce_fallback() { initialize(); }
            
            #[allow(deprecated)] fn initialize()
            {
                use ::panic::{self, PanicInfo};

                type PanicHook = dyn Fn(&PanicInfo) + Sync + Send + 'static;

                let null_hook: Box<PanicHook> = Box::new(|_panic_info| { /* ignore */ });
                let sanity_check = &*null_hook as *const PanicHook;
                let original_hook = panic::take_hook();
                panic::set_hook(null_hook);

                let works = panic::catch_unwind(proc_macro::Span::call_site).is_ok();
                WORKS.store(works as usize + 1, Ordering::Relaxed);

                let hopefully_null_hook = panic::take_hook();
                panic::set_hook(original_hook);
                if sanity_check != &*hopefully_null_hook {
                    panic!("observed race condition in proc_macro2::inside_proc_macro");
                }
            }
        }

        pub mod extra
        {
            use ::
            {
                fmt::{ self, Debug },
                process::macros::
                {
                    fallback, imp, Span, 
                },
                marker::{ ProcMacroAutoTraits, MARKER },
                *,
            };
            /*
            */
            /// Invalidate any `proc_macro2::Span` that exist on the current thread.
            pub fn invalidate_current_thread_spans()
            {
                ::process::macros::imp::invalidate_current_thread_spans();
            }
            /// An object that holds a [`Group`]'s `span_open()` and `span_close()` together
            /// in a more compact representation than holding those 2 spans individually.
            #[derive(Copy, Clone)]
            pub struct DelimSpan 
            {
                inner: DelimSpanEnum,
                _marker: ProcMacroAutoTraits,
            }

            #[derive(Copy, Clone)]
            enum DelimSpanEnum 
            {
                    Compiler {
                    join: proc_macro::Span,
                    open: proc_macro::Span,
                    close: proc_macro::Span,
                },
                Fallback(fallback::Span),
            }

            impl DelimSpan
            {
                pub fn new(group: &imp::Group) -> Self
               
                {
                    let inner = match group
                    {
                        imp::Group::Compiler(group) => DelimSpanEnum::Compiler
                        {
                            join: group.span(),
                            open: group.span_open(),
                            close: group.span_close(),
                        },
                        imp::Group::Fallback(group) => DelimSpanEnum::Fallback(group.span()),
                    };

                    DelimSpan
                    {
                        inner,
                        _marker: marker::MARKER,
                    }
                }
                /// Returns a span covering the entire delimited group.
                pub fn join( &self ) -> Span
                {
                    match &self.inner
                    {
                        DelimSpanEnum::Compiler { join, .. } => Span::_new(imp::Span::Compiler(*join)),
                        DelimSpanEnum::Fallback(span) => Span::_new_fallback(*span),
                    }
                }
                /// Returns a span for the opening punctuation of the group only.
                pub fn open( &self ) -> Span
                {
                    match &self.inner
                    {
                        DelimSpanEnum::Compiler { open, .. } => Span::_new(imp::Span::Compiler(*open)),
                        DelimSpanEnum::Fallback(span) => Span::_new_fallback(span.first_byte()),
                    }
                }
                /// Returns a span for the closing punctuation of the group only.
                pub fn close( &self ) -> Span
                {
                    match &self.inner
                    {
                        DelimSpanEnum::Compiler { close, .. } => Span::_new(imp::Span::Compiler(*close)),
                        DelimSpanEnum::Fallback(span) => Span::_new_fallback(span.last_byte()),
                    }
                }
            }

            impl Debug for DelimSpan           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    Debug::fmt(&self.join(), f)
                }
            }
        }
        
        pub mod fallback
        {
            use ::
            {
                cell::{ RefCell },
                collections::{ BTreeMap },
                convert::{ TryFrom },
                cmp::{ Ordering },
                fmt::{ self, Debug, Display, Write },
                ffi::{ CStr },
                mem::{ ManuallyDrop },
                ops::{ Range, RangeBounds },
                path::{ PathBuf },
                process::macros::
                {
                    location::LineColumn,
                    parse::{self, Cursor},
                    rcvec::{RcVec, RcVecBuilder, RcVecIntoIter, RcVecMut},
                    imp, Delimiter, Spacing, TokenTree,
                },
                str::{ FromStr },
                *,
            };
            /*
            */
            macro_rules! suffixed_numbers
            {
                ($($name:ident => $kind:ident,)*) => 
                ($(
                    pub fn $name(n: $kind) -> Literal 
                    {
                        Literal::_new(format!(concat!("{}", stringify!($kind)), n))
                    }
                )*)
            }

            macro_rules! unsuffixed_numbers
            {
                ($($name:ident => $kind:ident,)*) => 
                ($(
                    pub fn $name(n: $kind) -> Literal { Literal::_new(n.to_string()) }
                )*)
            }
            /// Force use of proc-macro2's fallback for now, even if the compiler's implementation is available.
            pub fn force()
            {
                ::process::macros::detection::force_fallback();
            }
            /// Resume using the compiler's implementation of the proc macro API if it is available.
            pub fn unforce()
            {
                ::process::macros::detection::unforce_fallback();
            }

            #[derive(Clone)]
            pub struct TokenStream 
            {
                inner: RcVec<TokenTree>,
            }

            #[derive(Debug)]
            pub struct LexError 
            {
                pub span: Span,
            }

            impl LexError           
            {
                pub fn span( &self ) -> Span { self.span }
                pub fn call_site() -> Self
                {
                    LexError
                    {
                        span: Span::call_site(),
                    }
                }
            }

            impl TokenStream
            {
                pub fn new() -> Self
                {
                    TokenStream
                    {
                        inner: RcVecBuilder::new().build(),
                    }
                }

                pub fn from_str_checked(src: &str) -> Result<Self, LexError>
               
                {
                    let mut cursor = get_cursor(src);
                    const BYTE_ORDER_MARK: &str = "\u{feff}";
                    if cursor.starts_with(BYTE_ORDER_MARK) {
                        cursor = cursor.advance(BYTE_ORDER_MARK.len());
                    }

                    parse::token_stream(cursor)
                }
                
                pub fn from_str_unchecked(src: &str) -> Self {
                    Self::from_str_checked(src).unwrap()
                }

                pub fn is_empty( &self ) -> bool
                {
                    self.inner.len() == 0
                }

                fn take_inner( self ) -> RcVecBuilder<TokenTree>
                {
                    let nodrop = ManuallyDrop::new( self );
                    unsafe { ptr::read(&nodrop.inner) }.make_owned()
                }
            }

            fn push_token_from_proc_macro(mut vec: RcVecMut<TokenTree>, token: TokenTree)
            {
                match token
                {
                    TokenTree::Literal
                    (
                        ::process::macros::Literal
                        {
                            inner: ::process::macros::imp::Literal::Fallback(literal),
                            _marker: marker::ProcMacroAutoTraits(_),
                        }
                    ) if literal.repr.starts_with('-') =>
                    {
                        push_negative_literal(vec, literal);
                    }
                    _ => vec.push(token),
                }

                #[cold] fn push_negative_literal(mut vec: RcVecMut<TokenTree>, mut literal: Literal)
                {
                    literal.repr.remove(0);
                    let mut punct = ::process::macros::Punct::new('-', Spacing::Alone);
                    punct.set_span(::process::macros::Span::_new_fallback(literal.span));
                    vec.push(TokenTree::Punct(punct));
                    vec.push(TokenTree::Literal(::process::macros::Literal::_new_fallback(literal)));
                }
            }
            
            impl Drop for TokenStream           
            {
                fn drop( &mut self )
                {
                    let mut stack = Vec::new();
                    let mut current = match self.inner.get_mut() {
                        Some(inner) => inner.take().into_iter(),
                        None => return,
                    };
                    loop {
                        while let Some(token) = current.next() {
                            let group = match token {
                                TokenTree::Group(group) => group.inner,
                                _ => continue,
                            };
                                            let group = match group {
                                ::process::macros::imp::Group::Fallback(group) => group,
                                ::process::macros::imp::Group::Compiler(_) => continue,
                            };
                            let mut group = group;
                            if let Some(inner) = group.stream.inner.get_mut() {
                                stack.push(current);
                                current = inner.take().into_iter();
                            }
                        }
                        match stack.pop() {
                            Some(next) => current = next,
                            None => return,
                        }
                    }
                }
            }

            pub struct TokenStreamBuilder {
                inner: RcVecBuilder<TokenTree>,
            }

            impl TokenStreamBuilder
            {
                pub fn new() -> Self {
                    TokenStreamBuilder {
                        inner: RcVecBuilder::new(),
                    }
                }

                pub fn with_capacity(cap: usize) -> Self {
                    TokenStreamBuilder {
                        inner: RcVecBuilder::with_capacity(cap),
                    }
                }

                pub fn push_token_from_parser(&mut self, tt: TokenTree) {
                    self.inner.push(tt);
                }

                pub fn build( self ) -> TokenStream {
                    TokenStream {
                        inner: self.inner.build(),
                    }
                }
            }
            
            fn get_cursor(src: &str) -> Cursor<'_>
            {
                SOURCE_MAP.with(|sm|
               
                {
                    let mut sm = sm.borrow_mut();
                    let span = sm.add_file(src);
                    Cursor {
                        rest: src,
                        off: span.lo,
                    }
                })
            }

            impl Display for LexError
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str("cannot parse string into token stream")
                }
            }

            impl Display for TokenStream
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    let mut joint = false;
                    for (i, tt) in self.inner.iter().enumerate() {
                        if i != 0 && !joint {
                            write!(f, " ")?;
                        }
                        joint = false;
                        match tt {
                            TokenTree::Group(tt) => Display::fmt(tt, f),
                            TokenTree::Ident(tt) => Display::fmt(tt, f),
                            TokenTree::Punct(tt) =>
                    {
                                joint = tt.spacing() == Spacing::Joint;
                                Display::fmt(tt, f)
                            }
                            TokenTree::Literal(tt) => Display::fmt(tt, f),
                        }?;
                    }

                    Ok(())
                }
            }

            impl Debug for TokenStream
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str("TokenStream ")?;
                    f.debug_list().entries(self.clone()).finish()
                }
            }
            
            impl From<proc_macro::TokenStream> for TokenStream
            {
                fn from(inner: proc_macro::TokenStream) -> Self {
                    TokenStream::from_str_unchecked(&inner.to_string())
                }
            }
            
            impl From<TokenStream> for proc_macro::TokenStream
            {
                fn from(inner: TokenStream) -> Self {
                    proc_macro::TokenStream::from_str_unchecked(&inner.to_string())
                }
            }

            impl From<TokenTree> for TokenStream
            {
                fn from(tree: TokenTree) -> Self
                {
                    let mut stream = RcVecBuilder::new();
                    push_token_from_proc_macro(stream.as_mut(), tree);
                    TokenStream {
                        inner: stream.build(),
                    }
                }
            }

            impl iter::FromIterator<TokenTree> for TokenStream
            {
                fn from_iter<I: IntoIterator<Item = TokenTree>>(tokens: I) -> Self
                {
                    let mut stream = TokenStream::new();
                    stream.extend(tokens);
                    stream
                }
            }

            impl iter::FromIterator<TokenStream> for TokenStream
            {
                fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self
                {
                    let mut v = RcVecBuilder::new();

                    for stream in streams {
                        v.extend(stream.take_inner());
                    }

                    TokenStream { inner: v.build() }
                }
            }

            impl Extend<TokenTree> for TokenStream
            {
                fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, tokens: I)
                {
                    let mut vec = self.inner.make_mut();
                    tokens
                        .into_iter()
                        .for_each(|token| push_token_from_proc_macro(vec.as_mut(), token));
                }
            }

            impl Extend<TokenStream> for TokenStream
            {
                fn extend<I: IntoIterator<Item = TokenStream>>(&mut self, streams: I) {
                    self.inner.make_mut().extend(streams.into_iter().flatten());
                }
            }

            pub type TokenTreeIter = RcVecIntoIter<TokenTree>;

            impl IntoIterator for TokenStream {
                type Item = TokenTree;
                type IntoIter = TokenTreeIter;
                fn into_iter( self ) -> TokenTreeIter {
                    self.take_inner().into_iter()
                }
            }

            thread_local! {
                static SOURCE_MAP: RefCell<SourceMap> = RefCell::new(SourceMap {
                   
                   
                    files: vec![FileInfo {
                        source_text: String::new(),
                        span: Span { lo: 0, hi: 0 },
                        lines: vec![0],
                        char_index_to_byte_offset: BTreeMap::new(),
                    }],
                });
            }

            pub fn invalidate_current_thread_spans() {
                #[cfg(not(fuzzing))]
                SOURCE_MAP.with(|sm| sm.borrow_mut().files.truncate(1));
            }

            struct FileInfo {
                source_text: String,
                span: Span,
                lines: Vec<usize>,
                char_index_to_byte_offset: BTreeMap<usize, usize>,
            }

            impl FileInfo
            {
                fn offset_line_column( &self, offset: usize) -> LineColumn {
                    assert!(self.span_within(Span {
                        lo: offset as u32,
                        hi: offset as u32,
                    }));
                    let offset = offset - self.span.lo as usize;
                    match self.lines.binary_search(&offset) {
                        Ok(found) => LineColumn {
                            line: found + 1,
                            column: 0,
                        },
                        Err(idx) => LineColumn {
                            line: idx,
                            column: offset - self.lines[idx - 1],
                        },
                    }
                }

                fn span_within( &self, span: Span) -> bool {
                    span.lo >= self.span.lo && span.hi <= self.span.hi
                }

                fn byte_range(&mut self, span: Span) -> Range<usize>
                {
                    let lo_char = (span.lo - self.span.lo) as usize;

                   
                   
                   
                    let (&last_char_index, &last_byte_offset) = self
                        .char_index_to_byte_offset
                        .range(..=lo_char)
                        .next_back()
                        .unwrap_or((&0, &0));

                    let lo_byte = if last_char_index == lo_char {
                        last_byte_offset
                    } else {
                        let total_byte_offset = match self.source_text[last_byte_offset..]
                            .char_indices()
                            .nth(lo_char - last_char_index)
                        {
                            Some((additional_offset, _ch)) => last_byte_offset + additional_offset,
                            None => self.source_text.len(),
                        };
                        self.char_index_to_byte_offset
                            .insert(lo_char, total_byte_offset);
                        total_byte_offset
                    };

                    let trunc_lo = &self.source_text[lo_byte..];
                    let char_len = (span.hi - span.lo) as usize;
                    lo_byte..match trunc_lo.char_indices().nth(char_len) {
                        Some((offset, _ch)) => lo_byte + offset,
                        None => self.source_text.len(),
                    }
                }

                fn source_text(&mut self, span: Span) -> String
                {
                    let byte_range = self.byte_range(span);
                    self.source_text[byte_range].to_owned()
                }
            }
            /// Computes the offsets of each line in the given source string and the total number of characters
            fn lines_offsets(s: &str) -> (usize, Vec<usize>)
            {
                let mut lines = vec![0];
                let mut total = 0;

                for ch in s.chars() {
                    total += 1;
                    if ch == '\n' {
                        lines.push(total);
                    }
                }

                (total, lines)
            }

            struct SourceMap {
                files: Vec<FileInfo>,
            }

            impl SourceMap
            {
                fn next_start_pos( &self ) -> u32 {
                   
                    //
                   
                   
                    self.files.last().unwrap().span.hi + 1
                }

                fn add_file(&mut self, src: &str) -> Span
                {
                    let (len, lines) = lines_offsets(src);
                    let lo = self.next_start_pos();
                    let span = Span {
                        lo,
                        hi: lo + (len as u32),
                    };

                    self.files.push(FileInfo {
                        source_text: src.to_owned(),
                        span,
                        lines,
                       
                        char_index_to_byte_offset: BTreeMap::new(),
                    });

                    span
                }

                fn find( &self, span: Span) -> usize {
                    match self.files.binary_search_by(|file| {
                        if file.span.hi < span.lo {
                            Ordering::Less
                        } else if file.span.lo > span.hi {
                            Ordering::Greater
                        } else {
                            assert!(file.span_within(span));
                            Ordering::Equal
                        }
                    }) {
                        Ok(i) => i,
                        Err(_) => unreachable!("Invalid span with no related FileInfo!"),
                    }
                }

                fn filepath( &self, span: Span) -> String
                {
                    let i = self.find(span);
                    if i == 0 {
                        "<unspecified>".to_owned()
                    } else {
                        format!("<parsed string {}>", i)
                    }
                }

                fn fileinfo( &self, span: Span) -> &FileInfo
                {
                    let i = self.find(span);
                    &self.files[i]
                }

                fn fileinfo_mut(&mut self, span: Span) -> &mut FileInfo
                {
                    let i = self.find(span);
                    &mut self.files[i]
                }
            }

            #[derive(Clone, Copy, PartialEq, Eq)]
            pub struct Span {
                    pub lo: u32,
                    pub hi: u32,
            }

            impl Span 
            {
                pub fn call_site() -> Self {
                    Span { lo: 0, hi: 0 }
                }

                pub fn mixed_site() -> Self {
                    Span::call_site()
                }
                
                pub fn def_site() -> Self {
                    Span::call_site()
                }

                pub fn resolved_at( &self, _other: Span) -> Span {
                    *self
                }

                pub fn located_at( &self, other: Span) -> Span {
                    other
                }

                pub fn byte_range( &self ) -> Range<usize>
                    {
                    #[cfg(fuzzing)]
                    return 0..0;

                    #[cfg(not(fuzzing))]
                    {
                        if self.is_call_site() {
                            0..0
                        } else {
                            SOURCE_MAP.with(|sm| sm.borrow_mut().fileinfo_mut(*self).byte_range(*self))
                        }
                    }
                }

                pub fn start( &self ) -> LineColumn {
                    #[cfg(fuzzing)]
                    return LineColumn { line: 0, column: 0 };

                    #[cfg(not(fuzzing))]
                    SOURCE_MAP.with(|sm| {
                        let sm = sm.borrow();
                        let fi = sm.fileinfo(*self);
                        fi.offset_line_column(self.lo as usize)
                    })
                }

                pub fn end( &self ) -> LineColumn {
                    #[cfg(fuzzing)]
                    return LineColumn { line: 0, column: 0 };

                    #[cfg(not(fuzzing))]
                    SOURCE_MAP.with(|sm| {
                        let sm = sm.borrow();
                        let fi = sm.fileinfo(*self);
                        fi.offset_line_column(self.hi as usize)
                    })
                }

                pub fn file( &self ) -> String {
                    #[cfg(fuzzing)]
                    return "<unspecified>".to_owned();

                    #[cfg(not(fuzzing))]
                    SOURCE_MAP.with(|sm| {
                        let sm = sm.borrow();
                        sm.filepath(*self)
                    })
                }

                pub fn local_file( &self ) -> Option<PathBuf>
                    {
                    None
                }

                pub fn join( &self, other: Span) -> Option<Span>
                    {
                    #[cfg(fuzzing)]
                    return {
                        let _ = other;
                        None
                    };

                    #[cfg(not(fuzzing))]
                    SOURCE_MAP.with(|sm| {
                        let sm = sm.borrow();
                       
                        if !sm.fileinfo(*self).span_within(other) {
                            return None;
                        }
                        Some(Span {
                            lo: cmp::min(self.lo, other.lo),
                            hi: cmp::max(self.hi, other.hi),
                        })
                    })
                }
                
                pub fn source_text( &self ) -> Option<String>
                    {
                    #[cfg(fuzzing)]
                    return None;

                    #[cfg(not(fuzzing))]
                    {
                        if self.is_call_site() {
                            None
                        } else {
                            Some(SOURCE_MAP.with(|sm| sm.borrow_mut().fileinfo_mut(*self).source_text(*self)))
                        }
                    }
                }
                
                pub fn first_byte( self ) -> Self {
                    Span {
                        lo: self.lo,
                        hi: cmp::min(self.lo.saturating_add(1), self.hi),
                    }
                }

                pub fn last_byte( self ) -> Self {
                    Span {
                        lo: cmp::max(self.hi.saturating_sub(1), self.lo),
                        hi: self.hi,
                    }
                }
                
                fn is_call_site( &self ) -> bool {
                    self.lo == 0 && self.hi == 0
                }
            }

            impl Debug for Span
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                            return write!(f, "bytes({}..{})", self.lo, self.hi);

                }
            }

            pub fn debug_span_field_if_nontrivial(debug: &mut fmt::DebugStruct, span: Span) {
                    {
                    if span.is_call_site() {
                        return;
                    }
                }

                if cfg!(span_locations) {
                    debug.field("span", &span);
                }
            }

            #[derive(Clone)]
            pub struct Group {
                delimiter: Delimiter,
                stream: TokenStream,
                span: Span,
            }

            impl Group
            {
                pub fn new(delimiter: Delimiter, stream: TokenStream) -> Self {
                    Group {
                        delimiter,
                        stream,
                        span: Span::call_site(),
                    }
                }

                pub fn delimiter( &self ) -> Delimiter {
                    self.delimiter
                }

                pub fn stream( &self ) -> TokenStream {
                    self.stream.clone()
                }

                pub fn span( &self ) -> Span {
                    self.span
                }

                pub fn span_open( &self ) -> Span {
                    self.span.first_byte()
                }

                pub fn span_close( &self ) -> Span {
                    self.span.last_byte()
                }

                pub fn set_span(&mut self, span: Span) {
                    self.span = span;
                }
            }

            impl Display for Group 
            {
               
               
               
               
               
               
               
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    let (open, close) = match self.delimiter {
                        Delimiter::Parenthesis => ("(", ")"),
                        Delimiter::Brace => ("{ ", "}"),
                        Delimiter::Bracket => ("[", "]"),
                        Delimiter::None => ("", ""),
                    };

                    f.write_str(open)?;
                    Display::fmt(&self.stream, f)?;
                    /*
                    if self.delimiter == Delimiter::Brace && !self.stream.inner.is_empty() {
                        f.write_str(" ")?;
                    } */
                    f.write_str(close)?;

                    Ok(())
                }
            }

            impl Debug for Group
            {
                fn fmt( &self, fmt: &mut fmt::Formatter) -> fmt::Result
                {
                    let mut debug = fmt.debug_struct("Group");
                    debug.field("delimiter", &self.delimiter);
                    debug.field("stream", &self.stream);
                    debug_span_field_if_nontrivial(&mut debug, self.span);
                    debug.finish()
                }
            }

            #[derive(Clone)]
            pub struct Ident 
            {
                sym: Box<str>,
                span: Span,
                raw: bool,
            }

            impl Ident 
            {
                #[track_caller]
                pub fn new_checked(string: &str, span: Span) -> Self {
                    validate_ident(string);
                    Ident::new_unchecked(string, span)
                }

                pub fn new_unchecked(string: &str, span: Span) -> Self {
                    Ident {
                        sym: Box::from(string),
                        span,
                        raw: false,
                    }
                }

                #[track_caller]
                pub fn new_raw_checked(string: &str, span: Span) -> Self {
                    validate_ident_raw(string);
                    Ident::new_raw_unchecked(string, span)
                }

                pub fn new_raw_unchecked(string: &str, span: Span) -> Self {
                    Ident {
                        sym: Box::from(string),
                        span,
                        raw: true,
                    }
                }

                pub fn span( &self ) -> Span {
                    self.span
                }

                pub fn set_span(&mut self, span: Span) {
                    self.span = span;
                }
            }

            #[track_caller] fn validate_ident(string: &str) {
                if string.is_empty() {
                    panic!("Ident is not allowed to be empty; use Option<Ident>");
                }

                if string.bytes().all(|digit| b'0' <= digit && digit <= b'9') {
                    panic!("Ident cannot be a number; use Literal instead");
                }

                fn ident_ok(string: &str) -> bool
                {
                    let mut chars = string.chars();
                    let first = chars.next().unwrap();
                    if !::is::ident_start(first) {
                        return false;
                    }
                    for ch in chars {
                        if !::is::ident_continue(ch) {
                            return false;
                        }
                    }
                    true
                }

                if !ident_ok(string) {
                    panic!("{:?} is not a valid Ident", string);
                }
            }

            #[track_caller] fn validate_ident_raw(string: &str) {
                validate_ident(string);

                match string {
                    "_" | "super" | "self" | "Self" | "crate" =>
                    {
                        panic!("`r#{}` cannot be a raw identifier", string);
                    }
                    _ =>
                    {}
                }
            }

            impl PartialEq for Ident
            {
                fn eq( &self, other: &Ident) -> bool {
                    self.sym == other.sym && self.raw == other.raw
                }
            }

            impl<T> PartialEq<T> for Ident where
            T: ?Sized + AsRef<str>
            {
                fn eq( &self, other: &T) -> bool
                {
                    let other = other.as_ref();
                    if self.raw {
                        other.starts_with("r#") && *self.sym == other[2..]
                    } else {
                        *self.sym == *other
                    }
                }
            }

            impl Display for Ident
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                    if self.raw {
                        f.write_str("r#")?;
                    }
                    Display::fmt(&self.sym, f)
                }
            }
            
            impl Debug for Ident
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
               
                {
                    let mut debug = f.debug_struct("Ident");
                    debug.field("sym", &format_args!("{}", self));
                    debug_span_field_if_nontrivial(&mut debug, self.span);
                    debug.finish()
                }
            }

            #[derive(Clone)]
            pub struct Literal {
                pub repr: String,
                span: Span,
            }

            impl Literal
            {
                pub fn _new(repr: String) -> Self {
                    Literal {
                        repr,
                        span: Span::call_site(),
                    }
                }

                pub fn from_str_checked(repr: &str) -> Result<Self, LexError>
                {
                    let mut cursor = get_cursor(repr);
                            let lo = cursor.off;

                    let negative = cursor.starts_with_char('-');
                    if negative {
                        cursor = cursor.advance(1);
                        if !cursor.starts_with_fn(|ch| ch.is_ascii_digit()) {
                            return Err(LexError::call_site());
                        }
                    }

                    if let Ok((rest, mut literal)) = parse::literal(cursor) {
                        if rest.is_empty() {
                            if negative {
                                literal.repr.insert(0, '-');
                            }
                            literal.span = Span {
                                                    lo,
                                                    hi: rest.off,
                            };
                            return Ok(literal);
                        }
                    }
                    Err(LexError::call_site())
                }

                pub unsafe fn from_str_unchecked(repr: &str) -> Self {
                    Literal::_new(repr.to_owned())
                }

                suffixed_numbers! {
                    u8_suffixed => u8,
                    u16_suffixed => u16,
                    u32_suffixed => u32,
                    u64_suffixed => u64,
                    u128_suffixed => u128,
                    usize_suffixed => usize,
                    i8_suffixed => i8,
                    i16_suffixed => i16,
                    i32_suffixed => i32,
                    i64_suffixed => i64,
                    i128_suffixed => i128,
                    isize_suffixed => isize,

                    f32_suffixed => f32,
                    f64_suffixed => f64,
                }

                unsuffixed_numbers! {
                    u8_unsuffixed => u8,
                    u16_unsuffixed => u16,
                    u32_unsuffixed => u32,
                    u64_unsuffixed => u64,
                    u128_unsuffixed => u128,
                    usize_unsuffixed => usize,
                    i8_unsuffixed => i8,
                    i16_unsuffixed => i16,
                    i32_unsuffixed => i32,
                    i64_unsuffixed => i64,
                    i128_unsuffixed => i128,
                    isize_unsuffixed => isize,
                }

                pub fn f32_unsuffixed(f: f32) -> Literal
                {
                    let mut s = f.to_string();
                    if !s.contains('.') {
                        s.push_str(".0");
                    }
                    Literal::_new(s)
                }

                pub fn f64_unsuffixed(f: f64) -> Literal
                {
                    let mut s = f.to_string();
                    if !s.contains('.') {
                        s.push_str(".0");
                    }
                    Literal::_new(s)
                }

                pub fn string(string: &str) -> Literal
                {
                    let mut repr = String::with_capacity(string.len() + 2);
                    repr.push('"');
                    escape_utf8(string, &mut repr);
                    repr.push('"');
                    Literal::_new(repr)
                }

                pub fn character(ch: char) -> Literal
                {
                    let mut repr = String::new();
                    repr.push('\'');
                    if ch == '"' {
                       
                        repr.push(ch);
                    } else {
                        repr.extend(ch.escape_debug());
                    }
                    repr.push('\'');
                    Literal::_new(repr)
                }

                pub fn byte_character(byte: u8) -> Literal
                {
                    let mut repr = "b'".to_string();
                    #[allow(clippy::match_overlapping_arm)]
                    match byte {
                        b'\0' => repr.push_str(r"\0"),
                        b'\t' => repr.push_str(r"\t"),
                        b'\n' => repr.push_str(r"\n"),
                        b'\r' => repr.push_str(r"\r"),
                        b'\'' => repr.push_str(r"\'"),
                        b'\\' => repr.push_str(r"\\"),
                        b'\x20'..=b'\x7E' => repr.push(byte as char),
                        _ =>
                    {
                            let _ = write!(repr, r"\x{:02X}", byte);
                        }
                    }
                    repr.push('\'');
                    Literal::_new(repr)
                }

                pub fn byte_string(bytes: &[u8]) -> Literal
                {
                    let mut repr = "b\"".to_string();
                    let mut bytes = bytes.iter();
                    while let Some(&b) = bytes.next() {
                        #[allow(clippy::match_overlapping_arm)]
                        match b {
                            b'\0' => repr.push_str(match bytes.as_slice().first() {
                               
                                Some(b'0'..=b'7') => r"\x00",
                                _ => r"\0",
                            }),
                            b'\t' => repr.push_str(r"\t"),
                            b'\n' => repr.push_str(r"\n"),
                            b'\r' => repr.push_str(r"\r"),
                            b'"' => repr.push_str("\\\""),
                            b'\\' => repr.push_str(r"\\"),
                            b'\x20'..=b'\x7E' => repr.push(b as char),
                            _ =>
                    {
                                let _ = write!(repr, r"\x{:02X}", b);
                            }
                        }
                    }
                    repr.push('"');
                    Literal::_new(repr)
                }

                pub fn c_string(string: &CStr) -> Literal
                {
                    let mut repr = "c\"".to_string();
                    let mut bytes = string.to_bytes();
                    while !bytes.is_empty() {
                        let (valid, invalid) = match str::from_utf8(bytes) {
                            Ok(all_valid) =>
                    {
                                bytes = b"";
                                (all_valid, bytes)
                            }
                            Err(utf8_error) =>
                    {
                                let (valid, rest) = bytes.split_at(utf8_error.valid_up_to());
                                let valid = str::from_utf8(valid).unwrap();
                                let invalid = utf8_error
                                    .error_len()
                                    .map_or(rest, |error_len| &rest[..error_len]);
                                bytes = &bytes[valid.len() + invalid.len()..];
                                (valid, invalid)
                            }
                        };
                        escape_utf8(valid, &mut repr);
                        for &byte in invalid {
                            let _ = write!(repr, r"\x{:02X}", byte);
                        }
                    }
                    repr.push('"');
                    Literal::_new(repr)
                }

                pub fn span( &self ) -> Span {
                    self.span
                }

                pub fn set_span(&mut self, span: Span) {
                    self.span = span;
                }

                pub fn subspan<R: RangeBounds<usize>>( &self, range: R) -> Option<Span> 
                {
                    use ::ops::Bound;

                    let lo = match range.start_bound() {
                        Bound::Included(start) =>
                    {
                            let start = u32::try_from(*start).ok()?;
                            self.span.lo.checked_add(start)?
                        }
                        Bound::Excluded(start) =>
                    {
                            let start = u32::try_from(*start).ok()?;
                            self.span.lo.checked_add(start)?.checked_add(1)?
                        }
                        Bound::Unbounded => self.span.lo,
                    };
                    let hi = match range.end_bound() {
                        Bound::Included(end) =>
                    {
                            let end = u32::try_from(*end).ok()?;
                            self.span.lo.checked_add(end)?.checked_add(1)?
                        }
                        Bound::Excluded(end) =>
                    {
                            let end = u32::try_from(*end).ok()?;
                            self.span.lo.checked_add(end)?
                        }
                        Bound::Unbounded => self.span.hi,
                    };
                    if lo <= hi && hi <= self.span.hi {
                        Some(Span { lo, hi })
                    } else {
                        None
                    }
                }
            }

            impl Display for Literal
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                    Display::fmt(&self.repr, f)
                }
            }

            impl Debug for Literal
            {
                fn fmt( &self, fmt: &mut fmt::Formatter) -> fmt::Result
                {
                    let mut debug = fmt.debug_struct("Literal");
                    debug.field("lit", &format_args!("{}", self.repr));
                    debug_span_field_if_nontrivial(&mut debug, self.span);
                    debug.finish()
                }
            }
            
            pub trait FromStr2: FromStr<Err = proc_macro::LexError>
            {
                    fn valid(src: &str) -> bool;

                    fn from_str_checked(src: &str) -> Result<Self, imp::LexError>
                    {
                   
                   
                   
                    if !Self::valid(src) {
                        return Err(imp::LexError::CompilerPanic);
                    }

                   
                    match panic::catch_unwind(|| Self::from_str(src)) {
                        Ok(Ok(ok)) => Ok(ok),
                        Ok(Err(lex)) => Err(imp::LexError::Compiler(lex)),
                        Err(_panic) => Err(imp::LexError::CompilerPanic),
                    }
                }

                fn from_str_unchecked(src: &str) -> Self {
                    Self::from_str(src).unwrap()
                }
            }
            
            impl FromStr2 for proc_macro::TokenStream
            {
                fn valid(src: &str) -> bool 
                {
                    TokenStream::from_str_checked(src).is_ok()
                }
            }
            
            impl FromStr2 for proc_macro::Literal
            {
                fn valid(src: &str) -> bool 
                {
                    Literal::from_str_checked(src).is_ok()
                }
            }

            fn escape_utf8(string: &str, repr: &mut String)
            {
                let mut chars = string.chars();
                while let Some(ch) = chars.next() {
                    if ch == '\0' {
                        repr.push_str(
                            if chars
                                .as_str()
                                .starts_with(|next| '0' <= next && next <= '7')
                            {
                               
                                r"\x00"
                            } else {
                                r"\0"
                            },
                        );
                    } else if ch == '\'' {
                       
                        repr.push(ch);
                    } else {
                        repr.extend(ch.escape_debug());
                    }
                }
            }
        }
           
        pub mod imp
        {
            use ::
            {
                ffi::{ CStr },
                fmt::{ self, Debug, Display },
                ops::{ Range, RangeBounds },
                path::{ PathBuf },
                process::
                {
                    macros::
                    {
                        detection::inside_proc_macro,
                        fallback::{self, FromStr2 as _},
                        location::LineColumn,
                        probe::{ proc_macro_span, proc_macro_span_file, proc_macro_span_location },
                        Delimiter, Punct, Spacing, TokenTree,                        
                    },
                },
                *,
            };
            /*
            */
            #[derive(Clone)]
            pub enum TokenStream 
            {
                Compiler(DeferredTokenStream),
                Fallback(fallback::TokenStream),
            }

            #[derive(Clone)]
            pub struct DeferredTokenStream 
            {
                stream: proc_macro::TokenStream,
                extra: Vec<proc_macro::TokenTree>,
            }

            pub enum LexError
            {
                Compiler(proc_macro::LexError),
                Fallback(fallback::LexError),
                CompilerPanic,
            }

            #[cold] fn mismatch(line: u32) -> !
            {
                let backtrace = ::backtrace::Backtrace::force_capture();
                panic!("compiler/fallback mismatch L{}\n\n{}", line, backtrace)
            }

            impl DeferredTokenStream           
            {
                fn new(stream: proc_macro::TokenStream) -> Self
                {
                    DeferredTokenStream
                    {
                        stream,
                        extra: Vec::new(),
                    }
                }

                fn is_empty( &self ) -> bool { self.stream.is_empty() && self.extra.is_empty() }

                fn evaluate_now( &mut self )
                {
                    if !self.extra.is_empty() { self.stream.extend(self.extra.drain(..)); }
                }

                fn into_token_stream(mut self) -> proc_macro::TokenStream
                {
                    self.evaluate_now();
                    self.stream
                }
            }

            impl TokenStream           
            {
                pub fn new() -> Self
                {
                    if inside_proc_macro() {
                        TokenStream::Compiler(DeferredTokenStream::new(proc_macro::TokenStream::new()))
                    } else {
                        TokenStream::Fallback(fallback::TokenStream::new())
                    }
                }

                pub fn from_str_checked(src: &str) -> Result<Self, LexError>
                    {
                    if inside_proc_macro() {
                        Ok(TokenStream::Compiler(DeferredTokenStream::new(
                            proc_macro::TokenStream::from_str_checked(src)?,
                        )))
                    } else {
                        Ok(TokenStream::Fallback(
                            fallback::TokenStream::from_str_checked(src)?,
                        ))
                    }
                }

                pub fn is_empty( &self ) -> bool
                {
                    match self {
                        TokenStream::Compiler(tts) => tts.is_empty(),
                        TokenStream::Fallback(tts) => tts.is_empty(),
                    }
                }

                fn unwrap_nightly( self ) -> proc_macro::TokenStream
                {
                    match self {
                        TokenStream::Compiler(s) => s.into_token_stream(),
                        TokenStream::Fallback(_) => mismatch(line!()),
                    }
                }

                fn unwrap_stable( self ) -> fallback::TokenStream           
                {
                    match self {
                        TokenStream::Compiler(_) => mismatch(line!()),
                        TokenStream::Fallback(s) => s,
                    }
                }
            }

            impl Display for TokenStream                  
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        TokenStream::Compiler(tts) => Display::fmt(&tts.clone().into_token_stream(), f),
                        TokenStream::Fallback(tts) => Display::fmt(tts, f),
                    }
                }
            }

            impl From<proc_macro::TokenStream> for TokenStream           
            {
                fn from(inner: proc_macro::TokenStream) -> Self
                {
                    TokenStream::Compiler(DeferredTokenStream::new(inner))
                }
            }

            impl From<TokenStream> for proc_macro::TokenStream           
            {
                fn from(inner: TokenStream) -> Self
                {
                    match inner {
                        TokenStream::Compiler(inner) => inner.into_token_stream(),
                        TokenStream::Fallback(inner) =>
                    {
                            proc_macro::TokenStream::from_str_unchecked(&inner.to_string())
                        }
                    }
                }
            }

            impl From<fallback::TokenStream> for TokenStream           
            {
                fn from(inner: fallback::TokenStream) -> Self
                {
                    TokenStream::Fallback(inner)
                }
            }
            
            fn into_compiler_token(token: TokenTree) -> proc_macro::TokenTree
            {
                match token {
                    TokenTree::Group(tt) => proc_macro::TokenTree::Group(tt.inner.unwrap_nightly()),
                    TokenTree::Punct(tt) =>
                    {
                        let spacing = match tt.spacing() {
                            Spacing::Joint => proc_macro::Spacing::Joint,
                            Spacing::Alone => proc_macro::Spacing::Alone,
                        };
                        let mut punct = proc_macro::Punct::new(tt.as_char(), spacing);
                        punct.set_span(tt.span().inner.unwrap_nightly());
                        proc_macro::TokenTree::Punct(punct)
                    }
                    TokenTree::Ident(tt) => proc_macro::TokenTree::Ident(tt.inner.unwrap_nightly()),
                    TokenTree::Literal(tt) => proc_macro::TokenTree::Literal(tt.inner.unwrap_nightly()),
                }
            }

            impl From<TokenTree> for TokenStream           
            {
                fn from(token: TokenTree) -> Self
                {
                    if inside_proc_macro() {
                        TokenStream::Compiler(DeferredTokenStream::new(proc_macro::TokenStream::from(
                            into_compiler_token(token),
                        )))
                    } else {
                        TokenStream::Fallback(fallback::TokenStream::from(token))
                    }
                }
            }

            impl iter::FromIterator<TokenTree> for TokenStream           
            {
                fn from_iter<I: IntoIterator<Item = TokenTree>>(trees: I) -> Self
                {
                    if inside_proc_macro() {
                        TokenStream::Compiler(DeferredTokenStream::new(
                            trees.into_iter().map(into_compiler_token).collect(),
                        ))
                    } else {
                        TokenStream::Fallback(trees.into_iter().collect())
                    }
                }
            }

            impl iter::FromIterator<TokenStream> for TokenStream
            {
                fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self
               
                {
                    let mut streams = streams.into_iter();
                    match streams.next() {
                        Some(TokenStream::Compiler(mut first)) =>
                    {
                            first.evaluate_now();
                            first.stream.extend(streams.map(|s| match s {
                                TokenStream::Compiler(s) => s.into_token_stream(),
                                TokenStream::Fallback(_) => mismatch(line!()),
                            }));
                            TokenStream::Compiler(first)
                        }
                        Some(TokenStream::Fallback(mut first)) =>
                    {
                            first.extend(streams.map(|s| match s {
                                TokenStream::Fallback(s) => s,
                                TokenStream::Compiler(_) => mismatch(line!()),
                            }));
                            TokenStream::Fallback(first)
                        }
                        None => TokenStream::new(),
                    }
                }
            }

            impl Extend<TokenTree> for TokenStream           
            {
                fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, stream: I)
                {
                    match self {
                        TokenStream::Compiler(tts) =>
                    {
                           
                            for token in stream {
                                tts.extra.push(into_compiler_token(token));
                            }
                        }
                        TokenStream::Fallback(tts) => tts.extend(stream),
                    }
                }
            }

            impl Extend<TokenStream> for TokenStream           
            {
                fn extend<I: IntoIterator<Item = TokenStream>>(&mut self, streams: I)
                {
                    match self {
                        TokenStream::Compiler(tts) =>
                    {
                            tts.evaluate_now();
                            tts.stream
                                .extend(streams.into_iter().map(TokenStream::unwrap_nightly));
                        }
                        TokenStream::Fallback(tts) =>
                    {
                            tts.extend(streams.into_iter().map(TokenStream::unwrap_stable));
                        }
                    }
                }
            }

            impl Debug for TokenStream           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        TokenStream::Compiler(tts) => Debug::fmt(&tts.clone().into_token_stream(), f),
                        TokenStream::Fallback(tts) => Debug::fmt(tts, f),
                    }
                }
            }

            impl LexError
            {
                pub fn span( &self ) -> Span
                {
                    match self {
                        LexError::Compiler(_) | LexError::CompilerPanic => Span::call_site(),
                        LexError::Fallback(e) => Span::Fallback(e.span()),
                    }
                }
            }

            impl From<proc_macro::LexError> for LexError           
            {
                fn from(e: proc_macro::LexError) -> Self
                {
                    LexError::Compiler(e)
                }
            }

            impl From<fallback::LexError> for LexError           
            {
                fn from(e: fallback::LexError) -> Self
                {
                    LexError::Fallback(e)
                }
            }

            impl Debug for LexError           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        LexError::Compiler(e) => Debug::fmt(e, f),
                        LexError::Fallback(e) => Debug::fmt(e, f),
                        LexError::CompilerPanic =>
                    {
                            let fallback = fallback::LexError::call_site();
                            Debug::fmt(&fallback, f)
                        }
                    }
                }
            }

            impl Display for LexError           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        LexError::Compiler(e) => Display::fmt(e, f),
                        LexError::Fallback(e) => Display::fmt(e, f),
                        LexError::CompilerPanic =>
                    {
                            let fallback = fallback::LexError::call_site();
                            Display::fmt(&fallback, f)
                        }
                    }
                }
            }

            #[derive(Clone)]
            pub enum TokenTreeIter 
            {
                Compiler(proc_macro::token_stream::IntoIter),
                Fallback(fallback::TokenTreeIter),
            }

            impl IntoIterator for TokenStream
            {
                type Item = TokenTree;
                type IntoIter = TokenTreeIter;
                fn into_iter( self ) -> TokenTreeIter
                {
                    match self {
                        TokenStream::Compiler(tts) =>
                    {
                            TokenTreeIter::Compiler(tts.into_token_stream().into_iter())
                        }
                        TokenStream::Fallback(tts) => TokenTreeIter::Fallback(tts.into_iter()),
                    }
                }
            }

            impl Iterator for TokenTreeIter
            {
                type Item = TokenTree;
                fn next( &mut self ) -> Option<TokenTree>
                {
                    let token = match self {
                        TokenTreeIter::Compiler(iter) => iter.next()?,
                        TokenTreeIter::Fallback(iter) => return iter.next(),
                    };
                    Some(match token {
                        proc_macro::TokenTree::Group(tt) =>
                    {
                            TokenTree::Group(::process::macros::Group::_new(Group::Compiler(tt)))
                        }
                        proc_macro::TokenTree::Punct(tt) =>
                    {
                            let spacing = match tt.spacing() {
                                proc_macro::Spacing::Joint => Spacing::Joint,
                                proc_macro::Spacing::Alone => Spacing::Alone,
                            };
                            let mut o = Punct::new(tt.as_char(), spacing);
                            o.set_span(::process::macros::Span::_new(Span::Compiler(tt.span())));
                            TokenTree::Punct(o)
                        }
                        proc_macro::TokenTree::Ident(s) =>
                    {
                            TokenTree::Ident(::process::macros::Ident::_new(Ident::Compiler(s)))
                        }
                        proc_macro::TokenTree::Literal(l) =>
                    {
                            TokenTree::Literal(::process::macros::Literal::_new(Literal::Compiler(l)))
                        }
                    })
                }

                fn size_hint( &self ) -> (usize, Option<usize>)
                {
                    match self {
                        TokenTreeIter::Compiler(tts) => tts.size_hint(),
                        TokenTreeIter::Fallback(tts) => tts.size_hint(),
                    }
                }
            }

            #[derive(Copy, Clone)]
            pub enum Span
            {
                Compiler(proc_macro::Span),
                Fallback(fallback::Span),
            }

            impl Span           
            {
                pub fn call_site() -> Self
                {
                    if inside_proc_macro() {
                        Span::Compiler(proc_macro::Span::call_site())
                    } else {
                        Span::Fallback(fallback::Span::call_site())
                    }
                }

                pub fn mixed_site() -> Self
                {
                    if inside_proc_macro() {
                        Span::Compiler(proc_macro::Span::mixed_site())
                    } else {
                        Span::Fallback(fallback::Span::mixed_site())
                    }
                }
                
                pub fn def_site() -> Self
                {
                    Span::Fallback(fallback::Span::def_site())
                    /*
                    if inside_proc_macro() {
                        Span::Compiler(proc_macro::Span::def_site())
                    } else {
                        Span::Fallback(fallback::Span::def_site())
                    } */
                }

                pub fn resolved_at( &self, other: Span) -> Span 
                {
                    match (self, other) {
                        (Span::Compiler(a), Span::Compiler(b)) => Span::Compiler(a.resolved_at(b)),
                        (Span::Fallback(a), Span::Fallback(b)) => Span::Fallback(a.resolved_at(b)),
                        (Span::Compiler(_), Span::Fallback(_)) => mismatch(line!()),
                        (Span::Fallback(_), Span::Compiler(_)) => mismatch(line!()),
                    }
                }

                pub fn located_at( &self, other: Span) -> Span 
                {
                    match (self, other) {
                        (Span::Compiler(a), Span::Compiler(b)) => Span::Compiler(a.located_at(b)),
                        (Span::Fallback(a), Span::Fallback(b)) => Span::Fallback(a.located_at(b)),
                        (Span::Compiler(_), Span::Fallback(_)) => mismatch(line!()),
                        (Span::Fallback(_), Span::Compiler(_)) => mismatch(line!()),
                    }
                }

                pub fn unwrap( self ) -> proc_macro::Span
                {
                    match self {
                        Span::Compiler(s) => s,
                        Span::Fallback(_) => panic!("proc_macro::Span is only available in procedural macros"),
                    }
                }

                pub fn byte_range( &self ) -> Range<usize>
                {
                    match self {
                        #[cfg(proc_macro_span)]
                        Span::Compiler(s) => proc_macro_span::byte_range(s),
                        #[cfg(not(proc_macro_span))]
                        Span::Compiler(_) => 0..0,
                        Span::Fallback(s) => s.byte_range(),
                    }
                }

                pub fn start( &self ) -> LineColumn
                {
                    match self {
                        #[cfg(proc_macro_span_location)]
                        Span::Compiler(s) => LineColumn {
                            line: proc_macro_span_location::line(s),
                            column: proc_macro_span_location::column(s).saturating_sub(1),
                        },
                        #[cfg(not(proc_macro_span_location))]
                        Span::Compiler(_) => LineColumn { line: 0, column: 0 },
                        Span::Fallback(s) => s.start(),
                    }
                }

                pub fn end( &self ) -> LineColumn
                {
                    match self {
                        #[cfg(proc_macro_span_location)]
                        Span::Compiler(s) =>
                    {
                            let end = proc_macro_span_location::end(s);
                            LineColumn {
                                line: proc_macro_span_location::line(&end),
                                column: proc_macro_span_location::column(&end).saturating_sub(1),
                            }
                        }
                        #[cfg(not(proc_macro_span_location))]
                        Span::Compiler(_) => LineColumn { line: 0, column: 0 },
                        Span::Fallback(s) => s.end(),
                    }
                }

                pub fn file( &self ) -> String
                {
                    match self {
                        #[cfg(proc_macro_span_file)]
                        Span::Compiler(s) => proc_macro_span_file::file(s),
                        #[cfg(not(proc_macro_span_file))]
                        Span::Compiler(_) => "<token stream>".to_owned(),
                        Span::Fallback(s) => s.file(),
                    }
                }

                pub fn local_file( &self ) -> Option<PathBuf>
                {
                    match self {
                        #[cfg(proc_macro_span_file)]
                        Span::Compiler(s) => proc_macro_span_file::local_file(s),
                        #[cfg(not(proc_macro_span_file))]
                        Span::Compiler(_) => None,
                        Span::Fallback(s) => s.local_file(),
                    }
                }

                pub fn join( &self, other: Span) -> Option<Span> 
               
                {
                    let ret = match (self, other) {
                        #[cfg(proc_macro_span)]
                        (Span::Compiler(a), Span::Compiler(b)) => Span::Compiler(proc_macro_span::join(a, b)?),
                        (Span::Fallback(a), Span::Fallback(b)) => Span::Fallback(a.join(b)?),
                        _ => return None,
                    };
                    Some(ret)
                }
                
                pub fn eq( &self, other: &Span) -> bool 
                {
                    match (self, other)
                    {
                        //(Span::Compiler(a), Span::Compiler(b)) => a.eq(b),
                        (Span::Fallback(a), Span::Fallback(b)) => a.eq(b),
                        _ => false,
                    }
                }

                pub fn source_text( &self ) -> Option<String>
                {
                    match self {
                        #[cfg(not(no_source_text))]
                        Span::Compiler(s) => s.source_text(),
                        #[cfg(no_source_text)]
                        Span::Compiler(_) => None,
                        Span::Fallback(s) => s.source_text(),
                    }
                }

                fn unwrap_nightly( self ) -> proc_macro::Span
                {
                    match self {
                        Span::Compiler(s) => s,
                        Span::Fallback(_) => mismatch(line!()),
                    }
                }
            }

            impl From<proc_macro::Span> for ::process::macros::Span           
            {
                fn from(proc_span: proc_macro::Span) -> Self
                {
                    ::process::macros::Span::_new(Span::Compiler(proc_span))
                }
            }

            impl From<fallback::Span> for Span           
            {
                fn from(inner: fallback::Span) -> Self
                {
                    Span::Fallback(inner)
                }
            }

            impl Debug for Span           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Span::Compiler(s) => Debug::fmt(s, f),
                        Span::Fallback(s) => Debug::fmt(s, f),
                    }
                }
            }

            pub fn debug_span_field_if_nontrivial(debug: &mut fmt::DebugStruct, span: Span)
            {
                match span {
                    Span::Compiler(s) =>
                    {
                        debug.field("span", &s);
                    }
                    Span::Fallback(s) => fallback::debug_span_field_if_nontrivial(debug, s),
                }
            }

            #[derive(Clone)]
            pub enum Group
            {
                Compiler(proc_macro::Group),
                Fallback(fallback::Group),
            }

            impl Group           
            {
                pub fn new(delimiter: Delimiter, stream: TokenStream) -> Self
                {
                    match stream {
                        TokenStream::Compiler(tts) =>
                    {
                            let delimiter = match delimiter {
                                Delimiter::Parenthesis => proc_macro::Delimiter::Parenthesis,
                                Delimiter::Bracket => proc_macro::Delimiter::Bracket,
                                Delimiter::Brace => proc_macro::Delimiter::Brace,
                                Delimiter::None => proc_macro::Delimiter::None,
                            };
                            Group::Compiler(proc_macro::Group::new(delimiter, tts.into_token_stream()))
                        }
                        TokenStream::Fallback(stream) =>
                    {
                            Group::Fallback(fallback::Group::new(delimiter, stream))
                        }
                    }
                }

                pub fn delimiter( &self ) -> Delimiter
                {
                    match self {
                        Group::Compiler(g) => match g.delimiter() {
                            proc_macro::Delimiter::Parenthesis => Delimiter::Parenthesis,
                            proc_macro::Delimiter::Bracket => Delimiter::Bracket,
                            proc_macro::Delimiter::Brace => Delimiter::Brace,
                            proc_macro::Delimiter::None => Delimiter::None,
                        },
                        Group::Fallback(g) => g.delimiter(),
                    }
                }

                pub fn stream( &self ) -> TokenStream
                {
                    match self {
                        Group::Compiler(g) => TokenStream::Compiler(DeferredTokenStream::new(g.stream())),
                        Group::Fallback(g) => TokenStream::Fallback(g.stream()),
                    }
                }

                pub fn span( &self ) -> Span
                {
                    match self {
                        Group::Compiler(g) => Span::Compiler(g.span()),
                        Group::Fallback(g) => Span::Fallback(g.span()),
                    }
                }

                pub fn span_open( &self ) -> Span
                {
                    match self {
                        Group::Compiler(g) => Span::Compiler(g.span_open()),
                        Group::Fallback(g) => Span::Fallback(g.span_open()),
                    }
                }

                pub fn span_close( &self ) -> Span
                {
                    match self {
                        Group::Compiler(g) => Span::Compiler(g.span_close()),
                        Group::Fallback(g) => Span::Fallback(g.span_close()),
                    }
                }

                pub fn set_span(&mut self, span: Span) {
                    match (self, span) {
                        (Group::Compiler(g), Span::Compiler(s)) => g.set_span(s),
                        (Group::Fallback(g), Span::Fallback(s)) => g.set_span(s),
                        (Group::Compiler(_), Span::Fallback(_)) => mismatch(line!()),
                        (Group::Fallback(_), Span::Compiler(_)) => mismatch(line!()),
                    }
                }

                fn unwrap_nightly( self ) -> proc_macro::Group
                {
                    match self {
                        Group::Compiler(g) => g,
                        Group::Fallback(_) => mismatch(line!()),
                    }
                }
            }

            impl From<fallback::Group> for Group           
            {
                fn from(g: fallback::Group) -> Self
                {
                    Group::Fallback(g)
                }
            }

            impl Display for Group           
            {
                fn fmt( &self, formatter: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Group::Compiler(group) => Display::fmt(group, formatter),
                        Group::Fallback(group) => Display::fmt(group, formatter),
                    }
                }
            }

            impl Debug for Group           
            {
                fn fmt( &self, formatter: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Group::Compiler(group) => Debug::fmt(group, formatter),
                        Group::Fallback(group) => Debug::fmt(group, formatter),
                    }
                }
            }

            #[derive(Clone)]
            pub enum Ident
            {
                Compiler(proc_macro::Ident),
                Fallback(fallback::Ident),
            }

            impl Ident 
            {
                #[track_caller]
                pub fn new_checked(string: &str, span: Span) -> Self
                {
                    match span {
                        Span::Compiler(s) => Ident::Compiler(proc_macro::Ident::new(string, s)),
                        Span::Fallback(s) => Ident::Fallback(fallback::Ident::new_checked(string, s)),
                    }
                }

                #[track_caller]
                pub fn new_raw_checked(string: &str, span: Span) -> Self
                {
                    match span {
                        Span::Compiler(s) => Ident::Compiler(proc_macro::Ident::new_raw(string, s)),
                        Span::Fallback(s) => Ident::Fallback(fallback::Ident::new_raw_checked(string, s)),
                    }
                }

                pub fn span( &self ) -> Span
                {
                    match self {
                        Ident::Compiler(t) => Span::Compiler(t.span()),
                        Ident::Fallback(t) => Span::Fallback(t.span()),
                    }
                }

                pub fn set_span(&mut self, span: Span) {
                    match (self, span) {
                        (Ident::Compiler(t), Span::Compiler(s)) => t.set_span(s),
                        (Ident::Fallback(t), Span::Fallback(s)) => t.set_span(s),
                        (Ident::Compiler(_), Span::Fallback(_)) => mismatch(line!()),
                        (Ident::Fallback(_), Span::Compiler(_)) => mismatch(line!()),
                    }
                }

                fn unwrap_nightly( self ) -> proc_macro::Ident
                {
                    match self {
                        Ident::Compiler(s) => s,
                        Ident::Fallback(_) => mismatch(line!()),
                    }
                }
            }

            impl From<fallback::Ident> for Ident           
            {
                fn from(inner: fallback::Ident) -> Self
                {
                    Ident::Fallback(inner)
                }
            }

            impl PartialEq for Ident           
            {
                fn eq( &self, other: &Ident) -> bool {
                    match (self, other) {
                        (Ident::Compiler(t), Ident::Compiler(o)) => t.to_string() == o.to_string(),
                        (Ident::Fallback(t), Ident::Fallback(o)) => t == o,
                        (Ident::Compiler(_), Ident::Fallback(_)) => mismatch(line!()),
                        (Ident::Fallback(_), Ident::Compiler(_)) => mismatch(line!()),
                    }
                }
            }

            impl<T> PartialEq<T> for Ident where
            T: ?Sized + AsRef<str>,
            {
                fn eq( &self, other: &T) -> bool
                {
                    let other = other.as_ref();
                    match self {
                        Ident::Compiler(t) => t.to_string() == other,
                        Ident::Fallback(t) => t == other,
                    }
                }
            }

            impl Display for Ident           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Ident::Compiler(t) => Display::fmt(t, f),
                        Ident::Fallback(t) => Display::fmt(t, f),
                    }
                }
            }

            impl Debug for Ident           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Ident::Compiler(t) => Debug::fmt(t, f),
                        Ident::Fallback(t) => Debug::fmt(t, f),
                    }
                }
            }

            #[derive(Clone)]
            pub enum Literal 
            {
                Compiler(proc_macro::Literal),
                Fallback(fallback::Literal),
            }

            macro_rules! suffixed_numbers 
            {
                ($($name:ident => $kind:ident,)*) => ($(
                    pub fn $name(n: $kind) -> Literal {
                        if inside_proc_macro() {
                            Literal::Compiler(proc_macro::Literal::$name(n))
                        } else {
                            Literal::Fallback(fallback::Literal::$name(n))
                        }
                    }
                )*)
            }

            macro_rules! unsuffixed_integers 
            {
                ($($name:ident => $kind:ident,)*) => ($(
                    pub fn $name(n: $kind) -> Literal {
                        if inside_proc_macro() {
                            Literal::Compiler(proc_macro::Literal::$name(n))
                        } else {
                            Literal::Fallback(fallback::Literal::$name(n))
                        }
                    }
                )*)
            }

            impl Literal
            {
                pub fn from_str_checked(repr: &str) -> Result<Self, LexError>
                    {
                    if inside_proc_macro() {
                        let literal = proc_macro::Literal::from_str_checked(repr)?;
                        Ok(Literal::Compiler(literal))
                    } else {
                        let literal = fallback::Literal::from_str_checked(repr)?;
                        Ok(Literal::Fallback(literal))
                    }
                }

                pub unsafe fn from_str_unchecked(repr: &str) -> Self
                {
                    if inside_proc_macro() {
                        Literal::Compiler(proc_macro::Literal::from_str_unchecked(repr))
                    } else {
                        Literal::Fallback(unsafe { fallback::Literal::from_str_unchecked(repr) })
                    }
                }

                suffixed_numbers! {
                    u8_suffixed => u8,
                    u16_suffixed => u16,
                    u32_suffixed => u32,
                    u64_suffixed => u64,
                    u128_suffixed => u128,
                    usize_suffixed => usize,
                    i8_suffixed => i8,
                    i16_suffixed => i16,
                    i32_suffixed => i32,
                    i64_suffixed => i64,
                    i128_suffixed => i128,
                    isize_suffixed => isize,

                    f32_suffixed => f32,
                    f64_suffixed => f64,
                }

                unsuffixed_integers! {
                    u8_unsuffixed => u8,
                    u16_unsuffixed => u16,
                    u32_unsuffixed => u32,
                    u64_unsuffixed => u64,
                    u128_unsuffixed => u128,
                    usize_unsuffixed => usize,
                    i8_unsuffixed => i8,
                    i16_unsuffixed => i16,
                    i32_unsuffixed => i32,
                    i64_unsuffixed => i64,
                    i128_unsuffixed => i128,
                    isize_unsuffixed => isize,
                }

                pub fn f32_unsuffixed(f: f32) -> Literal {
                    if inside_proc_macro() {
                        Literal::Compiler(proc_macro::Literal::f32_unsuffixed(f))
                    } else {
                        Literal::Fallback(fallback::Literal::f32_unsuffixed(f))
                    }
                }

                pub fn f64_unsuffixed(f: f64) -> Literal {
                    if inside_proc_macro() {
                        Literal::Compiler(proc_macro::Literal::f64_unsuffixed(f))
                    } else {
                        Literal::Fallback(fallback::Literal::f64_unsuffixed(f))
                    }
                }

                pub fn string(string: &str) -> Literal {
                    if inside_proc_macro() {
                        Literal::Compiler(proc_macro::Literal::string(string))
                    } else {
                        Literal::Fallback(fallback::Literal::string(string))
                    }
                }

                pub fn character(ch: char) -> Literal {
                    if inside_proc_macro() {
                        Literal::Compiler(proc_macro::Literal::character(ch))
                    } else {
                        Literal::Fallback(fallback::Literal::character(ch))
                    }
                }

                pub fn byte_character(byte: u8) -> Literal {
                    if inside_proc_macro() {
                        Literal::Compiler({
                            #[cfg(not(no_literal_byte_character))]
                            {
                                proc_macro::Literal::byte_character(byte)
                            }

                            #[cfg(no_literal_byte_character)]
                            {
                                let fallback = fallback::Literal::byte_character(byte);
                                proc_macro::Literal::from_str_unchecked(&fallback.repr)
                            }
                        })
                    } else {
                        Literal::Fallback(fallback::Literal::byte_character(byte))
                    }
                }

                pub fn byte_string(bytes: &[u8]) -> Literal {
                    if inside_proc_macro() {
                        Literal::Compiler(proc_macro::Literal::byte_string(bytes))
                    } else {
                        Literal::Fallback(fallback::Literal::byte_string(bytes))
                    }
                }

                pub fn c_string(string: &CStr) -> Literal {
                    if inside_proc_macro() {
                        Literal::Compiler({
                            #[cfg(not(no_literal_c_string))]
                            {
                                proc_macro::Literal::c_string(string)
                            }

                            #[cfg(no_literal_c_string)]
                            {
                                let fallback = fallback::Literal::c_string(string);
                                proc_macro::Literal::from_str_unchecked(&fallback.repr)
                            }
                        })
                    } else {
                        Literal::Fallback(fallback::Literal::c_string(string))
                    }
                }

                pub fn span( &self ) -> Span
                {
                    match self {
                        Literal::Compiler(lit) => Span::Compiler(lit.span()),
                        Literal::Fallback(lit) => Span::Fallback(lit.span()),
                    }
                }

                pub fn set_span(&mut self, span: Span) {
                    match (self, span) {
                        (Literal::Compiler(lit), Span::Compiler(s)) => lit.set_span(s),
                        (Literal::Fallback(lit), Span::Fallback(s)) => lit.set_span(s),
                        (Literal::Compiler(_), Span::Fallback(_)) => mismatch(line!()),
                        (Literal::Fallback(_), Span::Compiler(_)) => mismatch(line!()),
                    }
                }

                pub fn subspan<R: RangeBounds<usize>>( &self, range: R) -> Option<Span>
                {
                    match self {
                        #[cfg(proc_macro_span)]
                        Literal::Compiler(lit) => proc_macro_span::subspan(lit, range).map(Span::Compiler),
                        #[cfg(not(proc_macro_span))]
                        Literal::Compiler(_lit) => None,
                        Literal::Fallback(lit) => lit.subspan(range).map(Span::Fallback),
                    }
                }

                fn unwrap_nightly( self ) -> proc_macro::Literal
                {
                    match self {
                        Literal::Compiler(s) => s,
                        Literal::Fallback(_) => mismatch(line!()),
                    }
                }
            }

            impl From<fallback::Literal> for Literal           
            {
                fn from(s: fallback::Literal) -> Self
                {
                    Literal::Fallback(s)
                }
            }

            impl Display for Literal           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Literal::Compiler(t) => Display::fmt(t, f),
                        Literal::Fallback(t) => Display::fmt(t, f),
                    }
                }
            }

            impl Debug for Literal           
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Literal::Compiler(t) => Debug::fmt(t, f),
                        Literal::Fallback(t) => Debug::fmt(t, f),
                    }
                }
            }
            
            pub fn invalidate_current_thread_spans()
            {
                if inside_proc_macro()
                {
                    panic!
                    (
                        "proc_macro2::extra::invalidate_current_thread_spans is not available in procedural macros"
                    );
                }
                
                else
                {
                    ::process::macros::fallback::invalidate_current_thread_spans();
                }
            }
        }
        
        pub mod location
        {
            use ::
            {
                cmp::{ Ordering },
                *,
            };
            /*
            */
            /// A line-column pair representing the start or end of a `Span`.
            #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
            pub struct LineColumn 
            {
                /// The 1-indexed line in the source file on which the span starts or ends (inclusive).
                pub line: usize,
                /// The 0-indexed column (in UT8) in the source file on which the span starts or ends (inclusive).
                pub column: usize,
            }

            impl Ord for LineColumn
            {
                fn cmp( &self, other: &Self) -> Ordering
                {
                    self.line
                    .cmp(&other.line)
                    .then(self.column.cmp(&other.column))
                }
            }

            impl PartialOrd for LineColumn
            {
                fn partial_cmp( &self, other: &Self) -> Option<Ordering>
                {
                    Some(self.cmp(other))
                }
            }
        }
        /**
        A wrapper around the procedural macro API of the compiler's [`proc_macro`] crate.*/
        pub mod parse
        {
            use ::
            {
                process::
                {
                    macros::
                    {
                        fallback::
                        {
                            self, Group, Ident, LexError, Literal, Span, TokenStream, TokenStreamBuilder,
                        },
                        Delimiter, Punct, Spacing, TokenTree,
                    },
                },
                str::{ Bytes, CharIndices, Chars },
                *,
            };
            /*
            */
            /// Rustc's representation of a macro expansion error in expression position or type position.
            pub const ERROR: &str = "(/*ERROR*/)";

            pub type PResult<'a, O> = Result<(Cursor<'a>, O), Reject>;
            
            macro_rules! next_ch
            {
                ($chars:ident @ $pat:pat) =>
                {
                    match $chars.next()
                    {
                        Some((_, ch)) => match ch
                        {
                            $pat => ch,
                            _ => return Err(Reject),
                        },
                        None => return Err(Reject),
                    }
                };
            }

            #[derive(Copy, Clone, Eq, PartialEq)]
            pub struct Cursor<'a> 
            {
                pub rest: &'a str,
                    pub off: u32,
            }

            impl<'a> Cursor<'a>
            {
                pub fn advance( &self, bytes: usize) -> Cursor<'a>
                {
                    let (_front, rest) = self.rest.split_at(bytes);
                    Cursor {
                        rest,
                                    off: self.off + _front.chars().count() as u32,
                    }
                }

                pub fn starts_with( &self, s: &str) -> bool {
                    self.rest.starts_with(s)
                }

                pub fn starts_with_char( &self, ch: char) -> bool {
                    self.rest.starts_with(ch)
                }

                pub fn starts_with_fn<Pattern>( &self, f: Pattern) -> bool
                where
                    Pattern: FnMut(char) -> bool,
                {
                    self.rest.starts_with(f)
                }

                pub fn is_empty( &self ) -> bool {
                    self.rest.is_empty()
                }

                fn len( &self ) -> usize {
                    self.rest.len()
                }

                fn as_bytes( &self ) -> &'a [u8] {
                    self.rest.as_bytes()
                }

                fn bytes( &self ) -> Bytes<'a>
                    {
                    self.rest.bytes()
                }

                fn chars( &self ) -> Chars<'a>
                    {
                    self.rest.chars()
                }

                fn char_indices( &self ) -> CharIndices<'a>
                    {
                    self.rest.char_indices()
                }

                fn parse( &self, tag: &str) -> Result<Cursor<'a>, Reject>
                    {
                    if self.starts_with(tag) {
                        Ok(self.advance(tag.len()))
                    } else {
                        Err(Reject)
                    }
                }
            }

            pub struct Reject;
            
            fn skip_whitespace(input: Cursor) -> Cursor
            {
                let mut s = input;

                while !s.is_empty()
               
                {
                    let byte = s.as_bytes()[0];
                    
                    if byte == b'/'
                    {
                        if s.starts_with("//")
                            && (!s.starts_with("///") || s.starts_with("////"))
                            && !s.starts_with("//!")
                        {
                            let (cursor, _) = take_until_newline_or_eof(s);
                            s = cursor;
                            continue;
                        } else if s.starts_with("/**/") {
                            s = s.advance(4);
                            continue;
                        } else if s.starts_with("/*")
                            && (!s.starts_with("/**") || s.starts_with("/***"))
                            && !s.starts_with("/*!")
                        {
                            match block_comment(s) {
                                Ok((rest, _)) =>
                    {
                                    s = rest;
                                    continue;
                                }
                                Err(Reject) => return s,
                            }
                        }
                    }
                    
                    match byte
                    {
                        b' ' | 0x09..=0x0d =>
                        {
                            s = s.advance(1);
                            continue;
                        }

                        b if b.is_ascii() =>
                    {}
                        _ =>
                        {
                            let ch = s.chars().next().unwrap();

                            if is::whitespace(ch)
                            {
                                s = s.advance(ch.len_utf8());
                                continue;
                            }
                        }
                    }
                    return s;
                }
                s
            }

            fn block_comment(input: Cursor<'_>) -> PResult<'_, &str>
            {
                if !input.starts_with("/*") {
                    return Err(Reject);
                }

                let mut depth = 0usize;
                let bytes = input.as_bytes();
                let mut i = 0usize;
                let upper = bytes.len() - 1;

                while i < upper {
                    if bytes[i] == b'/' && bytes[i + 1] == b'*' {
                        depth += 1;
                        i += 1;
                    } else if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                        depth -= 1;
                        if depth == 0 {
                            return Ok((input.advance(i + 2), &input.rest[..i + 2]));
                        }
                        i += 1;
                    }
                    i += 1;
                }

                Err(Reject)
            }

            fn word_break(input: Cursor) -> Result<Cursor, Reject>
            {
                match input.chars().next() {
                    Some(ch) if is::ident_continue(ch) => Err(Reject),
                    Some(_) | None => Ok(input),
                }
            }

            pub fn token_stream(mut input: Cursor) -> Result<TokenStream, LexError>
            {
                let mut trees = TokenStreamBuilder::new();
                let mut stack = Vec::new();

                loop {
                    input = skip_whitespace(input);

                    if let Ok((rest, ())) = doc_comment(input, &mut trees) {
                        input = rest;
                        continue;
                    }

                            let lo = input.off;

                    let first = match input.bytes().next() {
                        Some(first) => first,
                        None => match stack.last() {
                            None => return Ok(trees.build()),
                                            Some((lo, _frame)) =>
                    {
                                return Err(LexError {
                                    span: Span { lo: *lo, hi: *lo },
                                })
                            }
                        },
                    };

                    if let Some(open_delimiter) = match first {
                        b'(' if !input.starts_with(ERROR) => Some(Delimiter::Parenthesis),
                        b'[' => Some(Delimiter::Bracket),
                        b'{' => Some(Delimiter::Brace),
                        _ => None,
                    } {
                        input = input.advance(1);
                        let frame = (open_delimiter, trees);
                                    let frame = (lo, frame);
                        stack.push(frame);
                        trees = TokenStreamBuilder::new();
                    } else if let Some(close_delimiter) = match first {
                        b')' => Some(Delimiter::Parenthesis),
                        b']' => Some(Delimiter::Bracket),
                        b'}' => Some(Delimiter::Brace),
                        _ => None,
                    } {
                        let frame = match stack.pop() {
                            Some(frame) => frame,
                            None => return Err(lex_error(input)),
                        };
                                    let (lo, frame) = frame;
                        let (open_delimiter, outer) = frame;
                        if open_delimiter != close_delimiter {
                            return Err(lex_error(input));
                        }
                        input = input.advance(1);
                        let mut g = Group::new(open_delimiter, trees.build());
                        g.set_span(Span {
                                            lo,
                                            hi: input.off,
                        });
                        trees = outer;
                        trees.push_token_from_parser(TokenTree::Group(::process::macros::Group::_new_fallback(g)));
                    } else {
                        let (rest, mut tt) = match leaf_token(input) {
                            Ok((rest, tt)) => (rest, tt),
                            Err(Reject) => return Err(lex_error(input)),
                        };
                        tt.set_span(::process::macros::Span::_new_fallback(Span {
                                            lo,
                                            hi: rest.off,
                        }));
                        trees.push_token_from_parser(tt);
                        input = rest;
                    }
                }
            }

            fn lex_error(cursor: Cursor) -> LexError
            {
                LexError
                {
                    span: Span
                    {
                        lo:cursor.off,
                        hi:cursor.off,
                    },
                }
            }

            fn leaf_token(input: Cursor) -> PResult<TokenTree>
            {
                if let Ok((input, l)) = literal(input) {
                   
                    Ok((input, TokenTree::Literal(::process::macros::Literal::_new_fallback(l))))
                } else if let Ok((input, p)) = punct(input) {
                    Ok((input, TokenTree::Punct(p)))
                } else if let Ok((input, i)) = ident(input) {
                    Ok((input, TokenTree::Ident(i)))
                } else if input.starts_with(ERROR)
                {
                    let rest = input.advance(ERROR.len());
                    let repr = ::process::macros::Literal::_new_fallback(Literal::_new(ERROR.to_owned()));
                    Ok((rest, TokenTree::Literal(repr)))
                } else {
                    Err(Reject)
                }
            }

            fn ident(input: Cursor) -> PResult<::process::macros::Ident>
            {
                if [
                    "r\"", "r#\"", "r##", "b\"", "b\'", "br\"", "br#", "c\"", "cr\"", "cr#",
                ]
                .iter()
                .any(|prefix| input.starts_with(prefix))
                {
                    Err(Reject)
                } else {
                    ident_any(input)
                }
            }

            fn ident_any(input: Cursor) -> PResult<::process::macros::Ident>
            {
                let raw = input.starts_with("r#");
                let rest = input.advance((raw as usize) << 1);

                let (rest, sym) = ident_not_raw(rest)?;

                if !raw
                {
                    let ident =
                        ::process::macros::Ident::_new_fallback(Ident::new_unchecked(sym, fallback::Span::call_site()));
                    return Ok((rest, ident));
                }

                match sym {
                    "_" | "super" | "self" | "Self" | "crate" => return Err(Reject),
                    _ =>
                    {}
                }

                let ident =
                    ::process::macros::Ident::_new_fallback(Ident::new_raw_unchecked(sym, fallback::Span::call_site()));
                Ok((rest, ident))
            }

            fn ident_not_raw(input: Cursor<'_>) -> PResult<'_, &str>
            {
                let mut chars = input.char_indices();

                match chars.next() {
                    Some((_, ch)) if is::ident_start(ch) =>
                    {}
                    _ => return Err(Reject),
                }

                let mut end = input.len();
                for (i, ch) in chars {
                    if !is::ident_continue(ch) {
                        end = i;
                        break;
                    }
                }

                Ok((input.advance(end), &input.rest[..end]))
            }

            pub fn literal(input: Cursor) -> PResult<Literal>
            {
                let rest = literal_nocapture(input)?;
                let end = input.len() - rest.len();
                Ok((rest, Literal::_new(input.rest[..end].to_string())))
            }

            fn literal_nocapture(input: Cursor) -> Result<Cursor, Reject>
            {
                if let Ok(ok) = string(input) {
                    Ok(ok)
                } else if let Ok(ok) = byte_string(input) {
                    Ok(ok)
                } else if let Ok(ok) = c_string(input) {
                    Ok(ok)
                } else if let Ok(ok) = byte(input) {
                    Ok(ok)
                } else if let Ok(ok) = character(input) {
                    Ok(ok)
                } else if let Ok(ok) = float(input) {
                    Ok(ok)
                } else if let Ok(ok) = int(input) {
                    Ok(ok)
                } else {
                    Err(Reject)
                }
            }

            fn literal_suffix(input: Cursor) -> Cursor
            {
                match ident_not_raw(input) {
                    Ok((input, _)) => input,
                    Err(Reject) => input,
                }
            }

            fn string(input: Cursor) -> Result<Cursor, Reject>
            {
                if let Ok(input) = input.parse("\"") {
                    cooked_string(input)
                } else if let Ok(input) = input.parse("r") {
                    raw_string(input)
                } else {
                    Err(Reject)
                }
            }

            fn cooked_string(mut input: Cursor) -> Result<Cursor, Reject>
            {
                let mut chars = input.char_indices();

                while let Some((i, ch)) = chars.next() {
                    match ch {
                        '"' =>
                    {
                            let input = input.advance(i + 1);
                            return Ok(literal_suffix(input));
                        }
                        '\r' => match chars.next() {
                            Some((_, '\n')) =>
                    {}
                            _ => break,
                        },
                        '\\' => match chars.next() {
                            Some((_, 'x')) =>
                    {
                                backslash_x_char(&mut chars)?;
                            }
                            Some((_, 'n' | 'r' | 't' | '\\' | '\'' | '"' | '0')) =>
                    {}
                            Some((_, 'u')) =>
                    {
                                backslash_u(&mut chars)?;
                            }
                            Some((newline, ch @ ('\n' | '\r'))) =>
                    {
                                input = input.advance(newline + 1);
                                trailing_backslash(&mut input, ch as u8)?;
                                chars = input.char_indices();
                            }
                            _ => break,
                        },
                        _ch =>
                    {}
                    }
                }
                Err(Reject)
            }

            fn raw_string(input: Cursor) -> Result<Cursor, Reject>
            {
                let (input, delimiter) = delimiter_of_raw_string(input)?;
                let mut bytes = input.bytes().enumerate();
                while let Some((i, byte)) = bytes.next() {
                    match byte {
                        b'"' if input.rest[i + 1..].starts_with(delimiter) =>
                    {
                            let rest = input.advance(i + 1 + delimiter.len());
                            return Ok(literal_suffix(rest));
                        }
                        b'\r' => match bytes.next() {
                            Some((_, b'\n')) =>
                    {}
                            _ => break,
                        },
                        _ =>
                    {}
                    }
                }
                Err(Reject)
            }

            fn byte_string(input: Cursor) -> Result<Cursor, Reject>
            {
                if let Ok(input) = input.parse("b\"") {
                    cooked_byte_string(input)
                } else if let Ok(input) = input.parse("br") {
                    raw_byte_string(input)
                } else {
                    Err(Reject)
                }
            }

            fn cooked_byte_string(mut input: Cursor) -> Result<Cursor, Reject>
            {
                let mut bytes = input.bytes().enumerate();
                while let Some((offset, b)) = bytes.next() {
                    match b {
                        b'"' =>
                    {
                            let input = input.advance(offset + 1);
                            return Ok(literal_suffix(input));
                        }
                        b'\r' => match bytes.next() {
                            Some((_, b'\n')) =>
                    {}
                            _ => break,
                        },
                        b'\\' => match bytes.next() {
                            Some((_, b'x')) =>
                    {
                                backslash_x_byte(&mut bytes)?;
                            }
                            Some((_, b'n' | b'r' | b't' | b'\\' | b'0' | b'\'' | b'"')) =>
                    {}
                            Some((newline, b @ (b'\n' | b'\r'))) =>
                    {
                                input = input.advance(newline + 1);
                                trailing_backslash(&mut input, b)?;
                                bytes = input.bytes().enumerate();
                            }
                            _ => break,
                        },
                        b if b.is_ascii() =>
                    {}
                        _ => break,
                    }
                }
                Err(Reject)
            }

            fn delimiter_of_raw_string(input: Cursor<'_>) -> PResult<'_, &str>
            {
                for (i, byte) in input.bytes().enumerate() {
                    match byte {
                        b'"' =>
                    {
                            if i > 255 {
                               
                                return Err(Reject);
                            }
                            return Ok((input.advance(i + 1), &input.rest[..i]));
                        }
                        b'#' =>
                    {}
                        _ => break,
                    }
                }
                Err(Reject)
            }

            fn raw_byte_string(input: Cursor) -> Result<Cursor, Reject>
            {
                let (input, delimiter) = delimiter_of_raw_string(input)?;
                let mut bytes = input.bytes().enumerate();
                while let Some((i, byte)) = bytes.next() {
                    match byte {
                        b'"' if input.rest[i + 1..].starts_with(delimiter) =>
                    {
                            let rest = input.advance(i + 1 + delimiter.len());
                            return Ok(literal_suffix(rest));
                        }
                        b'\r' => match bytes.next() {
                            Some((_, b'\n')) =>
                    {}
                            _ => break,
                        },
                        other =>
                    {
                            if !other.is_ascii() {
                                break;
                            }
                        }
                    }
                }
                Err(Reject)
            }

            fn c_string(input: Cursor) -> Result<Cursor, Reject>
            {
                if let Ok(input) = input.parse("c\"") {
                    cooked_c_string(input)
                } else if let Ok(input) = input.parse("cr") {
                    raw_c_string(input)
                } else {
                    Err(Reject)
                }
            }

            fn raw_c_string(input: Cursor) -> Result<Cursor, Reject>
            {
                let (input, delimiter) = delimiter_of_raw_string(input)?;
                let mut bytes = input.bytes().enumerate();
                while let Some((i, byte)) = bytes.next() {
                    match byte {
                        b'"' if input.rest[i + 1..].starts_with(delimiter) =>
                    {
                            let rest = input.advance(i + 1 + delimiter.len());
                            return Ok(literal_suffix(rest));
                        }
                        b'\r' => match bytes.next() {
                            Some((_, b'\n')) =>
                    {}
                            _ => break,
                        },
                        b'\0' => break,
                        _ =>
                    {}
                    }
                }
                Err(Reject)
            }

            fn cooked_c_string(mut input: Cursor) -> Result<Cursor, Reject>
            {
                let mut chars = input.char_indices();

                while let Some((i, ch)) = chars.next() {
                    match ch {
                        '"' =>
                    {
                            let input = input.advance(i + 1);
                            return Ok(literal_suffix(input));
                        }
                        '\r' => match chars.next() {
                            Some((_, '\n')) =>
                    {}
                            _ => break,
                        },
                        '\\' => match chars.next() {
                            Some((_, 'x')) =>
                    {
                                backslash_x_nonzero(&mut chars)?;
                            }
                            Some((_, 'n' | 'r' | 't' | '\\' | '\'' | '"')) =>
                    {}
                            Some((_, 'u')) =>
                    {
                                if backslash_u(&mut chars)? == '\0' {
                                    break;
                                }
                            }
                            Some((newline, ch @ ('\n' | '\r'))) =>
                    {
                                input = input.advance(newline + 1);
                                trailing_backslash(&mut input, ch as u8)?;
                                chars = input.char_indices();
                            }
                            _ => break,
                        },
                        '\0' => break,
                        _ch =>
                    {}
                    }
                }
                Err(Reject)
            }

            fn byte(input: Cursor) -> Result<Cursor, Reject>
            {
                let input = input.parse("b'")?;
                let mut bytes = input.bytes().enumerate();
                let ok = match bytes.next().map(|(_, b)| b) {
                    Some(b'\\') => match bytes.next().map(|(_, b)| b) {
                        Some(b'x') => backslash_x_byte(&mut bytes).is_ok(),
                        Some(b'n' | b'r' | b't' | b'\\' | b'0' | b'\'' | b'"') => true,
                        _ => false,
                    },
                    b => b.is_some(),
                };
                if !ok {
                    return Err(Reject);
                }
                let (offset, _) = bytes.next().ok_or(Reject)?;
                if !input.chars().as_str().is_char_boundary(offset) {
                    return Err(Reject);
                }
                let input = input.advance(offset).parse("'")?;
                Ok(literal_suffix(input))
            }

            fn character(input: Cursor) -> Result<Cursor, Reject>
            {
                let input = input.parse("'")?;
                let mut chars = input.char_indices();
                let ok = match chars.next().map(|(_, ch)| ch) {
                    Some('\\') => match chars.next().map(|(_, ch)| ch) {
                        Some('x') => backslash_x_char(&mut chars).is_ok(),
                        Some('u') => backslash_u(&mut chars).is_ok(),
                        Some('n' | 'r' | 't' | '\\' | '0' | '\'' | '"') => true,
                        _ => false,
                    },
                    ch => ch.is_some(),
                };
                if !ok {
                    return Err(Reject);
                }
                let (idx, _) = chars.next().ok_or(Reject)?;
                let input = input.advance(idx).parse("'")?;
                Ok(literal_suffix(input))
            }
            
            fn backslash_x_char<I>(chars: &mut I) -> Result<(), Reject> where
            I: Iterator<Item = (usize, char)>,
            {
                let _ = match chars.next()
                {
                    Some((_, ch)) => match ch
                    {
                        '0'..='7' => ch,
                        _ => return Err(Reject),
                    },
                    None => todo!(),
                };

                let _ = match chars.next()
                {
                    Some((_, ch)) => match ch
                    {
                        '0'..='9' | 'a'..='f' | 'A'..='F' => ch,
                        _ => return Err(Reject),
                    },
                    None => todo!(),
                };

                Ok(())
            } 

            fn backslash_x_byte<I>(chars: &mut I) -> Result<(), Reject> where
            I: Iterator<Item = (usize, u8)>,
            {
                /*
                ($chars:ident @ $pat:pat) =>
                {
                    match $chars.next()
                    {
                        Some((_, ch)) => match ch
                        {
                            $pat => ch,
                            _ => return Err(Reject),
                        },
                        None => return Err(Reject),
                    }
                };
                */
                let _ = match chars.next()
                {
                    Some((_, ch)) => match ch
                    {
                        b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' => ch,
                        _ => return Err(Reject),
                    },
                    None => return Err(Reject),
                };

                let _ = match chars.next()
                {
                    Some((_, ch)) => match ch
                    {
                        b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' => ch,
                        _ => return Err(Reject),
                    },
                    None => return Err(Reject),
                };

                Ok(())
            }

            fn backslash_x_nonzero<I>(chars: &mut I) -> Result<(), Reject> where
            I: Iterator<Item = (usize, char)>,
            {
                let first = match chars.next()
                {
                    Some((_, ch)) => match ch
                    {
                        '0'..='9' | 'a'..='f' | 'A'..='F' => ch,
                        _ => return Err(Reject),
                    },
                    None => return Err(Reject),
                };
                
                let second = match chars.next()
                {
                    Some((_, ch)) => match ch
                    {
                        '0'..='9' | 'a'..='f' | 'A'..='F' => ch,
                        _ => return Err(Reject),
                    },
                    None => return Err(Reject),
                };
                
                if first == '0' && second == '0' {
                    Err(Reject)
                } else {
                    Ok(())
                }
            }

            fn backslash_u<I>(chars: &mut I) -> Result<char, Reject> where
            I: Iterator<Item = (usize, char)>,
            {
                next_ch!(chars @ '{');
                let mut value = 0;
                let mut len = 0;
                for (_, ch) in chars
                {
                    let digit = match ch {
                        '0'..='9' => ch as u8 - b'0',
                        'a'..='f' => 10 + ch as u8 - b'a',
                        'A'..='F' => 10 + ch as u8 - b'A',
                        '_' if len > 0 => continue,
                        '}' if len > 0 => return char::from_u32(value).ok_or(Reject),
                        _ => break,
                    };
                    if len == 6 {
                        break;
                    }
                    value *= 0x10;
                    value += u32::from(digit);
                    len += 1;
                }
                Err(Reject)
            }

            fn trailing_backslash(input: &mut Cursor, mut last: u8) -> Result<(), Reject>
            {
                let mut whitespace = input.bytes().enumerate();
                loop {
                    if last == b'\r' && whitespace.next().map_or(true, |(_, b)| b != b'\n') {
                        return Err(Reject);
                    }
                    match whitespace.next() {
                        Some((_, b @ (b' ' | b'\t' | b'\n' | b'\r'))) =>
                    {
                            last = b;
                        }
                        Some((offset, _)) =>
                    {
                            *input = input.advance(offset);
                            return Ok(());
                        }
                        None => return Err(Reject),
                    }
                }
            }

            fn float(input: Cursor) -> Result<Cursor, Reject>
            {
                let mut rest = float_digits(input)?;
                if let Some(ch) = rest.chars().next() {
                    if is::ident_start(ch) {
                        rest = ident_not_raw(rest)?.0;
                    }
                }
                word_break(rest)
            }

            fn float_digits(input: Cursor) -> Result<Cursor, Reject>
            {
                let mut chars = input.chars().peekable();
                match chars.next() {
                    Some(ch) if '0' <= ch && ch <= '9' =>
                    {}
                    _ => return Err(Reject),
                }

                let mut len = 1;
                let mut has_dot = false;
                let mut has_exp = false;
                while let Some(&ch) = chars.peek() {
                    match ch {
                        '0'..='9' | '_' =>
                    {
                            chars.next();
                            len += 1;
                        }
                        '.' =>
                    {
                            if has_dot {
                                break;
                            }
                            chars.next();
                            if chars
                                .peek()
                                .map_or(false, |&ch| ch == '.' || is::ident_start(ch))
                            {
                                return Err(Reject);
                            }
                            len += 1;
                            has_dot = true;
                        }
                        'e' | 'E' =>
                    {
                            chars.next();
                            len += 1;
                            has_exp = true;
                            break;
                        }
                        _ => break,
                    }
                }

                if !(has_dot || has_exp) {
                    return Err(Reject);
                }

                if has_exp
                {
                    let token_before_exp = if has_dot {
                        Ok(input.advance(len - 1))
                    } else {
                        Err(Reject)
                    };
                    let mut has_sign = false;
                    let mut has_exp_value = false;
                    while let Some(&ch) = chars.peek() {
                        match ch {
                            '+' | '-' =>
                    {
                                if has_exp_value {
                                    break;
                                }
                                if has_sign {
                                    return token_before_exp;
                                }
                                chars.next();
                                len += 1;
                                has_sign = true;
                            }
                            '0'..='9' =>
                    {
                                chars.next();
                                len += 1;
                                has_exp_value = true;
                            }
                            '_' =>
                    {
                                chars.next();
                                len += 1;
                            }
                            _ => break,
                        }
                    }
                    if !has_exp_value {
                        return token_before_exp;
                    }
                }

                Ok(input.advance(len))
            }

            fn int(input: Cursor) -> Result<Cursor, Reject>
            {
                let mut rest = digits(input)?;
                if let Some(ch) = rest.chars().next() {
                    if is::ident_start(ch) {
                        rest = ident_not_raw(rest)?.0;
                    }
                }
                word_break(rest)
            }

            fn digits(mut input: Cursor) -> Result<Cursor, Reject>
            {
                let base = if input.starts_with("0x") {
                    input = input.advance(2);
                    16
                } else if input.starts_with("0o") {
                    input = input.advance(2);
                    8
                } else if input.starts_with("0b") {
                    input = input.advance(2);
                    2
                } else {
                    10
                };

                let mut len = 0;
                let mut empty = true;
                for b in input.bytes() {
                    match b {
                        b'0'..=b'9' =>
                    {
                            let digit = (b - b'0') as u64;
                            if digit >= base {
                                return Err(Reject);
                            }
                        }
                        b'a'..=b'f' =>
                    {
                            let digit = 10 + (b - b'a') as u64;
                            if digit >= base {
                                break;
                            }
                        }
                        b'A'..=b'F' =>
                    {
                            let digit = 10 + (b - b'A') as u64;
                            if digit >= base {
                                break;
                            }
                        }
                        b'_' =>
                    {
                            if empty && base == 10 {
                                return Err(Reject);
                            }
                            len += 1;
                            continue;
                        }
                        _ => break,
                    }
                    len += 1;
                    empty = false;
                }
                if empty {
                    Err(Reject)
                } else {
                    Ok(input.advance(len))
                }
            }

            fn punct(input: Cursor) -> PResult<Punct>
            {
                let (rest, ch) = punct_char(input)?;
                if ch == '\''
                {
                    let (after_lifetime, _ident) = ident_any(rest)?;
                    if after_lifetime.starts_with_char('\'')
                        || (after_lifetime.starts_with_char('#') && !rest.starts_with("r#"))
                    {
                        Err(Reject)
                    } else {
                        Ok((rest, Punct::new('\'', Spacing::Joint)))
                    }
                } else
                {
                    let kind = match punct_char(rest) {
                        Ok(_) => Spacing::Joint,
                        Err(Reject) => Spacing::Alone,
                    };
                    Ok((rest, Punct::new(ch, kind)))
                }
            }

            fn punct_char(input: Cursor) -> PResult<char>
            {
                if input.starts_with("//") || input.starts_with("/*") {
                   
                    return Err(Reject);
                }

                let mut chars = input.chars();
                let first = match chars.next() {
                    Some(ch) => ch,
                    None =>
                    {
                        return Err(Reject);
                    }
                };
                let recognized = "~!@#$%^&*-=+|;:,<.>/?'";
                if recognized.contains(first) {
                    Ok((input.advance(first.len_utf8()), first))
                } else {
                    Err(Reject)
                }
            }

            fn doc_comment<'a>(input: Cursor<'a>, trees: &mut TokenStreamBuilder) -> PResult<'a, ()>
            {
                let lo = input.off;
                let (rest, (comment, inner)) = doc_comment_contents(input)?;
                let fallback_span = Span {
                            lo,
                            hi: rest.off,
                };
                let span = ::process::macros::Span::_new_fallback(fallback_span);

                let mut scan_for_bare_cr = comment;
                while let Some(cr) = scan_for_bare_cr.find('\r')
                {
                    let rest = &scan_for_bare_cr[cr + 1..];
                    if !rest.starts_with('\n') {
                        return Err(Reject);
                    }
                    scan_for_bare_cr = rest;
                }

                let mut pound = Punct::new('#', Spacing::Alone);
                pound.set_span(span);
                trees.push_token_from_parser(TokenTree::Punct(pound));

                if inner
                {
                    let mut bang = Punct::new('!', Spacing::Alone);
                    bang.set_span(span);
                    trees.push_token_from_parser(TokenTree::Punct(bang));
                }

                let doc_ident = ::process::macros::Ident::_new_fallback(Ident::new_unchecked("doc", fallback_span));
                let mut equal = Punct::new('=', Spacing::Alone);
                equal.set_span(span);
                let mut literal = ::process::macros::Literal::_new_fallback(Literal::string(comment));
                literal.set_span(span);
                let mut bracketed = TokenStreamBuilder::with_capacity(3);
                bracketed.push_token_from_parser(TokenTree::Ident(doc_ident));
                bracketed.push_token_from_parser(TokenTree::Punct(equal));
                bracketed.push_token_from_parser(TokenTree::Literal(literal));
                let group = Group::new(Delimiter::Bracket, bracketed.build());
                let mut group = ::process::macros::Group::_new_fallback(group);
                group.set_span(span);
                trees.push_token_from_parser(TokenTree::Group(group));

                Ok((rest, ()))
            }

            fn doc_comment_contents(input: Cursor<'_>) -> PResult<'_, (&str, bool)>
            {
                if input.starts_with("//!")
                {
                    let input = input.advance(3);
                    let (input, s) = take_until_newline_or_eof(input);
                    Ok((input, (s, true)))
                } else if input.starts_with("/*!")
                {
                    let (input, s) = block_comment(input)?;
                    Ok((input, (&s[3..s.len() - 2], true)))
                } else if input.starts_with("///")
                {
                    let input = input.advance(3);
                    if input.starts_with_char('/') {
                        return Err(Reject);
                    }
                    let (input, s) = take_until_newline_or_eof(input);
                    Ok((input, (s, false)))
                } else if input.starts_with("/**") && !input.rest[3..].starts_with('*')
                {
                    let (input, s) = block_comment(input)?;
                    Ok((input, (&s[3..s.len() - 2], false)))
                } else {
                    Err(Reject)
                }
            }

            fn take_until_newline_or_eof(input: Cursor<'_>) -> (Cursor<'_>, &str) 
            {
                let chars = input.char_indices();

                for (i, ch) in chars {
                    if ch == '\n' {
                        return (input.advance(i), &input.rest[..i]);
                    } else if ch == '\r' && input.rest[i + 1..].starts_with('\n') {
                        return (input.advance(i + 1), &input.rest[..i]);
                    }
                }

                (input.advance(input.len()), input.rest)
            }
        }

        pub mod probe
        {
            use ::
            {
                *,
            };
            /*
            */
            pub mod proc_macro_span
            {
                use ::
                {
                    ops::{ Range, RangeBounds },
                    path::{ PathBuf },
                    proc_macro::{ Literal, Span },
                    *,
                };
                /*
                */
                pub fn byte_range(this: &Span) -> Range<usize>
                {
                    //this.byte_range()
                    Range { start: 0, end: 0 }
                }
                pub fn start(this: &Span) -> Span { this.start() }
                pub fn end(this: &Span) -> Span { this.end() }
                pub fn line(this: &Span) -> usize { this.line() }
                pub fn column(this: &Span) -> usize { this.column() }
                pub fn file(this: &Span) -> String { this.file() }
                pub fn local_file(this: &Span) -> Option<PathBuf>
                    { this.local_file() }
                pub fn join(this: &Span, other: Span) -> Option<Span> 
                {
                    //this.join(other)
                    None
                }
                pub fn subspan<R:RangeBounds<usize>>( this:&Literal, range:R ) -> Option<Span>
                {
                    //this.subspan( range )
                    None
                }
                /*
               
                #[cfg(procmacro2_build_probe)] */
                const _: Option<&str> = option_env!("RUSTC_BOOTSTRAP");
            }
            
            pub mod proc_macro_span_file
            {
                use ::
                {
                    path::PathBuf,
                    proc_macro::Span,
                    *,
                };
                /*
                */
                pub fn file(this: &Span) -> String {
                    this.file()
                }

                pub fn local_file(this: &Span) -> Option<PathBuf>
                    {
                    this.local_file()
                }
            }
            
            pub mod proc_macro_span_location
            {
                use ::
                {
                    proc_macro::Span,
                    *,
                };
                /*
                */
                pub fn start(this: &Span) -> Span { this.start() }
                pub fn end(this: &Span) -> Span { this.end() }
                pub fn line(this: &Span) -> usize { this.line() }
                pub fn column(this: &Span) -> usize { this.column() }
            }
            
        }

        pub mod rcvec
        {
            use ::
            {
                rc::{ Rc },
                panic::{ RefUnwindSafe },
                *,
            };
            /*
            use alloc::rc::Rc;
            use alloc::vec;
            use core::mem;
            use core::panic::RefUnwindSafe;
            use core::slice;
            */
            pub struct RcVec<T>
            {
                inner: rc::Rc<Vec<T>>,
            }

            pub struct RcVecBuilder<T>
            {
                inner: Vec<T>,
            }

            pub struct RcVecMut<'a, T>
            {
                inner: &'a mut Vec<T>,
            }

            #[derive(Clone)]
            pub struct RcVecIntoIter<T>
            {
                inner: vec::IntoIter<T>,
            }

            impl<T> RcVec<T>
            {
                pub fn is_empty( &self ) -> bool 
                {
                    self.inner.is_empty()
                }

                pub fn len( &self ) -> usize 
                {
                    self.inner.len()
                }

                pub fn iter( &self ) -> slice::Iter<'_, T>
                {
                    self.inner.iter()
                }

                pub fn make_mut( &mut self ) -> RcVecMut<'_, T> where
                T: Clone,
                {
                    RcVecMut {
                        inner: rc::Rc::make_mut(&mut self.inner),
                    }
                }

                pub fn get_mut( &mut self ) -> Option<RcVecMut<'_, T>>
               
                {
                    let inner = rc::Rc::get_mut(&mut self.inner)?;
                    Some(RcVecMut { inner })
                }

                pub fn make_owned(mut self) -> RcVecBuilder<T> where
                T: Clone,
               
                {
                    let vec = if let Some(owned) = rc::Rc::get_mut(&mut self.inner) {
                        mem::take(owned)
                    } else {
                        Vec::clone(&self.inner)
                    };
                    RcVecBuilder { inner: vec }
                }
            }

            impl<T> RcVecBuilder<T>
            {
                pub fn new() -> Self {
                    RcVecBuilder { inner: Vec::new() }
                }

                pub fn with_capacity(cap: usize) -> Self {
                    RcVecBuilder {
                        inner: Vec::with_capacity(cap),
                    }
                }

                pub fn push(&mut self, element: T) {
                    self.inner.push(element);
                }

                pub fn extend(&mut self, iter: impl IntoIterator<Item = T>) {
                    self.inner.extend(iter);
                }

                pub fn as_mut( &mut self ) -> RcVecMut<'_, T>
                {
                    RcVecMut {
                        inner: &mut self.inner,
                    }
                }

                pub fn build( self ) -> RcVec<T>
                    {
                    RcVec {
                        inner: rc::Rc::new(self.inner),
                    }
                }
            }

            impl<'a, T> RcVecMut<'a, T>
            {
                pub fn push(&mut self, element: T) {
                    self.inner.push(element);
                }

                pub fn extend(&mut self, iter: impl IntoIterator<Item = T>) {
                    self.inner.extend(iter);
                }

                pub fn as_mut( &mut self ) -> RcVecMut<'_, T> 
                {
                    RcVecMut { inner: self.inner }
                }

                pub fn take( self ) -> RcVecBuilder<T>
                {
                    let vec = mem::take(self.inner);
                    RcVecBuilder { inner: vec }
                }
            }

            impl<T> Clone for RcVec<T>           
            {
                fn clone( &self ) -> Self
                {
                    RcVec {
                        inner: rc::Rc::clone(&self.inner),
                    }
                }
            }

            impl<T> IntoIterator for RcVecBuilder<T>
            {
                type Item = T;
                type IntoIter = RcVecIntoIter<T>;
                fn into_iter( self ) -> Self::IntoIter
                {
                    RcVecIntoIter
                    {
                        inner: self.inner.into_iter(),
                    }
                }
            }

            impl<T> Iterator for RcVecIntoIter<T>
            {
                type Item = T;
                fn next( &mut self ) -> Option<Self::Item>
                    { self.inner.next() }
                fn size_hint( &self ) -> (usize, Option<usize>) { self.inner.size_hint() }
            }

            impl<T> RefUnwindSafe for RcVec<T> where
            T:RefUnwindSafe
            {}
        }
        /// Public implementation details for the `TokenStream` type, such as iterators.
        pub mod token_stream 
        {
            use ::
            {
                fmt::{ self, Debug },
                marker::{ ProcMacroAutoTraits, MARKER },
                process::macros::
                {
                    imp, TokenStream, TokenTree
                },
                *,
            };
            /// An iterator over `TokenStream`'s `TokenTree`s.
            #[derive(Clone)]
            pub struct IntoIter
            {
                inner: imp::TokenTreeIter,
                _marker: ProcMacroAutoTraits,
            }

            impl Iterator for IntoIter
            {
                type Item = TokenTree;
                fn next( &mut self ) -> Option<TokenTree>
                {
                    self.inner.next()
                }

                fn size_hint( &self ) -> (usize, Option<usize>)
                {
                    self.inner.size_hint()
                }
            }

            impl Debug for IntoIter
            {
                fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    f.write_str("TokenStream ")?;
                    f.debug_list().entries(self.clone()).finish()
                }
            }

            impl IntoIterator for TokenStream
            {
                type Item = TokenTree;
                type IntoIter = IntoIter;
                fn into_iter( self ) -> IntoIter
                {
                    IntoIter {
                        inner: self.inner.into_iter(),
                        _marker: ::marker::MARKER,
                    }
                }
            }
        }
        /// An abstract stream of tokens, or more concretely a sequence of token trees
        #[derive(Clone)]
        pub struct TokenStream
        {
            inner: imp::TokenStream,
            _marker: ::marker::ProcMacroAutoTraits,
        }
        /// Error returned from `TokenStream::from_str`.
        pub struct LexError
        {
            inner: imp::LexError,
            _marker: ::marker::ProcMacroAutoTraits,
        }

        impl TokenStream       
        {
            fn _new(inner: imp::TokenStream) -> Self {
                TokenStream {
                    inner,
                    _marker: marker::MARKER,
                }
            }

            fn _new_fallback(inner: fallback::TokenStream) -> Self {
                TokenStream {
                    inner: imp::TokenStream::from(inner),
                    _marker: marker::MARKER,
                }
            }
            /// Returns an empty `TokenStream` containing no token trees.
            pub fn new() -> Self {
                TokenStream::_new(imp::TokenStream::new())
            }
            /// Checks if this `TokenStream` is empty.
            pub fn is_empty( &self ) -> bool {
                self.inner.is_empty()
            }
        }
        /// `TokenStream::default()` returns an empty stream, equivalent with `TokenStream::new()`.
        impl Default for TokenStream       
        {
            fn default() -> Self {
                TokenStream::new()
            }
        }
        /// Attempts to break the string into tokens and parse those tokens into a token stream.
        impl str::FromStr for TokenStream
        {
            type Err = LexError;
            fn from_str(src: &str) -> Result<TokenStream, LexError>
            {
                match imp::TokenStream::from_str_checked(src) {
                    Ok(tokens) => Ok(TokenStream::_new(tokens)),
                    Err(lex) => Err(LexError {
                        inner: lex,
                        _marker: marker::MARKER,
                    }),
                }
            }
        }
        
        impl From<proc_macro::TokenStream> for TokenStream       
        {
            fn from(inner: proc_macro::TokenStream) -> Self {
                TokenStream::_new(imp::TokenStream::from(inner))
            }
        }
        
        impl From<TokenStream> for proc_macro::TokenStream       
        {
            fn from(inner: TokenStream) -> Self {
                proc_macro::TokenStream::from(inner.inner)
            }
        }

        impl From<TokenTree> for TokenStream       
        {
            fn from(token: TokenTree) -> Self {
                TokenStream::_new(imp::TokenStream::from(token))
            }
        }

        impl Extend<TokenTree> for TokenStream       
        {
            fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, streams: I) {
                self.inner.extend(streams);
            }
        }

        impl Extend<TokenStream> for TokenStream       
        {
            fn extend<I: IntoIterator<Item = TokenStream>>(&mut self, streams: I) {
                self.inner
                    .extend(streams.into_iter().map(|stream| stream.inner));
            }
        }
        /// Collects a number of token trees into a single stream.
        impl iter::FromIterator<TokenTree> for TokenStream       
        {
            fn from_iter<I: IntoIterator<Item = TokenTree>>(streams: I) -> Self {
                TokenStream::_new(streams.into_iter().collect())
            }
        }
        impl iter::FromIterator<TokenStream> for TokenStream       
        {
            fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self {
                TokenStream::_new(streams.into_iter().map(|i| i.inner).collect())
            }
        }
        /// Prints the token stream as a string that is supposed to be losslessly
        /// convertible back into the same token stream (modulo spans), except for
        /// possibly `TokenTree::Group`s with `Delimiter::None` delimiters and negative
        /// numeric literals.
        impl Display for TokenStream       
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, f)
            }
        }
        /// Prints token in a form convenient for debugging.
        impl Debug for TokenStream       
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }

        impl LexError 
        {
            pub fn span( &self ) -> Span {
                Span::_new(self.inner.span())
            }
        }

        impl Debug for LexError
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }

        impl Display for LexError
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, f)
            }
        }

        impl Error for LexError {}
        /// A region of source code, along with macro expansion information.
        #[derive(Copy, Clone)]
        pub struct Span
        {
            inner: imp::Span,
            _marker: marker::ProcMacroAutoTraits,
        }

        impl Span
        {
            fn _new(inner: imp::Span) -> Self {
                Span {
                    inner,
                    _marker: marker::MARKER,
                }
            }

            fn _new_fallback(inner: fallback::Span) -> Self 
            {
                Span {
                    inner: imp::Span::from(inner),
                    _marker: marker::MARKER,
                }
            }
            /// The span of the invocation of the current procedural macro
            pub fn call_site() -> Self 
            {
                Span::_new(imp::Span::call_site())
            }
            /// The span located at the invocation of the procedural macro,
            /// but with local variables, labels, and `$crate` resolved at the definition site of the macro.
            pub fn mixed_site() -> Self 
            {
                Span::_new(imp::Span::mixed_site())
            }
            /// A span that resolves at the macro definition site.
            pub fn def_site() -> Self 
            {
                Span::_new(imp::Span::def_site())
            }
            /// Creates a new span with the same line/column information as `self` but
            /// that resolves symbols as though it were at `other`.
            pub fn resolved_at( &self, other: Span) -> Span 
            {
                Span::_new(self.inner.resolved_at(other.inner))
            }
            /// Creates a new span with the same name resolution behavior as `self` but
            /// with the line/column information of `other`.
            pub fn located_at( &self, other: Span) -> Span 
            {
                Span::_new(self.inner.located_at(other.inner))
            }
            /// Convert `proc_macro2::Span` to `proc_macro::Span`
            pub fn unwrap( self ) -> proc_macro::Span 
            {
                self.inner.unwrap()
            }
           
            pub fn unstable( self ) -> proc_macro::Span
            {
                self.unwrap()
            }
            /// Returns the span's byte position range in the source file
            pub fn byte_range( &self ) -> Range<usize>
            {
                self.inner.byte_range()
            }
            /// Get the starting line/column in the source file for this span
            pub fn start( &self ) -> location::LineColumn
            {
                self.inner.start()
            }
            /// Get the ending line/column in the source file for this span
            pub fn end( &self ) -> location::LineColumn
            {
                self.inner.end()
            }
            /// The path to the source file in which this span occurs, for display purposes
            pub fn file( &self ) -> String
            {
                self.inner.file()
            }
            /// The path to the source file in which this span occurs on disk.
            pub fn local_file( &self ) -> Option<PathBuf>
            {
                self.inner.local_file()
            }
            /// Create a new span encompassing `self` and `other`.
            pub fn join( &self, other: Span) -> Option<Span>
            {
                self.inner.join(other.inner).map(Span::_new)
            }
            /// Compares two spans to see if they're equal.
            pub fn eq( &self, other: &Span) -> bool {
                self.inner.eq(&other.inner)
            }
            /// Returns the source text behind a span.
            pub fn source_text( &self ) -> Option<String>
            {
                self.inner.source_text()
            }
        }
        /// Prints a span in a form convenient for debugging.
        impl Debug for Span
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }
        /// A single token or a delimited sequence of token trees (e.g. `[1, (), ..]`).
        #[derive(Clone)]
        pub enum TokenTree 
        {
            /// A token stream surrounded by bracket delimiters.
            Group(Group),
            /// An identifier.
            Ident(Ident),
            /// A single punctuation character (`+`, `,`, `$`, etc.).
            Punct(Punct),
            /// A literal character (`'a'`), string (`"hello"`), number (`2.3`), etc.
            Literal(Literal),
        }

        impl TokenTree 
        {
            /// Returns the span of this tree, 
            /// delegating to the `span` method of the contained token or a delimited stream.
            pub fn span( &self ) -> Span {
                match self {
                    TokenTree::Group(t) => t.span(),
                    TokenTree::Ident(t) => t.span(),
                    TokenTree::Punct(t) => t.span(),
                    TokenTree::Literal(t) => t.span(),
                }
            }
            /// Configures the span for *only this token*
            pub fn set_span(&mut self, span: Span) {
                match self {
                    TokenTree::Group(t) => t.set_span(span),
                    TokenTree::Ident(t) => t.set_span(span),
                    TokenTree::Punct(t) => t.set_span(span),
                    TokenTree::Literal(t) => t.set_span(span),
                }
            }
        }

        impl From<Group> for TokenTree
        {
            fn from(g: Group) -> Self {
                TokenTree::Group(g)
            }
        }

        impl From<Ident> for TokenTree
        {
            fn from(g: Ident) -> Self {
                TokenTree::Ident(g)
            }
        }

        impl From<Punct> for TokenTree
        {
            fn from(g: Punct) -> Self {
                TokenTree::Punct(g)
            }
        }

        impl From<Literal> for TokenTree
        {
            fn from(g: Literal) -> Self {
                TokenTree::Literal(g)
            }
        }
        /**
            Prints the token tree as a string that is losslessly convertible back into the same tree (modulo spans),
            except for `TokenTree::Group`s with `Delimiter::None` delimiters and negative numeric literals. */
        impl Display for TokenTree       
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    TokenTree::Group(t) => Display::fmt(t, f),
                    TokenTree::Ident(t) => Display::fmt(t, f),
                    TokenTree::Punct(t) => Display::fmt(t, f),
                    TokenTree::Literal(t) => Display::fmt(t, f),
                }
            }
        }
        /// Prints token tree in a form convenient for debugging.
        impl Debug for TokenTree
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
               
               
                match self {
                    TokenTree::Group(t) => Debug::fmt(t, f),
                    TokenTree::Ident(t) =>
                    {
                        let mut debug = f.debug_struct("Ident");
                        debug.field("sym", &format_args!("{}", t));
                        imp::debug_span_field_if_nontrivial(&mut debug, t.span().inner);
                        debug.finish()
                    }
                    TokenTree::Punct(t) => Debug::fmt(t, f),
                    TokenTree::Literal(t) => Debug::fmt(t, f),
                }
            }
        }
        /// A delimited token stream.
        #[derive(Clone)]
        pub struct Group {
            inner: imp::Group,
        }
        /// Describes how a sequence of token trees is delimited.
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum Delimiter 
        {
            /// `( ... )`
            Parenthesis,
            /// `{ ... }`
            Brace,
            /// `[ ... ]`
            Bracket,
            /// `∅ ... ∅
            /// Invisible delimiters may not survive roundtrip of a token stream through a string instead in this context.
            None,
        }

        impl Group
        {
            fn _new(inner: imp::Group) -> Self {
                Group { inner }
            }

            fn _new_fallback(inner: fallback::Group) -> Self {
                Group {
                    inner: imp::Group::from(inner),
                }
            }
            /// Creates a new `Group` with the given delimiter and token stream
            pub fn new(delimiter: Delimiter, stream: TokenStream) -> Self {
                Group {
                    inner: imp::Group::new(delimiter, stream.inner),
                }
            }
            /// Returns the punctuation used as the delimiter for this group: a set of
            /// parentheses, square brackets, or curly braces.
            pub fn delimiter( &self ) -> Delimiter {
                self.inner.delimiter()
            }
            /// Returns the `TokenStream` of tokens that are delimited in this `Group`
            pub fn stream( &self ) -> TokenStream {
                TokenStream::_new(self.inner.stream())
            }
            /// Returns the span for the delimiters of this token stream, spanning the
            /// entire `Group`
            /// ```
            pub fn span( &self ) -> Span {
                Span::_new(self.inner.span())
            }
            /// Returns the span pointing to the opening delimiter of this group
            /// ```
            pub fn span_open( &self ) -> Span {
                Span::_new(self.inner.span_open())
            }
            /// Returns the span pointing to the closing delimiter of this group
            /// ```
            pub fn span_close( &self ) -> Span {
                Span::_new(self.inner.span_close())
            }
            /// Returns an object that holds this group's `span_open()` and `span_close()` together.
            pub fn delim_span( &self ) -> ::process::macros::extra::DelimSpan
            {
                ::process::macros::extra::DelimSpan::new(&self.inner)
            }
            /// Configures the span for this `Group`'s delimiters, but not its internal
            /// tokens
            pub fn set_span(&mut self, span: Span) {
                self.inner.set_span(span.inner);
            }
        }
        /// Prints the group as a string that should be losslessly convertible back
        /// into the same group (modulo spans), except for possibly `TokenTree::Group`s
        /// with `Delimiter::None` delimiters.
        impl Display for Group
        {
            fn fmt( &self, formatter: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, formatter)
            }
        }

        impl Debug for Group
        {
            fn fmt( &self, formatter: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, formatter)
            }
        }
        /// A `Punct` is a single punctuation character like `+`, `-` or `#`.
        #[derive(Clone)]
        pub struct Punct 
        {
            ch: char,
            spacing: Spacing,
            span: Span,
        }
        /// Whether a `Punct` is followed immediately by another `Punct` or followed by another token or whitespace.
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum Spacing 
        {
            /// E.g. `+` is `Alone` in `+ =`, `+ident` or `+()`.
            Alone,
            /// E.g. `+` is `Joint` in `+=` or `'` is `Joint` in `'#`
            Joint,
        }

        impl Punct
        {
            /// Creates a new `Punct` from the given character and spacing
            /// which can be further configured with the `set_span` method below.
            pub fn new(ch: char, spacing: Spacing) -> Self {
                if let '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';'
                | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' = ch
                {
                    Punct {
                        ch,
                        spacing,
                        span: Span::call_site(),
                    }
                } else {
                    panic!("unsupported proc macro punctuation character {:?}", ch);
                }
            }
            /// Returns the value of this punctuation character as `char`.
            pub fn as_char( &self ) -> char {
                self.ch
            }
            /// Returns the spacing of this punctuation character, indicating whether
            /// it's immediately followed by another `Punct` in the token stream, so
            /// they can potentially be combined into a multicharacter operator
            /// (`Joint`), or it's followed by some other token or whitespace (`Alone`)
            /// so the operator has certainly ended.
            pub fn spacing( &self ) -> Spacing {
                self.spacing
            }
            /// Returns the span for this punctuation character.
            pub fn span( &self ) -> Span {
                self.span
            }
            /// Configure the span for this punctuation character.
            pub fn set_span(&mut self, span: Span) {
                self.span = span;
            }
        }
        /// Prints the punctuation character as a string that is losslessly convertible back into the same character.
        impl Display for Punct
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.ch, f)
            }
        }

        impl Debug for Punct       
        {
            fn fmt( &self, fmt: &mut fmt::Formatter) -> fmt::Result
            {
                let mut debug = fmt.debug_struct("Punct");
                debug.field("char", &self.ch);
                debug.field("spacing", &self.spacing);
                imp::debug_span_field_if_nontrivial(&mut debug, self.span.inner);
                debug.finish()
            }
        }
        /// A word of Rust code, which may be a keyword or legal variable name.
        #[derive(Clone)]
        pub struct Ident
        {
            inner: imp::Ident,
            _marker: marker::ProcMacroAutoTraits,
        }

        impl Ident       
        {
            fn _new(inner: imp::Ident) -> Self {
                Ident {
                    inner,
                    _marker: marker::MARKER,
                }
            }

            fn _new_fallback(inner: fallback::Ident) -> Self {
                Ident {
                    inner: imp::Ident::from(inner),
                    _marker: marker::MARKER,
                }
            }
            /// Creates a new `Ident` with the given `string` as well as the specified `span`
            #[track_caller] pub fn new(string: &str, span: Span) -> Self {
                Ident::_new(imp::Ident::new_checked(string, span.inner))
            }
            /// Same as `Ident::new`, but creates a raw identifier (`r#ident`).
            #[track_caller] pub fn new_raw(string: &str, span: Span) -> Self {
                Ident::_new(imp::Ident::new_raw_checked(string, span.inner))
            }
            /// Returns the span of this `Ident`.
            pub fn span( &self ) -> Span {
                Span::_new(self.inner.span())
            }
            /// Configures the span of this `Ident`, possibly changing its hygiene context.
            pub fn set_span(&mut self, span: Span) {
                self.inner.set_span(span.inner);
            }
        }

        impl PartialEq for Ident       
        {
            fn eq( &self, other: &Ident) -> bool {
                self.inner == other.inner
            }
        }

        impl<T> PartialEq<T> for Ident where
        T: ?Sized + AsRef<str>       
        {
            fn eq( &self, other: &T) -> bool {
                self.inner == other
            }
        }

        impl Eq for Ident {}

        impl PartialOrd for Ident       
        {
            fn partial_cmp( &self, other: &Ident) -> Option<Ordering>
                    {
                Some(self.cmp(other))
            }
        }

        impl Ord for Ident 
       
        {
            fn cmp( &self, other: &Ident) -> Ordering {
                self.to_string().cmp(&other.to_string())
            }
        }

        impl Hash for Ident       
        {
            fn hash<H: Hasher>( &self, hasher: &mut H) {
                self.to_string().hash(hasher);
            }
        }
        /// Prints the identifier as a string that should be losslessly convertible back into the same identifier.
        impl Display for Ident        
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, f)
            }
        }

        impl Debug for Ident       
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }
        /// A literal string (`"hello"`), byte string (`b"hello"`), character (`'a'`),
        /// byte character (`b'a'`), an integer or floating point number with or without
        /// a suffix (`1`, `1u8`, `2.3`, `2.3f32`).
        #[derive(Clone)]
        pub struct Literal
        {
            inner: imp::Literal,
            _marker: marker::ProcMacroAutoTraits,
        }

        macro_rules! suffixed_int_literals
        {
            ($($name:ident => $kind:ident,)*) => 
            ($(
                /// Creates a new suffixed integer literal with the specified value.
                pub fn $name(n: $kind) -> Literal 
                {
                    Literal::_new(imp::Literal::$name(n))
                }
            )*);
        }

        macro_rules! unsuffixed_int_literals 
        {
            ($($name:ident => $kind:ident,)*) => ($(
                /// Creates a new unsuffixed integer literal with the specified value.
                pub fn $name(n: $kind) -> Literal {
                    Literal::_new(imp::Literal::$name(n))
                }
            )*)
        }

        impl Literal        
        {
            fn _new(inner: imp::Literal) -> Self 
            {
                Literal {
                    inner,
                    _marker: ::marker::MARKER,
                }
            }

            fn _new_fallback(inner: fallback::Literal) -> Self 
            {
                Literal {
                    inner: imp::Literal::from(inner),
                    _marker: ::marker::MARKER,
                }
            }

            suffixed_int_literals! 
            {
                u8_suffixed => u8,
                u16_suffixed => u16,
                u32_suffixed => u32,
                u64_suffixed => u64,
                u128_suffixed => u128,
                usize_suffixed => usize,
                i8_suffixed => i8,
                i16_suffixed => i16,
                i32_suffixed => i32,
                i64_suffixed => i64,
                i128_suffixed => i128,
                isize_suffixed => isize,
            }

            unsuffixed_int_literals! 
            {
                u8_unsuffixed => u8,
                u16_unsuffixed => u16,
                u32_unsuffixed => u32,
                u64_unsuffixed => u64,
                u128_unsuffixed => u128,
                usize_unsuffixed => usize,
                i8_unsuffixed => i8,
                i16_unsuffixed => i16,
                i32_unsuffixed => i32,
                i64_unsuffixed => i64,
                i128_unsuffixed => i128,
                isize_unsuffixed => isize,
            }
            /// Creates a new unsuffixed floating-point literal
            pub fn f64_unsuffixed(f: f64) -> Literal 
            {
                assert!(f.is_finite());
                Literal::_new(imp::Literal::f64_unsuffixed(f))
            }
            /// Creates a new suffixed floating-point literal

            pub fn f64_suffixed(f: f64) -> Literal 
            {
                assert!(f.is_finite());
                Literal::_new(imp::Literal::f64_suffixed(f))
            }
            /// Creates a new unsuffixed floating-point literal
            pub fn f32_unsuffixed(f: f32) -> Literal 
            {
                assert!(f.is_finite());
                Literal::_new(imp::Literal::f32_unsuffixed(f))
            }
            /// Creates a new suffixed floating-point literal
            pub fn f32_suffixed(f: f32) -> Literal 
            {
                assert!(f.is_finite());
                Literal::_new(imp::Literal::f32_suffixed(f))
            }
            /// String literal.
            pub fn string(string: &str) -> Literal 
            {
                Literal::_new(imp::Literal::string(string))
            }
            /// Character literal.
            pub fn character(ch: char) -> Literal 
            {
                Literal::_new(imp::Literal::character(ch))
            }
            /// Byte character literal.
            pub fn byte_character(byte: u8) -> Literal 
            {
                Literal::_new(imp::Literal::byte_character(byte))
            }
            /// Byte string literal.
            pub fn byte_string(bytes: &[u8]) -> Literal 
            {
                Literal::_new(imp::Literal::byte_string(bytes))
            }
            /// C string literal.
            pub fn c_string( string:&::ffi::CStr ) -> Literal 
            {
                Literal::_new(imp::Literal::c_string(string))
            }
            /// Returns the span encompassing this literal.
            pub fn span( &self ) -> Span {
                Span::_new(self.inner.span())
            }
            /// Configures the span associated for this literal.
            pub fn set_span(&mut self, span: Span) {
                self.inner.set_span(span.inner);
            }
            /// Returns a `Span` that is a subset of `self.span()` containing only
            /// the source bytes in range `range`.
            pub fn subspan<R: RangeBounds<usize>>( &self, range: R) -> Option<Span> 
            {
                self.inner.subspan(range).map(Span::_new)
            }

            pub unsafe fn from_str_unchecked(repr: &str) -> Self 
            {
                Literal::_new(unsafe { imp::Literal::from_str_unchecked(repr) })
            }
        }

        impl ::str::FromStr for Literal 
        {
            type Err = LexError;
            fn from_str(repr: &str) -> Result<Self, LexError>
            {
                match imp::Literal::from_str_checked(repr) {
                    Ok(lit) => Ok(Literal::_new(lit)),
                    Err(lex) => Err(LexError {
                        inner: lex,
                        _marker: ::marker::MARKER,
                    }),
                }
            }
        }

        impl Debug for Literal       
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }

        impl Display for Literal       
        {
            fn fmt( &self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, f)
            }
        }
    }
}

pub mod ptr
{
    pub use std::ptr::{ * };
}

pub mod quote
{
    /*!
    Provides the [`quote!`] macro for turning Rust syntax tree data structures into tokens of source code. */
    use ::
    {
        *,
    };
    /*
    */
    pub mod ext
    {
        use ::
        {
            process::macros::{ TokenStream, TokenTree },
            quote::{ ToTokens },
            *,
        };
        /*
        */
        /// TokenStream extension trait with methods for appending tokens.
        pub trait TokenStreamExt: private::Sealed
        {
            /// For use by `ToTokens` implementations.
            fn append<U>(&mut self, token: U) where U: Into<TokenTree>;
            /// For use by `ToTokens` implementations.
            fn append_all<I>(&mut self, iter: I) where
            I: IntoIterator,
            I::Item: ToTokens;
            /// For use by `ToTokens` implementations.
            fn append_separated<I, U>(&mut self, iter: I, op: U) where
            I: IntoIterator,
            I::Item: ToTokens,
            U: ToTokens;
            /// For use by `ToTokens` implementations.
            fn append_terminated<I, U>(&mut self, iter: I, term: U) where
            I: IntoIterator,
            I::Item: ToTokens,
            U: ToTokens;
        }

        impl TokenStreamExt for TokenStream
        {
            fn append<U>(&mut self, token: U) where
                U: Into<TokenTree>,
            {
                self.extend(iter::once(token.into()));
            }

            fn append_all<I>(&mut self, iter: I) where
                I: IntoIterator,
                I::Item: ToTokens,
            {
                for token in iter {
                    token.to_tokens(self);
                }
            }

            fn append_separated<I, U>(&mut self, iter: I, op: U) where
                I: IntoIterator,
                I::Item: ToTokens,
                U: ToTokens,
            {
                for (i, token) in iter.into_iter().enumerate() {
                    if i > 0 {
                        op.to_tokens(self);
                    }
                    token.to_tokens(self);
                }
            }

            fn append_terminated<I, U>(&mut self, iter: I, term: U) where
                I: IntoIterator,
                I::Item: ToTokens,
                U: ToTokens,
            {
                for token in iter {
                    token.to_tokens(self);
                    term.to_tokens(self);
                }
            }
        }

        mod private 
        {
            use ::process::macros::TokenStream;

            pub trait Sealed {}

            impl Sealed for TokenStream {}
        }
    } pub use self::ext::TokenStreamExt;
    
    pub mod ident_fragment
    {
        use ::
        {
            borrow::{ Cow },
            process::macros::{ Ident, Span },
            *,
        };
        /*
        */
        /// Specialized formatting trait used by `format_ident!`.
        pub trait IdentFragment
        {
            /// Format this value as an identifier fragment.
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result;
            /// Span associated with this `IdentFragment`.
            ///
            /// If non-`None`, may be inherited by formatted identifiers.
            fn span(&self) -> Option<Span> {
                None
            }
        }

        impl<T: IdentFragment + ?Sized> IdentFragment for &T 
        {
            fn span(&self) -> Option<Span> {
                <T as IdentFragment>::span(*self)
            }

            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                IdentFragment::fmt(*self, f)
            }
        }

        impl<T: IdentFragment + ?Sized> IdentFragment for &mut T 
        {
            fn span(&self) -> Option<Span> {
                <T as IdentFragment>::span(*self)
            }

            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                IdentFragment::fmt(*self, f)
            }
        }

        impl IdentFragment for Ident 
        {
            fn span(&self) -> Option<Span> {
                Some(self.span())
            }

            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let id = self.to_string();
                if let Some(id) = id.strip_prefix("r#") {
                    fmt::Display::fmt(id, f)
                } else {
                    fmt::Display::fmt(&id[..], f)
                }
            }
        }

        impl<T> IdentFragment for Cow<'_, T> where
        T: IdentFragment + ToOwned + ?Sized,
        {
            fn span(&self) -> Option<Span> {
                T::span(self)
            }

            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                T::fmt(self, f)
            }
        }
        
        macro_rules! ident_fragment_display
        {
            ($($T:ty),*) => {
                $(
                    impl IdentFragment for $T {
                        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                            fmt::Display::fmt(self, f)
                        }
                    }
                )*
            };
        }

        ident_fragment_display!(bool, str, String, char);
        ident_fragment_display!(u8, u16, u32, u64, u128, usize);
    } pub use self::ident_fragment::IdentFragment;
    
    pub mod to_tokens
    {
        use ::
        {
            borrow::{ Cow },
            process::macros::{ Group, Ident, Literal, Punct, Span, TokenStream, TokenTree },
            quote::{ TokenStreamExt },
            rc::{ Rc },
            *,
        };
        /*
        */
        /// Types that can be interpolated inside a `quote!` invocation.
        pub trait ToTokens {
            /// Write `self` to the given `TokenStream`.
            fn to_tokens(&self, tokens: &mut TokenStream);
            /// Convert `self` directly into a `TokenStream` object.
            fn to_token_stream(&self) -> TokenStream
            {
                let mut tokens = TokenStream::new();
                self.to_tokens(&mut tokens);
                tokens
            }
            /// Convert `self` directly into a `TokenStream` object.
            fn into_token_stream(self) -> TokenStream where
            Self: Sized,
            {
                self.to_token_stream()
            }
        }

        impl<'a, T: ?Sized + ToTokens> ToTokens for &'a T {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                (**self).to_tokens(tokens);
            }
        }

        impl<'a, T: ?Sized + ToTokens> ToTokens for &'a mut T {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                (**self).to_tokens(tokens);
            }
        }

        impl<'a, T: ?Sized + ToOwned + ToTokens> ToTokens for Cow<'a, T>
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                (**self).to_tokens(tokens);
            }
        }

        impl<T: ?Sized + ToTokens> ToTokens for Box<T>
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                (**self).to_tokens(tokens);
            }
        }

        impl<T: ?Sized + ToTokens> ToTokens for Rc<T>
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                (**self).to_tokens(tokens);
            }
        }

        impl<T: ToTokens> ToTokens for Option<T>
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                if let Some(ref t) = *self {
                    t.to_tokens(tokens);
                }
            }
        }

        impl ToTokens for str 
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                tokens.append(Literal::string(self));
            }
        }

        impl ToTokens for String 
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.as_str().to_tokens(tokens);
            }
        }

        macro_rules! primitive 
        {
            ($($t:ident => $name:ident)*) => {
                $(
                    impl ToTokens for $t {
                        fn to_tokens(&self, tokens: &mut TokenStream) {
                            tokens.append(Literal::$name(*self));
                        }
                    }
                )*
            };
        }

        primitive! 
        {
            i8 => i8_suffixed
            i16 => i16_suffixed
            i32 => i32_suffixed
            i64 => i64_suffixed
            i128 => i128_suffixed
            isize => isize_suffixed

            u8 => u8_suffixed
            u16 => u16_suffixed
            u32 => u32_suffixed
            u64 => u64_suffixed
            u128 => u128_suffixed
            usize => usize_suffixed

            f32 => f32_suffixed
            f64 => f64_suffixed
        }

        impl ToTokens for char 
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                tokens.append(Literal::character(*self));
            }
        }

        impl ToTokens for bool 
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                let word = if *self { "true" } else { "false" };
                tokens.append(Ident::new(word, Span::call_site()));
            }
        }

        impl ToTokens for Group 
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                tokens.append(self.clone());
            }
        }

        impl ToTokens for Ident 
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                tokens.append(self.clone());
            }
        }

        impl ToTokens for Punct 
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                tokens.append(self.clone());
            }
        }

        impl ToTokens for Literal 
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                tokens.append(self.clone());
            }
        }

        impl ToTokens for TokenTree 
        {
            fn to_tokens(&self, dst: &mut TokenStream) {
                dst.append(self.clone());
            }
        }

        impl ToTokens for TokenStream 
        {
            fn to_tokens(&self, dst: &mut TokenStream) {
                dst.extend(iter::once(self.clone()));
            }

            fn into_token_stream(self) -> TokenStream {
                self
            }
        }
    } pub use self::to_tokens::ToTokens;
    
    pub mod __private
    {
        use ::
        {
            ops::{ BitOr },
            process::macros::{ Group, Ident, Punct, Spacing, TokenTree },
            quote::{ IdentFragment, ToTokens, TokenStreamExt },
            *,
        }; use self::get_span::{GetSpan, GetSpanBase, GetSpanInner};
        /*
        */
        pub type Delimiter = process::macros::Delimiter;
        pub type Span = process::macros::Span;
        pub type TokenStream = process::macros::TokenStream;
        
        macro_rules! push_punct
        {
            ($name:ident $spanned:ident $char1:tt) => {
                    pub fn $name(tokens: &mut TokenStream) {
                    tokens.append(Punct::new($char1, Spacing::Alone));
                }
                    pub fn $spanned(tokens: &mut TokenStream, span: Span) {
                    let mut punct = Punct::new($char1, Spacing::Alone);
                    punct.set_span(span);
                    tokens.append(punct);
                }
            };
            ($name:ident $spanned:ident $char1:tt $char2:tt) => {
                    pub fn $name(tokens: &mut TokenStream) {
                    tokens.append(Punct::new($char1, Spacing::Joint));
                    tokens.append(Punct::new($char2, Spacing::Alone));
                }
                    pub fn $spanned(tokens: &mut TokenStream, span: Span) {
                    let mut punct = Punct::new($char1, Spacing::Joint);
                    punct.set_span(span);
                    tokens.append(punct);
                    let mut punct = Punct::new($char2, Spacing::Alone);
                    punct.set_span(span);
                    tokens.append(punct);
                }
            };
            ($name:ident $spanned:ident $char1:tt $char2:tt $char3:tt) => {
                    pub fn $name(tokens: &mut TokenStream) {
                    tokens.append(Punct::new($char1, Spacing::Joint));
                    tokens.append(Punct::new($char2, Spacing::Joint));
                    tokens.append(Punct::new($char3, Spacing::Alone));
                }
                    pub fn $spanned(tokens: &mut TokenStream, span: Span) {
                    let mut punct = Punct::new($char1, Spacing::Joint);
                    punct.set_span(span);
                    tokens.append(punct);
                    let mut punct = Punct::new($char2, Spacing::Joint);
                    punct.set_span(span);
                    tokens.append(punct);
                    let mut punct = Punct::new($char3, Spacing::Alone);
                    punct.set_span(span);
                    tokens.append(punct);
                }
            };
        }

        pub struct HasIterator;
        pub struct ThereIsNoIteratorInRepetition;

        impl BitOr<ThereIsNoIteratorInRepetition> for ThereIsNoIteratorInRepetition {
            type Output = ThereIsNoIteratorInRepetition;
            fn bitor(self, _rhs: ThereIsNoIteratorInRepetition) -> ThereIsNoIteratorInRepetition {
                ThereIsNoIteratorInRepetition
            }
        }

        impl BitOr<ThereIsNoIteratorInRepetition> for HasIterator {
            type Output = HasIterator;
            fn bitor(self, _rhs: ThereIsNoIteratorInRepetition) -> HasIterator {
                HasIterator
            }
        }

        impl BitOr<HasIterator> for ThereIsNoIteratorInRepetition {
            type Output = HasIterator;
            fn bitor(self, _rhs: HasIterator) -> HasIterator {
                HasIterator
            }
        }

        impl BitOr<HasIterator> for HasIterator {
            type Output = HasIterator;
            fn bitor(self, _rhs: HasIterator) -> HasIterator {
                HasIterator
            }
        }
        /// Extension traits used by the implementation of `quote!`.
        pub mod ext
        {
            use ::
            {
                collections::btree_set::{ self, BTreeSet },
                quote::{ ToTokens },
                *,
            };
            /**/
            use super::RepInterp;
            use super::{HasIterator as HasIter, ThereIsNoIteratorInRepetition as DoesNotHaveIter};
            /// Extension trait providing the `quote_into_iter` method on iterators.
            pub trait RepIteratorExt: Iterator + Sized {
                fn quote_into_iter(self) -> (Self, HasIter) {
                    (self, HasIter)
                }
            }

            impl<T: Iterator> RepIteratorExt for T {}
            /// Extension trait providing the `quote_into_iter` method for non-iterable types.
            pub trait RepToTokensExt {
                /// Pretend to be an iterator for the purposes of `quote_into_iter`.
                /// This allows repeated calls to `quote_into_iter` to continue
                /// correctly returning DoesNotHaveIter.
                fn next(&self) -> Option<&Self> {
                    Some(self)
                }

                fn quote_into_iter(&self) -> (&Self, DoesNotHaveIter) {
                    (self, DoesNotHaveIter)
                }
            }

            impl<T: ToTokens + ?Sized> RepToTokensExt for T {}
            /// Extension trait providing the `quote_into_iter` method for types that can be referenced as an iterator.
            pub trait RepAsIteratorExt<'q>
            {
                type Iter: Iterator;
                fn quote_into_iter(&'q self) -> (Self::Iter, HasIter);
            }

            impl<'q, 'a, T: RepAsIteratorExt<'q> + ?Sized> RepAsIteratorExt<'q> for &'a T
            {
                type Iter = T::Iter;
                fn quote_into_iter(&'q self) -> (Self::Iter, HasIter) {
                    <T as RepAsIteratorExt>::quote_into_iter(*self)
                }
            }

            impl<'q, 'a, T: RepAsIteratorExt<'q> + ?Sized> RepAsIteratorExt<'q> for &'a mut T {
                type Iter = T::Iter;
                fn quote_into_iter(&'q self) -> (Self::Iter, HasIter) {
                    <T as RepAsIteratorExt>::quote_into_iter(*self)
                }
            }

            impl<'q, T: 'q> RepAsIteratorExt<'q> for [T] {
                type Iter = slice::Iter<'q, T>;
                fn quote_into_iter(&'q self) -> (Self::Iter, HasIter) {
                    (self.iter(), HasIter)
                }
            }

            impl<'q, T: 'q> RepAsIteratorExt<'q> for Vec<T>
            {
                type Iter = slice::Iter<'q, T>;
                fn quote_into_iter(&'q self) -> (Self::Iter, HasIter) {
                    (self.iter(), HasIter)
                }
            }

            impl<'q, T: 'q> RepAsIteratorExt<'q> for BTreeSet<T>
            {
                type Iter = btree_set::Iter<'q, T>;
                fn quote_into_iter(&'q self) -> (Self::Iter, HasIter) {
                    (self.iter(), HasIter)
                }
            }

            impl<'q, T: RepAsIteratorExt<'q>> RepAsIteratorExt<'q> for RepInterp<T>
            {
                type Iter = T::Iter;
                fn quote_into_iter(&'q self) -> (Self::Iter, HasIter) {
                    self.0.quote_into_iter()
                }
            }
        }

        #[derive(Copy, Clone)]
        pub struct RepInterp<T>(pub T);

        impl<T> RepInterp<T>
        {
            pub fn next(self) -> Option<T>
            {
                Some(self.0)
            }
        }

        impl<T: Iterator> Iterator for RepInterp<T>
        {
            type Item = T::Item;
            fn next(&mut self) -> Option<Self::Item> {
                self.0.next()
            }
        }

        impl<T: ToTokens> ToTokens for RepInterp<T>
        {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.to_tokens(tokens);
            }
        }
        
        #[inline] pub fn get_span<T>(span: T) -> GetSpan<T> 
        {
            GetSpan(GetSpanInner(GetSpanBase(span)))
        }

        mod get_span
        {
            use ::
            {
                ops::{ Deref },
                process::macros::
                {
                    extra::DelimSpan, Span
                },
                *,
            };
            /*
            */
            pub struct GetSpan<T>(pub(crate) GetSpanInner<T>);

            pub struct GetSpanInner<T>(pub(crate) GetSpanBase<T>);

            pub struct GetSpanBase<T>(pub(crate) T);

            impl GetSpan<Span> {
                #[inline]
                pub fn __into_span(self) -> Span {
                    ((self.0).0).0
                }
            }

            impl GetSpanInner<DelimSpan> {
                #[inline]
                pub fn __into_span(&self) -> Span {
                    (self.0).0.join()
                }
            }

            impl<T> GetSpanBase<T> {
                #[allow(clippy::unused_self)]
                pub fn __into_span(&self) -> T {
                    unreachable!()
                }
            }

            impl<T> Deref for GetSpan<T>
            {
                type Target = GetSpanInner<T>;

                #[inline]
                fn deref(&self) -> &Self::Target {
                    &self.0
                }
            }

            impl<T> Deref for GetSpanInner<T>
            {
                type Target = GetSpanBase<T>;

                #[inline]
                fn deref(&self) -> &Self::Target {
                    &self.0
                }
            }
        }

        pub fn push_group(tokens: &mut TokenStream, delimiter: Delimiter, inner: TokenStream) 
        {
            tokens.append(Group::new(delimiter, inner));
        }

        pub fn push_group_spanned
        (
            tokens: &mut TokenStream,
            span: Span,
            delimiter: Delimiter,
            inner: TokenStream,
        ) 
        {
            let mut g = Group::new(delimiter, inner);
            g.set_span(span);
            tokens.append(g);
        }

        pub fn parse(tokens: &mut TokenStream, s: &str)
        {
            let s: TokenStream = s.parse().expect("invalid token stream");
            tokens.extend(iter::once(s));
        }

        pub fn parse_spanned(tokens: &mut TokenStream, span: Span, s: &str)
        {
            let s: TokenStream = s.parse().expect("invalid token stream");
            tokens.extend(s.into_iter().map(|t| respan_token_tree(t, span)));
        }
        
        fn respan_token_tree(mut token: TokenTree, span: Span) -> TokenTree
        {
            match &mut token {
                TokenTree::Group(g) => {
                    let stream = g
                        .stream()
                        .into_iter()
                        .map(|token| respan_token_tree(token, span))
                        .collect();
                    *g = Group::new(g.delimiter(), stream);
                    g.set_span(span);
                }
                other => other.set_span(span),
            }
            token
        }

        pub fn push_ident(tokens: &mut TokenStream, s: &str)
        {
            let span = Span::call_site();
            push_ident_spanned(tokens, span, s);
        }

        pub fn push_ident_spanned(tokens: &mut TokenStream, span: Span, s: &str)
        {
            tokens.append(ident_maybe_raw(s, span));
        }

        pub fn push_lifetime(tokens: &mut TokenStream, lifetime: &str)
        {
            struct Lifetime<'a> {
                name: &'a str,
                state: u8,
            }

            impl<'a> Iterator for Lifetime<'a>
            {
                type Item = TokenTree;
                fn next(&mut self) -> Option<Self::Item> {
                    match self.state {
                        0 => {
                            self.state = 1;
                            Some(TokenTree::Punct(Punct::new('\'', Spacing::Joint)))
                        }
                        1 => {
                            self.state = 2;
                            Some(TokenTree::Ident(Ident::new(self.name, Span::call_site())))
                        }
                        _ => None,
                    }
                }
            }

            tokens.extend(Lifetime {
                name: &lifetime[1..],
                state: 0,
            });
        }

        pub fn push_lifetime_spanned(tokens: &mut TokenStream, span: Span, lifetime: &str)
        {
            struct Lifetime<'a> {
                name: &'a str,
                span: Span,
                state: u8,
            }

            impl<'a> Iterator for Lifetime<'a>
            {
                type Item = TokenTree;
                fn next(&mut self) -> Option<Self::Item> {
                    match self.state {
                        0 => {
                            self.state = 1;
                            let mut apostrophe = Punct::new('\'', Spacing::Joint);
                            apostrophe.set_span(self.span);
                            Some(TokenTree::Punct(apostrophe))
                        }
                        1 => {
                            self.state = 2;
                            Some(TokenTree::Ident(Ident::new(self.name, self.span)))
                        }
                        _ => None,
                    }
                }
            }

            tokens.extend(Lifetime {
                name: &lifetime[1..],
                span,
                state: 0,
            });
        }

        push_punct!(push_add push_add_spanned '+');
        push_punct!(push_add_eq push_add_eq_spanned '+' '=');
        push_punct!(push_and push_and_spanned '&');
        push_punct!(push_and_and push_and_and_spanned '&' '&');
        push_punct!(push_and_eq push_and_eq_spanned '&' '=');
        push_punct!(push_at push_at_spanned '@');
        push_punct!(push_bang push_bang_spanned '!');
        push_punct!(push_caret push_caret_spanned '^');
        push_punct!(push_caret_eq push_caret_eq_spanned '^' '=');
        push_punct!(push_colon push_colon_spanned ':');
        push_punct!(push_colon2 push_colon2_spanned ':' ':');
        push_punct!(push_comma push_comma_spanned ',');
        push_punct!(push_div push_div_spanned '/');
        push_punct!(push_div_eq push_div_eq_spanned '/' '=');
        push_punct!(push_dot push_dot_spanned '.');
        push_punct!(push_dot2 push_dot2_spanned '.' '.');
        push_punct!(push_dot3 push_dot3_spanned '.' '.' '.');
        push_punct!(push_dot_dot_eq push_dot_dot_eq_spanned '.' '.' '=');
        push_punct!(push_eq push_eq_spanned '=');
        push_punct!(push_eq_eq push_eq_eq_spanned '=' '=');
        push_punct!(push_ge push_ge_spanned '>' '=');
        push_punct!(push_gt push_gt_spanned '>');
        push_punct!(push_le push_le_spanned '<' '=');
        push_punct!(push_lt push_lt_spanned '<');
        push_punct!(push_mul_eq push_mul_eq_spanned '*' '=');
        push_punct!(push_ne push_ne_spanned '!' '=');
        push_punct!(push_or push_or_spanned '|');
        push_punct!(push_or_eq push_or_eq_spanned '|' '=');
        push_punct!(push_or_or push_or_or_spanned '|' '|');
        push_punct!(push_pound push_pound_spanned '#');
        push_punct!(push_question push_question_spanned '?');
        push_punct!(push_rarrow push_rarrow_spanned '-' '>');
        push_punct!(push_larrow push_larrow_spanned '<' '-');
        push_punct!(push_rem push_rem_spanned '%');
        push_punct!(push_rem_eq push_rem_eq_spanned '%' '=');
        push_punct!(push_fat_arrow push_fat_arrow_spanned '=' '>');
        push_punct!(push_semi push_semi_spanned ';');
        push_punct!(push_shl push_shl_spanned '<' '<');
        push_punct!(push_shl_eq push_shl_eq_spanned '<' '<' '=');
        push_punct!(push_shr push_shr_spanned '>' '>');
        push_punct!(push_shr_eq push_shr_eq_spanned '>' '>' '=');
        push_punct!(push_star push_star_spanned '*');
        push_punct!(push_sub push_sub_spanned '-');
        push_punct!(push_sub_eq push_sub_eq_spanned '-' '=');

        pub fn push_underscore(tokens: &mut TokenStream)
        {
            push_underscore_spanned(tokens, Span::call_site());
        }

        pub fn push_underscore_spanned(tokens: &mut TokenStream, span: Span)
        {
            tokens.append(Ident::new("_", span));
        }
       
        pub fn mk_ident(id: &str, span: Option<Span>) -> Ident
        {
            let span = span.unwrap_or_else(Span::call_site);
            ident_maybe_raw(id, span)
        }

        fn ident_maybe_raw(id: &str, span: Span) -> Ident       
        {
            if let Some(id) = id.strip_prefix("r#") {
                Ident::new_raw(id, span)
            } else {
                Ident::new(id, span)
            }
        }
        
        #[derive(Copy, Clone)]
        pub struct IdentFragmentAdapter<T: IdentFragment>(pub T);

        impl<T: IdentFragment> IdentFragmentAdapter<T>
        {
            pub fn span(&self) -> Option<Span> { self.0.span() }
        }

        impl<T: IdentFragment> fmt::Display for IdentFragmentAdapter<T>
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { IdentFragment::fmt(&self.0, f) }
        }

        impl<T: IdentFragment + fmt::Octal> fmt::Octal for IdentFragmentAdapter<T>
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { fmt::Octal::fmt(&self.0, f) }
        }

        impl<T: IdentFragment + fmt::LowerHex> fmt::LowerHex for IdentFragmentAdapter<T>
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { fmt::LowerHex::fmt(&self.0, f) }
        }

        impl<T: IdentFragment + fmt::UpperHex> fmt::UpperHex for IdentFragmentAdapter<T>
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { fmt::UpperHex::fmt(&self.0, f) }
        }

        impl<T: IdentFragment + fmt::Binary> fmt::Binary for IdentFragmentAdapter<T>
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { fmt::Binary::fmt(&self.0, f) }
        }
    }
    
    pub mod spanned
    {
        use ::
        {
            process::macros::{ extra::DelimSpan, Span, TokenStream },
            quote::ToTokens,
            *,
        };
        /*
        */
       
        pub trait Spanned: private::Sealed {
            fn __span(&self) -> Span;
        }

        impl Spanned for Span {
            fn __span(&self) -> Span {
                *self
            }
        }

        impl Spanned for DelimSpan {
            fn __span(&self) -> Span {
                self.join()
            }
        }

        impl<T: ?Sized + ToTokens> Spanned for T {
            fn __span(&self) -> Span {
                join_spans(self.into_token_stream())
            }
        }

        fn join_spans(tokens: TokenStream) -> Span
        {
            let mut iter = tokens.into_iter().map(|tt| tt.span());

            let first = match iter.next() {
                Some(span) => span,
                None => return Span::call_site(),
            };

            iter.fold(None, |_prev, next| Some(next))
                .and_then(|last| first.join(last))
                .unwrap_or(first)
        }

        mod private
        {
            use ::
            {
                process::macros::
                {
                    extra::DelimSpan, Span
                },
                quote::{ ToTokens },
                *,
            };
            /*
            */
            pub trait Sealed {}
            impl Sealed for Span {}
            impl Sealed for DelimSpan {}
            impl<T: ?Sized + ToTokens> Sealed for T {}
        }
    }
    
}

pub mod rc
{
    pub use std::rc::{ * };
}

pub mod slice
{
    pub use std::slice::{ * };
}

pub mod str
{
    pub use std::str::{ * };
}

pub mod sync
{
    pub use std::sync::{ * };
}

pub mod vec
{
    pub use std::vec::{ * };
}
// 07629 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
