//! Implementation of Unicode Standard Annex #31 for 
//! determining which `char` values are valid in programming language identifiers.
/*
#![no_std]
#![doc(html_root_url = "https://docs.rs/unicode-ident/1.0.19")]
#![allow(clippy::doc_markdown, clippy::must_use_candidate)] */
#![feature
(
    
)]

#![allow
(
    unused_attributes,
    unused_imports,
)]

extern crate proc_macro;
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

pub mod cell
{
    pub use std::cell::{ * };
}

pub mod char
{
    pub use std::char::{ * };
}

pub mod collections
{
    pub use std::collections::{ * };
}

pub mod cmp
{
    pub use std::cmp::{ * };
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
        c == '_' || unicode_ident::is_xid_start(c)
    }
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
    pub struct ProcMacroAutoTraits(PhantomData<Rc<()>>);
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
            #[cfg(span_locations)]
            use core::ops::Range;
            use core::ops::RangeBounds;
            use core::str::FromStr;
            use std::error::Error;
            use std::ffi::CStr;
            #[cfg(span_locations)]
            use std::path::PathBuf;
            #[cfg(span_locations)]
            #[cfg_attr(docsrs, doc(cfg(feature = "span-locations")))]
            pub use ::process::macros::location::LineColumn;
        */
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
                            self, is_ident_continue, is_ident_start, Group, Ident, LexError, Literal, Span,
                            TokenStream, TokenStreamBuilder,
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
                #[cfg(span_locations)]
                pub off: u32,
            }

            impl<'a> Cursor<'a>
            {
                pub fn advance(&self, bytes: usize) -> Cursor<'a> {
                    let (_front, rest) = self.rest.split_at(bytes);
                    Cursor {
                        rest,
                        #[cfg(span_locations)]
                        off: self.off + _front.chars().count() as u32,
                    }
                }

                pub fn starts_with(&self, s: &str) -> bool {
                    self.rest.starts_with(s)
                }

                pub fn starts_with_char(&self, ch: char) -> bool {
                    self.rest.starts_with(ch)
                }

                pub fn starts_with_fn<Pattern>(&self, f: Pattern) -> bool
                where
                    Pattern: FnMut(char) -> bool,
                {
                    self.rest.starts_with(f)
                }

                pub fn is_empty(&self) -> bool {
                    self.rest.is_empty()
                }

                fn len(&self) -> usize {
                    self.rest.len()
                }

                fn as_bytes(&self) -> &'a [u8] {
                    self.rest.as_bytes()
                }

                fn bytes(&self) -> Bytes<'a> {
                    self.rest.bytes()
                }

                fn chars(&self) -> Chars<'a> {
                    self.rest.chars()
                }

                fn char_indices(&self) -> CharIndices<'a> {
                    self.rest.char_indices()
                }

                fn parse(&self, tag: &str) -> Result<Cursor<'a>, Reject> {
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

                while !s.is_empty() {
                    let byte = s.as_bytes()[0];
                    if byte == b'/' {
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
                                Ok((rest, _)) => {
                                    s = rest;
                                    continue;
                                }
                                Err(Reject) => return s,
                            }
                        }
                    }
                    match byte {
                        b' ' | 0x09..=0x0d => {
                            s = s.advance(1);
                            continue;
                        }
                        b if b.is_ascii() => {}
                        _ => {
                            let ch = s.chars().next().unwrap();
                            if is_whitespace(ch) {
                                s = s.advance(ch.len_utf8());
                                continue;
                            }
                        }
                    }
                    return s;
                }
                s
            }

            fn block_comment(input: Cursor) -> PResult<&str>
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
                        i += 1; // eat '*'
                    } else if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                        depth -= 1;
                        if depth == 0 {
                            return Ok((input.advance(i + 2), &input.rest[..i + 2]));
                        }
                        i += 1; // eat '/'
                    }
                    i += 1;
                }

                Err(Reject)
            }

            fn word_break(input: Cursor) -> Result<Cursor, Reject>
            {
                match input.chars().next() {
                    Some(ch) if is_ident_continue(ch) => Err(Reject),
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

                    #[cfg(span_locations)]
                    let lo = input.off;

                    let first = match input.bytes().next() {
                        Some(first) => first,
                        None => match stack.last() {
                            None => return Ok(trees.build()),
                            #[cfg(span_locations)]
                            Some((lo, _frame)) => {
                                return Err(LexError {
                                    span: Span { lo: *lo, hi: *lo },
                                })
                            }
                            #[cfg(not(span_locations))]
                            Some(_frame) => return Err(LexError { span: Span {} }),
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
                        #[cfg(span_locations)]
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
                        #[cfg(span_locations)]
                        let (lo, frame) = frame;
                        let (open_delimiter, outer) = frame;
                        if open_delimiter != close_delimiter {
                            return Err(lex_error(input));
                        }
                        input = input.advance(1);
                        let mut g = Group::new(open_delimiter, trees.build());
                        g.set_span(Span {
                            #[cfg(span_locations)]
                            lo,
                            #[cfg(span_locations)]
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
                            #[cfg(span_locations)]
                            lo,
                            #[cfg(span_locations)]
                            hi: rest.off,
                        }));
                        trees.push_token_from_parser(tt);
                        input = rest;
                    }
                }
            }

            fn lex_error(cursor: Cursor) -> LexError
            {
                #[cfg(not(span_locations))]
                let _ = cursor;
                LexError {
                    span: Span {
                        #[cfg(span_locations)]
                        lo: cursor.off,
                        #[cfg(span_locations)]
                        hi: cursor.off,
                    },
                }
            }

            fn leaf_token(input: Cursor) -> PResult<TokenTree>
            {
                if let Ok((input, l)) = literal(input) {
                    // must be parsed before ident
                    Ok((input, TokenTree::Literal(::process::macros::Literal::_new_fallback(l))))
                } else if let Ok((input, p)) = punct(input) {
                    Ok((input, TokenTree::Punct(p)))
                } else if let Ok((input, i)) = ident(input) {
                    Ok((input, TokenTree::Ident(i)))
                } else if input.starts_with(ERROR) {
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

                if !raw {
                    let ident =
                        ::process::macros::Ident::_new_fallback(Ident::new_unchecked(sym, fallback::Span::call_site()));
                    return Ok((rest, ident));
                }

                match sym {
                    "_" | "super" | "self" | "Self" | "crate" => return Err(Reject),
                    _ => {}
                }

                let ident =
                    ::process::macros::Ident::_new_fallback(Ident::new_raw_unchecked(sym, fallback::Span::call_site()));
                Ok((rest, ident))
            }

            fn ident_not_raw(input: Cursor) -> PResult<&str>
            {
                let mut chars = input.char_indices();

                match chars.next() {
                    Some((_, ch)) if is_ident_start(ch) => {}
                    _ => return Err(Reject),
                }

                let mut end = input.len();
                for (i, ch) in chars {
                    if !is_ident_continue(ch) {
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
                        '"' => {
                            let input = input.advance(i + 1);
                            return Ok(literal_suffix(input));
                        }
                        '\r' => match chars.next() {
                            Some((_, '\n')) => {}
                            _ => break,
                        },
                        '\\' => match chars.next() {
                            Some((_, 'x')) => {
                                backslash_x_char(&mut chars)?;
                            }
                            Some((_, 'n' | 'r' | 't' | '\\' | '\'' | '"' | '0')) => {}
                            Some((_, 'u')) => {
                                backslash_u(&mut chars)?;
                            }
                            Some((newline, ch @ ('\n' | '\r'))) => {
                                input = input.advance(newline + 1);
                                trailing_backslash(&mut input, ch as u8)?;
                                chars = input.char_indices();
                            }
                            _ => break,
                        },
                        _ch => {}
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
                        b'"' if input.rest[i + 1..].starts_with(delimiter) => {
                            let rest = input.advance(i + 1 + delimiter.len());
                            return Ok(literal_suffix(rest));
                        }
                        b'\r' => match bytes.next() {
                            Some((_, b'\n')) => {}
                            _ => break,
                        },
                        _ => {}
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
                        b'"' => {
                            let input = input.advance(offset + 1);
                            return Ok(literal_suffix(input));
                        }
                        b'\r' => match bytes.next() {
                            Some((_, b'\n')) => {}
                            _ => break,
                        },
                        b'\\' => match bytes.next() {
                            Some((_, b'x')) => {
                                backslash_x_byte(&mut bytes)?;
                            }
                            Some((_, b'n' | b'r' | b't' | b'\\' | b'0' | b'\'' | b'"')) => {}
                            Some((newline, b @ (b'\n' | b'\r'))) => {
                                input = input.advance(newline + 1);
                                trailing_backslash(&mut input, b)?;
                                bytes = input.bytes().enumerate();
                            }
                            _ => break,
                        },
                        b if b.is_ascii() => {}
                        _ => break,
                    }
                }
                Err(Reject)
            }

            fn delimiter_of_raw_string(input: Cursor) -> PResult<&str>
            {
                for (i, byte) in input.bytes().enumerate() {
                    match byte {
                        b'"' => {
                            if i > 255 {
                                // https://github.com/rust-lang/rust/pull/95251
                                return Err(Reject);
                            }
                            return Ok((input.advance(i + 1), &input.rest[..i]));
                        }
                        b'#' => {}
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
                        b'"' if input.rest[i + 1..].starts_with(delimiter) => {
                            let rest = input.advance(i + 1 + delimiter.len());
                            return Ok(literal_suffix(rest));
                        }
                        b'\r' => match bytes.next() {
                            Some((_, b'\n')) => {}
                            _ => break,
                        },
                        other => {
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
                        b'"' if input.rest[i + 1..].starts_with(delimiter) => {
                            let rest = input.advance(i + 1 + delimiter.len());
                            return Ok(literal_suffix(rest));
                        }
                        b'\r' => match bytes.next() {
                            Some((_, b'\n')) => {}
                            _ => break,
                        },
                        b'\0' => break,
                        _ => {}
                    }
                }
                Err(Reject)
            }

            fn cooked_c_string(mut input: Cursor) -> Result<Cursor, Reject>
            {
                let mut chars = input.char_indices();

                while let Some((i, ch)) = chars.next() {
                    match ch {
                        '"' => {
                            let input = input.advance(i + 1);
                            return Ok(literal_suffix(input));
                        }
                        '\r' => match chars.next() {
                            Some((_, '\n')) => {}
                            _ => break,
                        },
                        '\\' => match chars.next() {
                            Some((_, 'x')) => {
                                backslash_x_nonzero(&mut chars)?;
                            }
                            Some((_, 'n' | 'r' | 't' | '\\' | '\'' | '"')) => {}
                            Some((_, 'u')) => {
                                if backslash_u(&mut chars)? == '\0' {
                                    break;
                                }
                            }
                            Some((newline, ch @ ('\n' | '\r'))) => {
                                input = input.advance(newline + 1);
                                trailing_backslash(&mut input, ch as u8)?;
                                chars = input.char_indices();
                            }
                            _ => break,
                        },
                        '\0' => break,
                        _ch => {}
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
                //next_ch!(chars @ '0'..='7');
                match chars.next()
                {
                    Some((_, ch)) => match ch
                    {
                        '0'..='7' => ch,
                        _ => return Err(Reject),
                    },
                }

                match chars.next()
                {
                    Some((_, ch)) => match ch
                    {
                        '0'..='9' | 'a'..='f' | 'A'..='F' => ch,
                        _ => return Err(Reject),
                    },
                }

                Ok(())
            } 

            fn backslash_x_byte<I>(chars: &mut I) -> Result<(), Reject> where
            I: Iterator<Item = (usize, u8)>,
            {
                next_ch!(chars @ b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F');
                next_ch!(chars @ b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F');
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
                };*/
                
                //next_ch!(chars @ '0'..='9' | 'a'..='f' | 'A'..='F');


                let second = next_ch!(chars @ '0'..='9' | 'a'..='f' | 'A'..='F');
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
                for (_, ch) in chars {
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
                        Some((_, b @ (b' ' | b'\t' | b'\n' | b'\r'))) => {
                            last = b;
                        }
                        Some((offset, _)) => {
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
                    if is_ident_start(ch) {
                        rest = ident_not_raw(rest)?.0;
                    }
                }
                word_break(rest)
            }

            fn float_digits(input: Cursor) -> Result<Cursor, Reject>
            {
                let mut chars = input.chars().peekable();
                match chars.next() {
                    Some(ch) if '0' <= ch && ch <= '9' => {}
                    _ => return Err(Reject),
                }

                let mut len = 1;
                let mut has_dot = false;
                let mut has_exp = false;
                while let Some(&ch) = chars.peek() {
                    match ch {
                        '0'..='9' | '_' => {
                            chars.next();
                            len += 1;
                        }
                        '.' => {
                            if has_dot {
                                break;
                            }
                            chars.next();
                            if chars
                                .peek()
                                .map_or(false, |&ch| ch == '.' || is_ident_start(ch))
                            {
                                return Err(Reject);
                            }
                            len += 1;
                            has_dot = true;
                        }
                        'e' | 'E' => {
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

                if has_exp {
                    let token_before_exp = if has_dot {
                        Ok(input.advance(len - 1))
                    } else {
                        Err(Reject)
                    };
                    let mut has_sign = false;
                    let mut has_exp_value = false;
                    while let Some(&ch) = chars.peek() {
                        match ch {
                            '+' | '-' => {
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
                            '0'..='9' => {
                                chars.next();
                                len += 1;
                                has_exp_value = true;
                            }
                            '_' => {
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
                    if is_ident_start(ch) {
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
                        b'0'..=b'9' => {
                            let digit = (b - b'0') as u64;
                            if digit >= base {
                                return Err(Reject);
                            }
                        }
                        b'a'..=b'f' => {
                            let digit = 10 + (b - b'a') as u64;
                            if digit >= base {
                                break;
                            }
                        }
                        b'A'..=b'F' => {
                            let digit = 10 + (b - b'A') as u64;
                            if digit >= base {
                                break;
                            }
                        }
                        b'_' => {
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
                if ch == '\'' {
                    let (after_lifetime, _ident) = ident_any(rest)?;
                    if after_lifetime.starts_with_char('\'')
                        || (after_lifetime.starts_with_char('#') && !rest.starts_with("r#"))
                    {
                        Err(Reject)
                    } else {
                        Ok((rest, Punct::new('\'', Spacing::Joint)))
                    }
                } else {
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
                    // Do not accept `/` of a comment as a punct.
                    return Err(Reject);
                }

                let mut chars = input.chars();
                let first = match chars.next() {
                    Some(ch) => ch,
                    None => {
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
                    #[cfg(span_locations)]
                    lo,
                    #[cfg(span_locations)]
                    hi: rest.off,
                };
                let span = ::process::macros::Span::_new_fallback(fallback_span);

                let mut scan_for_bare_cr = comment;
                while let Some(cr) = scan_for_bare_cr.find('\r') {
                    let rest = &scan_for_bare_cr[cr + 1..];
                    if !rest.starts_with('\n') {
                        return Err(Reject);
                    }
                    scan_for_bare_cr = rest;
                }

                let mut pound = Punct::new('#', Spacing::Alone);
                pound.set_span(span);
                trees.push_token_from_parser(TokenTree::Punct(pound));

                if inner {
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

            fn doc_comment_contents(input: Cursor) -> PResult<(&str, bool)>
            {
                if input.starts_with("//!") {
                    let input = input.advance(3);
                    let (input, s) = take_until_newline_or_eof(input);
                    Ok((input, (s, true)))
                } else if input.starts_with("/*!") {
                    let (input, s) = block_comment(input)?;
                    Ok((input, (&s[3..s.len() - 2], true)))
                } else if input.starts_with("///") {
                    let input = input.advance(3);
                    if input.starts_with_char('/') {
                        return Err(Reject);
                    }
                    let (input, s) = take_until_newline_or_eof(input);
                    Ok((input, (s, false)))
                } else if input.starts_with("/**") && !input.rest[3..].starts_with('*') {
                    let (input, s) = block_comment(input)?;
                    Ok((input, (&s[3..s.len() - 2], false)))
                } else {
                    Err(Reject)
                }
            }

            fn take_until_newline_or_eof(input: Cursor) -> (Cursor, &str) 
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
                pub fn byte_range(this: &Span) -> Range<usize> { this.byte_range() }
                pub fn start(this: &Span) -> Span { this.start() }
                pub fn end(this: &Span) -> Span { this.end() }
                pub fn line(this: &Span) -> usize { this.line() }
                pub fn column(this: &Span) -> usize { this.column() }
                pub fn file(this: &Span) -> String { this.file() }
                pub fn local_file(this: &Span) -> Option<PathBuf> { this.local_file() }
                pub fn join(this: &Span, other: Span) -> Option<Span> { this.join(other) }
                pub fn subspan<R:RangeBounds<usize>>( this:&Literal, range:R ) -> Option<Span> { this.subspan( range ) }
                /*
                // Include in sccache cache key.
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

                pub fn local_file(this: &Span) -> Option<PathBuf> {
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
                inner: Rc<Vec<T>>,
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
                pub fn is_empty(&self) -> bool {
                    self.inner.is_empty()
                }

                pub fn len(&self) -> usize {
                    self.inner.len()
                }

                pub fn iter(&self) -> slice::Iter<T> {
                    self.inner.iter()
                }

                pub fn make_mut(&mut self) -> RcVecMut<T>
                where
                    T: Clone,
                {
                    RcVecMut {
                        inner: Rc::make_mut(&mut self.inner),
                    }
                }

                pub fn get_mut(&mut self) -> Option<RcVecMut<T>> {
                    let inner = Rc::get_mut(&mut self.inner)?;
                    Some(RcVecMut { inner })
                }

                pub fn make_owned(mut self) -> RcVecBuilder<T>
                where
                    T: Clone,
                {
                    let vec = if let Some(owned) = Rc::get_mut(&mut self.inner) {
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

                pub fn as_mut(&mut self) -> RcVecMut<T> {
                    RcVecMut {
                        inner: &mut self.inner,
                    }
                }

                pub fn build(self) -> RcVec<T> {
                    RcVec {
                        inner: Rc::new(self.inner),
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

                pub fn as_mut(&mut self) -> RcVecMut<T> {
                    RcVecMut { inner: self.inner }
                }

                pub fn take(self) -> RcVecBuilder<T> {
                    let vec = mem::take(self.inner);
                    RcVecBuilder { inner: vec }
                }
            }

            impl<T> Clone for RcVec<T> 
           
            {
                fn clone(&self) -> Self {
                    RcVec {
                        inner: Rc::clone(&self.inner),
                    }
                }
            }

            impl<T> IntoIterator for RcVecBuilder<T>
            {
                type Item = T;
                type IntoIter = RcVecIntoIter<T>;

                fn into_iter(self) -> Self::IntoIter {
                    RcVecIntoIter {
                        inner: self.inner.into_iter(),
                    }
                }
            }

            impl<T> Iterator for RcVecIntoIter<T>
            {
                type Item = T;

                fn next(&mut self) -> Option<Self::Item> {
                    self.inner.next()
                }

                fn size_hint(&self) -> (usize, Option<usize>) {
                    self.inner.size_hint()
                }
            }

            impl<T> RefUnwindSafe for RcVec<T> where T: RefUnwindSafe {}
        }
        //#[cfg(wrap_proc_macro)]
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
                    _ => {}
                }

                INIT.call_once(initialize);
                inside_proc_macro()
            }

            pub fn force_fallback() { WORKS.store(1, Ordering::Relaxed); }

            pub fn unforce_fallback() { initialize(); }
            
            fn initialize()
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
        
        pub mod fallback
        {
            use ::
            {
                cell::{ RefCell },
                collections::{ BTreeMap },
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
            #[cfg(wrap_proc_macro)]
            use ::process::macros::imp;
            #[cfg(span_locations)]
            use ::process::macros::location::LineColumn;
            use ::process::macros::parse::{self, Cursor};
            use ::process::macros::rcvec::{RcVec, RcVecBuilder, RcVecIntoIter, RcVecMut};
            use ::process::macros::{Delimiter, Spacing, TokenTree};
            #[cfg(all(span_locations, not(fuzzing)))]
            use alloc::collections::BTreeMap;
            #[cfg(all(span_locations, not(fuzzing)))]
            use core::cell::RefCell;
            #[cfg(span_locations)]
            use core::cmp;
            #[cfg(all(span_locations, not(fuzzing)))]
            use core::cmp::Ordering;
            use core::fmt::{self, Debug, Display, Write};
            use core::mem::ManuallyDrop;
            #[cfg(span_locations)]
            use core::ops::Range;
            use core::ops::RangeBounds;
            use core::ptr;
            use core::str;
            #[cfg(feature = "proc-macro")]
            use core::str::FromStr;
            use std::ffi::CStr;
            #[cfg(wrap_proc_macro)]
            use std::panic;
            #[cfg(span_locations)]
            use std::path::PathBuf;
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
                pub fn span(&self) -> Span { self.span }
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

                pub fn is_empty(&self) -> bool {
                    self.inner.len() == 0
                }

                fn take_inner(self) -> RcVecBuilder<TokenTree> {
                    let nodrop = ManuallyDrop::new(self);
                    unsafe { ptr::read(&nodrop.inner) }.make_owned()
                }
            }

            fn push_token_from_proc_macro(mut vec: RcVecMut<TokenTree>, token: TokenTree)
            {
                match token {
                    TokenTree::Literal(::process::macros::Literal {
                        #[cfg(wrap_proc_macro)]
                            inner: ::process::macros::imp::Literal::Fallback(literal),
                        #[cfg(not(wrap_proc_macro))]
                            inner: literal,
                        ..
                    }) if literal.repr.starts_with('-') => {
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
                fn drop(&mut self) {
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
                            #[cfg(wrap_proc_macro)]
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

                pub fn build(self) -> TokenStream {
                    TokenStream {
                        inner: self.inner.build(),
                    }
                }
            }
            
            fn get_cursor(src: &str) -> Cursor {
                #[cfg(fuzzing)]
                return Cursor { rest: src, off: 1 };

                // Create a dummy file & add it to the source map
                #[cfg(not(fuzzing))]
                SOURCE_MAP.with(|sm| {
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
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str("cannot parse string into token stream")
                }
            }

            impl Display for TokenStream
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    let mut joint = false;
                    for (i, tt) in self.inner.iter().enumerate() {
                        if i != 0 && !joint {
                            write!(f, " ")?;
                        }
                        joint = false;
                        match tt {
                            TokenTree::Group(tt) => Display::fmt(tt, f),
                            TokenTree::Ident(tt) => Display::fmt(tt, f),
                            TokenTree::Punct(tt) => {
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
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
                fn from(tree: TokenTree) -> Self {
                    let mut stream = RcVecBuilder::new();
                    push_token_from_proc_macro(stream.as_mut(), tree);
                    TokenStream {
                        inner: stream.build(),
                    }
                }
            }

            impl FromIterator<TokenTree> for TokenStream
            {
                fn from_iter<I: IntoIterator<Item = TokenTree>>(tokens: I) -> Self {
                    let mut stream = TokenStream::new();
                    stream.extend(tokens);
                    stream
                }
            }

            impl FromIterator<TokenStream> for TokenStream
            {
                fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self {
                    let mut v = RcVecBuilder::new();

                    for stream in streams {
                        v.extend(stream.take_inner());
                    }

                    TokenStream { inner: v.build() }
                }
            }

            impl Extend<TokenTree> for TokenStream
            {
                fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, tokens: I) {
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

                fn into_iter(self) -> TokenTreeIter {
                    self.take_inner().into_iter()
                }
            }

            #[cfg(all(span_locations, not(fuzzing)))]
            thread_local! {
                static SOURCE_MAP: RefCell<SourceMap> = RefCell::new(SourceMap {
                    // Start with a single dummy file which all call_site() and def_site()
                    // spans reference.
                    files: vec![FileInfo {
                        source_text: String::new(),
                        span: Span { lo: 0, hi: 0 },
                        lines: vec![0],
                        char_index_to_byte_offset: BTreeMap::new(),
                    }],
                });
            }

            #[cfg(span_locations)]
            pub fn invalidate_current_thread_spans() {
                #[cfg(not(fuzzing))]
                SOURCE_MAP.with(|sm| sm.borrow_mut().files.truncate(1));
            }

            #[cfg(all(span_locations, not(fuzzing)))]
            struct FileInfo {
                source_text: String,
                span: Span,
                lines: Vec<usize>,
                char_index_to_byte_offset: BTreeMap<usize, usize>,
            }

            #[cfg(all(span_locations, not(fuzzing)))]
            impl FileInfo
            {
                fn offset_line_column(&self, offset: usize) -> LineColumn {
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

                fn span_within(&self, span: Span) -> bool {
                    span.lo >= self.span.lo && span.hi <= self.span.hi
                }

                fn byte_range(&mut self, span: Span) -> Range<usize> {
                    let lo_char = (span.lo - self.span.lo) as usize;

                    // Look up offset of the largest already-computed char index that is
                    // less than or equal to the current requested one. We resume counting
                    // chars from that point.
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

                fn source_text(&mut self, span: Span) -> String {
                    let byte_range = self.byte_range(span);
                    self.source_text[byte_range].to_owned()
                }
            }
            /// Computes the offsets of each line in the given source string and the total number of characters
            #[cfg(all(span_locations, not(fuzzing)))]
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

            #[cfg(all(span_locations, not(fuzzing)))]
            struct SourceMap {
                files: Vec<FileInfo>,
            }

            #[cfg(all(span_locations, not(fuzzing)))]
            impl SourceMap
            {
                fn next_start_pos(&self) -> u32 {
                    // Add 1 so there's always space between files.
                    //
                    // We'll always have at least 1 file, as we initialize our files list
                    // with a dummy file.
                    self.files.last().unwrap().span.hi + 1
                }

                fn add_file(&mut self, src: &str) -> Span {
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
                        // Populated lazily by source_text().
                        char_index_to_byte_offset: BTreeMap::new(),
                    });

                    span
                }

                fn find(&self, span: Span) -> usize {
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

                fn filepath(&self, span: Span) -> String {
                    let i = self.find(span);
                    if i == 0 {
                        "<unspecified>".to_owned()
                    } else {
                        format!("<parsed string {}>", i)
                    }
                }

                fn fileinfo(&self, span: Span) -> &FileInfo {
                    let i = self.find(span);
                    &self.files[i]
                }

                fn fileinfo_mut(&mut self, span: Span) -> &mut FileInfo {
                    let i = self.find(span);
                    &mut self.files[i]
                }
            }

            #[derive(Clone, Copy, PartialEq, Eq)]
            pub struct Span {
                #[cfg(span_locations)]
                pub lo: u32,
                #[cfg(span_locations)]
                pub hi: u32,
            }

            impl Span {
                #[cfg(not(span_locations))]
                pub fn call_site() -> Self {
                    Span {}
                }

                #[cfg(span_locations)]
                pub fn call_site() -> Self {
                    Span { lo: 0, hi: 0 }
                }

                pub fn mixed_site() -> Self {
                    Span::call_site()
                }

                #[cfg(procmacro2_semver_exempt)]
                pub fn def_site() -> Self {
                    Span::call_site()
                }

                pub fn resolved_at(&self, _other: Span) -> Span {
                    *self
                }

                pub fn located_at(&self, other: Span) -> Span {
                    other
                }

                #[cfg(span_locations)]
                pub fn byte_range(&self) -> Range<usize> {
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

                #[cfg(span_locations)]
                pub fn start(&self) -> LineColumn {
                    #[cfg(fuzzing)]
                    return LineColumn { line: 0, column: 0 };

                    #[cfg(not(fuzzing))]
                    SOURCE_MAP.with(|sm| {
                        let sm = sm.borrow();
                        let fi = sm.fileinfo(*self);
                        fi.offset_line_column(self.lo as usize)
                    })
                }

                #[cfg(span_locations)]
                pub fn end(&self) -> LineColumn {
                    #[cfg(fuzzing)]
                    return LineColumn { line: 0, column: 0 };

                    #[cfg(not(fuzzing))]
                    SOURCE_MAP.with(|sm| {
                        let sm = sm.borrow();
                        let fi = sm.fileinfo(*self);
                        fi.offset_line_column(self.hi as usize)
                    })
                }

                #[cfg(span_locations)]
                pub fn file(&self) -> String {
                    #[cfg(fuzzing)]
                    return "<unspecified>".to_owned();

                    #[cfg(not(fuzzing))]
                    SOURCE_MAP.with(|sm| {
                        let sm = sm.borrow();
                        sm.filepath(*self)
                    })
                }

                #[cfg(span_locations)]
                pub fn local_file(&self) -> Option<PathBuf> {
                    None
                }

                #[cfg(not(span_locations))]
                pub fn join(&self, _other: Span) -> Option<Span> {
                    Some(Span {})
                }

                #[cfg(span_locations)]
                pub fn join(&self, other: Span) -> Option<Span> {
                    #[cfg(fuzzing)]
                    return {
                        let _ = other;
                        None
                    };

                    #[cfg(not(fuzzing))]
                    SOURCE_MAP.with(|sm| {
                        let sm = sm.borrow();
                        // If `other` is not within the same FileInfo as us, return None.
                        if !sm.fileinfo(*self).span_within(other) {
                            return None;
                        }
                        Some(Span {
                            lo: cmp::min(self.lo, other.lo),
                            hi: cmp::max(self.hi, other.hi),
                        })
                    })
                }

                #[cfg(not(span_locations))]
                pub fn source_text(&self) -> Option<String> {
                    None
                }

                #[cfg(span_locations)]
                pub fn source_text(&self) -> Option<String> {
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

                #[cfg(not(span_locations))]
                pub fn first_byte(self) -> Self {
                    self
                }

                #[cfg(span_locations)]
                pub fn first_byte(self) -> Self {
                    Span {
                        lo: self.lo,
                        hi: cmp::min(self.lo.saturating_add(1), self.hi),
                    }
                }

                #[cfg(not(span_locations))]
                pub fn last_byte(self) -> Self {
                    self
                }

                #[cfg(span_locations)]
                pub fn last_byte(self) -> Self {
                    Span {
                        lo: cmp::max(self.hi.saturating_sub(1), self.lo),
                        hi: self.hi,
                    }
                }

                #[cfg(span_locations)]
                fn is_call_site(&self) -> bool {
                    self.lo == 0 && self.hi == 0
                }
            }

            impl Debug for Span
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    #[cfg(span_locations)]
                    return write!(f, "bytes({}..{})", self.lo, self.hi);

                    #[cfg(not(span_locations))]
                    write!(f, "Span")
                }
            }

            pub fn debug_span_field_if_nontrivial(debug: &mut fmt::DebugStruct, span: Span) {
                #[cfg(span_locations)]
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

                pub fn delimiter(&self) -> Delimiter {
                    self.delimiter
                }

                pub fn stream(&self) -> TokenStream {
                    self.stream.clone()
                }

                pub fn span(&self) -> Span {
                    self.span
                }

                pub fn span_open(&self) -> Span {
                    self.span.first_byte()
                }

                pub fn span_close(&self) -> Span {
                    self.span.last_byte()
                }

                pub fn set_span(&mut self, span: Span) {
                    self.span = span;
                }
            }

            impl Display for Group {
                // We attempt to match libproc_macro's formatting.
                // Empty parens: ()
                // Nonempty parens: (...)
                // Empty brackets: []
                // Nonempty brackets: [...]
                // Empty braces: { }
                // Nonempty braces: { ... }
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    let (open, close) = match self.delimiter {
                        Delimiter::Parenthesis => ("(", ")"),
                        Delimiter::Brace => ("{ ", "}"),
                        Delimiter::Bracket => ("[", "]"),
                        Delimiter::None => ("", ""),
                    };

                    f.write_str(open)?;
                    Display::fmt(&self.stream, f)?;
                    if self.delimiter == Delimiter::Brace && !self.stream.inner.is_empty() {
                        f.write_str(" ")?;
                    }
                    f.write_str(close)?;

                    Ok(())
                }
            }

            impl Debug for Group
            {
                fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                    let mut debug = fmt.debug_struct("Group");
                    debug.field("delimiter", &self.delimiter);
                    debug.field("stream", &self.stream);
                    debug_span_field_if_nontrivial(&mut debug, self.span);
                    debug.finish()
                }
            }

            #[derive(Clone)]
            pub struct Ident {
                sym: Box<str>,
                span: Span,
                raw: bool,
            }

            impl Ident {
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

                pub fn span(&self) -> Span {
                    self.span
                }

                pub fn set_span(&mut self, span: Span) {
                    self.span = span;
                }
            }

            pub fn is_ident_start(c: char) -> bool {
                c == '_' || unicode_ident::is_xid_start(c)
            }

            pub fn is_ident_continue(c: char) -> bool {
                unicode_ident::is_xid_continue(c)
            }

            #[track_caller]
            fn validate_ident(string: &str) {
                if string.is_empty() {
                    panic!("Ident is not allowed to be empty; use Option<Ident>");
                }

                if string.bytes().all(|digit| b'0' <= digit && digit <= b'9') {
                    panic!("Ident cannot be a number; use Literal instead");
                }

                fn ident_ok(string: &str) -> bool {
                    let mut chars = string.chars();
                    let first = chars.next().unwrap();
                    if !is_ident_start(first) {
                        return false;
                    }
                    for ch in chars {
                        if !is_ident_continue(ch) {
                            return false;
                        }
                    }
                    true
                }

                if !ident_ok(string) {
                    panic!("{:?} is not a valid Ident", string);
                }
            }

            #[track_caller]
            fn validate_ident_raw(string: &str) {
                validate_ident(string);

                match string {
                    "_" | "super" | "self" | "Self" | "crate" => {
                        panic!("`r#{}` cannot be a raw identifier", string);
                    }
                    _ => {}
                }
            }

            impl PartialEq for Ident
            {
                fn eq(&self, other: &Ident) -> bool {
                    self.sym == other.sym && self.raw == other.raw
                }
            }

            impl<T> PartialEq<T> for Ident
            where
                T: ?Sized + AsRef<str>,
           
            {
                fn eq(&self, other: &T) -> bool {
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
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    if self.raw {
                        f.write_str("r#")?;
                    }
                    Display::fmt(&self.sym, f)
                }
            }

            #[allow(clippy::missing_fields_in_debug)]
            impl Debug for Ident {
                // Ident(proc_macro), Ident(r#union)
                #[cfg(not(span_locations))]
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    let mut debug = f.debug_tuple("Ident");
                    debug.field(&format_args!("{}", self));
                    debug.finish()
                }

                // Ident {
                //     sym: proc_macro,
                //     span: bytes(128..138)
                // }
                #[cfg(span_locations)]
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

                pub fn from_str_checked(repr: &str) -> Result<Self, LexError> {
                    let mut cursor = get_cursor(repr);
                    #[cfg(span_locations)]
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
                                #[cfg(span_locations)]
                                lo,
                                #[cfg(span_locations)]
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

                pub fn f32_unsuffixed(f: f32) -> Literal {
                    let mut s = f.to_string();
                    if !s.contains('.') {
                        s.push_str(".0");
                    }
                    Literal::_new(s)
                }

                pub fn f64_unsuffixed(f: f64) -> Literal {
                    let mut s = f.to_string();
                    if !s.contains('.') {
                        s.push_str(".0");
                    }
                    Literal::_new(s)
                }

                pub fn string(string: &str) -> Literal {
                    let mut repr = String::with_capacity(string.len() + 2);
                    repr.push('"');
                    escape_utf8(string, &mut repr);
                    repr.push('"');
                    Literal::_new(repr)
                }

                pub fn character(ch: char) -> Literal {
                    let mut repr = String::new();
                    repr.push('\'');
                    if ch == '"' {
                        // escape_debug turns this into '\"' which is unnecessary.
                        repr.push(ch);
                    } else {
                        repr.extend(ch.escape_debug());
                    }
                    repr.push('\'');
                    Literal::_new(repr)
                }

                pub fn byte_character(byte: u8) -> Literal {
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
                        _ => {
                            let _ = write!(repr, r"\x{:02X}", byte);
                        }
                    }
                    repr.push('\'');
                    Literal::_new(repr)
                }

                pub fn byte_string(bytes: &[u8]) -> Literal {
                    let mut repr = "b\"".to_string();
                    let mut bytes = bytes.iter();
                    while let Some(&b) = bytes.next() {
                        #[allow(clippy::match_overlapping_arm)]
                        match b {
                            b'\0' => repr.push_str(match bytes.as_slice().first() {
                                // circumvent clippy::octal_escapes lint
                                Some(b'0'..=b'7') => r"\x00",
                                _ => r"\0",
                            }),
                            b'\t' => repr.push_str(r"\t"),
                            b'\n' => repr.push_str(r"\n"),
                            b'\r' => repr.push_str(r"\r"),
                            b'"' => repr.push_str("\\\""),
                            b'\\' => repr.push_str(r"\\"),
                            b'\x20'..=b'\x7E' => repr.push(b as char),
                            _ => {
                                let _ = write!(repr, r"\x{:02X}", b);
                            }
                        }
                    }
                    repr.push('"');
                    Literal::_new(repr)
                }

                pub fn c_string(string: &CStr) -> Literal {
                    let mut repr = "c\"".to_string();
                    let mut bytes = string.to_bytes();
                    while !bytes.is_empty() {
                        let (valid, invalid) = match str::from_utf8(bytes) {
                            Ok(all_valid) => {
                                bytes = b"";
                                (all_valid, bytes)
                            }
                            Err(utf8_error) => {
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

                pub fn span(&self) -> Span {
                    self.span
                }

                pub fn set_span(&mut self, span: Span) {
                    self.span = span;
                }

                pub fn subspan<R: RangeBounds<usize>>(&self, range: R) -> Option<Span> {
                    #[cfg(not(span_locations))]
                    {
                        let _ = range;
                        None
                    }

                    #[cfg(span_locations)]
                    {
                        use core::ops::Bound;

                        let lo = match range.start_bound() {
                            Bound::Included(start) => {
                                let start = u32::try_from(*start).ok()?;
                                self.span.lo.checked_add(start)?
                            }
                            Bound::Excluded(start) => {
                                let start = u32::try_from(*start).ok()?;
                                self.span.lo.checked_add(start)?.checked_add(1)?
                            }
                            Bound::Unbounded => self.span.lo,
                        };
                        let hi = match range.end_bound() {
                            Bound::Included(end) => {
                                let end = u32::try_from(*end).ok()?;
                                self.span.lo.checked_add(end)?.checked_add(1)?
                            }
                            Bound::Excluded(end) => {
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
            }

            impl Display for Literal
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    Display::fmt(&self.repr, f)
                }
            }

            impl Debug for Literal
            {
                fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                    let mut debug = fmt.debug_struct("Literal");
                    debug.field("lit", &format_args!("{}", self.repr));
                    debug_span_field_if_nontrivial(&mut debug, self.span);
                    debug.finish()
                }
            }
            
            pub trait FromStr2: FromStr<Err = proc_macro::LexError>
            {
                #[cfg(wrap_proc_macro)]
                fn valid(src: &str) -> bool;

                #[cfg(wrap_proc_macro)]
                fn from_str_checked(src: &str) -> Result<Self, imp::LexError> {
                    // Validate using fallback parser, because rustc is incapable of
                    // returning a recoverable Err for certain invalid token streams, and
                    // will instead permanently poison the compilation.
                    if !Self::valid(src) {
                        return Err(imp::LexError::CompilerPanic);
                    }

                    // Catch panic to work around https://github.com/rust-lang/rust/issues/58736.
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
                #[cfg(wrap_proc_macro)]
                fn valid(src: &str) -> bool {
                    TokenStream::from_str_checked(src).is_ok()
                }
            }
            
            impl FromStr2 for proc_macro::Literal
            {
                #[cfg(wrap_proc_macro)]
                fn valid(src: &str) -> bool {
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
                                // circumvent clippy::octal_escapes lint
                                r"\x00"
                            } else {
                                r"\0"
                            },
                        );
                    } else if ch == '\'' {
                        // escape_debug turns this into "\'" which is unnecessary.
                        repr.push(ch);
                    } else {
                        repr.extend(ch.escape_debug());
                    }
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
                #[cfg(wrap_proc_macro)]
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
                        _marker: MARKER,
                    }
                }
                /// Returns a span covering the entire delimited group.
                pub fn join(&self) -> Span
                {
                    match &self.inner
                    {
                        DelimSpanEnum::Compiler { join, .. } => Span::_new(imp::Span::Compiler(*join)),
                        DelimSpanEnum::Fallback(span) => Span::_new_fallback(*span),
                    }
                }
                /// Returns a span for the opening punctuation of the group only.
                pub fn open(&self) -> Span
                {
                    match &self.inner
                    {
                        DelimSpanEnum::Compiler { open, .. } => Span::_new(imp::Span::Compiler(*open)),
                        DelimSpanEnum::Fallback(span) => Span::_new_fallback(span.first_byte()),
                    }
                }
                /// Returns a span for the closing punctuation of the group only.
                pub fn close(&self) -> Span
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
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    Debug::fmt(&self.join(), f)
                }
            }
        }
        /*
            #[cfg(not(wrap_proc_macro))]
            use ::process::macros::fallback as imp;
            #[path = "wrapper.rs"]
            #[cfg(wrap_proc_macro)] */    
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
            use ::process::macros::detection::inside_proc_macro;
            use ::process::macros::fallback::{self, FromStr2 as _};
            #[cfg(span_locations)]
            use ::process::macros::location::LineColumn;
            #[cfg(proc_macro_span)]
            use ::process::macros::probe::proc_macro_span;
            #[cfg(all(span_locations, proc_macro_span_file))]
            use ::process::macros::probe::proc_macro_span_file;
            #[cfg(all(span_locations, proc_macro_span_location))]
            use ::process::macros::probe::proc_macro_span_location;
            use ::process::macros::{Delimiter, Punct, Spacing, TokenTree};
            use core::fmt::{self, Debug, Display};
            #[cfg(span_locations)]
            use core::ops::Range;
            use core::ops::RangeBounds;
            use std::ffi::CStr;
            #[cfg(span_locations)]
            use std::path::PathBuf;
            */
            #[derive(Clone)]
            pub enum TokenStream {
                Compiler(DeferredTokenStream),
                Fallback(fallback::TokenStream),
            }

            #[derive(Clone)]
            pub struct DeferredTokenStream {
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

                fn is_empty(&self) -> bool { self.stream.is_empty() && self.extra.is_empty() }

                fn evaluate_now(&mut self)
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

                pub fn from_str_checked(src: &str) -> Result<Self, LexError> {
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

                pub fn is_empty(&self) -> bool
                {
                    match self {
                        TokenStream::Compiler(tts) => tts.is_empty(),
                        TokenStream::Fallback(tts) => tts.is_empty(),
                    }
                }

                fn unwrap_nightly(self) -> proc_macro::TokenStream
                {
                    match self {
                        TokenStream::Compiler(s) => s.into_token_stream(),
                        TokenStream::Fallback(_) => mismatch(line!()),
                    }
                }

                fn unwrap_stable(self) -> fallback::TokenStream           
                {
                    match self {
                        TokenStream::Compiler(_) => mismatch(line!()),
                        TokenStream::Fallback(s) => s,
                    }
                }
            }

            impl Display for TokenStream       
           
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
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
                        TokenStream::Fallback(inner) => {
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
                    TokenTree::Punct(tt) => {
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

            impl FromIterator<TokenTree> for TokenStream
           
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

            impl FromIterator<TokenStream> for TokenStream
           
            {
                fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self
                {
                    let mut streams = streams.into_iter();
                    match streams.next() {
                        Some(TokenStream::Compiler(mut first)) => {
                            first.evaluate_now();
                            first.stream.extend(streams.map(|s| match s {
                                TokenStream::Compiler(s) => s.into_token_stream(),
                                TokenStream::Fallback(_) => mismatch(line!()),
                            }));
                            TokenStream::Compiler(first)
                        }
                        Some(TokenStream::Fallback(mut first)) => {
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
                        TokenStream::Compiler(tts) => {
                            // Here is the reason for DeferredTokenStream.
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
                        TokenStream::Compiler(tts) => {
                            tts.evaluate_now();
                            tts.stream
                                .extend(streams.into_iter().map(TokenStream::unwrap_nightly));
                        }
                        TokenStream::Fallback(tts) => {
                            tts.extend(streams.into_iter().map(TokenStream::unwrap_stable));
                        }
                    }
                }
            }

            impl Debug for TokenStream
           
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        TokenStream::Compiler(tts) => Debug::fmt(&tts.clone().into_token_stream(), f),
                        TokenStream::Fallback(tts) => Debug::fmt(tts, f),
                    }
                }
            }

            impl LexError
            {
                pub fn span(&self) -> Span
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
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        LexError::Compiler(e) => Debug::fmt(e, f),
                        LexError::Fallback(e) => Debug::fmt(e, f),
                        LexError::CompilerPanic => {
                            let fallback = fallback::LexError::call_site();
                            Debug::fmt(&fallback, f)
                        }
                    }
                }
            }

            impl Display for LexError
           
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        LexError::Compiler(e) => Display::fmt(e, f),
                        LexError::Fallback(e) => Display::fmt(e, f),
                        LexError::CompilerPanic => {
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

                fn into_iter(self) -> TokenTreeIter
                {
                    match self {
                        TokenStream::Compiler(tts) => {
                            TokenTreeIter::Compiler(tts.into_token_stream().into_iter())
                        }
                        TokenStream::Fallback(tts) => TokenTreeIter::Fallback(tts.into_iter()),
                    }
                }
            }

            impl Iterator for TokenTreeIter
            {
                type Item = TokenTree;

                fn next(&mut self) -> Option<TokenTree> {
                    let token = match self {
                        TokenTreeIter::Compiler(iter) => iter.next()?,
                        TokenTreeIter::Fallback(iter) => return iter.next(),
                    };
                    Some(match token {
                        proc_macro::TokenTree::Group(tt) => {
                            TokenTree::Group(::process::macros::Group::_new(Group::Compiler(tt)))
                        }
                        proc_macro::TokenTree::Punct(tt) => {
                            let spacing = match tt.spacing() {
                                proc_macro::Spacing::Joint => Spacing::Joint,
                                proc_macro::Spacing::Alone => Spacing::Alone,
                            };
                            let mut o = Punct::new(tt.as_char(), spacing);
                            o.set_span(::process::macros::Span::_new(Span::Compiler(tt.span())));
                            TokenTree::Punct(o)
                        }
                        proc_macro::TokenTree::Ident(s) => {
                            TokenTree::Ident(::process::macros::Ident::_new(Ident::Compiler(s)))
                        }
                        proc_macro::TokenTree::Literal(l) => {
                            TokenTree::Literal(::process::macros::Literal::_new(Literal::Compiler(l)))
                        }
                    })
                }

                fn size_hint(&self) -> (usize, Option<usize>)
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

                #[cfg(super_unstable)]
                pub fn def_site() -> Self
                {
                    if inside_proc_macro() {
                        Span::Compiler(proc_macro::Span::def_site())
                    } else {
                        Span::Fallback(fallback::Span::def_site())
                    }
                }

                pub fn resolved_at(&self, other: Span) -> Span {
                    match (self, other) {
                        (Span::Compiler(a), Span::Compiler(b)) => Span::Compiler(a.resolved_at(b)),
                        (Span::Fallback(a), Span::Fallback(b)) => Span::Fallback(a.resolved_at(b)),
                        (Span::Compiler(_), Span::Fallback(_)) => mismatch(line!()),
                        (Span::Fallback(_), Span::Compiler(_)) => mismatch(line!()),
                    }
                }

                pub fn located_at(&self, other: Span) -> Span {
                    match (self, other) {
                        (Span::Compiler(a), Span::Compiler(b)) => Span::Compiler(a.located_at(b)),
                        (Span::Fallback(a), Span::Fallback(b)) => Span::Fallback(a.located_at(b)),
                        (Span::Compiler(_), Span::Fallback(_)) => mismatch(line!()),
                        (Span::Fallback(_), Span::Compiler(_)) => mismatch(line!()),
                    }
                }

                pub fn unwrap(self) -> proc_macro::Span
                {
                    match self {
                        Span::Compiler(s) => s,
                        Span::Fallback(_) => panic!("proc_macro::Span is only available in procedural macros"),
                    }
                }

                #[cfg(span_locations)]
                pub fn byte_range(&self) -> Range<usize>
                {
                    match self {
                        #[cfg(proc_macro_span)]
                        Span::Compiler(s) => proc_macro_span::byte_range(s),
                        #[cfg(not(proc_macro_span))]
                        Span::Compiler(_) => 0..0,
                        Span::Fallback(s) => s.byte_range(),
                    }
                }

                #[cfg(span_locations)]
                pub fn start(&self) -> LineColumn
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

                #[cfg(span_locations)]
                pub fn end(&self) -> LineColumn
                {
                    match self {
                        #[cfg(proc_macro_span_location)]
                        Span::Compiler(s) => {
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

                #[cfg(span_locations)]
                pub fn file(&self) -> String
                {
                    match self {
                        #[cfg(proc_macro_span_file)]
                        Span::Compiler(s) => proc_macro_span_file::file(s),
                        #[cfg(not(proc_macro_span_file))]
                        Span::Compiler(_) => "<token stream>".to_owned(),
                        Span::Fallback(s) => s.file(),
                    }
                }

                #[cfg(span_locations)]
                pub fn local_file(&self) -> Option<PathBuf>
                {
                    match self {
                        #[cfg(proc_macro_span_file)]
                        Span::Compiler(s) => proc_macro_span_file::local_file(s),
                        #[cfg(not(proc_macro_span_file))]
                        Span::Compiler(_) => None,
                        Span::Fallback(s) => s.local_file(),
                    }
                }

                pub fn join(&self, other: Span) -> Option<Span> {
                    let ret = match (self, other) {
                        #[cfg(proc_macro_span)]
                        (Span::Compiler(a), Span::Compiler(b)) => Span::Compiler(proc_macro_span::join(a, b)?),
                        (Span::Fallback(a), Span::Fallback(b)) => Span::Fallback(a.join(b)?),
                        _ => return None,
                    };
                    Some(ret)
                }

                #[cfg(super_unstable)]
                pub fn eq(&self, other: &Span) -> bool {
                    match (self, other) {
                        (Span::Compiler(a), Span::Compiler(b)) => a.eq(b),
                        (Span::Fallback(a), Span::Fallback(b)) => a.eq(b),
                        _ => false,
                    }
                }

                pub fn source_text(&self) -> Option<String>
                {
                    match self {
                        #[cfg(not(no_source_text))]
                        Span::Compiler(s) => s.source_text(),
                        #[cfg(no_source_text)]
                        Span::Compiler(_) => None,
                        Span::Fallback(s) => s.source_text(),
                    }
                }

                fn unwrap_nightly(self) -> proc_macro::Span
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
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
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
                    Span::Compiler(s) => {
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
                        TokenStream::Compiler(tts) => {
                            let delimiter = match delimiter {
                                Delimiter::Parenthesis => proc_macro::Delimiter::Parenthesis,
                                Delimiter::Bracket => proc_macro::Delimiter::Bracket,
                                Delimiter::Brace => proc_macro::Delimiter::Brace,
                                Delimiter::None => proc_macro::Delimiter::None,
                            };
                            Group::Compiler(proc_macro::Group::new(delimiter, tts.into_token_stream()))
                        }
                        TokenStream::Fallback(stream) => {
                            Group::Fallback(fallback::Group::new(delimiter, stream))
                        }
                    }
                }

                pub fn delimiter(&self) -> Delimiter
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

                pub fn stream(&self) -> TokenStream
                {
                    match self {
                        Group::Compiler(g) => TokenStream::Compiler(DeferredTokenStream::new(g.stream())),
                        Group::Fallback(g) => TokenStream::Fallback(g.stream()),
                    }
                }

                pub fn span(&self) -> Span
                {
                    match self {
                        Group::Compiler(g) => Span::Compiler(g.span()),
                        Group::Fallback(g) => Span::Fallback(g.span()),
                    }
                }

                pub fn span_open(&self) -> Span
                {
                    match self {
                        Group::Compiler(g) => Span::Compiler(g.span_open()),
                        Group::Fallback(g) => Span::Fallback(g.span_open()),
                    }
                }

                pub fn span_close(&self) -> Span
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

                fn unwrap_nightly(self) -> proc_macro::Group
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
                fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Group::Compiler(group) => Display::fmt(group, formatter),
                        Group::Fallback(group) => Display::fmt(group, formatter),
                    }
                }
            }

            impl Debug for Group
           
            {
                fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Group::Compiler(group) => Debug::fmt(group, formatter),
                        Group::Fallback(group) => Debug::fmt(group, formatter),
                    }
                }
            }

            #[derive(Clone)]
            pub enum Ident {
                Compiler(proc_macro::Ident),
                Fallback(fallback::Ident),
            }

            impl Ident {
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

                pub fn span(&self) -> Span
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

                fn unwrap_nightly(self) -> proc_macro::Ident
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
                fn eq(&self, other: &Ident) -> bool {
                    match (self, other) {
                        (Ident::Compiler(t), Ident::Compiler(o)) => t.to_string() == o.to_string(),
                        (Ident::Fallback(t), Ident::Fallback(o)) => t == o,
                        (Ident::Compiler(_), Ident::Fallback(_)) => mismatch(line!()),
                        (Ident::Fallback(_), Ident::Compiler(_)) => mismatch(line!()),
                    }
                }
            }

            impl<T> PartialEq<T> for Ident
            where
                T: ?Sized + AsRef<str>,
        
           
            {
                fn eq(&self, other: &T) -> bool {
                    let other = other.as_ref();
                    match self {
                        Ident::Compiler(t) => t.to_string() == other,
                        Ident::Fallback(t) => t == other,
                    }
                }
            }

            impl Display for Ident
           
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Ident::Compiler(t) => Display::fmt(t, f),
                        Ident::Fallback(t) => Display::fmt(t, f),
                    }
                }
            }

            impl Debug for Ident
           
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Ident::Compiler(t) => Debug::fmt(t, f),
                        Ident::Fallback(t) => Debug::fmt(t, f),
                    }
                }
            }

            #[derive(Clone)]
            pub enum Literal {
                Compiler(proc_macro::Literal),
                Fallback(fallback::Literal),
            }

            macro_rules! suffixed_numbers {
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

            macro_rules! unsuffixed_integers {
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
                pub fn from_str_checked(repr: &str) -> Result<Self, LexError> {
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

                pub fn span(&self) -> Span
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

                pub fn subspan<R: RangeBounds<usize>>(&self, range: R) -> Option<Span>
                {
                    match self {
                        #[cfg(proc_macro_span)]
                        Literal::Compiler(lit) => proc_macro_span::subspan(lit, range).map(Span::Compiler),
                        #[cfg(not(proc_macro_span))]
                        Literal::Compiler(_lit) => None,
                        Literal::Fallback(lit) => lit.subspan(range).map(Span::Fallback),
                    }
                }

                fn unwrap_nightly(self) -> proc_macro::Literal
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
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Literal::Compiler(t) => Display::fmt(t, f),
                        Literal::Fallback(t) => Display::fmt(t, f),
                    }
                }
            }

            impl Debug for Literal
           
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    match self {
                        Literal::Compiler(t) => Debug::fmt(t, f),
                        Literal::Fallback(t) => Debug::fmt(t, f),
                    }
                }
            }

            #[cfg(span_locations)]
            pub fn invalidate_current_thread_spans() {
                if inside_proc_macro() {
                    panic!(
                        "proc_macro2::extra::invalidate_current_thread_spans is not available in procedural macros"
                    );
                } else {
                    ::process::macros::fallback::invalidate_current_thread_spans();
                }
            }
        }
        //#[cfg(span_locations)]
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
                fn cmp(&self, other: &Self) -> Ordering
                {
                    self.line
                    .cmp(&other.line)
                    .then(self.column.cmp(&other.column))
                }
            }

            impl PartialOrd for LineColumn
            {
                fn partial_cmp(&self, other: &Self) -> Option<Ordering>
                {
                    Some(self.cmp(other))
                }
            }
        }
        /// An abstract stream of tokens, or more concretely a sequence of token trees
        #[derive(Clone)]
        pub struct TokenStream
        {
            inner: imp::TokenStream,
            _marker: ProcMacroAutoTraits,
        }
        /// Error returned from `TokenStream::from_str`.
        pub struct LexError {
            inner: imp::LexError,
            _marker: ProcMacroAutoTraits,
        }

        impl TokenStream
        {
            fn _new(inner: imp::TokenStream) -> Self {
                TokenStream {
                    inner,
                    _marker: MARKER,
                }
            }

            fn _new_fallback(inner: fallback::TokenStream) -> Self {
                TokenStream {
                    inner: imp::TokenStream::from(inner),
                    _marker: MARKER,
                }
            }
            /// Returns an empty `TokenStream` containing no token trees.
            pub fn new() -> Self {
                TokenStream::_new(imp::TokenStream::new())
            }
            /// Checks if this `TokenStream` is empty.
            pub fn is_empty(&self) -> bool {
                self.inner.is_empty()
            }
        }
        /// `TokenStream::default()` returns an empty stream,
        /// i.e. this is equivalent with `TokenStream::new()`.
        impl Default for TokenStream
        {
            fn default() -> Self {
                TokenStream::new()
            }
        }
        /// Attempts to break the string into tokens and parse those tokens into a token
        /// stream
        impl FromStr for TokenStream {
            type Err = LexError;

            fn from_str(src: &str) -> Result<TokenStream, LexError> {
                match imp::TokenStream::from_str_checked(src) {
                    Ok(tokens) => Ok(TokenStream::_new(tokens)),
                    Err(lex) => Err(LexError {
                        inner: lex,
                        _marker: MARKER,
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
        impl FromIterator<TokenTree> for TokenStream
        {
            fn from_iter<I: IntoIterator<Item = TokenTree>>(streams: I) -> Self {
                TokenStream::_new(streams.into_iter().collect())
            }
        }
        impl FromIterator<TokenStream> for TokenStream
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
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, f)
            }
        }
        /// Prints token in a form convenient for debugging.
        impl Debug for TokenStream
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }

        impl LexError {
            pub fn span(&self) -> Span {
                Span::_new(self.inner.span())
            }
        }

        impl Debug for LexError {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }

        impl Display for LexError {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, f)
            }
        }

        impl Error for LexError {}
        /// A region of source code, along with macro expansion information.
        #[derive(Copy, Clone)]
        pub struct Span {
            inner: imp::Span,
            _marker: ProcMacroAutoTraits,
        }

        impl Span {
            fn _new(inner: imp::Span) -> Self {
                Span {
                    inner,
                    _marker: MARKER,
                }
            }

            fn _new_fallback(inner: fallback::Span) -> Self {
                Span {
                    inner: imp::Span::from(inner),
                    _marker: MARKER,
                }
            }
            /// The span of the invocation of the current procedural macro
            pub fn call_site() -> Self {
                Span::_new(imp::Span::call_site())
            }
            /// The span located at the invocation of the procedural macro, but with
            /// local variables, labels, and `$crate` resolved at the definition site
            /// of the macro. This is the same hygiene behavior as `macro_rules`.
            pub fn mixed_site() -> Self {
                Span::_new(imp::Span::mixed_site())
            }
            /// A span that resolves at the macro definition site.
            ///
            /// This method is semver exempt and not exposed by default.
            #[cfg(procmacro2_semver_exempt)]
            #[cfg_attr(docsrs, doc(cfg(procmacro2_semver_exempt)))]
            pub fn def_site() -> Self {
                Span::_new(imp::Span::def_site())
            }
            /// Creates a new span with the same line/column information as `self` but
            /// that resolves symbols as though it were at `other`.
            pub fn resolved_at(&self, other: Span) -> Span {
                Span::_new(self.inner.resolved_at(other.inner))
            }
            /// Creates a new span with the same name resolution behavior as `self` but
            /// with the line/column information of `other`.
            pub fn located_at(&self, other: Span) -> Span {
                Span::_new(self.inner.located_at(other.inner))
            }
            /// Convert `proc_macro2::Span` to `proc_macro::Span`.
            ///
            /// This method is available when building with a nightly compiler, or when
            /// building with rustc 1.29+ *without* semver exempt features.
            ///
            /// # Panic
            #[cfg(wrap_proc_macro)]
            pub fn unwrap(self) -> proc_macro::Span {
                self.inner.unwrap()
            }

            // Soft deprecated. Please use Span::unwrap.
            #[cfg(wrap_proc_macro)]
            #[doc(hidden)]
            pub fn unstable(self) -> proc_macro::Span {
                self.unwrap()
            }
            /// Returns the span's byte position range in the source file.
            ///
            /// This method requires the `"span-locations"` feature to be enabled
            /// procedural macro, such as main.rs or build.rs, the byte range is always
            /// accurate regardless of toolchain.
            #[cfg(span_locations)]
            #[cfg_attr(docsrs, doc(cfg(feature = "span-locations")))]
            pub fn byte_range(&self) -> Range<usize> {
                self.inner.byte_range()
            }
            /// Get the starting line/column in the source file for this span.
            ///
            /// This method requires the `"span-locations"` feature to be enabled
            /// outside of a procedural macro, such as main.rs or build.rs, the
            /// line/column are always meaningful regardless of toolchain.
            #[cfg(span_locations)]
            #[cfg_attr(docsrs, doc(cfg(feature = "span-locations")))]
            pub fn start(&self) -> LineColumn {
                self.inner.start()
            }
            /// Get the ending line/column in the source file for this span.
            ///
            /// This method requires the `"span-locations"` feature to be enabled
            /// outside of a procedural macro, such as main.rs or build.rs, the
            /// line/column are always meaningful regardless of toolchain.
            #[cfg(span_locations)]
            #[cfg_attr(docsrs, doc(cfg(feature = "span-locations")))]
            pub fn end(&self) -> LineColumn {
                self.inner.end()
            }
            /// The path to the source file in which this span occurs, for display
            /// purposes.
            ///
            /// This might not correspond to a valid file system path. It might be
            /// remapped, or might be an artificial path such as `"<macro expansion>"`.
            #[cfg(span_locations)]
            #[cfg_attr(docsrs, doc(cfg(feature = "span-locations")))]
            pub fn file(&self) -> String {
                self.inner.file()
            }
            /// The path to the source file in which this span occurs on disk.
            ///
            /// This is the actual path on disk. It is unaffected by path remapping.
            ///
            /// This path should not be embedded in the output of the macro; prefer
            /// `file()` instead.
            #[cfg(span_locations)]
            #[cfg_attr(docsrs, doc(cfg(feature = "span-locations")))]
            pub fn local_file(&self) -> Option<PathBuf> {
                self.inner.local_file()
            }
            /// Create a new span encompassing `self` and `other`.
            ///
            /// Returns `None` if `self` and `other` are from different files
            pub fn join(&self, other: Span) -> Option<Span> {
                self.inner.join(other.inner).map(Span::_new)
            }
            /// Compares two spans to see if they're equal.
            ///
            /// This method is semver exempt and not exposed by default.
            #[cfg(procmacro2_semver_exempt)]
            #[cfg_attr(docsrs, doc(cfg(procmacro2_semver_exempt)))]
            pub fn eq(&self, other: &Span) -> bool {
                self.inner.eq(&other.inner)
            }
            /// Returns the source text behind a span. This preserves the original
            /// source code, including spaces and comments. It only returns a result if
            /// the span corresponds to real source code
            pub fn source_text(&self) -> Option<String> {
                self.inner.source_text()
            }
        }
        /// Prints a span in a form convenient for debugging.
        impl Debug for Span {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }
        /// A single token or a delimited sequence of token trees (e.g. `[1, (), ..]`).
        #[derive(Clone)]
        pub enum TokenTree {
            /// A token stream surrounded by bracket delimiters.
            Group(Group),
            /// An identifier.
            Ident(Ident),
            /// A single punctuation character (`+`, `,`, `$`, etc.).
            Punct(Punct),
            /// A literal character (`'a'`), string (`"hello"`), number (`2.3`), etc.
            Literal(Literal),
        }

        impl TokenTree {
            /// Returns the span of this tree, delegating to the `span` method of
            /// the contained token or a delimited stream.
            pub fn span(&self) -> Span {
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

        impl From<Group> for TokenTree {
            fn from(g: Group) -> Self {
                TokenTree::Group(g)
            }
        }

        impl From<Ident> for TokenTree {
            fn from(g: Ident) -> Self {
                TokenTree::Ident(g)
            }
        }

        impl From<Punct> for TokenTree {
            fn from(g: Punct) -> Self {
                TokenTree::Punct(g)
            }
        }

        impl From<Literal> for TokenTree {
            fn from(g: Literal) -> Self {
                TokenTree::Literal(g)
            }
        }
        /// Prints the token tree as a string that is supposed to be losslessly
        /// convertible back into the same token tree (modulo spans), except for
        /// possibly `TokenTree::Group`s with `Delimiter::None` delimiters and negative
        /// numeric literals.
        impl Display for TokenTree {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    TokenTree::Group(t) => Display::fmt(t, f),
                    TokenTree::Ident(t) => Display::fmt(t, f),
                    TokenTree::Punct(t) => Display::fmt(t, f),
                    TokenTree::Literal(t) => Display::fmt(t, f),
                }
            }
        }
        /// Prints token tree in a form convenient for debugging.
        impl Debug for TokenTree {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                // Each of these has the name in the struct type in the derived debug,
                // so don't bother with an extra layer of indirection
                match self {
                    TokenTree::Group(t) => Debug::fmt(t, f),
                    TokenTree::Ident(t) => {
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
        ///
        /// A `Group` internally contains a `TokenStream` which is surrounded by
        /// `Delimiter`s.
        #[derive(Clone)]
        pub struct Group {
            inner: imp::Group,
        }
        /// Describes how a sequence of token trees is delimited.
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum Delimiter {
            /// `( ... )`
            Parenthesis,
            /// `{ ... }`
            Brace,
            /// `[ ... ]`
            Bracket,
            /// `∅ ... ∅
            /// Invisible delimiters may not survive roundtrip of a token stream through
            /// a string.
            ///
            /// <div class="warning"
            /// Any `None`-delimited groups (re)created by a proc_macro will therefore not preserve
            /// operator priorities as indicated above. The other `Delimiter` variants should be used
            /// instead in this context. This is a rustc bug. For details, see
            /// [rust-lang/rust#67062](https://github.com/rust-lang/rust/issues/67062).
            ///
            /// </div>
            None,
        }

        impl Group {
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
            pub fn delimiter(&self) -> Delimiter {
                self.inner.delimiter()
            }
            /// Returns the `TokenStream` of tokens that are delimited in this `Group`.
            ///
            /// Note that the returned token stream does not include the delimiter
            /// returned above.
            pub fn stream(&self) -> TokenStream {
                TokenStream::_new(self.inner.stream())
            }
            /// Returns the span for the delimiters of this token stream, spanning the
            /// entire `Group`
            /// ```
            pub fn span(&self) -> Span {
                Span::_new(self.inner.span())
            }
            /// Returns the span pointing to the opening delimiter of this group
            /// ```
            pub fn span_open(&self) -> Span {
                Span::_new(self.inner.span_open())
            }
            /// Returns the span pointing to the closing delimiter of this group
            /// ```
            pub fn span_close(&self) -> Span {
                Span::_new(self.inner.span_close())
            }
            /// Returns an object that holds this group's `span_open()` and
            /// `span_close()` together (in a more compact representation than holding
            /// those 2 spans individually).
            pub fn delim_span(&self) -> DelimSpan {
                DelimSpan::new(&self.inner)
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
        impl Display for Group {
            fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, formatter)
            }
        }

        impl Debug for Group {
            fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, formatter)
            }
        }
        /// A `Punct` is a single punctuation character like `+`, `-` or `#`.
        ///
        /// Multicharacter operators like `+=` are represented as two instances of
        /// `Punct` with different forms of `Spacing` returned.
        #[derive(Clone)]
        pub struct Punct {
            ch: char,
            spacing: Spacing,
            span: Span,
        }
        /// Whether a `Punct` is followed immediately by another `Punct` or followed by
        /// another token or whitespace.
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum Spacing {
            /// E.g. `+` is `Alone` in `+ =`, `+ident` or `+()`.
            Alone,
            /// E.g. `+` is `Joint` in `+=` or `'` is `Joint` in `'#`.
            ///
            /// Additionally, single quote `'` can join with identifiers to form
            /// lifetimes `'ident`.
            Joint,
        }

        impl Punct {
            /// Creates a new `Punct` from the given character and spacing.
            ///
            /// The `ch` argument must be a valid punctuation character permitted by the
            /// language, otherwise the function will panic.
            ///
            /// The returned `Punct` will have the default span of `Span::call_site()`
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
            pub fn as_char(&self) -> char {
                self.ch
            }
            /// Returns the spacing of this punctuation character, indicating whether
            /// it's immediately followed by another `Punct` in the token stream, so
            /// they can potentially be combined into a multicharacter operator
            /// (`Joint`), or it's followed by some other token or whitespace (`Alone`)
            /// so the operator has certainly ended.
            pub fn spacing(&self) -> Spacing {
                self.spacing
            }
            /// Returns the span for this punctuation character.
            pub fn span(&self) -> Span {
                self.span
            }
            /// Configure the span for this punctuation character.
            pub fn set_span(&mut self, span: Span) {
                self.span = span;
            }
        }
        /// Prints the punctuation character as a string that should be losslessly
        /// convertible back into the same character.
        impl Display for Punct
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.ch, f)
            }
        }

        impl Debug for Punct
        {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result
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
            _marker: ProcMacroAutoTraits,
        }

        impl Ident
        {
            fn _new(inner: imp::Ident) -> Self {
                Ident {
                    inner,
                    _marker: MARKER,
                }
            }

            fn _new_fallback(inner: fallback::Ident) -> Self {
                Ident {
                    inner: imp::Ident::from(inner),
                    _marker: MARKER,
                }
            }
            /// Creates a new `Ident` with the given `string` as well as the specified `span`
            #[track_caller] pub fn new(string: &str, span: Span) -> Self {
                Ident::_new(imp::Ident::new_checked(string, span.inner))
            }
            /// Same as `Ident::new`, but creates a raw identifier (`r#ident`). The
            /// `string` argument must be a valid identifier permitted by the language
            /// (including keywords, e.g. `fn`). Keywords which are usable in path
            /// segments (e.g. `self`, `super`) are not supported, and will cause a
            /// panic.
            #[track_caller] pub fn new_raw(string: &str, span: Span) -> Self {
                Ident::_new(imp::Ident::new_raw_checked(string, span.inner))
            }
            /// Returns the span of this `Ident`.
            pub fn span(&self) -> Span {
                Span::_new(self.inner.span())
            }
            /// Configures the span of this `Ident`, possibly changing its hygiene
            /// context.
            pub fn set_span(&mut self, span: Span) {
                self.inner.set_span(span.inner);
            }
        }

        impl PartialEq for Ident
        {
            fn eq(&self, other: &Ident) -> bool {
                self.inner == other.inner
            }
        }

        impl<T> PartialEq<T> for Ident where
        T: ?Sized + AsRef<str>,
        {
            fn eq(&self, other: &T) -> bool {
                self.inner == other
            }
        }

        impl Eq for Ident {}

        impl PartialOrd for Ident 
        {
            fn partial_cmp(&self, other: &Ident) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for Ident 
        {
            fn cmp(&self, other: &Ident) -> Ordering {
                self.to_string().cmp(&other.to_string())
            }
        }

        impl Hash for Ident
        {
            fn hash<H: Hasher>(&self, hasher: &mut H) {
                self.to_string().hash(hasher);
            }
        }
        /// Prints the identifier as a string that should be losslessly convertible back into the same identifier.
        impl Display for Ident 
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, f)
            }
        }

        impl Debug for Ident
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
            _marker: ProcMacroAutoTraits,
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
                ///
                /// This function will create an integer like `1` where the integer
                /// value specified is the first part of the token. No suffix is
                /// specified on this token, meaning that invocations like
                /// `Literal::i8_unsuffixed(1)` are equivalent to
                /// `Literal::u32_unsuffixed(1)`. Literals created from negative numbers
                /// may not survive roundtrips through `TokenStream` or strings and may
                /// be broken into two tokens (`-` and positive literal).
                ///
                /// Literals created through this method have the `Span::call_site()`
                /// span by default, which can be configured with the `set_span` method
                /// below.
                pub fn $name(n: $kind) -> Literal {
                    Literal::_new(imp::Literal::$name(n))
                }
            )*)
        }

        impl Literal 
        {
            fn _new(inner: imp::Literal) -> Self {
                Literal {
                    inner,
                    _marker: MARKER,
                }
            }

            fn _new_fallback(inner: fallback::Literal) -> Self {
                Literal {
                    inner: imp::Literal::from(inner),
                    _marker: MARKER,
                }
            }

            suffixed_int_literals! {
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

            unsuffixed_int_literals! {
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

            pub fn f64_unsuffixed(f: f64) -> Literal {
                assert!(f.is_finite());
                Literal::_new(imp::Literal::f64_unsuffixed(f))
            }
            /// Creates a new suffixed floating-point literal

            pub fn f64_suffixed(f: f64) -> Literal {
                assert!(f.is_finite());
                Literal::_new(imp::Literal::f64_suffixed(f))
            }
            /// Creates a new unsuffixed floating-point literal

            pub fn f32_unsuffixed(f: f32) -> Literal {
                assert!(f.is_finite());
                Literal::_new(imp::Literal::f32_unsuffixed(f))
            }
            /// Creates a new suffixed floating-point literal

            pub fn f32_suffixed(f: f32) -> Literal {
                assert!(f.is_finite());
                Literal::_new(imp::Literal::f32_suffixed(f))
            }
            /// String literal.
            pub fn string(string: &str) -> Literal {
                Literal::_new(imp::Literal::string(string))
            }
            /// Character literal.
            pub fn character(ch: char) -> Literal {
                Literal::_new(imp::Literal::character(ch))
            }
            /// Byte character literal.
            pub fn byte_character(byte: u8) -> Literal {
                Literal::_new(imp::Literal::byte_character(byte))
            }
            /// Byte string literal.
            pub fn byte_string(bytes: &[u8]) -> Literal {
                Literal::_new(imp::Literal::byte_string(bytes))
            }
            /// C string literal.
            pub fn c_string(string: &CStr) -> Literal {
                Literal::_new(imp::Literal::c_string(string))
            }
            /// Returns the span encompassing this literal.
            pub fn span(&self) -> Span {
                Span::_new(self.inner.span())
            }
            /// Configures the span associated for this literal.
            pub fn set_span(&mut self, span: Span) {
                self.inner.set_span(span.inner);
            }
            /// Returns a `Span` that is a subset of `self.span()` containing only
            /// the source bytes in range `range`.
            pub fn subspan<R: RangeBounds<usize>>(&self, range: R) -> Option<Span> {
                self.inner.subspan(range).map(Span::_new)
            }

            #[doc(hidden)]
            pub unsafe fn from_str_unchecked(repr: &str) -> Self {
                Literal::_new(unsafe { imp::Literal::from_str_unchecked(repr) })
            }
        }

        impl FromStr for Literal 
        {
            type Err = LexError;

            fn from_str(repr: &str) -> Result<Self, LexError> {
                match imp::Literal::from_str_checked(repr) {
                    Ok(lit) => Ok(Literal::_new(lit)),
                    Err(lex) => Err(LexError {
                        inner: lex,
                        _marker: MARKER,
                    }),
                }
            }
        }

        impl Debug for Literal 
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.inner, f)
            }
        }

        impl Display for Literal 
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Display::fmt(&self.inner, f)
            }
        }
        /// Public implementation details for the `TokenStream` type, such as iterators.
        pub mod token_stream 
        {
            use ::fmt::{self, Debug};
            use ::marker::{ProcMacroAutoTraits, MARKER};
            use ::process::macros::{imp, TokenTree};
            pub use ::process::macros::TokenStream;

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

                fn next(&mut self) -> Option<TokenTree>
                {
                    self.inner.next()
                }

                fn size_hint(&self) -> (usize, Option<usize>)
                {
                    self.inner.size_hint()
                }
            }

            impl Debug for IntoIter
            {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
                {
                    f.write_str("TokenStream ")?;
                    f.debug_list().entries(self.clone()).finish()
                }
            }

            impl IntoIterator for TokenStream
            {
                type Item = TokenTree;
                type IntoIter = IntoIter;

                fn into_iter(self) -> IntoIter
                {
                    IntoIter {
                        inner: self.inner.into_iter(),
                        _marker: MARKER,
                    }
                }
            }
        }
    }
}

pub mod ptr
{
    pub use std::ptr::{ * };
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
// 03003 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
