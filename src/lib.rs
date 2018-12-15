//! Simple code generator for manual parsing
//!
//! ## Attributes
//!
//! ### Struct
//!
//! There are several attributes that control behaviour of parser
//! Each, attached to struct's field
//!
//! - `starts_with = <prefix>` - Specifies string with which next parse step should start(can be stacked). Errors if prefix is missing.
//! - `ends_with = <prefix>` - Specifies string with which parse step should end(can be stacked). Errors if suffix is missing. If empty, expects EOF.
//! - `skip = <chars>` - Specifies to skip characters, until not meeting character outside of specified in string.
//! - `skip(ws)` - Specifies to skip all white space characters.
//! - `format(<format>)` - Specifies list of characters that should contain value to parse from.
//! - `format(not(<format>))` - Specifies list of characters that should contain value to parse from.
//!
//! ##### Formats
//!
//! - Literal string - When string is specified as argument to `format`, it is used as set of characters.
//! - `numeric` - When specified, match using `char::is_numeric()`
//! - `digit(<base>)` - When specified, match using `char::is_digit(<base>)`
//! - `ascii` - When specified, match using `char::is_ascii()`
//! - `alphabetic` - When specified, match using `char::is_alphabetic()`
//!
//! ### Enum
//!
//! - `format = <format>` - Specifies string to match against.
//! - `case` - Specifies case sensitive match. By default it is insensitive.
//! - `default` - Specifies variant as taking default value. Should take only single `String` and
//! there can be only one
//!
//! ## Usage
//!
//! ### Struct
//!
//! ```rust
//! use std::str::FromStr;
//!
//! #[derive(banjin::Parser)]
//! pub struct Data {
//!     #[starts_with = "prefix"]
//!     #[skip(ws)]
//!     #[starts_with = "+"]
//!     #[skip(ws)]
//!     #[format(ascii)]
//!     #[format(digit(10))]
//!     pub first: u32,
//!     #[skip(ws)]
//!     #[format(not("d"))]
//!     #[format("13")]
//!     #[ends_with = "d"]
//!     #[ends_with = ""]
//!     pub second: String,
//! }
//!
//! fn main() {
//!     let data = Data::from_str("prefix + 666   13d").expect("Parse");
//!     assert_eq!(data.first, 666);
//!     assert_eq!(data.second, "13");
//!
//!     let data = Data::from_str("prefix + 666   13");
//!     assert!(data.is_err());
//!
//!     let data = Data::from_str("prefix 666   13d");
//!     assert!(data.is_err());
//!
//!     let data = Data::from_str("prefix + 666   13dg");
//!     assert!(data.is_err());
//!
//!     let data = Data::from_str("");
//!     assert!(data.is_err());
//! }
//!
//! ```
//!
//! ### Enum
//!
//! ```rust
//! use std::str::FromStr;
//!
//! #[derive(banjin::Parser, PartialEq, Eq, Debug)]
//! enum Gender {
//!     Male,
//!     #[case]
//!     Female,
//!     #[default]
//!     Other(String)
//! }
//!
//! fn main() {
//!     let gender = Gender::from_str("male").expect("Parse");
//!     assert_eq!(gender, Gender::Male);
//!
//!     let gender = Gender::from_str("female").expect("Parse");
//!     match gender {
//!         Gender::Other(text) => assert_eq!(text, "female"),
//!         _ => panic!("Unexpected!")
//!     }
//!
//!     let gender = Gender::from_str("none").expect("Parse");
//!     match gender {
//!         Gender::Other(text) => assert_eq!(text, "none"),
//!         _ => panic!("Unexpected!")
//!     }
//! }
//! ```
extern crate proc_macro;

use proc_macro::TokenStream;

#[cfg(feature = "no_std")]
const FROM_STR: &'static str = "core::str::FromStr";
#[cfg(not(feature = "no_std"))]
const FROM_STR: &'static str = "std::str::FromStr";

mod from_struct;
mod from_enum;

fn generate(ast: syn::DeriveInput) -> String {
    match ast.data {
        syn::Data::Struct(ref data) => from_struct::parse(&ast, data),
        syn::Data::Enum(ref data) => from_enum::parse(&ast, data),
        _ => panic!("derive(Parser) is available for structs only"),
    }
}

#[proc_macro_derive(Parser, attributes(skip, format, starts_with, ends_with, case, default))]
pub fn parser_derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    generate(ast).parse().expect("To parse generate")
}
