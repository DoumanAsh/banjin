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
//!     #[format = "None"]
//!     Other
//! }
//!
//! fn main() {
//!     let gender = Gender::from_str("male").expect("Parse");
//!     assert_eq!(gender, Gender::Male);
//!
//!     let gender = Gender::from_str("female");
//!     assert!(gender.is_err());
//!
//!     let gender = Gender::from_str("none").expect("Parse");
//!     assert_eq!(gender, Gender::Other);
//! }
//! ```
extern crate proc_macro;
extern crate syn;
extern crate quote;

use std::fmt::Write;

use proc_macro::TokenStream;
use quote::quote;

#[cfg(feature = "no_std")]
const FROM_STR: &'static str = "core::str::FromStr";
#[cfg(not(feature = "no_std"))]
const FROM_STR: &'static str = "std::str::FromStr";

enum FormatData {
    Str(String),
    Numeric,
    Digit(u64),
    Ascii,
    Alphabetic,
}

impl FormatData {
    fn is_empty(&self) -> bool {
        match self {
            FormatData::Str(ref format) => format.len() == 0,
            FormatData::Numeric => false,
            FormatData::Digit(_) => false,
            FormatData::Ascii => false,
            FormatData::Alphabetic => false,
        }
    }

    fn write(self, buf: &mut String) {
        match self {
            FormatData::Str(format) => for (idx, ch) in format.chars().enumerate() {
                if idx > 0 {
                    let _ = write!(buf, " || ");
                }
                let _ = write!(buf, "ch == '{}'", ch);
            },
            FormatData::Numeric => {
                let _ = write!(buf, "ch.is_numeric()");
            },
            FormatData::Digit(base) => {
                let _ = write!(buf, "ch.is_digit({})",  base);
            },
            FormatData::Ascii => {
                let _ = write!(buf, "ch.is_ascii()");
            },
            FormatData::Alphabetic => {
                let _ = write!(buf, "ch.is_alphabetic()");
            },
        }
    }
}

impl Into<FormatData> for String {
    fn into(self) -> FormatData {
        FormatData::Str(self)
    }
}

enum Format {
    Pos(FormatData),
    Neg(FormatData),
}

struct FormatBuilder {
    inner: Vec<Format>
}

impl FormatBuilder {
    fn new() -> Self {
        Self {
            inner: Vec::with_capacity(1),
        }
    }

    fn parse_digit_meta(meta: &syn::Meta) -> FormatData {
        match meta {
            syn::Meta::List(arg) => match arg.nested.first().expect("'format(digit(...))' must be with argument") {
                syn::punctuated::Pair::Punctuated(_, _) => panic!("'format(digit(...))' has more than one attribute"),
                syn::punctuated::Pair::End(arg) => match arg {
                    syn::NestedMeta::Literal(syn::Lit::Int(int)) => if int.value() > 36 {
                        panic!("'format(digit(...)) cannot be larger than 36");
                    } else {
                        FormatData::Digit(int.value())
                    },
                    _ => panic!("'format(digit(...))' expects integer argument"),
                }
            },
            _ => panic!("'format(digit)` expects single argument")
        }
    }

    fn parse(&mut self, meta: syn::Meta) {
        match meta {
            syn::Meta::List(meta) => {
                let arg = match meta.nested.first().expect("'format' needs at least one attribute") {
                    syn::punctuated::Pair::Punctuated(_, _) => panic!("'format' has more than one attribute"),
                    syn::punctuated::Pair::End(arg) => arg,
                };

                match arg {
                    syn::NestedMeta::Meta(arg) => {
                        if arg.name() == "not" {
                            match arg {
                                syn::Meta::List(arg) => match arg.nested.first().expect("format(not(...))' must be with argument") {
                                    syn::punctuated::Pair::Punctuated(_, _) => panic!("'format(not(...))' has more than one attribute"),
                                    syn::punctuated::Pair::End(arg) => match arg {
                                        syn::NestedMeta::Literal(syn::Lit::Str(text)) => self.inner.push(Format::Neg(text.value().into())),
                                        syn::NestedMeta::Meta(meta) => if meta.name() == "numeric" {
                                            self.inner.push(Format::Neg(FormatData::Numeric))
                                        } else if meta.name() == "digit" {
                                            let res = Self::parse_digit_meta(meta);
                                            self.inner.push(Format::Neg(res));
                                        } else if meta.name() == "ascii" {
                                            self.inner.push(Format::Neg(FormatData::Ascii))
                                        } else if meta.name() == "alphabetic" {
                                            self.inner.push(Format::Neg(FormatData::Alphabetic))
                                        } else {
                                            panic!("'format(not(...))' doesn't accept {}", meta.name());
                                        },
                                        _ => panic!("'format(not(...))' is used with wrong syntax"),
                                    },
                                },
                                _ => panic!("'format(not(...))' requires value"),
                            }
                        } else if arg.name() == "numeric" {
                            match arg {
                                syn::Meta::Word(_) => self.inner.push(Format::Pos(FormatData::Numeric)),
                                _ => panic!("'format(numeric)` doesn't accept any argument")
                            }
                        } else if arg.name() == "digit" {
                            let res = Self::parse_digit_meta(arg);
                            self.inner.push(Format::Pos(res));
                        } else if arg.name() == "ascii" {
                            match arg {
                                syn::Meta::Word(_) => self.inner.push(Format::Pos(FormatData::Ascii)),
                                _ => panic!("'format(ascii)` doesn't accept any argument")
                            }
                        } else if arg.name() == "alphabetic" {
                            match arg {
                                syn::Meta::Word(_) => self.inner.push(Format::Pos(FormatData::Alphabetic)),
                                _ => panic!("'format(alphabetic)` doesn't accept any argument")
                            }
                        } else {
                            panic!("'format' doesn't understand '{}'", arg.name())
                        }
                    },
                    syn::NestedMeta::Literal(lit) => match lit {
                        syn::Lit::Str(text) => self.inner.push(Format::Pos(text.value().into())),
                        _ => panic!("'format' requires string argument"),
                    }
                }

            },
            syn::Meta::NameValue(meta) => match meta.lit {
                syn::Lit::Str(text) => self.inner.push(Format::Pos(text.value().into())),
                _ => panic!("'format' requires string argument"),
            }
            _ => panic!("'format' attribute is invalid, should be list or value"),
        }
    }

    fn write(mut self, buf: &mut String, variable_name: &syn::Ident) {
        let _ = write!(buf, "\t\tlet variable_len = text.chars().take_while(|&ch| ");

        for (idx, format) in self.inner.drain(..).enumerate() {
            if idx > 0 {
                let _ = write!(buf, " && ");
            }

            let (format, is_negative) = match format {
                Format::Pos(format) => (format, false),
                Format::Neg(format) => (format, true),
            };

            if format.is_empty() {
                panic!("Empty 'format' at index {} for field '{}'", idx, variable_name);
            }

            if is_negative {
                let _ = write!(buf, "!(");
            }

            format.write(buf);

            if is_negative {
                let _ = write!(buf, ")");
            }

        }

        let _ = writeln!(buf, ").count();");
        let _ = writeln!(buf, "\t\tlet {} = FromStr::from_str(&text[..variable_len]).map_err(|_| \"Cannot parse '{0}'\")?;\n", variable_name);
        let _ = writeln!(buf, "\t\ttext = &text[variable_len..];\n");
    }

    fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }
}

fn write_field(buf: &mut String, field: &syn::Field) {
    let mut format = FormatBuilder::new();

    let mut ends_with = Vec::new();
    let variable_name = field.ident.as_ref().expect("Named field is needed");

    for meta in field.attrs.iter().filter_map(|attr| attr.interpret_meta()) {
        if meta.name() == "skip" {
            match meta {
                syn::Meta::List(meta) => {
                    let arg = match meta.nested.first().expect("'skip' needs at least one attribute") {
                        syn::punctuated::Pair::Punctuated(_, _) => panic!("'skip' has more than one attribute"),
                        syn::punctuated::Pair::End(syn::NestedMeta::Meta(arg)) => arg.name(),
                        syn::punctuated::Pair::End(syn::NestedMeta::Literal(_)) => panic!("'skip' expects meta item, not literal"),
                    };

                    if arg == "ws" {
                        let _ = writeln!(buf, "\t\ttext = text.trim_start();");
                    } else {
                        panic!("'skip' doesn't know attribute '{}'", arg);
                    }
                },
                syn::Meta::NameValue(meta) => match meta.lit {
                    syn::Lit::Str(format) => {
                        let _ = write!(buf, "\t\tlet skip_count = text.chars().take_while(|&ch| ");
                        for ch in format.value().chars() {
                            let _ = write!(buf, "ch == '{}' || ", ch);
                        }

                        buf.pop();
                        buf.pop();
                        buf.pop();

                        let _ = writeln!(buf, ").count();");
                        let _ = writeln!(buf, "\t\ttext = &text[skip_count..];\n");

                    },
                    _ => panic!("'skip' requires string argument"),
                }
                _ => panic!("'skip' attribute is invalid, should be name with value"),
            }
        } else if meta.name() == "starts_with" {
            match meta {
                syn::Meta::NameValue(meta) => match meta.lit {
                    syn::Lit::Str(start) => {
                        let start = start.value();

                        if start.len() == 0 {
                            panic!("'starts_with' specified with empty value. It makes no sense, onii-san!");
                        }

                        let _ = writeln!(buf, "\t\tif !text.starts_with(\"{0}\") {{ return Err(\"Expected prefix '{0}', but none is found\"); }}", start);
                        let _ = writeln!(buf, "\t\ttext = &text[{}..];", start.len());
                    }
                    _ => panic!("'starts_with' requires string argument"),
                },
                _ => panic!("'starts_with' requires string argument"),
            }
        } else if meta.name() == "ends_with" {
            match meta {
                syn::Meta::NameValue(meta) => match meta.lit {
                    syn::Lit::Str(end) => {
                        ends_with.push(end.value());
                    }
                    _ => panic!("'ends_with' requires string argument"),
                },
                _ => panic!("'ends_with' requires string argument"),
            }
        } else if meta.name() == "format" {
            format.parse(meta);
        }
    }

    if format.is_empty() {
        panic!("'format' is missing, do not know how to parse '{}'", variable_name)
    }

    format.write(buf, &variable_name);

    for end in ends_with {
        if end.len() == 0 {
            let _ = writeln!(buf, "\t\tif text.len() > 0 {{ return Err(\"Expected input to end after '{}', but some more remains\"); }}", variable_name);
        } else {
            let _ = writeln!(buf, "\t\tif !text.starts_with(\"{0}\") {{ return Err(\"Expected suffix '{0}' after '{1}', but none is found\"); }}", end, variable_name);
            let _ = writeln!(buf, "\t\ttext = &text[{}..];", end.len());
        }
    }
}

fn from_struct(ast: &syn::DeriveInput, payload: &syn::DataStruct) -> String {
    let mut buf = String::new();
    let (impl_gen, type_gen, where_clause) = ast.generics.split_for_impl();

    let _ = writeln!(buf, "{} {} for {}{} {{", quote!(impl#impl_gen), FROM_STR, ast.ident, quote!(#type_gen #where_clause));
    let _ = writeln!(buf, "\ttype Err = &'static str;\n");
    let _ = writeln!(buf, "\tfn from_str(text: &str) -> Result<Self, Self::Err> {{");
    let _ = writeln!(buf, "\t\tuse {};\n", FROM_STR);
    let _ = writeln!(buf, "\t\tlet mut text = text;\n");

    for field in payload.fields.iter() {
        write_field(&mut buf, field);
    }

    let _ = writeln!(buf, "\t\tOk(Self {{");
    for field in payload.fields.iter() {
        let _ = writeln!(buf, "\t\t\t{},", field.ident.as_ref().expect("Named field"));
    }
    let _ = writeln!(buf, "\t\t}})");

    let _ = writeln!(buf, "\t}}");
    let _ = writeln!(buf, "\n}}");

    buf

}

fn from_enum(ast: &syn::DeriveInput, payload: &syn::DataEnum) -> String {
    let mut buf = String::new();

    let (impl_gen, type_gen, where_clause) = ast.generics.split_for_impl();

    let _ = writeln!(buf, "{} {} for {}{} {{", quote!(impl#impl_gen), FROM_STR, ast.ident, quote!(#type_gen #where_clause));
    let _ = writeln!(buf, "\ttype Err = &'static str;\n");
    let _ = writeln!(buf, "\tfn from_str(text: &str) -> Result<Self, Self::Err> {{");

    for (idx, variant) in payload.variants.iter().enumerate() {
        let mut variant_name_match = None;
        let mut is_case = false;

        for meta in variant.attrs.iter().filter_map(|attr| attr.interpret_meta()) {
            if meta.name() == "case" {
                is_case = true;
            } else if meta.name() == "format" {
                match meta {
                    syn::Meta::NameValue(meta) => match meta.lit {
                        syn::Lit::Str(start) => {
                            variant_name_match = Some(start.value());
                        }
                        _ => panic!("'format' requires string argument"),
                    },
                    _ => panic!("'format' requires string argument"),
                }
            }
        }

        let variant_name_match = match variant_name_match {
            Some(name) => name,
            _ => format!("{}", variant.ident),
        };

        match variant.fields {
            syn::Fields::Unit => (),
            _ => panic!("Field '{}' is non-unit. Not supported!", variant.ident),
        }

        if idx == 0 {
            let _ = write!(buf, "\t\t");
        } else {
            let _ = write!(buf, " ");
        }

        let _ = match is_case {
            true => write!(buf, "if text == \"{2}\" {{ Ok({1}::{0}) }}\n\t\telse", variant.ident, ast.ident, variant_name_match),
            false => write!(buf, "if text.eq_ignore_ascii_case(\"{2}\") {{ Ok({1}::{0}) }}\n\t\telse", variant.ident, ast.ident, variant_name_match),
        };
    }

    let _ = writeln!(buf, "{{ Err(\"Unknown variant\") }}");

    let _ = writeln!(buf, "\t}}");
    let _ = writeln!(buf, "\n}}");

    println!("{}", buf);

    buf
}

fn generate(ast: syn::DeriveInput) -> String {
    match ast.data {
        syn::Data::Struct(ref data) => from_struct(&ast, data),
        syn::Data::Enum(ref data) => from_enum(&ast, data),
        _ => panic!("derive(Parser) is available for structs only"),
    }
}

#[proc_macro_derive(Parser, attributes(skip, format, starts_with, ends_with, case))]
pub fn parser_derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    generate(ast).parse().expect("To parse generate")
}
