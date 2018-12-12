//! Simple code generator for manual parsing
//!
//! ## Attributes
//!
//! There are several attributes that control behaviour of parser
//! Each, attached to struct's field
//!
//! - `starts_with = <prefix>` - Specifies string with which next parse step should start(can be stacked). Errors if prefix is missing.
//! - `ends_with = <prefix>` - Specifies string with which parse step should end(can be stacked). Errors if suffix is missing. If empty, expects EOF.
//! - `skip = <chars>` - Specifies to skip, until not meeting character outside of specified.
//! - `skip(ws)` - Specifies to skip all white space characters.
//! - `format(<chars>)` - Specifies list of characters that should contain value to parse from.
//! - `format(not(<chars>))` - Specifies list of characters that should contain value to parse from.
//!
//! ## Usage
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
//!     #[format("1234567890")]
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
extern crate proc_macro;
extern crate syn;
extern crate quote;

use std::fmt::Write;

use proc_macro::TokenStream;
use quote::quote;

enum FormatData {
    Str(String)
}

impl FormatData {
    fn write(self, buf: &mut String) {
        match self {
            FormatData::Str(format) => for (idx, ch) in format.chars().enumerate() {
                if idx > 0 {
                    let _ = write!(buf, " || ");
                }
                let _ = write!(buf, "ch == '{}'", ch);
            }
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

    fn parse(&mut self, meta: syn::Meta) {
        match meta {
            syn::Meta::List(meta) => {
                let arg = match meta.nested.first().expect("'format' needs at least one attribute") {
                    syn::punctuated::Pair::Punctuated(_, _) => panic!("'format' has more than one attribute"),
                    syn::punctuated::Pair::End(arg) => arg,
                };

                match arg {
                    syn::NestedMeta::Meta(arg) => match arg.name() == "not" {
                        true => match arg {
                            syn::Meta::List(arg) => match arg.nested.first().expect("format(not(...))' must be string literal") {
                                syn::punctuated::Pair::Punctuated(_, _) => panic!("'format(not(...))' has more than one attribute"),
                                syn::punctuated::Pair::End(arg) => match arg {
                                    syn::NestedMeta::Literal(syn::Lit::Str(text)) => self.inner.push(Format::Neg(text.value().into())),
                                    _ => panic!("'format(not(...))' must be string literal"),
                                },
                            },
                            _ => panic!("'format(not(...))' must be string literal"),
                        },
                        false => panic!("'format' accepts only 'not' modifier")
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
                let _ = write!(buf, " || ");
            }

            let (format, is_negative) = match format {
                Format::Pos(format) => (format, false),
                Format::Neg(format) => (format, true),
            };

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
            let _ = writeln!(buf, "\t\tif text.len() > 0 {{ return Err(\"Expected input to end after '{}', but there some remains\"); }}", variable_name);
        } else {
            let _ = writeln!(buf, "\t\tif !text.starts_with(\"{0}\") {{ return Err(\"Expected suffix '{0}' after '{1}', but none is found\"); }}", end, variable_name);
            let _ = writeln!(buf, "\t\ttext = &text[{}..];", end.len());
        }
    }
}

fn generate(ast: syn::DeriveInput) -> String {
    let mut buf = String::new();

    let payload = match ast.data {
        syn::Data::Struct(data) => data,
        _ => panic!("derive(Parser) is available for structs only"),
    };

    let (impl_gen, type_gen, where_clause) = ast.generics.split_for_impl();

    #[cfg(feature = "no_std")]
    let target = "core::str::FromStr";
    #[cfg(not(feature = "no_std"))]
    let target = "std::str::FromStr";

    let _ = writeln!(buf, "{} {} for {}{} {{", quote!(impl#impl_gen), target, ast.ident, quote!(#type_gen #where_clause));
    let _ = writeln!(buf, "\ttype Err = &'static str;\n");
    let _ = writeln!(buf, "\tfn from_str(text: &str) -> Result<Self, Self::Err> {{");
    let _ = writeln!(buf, "\t\tuse {};\n", target);
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

    println!("{}", buf);

    buf
}

#[proc_macro_derive(Parser, attributes(skip, format, starts_with, ends_with))]
pub fn parser_derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    generate(ast).parse().expect("To parse generate")
}
