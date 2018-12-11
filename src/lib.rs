//! Simple code generator for manual parsing
//!
//! ## Attributes
//!
//! There are several attributes that control behaviour of parser
//! Each, attached to struct's field
//!
//! - `skip = <chars>` - Specifies to skip, until not meeting character outside of specified.
//! - `skip(ws)` - Specifies to skip all white space characters.
//! - `format(<chars>)` - Specifies list of characters that should contain value to parse from.
//!
//! ## Usage
//!
//! ```rust
//! use std::str::FromStr;
//!
//! #[derive(banjin::Parser)]
//! pub struct Data {
//!     #[skip = "prefix"]
//!     #[format("1234567890")]
//!     pub first: u32,
//!     #[skip(ws)]
//!     #[format(not("d"))]
//!     pub second: String,
//! }
//!
//! fn main() {
//!
//!     let data = Data::from_str("ppp666   13d").expect("Parse");
//!     assert_eq!(data.first, 666);
//!     assert_eq!(data.second, "13");
//! }
//!
//! ```
extern crate proc_macro;
extern crate syn;
extern crate quote;

use std::fmt::Write;

use proc_macro::TokenStream;
use quote::quote;

enum Format {
    Pos(String),
    Neg(String),
}

fn write_field(buf: &mut String, field: &syn::Field) {
    let mut format = None;

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
            };
        } else if meta.name() == "format" {
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
                                        syn::NestedMeta::Literal(syn::Lit::Str(text)) => match format.is_some() {
                                            true => panic!("Multiple 'format' attributes!"),
                                            false => format = Some(Format::Neg(text.value())),
                                        },
                                        _ => panic!("'format(not(...))' must be string literal"),
                                    },
                                },
                                _ => panic!("'format(not(...))' must be string literal"),
                            },
                            false => panic!("'format' accepts only 'not' modifier")
                        },
                        syn::NestedMeta::Literal(lit) => match lit {
                            syn::Lit::Str(text) => match format.is_some() {
                                true => panic!("Multiple 'format' attributes!"),
                                false => format = Some(Format::Pos(text.value())),
                            },
                            _ => panic!("'format' requires string argument"),
                        }
                    }

                },
                syn::Meta::NameValue(meta) => match meta.lit {
                    syn::Lit::Str(text) => match format.is_some() {
                        true => panic!("Multiple 'format' attributes!"),
                        false => format = Some(Format::Pos(text.value())),
                    },
                    _ => panic!("'format' requires string argument"),
                }
                _ => panic!("'format' attribute is invalid, should be list or value"),
            }
        }
    }

    let (is_negative, format) = match format {
        None => panic!("'format' is missing, do not know how to parse '{}'", variable_name),
        Some(Format::Pos(format)) => (false, format),
        Some(Format::Neg(format)) => (true, format),
    };

    if format.len() == 0 {
        panic!("Format cannot be empty");
    }

    let _ = write!(buf, "\t\tlet variable_len = text.chars().take_while(|&ch| ");
    if is_negative {
        let _ = write!(buf, "!(");
    }

    for ch in format.chars() {
        let _ = write!(buf, "ch == '{}' || ", ch);
    }

    buf.pop();
    buf.pop();
    buf.pop();

    if is_negative {
        let _ = write!(buf, ")");
    }

    let _ = writeln!(buf, ").count();");
    let _ = writeln!(buf, "\t\tlet {} = FromStr::from_str(&text[..variable_len]).map_err(|_| \"Cannot parse '{0}'\")?;\n", variable_name);
    let _ = writeln!(buf, "\t\ttext = &text[variable_len..];\n");
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

#[proc_macro_derive(Parser, attributes(skip, format))]
pub fn parser_derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    generate(ast).parse().expect("To parse generate")
}
