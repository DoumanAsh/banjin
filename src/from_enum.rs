use super::FROM_STR;

use quote::quote;
use std::fmt::Write;

struct FormatData {
    name: String,
    is_case: bool,
    format: String,
}

struct FormatBuilder {
    name: String,
    //The field to take value if no match in others
    //Should be only one
    default: Option<String>,
    fields: Vec<FormatData>,

}

impl FormatBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            default: None,
            fields: Vec::with_capacity(2)
        }
    }

    pub fn add_field(&mut self, variant: &syn::Variant) {
        let mut variant_name_match = None;
        let mut is_default = false;
        let mut is_case = false;

        for meta in variant.attrs.iter().filter_map(|attr| attr.interpret_meta()) {
            if meta.name() == "default" {
                is_default = true;
            } else if meta.name() == "case" {
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

        match variant.fields {
            syn::Fields::Unit => match is_default {
                true => panic!("Default field '{}' needs to have 1 argument", variant.ident),
                false => (),
            },
            syn::Fields::Unnamed(ref fields) if is_default => {
                let field = if let Some(ref default) = self.default.as_ref() {
                    panic!("Second default field '{}' is specified. First one is '{}'", variant.ident, default);
                } else if fields.unnamed.len() > 1 {
                    panic!("Default field '{}' takes more than 1 argument", variant.ident);
                } else if let Some(field) = fields.unnamed.first() {
                    field
                } else {
                    panic!("Default field '{}' needs to have 1 argument", variant.ident);
                };

                let field = match field {
                    syn::punctuated::Pair::End(field) => field,
                    syn::punctuated::Pair::Punctuated(_, _) => panic!("Default field '{}' takes more than 1 argument", variant.ident),
                };

                match field.ty {
                    syn::Type::Path(ref path) => match path.path.is_ident("String") {
                        true => (),
                        false => panic!("Default field '{}' takes non-String", variant.ident),
                    },
                    _ => panic!("Default field '{}' takes non-String", variant.ident),
                }

                self.default = Some(format!("{}", variant.ident));
                return;
            },
            _ => panic!("Field '{}' is non-unit. Not supported!", variant.ident),
        }

        let format = match variant_name_match {
            Some(name) => name,
            _ => format!("{}", variant.ident),
        };

        self.fields.push(FormatData {
            name: format!("{}", variant.ident),
            is_case,
            format
        });
    }

    fn write_from_str(&self, buf: &mut String) {
        let _ = match self.default.is_some() {
            false => writeln!(buf, "\ttype Err = &'static str;\n"),
            true => writeln!(buf, "\ttype Err = ();\n"),
        };

        let _ = writeln!(buf, "\tfn from_str(text: &str) -> Result<Self, Self::Err> {{");

        for (idx, field) in self.fields.iter().enumerate() {
            let _ = match idx {
                0 => write!(buf, "\t\t"),
                _ => write!(buf, " "),
            };

            let _ = match field.is_case {
                true => write!(buf, "if text == \"{2}\" {{ Ok({1}::{0}) }}\n\t\telse", field.name, self.name, field.format),
                false => write!(buf, "if text.eq_ignore_ascii_case(\"{2}\") {{ Ok({1}::{0}) }}\n\t\telse", field.name, self.name, field.format),
            };
        }

        let _ = match self.default.as_ref() {
            Some(name) => writeln!(buf, " {{ Ok({1}::{0}(text.to_owned())) }}", name, self.name),
            None => writeln!(buf, " {{ Err(\"Unknown variant\") }}"),
        };

        let _ = writeln!(buf, "\t}}");
        let _ = writeln!(buf, "\n}}");
    }
}

pub fn parse(ast: &syn::DeriveInput, payload: &syn::DataEnum) -> String {
    let mut buf = String::new();

    let (impl_gen, type_gen, where_clause) = ast.generics.split_for_impl();

    let mut builder = FormatBuilder::new(format!("{}", ast.ident));

    let _ = writeln!(buf, "{} {} for {}{} {{", quote!(impl#impl_gen), FROM_STR, ast.ident, quote!(#type_gen #where_clause));

    for variant in payload.variants.iter() {
        builder.add_field(variant);
    }

    builder.write_from_str(&mut buf);

    println!("{}", buf);

    buf
}

