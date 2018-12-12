# banjin

[![Build Status](https://travis-ci.org/DoumanAsh/banjin?branch=master)](https://travis-ci.org/DoumanAsh/banjin)
[![Crates.io](https://img.shields.io/crates/v/banjin.svg)](https://crates.io/crates/banjin)
[![Documentation](https://docs.rs/banjin/badge.svg)](https://docs.rs/crate/banjin/)
[![dependency status](https://deps.rs/repo/github/DoumanAsh/banjin/status.svg)](https://deps.rs/repo/github/DoumanAsh/banjin)

Simple code generator for string parsing

## Attributes

There are several attributes that control behaviour of parser
Each, attached to struct's field

- `starts_with = <prefix>` - Specifies string with which next parse step should start(can be stacked). Errors if prefix is missing.
- `ends_with = <prefix>` - Specifies string with which parse step should end(can be stacked). Errors if suffix is missing. If empty, expects EOF.
- `skip = <chars>` - Specifies to skip, until not meeting character outside of specified.
- `skip(ws)` - Specifies to skip all white space characters.
- `format(<format>)` - Specifies list of characters that should contain value to parse from.
- `format(not(<format>))` - Specifies list of characters that should contain value to parse from.

## Formats

- Literal string - When string is specified as argument to `format`, it is used as set of characters.
- `numeric` - When specified, match using `char::is_numeric()`
- `digit(<base>)` - When specified, match using `char::is_digit(<base>)`
- `ascii` - When specified, match using `char::is_ascii()`
- 'alphabetic' - When specified, match using `char::is_alphabetic()`

## Usage

```rust
use std::str::FromStr;

#[derive(banjin::Parser)]
pub struct Data {
    #[starts_with = "prefix"]
    #[skip(ws)]
    #[starts_with = "+"]
    #[skip(ws)]
    #[format(ascii)]
    #[format(digit(10))]
    pub first: u32,
    #[skip(ws)]
    #[format(not("d"))]
    #[format("13")]
    #[ends_with = "d"]
    #[ends_with = ""]
    pub second: String,
}

fn main() {
    let data = Data::from_str("prefix + 666   13d").expect("Parse");
    assert_eq!(data.first, 666);
    assert_eq!(data.second, "13");

    let data = Data::from_str("prefix + 666   13");
    assert!(data.is_err());

    let data = Data::from_str("prefix 666   13d");
    assert!(data.is_err());

    let data = Data::from_str("prefix + 666   13dg");
    assert!(data.is_err());

    let data = Data::from_str("");
    assert!(data.is_err());
}

```
