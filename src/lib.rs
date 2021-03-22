use std::{fmt, str::FromStr};

use serde::{de, ser, Deserialize};

use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("{0}")]
    Message(String),

    #[error("cannot borrow deserialized data from NestedMeta")]
    BorrowingNotSupported,

    #[error("unexepected end of input")]
    EOF,

    #[error("unparsed content found at end of input")]
    TrailingItems,

    #[error("expected a boolean value")]
    ExpectedBoolean,

    #[error("expected a character value")]
    ExpectedChar,

    #[error("expected a floating point value")]
    ExpectedFloat,

    #[error("expected an integer value")]
    ExpectedInteger,

    #[error("expected a string")]
    ExpectedString,
}

impl ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Deserializer<'a> {
    meta: &'a [syn::NestedMeta],
    pos: usize,
}

impl<'a> Deserializer<'a> {
    pub fn new(meta: &'a [syn::NestedMeta]) -> Self {
        Self { meta, pos: 0 }
    }

    fn is_complete(&self) -> bool {
        self.meta.len() == self.pos
    }

    fn peek_meta(&mut self) -> Result<&syn::NestedMeta> {
        if let Some(next_meta) = self.meta[self.pos..].first() {
            Ok(next_meta)
        } else {
            Err(Error::EOF)
        }
    }

    fn pop_next(&mut self) -> Result<()> {
        self.pos += 1;
        Ok(())
    }

    fn parse_bool(&mut self) -> Result<bool> {
        if let syn::NestedMeta::Lit(syn::Lit::Bool(syn::LitBool { value, .. })) =
            *self.peek_meta()?
        {
            self.pop_next()?;
            Ok(value)
        } else {
            Err(Error::ExpectedBoolean)
        }
    }

    fn parse_integer<N>(&mut self) -> Result<N>
    where
        N: FromStr,
        N::Err: fmt::Display,
    {
        if let syn::NestedMeta::Lit(syn::Lit::Int(i)) = self.peek_meta()? {
            let value: N = i.base10_parse().map_err(|_| Error::ExpectedInteger)?;
            self.pop_next()?;
            Ok(value)
        } else {
            Err(Error::ExpectedInteger)
        }
    }

    fn parse_float<N>(&mut self) -> Result<N>
    where
        N: FromStr,
        N::Err: fmt::Display,
    {
        if let syn::NestedMeta::Lit(syn::Lit::Float(f)) = self.peek_meta()? {
            let value: N = f.base10_parse().map_err(|_| Error::ExpectedFloat)?;
            self.pop_next()?;
            Ok(value)
        } else {
            Err(Error::ExpectedFloat)
        }
    }

    fn parse_char(&mut self) -> Result<char> {
        if let syn::NestedMeta::Lit(syn::Lit::Char(c)) = self.peek_meta()? {
            let value = c.value();
            self.pop_next()?;
            Ok(value)
        } else {
            Err(Error::ExpectedChar)
        }
    }

    fn parse_string(&mut self) -> Result<String> {
        if let syn::NestedMeta::Lit(syn::Lit::Str(s)) = self.peek_meta()? {
            let value = s.value();
            self.pop_next()?;
            Ok(value)
        } else {
            Err(Error::ExpectedString)
        }
    }
}

pub fn from_nested_meta<'a, T>(meta: &'a [syn::NestedMeta]) -> Result<T>
where
    T: Deserialize<'a>,
{
    let mut deserializer = Deserializer::new(meta);
    let t = T::deserialize(&mut deserializer)?;
    if deserializer.is_complete() {
        Ok(t)
    } else {
        return Err(Error::TrailingItems);
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.peek_meta()? {
            syn::NestedMeta::Meta(m) => match m {
                syn::Meta::Path(_) => self.deserialize_seq(visitor),
                syn::Meta::List(_) => self.deserialize_seq(visitor),
                syn::Meta::NameValue(_) => self.deserialize_identifier(visitor),
            },
            syn::NestedMeta::Lit(l) => match l {
                syn::Lit::Str(_) => self.deserialize_string(visitor),
                syn::Lit::ByteStr(_) => self.deserialize_bytes(visitor),
                syn::Lit::Byte(_) => self.deserialize_u8(visitor),
                syn::Lit::Char(_) => self.deserialize_char(visitor),
                syn::Lit::Int(_) => self.deserialize_i64(visitor),
                syn::Lit::Float(_) => self.deserialize_f64(visitor),
                syn::Lit::Bool(_) => self.deserialize_bool(visitor),
                syn::Lit::Verbatim(_) => todo!("verbatim literal"),
            },
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_bool(self.parse_bool()?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i8(self.parse_integer()?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i16(self.parse_integer()?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.parse_integer()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.parse_integer()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u8(self.parse_integer()?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u16(self.parse_integer()?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.parse_integer()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.parse_integer()?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f32(self.parse_float()?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_f64(self.parse_float()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_char(self.parse_char()?)
    }

    fn deserialize_str<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::BorrowingNotSupported)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_string(self.parse_string()?)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("bytes")
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("byte_buf")
    }

    fn deserialize_option<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("option")
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("unit")
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("unit_struct")
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("newtype_struct")
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("seq")
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("tuple")
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("tuple_struct")
    }

    fn deserialize_map<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("map")
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("struct")
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("enum")
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("identifier")
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!("ignored_any")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct AttributeArgs(Vec<syn::NestedMeta>);

    impl syn::parse::Parse for AttributeArgs {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let mut all_meta = Vec::new();
            while let Ok(next_meta) = input.parse::<syn::NestedMeta>() {
                all_meta.push(next_meta);
            }
            Ok(Self(all_meta))
        }
    }

    macro_rules! assert_meta_eq {
        [$expected:expr => $ty:ty , [$( $t:tt )*]] => {{
            let AttributeArgs(args) = syn::parse_quote![$($t)*];
            let expected = $expected;
            let actual = from_nested_meta::<$ty>(&args).expect("failed to parse");
            assert_eq!(expected, actual, "expected: {:?}\nactual: {:?}", expected, actual);
        }};
    }

    #[test]
    fn booleans() {
        assert_meta_eq!(true => bool, [true]);
        assert_meta_eq!(false => bool, [false]);
    }

    #[test]
    fn integers() {
        assert_meta_eq!(10 => u8, [10]);
        assert_meta_eq!(-7 => i8, [-7]);
        assert_meta_eq!(110 => u16, [110]);
        assert_meta_eq!(-17 => i16, [-17]);
        assert_meta_eq!(101 => u32, [101]);
        assert_meta_eq!(-71 => i32, [-71]);
        assert_meta_eq!(9101 => u64, [9101]);
        assert_meta_eq!(-471 => i64, [-471]);
    }

    #[test]
    fn floats() {
        assert_meta_eq!(10.0 => f32, [10.0_f32]);
        assert_meta_eq!(1000.0 => f64, [1000.0_f32]);
    }

    #[test]
    fn characters() {
        assert_meta_eq!('a' => char, ['a']);
        assert_meta_eq!('z' => char, ['z']);
    }

    #[test]
    fn strings() {
        // TODO: fails, can this be fixed?
        // assert_meta_eq!("abcde" => &str, ["abcde"]);
        assert_meta_eq!("abcde" => String, ["abcde"]);
    }
}
