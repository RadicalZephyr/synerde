use std::fmt::Display;

use serde::{de, ser, Deserialize};

use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("{0}")]
    Message(String),

    #[error("unexepected end of input")]
    EOF,

    #[error("unparsed content found at end of input")]
    TrailingItems,

    #[error("expected a boolean value")]
    ExpectedBoolean,
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Deserializer<'a> {
    meta: &'a [syn::NestedMeta],
}

impl<'a> Deserializer<'a> {
    fn peek_meta(&mut self) -> Result<&syn::NestedMeta> {
        if let Some(next_meta) = self.meta.first() {
            Ok(next_meta)
        } else {
            Err(Error::EOF)
        }
    }

    fn parse_bool(&mut self) -> Result<bool> {
        match self.peek_meta()? {
            syn::NestedMeta::Lit(syn::Lit::Bool(v)) => {
                let value = v.value;
                self.meta = &self.meta[1..];
                Ok(value)
            }
            _ => Err(Error::ExpectedBoolean),
        }
    }
}

pub fn from_nested_meta<'a, T>(meta: &'a [syn::NestedMeta]) -> Result<T>
where
    T: Deserialize<'a>,
{
    let mut deserializer = Deserializer { meta };
    let t = T::deserialize(&mut deserializer)?;
    if deserializer.meta.is_empty() {
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
                syn::Lit::Verbatim(_) => todo!(),
            },
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_bool(self.parse_bool()?)
    }

    fn deserialize_i8<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i16<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i32<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i64<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u8<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u16<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u32<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u64<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_f32<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_f64<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_str<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_string<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_option<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
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
        todo!()
    }

    fn deserialize_map<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
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
        todo!()
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
        todo!()
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        println!("what the fuck");
        todo!()
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use super::*;

    use quote::ToTokens;

    struct AttributeArgs(Vec<syn::NestedMeta>);

    impl fmt::Debug for AttributeArgs {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let entries = self
                .0
                .iter()
                .map(|nm| nm.to_token_stream())
                .collect::<Vec<_>>();
            f.debug_list().entries(&entries).finish()
        }
    }

    impl syn::parse::Parse for AttributeArgs {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let mut all_meta = Vec::new();
            while let Ok(next_meta) = input.parse::<syn::NestedMeta>() {
                all_meta.push(next_meta);
            }
            Ok(Self(all_meta))
        }
    }

    #[test]
    fn booleans() {
        let AttributeArgs(args) = syn::parse_quote![true];
        let actual: bool = from_nested_meta(&args).expect("failed to parse");
        assert_eq!(true, actual);
        let AttributeArgs(args) = syn::parse_quote![false];
        let actual: bool = from_nested_meta(&args).expect("failed to parse");
        assert_eq!(false, actual);
    }
}
