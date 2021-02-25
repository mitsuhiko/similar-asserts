use std::fmt;

use serde::ser::{
    SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple,
    SerializeTupleStruct, SerializeTupleVariant,
};
use serde::{Serialize, Serializer};

pub struct Debug<'a, T: Serialize + ?Sized>(pub &'a T);

impl<'a, T: Serialize + ?Sized> fmt::Debug for Debug<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.serialize(DebugSerializer(f))?;
        Ok(())
    }
}

macro_rules! simple_serialize {
    ($name:ident, $type:ty) => {
        fn $name(self, v: $type) -> Result<Self::Ok, Self::Error> {
            fmt::Debug::fmt(&v, self.0)
        }
    };
}

pub struct DebugSerializer<'a, 'b: 'a>(pub &'a mut fmt::Formatter<'b>);

impl<'a, 'b: 'a> Serializer for DebugSerializer<'a, 'b> {
    type Ok = ();
    type Error = fmt::Error;

    type SerializeSeq = SeqSerializer<'a, 'b>;
    type SerializeTuple = TupleSerializer<'a, 'b>;
    type SerializeTupleStruct = TupleSerializer<'a, 'b>;
    type SerializeTupleVariant = TupleSerializer<'a, 'b>;
    type SerializeMap = MapSerializer<'a, 'b>;
    type SerializeStruct = StructSerializer<'a, 'b>;
    type SerializeStructVariant = StructSerializer<'a, 'b>;

    simple_serialize!(serialize_bool, bool);
    simple_serialize!(serialize_i8, i8);
    simple_serialize!(serialize_i16, i16);
    simple_serialize!(serialize_i32, i32);
    simple_serialize!(serialize_i64, i64);
    simple_serialize!(serialize_i128, i128);
    simple_serialize!(serialize_u8, u8);
    simple_serialize!(serialize_u16, u16);
    simple_serialize!(serialize_u32, u32);
    simple_serialize!(serialize_u64, u64);
    simple_serialize!(serialize_u128, u128);
    simple_serialize!(serialize_f32, f32);
    simple_serialize!(serialize_f64, f64);
    simple_serialize!(serialize_char, char);
    simple_serialize!(serialize_str, &str);
    simple_serialize!(serialize_bytes, &[u8]);

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit_struct("None")
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        self.serialize_newtype_struct("Some", value)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        write!(self.0, "()")
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        SerializeTupleStruct::end(self.serialize_tuple_struct(name, 0)?)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit_struct(variant)
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        let mut tuple = self.serialize_tuple_struct(name, 1)?;
        SerializeTupleStruct::serialize_field(&mut tuple, value)?;
        SerializeTupleStruct::end(tuple)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_newtype_struct(variant, value)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SeqSerializer(self.0.debug_list()))
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(TupleSerializer(self.0.debug_tuple(name)))
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_tuple_struct("", len)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.serialize_tuple_struct(variant, len)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(MapSerializer(self.0.debug_map()))
    }

    fn serialize_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(StructSerializer(self.0.debug_struct(name)))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.serialize_struct(variant, len)
    }
}

pub struct SeqSerializer<'a, 'b: 'a>(fmt::DebugList<'a, 'b>);

impl<'a, 'b: 'a> SerializeSeq for SeqSerializer<'a, 'b> {
    type Ok = ();
    type Error = fmt::Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.0.entry(&Debug(value));
        Ok(())
    }

    fn end(mut self) -> Result<(), Self::Error> {
        Ok(self.0.finish()?)
    }
}

pub struct TupleSerializer<'a, 'b: 'a>(fmt::DebugTuple<'a, 'b>);

impl<'a, 'b: 'a> SerializeTuple for TupleSerializer<'a, 'b> {
    type Ok = ();
    type Error = fmt::Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.0.field(&Debug(value));
        Ok(())
    }

    fn end(mut self) -> Result<(), Self::Error> {
        Ok(self.0.finish()?)
    }
}

impl<'a, 'b: 'a> SerializeTupleStruct for TupleSerializer<'a, 'b> {
    type Ok = ();
    type Error = fmt::Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<(), Self::Error> {
        SerializeTuple::end(self)
    }
}

impl<'a, 'b: 'a> SerializeTupleVariant for TupleSerializer<'a, 'b> {
    type Ok = ();
    type Error = fmt::Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<(), Self::Error> {
        SerializeTuple::end(self)
    }
}

pub struct MapSerializer<'a, 'b: 'a>(fmt::DebugMap<'a, 'b>);

impl<'a, 'b: 'a> SerializeMap for MapSerializer<'a, 'b> {
    type Ok = ();
    type Error = fmt::Error;

    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.0.key(&Debug(key));
        Ok(())
    }

    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.0.value(&Debug(value));
        Ok(())
    }

    fn serialize_entry<K: ?Sized + Serialize, V: ?Sized + Serialize>(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), Self::Error> {
        self.0.entry(&Debug(key), &Debug(value));
        Ok(())
    }

    fn end(mut self) -> Result<(), Self::Error> {
        Ok(self.0.finish()?)
    }
}

pub struct StructSerializer<'a, 'b: 'a>(fmt::DebugStruct<'a, 'b>);

impl<'a, 'b: 'a> SerializeStruct for StructSerializer<'a, 'b> {
    type Ok = ();
    type Error = fmt::Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.0.field(key, &Debug(value));
        Ok(())
    }

    fn end(mut self) -> Result<(), Self::Error> {
        Ok(self.0.finish()?)
    }
}

impl<'a, 'b: 'a> SerializeStructVariant for StructSerializer<'a, 'b> {
    type Ok = ();
    type Error = fmt::Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        SerializeStruct::serialize_field(self, key, value)
    }

    fn end(self) -> Result<(), Self::Error> {
        SerializeStruct::end(self)
    }
}
