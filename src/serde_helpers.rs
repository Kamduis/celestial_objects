//! Helper functions to be used for deserializing special cases.




//=============================================================================
// Crates


use serde::{Serialize, Deserialize, Serializer, Deserializer};




//=============================================================================
// Helpers for serialization and deserialization


/// Serialize an `Option` by unwrapping it. This is only useful in combination with `#[serde( default )]` and `#[serde( skip_serializing_if = "Option::is_none" )]`.
///
/// To be used with the `serde` annotation `with`.
pub mod option_wrapper {
	use super::*;

	/// If the value is `Some(...)`, it will be serialized as if there was no `option` wrapped around it.
	pub(crate) fn serialize<T, S>( value: &Option<T>, serializer: S ) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
		T: Serialize,
	{
		// This panics, if `value` is `None`. To avoid this you must use `#[serde( skip_serializing_if = "Option::is_none" )]`!
		value.as_ref().unwrap().serialize( serializer )
	}

	/// If the item is present (and must not be wrapped in `Some()`, of course), it will be deserialized as `Some(...)`.
	pub(crate) fn deserialize<'de, T, D>( deserializer: D ) -> Result<Option<T>, D::Error>
	where
		D: Deserializer<'de>,
		T: Deserialize<'de>,
	{
		T::deserialize( deserializer ).map( Some )
	}
}
