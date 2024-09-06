//! Helper functions to be used for deserializing special cases.




//=============================================================================
// Crates


use serde::{Deserialize, Deserializer};




//=============================================================================
// Helpers for serialization and deserialization


/// Serialize an `Option` by unwrapping it. This is only useful in combination with `#[serde( default )]` and `#[serde( skip_serializing_if = "Option::is_none" )]`.
///
/// To be used with the `serde` annotation `with`.
pub mod option_wrapper {
	use super::*;

	/// If the item is present (and must not be wrapped in `Some()`, of course), it will be deserialized as `Some(...)`.
	pub(crate) fn deserialize<'de, T, D>( deserializer: D ) -> Result<Option<T>, D::Error>
	where
		D: Deserializer<'de>,
		T: Deserialize<'de>,
	{
		T::deserialize( deserializer ).map( Some )
	}
}
