//! Helper functions to be used for deserializing special cases.




//=============================================================================
// Crates


use std::num::ParseFloatError;
use std::sync::LazyLock;

use chrono::TimeDelta;
use regex::Regex;
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use serde::de::Error as _;
use thiserror::Error;




//=============================================================================
// Errors


#[derive( Error, Debug )]
pub enum SerdeError {
	#[error( "Argument is malformed: {0}" )]
	ArgumentMalformed( String ),

	#[error( "Could not parse float from text." )]
	ParseFloatError( #[from] ParseFloatError ),
}




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


/// Serialize a `chrono::TimeDelta` to "32d 16h 54m 32.1s".
///
/// To be used with the `serde` annotation `with`.
pub mod timedelta_option {
	/// The regular expression used to get days, hours, minutes and seconds from a `"DdHhMmSs"` string.
	static REGEX_DHMS: LazyLock<Regex> = LazyLock::new( || {
		Regex::new( r"^(?<sign>[+-]?)((?<days>\d+\.?\d*)d)?\s*((?<hours>[+-]?\d+\.?\d*)h)?\s*((?<minutes>\d+\.?\d*)m)?\s*((?<seconds>\d+\.?\d*)s)?$" ).unwrap()
	} );

	use super::*;

	/// Converts a days-hours-minutes-seconds string into a `TimeDelta`.
	///
	/// `dhms` must be in the format of `"DdHhMmSs"` where `D` is the days, `H` is the hours, `M` the minutes and `S` the seconds (all with optional fraction part) while `h`, `m` and `s` are literal letters. Omitted elements are assumed to be zero. `"12h13.5s"` is allowed and equal to `"0d12h0m13.5s"`.
	///
	/// This function returns `None` if `dhms` is malformed.
	fn dhms_to_timedelta( dhms: &str ) -> Result<TimeDelta, SerdeError> {
		let caps = REGEX_DHMS.captures( dhms )
			.ok_or( SerdeError::ArgumentMalformed( dhms.to_string() ) )?;

		let is_neg = match &caps.name( "sign" ) {
			Some( x ) => x.as_str() == "-",
			None => false,
		};

		let days = match &caps.name( "days" ) {
			Some( x ) => x.as_str().parse::<f32>()?,
			None => 0.0,
		};

		let hours = match &caps.name( "hours" ) {
			Some( x ) => x.as_str().parse::<f32>()?,
			None => 0.0,
		};

		let minutes = match &caps.name( "minutes" ) {
			Some( x ) => x.as_str().parse::<f32>()?,
			None => 0.0,
		};

		let seconds = match &caps.name( "seconds" ) {
			Some( x ) => x.as_str().parse::<f32>()?,
			None => 0.0,
		};

		let mut secs_total = days * 86400.0 + hours * 3600.0 + minutes * 60.0 + seconds;
		if is_neg { secs_total *= -1.0 };

		let nanos = ( seconds.rem_euclid( 1.0 ) * 1e9 ) as u32;
		let res = TimeDelta::new( secs_total as i64, nanos ).unwrap();

		Ok( res )
	}

	/// If the value is `Some(...)`, it will be serialized as if there was no `option` wrapped around it.
	pub(crate) fn serialize<S>( value: &Option<TimeDelta>, serializer: S ) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		// This panics, if `value` is `None`. To avoid this you must use `#[serde( skip_serializing_if = "Option::is_none" )]`!
		let td = value.expect( r#"`value` must *not* be `None`. To avoid this you must use `#[serde( skip_serializing_if = "Option::is_none" )]"# );

		let mut elements = Vec::new();
		let td_abs = td.abs();
		if td_abs.num_days() != 0 {
			elements.push( format!( "{}d", td_abs.num_days().to_string() ) );
		}

		let hours_seg = ( td_abs - TimeDelta::days( td_abs.num_days() ) ).num_hours();
		let minutes_seg = ( td_abs - TimeDelta::hours( td_abs.num_hours() ) ).num_minutes();
		let seconds_seg = ( td_abs - TimeDelta::minutes( td_abs.num_minutes() ) ).num_seconds();

		let secs = seconds_seg as f64 + td_abs.subsec_nanos() as f64 / 1e9;

		elements.push( format!( "{}h {}m {}s", hours_seg, minutes_seg, secs ) );

		let mut stringified = elements.join( " " );

		if td < TimeDelta::zero() {
			stringified.insert( 0, '-' );
		}

		stringified.serialize( serializer )
	}

	/// If the item is present (and must not be wrapped in `Some()`, of course), it will be deserialized as `Some(...)`.
	pub(crate) fn deserialize<'de, D>( deserializer: D ) -> Result<Option<TimeDelta>, D::Error>
	where
		D: Deserializer<'de>,
	{
		let deserialized = String::deserialize( deserializer )?;

		let td = dhms_to_timedelta( &deserialized )
			.map_err( D::Error::custom )?;

		return Ok( Some( td ) )
	}
}
