//! Functions supporting travel.




//=============================================================================
// Crates


use std::str::FromStr;

#[cfg( feature = "i18n" )] use fluent_templates::Loader;
use chrono::TimeDelta;
use thiserror::Error;
#[cfg( feature = "i18n" )] use unic_langid::LanguageIdentifier;

#[cfg( feature = "i18n" )] use crate::LOCALES;
#[cfg( feature = "i18n" )] use crate::Locale;
use crate::Length;




//=============================================================================
// Errors


/// Represents the possible errors for space travel.
#[derive( Error, Debug )]
pub enum TravelError {
	#[error( "Cannot parse: {0}" )]
	Unparsable( String ),
}




//=============================================================================
// Space travel


/// Represents the kind of maneuver a space ship may aim for during the travel.
#[derive( Clone, Copy, PartialEq, Eq, Debug )]
pub enum SpaceTravelManeuver {
	/// The space ship makes a rendezvous at the end of its travel, meaning, it has the same speed at the and as it had at the start of it.
	Rendezvous,

	/// The space ship takes the least amount of time to its target, meaning there is probably a huge speed difference at the end.
	Intercept,
}

#[cfg( feature = "i18n" )]
impl Locale for SpaceTravelManeuver {
	/// Returns the string representation of `self` using the language according to the provided `locale`.
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::Rendezvous => LOCALES.lookup( locale, "maneuver-rendezvous" ),
			Self::Intercept => LOCALES.lookup( locale, "maneuver-intercept" ),
		}
	}
}

impl FromStr for SpaceTravelManeuver {
	type Err = TravelError;

	fn from_str( s: &str ) -> Result<Self, Self::Err> {
		let res = match s.to_lowercase().as_str() {
			"rendezvous" => Self::Rendezvous,
			"intercept" => Self::Intercept,
			_ => return Err( TravelError::Unparsable( s.to_string() ) ),
		};

		Ok( res )
	}
}


/// This struct represents the data of a space travel.
#[derive( Clone, Debug )]
pub struct SpaceTravel {
	/// The distance between the starting point and the end point of the travel.
	distance: Length,

	/// The acceleration of the spacecraft in m/s².
	acceleration: f64,

	/// The absolute speed limit imposed during this travel. Measured in m/s. This is typically 10% of the speed of light.
	speed_limit: Option<f64>,

	/// The kind of maneuver this space travel represents.
	maneuver: SpaceTravelManeuver,
}

impl SpaceTravel {
	/// Creates a new `SpaceTravel` from `distance` and a constant `acceleration`.
	///
	/// # Arguments
	/// * `distance` The distance to travel.
	/// * `acceleration` The acceleration of the spacecraft in m/s².
	pub fn from_dist_accel( distance: Length, acceleration: f64 ) -> SpaceTravel {
		Self {
			distance,
			acceleration,
			speed_limit: None,
			maneuver: SpaceTravelManeuver::Rendezvous,
		}
	}

	/// Creates a new `SpaceTravel` with `speed_limit` measured in m/s. When this speed has been reached, the spacecraft will stop accelerating and wait until it is time do decelerate.
	///
	/// It is possible to impose a `speed_limit` to the travel. As soon as thee `speed_limit` is reached, no further acceleration will happen. If the `spaceTravel` is a rendevouz maneuver, the acceleration will stop if the speed limit is reached until it is time to decelerate again (with the same acceleration rate as before, but in the opposite direction, of course.
	pub fn with_speed_limit( mut self, speed_limit: f64 ) -> Self {
		self.speed_limit = Some( speed_limit );
		self
	}

	/// Creates a new `SpaceTravel` with `maneuver`.
	pub fn with_maneuver( mut self, maneuver: SpaceTravelManeuver ) -> Self {
		self.maneuver = maneuver;
		self
	}

	/// Returns the total duration of the space travel.
	///
	/// The duration of the space travel depends on the maneuver performed. A `SpaceTravelManeuver::Rendevouz` means, that the spacecraft is accelerating towards the target for half the distance, and then decelerating at the same rate for the other half. Under a `SpaceTravelManeuver::Intercept` the spacecraft will accelerate for the full distance (or until the speed limit is reached.
	pub fn duration( &self ) -> TimeDelta {
		let accel_si = self.acceleration;

		let dur_seconds = match self.maneuver {
			SpaceTravelManeuver::Rendezvous => {
				let ( dur_half, _ ) = duration_acceleration(
					self.distance.meter() / 2.0,
					accel_si,
					self.speed_limit
				);
				2.0 * dur_half
			},
			SpaceTravelManeuver::Intercept => {
				let ( dur, _ ) = duration_acceleration(
					self.distance.meter(),
					accel_si,
					self.speed_limit
				);
				dur
			},
		};

		let nanos = ( dur_seconds.rem_euclid( 1.0 ) * 1e9 ) as u32;

		TimeDelta::new( dur_seconds as i64, nanos ).unwrap()
	}

	/// Returns the maximum speed reached during the space travel (relative to the starting speed) as m/s.
	pub fn speed_max( &self ) -> f64 {
		let accel_si = self.acceleration;

		let ( _, speed_max ) = match self.maneuver {
			SpaceTravelManeuver::Rendezvous => duration_acceleration(
				self.distance.meter() / 2.0,
				accel_si,
				self.speed_limit
			),
			SpaceTravelManeuver::Intercept => duration_acceleration(
				self.distance.meter(),
				accel_si,
				self.speed_limit
			),
		};

		speed_max
	}

	/// Returns if the `speed_limit` has been reached during this travel (`true`) or not (`false`).
	pub fn speed_limit_reached( &self ) -> bool {
		self.speed_limit.map( |x| self.speed_max() >= x - f64::EPSILON ).unwrap_or_default()
	}
}

impl Default for SpaceTravel {
	fn default() -> Self {
		Self {
			distance: 0.0.into(),
			acceleration: 0.0,
			speed_limit: None,
			maneuver: SpaceTravelManeuver::Rendezvous,
		}
	}
}


/// Returns the duration until `distance` has been travelled under `acceleration`.
///
/// # Arguments
/// * `distance` The distance to travel in m/s.
/// * `acceleration` The acceleration in m/s².
/// * `speed_limit` (m/s) The maximum speed to be reached. When this speed has been reached, no further acceleration will happen and the rest of the distance will be travelled in `speed_limit`.
///
/// # Returns
/// The returned tuple contains:
/// 0. The duration of the travel in s.
/// 1. The top speed reached in m/s². This can be lower than `speed_limit` (if provided), but never higher.
fn duration_acceleration( distance: f64, acceleration: f64, speed_limit: Option<f64> ) -> ( f64, f64 ) {
	let duration_naive = ( 2.0 * distance / acceleration ).sqrt();
	let v_reached = acceleration * duration_naive;

	let Some( speed_max ) = speed_limit else {
		return ( duration_naive, v_reached );
	};

	// Duration until `speed_max` has been reached.
	let duration_acc = speed_max / acceleration;

	// Is there enough time to reach `speed_max`?
	if duration_naive < duration_acc {
		return ( duration_naive, v_reached );
	}

	let dist_accel = 0.5 * acceleration * duration_acc.powi( 2 );

	// Durtion until `speed_max` has been reached plus duration at `speed_max`.
	let dist_at_max = distance - dist_accel;
	let dur_at_max = dist_at_max / speed_max;

	( duration_acc + dur_at_max, speed_max )
}




//=============================================================================
// Testing


#[cfg( test )]
mod tests {
	use super::*;

	#[test]
	fn test_duration_acceleration() {
		assert_eq!( duration_acceleration( 0.5, 1.0, None ), ( 1.0, 1.0 ) );
		assert_eq!( duration_acceleration( 1.0, 0.5, None ), ( 2.0, 1.0 ) );
		assert_eq!( duration_acceleration( 1.0, 1.0, Some( 1.0 ) ), ( 1.5, 1.0 ) );
	}

	#[test]
	fn test_duration_spaceflight() {
		let flight_0 = SpaceTravel::from_dist_accel( Length::from_meter( 100.0 ), 10.0 );
		assert_eq!( flight_0.duration(), TimeDelta::new( 6, 324555320 ).unwrap() );
		assert_eq!( flight_0.speed_max(), 31.622776601683796 );

		let flight_1 = SpaceTravel::from_dist_accel( Length::from_meter( 100.0 ), 10.0 )
			.with_maneuver( SpaceTravelManeuver::Intercept );
		assert_eq!( flight_1.duration(), TimeDelta::new( 4, 472135954 ).unwrap() );
		assert_eq!( flight_1.speed_max(), 44.721359549995796 );

		let flight_2 = SpaceTravel::from_dist_accel( Length::from_meter( 200.0 ), 5.0 );
		assert_eq!( flight_2.duration(), TimeDelta::new( 12, 649110640 ).unwrap() );
		assert_eq!( flight_2.speed_max(), 31.622776601683796 );

		let flight_3 = SpaceTravel::from_dist_accel( Length::from_meter( 200.0 ), 5.0 )
			.with_maneuver( SpaceTravelManeuver::Intercept );
		assert_eq!( flight_3.duration(), TimeDelta::new( 8, 944271909 ).unwrap() );
		assert_eq!( flight_3.speed_max(), 44.721359549995796 );

		let flight_4 = SpaceTravel::from_dist_accel( Length::from_meter( 100.0 ), 10.0 )
			.with_speed_limit( 1.0 );
		assert_eq!( flight_4.duration(), TimeDelta::new( 100, 100000000 ).unwrap() );
		assert_eq!( flight_4.speed_max(), 1.0 );
	}
}
