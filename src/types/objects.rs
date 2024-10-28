//! Provides types for celestial objects.




//=============================================================================
// Crates


use chrono::TimeDelta;
use glam::Vec3;
use serde::{Serialize, Deserialize};

use crate::calc;
use crate::units::{Mass, Length};

use super::AstronomicalObject;
use super::properties::{StarProperty, Orbit};




//=============================================================================
// Objects


/// Representing the theoretical gravitational center of two heavy masses orbiting each other.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct GravitationalCenter {
	/// The objects orbiting this gravitational center.
	pub(crate) satellites: Vec<Orbit>,
}

impl AstronomicalObject for GravitationalCenter {
	/// Returns `self` unmodified, since gravitational centers a theoretical objects that never have names.
	fn with_name( self, _name: &str ) -> Self {
		self
	}

	/// Return a new object from `self` with `satellites` orbiting it.
	fn with_satellites( mut self, satellites: Vec<Orbit> ) -> Self {
		self.satellites = satellites;
		self
	}

	fn satellites( &self ) -> &[Orbit] {
		&self.satellites
	}

	fn mass( &self ) -> Mass {
		Mass::ZERO
	}

	/// Returns the radius of this gravitational center, which is always 0.0.
	fn radius( &self ) -> Length {
		Length::ZERO
	}

	fn gravitation( &self ) -> Option<f32> {
		Some( 0.0 )
	}

	/// Always returns a rotation period of 0.0. `None` is considered to be bound rotation.
	/// TODO: Return an enum that reflects valid, invalid and bound rotation.
	fn rotation_period( &self ) -> Option<TimeDelta> {
		Some( TimeDelta::zero() )
	}
}


/// Representing a star of a planetary system.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct Star {
	/// The name of this star. If this is `None`, the star will be named by its hierarchy within the `CelestialSystem`.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	pub(crate) name: Option<String>,

	/// The mass in relation to the mass of Sol.
	pub(crate) mass: f32,

	/// The radius in relation to the radius of Sol.
	pub(crate) radius: f32,

	/// The luminosity in relation to the luminosity of Sol.
	pub(crate) luminosity: f32,

	/// The spectral class.
	pub(crate) spectral_class: String,

	/// The rotation period (sidereal time). This is the time duration it takes for the star to make a full rotation in relation to a fixed star.
	/// If this is `None`, this means the body's representation is gravitational bound around the object it orbits.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::timedelta_option" )]
	pub(crate) rotation_period: Option<TimeDelta>,

	/// Special properties of the star.
	#[serde( default )]
	properties: Vec<StarProperty>,

	/// The objects oribitng this star.
	pub(crate) satellites: Vec<Orbit>,
}

impl Star {
	/// Create a new `Star`.
	pub fn new( mass: f32, radius: f32, luminosity: f32, spectral_class: &str ) -> Self {
		Self {
			name: None,
			mass,
			radius,
			luminosity,
			spectral_class: spectral_class.to_string(),
			rotation_period: None,
			properties: Vec::new(),
			satellites: Vec::new(),
		}
	}

	/// Returns a `Star` from `self` with the `rotation_period`.
	pub fn with_rotation_period( mut self, rotation_period: TimeDelta ) -> Self {
		self.rotation_period = Some( rotation_period );
		self
	}

	/// Returns the luminosity of the star in relation to the luminosity of Sol.
	pub fn luminosity( &self ) -> f32 {
		self.luminosity
	}

	/// Returns the spectral class of the star.
	pub fn spectral_class( &self ) -> &str {
		&self.spectral_class
	}

	/// Returns the inner and outer radius of the habitable zone around the star.
	pub fn habitable_zone( &self ) -> [Length; 2] {
		let lengths: Vec<Length> = calc::habitable_zone( self.luminosity() ).iter()
			.map( |x| Length::from( *x ) )
			.collect();

		lengths.try_into().unwrap()
	}
}

impl AstronomicalObject for Star {
	/// Return a new object from `self` with `name`.
	fn with_name( mut self, name: &str ) -> Self {
		self.name = Some( name.to_string() );
		self
	}

	/// Return a new object from `self` with `satellites` orbiting it.
	fn with_satellites( mut self, satellites: Vec<Orbit> ) -> Self {
		self.satellites = satellites;
		self
	}

	fn satellites( &self ) -> &[Orbit] {
		&self.satellites
	}

	fn mass( &self ) -> Mass {
		Mass::from_mass_sol( self.mass )
	}

	fn radius( &self ) -> Length {
		Length::from_radius_sol( self.radius )
	}

	fn gravitation( &self ) -> Option<f32> {
		None
	}

	fn rotation_period( &self ) -> Option<TimeDelta> {
		self.rotation_period
	}
}


/// Representing a trabant. This could be a planet in the orbit of a star of a moon in the orbit of a planet.
#[derive( Serialize, Deserialize, Clone, PartialEq, Default, Debug )]
pub struct Trabant {
	/// The name of this trabant. If this is `None`, the trabant will be named by its hierarchy within the `CelestialSystem`.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	pub(crate) name: Option<String>,

	/// The radius in relation to the radius of Terra.
	pub(crate) radius: f32,

	/// The surface gravity of this trabant in relation to the surface gravity of Terra.
	pub(crate) gravity: f32,

	/// The rotation period (sidereal time). This is the time duration it takes for the body to make a full rotation. This is different from the "day" duration, which may be a little bit longer. Since when the body performed a full rotation it moved along it's orbit and is therefore not facing the same angle to it's sun.
	/// If this is `None`, this means the body's representation is gravitational bound around the object it orbits.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::timedelta_option" )]
	pub(crate) rotation_period: Option<TimeDelta>,

	/// The objects orbiting this trabant.
	pub(crate) satellites: Vec<Orbit>,
}

impl AstronomicalObject for Trabant {
	/// Return a new object from `self` with `name`.
	fn with_name( mut self, name: &str ) -> Self {
		self.name = Some( name.to_string() );
		self
	}

	/// Return a new object from `self` with `satellites` orbiting it.
	fn with_satellites( mut self, satellites: Vec<Orbit> ) -> Self {
		self.satellites = satellites;
		self
	}

	fn satellites( &self ) -> &[Orbit] {
		&self.satellites
	}

	fn mass( &self ) -> Mass {
		let mass_terra = self.gravity * self.radius.powi( 2 );
		Mass::from_mass_terra( mass_terra )
	}

	fn radius( &self ) -> Length {
		Length::from_radius_terra( self.radius )
	}

	fn gravitation( &self ) -> Option<f32> {
		Some( self.gravity )
	}

	/// The rotation period (sidereal time). This is the time duration it takes for the body to make a full rotation. This is different from the "day" duration, which may be a little bit longer. Since when the body performed a full rotation it moved along it's orbit and is therefore not facing the same angle to it's sun.
	/// If this is `None`, this means the body's representation is gravitational bound around the object it orbits.
	fn rotation_period( &self ) -> Option<TimeDelta> {
		self.rotation_period
	}
}


/// Representing a ring around an object.
#[derive( Serialize, Deserialize, Clone, PartialEq, Default, Debug )]
pub struct Ring {
	/// The width of the ring in AU.
	pub(crate) width: f32,
}

impl Ring {
	/// The width of the ring in AU.
	pub fn width( &self ) -> Length {
		Length::from( self.width )
	}
}


/// Representing a space station.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct Station {
	/// The name of this station.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	pub(crate) name: Option<String>,

	/// The mass in kg.
	pub(crate) mass: f32,

	/// The outline box of the station in meter. This is the box where the station is just fitting inside.
	pub(crate) size: Vec3,

	/// The gravity within the station in relation to the surface gravity of Terra.
	pub(crate) gravity: f32,

	/// The objects orbiting this station.
	pub(crate) satellites: Vec<Orbit>,
}

impl AstronomicalObject for Station {
	/// Return a new object from `self` with `name`.
	fn with_name( mut self, name: &str ) -> Self {
		self.name = Some( name.to_string() );
		self
	}

	/// Return a new object from `self` with `satellites` orbiting it.
	fn with_satellites( mut self, satellites: Vec<Orbit> ) -> Self {
		self.satellites = satellites;
		self
	}

	fn satellites( &self ) -> &[Orbit] {
		&self.satellites
	}

	fn mass( &self ) -> Mass {
		Mass::from( self.mass )
	}

	/// Returns the station's radius. This is mostly useless, since stations are rarely spherical.
	fn radius( &self ) -> Length {
		Length::from( self.size.max_element() )
	}

	fn gravitation( &self ) -> Option<f32> {
		Some( self.gravity )
	}
}
