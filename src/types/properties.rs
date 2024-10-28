//! Provides types for celestial objects.




//=============================================================================
// Crates


use std::fmt;

use serde::{Serialize, Deserialize};

use crate::units::Length;

use super::CelestialBody;




//=============================================================================
// Types


/// Classification of celestial bodies without further information.
#[derive( Clone, Copy, PartialEq, Debug )]
pub enum BodyType {
	/// A point in space that is the gravitational center of two masses orbiting each other.
	GravitationalCenter,
	Star,
	Trabant,
	Ring,
	Station,
}

impl From<&CelestialBody> for BodyType {
	fn from( item: &CelestialBody ) -> Self {
		match item {
			CelestialBody::GravitationalCenter( _ ) => Self::GravitationalCenter,
			CelestialBody::Star( _ ) => Self::Star,
			CelestialBody::Trabant( _ ) => Self::Trabant,
			CelestialBody::Ring( _ ) => Self::Ring,
			CelestialBody::Station( _ ) => Self::Station,
		}
	}
}

impl fmt::Display for BodyType {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		match self {
			Self::GravitationalCenter => write!( f, "Planet" ),
			Self::Star => write!( f, "Star" ),
			Self::Trabant => write!( f, "Trabant" ),
			Self::Ring => write!( f, "Ring" ),
			Self::Station => write!( f, "Station" ),
		}
	}
}


/// Classification of trabants.
#[derive( Clone, Copy, PartialEq, Eq, Debug )]
pub enum TrabantType {
	Planet,
	Moon,
}

impl fmt::Display for TrabantType {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		match self {
			Self::Planet => write!( f, "Planet" ),
			Self::Moon => write!( f, "Moon" ),
		}
	}
}


/// Representing a political affiliation.
#[derive( Serialize, Deserialize, PartialEq, Hash, Clone, Default, Debug )]
pub enum Affiliation {
	/// Part of the Union.
	Union,

	/// Is considered a border world.
	BorderWorld,

	/// Part of the free territories.
	Free,

	/// Is uninhabited.
	#[ default ]
	Uninhabited,
}


/// Representing the orbit of a `CelestialBody` around another `CelestialBody`.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct Orbit {
	/// The semi major axis of the `object`'s orbit in AU.
	pub axis_semi_major: f32,

	/// The eccentricity of the `object`'s orbit.
	pub eccentricity: f32,

	/// The object orbiting.
	pub body: CelestialBody,
}

impl Orbit {
	/// Returns the semi major axis of this orbit in meter.
	pub fn axis_semi_major( &self ) -> Length {
		Length::from_au( self.axis_semi_major )
	}

	/// Returns the semi minor axis of this orbit in meter.
	pub fn axis_semi_minor( &self ) -> Length {
		let res_in_au = ( self.axis_semi_major.powi( 2 ) - ( self.eccentricity * self.axis_semi_major ).powi( 2 ) ).sqrt();

		Length::from_au( res_in_au )
	}

	/// Returns the eccentricity of this orbit.
	pub fn eccentricity( &self ) -> f32 {
		self.eccentricity
	}

	/// Calculates the periapsis of the orbit.
	pub fn periapsis( &self ) -> Length {
		let res = self.axis_semi_major * ( 1.0 - self.eccentricity );

		Length::from_au( res )
	}

	/// Calculates the apoapsis of the orbit.
	pub fn apoapsis( &self ) -> Length {
		let res = self.axis_semi_major * ( 1.0 + self.eccentricity );

		Length::from_au( res )
	}

	/// Radius of the border to the focal point 1 at `angle`. This is the distance of the ellipsis to focal point at `angle`. `angle` is in radians.
	pub fn radius_from_focal( &self, angle: f32 ) -> Length {
		let p = self.axis_semi_minor().powi( 2 ) / self.axis_semi_major();

		p / ( 1.0 + self.eccentricity * angle.cos() )
	}
}


/// Some stars have special properties.
#[derive( Serialize, Deserialize, Clone, PartialEq, Eq, Debug )]
pub enum StarProperty {
	/// A star with this property exhibit unusually violent flare activity. Flares occur sporadically, with successive flares spaced anywhere from an hour to a few days apart. Flares may emit up to 10'000 times the amount of radioactive radiation as a comparably sized flare on Sol. This would be lethal to any life forms on planets near the flare star.
	FlareStar,

	/// A white dwarf star.
	WhiteDwarf,

	/// A red giant is a luminous giant star of low mass (between 0.25 to 8 M☉).
	RedGiant,

	/// Higher-mass stars leave the main sequence to become blue giants, then bright blue giants, and then blue supergiants, before expanding into red supergiants.
	BlueGiant,
}
