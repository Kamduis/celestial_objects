//! Provides types for celestial objects.




//=============================================================================
// Crates


use std::collections::BTreeMap;
use std::fmt;

use serde::{Serialize, Deserialize};

#[cfg( feature = "tex" )] use crate::traits::Latex;
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


/// The composition of an atmosphere.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct Atmosphere {
	/// The pressure of the atmosphere in bar.
	pub(crate) pressure: f32,

	/// The quality of the atmosphere to humans.
	pub(crate) quality: AtmosphereQuality,

	/// The composition of the atmosphere.
	pub(crate) composition: GasComposition,
}

impl Atmosphere {
	/// Returns the pressure of the atmosphere in bar.
	pub fn pressure( &self ) -> f32 {
		self.pressure
	}

	/// Returns the quality of the atmosphere.
	pub fn quality( &self ) -> AtmosphereQuality {
		self.quality
	}

	/// Returns the composition of the atmosphere.
	pub fn composition( &self ) -> &GasComposition {
		&self.composition
	}
}


/// The composition of the atmosphere.
#[derive( Serialize, Deserialize, Clone, PartialEq, Default, Debug )]
pub struct GasComposition( BTreeMap<Molecule, f64> );

impl GasComposition {
	/// Return the amount of other/unknown elements.
	#[cfg( feature = "tex" )]
	fn other( &self ) -> f64 {
		let known: f64 = self.0.values().sum();

		1.0 - known
	}
}

impl<const N: usize> From<[( Molecule, f64 ); N]> for GasComposition {
	fn from( arr: [( Molecule, f64 ); N] ) -> Self {
		if N == 0 {
			return Self::default();
		}

		Self( BTreeMap::from( arr ) )
	}
}

#[cfg( feature = "tex" )]
impl Latex for GasComposition {
	fn to_latex( &self ) -> String {
		let mut tmp = self.clone();

		tmp.0.insert( Molecule::Other, self.other() );

		tmp.0.iter()
			.map( |( k, v )| format!( r"{}\,\qty{{{:.1}}}{{\percent}}", k.to_latex(), v * 100.0 ) )
			.collect::<Vec<String>>()
			.join( ", " )
	}
}


/// Represents the quality of an existing atmosphere.
#[derive( Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Debug )]
pub enum AtmosphereQuality {
	/// The atmosphere is breathable to humans.
	Breathable,

	/// The atmosphere is non-toxic for humans, but they need oxygen masks, to support themselves.
	NonToxic,

	/// The atmosphere is toxic for humans and they need protective gear with a full air supply.
	Toxic,
}

impl AtmosphereQuality {
	#[cfg( feature = "tex" )]
	pub fn to_latex_symbol( &self ) -> String {
		match self {
			Self::Breathable => r"\symbOkay{}".to_string(),
			Self::NonToxic => r"\symbOxygen{}".to_string(),
			Self::Toxic => r"\symbDeadly{}".to_string(),
		}
	}
}

impl fmt::Display for AtmosphereQuality {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		match self {
			Self::Breathable => write!( f, "Atembar" ),
			Self::NonToxic => write!( f, "Ungiftig" ),
			Self::Toxic => write!( f, "Giftig" ),
		}
	}
}


/// Representing Molekules in an atmosphere.
#[derive( Serialize, Deserialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug )]
pub enum Molecule {
	Ammonia,
	Argon,
	CarbonDioxide,
	CarbonMonoxide,
	Ethane,
	Helium,
	Hydrogen,
	Kalium,
	Methane,
	Natrium,
	Neon,
	Nitrogen,
	Oxygen,
	SulfurDioxide,
	SulfurMonoxide,
	Water,
	Other,
}

#[cfg( feature = "tex" )]
impl Latex for Molecule {
	fn to_latex( &self ) -> String {
		let res = match self {
			Self::Ammonia => r"\ce{NH3}",
			Self::Argon => r"\ce{Ar}",
			Self::CarbonDioxide => r"\ce{CO2}",
			Self::CarbonMonoxide => r"\ce{CO}",
			Self::Ethane => r"\ce{C2H6}",
			Self::Helium => r"\ce{He}",
			Self::Hydrogen => r"\ce{H}",
			Self::Kalium => r"\ce{K}",
			Self::Methane => r"\ce{CH4}",
			Self::Natrium => r"\ce{Na}",
			Self::Neon => r"\ce{Ne}",
			Self::Nitrogen => r"\ce{N2}",
			Self::Oxygen => r"\ce{O2}",
			Self::SulfurDioxide => r"\ce{SO2}",
			Self::SulfurMonoxide => r"\ce{SO}",
			Self::Water => r"\ce{H2O}",
			Self::Other => r"andere",
		};

		res.to_string()
	}
}

impl fmt::Display for Molecule {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		match self {
			Self::Ammonia =>        write!( f, "NH3" ),
			Self::Argon =>          write!( f, "Ar" ),
			Self::CarbonDioxide =>  write!( f, "CO2" ),
			Self::CarbonMonoxide => write!( f, "CO" ),
			Self::Ethane =>         write!( f, "C2H6" ),
			Self::Helium =>         write!( f, "He" ),
			Self::Hydrogen =>       write!( f, "H" ),
			Self::Kalium =>         write!( f, "K" ),
			Self::Methane =>        write!( f, "CH4" ),
			Self::Natrium =>        write!( f, "Na" ),
			Self::Neon =>           write!( f, "Ne" ),
			Self::Nitrogen =>       write!( f, "N2" ),
			Self::Oxygen =>         write!( f, "O2" ),
			Self::SulfurDioxide =>  write!( f, "SO2" ),
			Self::SulfurMonoxide => write!( f, "SO" ),
			Self::Water =>          write!( f, "H2O" ),
			Self::Other =>          write!( f, r"andere" ),
		}
	}
}
