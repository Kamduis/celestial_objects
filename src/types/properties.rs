//! Provides types for celestial objects.




//=============================================================================
// Crates


use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;
use std::sync::LazyLock;

use regex::Regex;
use serde::{Serialize, Serializer, Deserialize, Deserializer};
use thiserror::Error;

#[cfg( feature = "tex" )] use crate::traits::Latex;
use crate::units::Length;

use super::CelestialBody;




//=============================================================================
// Errors


#[derive( Error, Debug )]
pub enum SpectralClassError {
	#[error( "Cannot derive type from string: {0}" )]
	FromStrError( String ),
}




//=============================================================================
// Constants


/// The regular expression used to get star type and star type subdivision a string.
static REGEX_SPECTRAL_CLASS: LazyLock<Regex> = LazyLock::new( || {
	Regex::new( r"^(?<st>[A-Z]+)?((?<sdiv>\d+\.?\d*)?)$" ).unwrap()
} );




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


/// The star type based on the spectral type. Only the letter part of the spectral class.
#[derive( Clone, PartialEq, Eq, PartialOrd, Ord, Debug )]
pub enum StarType {
	O,
	B,
	A,
	F,
	G,
	K,
	M,
	T,
	DA,
	DC,
	DQ,
	DZ,
}

impl StarType {
	/// Returns all available star types.
	pub const ALL: [Self; 12] = [
		Self::O,
		Self::B,
		Self::A,
		Self::F,
		Self::G,
		Self::K,
		Self::M,
		Self::T,
		Self::DA,
		Self::DC,
		Self::DQ,
		Self::DZ,
	];

	/// Returns `true` if the star type refers to a white dwarf star.
	pub fn is_white_dwarf( &self ) -> bool {
		match self {
			Self::DA | Self::DC | Self::DQ | Self::DZ => true,
			_ => false,
		}
	}
}

impl FromStr for StarType {
	type Err = SpectralClassError;

	fn from_str( s: &str ) -> Result<Self, Self::Err> {
		let res = match s.to_uppercase().as_str() {
			"O" => Self::O,
			"B" => Self::B,
			"A" => Self::A,
			"F" => Self::F,
			"G" => Self::G,
			"K" => Self::K,
			"M" => Self::M,
			"T" => Self::T,
			"DA" => Self::DA,
			"DC" => Self::DC,
			"DQ" => Self::DQ,
			"DZ" => Self::DZ,
			_ => return Err( SpectralClassError::FromStrError( s.to_string() ) ),
		};

		Ok( res )
	}
}

impl fmt::Display for StarType {
	fn fmt( &self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
		let res = match self {
			Self::O => "O",
			Self::B => "B",
			Self::A => "A",
			Self::F => "F",
			Self::G => "G",
			Self::K => "K",
			Self::M => "M",
			Self::T => "T",
			Self::DA => "DA",
			Self::DC => "DC",
			Self::DQ => "DQ",
			Self::DZ => "DZ",
		};

		write!( f, "{}", res )
	}
}


/// The spectral class of a star.
#[derive( Clone, PartialEq, PartialOrd, Debug )]
pub struct SpectralClass {
	star_type: StarType,
	subdivision: Option<f32>,
}

impl SpectralClass {
	/// Create a new `SpectralClass` from type and `subdivision`. If `tar_type` is one of the white dwarf types, `subdivision` is ignored.
	pub fn new( star_type: StarType, subdivision: f32 ) -> Self {
		let sdiv = match star_type {
			StarType::DA | StarType::DC | StarType::DQ | StarType::DZ => None,
			_ => Some( subdivision )
		};

		Self {
			star_type,
			subdivision: sdiv,
		}
	}

	/// Returns the type of star.
	pub fn type_star( &self ) -> &StarType {
		&self.star_type
	}
}

impl FromStr for SpectralClass {
	type Err = SpectralClassError;

	fn from_str( s: &str ) -> Result<Self, Self::Err> {
		let caps = REGEX_SPECTRAL_CLASS.captures( s )
			.ok_or( SpectralClassError::FromStrError( s.to_string() ) )?;

		let star_type = caps.name( "st" )
			.ok_or_else( || SpectralClassError::FromStrError( s.to_string() ) )?
			.as_str()
			.parse::<StarType>()?;

		let subdivision = match caps.name( "sdiv" ) {
			Some( x ) => {
				let sdiv = x.as_str()
					.parse::<f32>()
					.map_err( |_| SpectralClassError::FromStrError( s.to_string() ) )?;
					Some( sdiv )
			},
			None => None,
		};

		let res = Self {
			star_type,
			subdivision,
		};

		Ok( res )
	}
}

impl fmt::Display for SpectralClass {
	fn fmt( &self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
		match self.subdivision {
			Some( x ) if x.fract() == 0.0 => write!( f, "{}{}", self.star_type, x ),
			Some( x ) => write!( f, "{}{:.1}", self.star_type, x ),
			None => write!( f, "{}", self.star_type ),
		}
	}
}

impl Serialize for SpectralClass {
	fn serialize<S>( &self, serializer: S ) -> Result<S::Ok, S::Error>
		where S: Serializer
	{
		serializer.serialize_str( &self.to_string() )
	}
}

impl<'de> Deserialize<'de> for SpectralClass {
	fn deserialize<D>( deserializer: D ) -> Result<Self, D::Error>
		where D: Deserializer<'de>
	{
		let s = String::deserialize( deserializer )?;
		FromStr::from_str( &s ).map_err( serde::de::Error::custom )
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




//=============================================================================
// Testing


#[cfg( test )]
mod tests {
	use super::*;

	#[test]
	fn test_spectral_class_from_str() {
		assert_eq!( "O3".parse::<SpectralClass>().unwrap(), SpectralClass::new( StarType::O, 3.0 ) );
		assert_eq!( "G3.5".parse::<SpectralClass>().unwrap(), SpectralClass::new( StarType::G, 3.5 ) );
	}

	#[test]
	fn test_spectral_class_deserialize() {
		assert_eq!( ron::from_str::<SpectralClass>( r#""O3""# ).unwrap(), SpectralClass::new( StarType::O, 3.0 ) );
		assert_eq!( ron::from_str::<SpectralClass>( r#""G3.5""# ).unwrap(), SpectralClass::new( StarType::G, 3.5 ) );
	}
}
