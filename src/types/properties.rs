//! Provides types for celestial objects.




//=============================================================================
// Crates


use std::collections::{btree_map, BTreeMap};
use std::fmt;
use std::str::FromStr;
use std::sync::LazyLock;

#[cfg( feature = "i18n" )] use fluent_templates::Loader;
use regex::Regex;
use serde::{Serialize, Serializer, Deserialize, Deserializer};
use thiserror::Error;
#[cfg( feature = "i18n" )] use unic_langid::LanguageIdentifier;

use crate::units::Length;
#[cfg( feature = "tex" )] use crate::traits::Latex;
#[cfg( feature = "i18n" )] use crate::traits::Locale;
#[cfg( all( feature = "i18n", feature = "tex" ) )] use crate::traits::LocaleLatex;
#[cfg( feature = "i18n" )] use crate::LOCALES;

use super::CelestialBody;




//=============================================================================
// Errors


#[derive( Error, Debug )]
pub enum PropertiesError {
	#[error( "Cannot derive spectral class from string: {0}" )]
	SpectralClassFromStrError( String ),

	#[error( "Cannot derive localized text from string: {0}" )]
	LocalizedTextFromStrError( String ),
}




//=============================================================================
// Constants


/// The regular expression used to get star type and star type subdivision a string.
static REGEX_SPECTRAL_CLASS: LazyLock<Regex> = LazyLock::new( || {
	Regex::new( r"^(?<st>[A-Z]+)?((?<sdiv>\d+\.?\d*)?)$" ).unwrap()
} );




//=============================================================================
// Types


/// Possible colors of stars.
#[derive( Clone, Copy, Debug )]
pub enum StarColor {
	Blue,
	BluishWhite,
	White,
	YellowishWhite,
	Yellow,
	LightOrange,
	OrangishRed,
	Brown,
}

impl fmt::Display for StarColor {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		match self {
			Self::Blue => write!( f, "blue" ),
			Self::BluishWhite => write!( f, "bluish white" ),
			Self::White => write!( f, "white" ),
			Self::YellowishWhite => write!( f, "yellowish white" ),
			Self::Yellow => write!( f, "yellow" ),
			Self::LightOrange => write!( f, "orange" ),
			Self::OrangishRed => write!( f, "red" ),
			Self::Brown => write!( f, "brown" ),
		}
	}
}

#[cfg( feature = "i18n" )]
impl Locale for StarColor {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::Blue => LOCALES.lookup( locale, "blue" ),
			Self::BluishWhite => LOCALES.lookup( locale, "bluish-white" ),
			Self::White => LOCALES.lookup( locale, "white" ),
			Self::YellowishWhite => LOCALES.lookup( locale, "yellowish-white" ),
			Self::Yellow => LOCALES.lookup( locale, "yellow" ),
			Self::LightOrange => LOCALES.lookup( locale, "orange" ),
			Self::OrangishRed => LOCALES.lookup( locale, "red" ),
			Self::Brown => LOCALES.lookup( locale, "brown" ),
		}
	}
}


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
			Self::GravitationalCenter => write!( f, "gravitational center" ),
			Self::Star => write!( f, "star" ),
			Self::Trabant => write!( f, "trabant" ),
			Self::Ring => write!( f, "ring" ),
			Self::Station => write!( f, "station" ),
		}
	}
}

#[cfg( feature = "i18n" )]
impl Locale for BodyType {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::GravitationalCenter => LOCALES.lookup( locale, "gravitational-center" ),
			Self::Star => LOCALES.lookup( locale, "star" ),
			Self::Trabant => LOCALES.lookup( locale, "trabant" ),
			Self::Ring => LOCALES.lookup( locale, "ring" ),
			Self::Station => LOCALES.lookup( locale, "station" ),
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
			Self::Planet => write!( f, "planet" ),
			Self::Moon => write!( f, "moon" ),
		}
	}
}

#[cfg( feature = "i18n" )]
impl Locale for TrabantType {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::Planet => LOCALES.lookup( locale, "planet" ),
			Self::Moon => LOCALES.lookup( locale, "moon" ),
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

	/// Is occupied by the Sket.
	Sket,
}

impl fmt::Display for Affiliation {
	fn fmt( &self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
		let res = match self {
			Self::Union => "Union",
			Self::BorderWorld => "Border World",
			Self::Free => "Free Territories",
			Self::Uninhabited => "Uninhabited",
			Self::Sket => "Sket",
		};

		write!( f, "{res}" )
	}
}

#[cfg( feature = "i18n" )]
impl Locale for Affiliation {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::Union => LOCALES.lookup( locale, "Union" ),
			Self::BorderWorld => LOCALES.lookup( locale, "Border-World" ),
			Self::Free => LOCALES.lookup( locale, "Free-Territories" ),
			Self::Uninhabited => LOCALES.lookup( locale, "Uninhabited" ),
			Self::Sket => "Sket".to_string(),
		}
	}
}


/// Representing an institution based or provided.
#[derive( Serialize, Deserialize, PartialEq, Hash, Clone, Debug )]
pub enum Institution {
	/// A delegation of the Union space fleet is based.
	UnionFleet,
}

impl fmt::Display for Institution {
	fn fmt( &self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
		let res = match self {
			Self::UnionFleet => "Union Space Fleet",
		};

		write!( f, "{res}" )
	}
}

#[cfg( feature = "i18n" )]
impl Locale for Institution {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::UnionFleet => LOCALES.lookup( locale, "Union-Fleet" ),
		}
	}
}


/// Representing the orbit of a `CelestialBody` around another `CelestialBody`.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct Orbit {
	/// The semi major axis of the `object`'s orbit in AU.
	pub axis_semi_major: f64,

	/// The eccentricity of the `object`'s orbit.
	pub eccentricity: f64,

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
	pub fn eccentricity( &self ) -> f64 {
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
	pub fn radius_from_focal( &self, angle: f64 ) -> Length {
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

	fn color( &self ) -> StarColor {
		match self {
			Self::O => StarColor::Blue,
			Self::B => StarColor::BluishWhite,
			Self::A => StarColor::White,
			Self::F => StarColor::YellowishWhite,
			Self::G => StarColor::Yellow,
			Self::K => StarColor::LightOrange,
			Self::M => StarColor::OrangishRed,
			Self::T => StarColor::Brown,
			Self::DA
				| Self::DC
				| Self::DQ
				| Self::DZ => StarColor::White,
		}
	}

	/// Returns `true` if the star type refers to a white dwarf star.
	pub fn is_white_dwarf( &self ) -> bool {
		matches!( self, Self::DA | Self::DC | Self::DQ | Self::DZ )
	}
}

impl FromStr for StarType {
	type Err = PropertiesError;

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
			_ => return Err( PropertiesError::SpectralClassFromStrError( s.to_string() ) ),
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

		write!( f, "{res}" )
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

	/// Return the color type of the star.
	pub fn color( &self ) -> StarColor {
		self.star_type.color()
	}
}

impl FromStr for SpectralClass {
	type Err = PropertiesError;

	fn from_str( s: &str ) -> Result<Self, Self::Err> {
		let caps = REGEX_SPECTRAL_CLASS.captures( s )
			.ok_or( PropertiesError::SpectralClassFromStrError( s.to_string() ) )?;

		let star_type = caps.name( "st" )
			.ok_or_else( || PropertiesError::SpectralClassFromStrError( s.to_string() ) )?
			.as_str()
			.parse::<StarType>()?;

		let subdivision = match caps.name( "sdiv" ) {
			Some( x ) => {
				let sdiv = x.as_str()
					.parse::<f32>()
					.map_err( |_| PropertiesError::SpectralClassFromStrError( s.to_string() ) )?;
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


/// Policies that can be enforced.
#[derive( Serialize, Deserialize, Clone, PartialEq, Eq, Debug )]
pub enum Policy {
	/// Visiting this system is forbidden without a special permit.
	Restricted,

	/// Information about this system is considered a secret.
	Secret,
}

impl fmt::Display for Policy {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		match self {
			Self::Restricted => write!( f, "restricted" ),
			Self::Secret => write!( f, "secret" ),
		}
	}
}

#[cfg( feature = "i18n" )]
impl Locale for Policy {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::Restricted => LOCALES.lookup( locale, "restricted" ),
			Self::Secret => LOCALES.lookup( locale, "secret" ),
		}
	}
}


/// Special properties.
#[derive( Serialize, Deserialize, Clone, PartialEq, Eq, Debug )]
pub enum Property {
	/// A star with this property exhibit unusually violent flare activity. Flares occur sporadically, with successive flares spaced anywhere from an hour to a few days apart. Flares may emit up to 10'000 times the amount of radioactive radiation as a comparably sized flare on Sol. This would be lethal to any life forms on planets near the flare star.
	FlareStar,

	/// A white dwarf star.
	WhiteDwarf,

	/// A subgiant is a star that is brighter than a normal main-sequence star of the same spectral class, but not as bright as giant stars.
	Subgiant,

	/// A red giant is a luminous giant star of low mass (between 0.25 to 8 M☉).
	RedGiant,

	/// Higher-mass stars leave the main sequence to become blue giants, then bright blue giants, and then blue supergiants, before expanding into red supergiants.
	BlueGiant,
}

impl fmt::Display for Property {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		match self {
			Self::FlareStar => write!( f, "Flare Star" ),
			Self::WhiteDwarf => write!( f, "White Dwarf" ),
			Self::Subgiant => write!( f, "Subgiant" ),
			Self::RedGiant => write!( f, "Red Giant" ),
			Self::BlueGiant => write!( f, "Blue Giant" ),
		}
	}
}

#[cfg( feature = "i18n" )]
impl Locale for Property {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::FlareStar => LOCALES.lookup( locale, "Flare-Star" ),
			Self::WhiteDwarf => LOCALES.lookup( locale, "White-Dwarf" ),
			Self::Subgiant => LOCALES.lookup( locale, "Subgiant" ),
			Self::RedGiant => LOCALES.lookup( locale, "Red-Giant" ),
			Self::BlueGiant => LOCALES.lookup( locale, "Blue-Giant" ),
		}
	}
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
	pub fn other( &self ) -> f64 {
		let known: f64 = self.0.values().sum();

		1.0 - known
	}

	/// Return an iterator of `GasComposition`.
	pub fn iter( &self ) -> btree_map::Iter<Molecule, f64> {
		self.into_iter()
	}
}

impl IntoIterator for GasComposition {
	type Item = ( Molecule, f64 );
	type IntoIter = btree_map::IntoIter<Molecule, f64>;

	fn into_iter( self ) -> Self::IntoIter {
		self.0.into_iter()
	}
}

impl<'a> IntoIterator for &'a GasComposition {
	type Item = ( &'a Molecule, &'a f64 );
	type IntoIter = btree_map::Iter<'a, Molecule, f64>;
	fn into_iter( self ) -> Self::IntoIter {
		self.0.iter()
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

impl fmt::Display for GasComposition {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		let mut tmp = self.clone();

		tmp.0.insert( Molecule::Other, self.other() );

		tmp.0.iter()
			.map( |( k, v )| format!( r"{} {:.1}%", k, v * 100.0 ) )
			.collect::<Vec<String>>()
			.join( ", " );

		write!( f, "{tmp}" )
	}
}

#[cfg( feature = "i18n" )]
impl Locale for GasComposition {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		let mut tmp = self.clone();

		tmp.0.insert( Molecule::Other, self.other() );

		tmp.0.iter()
			.map( |( k, v )| format!( r"{} {:.1}%", k.to_string_locale( locale ), v * 100.0 ) )
			.collect::<Vec<String>>()
			.join( ", " )
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

#[cfg( all( feature = "i18n", feature = "tex" ) )]
impl LocaleLatex for GasComposition {
	fn to_latex_locale( &self, locale: &LanguageIdentifier ) -> String {
		let mut tmp = self.clone();

		tmp.0.insert( Molecule::Other, self.other() );

		tmp.0.iter()
			.map( |( k, v )| format!( r"{}\,\qty{{{:.1}}}{{\percent}}", k.to_latex_locale( locale ), v * 100.0 ) )
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
			Self::Breathable => write!( f, "breathable" ),
			Self::NonToxic => write!( f, "non-toxic" ),
			Self::Toxic => write!( f, "toxic" ),
		}
	}
}

#[cfg( feature = "i18n" )]
impl Locale for AtmosphereQuality {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::Breathable => LOCALES.lookup( locale, "breathable" ),
			Self::NonToxic => LOCALES.lookup( locale, "nontoxic" ),
			Self::Toxic => LOCALES.lookup( locale, "toxic" ),
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
			Self::Other =>          write!( f, r"other" ),
		}
	}
}

#[cfg( feature = "i18n" )]
impl Locale for Molecule {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::Other => LOCALES.lookup( locale, "other" ),
			_ => self.to_string(),
		}
	}
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
			Self::Other => r"other",
		};

		res.to_string()
	}
}

#[cfg( all( feature = "i18n", feature = "tex" ) )]
impl LocaleLatex for Molecule {
	fn to_latex_locale( &self, locale: &LanguageIdentifier ) -> String {
		match self {
			Self::Other => LOCALES.lookup( locale, "other" ),
			_ => self.to_latex(),
		}
	}
}


/// Representing Text that is possibly available in multiple languages.
///
/// There is always a fallback text, that is always available.
#[derive( Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Debug )]
pub struct LocalizedText {
	fallback: String,

	#[cfg( feature = "i18n" )]
	#[serde( default )]
	locales: BTreeMap<LanguageIdentifier, String>,
}

impl LocalizedText {
	/// Create a new instance of `LocalizedText` with `text` as fallback text.
	pub fn new( text: &str ) -> Self {
		Self {
			fallback: text.to_string(),

			#[cfg( feature = "i18n" )]
			locales: BTreeMap::new(),
		}
	}

	/// Create a new instance of `LocalizedText` from `self` with `lang` as language identifier following language IDs and `text` as language specific text.
	#[cfg( feature = "i18n" )]
	pub fn add_language( mut self, lang: LanguageIdentifier, text: &str ) -> Self {
		self.locales.insert( lang, text.to_string() );
		self
	}
}

impl FromStr for LocalizedText {
	type Err = PropertiesError;

	fn from_str( s: &str ) -> Result<Self, Self::Err> {
		let res = Self {
			fallback: s.to_string(),

			#[cfg( feature = "i18n" )]
			locales: Default::default(),
		};

		Ok( res )
	}
}

impl fmt::Display for LocalizedText {
	fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
		write!( f, "{}", self.fallback )
	}
}

#[cfg( feature = "i18n" )]
impl Locale for LocalizedText {
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String {
		self.locales.get( locale )
			.unwrap_or( &self.fallback )
			.clone()
	}
}




//=============================================================================
// Testing


#[cfg( test )]
mod tests {
	#[cfg( feature = "i18n" )] use unic_langid::langid;

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

	#[test]
	#[cfg( feature = "i18n" )]
	fn test_localized_text() {
		assert_eq!( LocalizedText::new( "Test" ).to_string(), "Test".to_string() );

		let text = LocalizedText::new( "Fallback Text" )
			.add_language( langid!( "de-DE" ), "Ausweichtext" );
		assert_eq!( text.to_string_locale( &langid!( "de-DE" ) ), "Ausweichtext".to_string() );
	}
}
