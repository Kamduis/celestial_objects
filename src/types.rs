//! Provides types for celestial objects.




//=============================================================================
// Crates


use serde::{Serialize, Deserialize};
use thiserror::Error;

use crate::coords::EquatorialCoords;




//=============================================================================
// Constants


/// Letters
const LETTERS: [&str; 26] = [
	"a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
	"k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
	"u", "v", "w", "x", "y", "z",
];


/// Greek letters
const LETTERS_GREEK: [&str; 28] = [
	"α", "β", "γ", "δ", "ε", "ζ", "η", "κ", "μ", "ρ",
	"σ", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π",
	"ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω",
];




//=============================================================================
// Errors


#[derive( Error, Debug )]
pub enum CelestialSystemError {
	#[error( "No celestial body at index: `{0}`" )]
	IllegalIndex( String ),

	#[error( "The body at index `{0}` is not a star." )]
	NotAStar( String ),

	#[error( "There is no star present in the system." )]
	NoStarPresent,
}




//=============================================================================
// Traits


pub trait AstronomicalObject {
	/// Return a new object from `self` with `name`.
	fn with_name( self, name: &str ) -> Self;

	/// Return a new object from `self` with `satellites` orbiting it.
	fn with_satellites( self, satellites: Vec<Orbit> ) -> Self;

	/// Returns the satellites of this object.
	fn satellites( &self ) -> &Vec<Orbit>;

	/// Returns the mass of the astronomical object.
	fn mass( &self ) -> f32;

	/// Returns the radius of the astronomical object in meter.
	fn radius( &self ) -> f32;
}




//=============================================================================
// Helper functions


/// Get the `index`th object orbiting `center`.
///
/// **Note:** `index` is *not* 0-based but 1-based. `index = 1` provides the first body orbiting `center`. `index = 0` provides `center`.
///
/// # Returns
/// * First item of tuple: The `CelestialBody` at `index`.
/// * Second item of tuple: The letter hierarchy to be used as name, if no dedicated name exists.
fn satellite_getter<'a>(
	center: &'a CelestialBody,
	index: &'a [usize]
) -> Result<&'a CelestialBody, CelestialSystemError> {
	if index.is_empty() {
		return Err( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) );
	}

	if index[0] == 0 {
		return Ok( center );
	}

	let orbit = &center.satellites().get( index[0] - 1 )
		.ok_or( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) )?;

	if index.len() == 1 {
		return Ok( &orbit.body );
	}

	satellite_getter( &orbit.body, &index[1..] )
}


/// Get the `index`th object orbiting `center`.
///
/// **Note:** `index` is *not* 0-based but 1-based. `index = 1` provides the first body orbiting `center`. `index = 0` provides `center`.
///
/// # Returns
/// * First item of tuple: The `CelestialBody` at `index`.
/// * Second item of tuple: The letter hierarchy to be used as name, if no dedicated name exists.
fn satellite_getter_hierarchical<'a>( center: &'a CelestialBody, index: &'a [usize], hierarchy: &str ) -> Result<( &'a CelestialBody, String ), CelestialSystemError> {
	if index.is_empty() {
		return Err( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) );
	}

	if index[0] == 0 {
		return Ok( ( center, hierarchy.to_string() ) );
	}

	let orbit = &center.satellites().get( index[0] - 1 )
		.ok_or( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) )?;

	let hierarchy_letter = match ( center, &orbit.body ) {
		( _, CelestialBody::GravitationalCenter( _ ) ) => unimplemented!( "Gravitational center not expected!" ),
		( _, CelestialBody::Star( _ ) ) => LETTERS[index[0] - 1].to_uppercase(),
		( CelestialBody::Trabant( _ ), CelestialBody::Trabant( _ ) ) => LETTERS_GREEK[index[0] - 1].to_string(),
		( _, CelestialBody::Trabant( _ ) ) => LETTERS[index[0] - 1].to_string(),
		( _, CelestialBody::Station( _ ) ) => index[0].to_string(),
	};

	let hierarchy_new = format!( "{}{}", hierarchy, hierarchy_letter );

	if index.len() == 1 {
		return Ok( ( &orbit.body, hierarchy_new ) );
	}

	satellite_getter_hierarchical( &orbit.body, &index[1..], &hierarchy_new )
}


/// Get the main star `center`. This is a recursive function walking through the hierarchy until a star is encountered which is then returned. Must be called with `do_stop` being `false`.
///
/// # Arguments
/// * `center` the center object to look for stars orbiting it.
/// * `do_stop` If a star has been encountered, stop walking through the hierarchy and return the star.
///
/// # Returns
/// The first `CelestialBody` encountered. If no star has been encountered, this returns `None`.
fn get_main_star<'a>( center: &'a CelestialBody ) -> Option<&'a CelestialBody> {
	if let CelestialBody::Star( _ ) = center {
		return Some( center );
	}

	for sat in center.satellites() {
		if let Some( x ) = get_main_star( &sat.body ) {
			return Some( x );
		}
	}

	None
}




//=============================================================================
// Enums


/// Classification of celestial bodies without further information.
#[derive( Clone, Copy, PartialEq, Debug )]
pub enum BodyType {
	/// A point in space that is the gravitational center of two masses orbiting each other.
	GravitationalCenter,
	Star,
	Trabant,
	Station,
}

impl From<&CelestialBody> for BodyType {
	fn from( item: &CelestialBody ) -> Self {
		match item {
			CelestialBody::GravitationalCenter( _ ) => Self::GravitationalCenter,
			CelestialBody::Star( _ ) => Self::Star,
			CelestialBody::Trabant( _ ) => Self::Trabant,
			CelestialBody::Station( _ ) => Self::Station,
		}
	}
}


/// Classification of celestial bodies.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub enum CelestialBody {
	/// A point in space that is the gravitational center of two masses orbiting each other.
	GravitationalCenter( GravitationalCenter ),

	Star( Star ),

	Trabant( Trabant ),

	Station( Station ),
}

impl AstronomicalObject for CelestialBody {
	/// Return a new `CelestialBody` from `self` with `name`.
	fn with_name( self, name: &str ) -> Self {
		match self {
			Self::GravitationalCenter( x ) => Self::GravitationalCenter( x.with_name( name ) ),
			Self::Star( x ) => Self::Star( x.with_name( name ) ),
			Self::Trabant( x ) => Self::Trabant( x.with_name( name ) ),
			Self::Station( x ) => Self::Station( x.with_name( name ) ),
		}
	}

	/// Return a new `CelestialBody` from `self` with `satellites`.
	fn with_satellites( self, satellites: Vec<Orbit> ) -> Self {
		match self {
			Self::GravitationalCenter( x ) => Self::GravitationalCenter( x.with_satellites( satellites ) ),
			Self::Star( x ) => Self::Star( x.with_satellites( satellites ) ),
			Self::Trabant( x ) => Self::Trabant( x.with_satellites( satellites ) ),
			Self::Station( x ) => Self::Station( x.with_satellites( satellites ) ),
		}
	}

	fn satellites( &self ) -> &Vec<Orbit> {
		match self {
			Self::GravitationalCenter( x ) => x.satellites(),
			Self::Star( x ) => x.satellites(),
			Self::Trabant( x ) => x.satellites(),
			Self::Station( x ) => x.satellites(),
		}
	}

	fn mass( &self ) -> f32 {
		match self {
			Self::GravitationalCenter( x ) => x.mass(),
			Self::Star( x ) => x.mass(),
			Self::Trabant( x ) => x.mass(),
			Self::Station( x ) => x.mass(),
		}
	}

	fn radius( &self ) -> f32 {
		match self {
			Self::GravitationalCenter( x ) => x.radius(),
			Self::Star( x ) => x.radius(),
			Self::Trabant( x ) => x.radius(),
			Self::Station( x ) => x.radius(),
		}
	}
}




//=============================================================================
// Structs


/// A `CelestialSystem` is the framework around a hierarchy of nested `CelestialBody`s.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct CelestialSystem {
	/// The identifier of the celestial system. Old catalog names, mostly of the main star within this system, if it is a system with multiple stars.
	identifier: String,

	/// The name as the system, as it is known to the people.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	name: Option<String>,

	/// The coordinates of the `CelestialSystem` in equatorial coordinates. These coordinates assume Epoch J2000.0 and  represent the systems position ([equatorial coordinate system](https://en.wikipedia.org/wiki/Equatorial_coordinate_system)).
	coordinates: EquatorialCoords,

	/// The political affiliation of the system.
	affiliation: Affiliation,

	/// An optional description of this system.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	description: Option<String>,

	/// The main body of this system. This `CelestialBody` may have one or more bodies orbiting it. Systems with a single sun as it's center have a `CelestialBody` representing this sun as `body`. Multi-star-systems will have either the main star as `body` (if it is much more massive than the other stars or a `GravitationalCenter` as the theoretical body, that is then orbited by multiple stars.
	body: CelestialBody,
}

impl CelestialSystem {
	/// Create a new `CelestialSystem`.
	pub fn new( identifier: &str, coordinates: &EquatorialCoords, body: CelestialBody ) -> Self {
		Self {
			identifier: identifier.to_string(),
			name: None,
			coordinates: coordinates.clone(),
			affiliation: Default::default(),
			description: None,
			body,
		}
	}

	/// Return a new object from `self` with `name`.
	pub fn with_name( mut self, name: &str ) -> Self {
		self.name = Some( name.to_string() );
		self
	}

	/// Return a new object from `self` with `affiliation`.
	pub fn with_affiliation( mut self, affiliation: Affiliation ) -> Self {
		self.affiliation = affiliation;
		self
	}

	/// Return a new object from `self` with `affiliation`.
	pub fn with_description( mut self, desc: &str ) -> Self {
		self.description = Some( desc.to_string() );
		self
	}

	/// Returns the identifier of the `CelestialSystem`.
	pub fn identifier( &self ) -> &str {
		&self.identifier
	}

	/// Returns the name of the objects within this celestial system. Indexing is done by array slice.
	///
	/// `&[]` represents the system itself.
	/// `&[0]` represents the root object of the system. For a singular star system, this is the star. Forr a binary star system, this is the gravitational center of the two stars.
	/// `&[1]` represents the first object orbiting `&[0]`.
	/// `&[2]` represents the second object orbiting `&[0]`.
	/// `&[1,0]` represents the first object orbiting `&[0]` (identical to `&[1]`).
	/// `&[1,1]` represents the first object orbiting `&[1]` (the first object orbiting `&[0]`)
	pub fn name( &self, index: &[usize] ) -> Result<String, CelestialSystemError> {
		if index.is_empty() {
			return Ok( self.name.as_ref().cloned().unwrap_or( self.identifier.to_string() ) );
		}

		if index[0] == 0 {
			match &self.body {
				CelestialBody::GravitationalCenter( _ ) => {
					return Ok( format!( "{} AB", self.name( &[] )? ) );
				},
				CelestialBody::Star( x ) => {
					// If no name is given, is the name of the central star equal to the name of the system.
					return Ok( x.name.as_ref().cloned().unwrap_or( self.name( &[] )? ) );
				},
				_ => unimplemented!( "Center bodies should never by planets, moons or stations." ),
			}
		} else {
			let ( body_got, hierarchy ) = &satellite_getter_hierarchical( &self.body, &index, "" )?;

			let name = match &body_got {
				CelestialBody::GravitationalCenter( _ ) => unreachable!( "No gravitational center expected." ),
				CelestialBody::Star( x ) => x.name.as_ref(),
				CelestialBody::Trabant( x ) => x.name.as_ref(),
				CelestialBody::Station( x ) => x.name.as_ref(),
			};

			let res = name
				.cloned()
				.unwrap_or( format!( "{} {}", self.name( &[0] )?, hierarchy ) );

			return Ok( res )
		}
	}

	/// Returns the equatorial coordinates of this `CelestialSystem`.
	pub fn coordinates( &self ) -> &EquatorialCoords {
		&self.coordinates
	}

	/// Returns the political affiliation of the system.
	pub fn affiliation( &self ) -> &Affiliation {
		&self.affiliation
	}

	/// Returns the description of the system.
	pub fn description( &self ) -> Option<&str> {
		self.description.as_ref().map( |x| x.as_str() )
	}

	/// Returns the body type of the indexed object.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	pub fn body_type( &self, index: &[usize] ) -> Result<BodyType, CelestialSystemError> {
		if index.is_empty() {
			return Err( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) );
		}

		if index[0] == 0 {
			return Ok( BodyType::from( &self.body ) );
		}

		let body_got = &satellite_getter( &self.body, &index )?;
		Ok( BodyType::from( *body_got ) )
	}

	/// Returns the radius (in relation to Sol) of the indexed object.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// If the system index is used (`&[]`), this method returns the radius of the main star.
	pub fn radius( &self, index: &[usize] ) -> Result<f32, CelestialSystemError> {
		if index.is_empty() {
			match &self.body {
				CelestialBody::GravitationalCenter( _ ) => {
					let Some( body ) = get_main_star( &self.body ) else {
						return Err( CelestialSystemError::NoStarPresent );
					};
					return Ok( body.radius() );
				},
				_ => return Ok( self.body.radius() ),
			}
		}

		if index[0] == 0 {
			return Ok( self.body.radius() );
		}

		let body_got = &satellite_getter( &self.body, &index )?;
		Ok( body_got.radius() )
	}

	/// Returns the spectral class of the indexed object.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// If the system index is used (`&[]`), this method returns the spectral class of the main star.
	pub fn spectral_class<'a>( &'a self, index: &'a [usize] ) -> Result<&'a str, CelestialSystemError> {
		if index.is_empty() {
			match &self.body {
				CelestialBody::GravitationalCenter( _ ) => {
					let Some( body ) = get_main_star( &self.body ) else {
						return Err( CelestialSystemError::NoStarPresent );
					};
					let CelestialBody::Star( ref star ) = body else {
						unreachable!( "`get_main_star()` should only ever return stars." );
					};
					return Ok( star.spectral_class.as_str() );
				},
				CelestialBody::Star( x ) => return Ok( &x.spectral_class ),
				_ => unreachable!( "Only gravitational centers or stars should be the center object of a planetary system." ),
			}
		}

		if index[0] == 0 {
			let CelestialBody::Star( ref star ) = self.body else {
				return Err( CelestialSystemError::NotAStar( format!( "{:?}", index ) ) );
			};
			return Ok( &star.spectral_class );
		}

		let body_got = &satellite_getter( &self.body, &index )?;
		let CelestialBody::Star( star ) = body_got else {
			return Err( CelestialSystemError::NotAStar( format!( "{:?}", index ) ) );
		};
		return Ok( &star.spectral_class );
	}

	/// Returns the mass of this system's main star in relation to Sol.
	pub fn mass_main_star( &self ) -> f32 {
		let star_main = self.stars().nth( 0 )
			.expect( "Each system should have at least one star." );
		star_main.mass()
	}

	/// Returns the spectral class of this system's main star.
	pub fn stars( &self ) -> CelestialSystemStarsIterator {
		let mut iter_obj = CelestialSystemStarsIterator {
			body: &self.body,
			stars: Vec::new(),
			index: 0,
		};

		iter_obj.walker( &self.body );

		iter_obj
	}
}


/// Iterator for stars within a `CelestialSystem`.
///
/// TODO: The implementation is very inefficient, creating a `Vec` each time the iterator is newly created.
pub struct CelestialSystemStarsIterator<'a> {
	/// The central body of the system.
	body: &'a CelestialBody,

	stars: Vec<&'a Star>,

	index: usize,
}

impl<'a> CelestialSystemStarsIterator<'a> {
	/// Walking all objects within this system and collecting stars.
	fn walker( &mut self, body: &'a CelestialBody ) {
		match body {
			// A star may orbit a gravitational center.
			CelestialBody::GravitationalCenter( x ) => {
				for sat in &x.satellites {
					self.walker( &sat.body );
				}

				return;
			},

			// A star may orbit another star.
			CelestialBody::Star( x ) => {
				self.stars.push( x );

				for sat in &x.satellites {
					self.walker( &sat.body );
				}

				return;
			},

			// No star will be orbiting a trabant or station.
			_ => return,
		}
	}
}

impl<'a> Iterator for CelestialSystemStarsIterator<'a> {
	type Item = &'a Star;

	fn next( &mut self ) -> Option<Self::Item> {
		if self.index >= self.stars.len() {
			return None;
		}

		let result = Some( self.stars[ self.index ] ) ;
		self.index += 1;

		result
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

	/// The objects orbiting.
	pub body: CelestialBody,
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
}


/// Representing the theoretical gravitational center of two heavy masses orbiting each other.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct GravitationalCenter {
	/// The objects orbiting this gravitational center.
	pub(super) satellites: Vec<Orbit>,
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

	fn satellites( &self ) -> &Vec<Orbit> {
		&self.satellites
	}

	fn mass( &self ) -> f32 {
		0.0
	}

	/// Returns the star's radius with respect to the radius of Sol.
	fn radius( &self ) -> f32 {
		0.0
	}
}


/// Representing a star of a planetary system.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct Star {
	/// The name of this star. If this is `None`, the star will be named by its hierarchy within the `CelestialSystem`.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	pub(super) name: Option<String>,

	/// The mass in relation to the mass of Sol.
	pub(super) mass: f32,

	/// The radius in relation to the radius of Sol.
	pub(super) radius: f32,

	/// The luminosity in relation to the luminosity of Sol.
	pub(super) luminosity: f32,

	/// The spectral class.
	pub(super) spectral_class: String,

	/// Special properties of the star.
	#[serde( default )]
	properties: Vec<StarProperty>,

	/// The objects oribitng this star.
	pub(super) satellites: Vec<Orbit>,
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
			properties: Vec::new(),
			satellites: Vec::new(),
		}
	}

	/// Returns the spectral class of the star.
	pub fn spectral_class( &self ) -> &str {
		&self.spectral_class
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

	fn satellites( &self ) -> &Vec<Orbit> {
		&self.satellites
	}

	fn mass( &self ) -> f32 {
		self.mass
	}

	/// Returns the star's radius with respect to the radius of Sol.
	fn radius( &self ) -> f32 {
		self.radius
	}
}


/// Representing a trabant. This could be a planet in the orbit of a star of a moon in the orbit of a planet.
#[derive( Serialize, Deserialize, Clone, PartialEq, Default, Debug )]
pub struct Trabant {
	/// The name of this trabant. If this is `None`, the trabant will be named by its hierarchy within the `CelestialSystem`.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	pub(super) name: Option<String>,

	/// The radius in relation to the radius of Terra.
	pub(super) radius: f32,

	/// The surface gravity of this trabant in relation to the surface gravity of Terra.
	pub(super) gravity: f32,

	/// The objects oribitng this trabant.
	pub(super) satellites: Vec<Orbit>,
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

	fn satellites( &self ) -> &Vec<Orbit> {
		&self.satellites
	}

	fn mass( &self ) -> f32 {
		self.gravity * self.radius.powi( 2 )
	}

	/// Returns the star's radius with respect to the radius of Sol.
	fn radius( &self ) -> f32 {
		self.radius
	}
}


/// Representing a space station.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct Station {
	/// The name of this station.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	pub(super) name: Option<String>,

	/// The mass in kg.
	pub(super) mass: f32,

	/// The radius in meter.
	pub(super) radius: f32,

	/// The gravity within the station in relation to the surface gravity of Terra.
	pub(super) gravity: f32,

	/// The objects oribitng this station.
	pub(super) satellites: Vec<Orbit>,
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

	fn satellites( &self ) -> &Vec<Orbit> {
		&self.satellites
	}

	fn mass( &self ) -> f32 {
		self.mass
	}

	/// Returns the star's radius with respect to the radius of Sol.
	fn radius( &self ) -> f32 {
		self.radius
	}
}




//=============================================================================
// Testing


#[cfg( test )]
mod tests {
	// use super::*;

	use crate::tests::systems_examples;

	#[test]
	fn iterator_of_stars() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert_eq!( sol.stars().count(), 1 );

		let centauri = &systems[1];

		assert_eq!( centauri.stars().count(), 3 );
	}

	#[test]
	fn test_identifier() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert_eq!( sol.identifier(), "Sol" );

		let centauri = &systems[1];

		assert_eq!( centauri.identifier(), "Alpha Centauri" );
	}

	#[test]
	fn test_names() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert_eq!( sol.name( &[] ).unwrap(), "Sol" );  // <- The system itself
		assert_eq!( sol.name( &[0] ).unwrap(), "Sol" );  // <- The singular star
		assert_eq!( sol.name( &[1] ).unwrap(), "Mercury" );  // <- The first planet
		assert_eq!( sol.name( &[2] ).unwrap(), "Venus" );  // <- The second planet
		assert_eq!( sol.name( &[3] ).unwrap(), "Terra" );  // <- The third planet
		assert_eq!( sol.name( &[3,0] ).unwrap(), "Terra" );  // <- The third planet
		assert_eq!( sol.name( &[3,1] ).unwrap(), "Luna" );  // <- The first moon of the third planet

		let centauri = &systems[1];

		assert_eq!( centauri.name( &[] ).unwrap(), "Centauri" );  // <- The system itself
		assert_eq!( centauri.name( &[0] ).unwrap(), "Centauri AB" );  // <- Gravitational center of the trinary star system.
		assert_eq!( centauri.name( &[1] ).unwrap(), "Centauri AB A" );  // The first star
		assert_eq!( centauri.name( &[2] ).unwrap(), "Centauri AB B" );  // The second star
		assert_eq!( centauri.name( &[3] ).unwrap(), "Proxima" );  // The third star
		assert_eq!( centauri.name( &[1,0] ).unwrap(), "Centauri AB A" );  // The first star
		assert_eq!( centauri.name( &[1,1] ).unwrap(), "Minos" );  // The first planet of the first star
		assert_eq!( centauri.name( &[2,1] ).unwrap(), "Taurus" );  // The first planet of the second star
	}
}
