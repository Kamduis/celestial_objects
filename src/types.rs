//! Provides types for celestial objects.




//=============================================================================
// Crates


use std::fmt;

use chrono::TimeDelta;
use glam::Vec3;
use serde::{Serialize, Deserialize};
use thiserror::Error;

use crate::coords::EquatorialCoords;
use crate::calc;
use crate::units::{Mass, Length};




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


/// Ringed numbers.
const NUMBERS_RINGED: [&str; 21] = [
	"⓪", "①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨",
	"⑩", "⑪", "⑫", "⑬", "⑭", "⑮", "⑯", "⑰", "⑱", "⑲",
	"⑳",
];




//=============================================================================
// Errors


#[derive( Error, PartialEq, Debug )]
pub enum CelestialSystemError {
	#[error( "No celestial body at index: `{0}`" )]
	IllegalIndex( String ),

	#[error( "Body at index `{0}` does not have a center object." )]
	NoCenterObject( String ),

	#[error( "The object at index `{0}` is a gravitational center, not a real object." )]
	NotARealObject( String ),

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
	fn satellites( &self ) -> &[Orbit];

	/// Returns the mass of the astronomical object.
	fn mass( &self ) -> Mass;

	/// Returns the radius of the astronomical object in meter.
	fn radius( &self ) -> Length;

	/// Returns the surface gravity of the astronomical object in m/s².
	///
	/// This method returns `None` if the astronomical object has no sensible surface gravitation. A G2 star (loke Sol) for example has no surface, so it would return `None`.
	fn gravitation( &self ) -> Option<f32>;

	/// Returns the duration of one rotation of the `AstronomicalObject`. If it's rotation is locked, this method returns `None`.
	fn rotation_period( &self ) -> Option<TimeDelta> {
		None
	}
}




//=============================================================================
// Helper functions


/// Get the `index`th orbit around `center`.
///
/// **Note:** `index` is *not* 0-based but 1-based. `index = 1` provides the first body orbiting `center`. `index = 0` provides `center`.
///
/// # Returns
/// * The `CelestialBody` at `index`.
fn orbit_getter<'a>(
	center: &'a CelestialBody,
	index: &'a [usize]
) -> Result<&'a Orbit, CelestialSystemError> {
	dbg!( index );
	if index.is_empty() || index[0] == 0 {
		return Err( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) );
	}
	let orbit = &center.satellites().get( index[0] - 1 )
		.ok_or( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) )?;

	// An index ending in 0 (`&[3,0]`) is identical to the same index without the 0 (`&[3]`).
	if index.len() == 1 || ( index.len() == 2 && index[1] == 0 ) {
		return Ok( &orbit );
	}

	orbit_getter( &orbit.body, &index[1..] )
}


/// Get the `index`th object orbiting `center`.
///
/// **Note:** `index` is *not* 0-based but 1-based. `index = 1` provides the first body orbiting `center`. `index = 0` provides `center`.
///
/// # Returns
/// The `CelestialBody` at `index`.
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
		( _, CelestialBody::Ring( _ ) ) => NUMBERS_RINGED[index[0]].to_string(),
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


/// Classification of celestial bodies.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub enum CelestialBody {
	/// A point in space that is the gravitational center of two masses orbiting each other.
	GravitationalCenter( GravitationalCenter ),

	Star( Star ),

	Trabant( Trabant ),

	Ring( Ring ),

	Station( Station ),
}

impl AstronomicalObject for CelestialBody {
	/// Return a new `CelestialBody` from `self` with `name`.
	fn with_name( self, name: &str ) -> Self {
		match self {
			Self::GravitationalCenter( x ) => Self::GravitationalCenter( x.with_name( name ) ),
			Self::Star( x ) => Self::Star( x.with_name( name ) ),
			Self::Trabant( x ) => Self::Trabant( x.with_name( name ) ),
			Self::Ring( _ ) => unimplemented!( "Rings don't support names currently." ),
			Self::Station( x ) => Self::Station( x.with_name( name ) ),
		}
	}

	/// Return a new `CelestialBody` from `self` with `satellites`.
	fn with_satellites( self, satellites: Vec<Orbit> ) -> Self {
		match self {
			Self::GravitationalCenter( x ) => Self::GravitationalCenter( x.with_satellites( satellites ) ),
			Self::Star( x ) => Self::Star( x.with_satellites( satellites ) ),
			Self::Trabant( x ) => Self::Trabant( x.with_satellites( satellites ) ),
			Self::Ring( _ ) => unimplemented!( "Rings don't support satellites." ),
			Self::Station( x ) => Self::Station( x.with_satellites( satellites ) ),
		}
	}

	fn satellites( &self ) -> &[Orbit] {
		match self {
			Self::GravitationalCenter( x ) => x.satellites(),
			Self::Star( x ) => x.satellites(),
			Self::Trabant( x ) => x.satellites(),
			Self::Ring( _ ) => &[], // Rings don't support satellites.
			Self::Station( x ) => x.satellites(),
		}
	}

	fn mass( &self ) -> Mass {
		match self {
			Self::GravitationalCenter( x ) => x.mass(),
			Self::Star( x ) => x.mass(),
			Self::Trabant( x ) => x.mass(),
			Self::Ring( _ ) => unimplemented!( "Rings don't support having a mass." ),
			Self::Station( x ) => x.mass(),
		}
	}

	fn radius( &self ) -> Length {
		match self {
			Self::GravitationalCenter( x ) => x.radius(),
			Self::Star( x ) => x.radius(),
			Self::Trabant( x ) => x.radius(),
			Self::Ring( x ) => x.width() / 2.0,
			Self::Station( x ) => x.radius(),
		}
	}

	fn gravitation( &self ) -> Option<f32> {
		match self {
			Self::GravitationalCenter( x ) => x.gravitation(),
			Self::Star( x ) => x.gravitation(),
			Self::Trabant( x ) => x.gravitation(),
			Self::Ring( _ ) => None,
			Self::Station( x ) => x.gravitation(),
		}
	}

	fn rotation_period( &self ) -> Option<TimeDelta> {
		match self {
			Self::GravitationalCenter( x ) => x.rotation_period(),
			Self::Star( x ) => x.rotation_period(),
			Self::Trabant( x ) => x.rotation_period(),
			Self::Ring( _ ) => None,
			Self::Station( x ) => x.rotation_period(),
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
	#[serde( default )]
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

	/// Returns all possible indices of objects in this celestial system.
	///
	/// Always returns at least `vec![vec![]]` (representing the system itself.
	///
	/// `vec![]` represents the system itself.
	/// `vec![0]` represents the root object of the system. For a singular star system, this is the star. For a binary star system, this is the gravitational center of the two stars.
	/// `vec![1]` represents the first object orbiting `vec![0]`.
	/// `vec![2]` represents the second object orbiting `vec![0]`.
	/// `vec![1,0]` represents the first object orbiting `vec![0]` (identical to `vec![1]`). <- Will not be part of the returned `Vec`, otherwise the object behind `vec![1]` would be counted twice.
	/// `vec![1,1]` represents the first object orbiting `vec![1]` (the first object orbiting `vec![0]`)
	pub fn indices( &self ) -> Vec<Vec<usize>> {
		let mut res = vec![ Vec::new() ];
		fn walker( center: &CelestialBody, indcs: &mut Vec<Vec<usize>> ) {
			let idx_new_level = indcs.last().unwrap().clone();
			for ( i, orb ) in center.satellites().iter().enumerate() {
				let mut idx_new = idx_new_level.clone();
				idx_new.push( i + 1 );
				indcs.push( idx_new );

				// Walk through this objects orbits.
				walker( &orb.body, indcs );
			}
		}

		walker( &self.body, &mut res );

		// Add the index for the center object.
		res.insert( 1, vec![ 0 ] );

		res
	}

	/// Returns all possible indices of satellites of an index in this celestial system.
	///
	/// If you need to know the indices of the moons and stations orbiting Terra, you would call:
	/// ```ignore
	/// assert_eq!( system.indices_satellites( &[3] ), vec![vec![3,1], vec![3,2]] );
	/// ```
	///
	/// If you need to know the indices of the planets orbiting Sol, you would call:
	/// ```ignore
	/// assert_eq!(
	///     system.indices_satellites( &[0] ),
	///     vec![vec![1], vec![2], vec![3], vec![4], vec![5], vec![6], vec![7], vec![8]]
	/// );
	/// ```
	///
	/// Trying to return the satellite indices of Mercury, not having any moons, would return an empty vector.
	/// ```ignore
	/// assert_eq!( system.indices_satellites( &[1] ), vec![] );
	/// ```
	pub fn indices_satellites( &self, index: &[usize] ) -> Result<Vec<Vec<usize>>, CelestialSystemError> {
		let body = satellite_getter( &self.body, index )?;

		let mut res = Vec::new();

		for ( i, _ ) in body.satellites().iter().enumerate() {
			let mut idx_new = if index == &[0] {
				Vec::new()
			} else {
				index.to_vec()
			};
			idx_new.push( i + 1 );
			res.push( idx_new );
		}

		Ok( res )
	}

	/// Returns the index of an object within this system with `name`.
	///
	/// # Returns
	/// If `name` cannot be found in the system, this method returns `None`.
	pub fn index_of( &self, name: &str ) -> Option<Vec<usize>> {
		self.indices().iter()
			.skip( 1 )  // Skipping the empty index (the system itself).
			.map( |x| ( x, self.name( x ).expect( "Index should have been existed!" ) ) )
			.find( |( _, y )| y == name )
			.map( |( x, _ )| x )
			.cloned()
	}

	/// Returns the identifier of the `CelestialSystem`.
	pub fn identifier( &self ) -> &str {
		&self.identifier
	}

	/// Returns the name of the objects within this celestial system. Indexing is done by array slice.
	///
	/// `&[]` represents the system itself.
	/// `&[0]` represents the root object of the system. For a singular star system, this is the star. For a binary star system, this is the gravitational center of the two stars.
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
				CelestialBody::Star( x ) => x.name.as_ref().map( |y| y.as_str() ),
				CelestialBody::Trabant( x ) => x.name.as_ref().map( |y| y.as_str() ),
				CelestialBody::Ring( _ ) => None,
				CelestialBody::Station( x ) => x.name.as_ref().map( |y| y.as_str() ),
			};

			let res = match name {
				Some( x ) => x.to_string(),
				None => format!( "{} {}", self.name( &[0] )?, hierarchy ),

			};

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
	///
	/// # Returns
	/// This method returns an error if `index` is `&[]`, since the system itself is not a body.
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

	/// Returns the orbit level of the indexed object.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// # Returns
	/// The level represents the number of center objects + 1.
	/// The main star of a single-star-system has level 0.
	/// Planets in a single-star-system have level 1.
	/// Moons in a single-star-system have level 2.
	pub fn level( &self, index: &[usize] ) -> usize {
		index.len() + 1
	}

	/// Returns the type of trabant of the indexed object.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// # Returns
	/// If the indexed object is not a `Trabant`, this method returns `Ok( None )`
	pub fn trabant_type( &self, index: &[usize] ) -> Result<Option<TrabantType>, CelestialSystemError> {
		let BodyType::Trabant = self.body_type( index )? else {
			return Ok( None );
		};

		let res = match self.body_type( &self.index_of_center_of( index )? )? {
			BodyType::Star => TrabantType::Planet,
			BodyType::Trabant => TrabantType::Moon,
			_ => unreachable!( "Trabants should only ever orbit stars or planets. This one does not!" ),
		};

		Ok( Some( res ) )
	}

	/// Returns the radius of the indexed object.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// If the system index is used (`&[]`), this method returns the radius of the main star.
	pub fn radius( &self, index: &[usize] ) -> Result<Length, CelestialSystemError> {
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

	/// Returns the mass of the indexed object.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// If the system index is used (`&[]`), this method returns the radius of the main star.
	pub fn mass( &self, index: &[usize] ) -> Result<Mass, CelestialSystemError> {
		if index.is_empty() {
			match &self.body {
				CelestialBody::GravitationalCenter( _ ) => {
					let Some( body ) = get_main_star( &self.body ) else {
						return Err( CelestialSystemError::NoStarPresent );
					};
					return Ok( body.mass() );
				},
				_ => return Ok( self.body.mass() ),
			}
		}

		if index[0] == 0 {
			return Ok( self.body.mass() );
		}

		let body_got = &satellite_getter( &self.body, &index )?;
		Ok( body_got.mass() )
	}

	/// Returns the surface gravitation of the indexed object.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	pub fn gravitation( &self, index: &[usize] ) -> Result<Option<f32>, CelestialSystemError> {
		if index.is_empty() {
			return Err( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) );
		}

		if index[0] == 0 {
			return Ok( self.body.gravitation() );
		}

		let body_got = satellite_getter( &self.body, &index )?;
		Ok( body_got.gravitation() )
	}

	/// Returns the luminosity of the indexed object relative to the luminosity Sol. Sol itself has therefor a `luminosity()` of 1.0.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	pub fn luminosity( &self, index: &[usize] ) -> Result<f32, CelestialSystemError> {
		if index.is_empty() {
			return Err( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) );
		}

		// if index[0] == 0 {
		// 	if let CelestialBody::Star( x ) = self.body {
		// 		return Ok( x.luminosity() );
		// 	}
		// }

		let body_got = satellite_getter( &self.body, &index )?;
		if let CelestialBody::Star( x ) = body_got {
			return Ok( x.luminosity() );
		}

		Err( CelestialSystemError::NotAStar( format!( "{:?}", index ) ) )
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

	/// Returns the semi major axis of the indexed object to the center it orbits.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// Since the system itself and the center object of the system are not orbiting anything, the indices `&[]` and `&[0]` are illegal and cause an error to be returned.
	pub fn axis_semi_major( &self, index: &[usize] ) -> Result<Length, CelestialSystemError> {
		if index.is_empty() || index[0] == 0 {
			return Err( CelestialSystemError::NoCenterObject( format!( "{:?}", index ) ) );
		}

		let orbit_got = orbit_getter( &self.body, &index )?;

		Ok( orbit_got.axis_semi_major() )
	}

	/// Returns the orbit of the indexed object to the center it orbits.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// Since the system itself and the center object of the system are not orbiting anything, the indices `&[]` and `&[0]` are illegal and cause an error to be returned.
	pub fn orbit<'a>( &'a self, index: &'a [usize] ) -> Result<&'a Orbit, CelestialSystemError> {
		if index.is_empty() || index[0] == 0 {
			return Err( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) );
		}

		orbit_getter( &self.body, &index )
	}

	/// Returns the orbital period of the indexed object around it's center.
	///
	/// If the object at `index` does not have an orbit center, this method returns an error.
	pub fn orbital_period( &self, index: &[usize] ) -> Result<TimeDelta, CelestialSystemError> {
		let axis_semi_major = self.axis_semi_major( index )?;
		let mass_center = self.mass( &self.index_of_center_of( index )? )?;
		let mass_orbiter = self.mass( index )?;

		let seconds = calc::orbital_period( axis_semi_major, mass_center, mass_orbiter );

		Ok( TimeDelta::seconds( seconds as i64 ) )
	}

	/// Returns the rotation period of the `index`ed object in `TimeDelta`.
	///
	/// If the indexed object's rotation is locked to the orbiting body, this is the same as the orbital period of the indexed object. An example of a moon with locked rotation is Luna orbiting Terra.
	/// If the object at `index` does not have an orbit center, this method returns an error.
	pub fn rotation_period( &self, index: &[usize] ) -> Result<TimeDelta, CelestialSystemError> {
		if index.is_empty() {
			return Err( CelestialSystemError::IllegalIndex( format!( "{:?}", index ) ) );
		}

		let body_got = satellite_getter( &self.body, &index )?;

		let res = match body_got.rotation_period() {
			Some( x ) => x,
			None => self.orbital_period( index )?,
		};

		Ok( res )
	}

	/// Returns the duration of the local day (solar day) of the indexed object in `TimeDelta`.
	///
	/// If the object at `index` is a world orbiting it's center (which is a star) with a locked rotation, this method returns `None`, since it has effectively a day of infinite length.
	/// If the object at `index` does not have an orbit center, this method returns an error.
	pub fn day_solar( &self, index: &[usize] ) -> Result<Option<TimeDelta>, CelestialSystemError> {
		let orbital_period = self.orbital_period( index )?;
		let sideral_day = self.rotation_period( index )?;

		let orbital_period_float = orbital_period.num_seconds() as f64 + ( orbital_period.subsec_nanos() as f64 / 1e9 );
		let sideral_day_float = sideral_day.num_seconds() as f64 + ( sideral_day.subsec_nanos() as f64 / 1e9 );

		if orbital_period_float == sideral_day_float {
			// Planets with a locked rotation around the sun have a solar day of infinite duration.
			let idx_cent = self.index_of_center_of( index )?;
			if let BodyType::Star = self.body_type( &idx_cent )? {
				return Ok( None );
			} else {
				return self.day_solar( &idx_cent );
			}
		}

		let res = ( orbital_period_float * sideral_day_float ) / ( orbital_period_float - sideral_day_float );

		Ok( Some(  TimeDelta::seconds( res as i64 ) ) )
	}

	/// Returns the index of the center object of the orbit of the object of `index`.
	///
	/// # Arguments
	/// * `index` See [`self.name()`].
	///
	/// Since the system itself and the center object of the system are not orbiting anything, the indices `&[]` and `&[0]` are illegal and cause an error to be returned.
	pub fn index_of_center_of( &self, index: &[usize] ) -> Result<Vec<usize>, CelestialSystemError> {
		if index.is_empty() || index[0] == 0 {
			return Err( CelestialSystemError::NoCenterObject( format!( "{:?}", index ) ) );
		}

		let mut idx = index.to_vec();
		if index[index.len()-1] == 0 {
			let _ = std::mem::replace( &mut idx[index.len()-2], 0 );
		} else {
			let _ = std::mem::replace( &mut idx[index.len()-1], 0 );
		}

		Ok( idx )
	}

	/// Returns the mass of this system's main star in kg.
	pub fn mass_main_star( &self ) -> Mass {
		let star_main = self.stars().nth( 0 )
			.expect( "Each system should have at least one star." );
		star_main.mass()
	}

	/// Returns an iterator of all stars within this system.
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
	pub(super) name: Option<String>,

	/// The mass in relation to the mass of Sol.
	pub(super) mass: f32,

	/// The radius in relation to the radius of Sol.
	pub(super) radius: f32,

	/// The luminosity in relation to the luminosity of Sol.
	pub(super) luminosity: f32,

	/// The spectral class.
	pub(super) spectral_class: String,

	/// The rotation period (sidereal time). This is the time duration it takes for the star to make a full rotation in relation to a fixed star.
	/// If this is `None`, this means the body's representation is gravitational bound around the object it orbits.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::timedelta_option" )]
	pub(super) rotation_period: Option<TimeDelta>,

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
	pub(super) name: Option<String>,

	/// The radius in relation to the radius of Terra.
	pub(super) radius: f32,

	/// The surface gravity of this trabant in relation to the surface gravity of Terra.
	pub(super) gravity: f32,

	/// The rotation period (sidereal time). This is the time duration it takes for the body to make a full rotation. This is different from the "day" duration, which may be a little bit longer. Since when the body performed a full rotation it moved along it's orbit and is therefore not facing the same angle to it's sun.
	/// If this is `None`, this means the body's representation is gravitational bound around the object it orbits.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::timedelta_option" )]
	pub(super) rotation_period: Option<TimeDelta>,

	/// The objects orbiting this trabant.
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
	pub(super) width: f32,
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
	pub(super) name: Option<String>,

	/// The mass in kg.
	pub(super) mass: f32,

	/// The outline box of the station in meter. This is the box where the station is just fitting inside.
	pub(super) size: Vec3,

	/// The gravity within the station in relation to the surface gravity of Terra.
	pub(super) gravity: f32,

	/// The objects orbiting this station.
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




//=============================================================================
// Testing


#[cfg( test )]
mod tests {
	use super::*;

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
	fn test_indices() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];
		assert_eq!( sol.indices(), vec![
			vec![],
			vec![0],
			vec![1],
			vec![2],
			vec![3], vec![3,1], vec![3,2],
			vec![4], vec![4,1],
			vec![5], vec![5,1],
		] );

		let centauri = &systems[1];
		assert_eq!( centauri.indices(), vec![
			vec![],
			vec![0],
			vec![1], vec![1,1],
			vec![2], vec![2,1],
			vec![3],
		] );
	}

	#[test]
	fn test_indices_satellites() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert_eq!( sol.indices_satellites( &[0] ).unwrap(), vec![
			vec![1],
			vec![2],
			vec![3],
			vec![4],
			vec![5],
		] );

		assert_eq!( sol.indices_satellites( &[1] ).unwrap(), Vec::<Vec<usize>>::new() );

		assert_eq!( sol.indices_satellites( &[3] ).unwrap(), vec![ vec![3,1], vec![3,2], ] );
	}

	#[test]
	fn test_object_by_name() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert_eq!( sol.index_of( "Sol" ).unwrap(), vec![ 0 ] );
		assert_eq!( sol.index_of( "Mercury" ).unwrap(), vec![ 1 ] );
		assert_eq!( sol.index_of( "Luna" ).unwrap(), vec![ 3, 1 ] );

		let centauri = &systems[1];

		assert_eq!( centauri.index_of( "Centauri AB A" ).unwrap(), vec![ 1 ] );
		assert_eq!( centauri.index_of( "Minos" ).unwrap(), vec![ 1, 1 ] );
		assert_eq!( centauri.index_of( "Centauri AB B" ).unwrap(), vec![ 2 ] );
		assert_eq!( centauri.index_of( "Taurus" ).unwrap(), vec![ 2, 1 ] );
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

	#[test]
	fn test_body_type() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert!( sol.body_type( &[] ).is_err() );  // <- The system itself
		assert_eq!( sol.body_type( &[0] ).unwrap(), BodyType::Star );  // <- The singular star
		assert_eq!( sol.body_type( &[1] ).unwrap(), BodyType::Trabant );  // <- The first planet
		assert_eq!( sol.body_type( &[2] ).unwrap(), BodyType::Trabant );  // <- The second planet
		assert_eq!( sol.body_type( &[3] ).unwrap(), BodyType::Trabant );  // <- The third planet
		assert_eq!( sol.body_type( &[3,0] ).unwrap(), BodyType::Trabant );  // <- The third planet
		assert_eq!( sol.body_type( &[3,1] ).unwrap(), BodyType::Trabant );  // <- The first moon of the third planet

		let centauri = &systems[1];

		assert!( centauri.body_type( &[] ).is_err() );  // <- The system itself
		assert_eq!( centauri.body_type( &[0] ).unwrap(), BodyType::GravitationalCenter );  // <- Gravitational center of the trinary star system.
		assert_eq!( centauri.body_type( &[1] ).unwrap(), BodyType::Star );  // The first star
		assert_eq!( centauri.body_type( &[2] ).unwrap(), BodyType::Star );  // The second star
		assert_eq!( centauri.body_type( &[3] ).unwrap(), BodyType::Star );  // The third star
		assert_eq!( centauri.body_type( &[1,0] ).unwrap(), BodyType::Star );  // The first star
		assert_eq!( centauri.body_type( &[1,1] ).unwrap(), BodyType::Trabant );  // The first planet of the first star
		assert_eq!( centauri.body_type( &[2,1] ).unwrap(), BodyType::Trabant );  // The first planet of the second star
	}

	#[test]
	fn test_trabant_type() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert!( sol.trabant_type( &[] ).is_err() );  // <- The system itself
		assert_eq!( sol.trabant_type( &[0] ).unwrap(), None );  // <- The singular star
		assert_eq!( sol.trabant_type( &[1] ).unwrap().unwrap(), TrabantType::Planet );  // <- The first planet
		assert_eq!( sol.trabant_type( &[2] ).unwrap().unwrap(), TrabantType::Planet );  // <- The second planet
		assert_eq!( sol.trabant_type( &[3] ).unwrap().unwrap(), TrabantType::Planet );  // <- The third planet
		assert_eq!( sol.trabant_type( &[3,0] ).unwrap().unwrap(), TrabantType::Planet );  // <- The third planet
		assert_eq!( sol.trabant_type( &[3,1] ).unwrap().unwrap(), TrabantType::Moon );  // <- The first moon of the third planet

		let centauri = &systems[1];

		assert!( centauri.trabant_type( &[] ).is_err() );  // <- The system itself
		assert_eq!( centauri.trabant_type( &[0] ).unwrap(), None );  // <- Gravitational center of the trinary star system.
		assert_eq!( centauri.trabant_type( &[1] ).unwrap(), None );  // The first star
		assert_eq!( centauri.trabant_type( &[2] ).unwrap(), None );  // The second star
		assert_eq!( centauri.trabant_type( &[3] ).unwrap(), None );  // The third star
		assert_eq!( centauri.trabant_type( &[1,0] ).unwrap(), None );  // The first star
		assert_eq!( centauri.trabant_type( &[1,1] ).unwrap().unwrap(), TrabantType::Planet );  // The first planet of the first star
		assert_eq!( centauri.trabant_type( &[2,1] ).unwrap().unwrap(), TrabantType::Planet );  // The first planet of the second star
	}

	#[test]
	fn test_stars_of_system() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert_eq!(
			sol.stars()
				.map( |x| x.mass )
				.collect::<Vec<_>>(),
			vec![ 1.0 ]
		);

		let centauri = &systems[1];

		dbg!( centauri.stars()
				.map( |x| x.mass )
				.collect::<Vec<_>>() );

		assert_eq!(
			centauri.stars()
				.map( |x| x.mass )
				.collect::<Vec<_>>(),
			vec![ 1.0788, 0.9092, 0.1221 ]
		);
	}

	#[test]
	fn test_rotation_period() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert!( sol.rotation_period( &[] ).is_err() );  // <- The system itself
		assert_eq!( sol.rotation_period( &[0] ).unwrap(), TimeDelta::seconds( 2192832 ) );  // <- The singular star
		assert_eq!( sol.rotation_period( &[1] ).unwrap(), TimeDelta::seconds( 5067360 ) );  // <- The first planet
		assert_eq!( sol.rotation_period( &[2] ).unwrap(), TimeDelta::seconds( -20997360 ) );  // <- The second planet
		assert_eq!( sol.rotation_period( &[3] ).unwrap(), TimeDelta::seconds( 86164 ) );  // <- The third planet
		assert_eq!( sol.rotation_period( &[3,0] ).unwrap(), TimeDelta::seconds( 86164 ) );  // <- The third planet
		assert_eq!( sol.rotation_period( &[3,1] ).unwrap(), TimeDelta::seconds( 2357955 ) );  // <- The first moon of the third planet

		let centauri = &systems[1];

		assert!( centauri.rotation_period( &[] ).is_err() );  // <- The system itself
		assert_eq!( centauri.rotation_period( &[0] ).unwrap(), TimeDelta::zero() );  // <- Gravitational center of the trinary star system.
		assert_eq!( centauri.rotation_period( &[1] ).unwrap(), TimeDelta::seconds( 1900800 ) );  // The first star
		assert_eq!( centauri.rotation_period( &[2] ).unwrap(), TimeDelta::seconds( 3542400 ) );  // The second star
		assert_eq!( centauri.rotation_period( &[3] ).unwrap(), TimeDelta::seconds( 7776000 ) );  // The third star
		assert_eq!( centauri.rotation_period( &[1,0] ).unwrap(), TimeDelta::seconds( 1900800 ) );  // The first star
		assert_eq!( centauri.rotation_period( &[1,1] ).unwrap(), TimeDelta::seconds( 65220 ) );  // The first planet of the first star
		assert_eq!( centauri.rotation_period( &[2,1] ).unwrap(), TimeDelta::seconds( 110520 ) );  // The first planet of the second star
	}

	#[test]
	fn test_local_day_solar() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert!( sol.day_solar( &[] ).is_err() );  // <- The system itself
		assert!( sol.day_solar( &[0] ).is_err() );  // <- The singular star (is not orbiting anything, so has no "day")
		assert_eq!( sol.day_solar( &[1] ).unwrap().unwrap(), TimeDelta::seconds( 15205216 ) );  // <- The first planet
		assert_eq!( sol.day_solar( &[2] ).unwrap().unwrap(), TimeDelta::seconds( -10087183 ) );  // <- The second planet
		assert_eq!( sol.day_solar( &[3] ).unwrap().unwrap(), TimeDelta::seconds( 86399 ) );  // <- The third planet
		assert_eq!( sol.day_solar( &[3,0] ).unwrap().unwrap(), TimeDelta::seconds( 86399 ) );  // <- The third planet
		assert_eq!( sol.day_solar( &[3,1] ).unwrap().unwrap(), TimeDelta::seconds( 86399 ) );  // <- The first moon of the third planet
	}
}
