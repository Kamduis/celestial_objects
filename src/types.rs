//! Provides types for celestial objects.




//=============================================================================
// Crates


use serde::{Serialize, Deserialize};

use crate::coords::EquatorialCoords;




//=============================================================================
// Traits


pub trait AstronomicalObject {
	/// Return a new object from `self` with `name`.
	fn with_name( self, name: &str ) -> Self;

	/// Return a new object from `self` with `satellites` orbiting it.
	fn with_satellites( self, satellites: Vec<Orbit> ) -> Self;
}




//=============================================================================
// Enums


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

	/// Returns the name of the `CelestialSystem`. If this system has been given a popular name, this will be returned. If not, this returnes the same as `identifier()`.
	pub fn name( &self ) -> &str {
		if let Some( x ) = &self.name {
			return x;
		}

		self.identifier()
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

	/// Returns the radius of this system's main star in relation to Sol.
	pub fn radius_main_star( &self ) -> f32 {
		let star_main = self.stars().nth( 0 )
			.expect( "Each system should have at least one star." );
		star_main.radius()
	}

	/// Returns the spectral class of this system's main star.
	pub fn spectral_class_main_star( &self ) -> &str {
		let star_main = self.stars().nth( 0 )
			.expect( "Each system should have at least one star." );
		star_main.spectral_class()
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
struct CelestialSystemStarsIterator<'a> {
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

	/// Returns the star's radius with respect to the radius of Sol.
	fn radius( &self ) -> f32 {
		self.radius
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
}


/// Representing a space station.
#[derive( Serialize, Deserialize, Clone, PartialEq, Debug )]
pub struct Station {
	/// The name of this station.
	#[serde( default )]
	#[serde( skip_serializing_if = "Option::is_none" )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	pub(super) name: Option<String>,

	// /// The mass in kg.
	// pub(super) mass: f64,

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
}




//=============================================================================
// Testing


#[cfg( test )]
mod tests {
	use super::*;

	use crate::tests::systems_examples;

	#[test]
	fn data_of_nested_worlds() {
		let systems = systems_examples::systems_example();

		let sol = systems[0].clone();

		assert_eq!( sol.identifier(), "Sol" );
		assert_eq!( sol.name(), "Sol" );
		// assert_eq!( sol.coordinates(), EquatorialCoordinates::new( "0h 0m 0s", "0° 0m 0s", 0.0 ) );

		let centauri = systems[1].clone();

		assert_eq!( centauri.identifier(), "Alpha Centauri" );
		assert_eq!( centauri.name(), "Centauri" );
		// assert_eq!( centauri.coordinates(), EquatorialCoordinates::new( "14h 39m 36.49400s", "-60° 50m 2.3737s", 4.344 ) );
	}

	#[test]
	fn iterator_of_stars() {
		let systems = systems_examples::systems_example();

		let sol = &systems[0];

		assert_eq!( sol.stars().count(), 1 );

		let centauri = &systems[1];

		assert_eq!( centauri.stars().count(), 3 );
	}
}
