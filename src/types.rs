//! Provides types for celestial objects.




//=============================================================================
// Crates


use serde::Deserialize;




//=============================================================================
// Enums


/// Classification of celestial bodies.
#[derive( Deserialize, Clone, PartialEq, Debug )]
pub enum CelestialBody {
	/// A point in space that is the gravitational center of two masses orbiting each other.
	GravitationalCenter( GravitationalCenter ),

	Star( Star ),

	Trabant( Trabant ),

	Station( Station ),
}




//=============================================================================
// Structs


/// A `CelestialSystem` is the framework around a hierarchy of nested `CelestialBody`s.
#[derive( Deserialize, Clone, PartialEq, Debug )]
pub struct CelestialSystem {
	/// The identifier of the celestial system. Old catalog names, mostly of the main star within this system, if it is a system with multiple stars.
	identifier: String,

	/// The name as the system, as it is known to the people.
	#[serde( default )]
	#[serde( with = "crate::serde_helpers::option_wrapper" )]
	name: Option<String>,

	/// The coordinates of the `CelestialSystem` in equatorial coordinates. These coordinates assume Epoch J2000.0 and  represent the systems position ([equatorial coordinate system](https://en.wikipedia.org/wiki/Equatorial_coordinate_system)).
	coordinates: EquatorialCoordinates,

	/// The main body of this system. This `CelestialBody` may have one or more bodies orbiting it. Systems with a single sun as it's center have a `CelestialBody> representing this sun as `body`. Multi-star-systems will have either the main star as `body` (if it is much more massive than the other stars or a `GravitationalCenter` as the theoretical body, that is then orbited by multiple stars.
	body: CelestialBody,
}

impl CelestialSystem {
	/// Create a new `CelestialSystem`.
	pub fn new( identifier: &str, coordinates: &EquatorialCoordinates, body: CelestialBody ) -> Self {
		Self {
			identifier: identifier.to_string(),
			name: None,
			coordinates: coordinates.clone(),
			body,
		}
	}

	/// Returns the identifier of the `CelestialSystem`.
	pub fn identifier( &self ) -> &str {
		&self.identifier
	}
}


/// Equatorial coordinates (assuming Epoch J2000.0) representing a star's position ([Galactic coordinate system](https://en.wikipedia.org/wiki/Equatorial_coordinate_system)).
#[derive( Deserialize, Clone, PartialEq, Debug )]
pub struct EquatorialCoordinates {
	pub ra: String,
	pub dec: String,
	pub dist: f32,
}

impl EquatorialCoordinates {
	pub fn new( ra: &str, dec: &str, dist: f32 ) -> Self {
		Self {
			ra: ra.to_string(),
			dec: dec.to_string(),
			dist,
		}
	}
}


/// Representing the theoretical gravitational center of two heavy masses orbiting each other.
#[derive( Deserialize, Clone, PartialEq, Debug )]
pub struct GravitationalCenter {
	/// The objects oribitng this gravitational center..
	pub(super) satellites: Vec<CelestialBody>
}


/// Representing a star of a planetary system.
#[derive( Deserialize, Clone, PartialEq, Debug )]
pub struct Star {}


/// Representing a trabant. This could be a planet in the orbit of a star of a moon in the orbit of a planet.
#[derive( Deserialize, Clone, PartialEq, Default, Debug )]
pub struct Trabant;


/// Representing a space station.
#[derive( Deserialize, Clone, PartialEq, Debug )]
pub struct Station;
