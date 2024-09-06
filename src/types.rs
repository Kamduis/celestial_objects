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
	GravitationalCenter,

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
}


/// Representing a star of a planetary system.
#[derive( Deserialize, Clone, PartialEq, Debug )]
pub struct Star;


/// Representing a trabant. This could be a planet in the orbit of a star of a moon in the orbit of a planet.
#[derive( Deserialize, Clone, PartialEq, Default, Debug )]
pub struct Trabant;


/// Representing a space station.
#[derive( Deserialize, Clone, PartialEq, Debug )]
pub struct Station;
