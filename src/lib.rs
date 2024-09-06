//! Provides data for celestial objects (stars, planets, moons etc.) orbiting each other.




//=============================================================================
// Crates


use std::fs;
use std::path::PathBuf;
use std::sync::{OnceLock, Mutex};

mod types;
mod serde_helpers;

use crate::types::CelestialSystem;




//=============================================================================
// Storage for all worlds.


/// Get access to the internal database for worlds.
fn db_worlds() -> &'static Mutex<Vec<CelestialSystem>> {
	static DB_WORLDS: OnceLock<Mutex<Vec<CelestialSystem>>> = OnceLock::new();

	DB_WORLDS.get_or_init( || {
		Mutex::new( Vec::new() )
	} )
}




//=============================================================================
// Adding or modifying worlds.


/// Adds a new commodity to the internal database.
pub fn clear_worlds() {
	let mut db = db_worlds().lock().unwrap();
	db.clear();
}




//=============================================================================
// Reading from files.


/// Import worlds from a file using the RON data format.
pub fn import( path: &PathBuf ) -> std::io::Result<()> {
	let contents = fs::read_to_string( path )?;

	let worlds_parsed: Vec<CelestialSystem> = ron::de::from_str( &contents ).unwrap();

	let mut db = db_worlds().lock().unwrap();
	for wrld in worlds_parsed {
		db.push( wrld );
	}

	Ok( () )
}




//=============================================================================
// Testing


#[cfg( test )]
pub(crate) mod tests {
	use super::*;

	use serial_test::serial;
	// use tempfile::tempdir;

	use crate::types::{CelestialBody, EquatorialCoordinates, Star};

	fn systems_example() -> Vec<CelestialSystem> {
		vec![
			CelestialSystem::new(
				"Sol",
				&EquatorialCoordinates::new( "0h 0m 0s", "0° 0m 0s", 0.0 ),
				CelestialBody::Star( Star {} )
			),
			CelestialSystem::new(
				"Alpha Centauri",
				&EquatorialCoordinates::new( "14h 39m 36.49400s", "-60° 50m 2.3737s", 4.344 ),
				CelestialBody::Star( Star {} )
			),
		]
	}

	#[test]
	#[serial]
	fn import_worlds() {
		clear_worlds();

		let path = PathBuf::from( "./tests/systems-example.ron" );

		import( &path ).unwrap();

		let expected = systems_example();
		let received = db_worlds().lock().unwrap();
		for ( recei, expect ) in received.iter().zip( expected ) {
			assert_eq!( recei.identifier(), expect.identifier() );
		}
	}
}
