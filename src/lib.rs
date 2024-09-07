//! Provides data for celestial objects (stars, planets, moons etc.) orbiting each other.




//=============================================================================
// Crates


use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{OnceLock, Mutex};

use ron::ser::PrettyConfig;
// use thiserror::Error;

mod coords;
mod types;
mod serde_helpers;

pub use crate::coords::{EquatorialCoords, GalacticCoords};
pub use crate::types::{CelestialSystem, BodyType, Affiliation, StarProperty};




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
// Access worlds


/// Returns a celestial system. The system is the root of the nested world data. If no system with `identifier` is found, this function returns `None`.
///
/// # Arguments
/// * `identifier` The unique identifier of the celestial system.
pub fn system( identifier: &str ) -> Option<CelestialSystem> {
	let db = db_worlds().lock().unwrap();

	db.iter()
		.find( |x| x.identifier() == identifier ).cloned()
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
// Writing to files.


/// Write worlds to a RON-formatted text file.
pub fn export( path: &PathBuf ) -> std::io::Result<()> {
	let db = db_worlds().lock().unwrap();

	let mut file = fs::File::create( path )?;

	let content = ron::ser::to_string_pretty(
		&*db,
		PrettyConfig::new()
			.depth_limit( 12 )
			.indentor( "\t".to_string() )
	).unwrap();

	file.write_all( content.as_bytes() )?;

	Ok( () )
}




//=============================================================================
// Testing


#[cfg( test )]
pub(crate) mod tests {
	use super::*;

	use serial_test::serial;
	use tempfile::tempdir;

	pub(crate) mod systems_examples;

	#[test]
	#[serial]
	fn import_worlds() {
		clear_worlds();

		let path = PathBuf::from( "./tests/systems-example.ron" );

		import( &path ).unwrap();

		let expected = systems_examples::systems_example();
		let received = db_worlds().lock().unwrap();

		for ( recei, expect ) in received.iter().zip( expected ) {
			assert_eq!( recei.identifier(), expect.identifier() );
			assert_eq!( recei, &expect );
		}
	}

	#[test]
	#[serial]
	fn export_import_worlds() {
		clear_worlds();

		let path = PathBuf::from( "./tests/systems-example.ron" );

		import( &path ).unwrap();

		// Save original worlds for later comparison.
		let db_expected = db_worlds().lock().unwrap().clone();

		// Create a directory inside of `std::env::temp_dir()`.
		let dir = tempdir().unwrap();

		// Create path for RON-files to be exported into.
		let path_ron: PathBuf = dir.path().join( path.file_name().unwrap() );

		export( &path_ron ).unwrap();

		// Pretty-print the contents of the exported file.
		let contents = fs::read_to_string( &path_ron ).unwrap();
		println!( "{}", contents );

		// Clear database before reading again.
		clear_worlds();

		// Re-import the files again.
		import( &path_ron ).unwrap();

		let db = db_worlds().lock().unwrap();

		// Verify that the imported worlds are identical to the original ones.
		for ( i, expected ) in db_expected.iter().enumerate() {
			assert_eq!( &db[i], expected );
		}
	}

	#[test]
	#[serial]
	fn test_access_system() {
		clear_worlds();

		let path = PathBuf::from( "./tests/systems-example.ron" );

		import( &path ).unwrap();

		let sys_sol = system( "Sol" );

		assert!( sys_sol.is_some() );
		assert_eq!( sys_sol.unwrap().identifier(), "Sol" );
	}
}
