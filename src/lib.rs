//! Provides data for celestial objects (stars, planets, moons etc.) orbiting each other.




//=============================================================================
// Crates


mod coords;
mod types;
mod serde_helpers;

pub use crate::coords::{EquatorialCoords, GalacticCoords};
pub use crate::types::{CelestialSystem, BodyType, Affiliation, StarProperty};




//=============================================================================
// Testing


#[cfg( test )]
pub(crate) mod tests {
	use std::fs;
	use std::path::PathBuf;

	use serial_test::serial;
	use tempfile::tempdir;

	mod database_examples;
	pub(crate) mod systems_examples;

	#[test]
	#[serial]
	fn import_worlds() {
		database_examples::clear_worlds();

		let path = PathBuf::from( "./tests/systems-example.ron" );

		database_examples::import( &path ).unwrap();

		let expected = systems_examples::systems_example();
		let received = database_examples::db_worlds().lock().unwrap();

		for ( recei, expect ) in received.iter().zip( expected ) {
			assert_eq!( recei.identifier(), expect.identifier() );
			assert_eq!( recei, &expect );
		}
	}

	#[test]
	#[serial]
	fn export_import_worlds() {
		database_examples::clear_worlds();

		let path = PathBuf::from( "./tests/systems-example.ron" );

		database_examples::import( &path ).unwrap();

		// Save original worlds for later comparison.
		let db_expected = database_examples::db_worlds().lock().unwrap().clone();

		// Create a directory inside of `std::env::temp_dir()`.
		let dir = tempdir().unwrap();

		// Create path for RON-files to be exported into.
		let path_ron: PathBuf = dir.path().join( path.file_name().unwrap() );

		database_examples::export( &path_ron ).unwrap();

		// Pretty-print the contents of the exported file.
		let contents = fs::read_to_string( &path_ron ).unwrap();
		println!( "{}", contents );

		// Clear database before reading again.
		database_examples::clear_worlds();

		// Re-import the files again.
		database_examples::import( &path_ron ).unwrap();

		let db = database_examples::db_worlds().lock().unwrap();

		// Verify that the imported worlds are identical to the original ones.
		for ( i, expected ) in db_expected.iter().enumerate() {
			assert_eq!( &db[i], expected );
		}
	}

	#[test]
	#[serial]
	fn test_access_system() {
		database_examples::clear_worlds();

		let path = PathBuf::from( "./tests/systems-example.ron" );

		database_examples::import( &path ).unwrap();

		let sys_sol = database_examples::system( "Sol" );

		assert!( sys_sol.is_some() );
		assert_eq!( sys_sol.unwrap().identifier(), "Sol" );
	}
}
