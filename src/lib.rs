//! Provides data for celestial objects (stars, planets, moons etc.) orbiting each other.




//=============================================================================
// Crates


use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{OnceLock, Mutex};

use ron::ser::PrettyConfig;

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

	use crate::types::AstronomicalObject;
	use crate::types::{CelestialBody, EquatorialCoordinates, Orbit, GravitationalCenter, Star, Trabant, Station};

	fn systems_example() -> Vec<CelestialSystem> {
		vec![
			CelestialSystem::new(
				"Sol",
				&EquatorialCoordinates::new( "0h 0m 0s", "0° 0m 0s", 0.0 ),
				CelestialBody::Star( Star::new( 1.0, 1.0, 1.0, "G2", ) )
					.with_satellites( vec![
						Orbit {
							axis_semi_major: 0.723332,
							eccentricity: 0.006772,
							body: CelestialBody::Trabant( Trabant {
								name: Some( "Venus".to_string() ),
								radius: 0.9499,
								gravity: 0.904,
								satellites: Vec::new(),
							} ),
						},
						Orbit {
							axis_semi_major: 149.598261e9,
							eccentricity: 0.01671123,
							body: CelestialBody::Trabant( Trabant {
								name: Some( "Terra".to_string() ),
								radius: 1.0,
								gravity: 1.0,
								satellites: vec![
									Orbit {
										axis_semi_major: 1.0000010180626,
										eccentricity: 0.0549,
										body: CelestialBody::Trabant( Trabant {
											name: Some( "Luna".to_string() ),
											radius: 0.2731,
											gravity: 0.1654,
											satellites: Vec::new(),
										} ),
									},
									Orbit {
										axis_semi_major: 497.7e6,
										eccentricity: 0.001,
										body: CelestialBody::Station( Station {
											name: Some( "Argus".to_string() ),
											radius: 6e3,
											gravity: 1.0,
											satellites: Vec::new(),
										} )
									},
								],
							} ),
						},
						Orbit {
							axis_semi_major: 30.07,
							eccentricity: 0.0113,
							body: CelestialBody::Trabant( Trabant {
								name: Some( "Neptun".to_string() ),
								radius: 3.829,
								gravity: 1.137,
								satellites: vec![
									Orbit {
										axis_semi_major: 0.00237,
										eccentricity: 0.000016,
										body: CelestialBody::Trabant( Trabant {
											name: Some( "Triton".to_string() ),
											radius: 0.2122,
											gravity: 0.0794,
											satellites: Vec::new(),
										} ),
									},
								],
							} ),
						},
					] ),
			),
			CelestialSystem::new(
				"Alpha Centauri",
				&EquatorialCoordinates::new( "14h 39m 36.49400s", "-60° 50m 2.3737s", 4.344 ),
				CelestialBody::GravitationalCenter( GravitationalCenter {
					satellites: vec![
						Orbit {
							axis_semi_major: 17.493,
							eccentricity: 0.51947,
							body: CelestialBody::Star( Star::new( 1.0788, 1.2175, 1.5059, "G2", ) )
								.with_satellites( vec![
									Orbit {
										axis_semi_major: 1.1,
										eccentricity: 0.019345,
										body: CelestialBody::Trabant( Trabant {
											name: Some( "Minos".to_string() ),
											radius: 1.1,
											gravity: 1.05,
											satellites: Vec::new(),
										} ),
									},
								] ),
						},
						Orbit {
							axis_semi_major: 17.493,
							eccentricity: 0.51947,
							body: CelestialBody::Star( Star::new( 0.9092, 0.8591, 0.4981, "K4", ) )
								.with_satellites( vec![
									Orbit {
										axis_semi_major: 0.8,
										eccentricity: 0.00824,
										body: CelestialBody::Trabant( Trabant {
											name: Some( "Taurus".to_string() ),
											radius: 0.9,
											gravity: 1.1,
											satellites: Vec::new(),
										} ),
									},
								] ),
						},
					],
				} )
			)
				.with_name( "Centauri" ),
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
}
