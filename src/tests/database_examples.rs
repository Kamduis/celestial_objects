//! Provides methods to read celestial systems from a RON file into a database to use them for testing.




//=============================================================================
// Crates


use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{OnceLock, Mutex};

use ron::ser::PrettyConfig;

use crate::types::CelestialSystem;




//=============================================================================
// Storage for all worlds.


/// Get access to the internal database for worlds.
pub(super) fn db_worlds() -> &'static Mutex<Vec<CelestialSystem>> {
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
pub(super) fn system( identifier: &str ) -> Option<CelestialSystem> {
	let db = db_worlds().lock().unwrap();

	db.iter()
		.find( |x| x.identifier() == identifier ).cloned()
}




//=============================================================================
// Adding or modifying worlds.


/// Adds a new commodity to the internal database.
pub(super) fn clear_worlds() {
	let mut db = db_worlds().lock().unwrap();
	db.clear();
}




//=============================================================================
// Reading from files.


/// Import worlds from a file using the RON data format.
pub(super) fn import( path: &PathBuf ) -> std::io::Result<()> {
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
pub(super) fn export( path: &PathBuf ) -> std::io::Result<()> {
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




