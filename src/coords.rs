//! Coordinate transformations.




//=============================================================================
// Crates


use std::f32::consts::TAU;
use std::num::ParseFloatError;
use std::sync::LazyLock;

use glam::f32::{Vec3, Mat3};
use regex::Regex;
use thiserror::Error;




//=============================================================================
// Errors


#[derive( Error, Debug )]
pub enum CoordError {
	#[error( "Argument is malformed: {0}" )]
	ArgumentMalformed( String ),

	#[error( "Could not parse float from text." )]
	ParseFloatError( #[from] ParseFloatError ),
}




//=============================================================================
// Constants


/// The regular expression used to get hours, minutes and seconds from a `"HhMmSs"` string.
static REGEX_HMS: LazyLock<Regex> = LazyLock::new( || {
	Regex::new( r"^((?<hours>[+-]?\d+\.?\d*)h)?\s*((?<minutes>\d+\.?\d*)m)?\s*((?<seconds>\d+\.?\d*)s)?$" ).unwrap()
} );


/// The regular expression used to get degrees, minutes and seconds from a `"DdMmSs"` string.
static REGEX_DMS: LazyLock<Regex> = LazyLock::new( || {
	Regex::new( r"^((?<degrees>[+-−]?\d+\.?\d*)[d°])?\s*((?<minutes>\d+\.?\d*)[m′])?\s*((?<seconds>\d+\.?\d*)[s″])?$" ).unwrap()
} );


/// Rotation matrix to convert from equatorial to galactic coordinates (using J2000.0 epoch).
///
/// To use this matrix to convert from galactic to equatorial coordinates, you need to transpose the matrix.
static ROT_EQUA_GAL: LazyLock<Mat3> = LazyLock::new( || {
	// `Mat3` is column major, but the mathematical definition is row major, so this matrix is being transposed to convert it from row major to column major.
	Mat3::from_cols_array( &[
		-0.054876, -0.873437, -0.483835,
		0.494109, -0.444830,  0.746982,
		-0.867666, -0.198076, 0.455984,
	] ).transpose()
} );




//=============================================================================
// Helper functions


/// Converts a sexagesimal hours-minutes-seconds string into hours, minutes and seconds as separate numbers.
///
/// `hms` must be in the format of `"HhMmSs"` where `H` is the hours, `M` the minutes and `S` the seconds (all with optional fraction part) while `h`, `m` and `s` are literal letters. Omitted elements are assumed to be zero. `"12h13.5s"` is allowed and equal to `"12h0m13.5s"`.
///
/// This function returns `None` if `hms` is malformed.
fn split_hms( hms: &str ) -> Result<[f32; 3], CoordError> {
	let caps = REGEX_HMS.captures( hms )
		.ok_or( CoordError::ArgumentMalformed( hms.to_string() ) )?;

	let hours = match &caps.name( "hours" ) {
		Some( x ) => x.as_str().parse::<f32>()?,
		None => 0.0,
	};

	let minutes = match &caps.name( "minutes" ) {
		Some( x ) => x.as_str().parse::<f32>()?,
		None => 0.0,
	};

	let seconds = match &caps.name( "seconds" ) {
		Some( x ) => x.as_str().parse::<f32>()?,
		None => 0.0,
	};

	Ok( [ hours, minutes, seconds ] )
}


/// Converts a sexagesimal degrees-minutes-seconds string into degrees, minutes and seconds as separate numbers.
///
/// `dms` must be in the format of `"DdMmSs"` where `D` is degrees, `M` the minutes and `S` the seconds (all with optional fraction part) while `d`, `m` and `s` are literal letters. Omitted elements are assumed to be zero. `"45d13.5s"` is allowed and equal to `"45h0m13.5s"`.
///
/// This function returns `None` if `dms` is malformed.
fn split_dms( dms: &str ) -> Result<[f32; 3], CoordError> {
	let caps = REGEX_DMS.captures( dms )
		.ok_or( CoordError::ArgumentMalformed( dms.to_string() ) )?;

	let degrees = match &caps.name( "degrees" ) {
		Some( x ) => x.as_str().replace( '−', "-" ).parse::<f32>()?,
		None => 0.0,
	};

	let minutes = match &caps.name( "minutes" ) {
		Some( x ) => x.as_str().parse::<f32>()?,
		None => 0.0,
	};

	let seconds = match &caps.name( "seconds" ) {
		Some( x ) => x.as_str().parse::<f32>()?,
		None => 0.0,
	};

	Ok( [ degrees, minutes, seconds ] )
}


/// Converts hours, minutes and seconds into radians.
fn sexagesimal_hms_to_radians( hours: f32, minutes: f32, seconds: f32 ) -> f32 {
	TAU * ( ( hours * 3600.0 + minutes * 60.0 + seconds ) / 86400.0 )
}


/// Converts degrees with minutes and seconds to radians
fn sexagesimal_dms_to_radians( degrees: f32, minutes: f32, seconds: f32 ) -> f32 {
	// The minus sign of `degrees` means that the whole sexagesimal is negative.
	let ( mins, secs ) = if degrees < 0.0 {
		( minutes * -1.0, seconds * -1.0 )
	} else {
		( minutes, seconds )
	};

	( degrees + mins / 60.0 + secs / 3600.0 ).to_radians()
}


/// Transforms spherical coordinates to cartesian coordinates.
///
/// # Arguments
/// * `lat` latitude in radians.
/// * `lon` longitude in radians.
fn sphere_to_cart( lat: f32, lon: f32 ) -> [f32; 3] {
	let lon_cos = lon.cos();
	let x = lon_cos * lat.cos();
	let y = lon_cos * lat.sin();
	let z = lon.sin();

	[ x, y, z ]
}


/// Transforms cartesian coordinates to spherical coordinates.
///
/// # Returns
/// The first item in the returned array is latitude, the second item is longitude.
fn cart_to_sphere( x: f32, y: f32, z: f32 ) -> [f32; 2] {
	let lat = z.asin();
	let lon = y.atan2( x );

	[ lat, lon ]
}




//=============================================================================
// Transformations


/// Conversion from equatorial to galactic coordinates (source [Wikipedia][]).
///
/// α is right ascension, δ is declination. NGP refers to the coordinate values of the north galactic pole and NCP to those of the north celestial pole.
///
/// # Arguments
/// * `alpha` (right ascension or α) in radians.
/// * `delta` (declination or δ) in radians.
///
/// # Returns
/// The returned array has the galactic longitude l as first item and the galactic latitude b as second, both in radians.
///
/// [Wikipedia]: https://en.wikipedia.org/wiki/Galactic_coordinate_system
fn equatorial_to_galactic_coordinates( alpha: f32, delta: f32 ) -> [f32; 2] {
	// Using matrix multiplication.
	// Calculate the vector to the star in equatorial coordinates (cartesian).
	let vec_equa = Vec3::from( sphere_to_cart( alpha, delta ) );

	// Rotate the vector
	let vec_gal = ROT_EQUA_GAL.mul_vec3( vec_equa );

	let vec_gal_sphere = cart_to_sphere( vec_gal.x, vec_gal.y, vec_gal.z );

	let l = vec_gal_sphere[1];
	let b = vec_gal_sphere[0];

	let l_pos = if l < 0.0 { l + TAU } else { l };

	[ l_pos, b ]
}


/// Conversion from galactic to equatorial coordinates (source [Wikipedia][]).
///
/// α is right ascension, δ is declination. NGP refers to the coordinate values of the north galactic pole and NCP to those of the north celestial pole.
///
/// # Arguments
/// * `l` (galactic longitude) in radian.
/// * `b` (galactic latitude) in radian.
///
/// # Returns
/// The returned array has the right ascension as first item and the declination as second, both in radians.
///
/// [Wikipedia]: https://en.wikipedia.org/wiki/Equatorial_coordinate_system
fn galactic_to_equatorial_coordinates( l: f32, b: f32 ) -> [f32; 2] {
	// Using matrix multiplication.
	// Calculate the vector to the star in equatorial coordinates (cartesian).
	let vec_equa = Vec3::from( sphere_to_cart( l, b ) );

	// Rotate the vector
	let vec_gal = ROT_EQUA_GAL.transpose().mul_vec3( vec_equa );

	let vec_gal_sphere = cart_to_sphere( vec_gal.x, vec_gal.y, vec_gal.z );

	let l = vec_gal_sphere[1];
	let b = vec_gal_sphere[0];

	let l_pos = if l < 0.0 { l + TAU } else { l };

	[ l_pos, b ]
}




//=============================================================================
// Coordinate systems


/// This struct represents the coordinates in the equatorial coordinate system. This is a very simple representation, assuming Epoch J2000.0 and Equinox J2000.0.
#[derive( Debug )]
pub struct EquatorialCoords {
	/// Right ascension (symbol `α`) is the angular distance of an object eastward along the celestial equator from the March equinox to the hour circle passing through the object. The March equinox point is one of the two points where the ecliptic intersects the celestial equator. The right ascension is stored in radians.
	ra: f32,

	/// The declination (symbol `δ`) is the angular distance of an object perpendicular to the celestial equator, positive to the north, negative to the south. The north celestial pole has therefore a declination of +90°. The origin for declination is the celestial equator, which is the projection of the Earth's equator onto the celestial sphere. Declination is analogous to terrestrial latitude. The declination is stored in radians.
	dec: f32,

	/// Distance from Sol in light years.
	distance: f32,
}

impl EquatorialCoords {
	/// Create a new instance from right ascension in sidereal hours, minutes and seconds, declination in degrees, minutes and seconds and distance in light years.
	pub fn try_from_hms_dms_ly( hms: &str, dms: &str, dist: f32 ) -> Result<Self, CoordError> {
		let hms_nums = split_hms( hms )?;
		let dms_nums = split_dms( dms )?;

		let ra = sexagesimal_hms_to_radians( hms_nums[0], hms_nums[1], hms_nums[2] );
		let dec = sexagesimal_dms_to_radians( dms_nums[0], dms_nums[1], dms_nums[2] );

		let res = Self {
			ra,
			dec,
			distance: dist,
		};

		Ok( res )
	}

	/// Returns the cartesian equatorial coordinates. All units are in light years.
	pub fn cartesian( &self ) -> [f32; 3] {
		let mut vec_cart = sphere_to_cart( self.ra, self.dec );
		for item in vec_cart.iter_mut() {
			*item *= self.distance;
		}

		vec_cart
	}
}

impl From<GalacticCoords> for EquatorialCoords {
	fn from( item: GalacticCoords ) -> Self {
		let trans = galactic_to_equatorial_coordinates( item.b, item.l );

		Self {
			ra: trans[0],
			dec: trans[1],
			distance: item.distance,
		}
	}
}


/// This struct represents the galactic coordinate system.
#[derive( Debug )]
pub struct GalacticCoords {
	/// Longitude (symbol `l`) is the angular distance of an object eastward along the galactic equator from the Galactic Center. Galactic longitude is usually measured in degrees (°), but this attribute stores the longitude in radians.
	l: f32,

	/// Latitude (symbol `b`) measures the angle of an object northward of the galactic equator (or midplane) as viewed from Earth. Galactic latitude is usually measured in degrees (°), but this attribute stores the latitude in radians.
	b: f32,

	/// Distance from the earth in light years.
	distance: f32,
}

impl GalacticCoords {
	/// Returns the cartesian galactic coordinates, where `x` (first item) points to the galactic center, `y` (second item) points into the direction of rotation parallel to the galactic plane and `z` (third item) points orthogonal to `x` and `y` to the galactic north pole. All units are in light years.
	pub fn cartesian( &self ) -> [f32; 3] {
		let mut vec_cart = sphere_to_cart( self.b, self.l );
		for item in vec_cart.iter_mut() {
			*item *= self.distance;
		}

		vec_cart
	}
}

impl From<EquatorialCoords> for GalacticCoords {
	fn from( item: EquatorialCoords ) -> Self {
		let trans = equatorial_to_galactic_coordinates( item.ra, item.dec );

		Self {
			b: trans[0],
			l: trans[1],
			distance: item.distance,
		}
	}
}




//=============================================================================
// Testing


#[cfg( test )]
mod tests {
	use super::*;

	use approx::assert_abs_diff_eq;

	#[test]
	fn test_split_hms() {
		assert_eq!( split_hms( "14h39m36.49400s" ).unwrap(), [ 14.0, 39.0, 36.494 ] );
		assert_eq!( split_hms( "12h30m30s" ).unwrap(), [ 12.0, 30.0, 30.0 ] );
		assert_eq!( split_hms( "12h30m30s" ).unwrap(), [ 12.0, 30.0, 30.0 ] );
		assert_eq!( split_hms( "12h30m30.0s" ).unwrap(), [ 12.0, 30.0, 30.0 ] );
		assert_eq!( split_hms( "-12h30m30.0s" ).unwrap(), [ -12.0, 30.0, 30.0 ] );
		assert_eq!( split_hms( "12h0m" ).unwrap(), [ 12.0, 0.0, 0.0 ] );
		assert_eq!( split_hms( "12h0s" ).unwrap(), [ 12.0, 0.0, 0.0 ] );
		assert_eq!( split_hms( "12h" ).unwrap(), [ 12.0, 0.0, 0.0 ] );
		assert_eq!( split_hms( "30m30.0s" ).unwrap(), [ 0.0, 30.0, 30.0 ] );
		assert_eq!( split_hms( "" ).unwrap(), [ 0.0, 0.0, 0.0 ] );
		assert_eq!( split_hms( "06h 45m 08.917s" ).unwrap(), [ 6.0, 45.0, 8.917 ] );
	}

	#[test]
	fn test_split_dms() {
		assert_eq!( split_dms( "-60d50m2.3737s" ).unwrap(), [ -60.0, 50.0, 2.3737 ] );
		assert_eq!( split_dms( "45d30m30s" ).unwrap(), [ 45.0, 30.0, 30.0 ] );
		assert_eq!( split_dms( "45d30m30.0s" ).unwrap(), [ 45.0, 30.0, 30.0 ] );
		assert_eq!( split_dms( "-45d30m30.0s" ).unwrap(), [ -45.0, 30.0, 30.0 ] );
		assert_eq!( split_dms( "45d0m" ).unwrap(), [ 45.0, 0.0, 0.0 ] );
		assert_eq!( split_dms( "45d0s" ).unwrap(), [ 45.0, 0.0, 0.0 ] );
		assert_eq!( split_dms( "45d" ).unwrap(), [ 45.0, 0.0, 0.0 ] );
		assert_eq!( split_dms( "30m30.0s" ).unwrap(), [ 0.0, 30.0, 30.0 ] );
		assert_eq!( split_dms( "" ).unwrap(), [ 0.0, 0.0, 0.0 ] );
		assert_eq!( split_dms( "−16° 42′ 58.02″" ).unwrap(), [ -16.0, 42.0, 58.02 ] );
	}

	#[test]
	fn test_sexagesimal_hms_to_radians() {
		assert_eq!( sexagesimal_hms_to_radians( 24.0, 0.0, 0.0 ), TAU );
		assert_eq!( sexagesimal_hms_to_radians( 12.0, 0.0, 0.0 ), TAU / 2.0 );
		assert_eq!( sexagesimal_hms_to_radians( 12.0, 51.0, 26.282 ), 3.3660333 );
		assert_eq!( sexagesimal_hms_to_radians( 14.0, 39.0, 36.494 ), 3.8380149498293004 );
		assert_eq!( sexagesimal_hms_to_radians( 6.0, 45.0, 8.917 ), 1.7677943301834265 );
	}

	#[test]
	fn test_sexagesimal_dms_to_radians() {
		assert_eq!( sexagesimal_dms_to_radians( 360.0, 0.0, 0.0 ), TAU );
		assert_eq!( sexagesimal_dms_to_radians( 180.0, 0.0, 0.0 ), TAU / 2.0 );
		assert_eq!( sexagesimal_dms_to_radians( 0.0, 60.0, 0.0 ), 1.0_f32.to_radians() );
		assert_eq!( sexagesimal_dms_to_radians( 0.0, 0.0, 60.0 ), 1.0_f32.to_radians() / 60.0 );
		assert_eq!( sexagesimal_dms_to_radians( 27.0, 7.0, 42.01 ), 0.4734788 );
		assert_eq!( sexagesimal_dms_to_radians( -60.0, 0.0, 0.0 ), -1.0471975511965976 );
		assert_abs_diff_eq!( sexagesimal_dms_to_radians( -60.0, 50.0, 0.0 ), -1.0617419616298838 );
		assert_abs_diff_eq!( sexagesimal_dms_to_radians( -60.0, 50.0, 2.3737 ), -1.0617534696522324 );
		assert_abs_diff_eq!( sexagesimal_dms_to_radians( -16.0, 42.0, 58.02 ), -0.2917512739808327 );
	}

	#[test]
	fn test_conversion_from_human_readable() {
		// Alpha Centauri
		let coord_equa_0 = EquatorialCoords::try_from_hms_dms_ly( "14h39m36.49400s", "-60d50m2.3737s", 4.344 ).unwrap();
		assert_abs_diff_eq!( coord_equa_0.ra, 3.8380149498293004, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_equa_0.dec, -1.0617534696522324, epsilon=5e-6 );
		assert_eq!( coord_equa_0.distance, 4.344 );

		// Sirius
		let coord_equa_1 = EquatorialCoords::try_from_hms_dms_ly( "06h 45m 08.917s", "−16° 42′ 58.02″", 8.60 ).unwrap();
		assert_abs_diff_eq!( coord_equa_1.ra, 1.7677943301834265, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_equa_1.dec, -0.2917512739808327, epsilon=5e-6 );
		assert_eq!( coord_equa_1.distance, 8.60 );
	}

	#[test]
	fn test_conversion_equitorial_to_galactic() {
		// Alpha Centauri
		let coord_gal_0 = equatorial_to_galactic_coordinates(
			sexagesimal_hms_to_radians( 14.0, 39.0, 36.494 ),
			sexagesimal_dms_to_radians( -60.0, 50.0, 2.3737 )
		);
		assert_abs_diff_eq!( coord_gal_0[0], 5.51060085, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_gal_0[1], -0.01186224, epsilon=5e-6 );

		let coord_equa_0 = EquatorialCoords::try_from_hms_dms_ly( "14h39m36.49400s", "-60d50m2.3737s", 4.344 ).unwrap();
		let coord_gal_0 = GalacticCoords::from( coord_equa_0 );
		assert_abs_diff_eq!( coord_gal_0.b, 5.51060085, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_gal_0.l, -0.01186224, epsilon=5e-6 );
		assert_eq!( coord_gal_0.distance, 4.344 );

		// Sirius
		let coord_gal_1 = equatorial_to_galactic_coordinates(
			sexagesimal_hms_to_radians( 6.0, 45.0, 8.917 ),
			sexagesimal_dms_to_radians( -16.0, 42.0, 58.02 )
		);
		assert_abs_diff_eq!( coord_gal_1[0], 3.9659167469900853, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_gal_1[1], -0.15516470215236877, epsilon=5e-6 );

		let coord_equa_1 = EquatorialCoords::try_from_hms_dms_ly( "06h 45m 08.917s", "−16° 42′ 58.02″", 8.60 ).unwrap();
		let coord_gal_1 = GalacticCoords::from( coord_equa_1 );
		assert_abs_diff_eq!( coord_gal_1.b, 3.9659167469900853, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_gal_1.l, -0.15516470215236877, epsilon=5e-6 );
		assert_eq!( coord_gal_1.distance, 8.60 );
	}

	#[test]
	fn test_transforming_back_and_forth() {
		// Alpha Centauri
		let coord_equa_0 = EquatorialCoords::try_from_hms_dms_ly( "14h39m36.49400s", "-60d50m2.3737s", 4.344 ).unwrap();
		let coord_gal_0 = GalacticCoords::from( coord_equa_0 );
		assert_abs_diff_eq!( coord_gal_0.b, 5.51060085, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_gal_0.l, -0.01186224, epsilon=5e-6 );
		assert_eq!( coord_gal_0.distance, 4.344 );

		let coord_equa_back_0 = EquatorialCoords::from( coord_gal_0 );
		assert_abs_diff_eq!( coord_equa_back_0.ra, 3.8380149498293, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_equa_back_0.dec, -1.0617534696522324, epsilon=5e-6 );
		assert_eq!( coord_equa_back_0.distance, 4.344 );

		// Sirius
		let coord_equa_1 = EquatorialCoords::try_from_hms_dms_ly( "06h 45m 08.917s", "−16° 42′ 58.02″", 8.60 ).unwrap();
		let coord_gal_1 = GalacticCoords::from( coord_equa_1 );
		assert_abs_diff_eq!( coord_gal_1.b, 3.9659167469900853, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_gal_1.l, -0.15516470215236877, epsilon=5e-6 );
		assert_eq!( coord_gal_1.distance, 8.60 );

		let coord_equa_back_1 = EquatorialCoords::from( coord_gal_1 );
		assert_abs_diff_eq!( coord_equa_back_1.ra, 1.7677943301834265, epsilon=5e-6 );
		assert_abs_diff_eq!( coord_equa_back_1.dec, -0.2917512739808327, epsilon=5e-6 );
		assert_eq!( coord_equa_back_1.distance, 8.60 );
	}

	#[test]
	fn test_cartesian() {
		// Alpha Centauri
		let coord_equa_0 = EquatorialCoords::try_from_hms_dms_ly( "14h39m36.49400s", "-60d50m2.3737s", 4.344 ).unwrap();
		let coord_equa_cart_0 = coord_equa_0.cartesian();
		assert_abs_diff_eq!( coord_equa_cart_0[0], -1.62404925, epsilon=10e-6 );
		assert_abs_diff_eq!( coord_equa_cart_0[1], -1.35801508, epsilon=10e-6 );
		assert_abs_diff_eq!( coord_equa_cart_0[2], -3.79323016, epsilon=10e-6 );

		let coord_gal_0 = GalacticCoords::from( coord_equa_0 );
		let coord_gal_cart_0 = coord_gal_0.cartesian();

		assert_abs_diff_eq!( coord_gal_cart_0[0], 3.110559, epsilon=10e-6 );
		assert_abs_diff_eq!( coord_gal_cart_0[1], -3.031848, epsilon=10e-6 );
		assert_abs_diff_eq!( coord_gal_cart_0[2], -0.051528, epsilon=10e-6 );

		// Sirius
		let coord_equa_1 = EquatorialCoords::try_from_hms_dms_ly( "06h 45m 08.917s", "−16° 42′ 58.02″", 8.60 ).unwrap();
		let coord_equa_cart_1 = coord_equa_1.cartesian();
		assert_abs_diff_eq!( coord_equa_cart_1[0], -1.6121148, epsilon=10e-6 );
		assert_abs_diff_eq!( coord_equa_cart_1[1], 8.07727075, epsilon=10e-6 );
		assert_abs_diff_eq!( coord_equa_cart_1[2], -2.47361743, epsilon=10e-6 );

		let coord_gal_1 = GalacticCoords::from( coord_equa_1 );
		let coord_gal_cart_1 = coord_gal_1.cartesian();

		assert_abs_diff_eq!( coord_gal_cart_1[0], -5.76969882, epsilon=10e-6 );
		assert_abs_diff_eq!( coord_gal_cart_1[1], -6.23731938, epsilon=10e-6 );
		assert_abs_diff_eq!( coord_gal_cart_1[2], -1.32906829, epsilon=10e-6 );
	}
}
