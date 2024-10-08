//! Providing example celestial systems to test aginst.




//=============================================================================
// Crates


use glam::Vec3;

use crate::CelestialSystem;
use crate::coords::EquatorialCoords;
use crate::types::AstronomicalObject;
use crate::types::{CelestialBody, Affiliation, Orbit, GravitationalCenter, Star, Trabant, Station};




//=============================================================================
// Example systems


pub(crate) fn systems_example() -> Vec<CelestialSystem> {
	vec![
		CelestialSystem::new(
			"Sol",
			&EquatorialCoords::try_from_hms_dms_ly( "0h 0m 0s", "0° 0m 0s", 0.0 ).unwrap(),
			CelestialBody::Star( Star::new( 1.0, 1.0, 1.0, "G2", ) )
				.with_satellites( vec![
					Orbit {
						axis_semi_major: 0.387098,
						eccentricity: 0.205630,
						body: CelestialBody::Trabant( Trabant {
							name: Some( "Mercury".to_string() ),
							radius: 0.3829,
							gravity: 0.38,
							satellites: Vec::new(),
						} ),
					},
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
										mass: 4180e3,
										size: Vec3::new( 200.0, 200.0, 6e3 ),
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
		)
			.with_affiliation( Affiliation::Union )
			.with_description( "Original home system of humans and AIs." ),
		CelestialSystem::new(
			"Alpha Centauri",
			&EquatorialCoords::try_from_hms_dms_ly( "14h 39m 36.49400s", "-60° 50m 2.3737s", 4.344 ).unwrap(),
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
					Orbit {
						axis_semi_major: 8700.0,
						eccentricity: 0.50,
						body: CelestialBody::Star( Star::new( 1.221, 0.1542, 0.001567, "M5.5" )
							.with_name( "Proxima" ),
						),
					},
				],
			} )
		)
			.with_name( "Centauri" )
			.with_affiliation( Affiliation::Union )
			.with_description( "Capital of the Union." ),
	]
}
