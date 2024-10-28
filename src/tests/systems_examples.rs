//! Providing example celestial systems to test aginst.




//=============================================================================
// Crates


use chrono::TimeDelta;
use glam::Vec3;

use crate::CelestialSystem;
use crate::coords::EquatorialCoords;
use crate::types::AstronomicalObject;
use crate::types::CelestialBody;
use crate::types::properties::{Affiliation, Orbit, Atmosphere, AtmosphereQuality, GasComposition, Molecule};
use crate::types::objects::{GravitationalCenter, Star, Trabant, Ring, Station};




//=============================================================================
// Example systems


pub(crate) fn systems_example() -> Vec<CelestialSystem> {
	vec![
		CelestialSystem::new(
			"Sol",
			&EquatorialCoords::try_from_hms_dms_ly( "0h 0m 0s", "0° 0m 0s", 0.0 ).unwrap(),
			CelestialBody::Star(
				Star::new( 1.0, 1.0, 1.0, "G2", )
					.with_rotation_period( TimeDelta::days( 25 ) + TimeDelta::hours( 9 ) + TimeDelta::minutes( 7 ) + TimeDelta::seconds( 12 ) )
			)
				.with_satellites( vec![
					Orbit {
						axis_semi_major: 0.387098,
						eccentricity: 0.205630,
						body: CelestialBody::Trabant( Trabant {
							name: Some( "Mercury".to_string() ),
							radius: 0.3829,
							gravity: 0.38,
							rotation_period: Some( TimeDelta::days( 58 ) + TimeDelta::hours( 15 ) + TimeDelta::minutes( 36 ) ),
							temperature: [ -173.0, 167.0, 427.0 ],
							atmosphere: None,
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
							rotation_period: Some( ( TimeDelta::days( 243 ) + TimeDelta::minutes( 36 ) ) * -1 ),
							temperature: [ 437.0, 464.0, 497.0 ],
							atmosphere: Some( Atmosphere {
								pressure: 92.0,
								quality: AtmosphereQuality::Toxic,
								composition: GasComposition::from( [
									( Molecule::CarbonDioxide, 0.912 ),
									( Molecule::Nitrogen, 0.035 ),
									( Molecule::Oxygen, 0.051 ),
									( Molecule::SulfurDioxide, 0.001 ),
								] ),
							} ),
							satellites: Vec::new(),
						} ),
					},
					Orbit {
						axis_semi_major: 1.0,
						eccentricity: 0.01671123,
						body: CelestialBody::Trabant( Trabant {
							name: Some( "Terra".to_string() ),
							radius: 1.0,
							gravity: 1.0,
							rotation_period: Some( TimeDelta::hours( 23 ) + TimeDelta::minutes( 56 ) + TimeDelta::seconds( 4 ) ),
							temperature: [ -89.0, 15.0, 58.0 ],
							atmosphere: Some( Atmosphere {
								pressure: 1.01,
								quality: AtmosphereQuality::Toxic,
								composition: GasComposition::from( [
									( Molecule::Argon, 0.0093 ),
									( Molecule::CarbonDioxide, 0.17038 ),
									( Molecule::Nitrogen, 0.7808 ),
									( Molecule::Oxygen, 0.00095 ),
									( Molecule::SulfurDioxide, 0.0301 ),
								] ),
							} ),
							satellites: vec![
								Orbit {
									axis_semi_major: 0.00257,
									eccentricity: 0.0549,
									body: CelestialBody::Trabant( Trabant {
										name: Some( "Luna".to_string() ),
										radius: 0.2731,
										gravity: 0.1654,
										rotation_period: None,
										temperature: [ -173.0, -23.0, 116.0 ],
										atmosphere: None,
										satellites: Vec::new(),
									} ),
								},
								Orbit {
									axis_semi_major: 0.0001,
									eccentricity: 0.001,
									body: CelestialBody::Station( Station {
										name: Some( "Argus".to_string() ),
										mass: 4180e3,
										size: Vec3::new( 200.0, 200.0, 6e3 ),
										gravity: 1.0,
										temperature: [ 15.0, 21.0, 27.0 ],
										atmosphere: Some( Atmosphere {
											pressure: 1.0,
											quality: AtmosphereQuality::Breathable,
											composition: GasComposition::from( [
												( Molecule::CarbonDioxide, 0.004 ),
												( Molecule::Nitrogen, 0.787 ),
												( Molecule::Oxygen, 0.207 ),
											] ),
										} ),
										satellites: Vec::new(),
									} )
								},
							],
						} ),
					},
					Orbit {
						axis_semi_major: 9.5826,
						eccentricity: 0.0565,
						body: CelestialBody::Trabant( Trabant {
							name: Some( "Saturn".to_string() ),
							radius: 9.449,
							gravity: 1.065,
							rotation_period: Some( TimeDelta::hours( 10 ) + TimeDelta::minutes( 33 ) ),
							temperature: [ -185.0, -152.0, -120.0 ],
							atmosphere: Some( Atmosphere {
								pressure: 1.4,
								quality: AtmosphereQuality::Toxic,
								composition: GasComposition::from( [
									( Molecule::Ammonia, 0.125e-3 ),
									( Molecule::Helium, 0.0325 ),
									( Molecule::Hydrogen, 0.963 ),
									( Molecule::Methane, 4.5e-3 ),
								] ),
							} ),
							satellites: vec![
								Orbit {
									axis_semi_major: 425.574205e-6,
									eccentricity: 0.0,
									body: CelestialBody::Ring( Ring {
										width: 762.51079e-6,
									} ),
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
							rotation_period: Some( TimeDelta::hours( 15 ) + TimeDelta::minutes( 57 ) + TimeDelta::seconds( 59 ) ),
							temperature: [ -218.0, -209.0, -201.0 ],
							atmosphere: Some( Atmosphere {
								pressure: 1.0,
								quality: AtmosphereQuality::Toxic,
								composition: GasComposition::from( [
									( Molecule::Hydrogen, 0.79 ),
									( Molecule::Helium, 0.19 ),
									( Molecule::Methane, 0.015 ),
								] ),
							} ),
							satellites: vec![
								Orbit {
									axis_semi_major: 0.00237,
									eccentricity: 0.000016,
									body: CelestialBody::Trabant( Trabant {
										name: Some( "Triton".to_string() ),
										radius: 0.2122,
										gravity: 0.0794,
										rotation_period: None,
										temperature: [ -235.0, -235.0, -235.0 ],
										atmosphere: None,
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
						body: CelestialBody::Star( Star::new( 1.0788, 1.2175, 1.5059, "G2", )
							.with_rotation_period( TimeDelta::days( 22 ) )
						)
							.with_satellites( vec![
								Orbit {
									axis_semi_major: 1.1,
									eccentricity: 0.019345,
									body: CelestialBody::Trabant( Trabant {
										name: Some( "Minos".to_string() ),
										radius: 1.1,
										gravity: 1.05,
										rotation_period: Some( TimeDelta::hours( 18 ) + TimeDelta::minutes( 7 ) ),
										temperature: [ -7.0, 12.0, 89.0 ],
										atmosphere: Some( Atmosphere {
											pressure: 1.2,
											quality: AtmosphereQuality::Breathable,
											composition: GasComposition::from( [
												( Molecule::CarbonDioxide, 0.00054 ),
												( Molecule::Nitrogen, 0.77 ),
												( Molecule::Oxygen, 0.214 ),
												( Molecule::Argon, 0.002 ),
											] ),
										} ),
										satellites: Vec::new(),
									} ),
								},
							] ),
					},
					Orbit {
						axis_semi_major: 17.493,
						eccentricity: 0.51947,
						body: CelestialBody::Star( Star::new( 0.9092, 0.8591, 0.4981, "K4", )
							.with_rotation_period( TimeDelta::days( 41 ) )
						)
							.with_satellites( vec![
								Orbit {
									axis_semi_major: 0.8,
									eccentricity: 0.00824,
									body: CelestialBody::Trabant( Trabant {
										name: Some( "Taurus".to_string() ),
										radius: 0.9,
										gravity: 1.1,
										rotation_period: Some( TimeDelta::hours( 30 ) + TimeDelta::minutes( 42 ) ),
										temperature: [ -120.0, 5.0, 127.0 ],
										atmosphere: Some( Atmosphere {
											pressure: 0.36,
											quality: AtmosphereQuality::Breathable,
											composition: GasComposition::from( [
												( Molecule::CarbonDioxide, 0.00092 ),
												( Molecule::Nitrogen, 0.792 ),
												( Molecule::Oxygen, 0.179 ),
												( Molecule::Argon, 0.015 ),
											] ),
										} ),
										satellites: Vec::new(),
									} ),
								},
							] ),
					},
					Orbit {
						axis_semi_major: 8700.0,
						eccentricity: 0.50,
						body: CelestialBody::Star( Star::new( 0.1221, 0.1542, 0.001567, "M5.5" )
							.with_rotation_period( TimeDelta::days( 90 ) )
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
