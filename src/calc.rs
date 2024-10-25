//! Calculations of orbital mechanics.




//=============================================================================
// Crates


use crate::units::{Mass, Length};




//=============================================================================
// Constants


/// The length of the astronomical unit in meter.
pub const ASTRONOMICAL_UNIT: f32 = 149_597_870_700.0;


/// Newtonian constant of gravitation in m<sup>3</sup>⋅kg<sup>-1</sup>⋅s<sup>-2</sup>.
pub const G: f32 = 66.7430e-12;


/// The radius of Sol in meter.
pub const RADIUS_SOL: f32 = 695e6;


/// The radius of Terra in meter.
pub const RADIUS_TERRA: f32 = 6.378e6;


/// The mass of Sol in kg.
pub const MASS_SOL: f32 = 1.98855e30;


/// The mass of Terra in kg.
pub const MASS_TERRA: f32 = 5.97219e24;




//=============================================================================
// Orbit


/// Calculates the duration orbital period in seconds.
///
/// $$T = 2 * π * √(a³/(G (m_1 + m_2)))$$
///
/// # Arguments
///
/// * `axis`: Semi major axis in meters.
/// * `mass_central`: Mass of the central body in kg.
/// * `mass_satellite`: Mass of the orbiting satellite in kg.
pub fn orbital_period( axis: Length, mass_central: Mass, mass_satellite: Mass ) -> f32 {
	let temp = axis.powi( 3 ).meter() / ( G * ( mass_central + mass_satellite ) ).kg();
	2.0 * std::f32::consts::PI * temp.sqrt()
}




//=============================================================================
// Testing


#[cfg( test )]
mod tests {
	use super::*;

	#[test]
	fn test_orbital_periods() {
		let period = orbital_period( 384.399e6.into(), 5.972168e24.into(), 7.342e22.into() );
		let abs_difference = ( period - 27.2845857371 * 86400.0 ).abs();

		assert!( abs_difference < 1e-6 );
	}
}