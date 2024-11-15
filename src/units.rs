//! Provides types for units to be used for celestial objects and distances.




//=============================================================================
// Crates


use std::ops::{Add, Sub, Mul, Div};

use crate::calc;




//=============================================================================
// Units


/// Representing a length that can be expressed in different units.
#[derive( Clone, Copy, PartialEq, PartialOrd, Debug )]
pub struct Length( f32 );

impl Length {
	/// A `Length` of 0 m.
	pub const ZERO: Length = Self( 0.0 );

	/// Creates a new `Length` from astronomical units.
	pub fn from_au( val: f32 ) -> Self {
		Self( val * calc::ASTRONOMICAL_UNIT )
	}

	/// Creates a new `Length` from a factor of the radius of Sol.
	pub fn from_radius_sol( val: f32 ) -> Self {
		Self( val * calc::RADIUS_SOL )
	}

	/// Creates a new `Length` from a factor of the radius of Terra.
	pub fn from_radius_terra( val: f32 ) -> Self {
		Self( val * calc::RADIUS_TERRA )
	}

	/// Returns the length in meter.
	pub fn meter( &self ) -> f32 {
		self.0
	}

	/// Returns the length in astronomical units.
	pub fn au( &self ) -> f32 {
		self.0 / calc::ASTRONOMICAL_UNIT
	}

	/// Returns the length in relation to the radius of Sol.
	pub fn radius_sol( &self ) -> f32 {
		self.0 / calc::RADIUS_SOL
	}

	/// Returns the length in relation to the radius of Terra.
	pub fn radius_terra( &self ) -> f32 {
		self.0 / calc::RADIUS_TERRA
	}

	/// Raises the `Length` to an integer power.
	pub fn powi( &self, n: i32 ) -> Self {
		Self::from( self.0.powi( n ) )
	}
}

impl From<f32> for Length {
	/// Creating a new `Length` from a number assuming it represents a length in meter.
	fn from( value: f32 ) -> Self {
		Self( value )
	}
}

impl Add for Length {
	type Output = Self;

	fn add( self, rhs: Self ) -> Self {
		Self::from( self.0 + rhs.0 )
	}
}

impl Sub for Length {
	type Output = Self;

	fn sub( self, rhs: Self ) -> Self {
		Self::from( self.0 - rhs.0 )
	}
}

impl Mul for Length {
	type Output = Self;

	fn mul( self, rhs: Self ) -> Self {
		Self::from( self.0 * rhs.0 )
	}
}

impl Mul<f32> for Length {
	type Output = Self;

	fn mul( self, rhs: f32 ) -> Self {
		Self::from( self.0 * rhs )
	}
}

impl Mul<Length> for f32 {
	type Output = Length;

	fn mul( self, rhs: Length ) -> Length {
		Length::from( self * rhs.0 )
	}
}

impl Div for Length {
	type Output = Self;

	fn div( self, rhs: Self ) -> Self {
		Self::from( self.0 / rhs.0 )
	}
}

impl Div<f32> for Length {
	type Output = Self;

	fn div( self, rhs: f32 ) -> Self {
		Self::from( self.0 / rhs )
	}
}

impl Div<Length> for f32 {
	type Output = Length;

	fn div( self, rhs: Length ) -> Length {
		Length::from( self / rhs.0 )
	}
}


/// Representing a mass that can be expressed in different units.
#[derive( Clone, Copy, PartialEq, PartialOrd, Debug )]
pub struct Mass( f32 );

impl Mass {
	/// A `Mass` of 0 kg.
	pub const ZERO: Mass = Self( 0.0 );

	/// Creates a new `Mass` from a factor of the mass of Sol.
	pub fn from_mass_sol( val: f32 ) -> Self {
		Self( val * calc::MASS_SOL )
	}

	/// Creates a new `Mass` from a factor of the mass of Terra.
	pub fn from_mass_terra( val: f32 ) -> Self {
		Self( val * calc::MASS_TERRA )
	}

	/// Returns the mass in kg.
	pub fn kg( &self ) -> f32 {
		self.0
	}

	/// Returns the mass in relation to the mass of Sol.
	pub fn sol( &self ) -> f32 {
		self.0 / calc::MASS_SOL
	}

	/// Returns the mass in relation to the mass of Terra.
	pub fn terra( &self ) -> f32 {
		self.0 / calc::MASS_TERRA
	}

	/// Raises the `Mass` to an integer power.
	pub fn powi( &self, n: i32 ) -> Self {
		Self::from( self.0.powi( n ) )
	}
}

impl From<f32> for Mass {
	/// Creating a new `Mass` from a number assuming it represents a mass in kg.
	fn from( value: f32 ) -> Self {
		Self( value )
	}
}

impl Add for Mass {
	type Output = Self;

	fn add( self, rhs: Self ) -> Self {
		Self::from( self.0 + rhs.0 )
	}
}

impl Sub for Mass {
	type Output = Self;

	fn sub( self, rhs: Self ) -> Self {
		Self::from( self.0 - rhs.0 )
	}
}

impl Mul for Mass {
	type Output = Self;

	fn mul( self, rhs: Self ) -> Self {
		Self::from( self.0 * rhs.0 )
	}
}

impl Mul<f32> for Mass {
	type Output = Self;

	fn mul( self, rhs: f32 ) -> Self {
		Self::from( self.0 * rhs )
	}
}

impl Mul<Mass> for f32 {
	type Output = Mass;

	fn mul( self, rhs: Mass ) -> Mass {
		Mass::from( self * rhs.0 )
	}
}

impl Div for Mass {
	type Output = Self;

	fn div( self, rhs: Self ) -> Self {
		Self::from( self.0 / rhs.0 )
	}
}

impl Div<f32> for Mass {
	type Output = Self;

	fn div( self, rhs: f32 ) -> Self {
		Self::from( self.0 / rhs )
	}
}

impl Div<Mass> for f32 {
	type Output = Mass;

	fn div( self, rhs: Mass ) -> Mass {
		Mass::from( self / rhs.0 )
	}
}
