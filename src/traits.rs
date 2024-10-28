//! Provides traits.




//=============================================================================
// Traits


/// Providing conversion into LaTeX code.
///
/// **Note:** This struct is only available, if the **`tex`** feature has been enabled.
#[cfg( feature = "tex" )]
pub trait Latex {
	/// Converts the entity into a LaTeX-string.
	fn to_latex( &self ) -> String;
}


/// Providing conversion into LaTeX code to print symbols instead of text. This is mostly implemented to print out SI prefixes and units like `\kilo\meter` or `\milli\ampere` (using the LaTeX package `{siunitx}`) instead of words.
///
/// This Trait is only available, if the **`tex`** feature has been enabled.
#[cfg( feature = "tex" )]
pub trait LatexSym: Latex {
	/// Converts the entity into a LaTeX-string displaying symbols instead of written words.
	fn to_latex_sym( &self ) -> String {
		self.to_latex()
	}
}
