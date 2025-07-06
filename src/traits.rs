//! Provides traits.




//=============================================================================
// Crates


#[cfg( feature = "i18n" )] use unic_langid::LanguageIdentifier;




//=============================================================================
// Traits


/// Providing localized string output.
///
/// **Note:** This trait is only available, if the **`i18n`** feature has been enabled.
#[cfg( feature = "i18n" )]
pub trait Locale {
	/// Returns the string representation of `self` using the language according to the provided `locale`.
	fn to_string_locale( &self, locale: &LanguageIdentifier ) -> String;
}


/// Providing conversion into LaTeX code.
///
/// **Note:** This trait is only available, if the **`tex`** feature has been enabled.
#[cfg( feature = "tex" )]
pub trait Latex {
	/// Converts the entity into a LaTeX-string.
	fn to_latex( &self ) -> String;
}


/// Providing conversion into LaTeX code to print symbols instead of text. This is mostly implemented to print out SI prefixes and units like `\kilo\meter` or `\milli\ampere` (using the LaTeX package `{siunitx}`) instead of words.
///
/// This trait is only available, if the **`tex`** feature has been enabled.
#[cfg( feature = "tex" )]
pub trait LatexSym: Latex {
	/// Converts the entity into a LaTeX-string displaying symbols instead of written words.
	fn to_latex_sym( &self ) -> String {
		self.to_latex()
	}
}


/// Providing localized LaTeX code output.
///
/// **Note:** This trait is only available, if the **`i18n`** and **`tex`** features have been enabled.
#[cfg( all( feature = "i18n", feature = "tex" ) )]
pub trait LocaleLatex: Locale + Latex {
	/// Returns the LaTeX code representation of `self` using the language according to the provided `locale`.
	fn to_latex_locale( &self, locale: &LanguageIdentifier ) -> String;
}
