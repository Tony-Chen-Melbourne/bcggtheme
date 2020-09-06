# bcggtheme
A ggplot2 theme to create graphs in the style of BCG

The package needs to be downloaded and installed from Github. The easiest way to do this is with the devtools package. If you don’t have the devtools package, install it:

`install.packages("devtools")`

Once devtools is installed, you can download and install the theme as follows:

`devtools::install_github("Tony-Chen-Melbourne/bcggtheme")`

One final step before use is to load Windows system fonts into R, in order to access the standard MS Trebuchet font. First install the `extrafonts` library package:

`install.packages("extrafont")`

The run the code:

`extrafont::font_import()`

This will take a few minutes to do its thing, after which you're good to go. 

Currently the package only works on Windows computers. That isn't a major problem given that BCG uses windows computers, but is something which I'd like to fix in future versions of this package.

The vignettes file explains how the package works.

The package draws heavily on [grattantheme](https://github.com/grattan/grattantheme) created by Matt Cowgill, and maintained by Matt Cowgill and Will Mackey.
