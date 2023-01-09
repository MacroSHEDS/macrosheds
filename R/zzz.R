.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname),
                        " of ", pkgname, "\n",
                        "this package has further license information in the LICENSE file found here:",
                        "    https://github.com/MacroSHEDS/macrosheds/blob/master/LICENSE")
}
