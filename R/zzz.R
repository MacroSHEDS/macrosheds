.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname),
                        " of ", pkgname, "\n",
                        "This package is licensed under MIT, but licensing of MacroSheds data is more complex. See ",
                        "https://docs.google.com/document/d/1CPaQ705QyoWfu6WjA4xHgQooCQ8arq3NZ8DO9tb9jZ0/edit?usp=sharing")
}
