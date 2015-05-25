.onAttach <- function(libname, pkgname)
{
  packageStartupMessage(
    "The package 'datamerge' has been discontinued since the functionality of ",
    "the package is better implemented in more modern tools such as data.table ",
    "or dplyr. I recommend that you learn to use those instead.")
}
