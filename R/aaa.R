.onAttach <- function(libname, pkgname) {
  packageStartupMessage("survnma v0.1.1 --- Sep 2020 --- type ?survnma for help")
  packageStartupMessage("WinBUGS14 is required to run correctly. Default folder is C:/WinBUGS14/")
  packageStartupMessage("Type vignette('survnma_guide') for basic examples and vignette('multiple_nma') for larger analyses.")
}
