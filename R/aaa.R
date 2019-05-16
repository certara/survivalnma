.onAttach <- function(libname, pkgname) {
  packageStartupMessage("survivalnma package --- May 2019 version --- type ?survnma for help")
  packageStartupMessage("WinBUGS14 required to run correctly. Default folder is C:/WinBUGS14/")
  packageStartupMessage("Type vignette('survivalnma_guide') for basic examples.")
}
