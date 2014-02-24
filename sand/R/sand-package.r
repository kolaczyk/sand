
install_sand_packages <- function() {
  pkgs <- strsplit(packageDescription("sand")$Suggests, "[, \n]+")[[1]]
  source("http://bioconductor.org/biocLite.R")
  biocLite(pkgs, suppressUpdates=TRUE, suppressAutoUpdate=TRUE)
}
