\name{install_sand_packages}
\alias{install_sand_packages}
\title{Install all packages used in the book}
\description{This function makes it easy to download and install
  all R packages that are used in the book \sQuote{Statistical Analysis
    of Network Data with R}.
}
\usage{
install_sand_packages()
}
\details{
  The function uses the BioConductor installer, as this can
  install both all required BioConductor and CRAN packages.
}
\value{
  Returns the names of the installed packages, invisibly.
}
\author{
  Gabor Csardi \email{csardi.gabor@gmail.com}
}
