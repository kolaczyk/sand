\name{strike}
\alias{strike}
\title{Michael's strike network}
\description{
  A representation of who consults with whom in the context of a strike at a forest products manufacturing facility, following proposed changes to the workers' compensation package.  An edge between two workers indicates that at least one of them said they consult with the other with moderate frequency.
}
\usage{
strike
}
\format{
  An undirected igraph graph object with 24 vertices and 38 edges.  The graph has two vertex attributes, \sQuote{name} is the first name of the individual and \sQuote{race} is a compilation of age (young or old) and language spoken (English or Spanish).
}
\source{
  This dataset was constructed from the corresponding version in Chapter 7 of W. De Nooy, A. Mrvar, and V. Batagelj, Exploratory Social Network Analysis with Pajek. Cambridge University Press, 2011, vol. 27.
  
  Please cite the original reference below if you use this dataset in your work.
}
\references{
  J.H. Michael, "Labor dispute reconciliation in a forest productsmanufacturing   facility," Forest Products Journal, vol. 47, no. 11/12, p. 41, 1997.
}
\keyword{datasets}
