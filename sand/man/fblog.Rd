\name{fblog}
\alias{fblog}
\title{Network of French political blogs}
\description{
  Subnetwork of French political blogs, extracted from a snapshot of
  over 1,100 such blogs on a single day in October of 2006 and
  classified by the \dQuote{Observatoire Presidentielle} project as to
  political affiliation.
}
\usage{
fblog
}
\format{
  An undirected igraph graph with 192 vertices and 1431 edges. Note that
  the graph is undirected. The graph has two vertex attributes,
  \sQuote{name} is the URL of the blog, and \sQuote{PolParty} is the
  assigned political affiliation, a political party.
}
\source{
  The \code{mixer} R package, see also
  \url{http://observatoire-presidentielle.fr/}.
}
% \references{}
\keyword{datasets}
