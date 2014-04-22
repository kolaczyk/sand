\name{lazega}
\alias{lazega}
\alias{elist.lazega}
\alias{v.attr.lazega}
\title{Lazega lawyers network data}
\description{
  This data set comes from a network study of corporate law partnership
  that was carried out in a Northeastern US corporate law firm, referred
  to as SG&R, 1988-1991 in New England. It includes (among others)
  measurements of networks among the 71 attorneys (partners and
  associates) of this firm, i.e. their strong-coworker network, advice
  network, friendship network, and indirect control networks. Various
  members' attributes are also part of the dataset, including seniority,
  formal status, office in which they work, gender, lawschool attended,
  individual performance measurements (hours worked, fees brought in),
  attitudes concerning various management policy options, etc.

  Note that this is only a subset of the originally collected data,
  including the 36 partners of the firm.
}
\usage{
lazega
elist.lazega
v.attr.lazega
}
\format{
  \code{lazega} is an igraph graph object, undirected. It has the
  following vertex attributes: \sQuote{name}, \sQuote{Seniority},
  \sQuote{Status} (all 1, meaning partner), \sQuote{Gender} (1 is man, 2
  is woman), \sQuote{Office} (1 is Boston, 2 is Hartford, 3 is
  Providence), \sQuote{Years} (years with the firm), \sQuote{Age},
  \sQuote{Practice} (1 is litigation, 2 is corporate),
  and \sQuote{School} (1 is Harvard or Yale, 2 is University of
  Connecticut, 3 is other). See the reference below for more.

  \code{elist.lazega} is a data frame containing an edge list of the
  network.

  \code{v.attr.lazega} is a data frame containing the vertex attributes
  only.
}
\source{
  Provided to us by Emmanuel Lazega. Please cite the
  reference below if you use this dataset in your work.
}
\references{
  E. Lazega, The Collegial Phenomenon: The Social Mechanisms of
  Cooperation Among Peers in a Corporate Law Partnership. Oxford
  University Press, Oxford (2001).
}
\keyword{datasets}
