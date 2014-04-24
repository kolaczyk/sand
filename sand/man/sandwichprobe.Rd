\name{sandwichprobe}
\alias{sandwichprobe}
\alias{delaydata}
\alias{host.locs}
\title{Internet packet probes data}
\description{
  These data correspond to an experiment conducted by Coates et al. to 
  measure the difference in delay experienced by packet probes sent over
  the Internet during a short period in 2001, from a desktop computer in
  the ECE department at Rice University to similar machines at ten other
  university locations. The data were intended for use with a newly
  proposed method of Internet topology inference.
}
\usage{
delaydata
host.locs
}
\format{
  The data is provided in two files. \code{delaydata} is a three-column
  data frame. The first columnf is the difference in delay of the small
  packets (in milliseconds). The second column is the numeric code of
  the destination of small packets. The third column is the numeric code
  of the destination of large packet.

  \code{host.locs} contains the character code of the destinations:
  \enumerate{
    \item \sQuote{IST} Instituto Superior Tecnico (Portugal)
    \item \sQuote{IT} Instituto de Telecomunicacoes (Portugal)
    \item \sQuote{Bkly} University of California, Berkeley
    \item \sQuote{MSU1} Michigan State University (Host 1)
    \item \sQuote{MSU2} Michigan State University (Host 2)
    \item \sQuote{UIUC} University of Illinois, Urbana-Champaign
    \item \sQuote{UWisc1} University of Wisconsin, Madison (Host 1)
    \item \sQuote{UWisc2} University of Wisconsin, Madison (Host 2)
    \item \sQuote{RiceU1} Rice University (Host 1)
    \item \sQuote{RiceU2} Rice University (Host 2)
  }
}
\source{
  Provided by Mark Coates, see reference below.
  Please cite the reference below if you use this dataset in your work.
}
\references{
  M. Coates, R. Castro, R. Nowak, M. Gadhiok, R. King, Y. Tsang, Maximum
  likelihood network topology identification from edge-based unicast
  measurements. Proceedings of the 2002 ACM SIGMETRICS International
  Conference on Measurement and Modeling of Computer Systems, 2002,
  pp. 11-20.
}
\keyword{datasets}
