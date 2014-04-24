\name{calldata}
\alias{calldata}
\title{Austrian phone call network data}
\description{
  A set of data for phone traffic 60 between 32 telecommunication
  districts in Austria throughout a period during the 61 year 1991.
}
\usage{
calldata
}
\format{
  A data frame with 32 x 31 flow measurements, 992 rows, and seven
  columns:
  \itemize{
    \item \code{Orig}: factor, the origin district.   
    \item \code{Dest}: factor, the destination district.
    \item \code{DistEuc}: numeric, Euclidean distance between the
      districts.
    \item \code{DistRd}: numeric, road distance between districts.
    \item \code{O.GRP}: numeric, gross regional product of the origin
      district, in Austrian schillings.
    \item \code{D.GRP}: numeric, gross regional product of the
      destination district, in Austrian schillings.
    \item \code{Flow}: the \dQuote{amount} of phone calls from the
      origin district to the destination district, in erlang units
      (number of phone calls, including faxes, times the average length
      of the call divided by the duration of the measurement period).
  }
}
\source{
  This dataset was provided to us by Suchi Gopal.
  Please cite the reference below if you use this dataset in your work.
}
\references{
  M. Fischer, S. Gopal: Artificial neural networks: a new approach to
  modeling interregional telecommunication flows. J. Reg. Sci. 34(4),
  503-527 (1994).
}
\keyword{datasets}
