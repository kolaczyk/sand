\name{hc}
\alias{hc}
\title{Hospital encounter network data}
\description{
  Records of contacts among patients and various types of health care
  workers in the geriatric unit of a hospital in Lyon, France, in
  2010, from 1pm on Monday, December 6 to 2pm on Friday, December
  10. Each of the 75 people in this study consented to wear RFID 
  sensors on small identification badges during this period, which made
  it possible to record when any two of them were in face-to-face
  contact with each other (i.e., within 1-1.5 m of each other) during
  a 20-second interval of time.
}
\usage{
hc
}
\format{
  A data frame, where each row is an interaction. It has five columns:
  \itemize{
    \item \code{Time}: integer, time in seconds when the 20 second
      encounter terminated.
    \item \code{ID1}: integer, numeric ID of the first person.
    \item \code{ID2}: integer, numeric ID of the second person.
    \item \code{S1}: factor, the status of the first person, see below.
    \item \code{S2}: factor, the status of the second person, see below.
  }

  Status codes: administrative staff (ADM), medical doctor (MED),
  paramedical staff, such as nurses or nurses' aides (NUR), and
  patients (PAT).
}
\source{
  See the reference below.
  Please cite the it if you use this dataset in your work.
}
\references{
  P. Vanhems, A. Barrat, C. Cattuto, J.-F. Pinton, N. Khanafer,
  C. Regis, B.-a. Kim, B. Comte, N. Voirin: Estimating potential
  infection transmission routes in hospital wards using wearable
  proximity sensors. PloS One 8(9), e73970 306 (2013).
}
\keyword{datasets}
