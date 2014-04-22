\name{sand}
\alias{sand}
\alias{C2}
\alias{C3}
\alias{C4}
\alias{C5}
\alias{C6}
\alias{C7}
\alias{C8}
\alias{C9}
\alias{C10}
\alias{N}
\alias{P}
\title{The sand package}
\description{
  This R package accompanies the book \sQuote{Statistical Analysis of
    Network Data with R}. It contains some of the data sets used in the
  book (the others are in the \code{igraphdata} package). It also
  contains the code from the book, and some simple functions to run the
  code without the need for typing it in.
}
\section{In brief}{
  Type in \code{N<enter>} to run the next chunk of code,
  and \code{C<x>} to jump to Chapter \code{x}, where \code{x} is between
  2 and 10. E.g. \code{C6<enter>} resets R and \dQuote{loads} Chapter
  6. \code{P<enter>} prints the next code chunk to be run (without
  actually running it).
}
\section{The data sets}{
  The various data sets are loaded from the code chunks in the book. The
  sand package contains the following data sets, each is documented in
  its on manual page: \code{\link{Ecoli}}, \code{\link{aidsblog}},
  \code{\link{calldata}}, \code{\link{elist.lazega}},
  \code{\link{fblog}}, \code{\link{g.bip}}, \code{\link{hc}},
  \code{\link{lazega}}, \code{\link{ppi.CC}},
  \code{\link{sandwichprobe}}, \code{\link{v.attr.lazega}}.
}
\section{Code chunks}{
  Code chunks of the book are numbered by chapter and each chunk is
  identified the chapter number and the chunk number connected by a
  dot.

  The reader is supposed to run the code chapter by chapter, ideally,
  starting from a clean, new R session. This might not be critical, but
  it is not always possible to unload packages in R, so it is the only
  way to make sure that the code works correctly.

  To make it easy to step through the code, the sand package define some
  \dQuote{commands}. Note that these are are not functions, and also
 q  that they are meant to be used interactively, and not
  programatically.
}
\section{The cursor}{
  The cursor marks the point the reader is at in the book, and commands
  discussed below move the cursor and run the code the cursor is at.
  
  The \sQuote{C} commands clear R, i.e. unload all loaded packages
  except for \code{sand} and its dependencies, and delete all objects
  from the global workspace. They also set the cursor to the first
  chunk of the given chapter: there are nine \sQuote{C} commands, from
  \sQuote{C2} to \sQuote{C10}, one for each Chapter of the
  book. (Chapter 1 has no code to run.)

  The command \sQuote{N} runs the chunk at the cursor, and steps the
  cursor to the next chunk. It is possible to run multiple chunks at
  once, with the form \sQuote{N + x} (with or without the spaces), where
  \sQuote{x} is the number of additional chunks to run. (I.e. \sQuote{N
  + 2} runs three chunks.)

  The command \sQuote{P} prints the chunk at the cursor, without running
  it. It is possible to print other chunks as well: \sQuote{P - 1}
  prints the previous chunk, \sQuote{P - 2} the one before that, etc.,
  \sQuote{P + 1} prints the next chunk, etc.

  The reader is welcome to inspect R objects, or run arbitrary R code
  between the \sQuote{N} and \sQuote{P} commands.
}
\author{Gabor Csardi <csardi.gabor@gmail.com>}
\seealso{\code{\link{install_sand_packages}} to install all R packages
  needed for the book.}
\examples{
## Start with Chapter 2
C2

## Run first code chunk
N

## Run next code chunk
N

## Jump to Chapter 5
C5

## Run first code chunk in Chapter 5
## It will create a plot
N
}
