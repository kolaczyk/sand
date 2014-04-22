\name{Ecoli}
\alias{Ecoli}
\alias{Ecoli.expr}
\alias{Ecoli.data}
\alias{regDB.adj}
\title{E. coli gene expression levels}
\description{
  Gene expression levels in the bacteria Escherichia coli (E. coli),
  measured for 153 genes under each of 40 different experimental
  conditions.
}
\usage{
data(Ecoli.data)
Ecoli.expr
regDB.adj
}
\format{
  \code{Ecoli.expr} is a 40 by 153 matrix of (log) gene expression
  levels in the bacteria Escherichia coli (E. coli), measured for
  153 transcription factors under each of 40 different experimental
  conditions,  averaged over three replicates of each experiment. The data 
  are a subset of those published in the reference below. The
  experiments were genetic perturbation experiments,in which a given
  gene was \sQuote{turned off}, for each of 40 different genes.

  \code{regDB.adj} is an adjacency matrix of regulatory relationships in
  E. coli, extracted from the RegulonDB
  (\url{http://regulondb.ccg.unam.mx/}) database at the same time the
  experimental data were collected. 
}
\source{
  See the reference below. Please cite it if you use this dataset in
  your work.
}
\references{
  J. Faith, B. Hayete, J. Thaden, I. Mogno, J. Wierzbowski, G. Cottarel,
  S. Kasif, J. Collins, T. Gardner: Large-scale mapping and
  validation of Escherichia coli transcriptional regulation from a
  compendium of expression profiles. PLoS Biol. 5(1), e8 (2007).
}
\keyword{datasets}
