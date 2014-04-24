\name{ppi.CC}
\alias{ppi.CC}
\title{Yeast protein interaction network}
\description{
  A network of 241 interactions among 134 proteins.  They were assembled
  by Jiang et al. (see below), from various sources, and pertain to only
  those proteins annotated, as of January 2007, with the term \dQuote{cell 
    communication} in the gene ontology (GO) database.
}
\usage{
ppi.CC
}
\format{
  An undirected igraph graph object, with vertex attributes:
  \itemize{
    \item \sQuote{name}: the name of the protein.
    \item \sQuote{ICSC}: whether the protein is annotated with the
      \dQuote{intracellular signaling cascade} GO term, zero or one.
    \item \sQuote{IPR000198}: whether the protein contains the
      \sQuote{rho GTPase-activating protein domain} (IPR000198) motif.
    \item \sQuote{IPR000403}: whether the protein contains the
      \sQuote{phosphatidylinositol 3-/4-kinase, catalytic domain}
      (IPR000403) motif.
    \item \sQuote{IPR001806}: whether the protein contains the
      \sQuote{small GTPase superfamily} (IPR001806) motif.
    \item \sQuote{IPR001849}: whether the protein contains the
      \sQuote{pleckstrin homology domain} (IPR001849) motif.
    \item \sQuote{IPR002041}: whether the protein contains the
      \sQuote{ran GTPase} (IPR002041) motif.
    \item \sQuote{IPR003527}: whether the protein contains the
      \sQuote{mitogen-activated protein (MAP) kinase, conserved site}
      (IPR003527) motif.
  }
}
\source{
  See the reference below. Please cite it if you use this dataset in
  your work.
}
\references{
  X. Jiang, N. Nariai, M. Steffen, S. Kasif, E. Kolaczyk: Integration of
  relational and hierarchical network information for protein
  function prediction.  BMC Bioinform. 9, 350 (2008).
}
\keyword{datasets}
