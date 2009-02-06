\name{OLINgui}
\alias{OLINgui}
\title{Graphical user interface for OLIN package}
\description{The function \code{OLINgui} provides a graphical user interface for
visualisation, normalisation and quality testing of two-channel microarray data.
It is based on the functions of the OLIN package. 
}
\usage{
OLINgui()
}
\details{The function \code{OLINgui} launches a graphical user interface for the OLIN package.
It is based on Tk widgets using the R TclTk interface by Peter Dalgaard. It also employs
some pre-made widgets from the tkWidgets Bioconductor-package by Jianhua Zhang for
the selection of objects/files to be loaded. 

OLINgui provides a convient interface to most functionalities of the OLIN package without restriction
of options. An exception is the visualisaton, where default arguments for plotting 
are used. To fine-tune plots, the underlying plotting functions in the OLIN package
can be applied. 
    
The usage of OLINgui assumes existing marray objects for the batch of arrays to 
be analysed and normalised.  To construct marray objects using a graphical interface, refer to  \code{\link[marrayInput]{read.marrayRaw}}.
Some underlying functions (e.g.\code{\link[OLIN]{mxy2.plot}} and optinally \code{\link[OLIN]{olin}})
) require an additonal list of X- and Y-coordinates of spots. 
This has to be done \dQuote{by hand} yet. However, most functions do not need this list.  

For a overview of the functionality of OLINgui, please refer to the package documentation. 
}
\value{OLINgui returns a tclObj object.}

\references{

\enumerate{
\item M.Futschik and T.Crompton (2004) \emph{Model selection and efficiency testing for normalization of cDNA microarray data},
\strong{Genome Biology}, 5:R60
\item M.Futschik and T.Crompton (2005),  Optimized
      normalization, visualization and quality testing for two-channel
      microarray data, \emph{Bioinformatics}, 21(8):1724-6    
\item  OLIN web-page: \url{http://itb.biologie.hu-berlin.de/~futschik/software/R/OLIN}}}

\author{ Matthias E. Futschik
(\url{http://itb.biologie.hu-berlin.de/~futschik})}
\note{ The newest versions of \code{OLINgui} can be found at the OLIN webpage 
 (\url{http://itb.biologie.hu-berlin.de/~futschik/software/R/OLIN}).}

\seealso{\code{\link[OLIN]{olin}} }

\keyword{misc }
\keyword{nonlinear}
\keyword{multivariate}
