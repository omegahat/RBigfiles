\name{bigfiles}
\alias{bigfiles}
\title{Find the largest files in a directory and its sub-directories}
\description{
  This is a function that is useful for identifying large files
  in a hierarchy of directories.  This can recursively process
  directories and return a data frame with information about the
  \code{num} largest files.
  One can exclude certain files (including directories)
  by using pattern matching on the  names
}
\usage{
bigfiles(dir = ".", recursive = TRUE, num = 50, maxDepth = 10, excludePattern = character(), files = list.files(dir, full.names = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{character, the top-level directory in which to find large files}
  \item{recursive}{a logical value indicating whether to process
    sub-directories or not.}
  \item{num}{the maximum number of files to return in the result.}
  \item{maxDepth}{a non-negative integer indicating how many
    sub-directories deep we should process. This is only used when
    \code{recursive} is \code{TRUE}.}
  \item{excludePattern}{a regular expression passed to
    \code{\link[base]{grep}} to identify files by  name
    that we should exclude. This applies to directories as well as
    files.  Directories that are excluded will not be processed recursively.
  }
  \item{files}{a character vector naming the top-level files to process.
    This is typically not specified by the caller. However, one can
    specify a subset of the files in the top-level directory, e.g.
    to exclude certain directories (or even files).
  }
}
\value{
  A data frame similar to \code{\link[base]{file.info}}
  with the largest files.
}
\author{
 Duncan Temple Lang 
}

\seealso{
  \code{\link[base]{file.info}}
  \code{\link[base]{list.files}}  
}
\examples{

\dontrun{  
  a = bigfiles("~/Projects/org/omegahat/R", maxDepth = 3, excludePattern = "(tar.gz|tu)$")
}

 big = bigfiles(.libPaths()[1])
 big = bigfiles(R.home())
}
\keyword{programming}
