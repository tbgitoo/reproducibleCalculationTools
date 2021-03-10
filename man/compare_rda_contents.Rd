\name{compare_rda_contents}
\alias{compare_rda_contents}

\title{
compare_rda_contents
}
\description{
Compares the variables contained in two distinct .rda files to see if they contain the same data
}
\usage{
compare_rda_contents(path1,path2,relative_numerical_tolerance=0,verbose=TRUE,exclude_variables=vector(mode="character",length=0))
}

\arguments{
  \item{path1}{
Path to the first R-Data file (a .rda or .RData file that can be opened with \link{load})
}
\item{path2}{Path to the second R-Data file (a .rda or .RData file that can be opened with \link{load})}


 \item{relative_numerical_tolerance}{Allow for some numerical tolerance. This is the relative deviation, as compared to the mean value of two corresponding entries}
\item{verbose}{Should the function output what it is doing or remain silent}
\item{exclude_variables}{Variables that should be excluded from comparison, for example locally varying access paths or randomly drawn variables. It is also possible to use a syntax of the type "variable/column" to exclude individual columns for variables that do have column names such as \link{data.frame} variables.}
}

\value{
\code{TRUE} indicating that the two .rda files contain identical variables (for as far as the tests go) or \code{FALSE}, indicating that there is some differences.
}

\details{The function checks that the two R-Data files contain objects having the same name, and also performs some checks on the objects contained. These checks include whether variables (functions are not analyzed) have the same column names, and whether the values are identical (to within \code{relative_numerical_tolerance} if \code{relative_numerical_tolerance>0}). The function is meant to validate equality, so it doesn't deal with differently structured variables, and also not with functions. Lists are compared element by element. }


\author{
Thomas Braschler
}


\keyword{ misc }

