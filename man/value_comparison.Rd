\name{value_comparison}
\alias{value_comparison}

\title{
value_comparison
}
\description{
Compares two variables for identity to within specified precision
}
\usage{
value_comparison(v1,v2,verbose=TRUE,numerical_tolerance=0,relative_tolerance_evaluation=FALSE)
}

\arguments{
  \item{v1}{
First variable
}
\item{v2}{Second variable}
 \item{numerical_tolerance}{Allow for some numerical tolerance. This is the relative deviation, as compared to the mean value of two corresponding entries}
\item{verbose}{Should the function output what it is doing or remain silent}
\item{relative_tolerance_evaluation}{Should numerical precision in comparison of corresponding values be check at the level of the absolute differences (the default) or relative to the mean absolute value of the two numbers being compared (pass \code{TRUE}) for this).}
}

\value{
\code{TRUE} indicating that the variables are identical or at least sufficiently similar (for as far as the tests go) or \code{FALSE}, indicating that there is some differences.
}

\details{This function looks for identity of content, not identity of memory storage. Further, this function has been designed as an aid to \link{compare_rda_contents}. The variables should be reasonable, for instance, numbers, strings, vectors, or at most \link{data.frame}. The function is not designed to handle variables pointing to functions or lists with various types.}


\author{
Thomas Braschler
}


\keyword{ misc }

\examples{
# Example 1: Identical string values
value_comparison("the same","the same",verbose=FALSE) #Anticipated: TRUE

# Example 2: Non-identical string values
value_comparison("the same","not the same",verbose=FALSE) # Anticipated: FALSE

# Example 3: Vectors
value_comparison(rep("the same",5),rep("the same",5),verbose=FALSE) # Anticipated: TRUE
value_comparison(rep("the same",5),rep("the same",4),verbose=FALSE) # Anticipated: FALSE
value_comparison(rep("the same",5),c(rep("the same",4),"not the same"),verbose=FALSE) # Anticipated: FALSE

# Example 4: Attempt at numerical conversion
value_comparison(4,"4") # Anticipated TRUE, the function tries to see whether the values could not be converted to numerical

#Example 5: Difference in a dataframe
dataframe1=data.frame(col_A=c(0,1,2,3), col_B=c("A","B","C","D"))
dataframe2=data.frame(col_A=c(0,1,2,4), col_B=c("A","B","C","D"))
value_comparison(dataframe1,dataframe1) # Anticipated TRUE
value_comparison(dataframe1,dataframe2) # Anticipated FALSE

#Example 6: Numerical tolerance
value_comparison(5,"5.00001",numerical_tolerance=1e-3) # Anticipated: TRUE
value_comparison("5.0002","5.00001",numerical_tolerance=1e-3) # Anticipated: TRUE
value_comparison("5.1002","5.00001",numerical_tolerance=1e-3) # Anticipated: FALSE







}

