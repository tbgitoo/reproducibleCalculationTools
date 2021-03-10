\name{compare_rda_contents}
\alias{compare_rda_contents}

\title{
compare_rda_contents
}
\description{
Compares the variables contained in two distinct .rda files to see if they contain the same data
}
\usage{
compare_rda_contents(path1,path2,numerical_tolerance=0,verbose=TRUE,exclude_variables=vector(mode="character",length=0),relative_tolerance_evaluation=FALSE)
}

\arguments{
  \item{path1}{
Path to the first R-Data file (a .rda or .RData file that can be opened with \link{load})
}
\item{path2}{Path to the second R-Data file (a .rda or .RData file that can be opened with \link{load})}


 \item{numerical_tolerance}{Allow for some numerical tolerance. This is the relative deviation, as compared to the mean value of two corresponding entries}
\item{verbose}{Should the function output what it is doing or remain silent}
\item{exclude_variables}{Variables that should be excluded from comparison, for example locally varying access paths or randomly drawn variables. It is also possible to use a syntax of the type "variable/column" to exclude individual columns for variables that do have column names such as \link{data.frame} variables.}
}

\value{
\code{TRUE} indicating that the two .rda files contain identical variables (for as far as the tests go) or \code{FALSE}, indicating that there is some differences.
}

\details{The function checks that the two R-Data files contain objects having the same name, and also performs some checks on the objects contained. These checks include whether variables (functions are not analyzed) have the same column names, and whether the values are identical (to within \code{numerical_tolerance} if \code{numerical_tolerance>0}). The function is meant to validate equality, so it doesn't deal with differently structured variables, and also not with functions. Lists are compared element by element. \cr\cr
Regarding numerical precision, this can be either checked on an absolute level (the default, \code{relative_tolerance_evaluation}) or relative to the mean absolute value of pairs of numbers being compared (when passing \code{relative_tolerance_evaluation=TRUE}). The function also attempts to apply comparison within numerical precision by conversion of strings to numerical values, but not within \link{data.frame} columns that explicitly have string column type. }


\author{
Thomas Braschler
}


\keyword{ misc }

\examples{
# Example 1: Compare identical rda files
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
identical_comparison=compare_rda_contents(path_base,path_base)
identical_comparison

# Example 2: Rda file 2 has one more variable
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_one_more_variable = system.file("one_more_variable.rda",package="reproducibleCalculationTools")
comparison_with_one_more_variable=compare_rda_contents(path_base,path_one_more_variable)
comparison_with_one_more_variable

# Example 3: Rda file 2 has one more variable, excluded during comparison
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_one_more_variable = system.file("one_more_variable.rda",package="reproducibleCalculationTools")
comparison_with_one_more_variable=compare_rda_contents(path_base,path_one_more_variable,exclude_variables="anotherNumber")
comparison_with_one_more_variable

#Example 4: rda file 2 has a different entry for a numerical variable
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_different_number = system.file("different_single_number.rda",package="reproducibleCalculationTools")
comparison_with_different_number=compare_rda_contents(path_base,path_different_number)
comparison_with_different_number

#Example 5: rda file 2 has a different entry for a numerical variable, but allow for different levels of absolute tolerance
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_different_number = system.file("different_single_number.rda",package="reproducibleCalculationTools")
comparison_with_different_number_large_tolerance=compare_rda_contents(path_base,path_different_number,numerical_tolerance=0.7)
comparison_with_different_number_large_tolerance

comparison_with_different_number_small_tolerance=compare_rda_contents(path_base,path_different_number,numerical_tolerance=1e-10)
comparison_with_different_number_small_tolerance


#Example 6: rda file 2 has a different entry for a numerical variable, but allow for different levels of relative tolerance
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_different_number = system.file("different_single_number.rda",package="reproducibleCalculationTools")
comparison_with_different_number_large_tolerance=compare_rda_contents(path_base,path_different_number,numerical_tolerance=0.2,relative_tolerance_evaluation=TRUE)
comparison_with_different_number_large_tolerance

comparison_with_different_number_small_tolerance=compare_rda_contents(path_base,path_different_number,numerical_tolerance=1e-6,relative_tolerance_evaluation=TRUE)
comparison_with_different_number_small_tolerance

#Example 7: rda file 2 has a different entry for a numerical variable, but allow for different levels of relative tolerance
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_different_number = system.file("different_single_number.rda",package="reproducibleCalculationTools")
comparison_with_different_number_large_tolerance=compare_rda_contents(path_base,path_different_number,numerical_tolerance=0.2,relative_tolerance_evaluation=TRUE)
comparison_with_different_number_large_tolerance

comparison_with_different_number_small_tolerance=compare_rda_contents(path_base,path_different_number,numerical_tolerance=1e-6,relative_tolerance_evaluation=TRUE)
comparison_with_different_number_small_tolerance

# Example 8: rda file 2 has a different string value
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_different_string = system.file("different_string.rda",package="reproducibleCalculationTools")
comparison_with_different_string=compare_rda_contents(path_base,path_different_string)
comparison_with_different_string
# The string cannot be converted to numerical but this shouldn't interfere
comparison_with_different_string=compare_rda_contents(path_base,path_different_string,numerical_tolerance=1e-6,relative_tolerance_evaluation=TRUE)
comparison_with_different_string

# Example 9: rda file 2 has a different string value in its dataframe example
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_different_string_dataframe = system.file("string_change_in_dataframe.rda",package="reproducibleCalculationTools")
comparison_with_different_string=compare_rda_contents(path_base,path_different_string_dataframe)
comparison_with_different_string
# The string cannot be converted to numerical but this shouldn't interfere
comparison_with_different_string=compare_rda_contents(path_base,path_different_string_dataframe,numerical_tolerance=1e-6,relative_tolerance_evaluation=TRUE)
comparison_with_different_string

# Example 10: rda file 2 has a different string value in its dataframe example, but the column is explicitly excluded
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_different_string_dataframe = system.file("string_change_in_dataframe.rda",package="reproducibleCalculationTools")
comparison_with_different_string=compare_rda_contents(path_base,path_different_string_dataframe,exclude_variables="aDataframe/string_column")
comparison_with_different_string

# Example 11: rda file 2 has a different string value in its dataframe example, exclusion of an unrelated column should not interfer with detection of difference
path_base = system.file("data_to_compare.rda",package="reproducibleCalculationTools")
path_different_string_dataframe = system.file("string_change_in_dataframe.rda",package="reproducibleCalculationTools")
comparison_with_different_string=compare_rda_contents(path_base,path_different_string_dataframe,exclude_variables="aDataframe/number_column")
comparison_with_different_string


}

