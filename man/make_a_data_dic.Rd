% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_a_data_dic.R
\name{make_a_data_dic}
\alias{make_a_data_dic}
\title{make a simple data dictionary}
\usage{
make_a_data_dic(a_df, var_of_interest = 1:dim(a_df)[2])
}
\arguments{
\item{a_df}{A dataframe}

\item{var_of_interest}{Numeric positions of interesting variables, and the default is to choose all the variables}
}
\description{
The output will be a simple data dictionary with outputs 'var_names', 'var_type', 'no_of_unique_values', 'sample_values'.
}
\examples{
temp_df <-
  data.frame(a = 1:10,
             b = NA,
             d = c(1:9, NA),
             e = c(letters[1:8], NA, NA),
             stringsAsFactors = FALSE)
x <- make_a_data_dic(temp_df)

}
\author{
Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}
}
