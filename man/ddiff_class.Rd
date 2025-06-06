% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddiff_class.R
\name{ddiff_class}
\alias{ddiff_class}
\title{obtain types of variables from given two datasets}
\usage{
ddiff_class(df_ref, df, key = character(0), ...)
}
\arguments{
\item{df_ref}{reference data.frame relative to which comparison is made.}

\item{df}{a data.frame compared to the refernce}

\item{key}{Unique identifiers to link two datasets}

\item{...}{potential argument to added}
}
\value{
The types of variables from given two datasets.
}
\description{
This function could obtain types of variables from given two datasets. Note that types of variables are defined as follows : continuous, categorical and binary
}
\examples{
d = test_data()
ddiff_rpt <- ddiff_class(d$identical$new, d$identical$old, c("ID", "Days"))
data.tree::FromListSimple(ddiff_rpt)
str(ddiff_rpt)
}
