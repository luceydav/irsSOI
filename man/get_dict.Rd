% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dict.R
\name{get_dict}
\alias{get_dict}
\title{Import NBER IRS Annual Data Dictionary}
\usage{
get_dict(year)
}
\arguments{
\item{year}{int year between 2005-2021}
}
\description{
Given a year, scrape NBER dictionary and return a data.table
(https://www.nber.org/research/data/individual-income-tax-statistics-zip-code-data-soi)
}
\examples{
\dontrun{
dict <- get_dict(2016)}

}
