% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_agi.R
\name{calc_agi}
\alias{calc_agi}
\title{Helper function to clean wrongly allocated 2006 agi_category levels}
\usage{
calc_agi(x, y)
}
\arguments{
\item{x}{a00100 (AGI)}

\item{y}{n1 (number of returns)}
}
\description{
2006 Data had incorrectly specified agi_levels above 7 so best efforts to fix
}
