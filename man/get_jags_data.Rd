% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_jags_data.R
\name{get_jags_data}
\alias{get_jags_data}
\title{Reads in and formats data so that it can be crammed into JAGS model}
\usage{
get_jags_data(file_path, model)
}
\arguments{
\item{file_path}{(character) Directory to the xlsx file housing the input data. Note: the format of the excel table needs to be standardized (see WorkingDirectory/Data/2020-Data-Median.xlsx).}

\item{model}{(character) The model being run.}
}
\description{
Reads in and formats data so that it can be crammed into JAGS model
}
