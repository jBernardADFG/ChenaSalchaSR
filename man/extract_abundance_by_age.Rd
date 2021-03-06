% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_abundance_by_age.R
\name{extract_abundance_by_age}
\alias{extract_abundance_by_age}
\title{Extract estimates of total run abundance by age from Jags output}
\usage{
extract_abundance_by_age(samples, alpha, file_path)
}
\arguments{
\item{samples}{(data.frame) output of clean_chains.}

\item{alpha}{(numeric) desired significance level for credible intervals.}

\item{file_path}{(character) file path to store output xlsx file.}
}
\description{
Extract estimates of total run abundance by age from Jags output
}
