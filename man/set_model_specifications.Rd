% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_model_specifications.R
\name{set_model_specifications}
\alias{set_model_specifications}
\title{Specify how the model should be run and document your thinking in log book (WorkingDirectory/Tables/run_info).}
\usage{
set_model_specifications(
  run_name,
  notes,
  n_chains = 4,
  n_iter = 5e+05,
  n_burnin = 250000,
  n_thin = 100,
  parallel = F
)
}
\arguments{
\item{run_name}{(character) run identifier. keep it short and informative.}

\item{notes}{(character) note to be stored in log book}

\item{n_chains}{(numeric) the number of chains}

\item{n_iter}{(numeric) the number of iterations excluding burnin}

\item{n_burnin}{(numeric) the number of iterations to discard as burnin}

\item{n_thin}{(numeric) the thinning period}

\item{parallel}{(logical) use parallel = T for parallel chains}
}
\description{
Specify how the model should be run and document your thinking in log book (WorkingDirectory/Tables/run_info).
}
