% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_dirs.R
\name{get_RSprob_path_topdir}
\alias{get_RSprob_path_topdir}
\title{Build top directory path for RS problem}
\usage{
get_RSprob_path_topdir(rsprob, exp_root_dir)
}
\arguments{
\item{rsprob}{a reserve selection problem object, e.g., a Xu_bd_problem}

\item{exp_root_dir}{character string path to root directory of an
experiment, usually parameters$fullOutputDir_NO_slash}
}
\value{
Returns character string path to highest level directory for a
    reserve selection problem
}
\description{
Build a full path to the highest level directory for a reserve selection
problem.
}
