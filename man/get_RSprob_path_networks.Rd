% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_dirs.R
\name{get_RSprob_path_networks}
\alias{get_RSprob_path_networks}
\title{Get path to networks directory of RS problem}
\usage{
get_RSprob_path_networks(rsprob, exp_root_dir)
}
\arguments{
\item{rsprob}{a reserve selection problem object, e.g., a Xu_bd_problem}

\item{exp_root_dir}{character string path to root directory of an
experiment, usually parameters$fullOutputDir_NO_slash}
}
\value{
Returns character string path to directory where networks are written
    for a reserve selection problem
}
\description{
Build a full path to the directory where networks are written for a reserve
selection problem.
}
\section{Local Variable Structures and examples}{

Here is the output of str() for each variable visible in the function.
Note that the particular counts and values given are just examples to show
what the data might look like.

\subsection{exp_root_dir}{
\preformatted{
exp_root_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
}}
\subsection{networksdir}{
\preformatted{
networksdir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
}}
\subsection{rsprob}{
\preformatted{
rsprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
}}
}
