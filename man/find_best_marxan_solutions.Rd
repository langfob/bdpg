% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gscp_14a_compute_marxan_solution_scores.R
\name{find_best_marxan_solutions}
\alias{find_best_marxan_solutions}
\title{Find best marxan solutions}
\usage{
find_best_marxan_solutions(
  marxan_output_dir_path,
  num_spp,
  cor_PU_costs,
  app_PU_costs,
  cor_bpm,
  app_bpm,
  marxan_best_df_sorted_as_vector,
  plot_output_dir,
  largest_PU_ID,
  largest_spp_ID,
  targets,
  marxan_top_dir
)
}
\arguments{
\item{marxan_output_dir_path}{character string}

\item{num_spp}{integer number of species in the problem}

\item{cor_PU_costs}{numeric vector giving correct cost of each planning unit}

\item{app_PU_costs}{numeric vector giving apparent cost of each planning unit}

\item{cor_bpm}{matrix}

\item{app_bpm}{matrix}

\item{marxan_best_df_sorted_as_vector}{data frame}

\item{plot_output_dir}{character string}

\item{largest_PU_ID}{integer}

\item{largest_spp_ID}{integer}

\item{targets}{numeric vector}

\item{marxan_top_dir}{character string}
}
\value{
Returns nothing
}
\description{
Find best marxan soltuions
}
