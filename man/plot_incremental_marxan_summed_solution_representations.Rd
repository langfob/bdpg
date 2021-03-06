% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/gscp_14c_plot_incremental_marxan_summed_solution_reps.R
\name{plot_incremental_marxan_summed_solution_representations}
\alias{plot_incremental_marxan_summed_solution_representations}
\title{Plot incremental marxan summed solution representations}
\usage{
plot_incremental_marxan_summed_solution_representations(
  marxan_ssoln_df,
  cor_PU_costs,
  optimum_cost,
  bpm,
  cor_app_prefix_string,
  num_spp,
  plot_output_dir
)
}
\arguments{
\item{marxan_ssoln_df}{data frame}

\item{cor_PU_costs}{numeric vector giving correct cost of each planning unit}

\item{optimum_cost}{numeric}

\item{bpm}{bipartite matrix; integer matrix with one row for each species
and one column for each planning unit.  Each matrix entry specifies
whether that species occupies that planning unit; 1 indicates the
species does occupy the planning unit and 0 indicates it does not.
Same as occ_matrix.}

\item{cor_app_prefix_string}{character string}

\item{num_spp}{integer number of species in the problem}

\item{plot_output_dir}{character string}
}
\value{
Returns vector subset of ranked marxan summed solution PU IDs that
first covers all species' targets
}
\description{
For each step in order by Marxan summed solution PU ID:
 Want the fraction of all species who have met or exceeded their target
 when all PUs with the same number of votes or more are included in the
 solution.
}
