% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/gscp_14c_plot_incremental_marxan_summed_solution_reps.R
\name{plot_incremental_marxan_summed_solution_reps_for_COR_and_APP}
\alias{plot_incremental_marxan_summed_solution_reps_for_COR_and_APP}
\title{Plot how marxan is actually doing vs. how marxan thinks it's doing}
\usage{
plot_incremental_marxan_summed_solution_reps_for_COR_and_APP(
  marxan_ssoln_df,
  cor_PU_costs,
  app_PU_costs,
  correct_solution_cost,
  app_optimum_cost,
  cor_bpm,
  app_bpm,
  num_spp,
  plot_output_dir
)
}
\arguments{
\item{marxan_ssoln_df}{data frame}

\item{cor_PU_costs}{numeric vector giving correct cost of each planning unit}

\item{app_PU_costs}{numeric vector giving apparent cost of each planning unit}

\item{correct_solution_cost}{numeric}

\item{app_optimum_cost}{numeric}

\item{cor_bpm}{matrix}

\item{app_bpm}{matrix}

\item{num_spp}{integer}

\item{plot_output_dir}{character string}
}
\value{
Returns nothing
}
\description{
Evaluate apparent summed solutions as a function of the correct
 problem structure and the apparent problem structure, i.e.,
 how marxan is really doing vs. how marxan thinks it's doing.
}
