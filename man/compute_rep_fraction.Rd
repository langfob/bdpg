% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/gscp_10b_compute_solution_rep_levels_and_costs.R
\name{compute_rep_fraction}
\alias{compute_rep_fraction}
\title{Compute species representation fractions achieved}
\usage{
compute_rep_fraction(
  spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU,
  PU_set_to_test,
  spp_rep_targets = 1
)
}
\arguments{
\item{spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU}{integer matrix of
abundance of each species in each planning unit, with a row for each
species ID and a column for each planning unit ID}

\item{PU_set_to_test}{integer vector of planning unit IDs where each ID
specifies a planning unit to include in the set to be tested}

\item{spp_rep_targets}{numeric vector with a target abundance for each
species in the problem}
}
\value{
Returns spp_rep_fracs numeric vector of fractions of species
    representation targets achieved by a given set of planning units
}
\description{
Compute fractions of species representation targets (shortfall or overrep)
achieved by a given set of planning units given a spp rows by
PU columns adjacency matrix.
}
