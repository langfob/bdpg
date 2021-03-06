% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_error_to_spp_occupancy_data.R
\name{match_FP_and_FN_counts_to_smaller_of_the_two}
\alias{match_FP_and_FN_counts_to_smaller_of_the_two}
\title{Match FP and FN counts to smaller of the two}
\usage{
match_FP_and_FN_counts_to_smaller_of_the_two(
  num_TPs,
  num_TNs,
  FP_const_rate,
  FN_const_rate
)
}
\arguments{
\item{num_TPs}{integer}

\item{num_TNs}{integer}

\item{FP_const_rate}{numeric}

\item{FN_const_rate}{numeric}
}
\value{
Returns FP_FN_const_rate_pair list
}
\description{
Usually, the number of TNs and TPs will be unbalanced (i.e., there will
 usually be far more absences (TNs) than presences (TPs).
 Therefore, there will be far more opportunities to inject
 FPs than FNs or vice versa.
 Consequently, even if the FP and FN rates are set to the
 same value, there are likely to be far more FPs than FNs
 or vice versa in the apparent matrix.  In many ecological cases, this would
 yield an unreasonable number of False Positives since False Positives
 are generally much less likely than False Negatives in field studies.
}
\details{
So, this routine will adjust the error rate of the dominant value to
 yield the same _count_ as the other, e.g., if TNs are dominant, then the
 adjusted_P(FP) = num_FNs / num_TNs
}
