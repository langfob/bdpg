% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gscp_12_write_network_to_marxan_files.R
\name{compute_marxan_species_penalty_factor}
\alias{compute_marxan_species_penalty_factor}
\title{Compute marxan species penalty factor}
\usage{
compute_marxan_species_penalty_factor(spf_rule_to_use, num_spp, parameters)
}
\arguments{
\item{spf_rule_to_use}{character string}

\item{num_spp}{integer}

\item{parameters}{list}
}
\value{
numeric
}
\description{
Choosing spf values
}
\details{
Taken from pp. 38-39 of Marxan_User_Manual_2008.pdf.
 Particularly note the second paragraph, titled "Getting Started".

3.2.2.4 Conservation Feature Penalty Factor

Variable – ‘spf’ Required: Yes
Description: The letters ‘spf’ stands for Species Penalty Factor. This
variable is more correctly referred to as the Conservation Feature
Penalty Factor. The penalty factor is a multiplier that determines the
size of the penalty that will be added to the objective function if the
target for a conservation feature is not met in the current reserve
scenario (see Appendix B -1.4 for details of how this penalty is
calculated and applied). The higher the value, the greater the relative
penalty, and the more emphasis Marxan will place on ensuring that
feature’s target is met. The SPF thus serves as a way of distinguishing
the relative importance of different conservation features. Features of
high conservation value, for example highly threatened features or those
of significant social or economic importance, should have higher SPF
values than less important features. This signifies that you are less
willing to compromise their representation in the reserve system.
Choosing a suitable value for this variable is essential to achieving
good solutions in Marxan. If it is too low, the representation of
conservation features may fall short of the targets. If it is too high,
Marxan’s ability to find good solutions will be impaired (i.e. it will
sacrifice other system properties such as lower cost and greater
compactness in an effort to fully meet the conservation feature targets).

Getting Started: It will often require some experimentation to determine
appropriate SPFs. This should be done in an iterative fashion. A good
place to start is to choose the lowest value that is of the same order
of magnitude as the number of conservation features, e.g. if you have 30
features, start with test SPFs of, say, 10 for all features. Do a number
of repeat of runs (perhaps 10) and see if your targets are being met in
the solutions. If not all targets are being met try increasing the SPF
by a factor of two and doing the repeat runs again. When you get to a
point where all targets are being met, decrease the SPFs slightly and
see if they are still being met.

After test runs are sorted out, then
differing relative values can be applied, based on considerations such
as rarity, ecological significance, etc., as outlined above.
Even if all your targets are being met, always try lower values . By
trying to achieve the lowest SPF that produces satisfactory solutions,
Marxan has the greatest flexibility to find good solutions. In general,
unless you have some a priori reason to weight the inclusion of features
in your reserve system, you should start all features with the same SPF.
If however, the targets for one or two features are consistently being
missed even when all other features are adequately represented , it may
be appropriate to raise the SPF for these features. Once again, see the
MGPH for more detail on setting SPFs.
}
