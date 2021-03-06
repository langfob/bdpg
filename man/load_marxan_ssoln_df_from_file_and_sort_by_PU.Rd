% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gscp_14_read_marxan_output_files.R
\name{load_marxan_ssoln_df_from_file_and_sort_by_PU}
\alias{load_marxan_ssoln_df_from_file_and_sort_by_PU}
\title{Load marxan's summed solutions vector from its marxan output file.}
\usage{
load_marxan_ssoln_df_from_file_and_sort_by_PU(
  marxan_output_dir_path,
  all_correct_node_IDs
)
}
\arguments{
\item{marxan_output_dir_path}{character string}

\item{all_correct_node_IDs}{integer vector}
}
\value{
Returns marxan_ssoln_df data frame
}
\description{
For each PU ID, it shows the number of solution vectors that
 included that PU in the solution.
}
