% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_seed_setting_utilities.R
\name{gen_new_seed_from_cur_time}
\alias{gen_new_seed_from_cur_time}
\title{Generate a new random seed based on the current time}
\usage{
gen_new_seed_from_cur_time()
}
\value{
Returns an integer seed value
}
\description{
This function is probably similar to the seed that set.seed() uses when
you don't give it an argument.  This function allows you to create a
similar seed but know what its value is.  Using set.seed() without an
argument doesn't allow you to easily regenerate the same thing without
saving the large array form of seed that R uses internally.
}
