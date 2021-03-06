% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_checksums.R
\name{compute_obj_checksum}
\alias{compute_obj_checksum}
\title{Compute a checksum for the object}
\usage{
compute_obj_checksum(
  obj_with_UUID_and_checksum,
  base_outdir_for_checksum = "."
)
}
\arguments{
\item{obj_with_UUID_and_checksum}{object whose checksum is to be computed}

\item{base_outdir_for_checksum}{directory where temporary file for use in checksum
computation will be written and then deleted}
}
\value{
character string checksum of the object when object has been written
    out as a file
}
\description{
The point of this function is to give a quick way to compare two complex
objects to see whether they are really the same object excluding their UUID
and checksum fields.  This is useful when running lots of experiments over
and over during development where the same random seed may end up
accidentally being re-used and generating identical experiments.  When lots
experiments are archived, it may be necessary to quickly cross-compare many
archived experiments against each other and having a stored checksum will
make it easier to do this and therfore, easier to spot accidental
duplicates.
}
\details{
This function works by replacing the UUID and checksum fields of the local
copy of the object with empty strings and then writing the object copy to a
temporary file.  Then, a checksum is computed for that temporary file and
file.  The modified original object is not returned from the function.
}
