#===============================================================================

        #  compute_pu_and_spp_histograms_from_marxan_pusvpr_file.R

#===============================================================================

#  Hack to quiet CHECK command for data frame column names that CHECK flags
#  as "no visible binding for global variable".
#  This is the officially sanctioned hack for doing this, e.g., see
#  https://github.com/STAT545-UBC/Discussion/issues/451
#  https://github.com/tidyverse/magrittr/issues/29
#  http://r.789695.n4.nabble.com/globalVariables-td4593980.html

if(getRversion() >= "2.15.1")  utils::globalVariables(c("pu", "species", "amount"))

#===============================================================================

#-------------------------------------------------------------------------------

#' Compute distributions for planning units and species from marxan input file
#'
#' This function groups the data by planning unit to summarize the number of
#' species on each planning unit, then regroups the data by species to compute
#' the number of patches for each species and the total amount of each species
#' across all patches.  All 3 of these distributions are returned, each as a
#' distribution.  Each of them is also plotted.
#'
#-------------------------------------------------------------------------------

#' @param pu_spp_amt_df data frame containing the marxan planning unit vs.
#'     species data (i.e., data frame with columns for pu, species, and amount).
#'
#' @return Returns list containing 3 named elements, each of which is a data frame:
#'     num_spp_per_patch, num_PUs_per_spp, and amt_per_spp.
#'
#' @export

#-------------------------------------------------------------------------------

summarize_pu_and_spp_dist_data_from_pu_spp_amt_df <- function (pu_spp_amt_df)
    {
        #  Compute distribution of number of species on each patch.
    num_spp_per_patch <- dplyr::count (pu_spp_amt_df, pu)
    hist_num_spp_per_patch <- hist (num_spp_per_patch$n)

        #  Compute distribution of number of patches for each species.
    num_PUs_per_spp <- dplyr::count (pu_spp_amt_df, species)
    hist_num_PUs_per_spp <- hist (num_PUs_per_spp$n)

        #  Compute amount of each species rather than
        #  count of PUs each occurs on.
    grouped_by_spp <- dplyr::group_by (pu_spp_amt_df, species)
    amt_per_spp <- dplyr::summarize (grouped_by_spp, amt=sum(amount))
    plot (amt_per_spp$species, amt_per_spp$amt)

        #  Return the 3 distributions.
    pu_and_spp_dist_data <- list (num_spp_per_patch=num_spp_per_patch,
                                  num_PUs_per_spp=num_PUs_per_spp,
                                  amt_per_spp=amt_per_spp)

    return (pu_and_spp_dist_data)
}

#===============================================================================

#-------------------------------------------------------------------------------

#' Summarize PU and spp dist data from marxan puvspr input file
#'
#' Summarize planning unit and species distribution data from marxan puvspr
#' input file.
#'
#-------------------------------------------------------------------------------

#' @param infile_path character string giving path to the marxan puvspr.dat
#'     file to be analyzed (including the name of the file itself), e.g.,
#'     "~/marxan/input/puvspr.dat"
#'
#' @return Returns pu_and_spp_dist_data
#' @export
#'
#' @examples \dontrun{
#' too_long_path_part1 = "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/"
#' too_long_path_part2 = "1834_marxan_simulated_annealing.completedTzarEmulation/"
#' too_long_path_part3 = "RSrun_-COR-Wrap-Marxan_SA."
#' too_long_path_part4 = "408782d5-2bb6-4c09-8b18-f05ad3089b15/input/puvspr.dat"
#' infile_path <- paste0 (too_long_path_part1, too_long_path_part2,
#'                        too_long_path_part3, too_long_path_part4)
#' pu_and_spp_dist_data <-
#'   summarize_pu_and_spp_dist_data_from_marxan_puvspr_input_file (infile_path)
#' }

#-------------------------------------------------------------------------------

summarize_pu_and_spp_dist_data_from_marxan_puvspr_input_file <-
    function (infile_path)
    {
        #  Load marxan's puvspr.dat file into a data frame.
    pu_spp_amt_df <- read.csv (normalizePath (infile_path), header=TRUE)

        #  Extract the planning unit and species distributions from the
        #  data frame.
    pu_and_spp_dist_data <-
        summarize_pu_and_spp_dist_data_from_pu_spp_amt_df (pu_spp_amt_df)

    return (pu_and_spp_dist_data)
    }

#===============================================================================


