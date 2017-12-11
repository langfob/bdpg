#===============================================================================

        #  compute_pu_and_spp_histograms_from_marxan_pusvpr_file.R

#===============================================================================

#' Compute distributions for planning units and species from marxan input file
#'
#' This function groups the data by planning unit to summarize the number of
#' species on each planning unit, then regroups the data by species to compute
#' the number of patches for each species and the total amount of each species
#' across all patches.  All 3 of these distributions are returned, each as a
#' distribution.  Each of them is also plotted.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{amt_per_spp}{
#' \preformatted{
#' amt_per_spp : Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1277 obs. of  2 variables:
#' $ species: int  1 2 3 4 5 6 7 8 9 10 ...
#' $ amt    : int  2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{grouped_by_spp}{
#' \preformatted{
#' grouped_by_spp : Classes ‘grouped_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	3037 obs. of  3 variables:
#' $ species: int  1 1 65 88 139 215 227 256 259 262 ...
#' $ pu     : int  1 2 2 2 2 2 2 2 2 2 ...
#' $ amount : int  1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{hist_num_PUs_per_spp}{
#' \preformatted{
#' hist_num_PUs_per_spp : List of 6
#' $ breaks  : num [1:11] 2 2.5 3 3.5 4 4.5 5 5.5 6 6.5 ...
#' $ counts  : int [1:10] 891 311 0 57 0 15 0 2 0 1
#' $ density : num [1:10] 1.3955 0.4871 0 0.0893 0 ...
#' $ mids    : num [1:10] 2.25 2.75 3.25 3.75 4.25 4.75 5.25 5.75 6.25 6.75
#' $ xname   : chr "num_PUs_per_spp$n"
#' $ equidist: logi TRUE
#' }}
#' \subsection{hist_num_spp_per_patch}{
#' \preformatted{
#' hist_num_spp_per_patch : List of 6
#' $ breaks  : num [1:10] 0 5 10 15 20 25 30 35 40 45
#' $ counts  : int [1:9] 300 31 1 1 2 14 25 15 4
#' $ density : num [1:9] 0.152672 0.015776 0.000509 0.000509 0.001018 ...
#' $ mids    : num [1:9] 2.5 7.5 12.5 17.5 22.5 27.5 32.5 37.5 42.5
#' $ xname   : chr "num_spp_per_patch$n"
#' $ equidist: logi TRUE
#' }}
#' \subsection{num_PUs_per_spp}{
#' \preformatted{
#' num_PUs_per_spp : Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1277 obs. of  2 variables:
#' $ species: int  1 2 3 4 5 6 7 8 9 10 ...
#' $ n      : int  2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{num_spp_per_patch}{
#' \preformatted{
#' num_spp_per_patch : Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	393 obs. of  2 variables:
#' $ pu: int  1 2 3 4 5 6 7 8 9 10 ...
#' $ n : int  1 32 1 38 1 36 1 40 1 33 ...
#' }}
#' \subsection{pu_and_spp_dist_data}{
#' \preformatted{
#' pu_and_spp_dist_data : List of 3
#' $ num_spp_per_patch:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	393 obs. of  2 variables:
#' $ num_PUs_per_spp  :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1277 obs. of  2 variables:
#' $ amt_per_spp      :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1277 obs. of  2 variables:
#' }}
#' \subsection{pu_spp_amt_df}{
#' \preformatted{
#'pu_spp_amt_df : 'data.frame':	3037 obs. of  3 variables:
#' $ species: int  1 1 65 88 139 215 227 256 259 262 ...
#' $ pu     : int  1 2 2 2 2 2 2 2 2 2 ...
#' $ amount : int  1 1 1 1 1 1 1 1 1 1 ...
#' }}
#'
#'
#' @param pu_spp_amt_df data frame containing the marxan planning unit vs.
#'     species data (i.e., data frame with columns for pu, species, and amount).
#'
#' @return Returns list containing 3 named elements, each of which is a data frame:
#'     num_spp_per_patch, num_PUs_per_spp, and amt_per_spp.
#'
#' @export

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

#docaids::doc_vars_in_this_func_once ()
    return (pu_and_spp_dist_data)
}

#-------------------------------------------------------------------------------

#' Summarize PU and spp dist data from marxan puvspr input file
#'
#' Summarize planning unit and species distribution data from marxan puvspr
#' input file.
#'
#' @param infile_path character string giving path to the marxan puvspr.dat
#'     file to be analyzed (including the name of the file itself), e.g.,
#'     "~/marxan/input/puvspr.dat"
#'
#' @return Returns pu_and_spp_dist_data
#' @export
#'
#' @examples \dontrun{
#' infile_path <- "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1834_marxan_simulated_annealing.completedTzarEmulation/RSrun_-COR-Wrap-Marxan_SA.408782d5-2bb6-4c09-8b18-f05ad3089b15/input/puvspr.dat"
#' pu_and_spp_dist_data <-
#'   summarize_pu_and_spp_dist_data_from_marxan_puvspr_input_file (infile_path)
#' }

summarize_pu_and_spp_dist_data_from_marxan_puvspr_input_file <-
    function (infile_path)
    {
        #  Load marxan's puvspr.dat file into a data frame.
    pu_spp_amt_df <- read.csv (normalizePath (infile_path), header=TRUE)

        #  Extract the planning unit and species distributions from the
        #  data frame.
    pu_and_spp_dist_data <-
        summarize_pu_and_spp_dist_data_from_pu_spp_amt_df (pu_spp_amt_df)

#docaids::doc_vars_in_this_func_once ()
    return (pu_and_spp_dist_data)
    }

#===============================================================================


