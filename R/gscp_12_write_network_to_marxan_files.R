#===============================================================================

                #  gscp_12_write_network_to_marxan_files.R

#===============================================================================

#' Compute marxan species penalty factor
#'
#'  Choosing spf values
#'
#'  Taken from pp. 38-39 of Marxan_User_Manual_2008.pdf.
#'  Particularly note the second paragraph, titled "Getting Started".
#'
#' 3.2.2.4 Conservation Feature Penalty Factor
#'
#' Variable – ‘spf’ Required: Yes
#' Description: The letters ‘spf’ stands for Species Penalty Factor. This
#' variable is more correctly referred to as the Conservation Feature
#' Penalty Factor. The penalty factor is a multiplier that determines the
#' size of the penalty that will be added to the objective function if the
#' target for a conservation feature is not met in the current reserve
#' scenario (see Appendix B -1.4 for details of how this penalty is
#' calculated and applied). The higher the value, the greater the relative
#' penalty, and the more emphasis Marxan will place on ensuring that
#' feature’s target is met. The SPF thus serves as a way of distinguishing
#' the relative importance of different conservation features. Features of
#' high conservation value, for example highly threatened features or those
#' of significant social or economic importance, should have higher SPF
#' values than less important features. This signifies that you are less
#' willing to compromise their representation in the reserve system.
#' Choosing a suitable value for this variable is essential to achieving
#' good solutions in Marxan. If it is too low, the representation of
#' conservation features may fall short of the targets. If it is too high,
#' Marxan’s ability to find good solutions will be impaired (i.e. it will
#' sacrifice other system properties such as lower cost and greater
#' compactness in an effort to fully meet the conservation feature targets).
#'
#' Getting Started: It will often require some experimentation to determine
#' appropriate SPFs. This should be done in an iterative fashion. A good
#' place to start is to choose the lowest value that is of the same order
#' of magnitude as the number of conservation features, e.g. if you have 30
#' features, start with test SPFs of, say, 10 for all features. Do a number
#' of repeat of runs (perhaps 10) and see if your targets are being met in
#' the solutions. If not all targets are being met try increasing the SPF
#' by a factor of two and doing the repeat runs again. When you get to a
#' point where all targets are being met, decrease the SPFs slightly and
#' see if they are still being met.
#'
#' After test runs are sorted out, then
#' differing relative values can be applied, based on considerations such
#' as rarity, ecological significance, etc., as outlined above.
#' Even if all your targets are being met, always try lower values . By
#' trying to achieve the lowest SPF that produces satisfactory solutions,
#' Marxan has the greatest flexibility to find good solutions. In general,
#' unless you have some a priori reason to weight the inclusion of features
#' in your reserve system, you should start all features with the same SPF.
#' If however, the targets for one or two features are consistently being
#' missed even when all other features are adequately represented , it may
#' be appropriate to raise the SPF for these features. Once again, see the
#' MGPH for more detail on setting SPFs.
#'
#===============================================================================

#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 1277
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{spf_const}{
#' \preformatted{
#' spf_const :  num 950
#' }}
#' \subsection{spf_const_power_of_10}{
#' \preformatted{
#' spf_const_power_of_10 :  num 1000
#' }}
#' \subsection{spf_rule_to_use}{
#' \preformatted{
#' spf_rule_to_use :  chr "POWER_OF_10"
#' }}
#'
#-------------------------------------------------------------------------------

#' @param spf_rule_to_use character string
#' @param num_spp integer
#' @param parameters list
#'
#' @return numeric
#' @export

#-------------------------------------------------------------------------------

compute_marxan_species_penalty_factor <- function (spf_rule_to_use,
                                                   num_spp,
                                                   parameters)
    {
      if (spf_rule_to_use == "POWER_OF_10")
        {
        spf_const_power_of_10 = 10 ^ (floor (log10 (num_spp)))
            #  Marxan manual (quoted above) says to back off slightly
            #  from the power of 10.
            #  Not sure what "slightly" means, but I'll try decreasing by 5%.
        spf_const = round (0.95 * spf_const_power_of_10)    #  "decrease the SPFs slightly"

        } else if (spf_rule_to_use == "CONSTANT")
        {
        spf_const = parameters$marxan_spf_const
        } else
        {
        stop (paste0 ("\n\nERROR: marxan_spf_rule = '", spf_rule_to_use, "'",
             "\nMust be one of: POWER_OF_10 or CONSTANT.\n\n"))
        }

#docaids::doc_vars_in_this_func_once ()
      return (spf_const)
    }

#===============================================================================

#  Hack to quiet CHECK command for data frame column names that CHECK flags
#  as "no visible binding for global variable".
#  This is the officially sanctioned hack for doing this, e.g., see
#  https://github.com/STAT545-UBC/Discussion/issues/451
#  https://github.com/tidyverse/magrittr/issues/29
#  http://r.789695.n4.nabble.com/globalVariables-td4593980.html

if(getRversion() >= "2.15.1")  utils::globalVariables(c("pu", "species"))

#-------------------------------------------------------------------------------

    #  Write out the data as Marxan input files.

#-------------------------------------------------------------------------------

#' Write network to marxan input file
#'
#' Write the network as a marxan input file.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{costs}{
#' \preformatted{
#' costs :  num [1:407] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{marxan_input_dir}{
#' \preformatted{
#' marxan_input_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{marxan_output_dir}{
#' \preformatted{
#' marxan_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{num_PU_spp_pairs}{
#' \preformatted{
#' num_PU_spp_pairs :  int 3037
#' }}
#' \subsection{num_PUs}{
#' \preformatted{
#' num_PUs :  int 407
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 1277
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{PU_col_name}{
#' \preformatted{
#' PU_col_name :  chr "PU_ID"
#' }}
#' \subsection{pu_dat_file_to_cp}{
#' \preformatted{
#' pu_dat_file_to_cp :  chr "pu.dat /Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marx"| __truncated__
#' }}
#' \subsection{PU_IDs}{
#' \preformatted{
#' PU_IDs :  int [1:407] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{PU_spp_pair_indices}{
#' \preformatted{
#' PU_spp_pair_indices : 'data.frame':	3037 obs. of  2 variables:
#'  $ PU_ID : num  1 2 3 4 5 6 7 8 9 10 ...
#'  $ spp_ID: num  1 1 2 2 3 3 4 4 5 5 ...
#' }}
#' \subsection{puvspr_file_to_cp}{
#' \preformatted{
#' puvspr_file_to_cp :  chr "puvspr.dat /Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-"| __truncated__
#' }}
#' \subsection{spec_file_to_cp}{
#' \preformatted{
#' spec_file_to_cp :  chr "spec.dat /Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Ma"| __truncated__
#' }}
#' \subsection{spf_const}{
#' \preformatted{
#' spf_const :  num 950
#' }}
#' \subsection{spp_col_name}{
#' \preformatted{
#' spp_col_name :  chr "spp_ID"
#' }}
#' \subsection{spp_IDs}{
#' \preformatted{
#' spp_IDs :  int [1:1277] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{spp_PU_amount_table}{
#' \preformatted{
#' spp_PU_amount_table : 'data.frame':	3037 obs. of  3 variables:
#'  $ species: num  1 1 65 88 139 215 227 256 259 262 ...
#'  $ pu     : num  1 2 2 2 2 2 2 2 2 2 ...
#'  $ amount : num  1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{sppAmount}{
#' \preformatted{
#' sppAmount :  num 1
#' }}
#' \subsection{targets}{
#' \preformatted{
#' targets :  num [1:1277] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param PU_spp_pair_indices data frame
#' @param PU_IDs integer vector
#' @param spp_IDs integer vector
#' @param PU_col_name character string
#' @param spp_col_name character string
#' @param parameters lilst
#' @param marxan_input_dir character string
#' @param marxan_output_dir character string
#' @param spf_const numeric
#' @param targets numeric vector
#' @param costs numeric vector
#'
#' @return numeric spf_const
#' @export

#-------------------------------------------------------------------------------

write_network_to_marxan_files = function (PU_spp_pair_indices,       #  app values if running on app
                                          PU_IDs, #####!!!!!#####    #  All values, i.e., cor values?
                                          spp_IDs,  #####!!!!!#####  #  All values, i.e., cor values?

                                          PU_col_name,
                                          spp_col_name,
                                          parameters,
                                          marxan_input_dir,
                                          marxan_output_dir,

                                          spf_const,
                                          targets = rep (1, length (spp_IDs)),
                                          costs = rep (1, length (PU_IDs))
                                          )
    {

    cat ("\n\n--------------------  Writing out the data as Marxan input files.\n")

    sppAmount = 1

    #  Wasn't this value (num_PU_spp_pairs) already set earlier?
    #  Is this measuring the same thing and therefore, should not be reset here?

    num_PU_spp_pairs = length (PU_spp_pair_indices [,PU_col_name])

#####!!!!!#####    PU_IDs = unique (PU_spp_pair_indices [,PU_col_name])
    num_PUs = length (PU_IDs)

#####!!!!!#####    spp_IDs = unique (PU_spp_pair_indices [,spp_col_name])
    num_spp = length (spp_IDs)

    spp_PU_amount_table =
        data.frame (species = PU_spp_pair_indices [,spp_col_name],
                    pu      = PU_spp_pair_indices [,PU_col_name],
                    amount  = rep (sppAmount, num_PU_spp_pairs))

    #----------------------------------------------------------------------
    #  Sort the table in ascending order by species within planning unit.
    #  Taken from Wickham comment in:
    #  http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
    #
    #  BTL - 2014 11 28
    #
    #  Note that Marxan doesn't work correctly if the table is not sorted
    #  by planning unit ID (e.g., it can't find satisfying solutions).
    #  Ascelin said that the Marxan manual shows a picture of the values
    #  sorted incorrectly, i.e., by species.
    #
    #  I should probably move this arrange() call into the code that
    #  writes the table as a Marxan input file.  That would make sure
    #  that no one can write the table out incorrectly if they use that
    #  call.
    #----------------------------------------------------------------------

    spp_PU_amount_table = plyr::arrange (spp_PU_amount_table, pu, species)

    #-------------------------------------------------------------------------------

      #***  Need to modify the write_all...() function to prepend the
      #***  name of the directory to put the results in (but can
      #***  default to writing in "." instead?).
      #***  Will do this later because it's not a big deal to do the copies
      #***  below and when I make this change I will have to rebuild the
      #***  marxan package and move copies to the R library and the nectar
      #***  machines.

    # write_all_marxan_input_files (PU_IDs, spp_IDs, spp_PU_amount_table,
    #                             spf_const)
    write_all_marxan_input_files (PU_IDs,
                                  spp_IDs,
                                  spp_PU_amount_table,
                                  targets,
                                  costs,
                                  spf_const)

        #  Write pu.dat file.
    pu_dat_file_to_cp = paste0 (parameters$marxan_pu_file_name, " ", marxan_input_dir)
    system (paste0 ("cp ./", pu_dat_file_to_cp))

        #  Write spec file.
    spec_file_to_cp = paste0 (parameters$marxan_spec_file_name, " ", marxan_input_dir)
    system (paste0 ("cp ./", spec_file_to_cp))

        #  Write puvspr file.
    puvspr_file_to_cp = paste0 (parameters$marxan_puvspr_file_name, " ", marxan_input_dir)
    system (paste0 ("cp ./", puvspr_file_to_cp))

#docaids::doc_vars_in_this_func_once ()
    return (spf_const)
    }

#===============================================================================

