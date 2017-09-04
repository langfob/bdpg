#===============================================================================
#
#                       do_graph_and_marxan_analysis.R
#
#  Run code to run marxan and dump results to file.
#
#  Used to do graph analysis here too, but no longer.  Should rename file.
#
#===============================================================================

#' Create an RSrun
#'
#' Create a run of a reserve selector
#'
#' Note that the forced_seed argument should normally be omitted.  It's only
#' there to allow the reproduction of a previous execution of the code that
#' generates this object.  If omitted, a seed is randomly chosen and saved
#' as the rand_seed slot in the object.  If you want to reproduce the generation
#' of the current object, you would use the value stored in the rand_seed
#' slot as the value of the forced_seed argument in this function call.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{basic_or_wrapped_or_comb_str}{
#' \preformatted{
#' basic_or_wrapped_or_comb_str :  chr "Wrap"
#' }}
#' \subsection{cor_or_app_str}{
#' \preformatted{
#' cor_or_app_str :  chr "COR"
#' }}
#' \subsection{method_name}{
#' \preformatted{
#' method_name :  chr "Marxan_SA"
#' }}
#' \subsection{prob_UUID}{
#' \preformatted{
#' prob_UUID :  chr "9b4b5b43-8be8-4e9c-bd7d-d1938d9921bd"
#' }}
#' \subsection{rsrun}{
#' \preformatted{
#' rsrun : Formal class 'RSrun' [package "bdpg"] with 12 slots
#' }}
#' \subsection{starting_dir}{
#' \preformatted{
#' starting_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{targets}{
#' \preformatted{
#' targets :  num [1:1277] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param prob_UUID UUID for the biodiversity problem the reserve selector is
#'     run over
#' @param targets numeric vector
#' @param starting_dir character string
#' @param cor_or_app_str character string
#' @param basic_or_wrapped_or_comb_str character string
#' @param method_name character string for reserve selection method, e.g.,
#'     "marxan_sa"
#'
#' @return Returns an RSrun object

#-------------------------------------------------------------------------------

create_RSrun <- function (prob_UUID,
                          targets,
                          starting_dir,
                          cor_or_app_str,
                          basic_or_wrapped_or_comb_str,
                          method_name,
                          forced_seed=NULL
                          )
    {
    new_seed = get_and_set_new_rand_seed ("Start of create_RSrun()",
                                          forced_seed)

    rsrun <- new ("RSrun")

    rsrun@UUID             <- uuid::UUIDgenerate()
    rsrun@run_on_prob_UUID <- prob_UUID

    rsrun@rand_seed        = new_seed

    rsrun@targets  <- targets

    rsrun@obj_type_str   = "RSrun_"
    rsrun@rs_method_name = method_name
    rsrun@cor_or_app_str = cor_or_app_str
    rsrun@basic_or_wrapped_or_comb_str = basic_or_wrapped_or_comb_str


    rsrun@file_name_prefix =
                            paste (rsrun@obj_type_str,
                                   rsrun@cor_or_app_str,
                                   rsrun@basic_or_wrapped_or_comb_str,
                                   rsrun@rs_method_name,
                                   sep='-')

    create_RSrun_dir_and_subdirs (rsrun, starting_dir)

#####Shouldn't this be save_rsrun?
#    rsrun <- save_rsprob (rsrun, starting_dir)
    rsrun <- save_rsrun (rsrun, starting_dir)

#docaids::doc_vars_in_this_func_once ()
    return (rsrun)
    }

#===============================================================================

#' Run marxan on COR problem and write output from all analysis
#'
#' Run marxan on COR problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{COR_bd_prob}{
#' \preformatted{
#' COR_bd_prob : Formal class 'Xu_wrapped_bd_problem' [package "bdpg"] with 36 slots
#' }}
#' \subsection{COR_marxan_run}{
#' \preformatted{
#' COR_marxan_run : Formal class 'RSrun' [package "bdpg"] with 12 slots
#' }}
#' \subsection{marxan_control_values}{
#' \preformatted{
#' marxan_control_values : List of 15
#'  $ marxan_PROP      : num 0.5
#'  $ marxan_RANDSEED  : num 10102
#'  $ marxan_NUMREPS   : num 4
#'  $ marxan_NUMITNS   : chr "1000000"
#'  $ marxan_STARTTEMP : num -1
#'  $ marxan_NUMTEMP   : num 10000
#'  $ marxan_COSTTHRESH: chr "0.00000000000000E+0000"
#'  $ marxan_THRESHPEN1: chr "1.40000000000000E+0001"
#'  $ marxan_THRESHPEN2: chr "1.00000000000000E+0000"
#'  $ marxan_RUNMODE   : num 1
#'  $ marxan_MISSLEVEL : num 1
#'  $ marxan_ITIMPTYPE : num 0
#'  $ marxan_HEURTYPE  : num -1
#'  $ marxan_CLUMPTYPE : num 0
#'  $ spf_const        : num 950
#' }}
#' \subsection{marxan_output_values}{
#' \preformatted{
#' marxan_output_values : List of 3
#'  $ marxan_best_df_sorted:'data.frame':	407 obs. of  2 variables:
#'  $ marxan_ssoln_df      :'data.frame':	407 obs. of  2 variables:
#'  $ marxan_mvbest_df     :'data.frame':	1277 obs. of  10 variables:
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1843_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1843_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{targets}{
#' \preformatted{
#' targets :  num [1:1277] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param COR_bd_prob a Xu_bd_problem
#' @param parameters list
#' @param src_rds_file_dir character string
#' @param targets numeric vector
#'
#' @return Returns nothing
#' @export
#'
#-------------------------------------------------------------------------------

do_COR_marxan_analysis_and_output <- function (COR_bd_prob, parameters,
                                               src_rds_file_dir=NULL,
                                               targets=rep(1,COR_bd_prob@num_spp))
    {
        #---------------
        #  Run marxan.
        #---------------

    COR_marxan_run <- create_RSrun (COR_bd_prob@UUID,
                                    targets,
                                    parameters$fullOutputDir_NO_slash,
                                    COR_bd_prob@cor_or_app_str,
                                    COR_bd_prob@basic_or_wrapped_or_comb_str,
                                    method_name = "Marxan_SA"
                                    )

    marxan_control_values = set_up_for_and_run_marxan_COR (COR_bd_prob,
                                                           COR_marxan_run,
                                                           parameters)

        #---------------------------
        #  Collect marxan results.
        #---------------------------

#                                             #  Guessing at these args for now...
#     marxan_output_values = read_COR_marxan_output_files (COR_marxan_run,
#                                                          COR_bd_prob,
#                                                          parameters)
#
# #docaids::doc_vars_in_this_func_once ()
#
#         #-----------------------------------------------
#         #  Dump all of the different kinds of results.
#         #-----------------------------------------------
#
# #     save_rsrun_results_data_for_one_rsrun (parameters,
# # #    build_full_output_df_for_one_RSrun (parameters,
# #                                         COR_marxan_run,
# #                                         COR_bd_prob
# #                                         # ,
# #                                         # parameters$run_ID
# #                                         )
    save_rsrun_results_data_for_one_rsrun (parameters,
                                              COR_marxan_run,
                                              COR_bd_prob,
                                              COR_bd_prob,
                                           src_rds_file_dir)

    if (FALSE)  #  2017 02 23 - BTL - rebuilding this code, so don't run it yet
    {
    create_COR_master_output_structure (marxan_control_values,
                                        marxan_output_values,
                                        parameters,

                                        COR_bd_prob,
                                        COR_marxan_run
                                        )

    }

    }  #  end function - do_COR_marxan_analysis_and_output

#===============================================================================
#===============================================================================
#===============================================================================

#' Run marxan on APP problem and write output from all analysis
#'
#' Run marxan on APP problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{APP_bd_prob}{
#' \preformatted{
#' APP_bd_prob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{APP_marxan_run}{
#' \preformatted{
#' APP_marxan_run : Formal class 'RSrun' [package "bdpg"] with 12 slots
#' }}
#' \subsection{COR_bd_prob}{
#' \preformatted{
#' COR_bd_prob : Formal class 'Xu_wrapped_bd_problem' [package "bdpg"] with 36 slots
#' }}
#' \subsection{marxan_control_values}{
#' \preformatted{
#' marxan_control_values : List of 15
#'  $ marxan_PROP      : num 0.5
#'  $ marxan_RANDSEED  : num 10102
#'  $ marxan_NUMREPS   : num 4
#'  $ marxan_NUMITNS   : chr "1000000"
#'  $ marxan_STARTTEMP : num -1
#'  $ marxan_NUMTEMP   : num 10000
#'  $ marxan_COSTTHRESH: chr "0.00000000000000E+0000"
#'  $ marxan_THRESHPEN1: chr "1.40000000000000E+0001"
#'  $ marxan_THRESHPEN2: chr "1.00000000000000E+0000"
#'  $ marxan_RUNMODE   : num 1
#'  $ marxan_MISSLEVEL : num 1
#'  $ marxan_ITIMPTYPE : num 0
#'  $ marxan_HEURTYPE  : num -1
#'  $ marxan_CLUMPTYPE : num 0
#'  $ spf_const        : num 950
#' }}
#' \subsection{marxan_output_values}{
#' \preformatted{
#' marxan_output_values : List of 3
#'  $ marxan_best_df_sorted:'data.frame':	407 obs. of  2 variables:
#'  $ marxan_ssoln_df      :'data.frame':	407 obs. of  2 variables:
#'  $ marxan_mvbest_df     :'data.frame':	1277 obs. of  10 variables:
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1845_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1845_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{targets}{
#' \preformatted{
#' targets :  num [1:1277] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param APP_bd_prob a Xu_bd_problem
#' @param COR_bd_prob a Xu_bd_problem
#' @param parameters list
#' @param src_rds_file_dir character string
#' @param targets numeric vector
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_APP_marxan_analysis_and_output <- function (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                               src_rds_file_dir=NULL,
                                               targets=rep(1,COR_bd_prob@num_spp)
                                               )
    {
        #---------------
        #  Run marxan.
        #---------------

    APP_marxan_run <- create_RSrun (APP_bd_prob@UUID,
                                    targets,
                                    parameters$fullOutputDir_NO_slash,
                                    APP_bd_prob@cor_or_app_str,
                                    APP_bd_prob@basic_or_wrapped_or_comb_str,
                                    method_name = "Marxan_SA"
                                    )

    marxan_control_values = set_up_for_and_run_marxan_APP (APP_bd_prob,
                                                           COR_bd_prob,
                                                           APP_marxan_run,
                                                           parameters)

        #---------------------------
        #  Collect marxan results.
        #---------------------------

#     marxan_output_values = read_APP_marxan_output_files (APP_marxan_run,
#                                                          APP_bd_prob,
#                                                          COR_bd_prob,
#                                                          parameters)
#
# #docaids::doc_vars_in_this_func_once ()
#
#         #-----------------------------------------------
#         #  Dump all of the different kinds of results.
#         #-----------------------------------------------
#
# #     save_rsrun_results_data_for_one_rsrun (parameters,
# # #    build_full_output_df_for_one_RSrun (parameters,
# #                                         APP_marxan_run,
# #                                         APP_bd_prob
# #                                         # ,
# #                                         # parameters$run_ID
# #                                         )
    save_rsrun_results_data_for_one_rsrun (parameters,
                                              APP_marxan_run,
                                              COR_bd_prob,
                                              APP_bd_prob,
                                           src_rds_file_dir)

    if (FALSE)  #  2017 02 23 - BTL - rebuilding this code, so don't run it yet
    {
                                        #  Guessing at these args for now...
    create_APP_master_output_structure (marxan_control_values,
                                        marxan_output_values,
                                        parameters,

                                        COR_bd_prob,
                                        APP_bd_prob,
                                        APP_marxan_run
                                        )
    }
    }  #  end function - do_APP_marxan_analysis_and_output

#===============================================================================



