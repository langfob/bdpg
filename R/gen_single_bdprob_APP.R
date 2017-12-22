#===============================================================================

#                       gen_single_bdprob_APP.R

#  Generate an APPARENT Xu problem from a CORRECT one.

#===============================================================================

#-------------------------------------------------------------------------------
#' Create and initialize apparent bdproblem from a correct problem.
#'
#'  All values that are altered for the apparent problem should be assumed to
#'  be altered in the APP_prob_info_class object in the problem's APP_prob_info
#'  slot.
#'  Values in the main part of the Xu_bdprob_APP object should be the same
#'  as the values for the underlying COR problem that the APP problem is
#'  adding error to, with the exception of a few object-specific things
#'  like the UUID of the APP problem itself.
#'
#-------------------------------------------------------------------------------
#' @param Xu_bdprob_COR a Xu_bd_problem
#' @inheritParams std_param_defns
#'
#' @return an apparent Xu_bd_problem
#-------------------------------------------------------------------------------

create_and_init_APP_bdprob <- function (Xu_bdprob_COR,
                                        parameters)
    {
    # forced_seed =
    #     get_forced_seed_value_if_necessary (is_rsrun = FALSE,
    #                                         is_rsprob = TRUE,
    #                                         parameters,
    #                                         "APP",
    #                                         Xu_bdprob_COR@basic_or_wrapped_or_comb_str)
    #
    # new_seed =
    #     set_new_or_forced_rand_seed_if_necessary (value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
    #                                               paste0 ("Start of create_and_init_APP_bdprob(),APP,",
    #                                                       Xu_bdprob_COR@basic_or_wrapped_or_comb_str),
    #                                               forced_seed)

    new_seed_list =
        set_new_or_forced_rand_seed_if_necessary (is_rsrun = FALSE,
                                                  is_rsprob = TRUE,
                                                  parameters,
                                                  cor_or_app_str = "APP",
                                                  basic_or_wrapped_or_comb_str = Xu_bdprob_COR@basic_or_wrapped_or_comb_str,
                                                  location_string = paste0 ("Start of create_and_init_APP_bdprob(),APP,",
                                                                            Xu_bdprob_COR@basic_or_wrapped_or_comb_str))

        #------------------------------------------------------------
        #  Save data known so far for the newly created Xu problem.
        #  Note that the base class here may be a basic Xu problem
        #  or a wrapped one.
        #  If it's not a basic one, then extra data may need to be
        #  copied into the APP problem.
        #------------------------------------------------------------

    Xu_bdprob_APP = new (class (Xu_bdprob_COR))

        #------------------------------------------------------------
        #  If the COR problem is a wrapped problem,
        #  then you need to copy the UUID of its base problem.
        #------------------------------------------------------------

    if (Xu_bdprob_COR@basic_or_wrapped_or_comb_str == "Wrap")
        Xu_bdprob_APP@UUID_of_base_problem_that_is_wrapped =
            Xu_bdprob_COR@UUID_of_base_problem_that_is_wrapped

        #---------------------------------------------------------------
        #  Assign a unique identifier to this newly generated problem.
        #  These IDs are useful when combining or adding error to
        #  problems so that you can identify exactly which problems
        #  were combined or used as a base when provenance might get
        #  confusing.
        #---------------------------------------------------------------

    Xu_bdprob_APP@UUID = uuid::UUIDgenerate()

    Xu_bdprob_APP@prob_is_ok                       = FALSE
    Xu_bdprob_APP@rand_seed                        = new_seed_list$seed_value
    Xu_bdprob_APP@R_internal_seed_array            = new_seed_list$R_internal_seed_array

        #-----------------------------------------------------------------
        #  Copy main problem values from the COR problem the APP problem
        #  is adding error to.  APP_prob_info_class values will be set
        #  outside this function.
        #-----------------------------------------------------------------

    Xu_bdprob_APP@prob_type     = Xu_bdprob_COR@prob_type
    Xu_bdprob_APP@prob_gen_info = Xu_bdprob_COR@prob_gen_info

    Xu_bdprob_APP@prob_generator_params_known      = Xu_bdprob_COR@prob_generator_params_known
    Xu_bdprob_APP@correct_solution_vector_is_known = Xu_bdprob_COR@correct_solution_vector_is_known

#    Xu_bdprob_APP@PU_spp_pair_indices       = Xu_bdprob_COR@PU_spp_pair_indices
    Xu_bdprob_APP@cor_PU_spp_pair_indices       = Xu_bdprob_COR@cor_PU_spp_pair_indices

    Xu_bdprob_APP@all_PU_IDs                = Xu_bdprob_COR@all_PU_IDs
    Xu_bdprob_APP@all_spp_IDs               = Xu_bdprob_COR@all_spp_IDs

    Xu_bdprob_APP@PU_col_name               = Xu_bdprob_COR@PU_col_name
    Xu_bdprob_APP@spp_col_name              = Xu_bdprob_COR@spp_col_name
    Xu_bdprob_APP@num_PUs                   = Xu_bdprob_COR@num_PUs
    Xu_bdprob_APP@num_spp                   = Xu_bdprob_COR@num_spp
    Xu_bdprob_APP@correct_solution_cost     = Xu_bdprob_COR@correct_solution_cost

    Xu_bdprob_APP@nodes                     = Xu_bdprob_COR@nodes

        #------------------------------------------------------------------
        #  IN FUTURE ERROR MODELS, PU_COSTS COULD BE CHANGED.
        #  IF THE COSTS ARE CHANGED THOUGH, THAT SHOULD BE DONE IN THE
        #  APP_prob_info OBJECT, NOT IN THE MAIN PROBLEM OBJECT.
        #  The COR costs should still be retained here for comparison and
        #  error calculations.
        #------------------------------------------------------------------

    Xu_bdprob_APP@PU_costs                  = Xu_bdprob_COR@PU_costs

    return (Xu_bdprob_APP)
    }

#===============================================================================

#-------------------------------------------------------------------------------
#' Create directories for this apparent problem.
#'
#-------------------------------------------------------------------------------
#' @param Xu_bdprob_APP an apparent Xu_bd_problem
#' @param basic_or_wrapped_or_comb_str a character string
#' @param starting_dir a character string
#'
#' @return an apparent Xu_bd_problem
#-------------------------------------------------------------------------------

create_dirs_for_APP_prob <- function (Xu_bdprob_APP,
                                      basic_or_wrapped_or_comb_str,
                                      starting_dir
                                      )
    {
        #  Build file name prefix.
    Xu_bdprob_APP@obj_type_str                     = "RSprob"
    Xu_bdprob_APP@basic_or_wrapped_or_comb_str     = basic_or_wrapped_or_comb_str
    Xu_bdprob_APP@cor_or_app_str                   = "APP"
    Xu_bdprob_APP@file_name_prefix =
                            paste (Xu_bdprob_APP@obj_type_str,
                                   Xu_bdprob_APP@cor_or_app_str,
                                   Xu_bdprob_APP@basic_or_wrapped_or_comb_str,
                                   sep='-')

        #  Create directories.
    create_RSprob_dir_and_subdirs (starting_dir, Xu_bdprob_APP)

#docaids::doc_vars_in_this_func_once ()
    return (Xu_bdprob_APP)
    }

#===============================================================================

#-------------------------------------------------------------------------------
#' Compute and save the distribution and network metrics for the problem.
#'
#-------------------------------------------------------------------------------
#' @inheritParams std_param_defns
#' @param Xu_bdprob_APP an apparent Xu_bd_problem
#' @param Xu_bdprob_COR a Xu_bd_problem
#' @param starting_dir a character string
#'
#' @return an apparent Xu_bd_problem
#-------------------------------------------------------------------------------

compute_and_save_dist_and_network_metrics_for_prob <- function (Xu_bdprob_APP,
                                                                Xu_bdprob_COR,
                                                                #compute_network_metrics_for_this_prob,
                                                                starting_dir,
                                                                parameters
                                                                )
    {
        #  Summarize and plot graph and distribution structure information.
    Xu_bdprob_APP@final_link_counts_for_each_node =
        summarize_and_plot_graph_and_distribution_structure_information (
#                  Xu_bdprob_APP@PU_spp_pair_indices,
                  Xu_bdprob_APP@APP_prob_info@app_PU_spp_pair_indices,

                  "APP",
                  Xu_bdprob_COR@all_PU_IDs,    #####!!!!!#####all_correct_node_IDs,

#                  Xu_bdprob_APP@derived_bdpg_dir_names$plot_output_dir,
                  get_RSprob_path_plots (Xu_bdprob_APP, starting_dir),

                  Xu_bdprob_APP@spp_col_name,
                  Xu_bdprob_APP@PU_col_name,
                  Xu_bdprob_APP@presences_col_name
                  )

        #  Compute network metrics.
    Xu_bdprob_APP =
        init_object_graph_data (
            Xu_bdprob_APP,
            starting_dir,

            parameters$compute_network_metrics,

            parameters$compute_network_metrics_APP,
            #compute_network_metrics_for_this_prob,

            parameters$use_igraph_metrics,
            parameters$use_bipartite_metrics,
            parameters$bipartite_metrics_to_use)

#docaids::doc_vars_in_this_func_once ()
    return (Xu_bdprob_APP)
    }

#===============================================================================

#-------------------------------------------------------------------------------
#' Add error to the species occupancy data and save to APP_prob_info structure.
#'
#-------------------------------------------------------------------------------
#' @inheritParams std_param_defns
#' @param Xu_bdprob_COR a Xu_bd_problem
#' @param Xu_bdprob_APP an apparent Xu_bd_problem
#'
#' @return an apparent Xu_bd_problem
#-------------------------------------------------------------------------------

create_APP_prob_info_by_adding_error_to_spp_occ_data <- function (Xu_bdprob_COR,
                                                                  Xu_bdprob_APP,
                                                                  parameters,
                                                                  bdpg_error_codes
                                                                  )
    {
    APP_prob_info = new ("APP_prob_info_class")

    APP_prob_info@UUID_of_base_problem_that_has_err_added = Xu_bdprob_COR@UUID

#-------------------------------------------------------------------------
#  2017 12 02 - BTL
#  Need to add APP costs to the object structure and set its value here.
#  Also need to have a way to specify an error model for APP costs.
#-------------------------------------------------------------------------

#  ???  app costs = cor costs  ???

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#  2017 12 02 - BTL
#  Most, if not all, of this section needs to change for different kinds
#  of errors.  Right now it just assumes all errors will be constant.
#
#  Need to figure out how to express that generically
#  both for the function calls to make and
#  for the values to return in the results output file.
#-------------------------------------------------------------------------

ret_vals_from_build_const_err =
  build_const_err_FP_and_FN_matrices (parameters,
                                      Xu_bdprob_COR@bpm,     #cor_bpm,
                                                  #cor_num_PU_spp_pairs,
                                      Xu_bdprob_COR@num_PUs,     #cor_num_PUs,
                                      Xu_bdprob_COR@num_spp,     #cor_num_spp,
                                      bdpg_error_codes)

APP_prob_info@original_FP_const_rate = ret_vals_from_build_const_err$original_FP_const_rate
APP_prob_info@original_FN_const_rate = ret_vals_from_build_const_err$original_FN_const_rate
APP_prob_info@match_error_counts     = ret_vals_from_build_const_err$match_error_counts
APP_prob_info@FP_const_rate          = ret_vals_from_build_const_err$FP_const_rate
APP_prob_info@FN_const_rate          = ret_vals_from_build_const_err$FN_const_rate

ret_vals_from_apply_errors =
    apply_const_error_to_spp_occupancy_data (Xu_bdprob_COR@num_PUs,     #cor_num_PUs,
                                             Xu_bdprob_COR@num_spp,     #cor_num_spp,
                                             Xu_bdprob_COR@bpm,         #cor_bpm,
                                             ret_vals_from_build_const_err$FP_rates_matrix,     #FP_rates_matrix,
                                             ret_vals_from_build_const_err$FN_rates_matrix,     #FN_rates_matrix,
                                             bdpg_error_codes)
      # apply_const_error_to_spp_occupancy_data (parameters,
      #                                  Xu_bdprob_COR@bpm,     #  cor_bpm,
      #                                  Xu_bdprob_COR@num_PU_spp_pairs,     #  cor_num_PU_spp_pairs,
      #                                  Xu_bdprob_COR@num_PUs,     #  cor_num_PUs,
      #                                  Xu_bdprob_COR@num_spp,     #  cor_num_spp,
      #                                  bdpg_error_codes)


    #  Save the realized error rates.

APP_prob_info@realized_FP_rate          = ret_vals_from_apply_errors$realized_FP_rate
APP_prob_info@realized_FN_rate          = ret_vals_from_apply_errors$realized_FN_rate

#-------------------------------------------------------------------------
#  2017 12 02 - BTL
#  This section probably needs to be solidified as either only using
#  the COR values or reporting the APP values but never using them
#  for any dimensioning and only using them as values to report.
#-------------------------------------------------------------------------

        #  Save the chosen error parameters to output later with results.

        #APP num_spp MAY DIFFER FROM COR IF A SPECIES IS MISSING IN APPARENT DATA?
        #NOT SURE WHAT ALL IT'S USED FOR THOUGH.  IF DIMENSIONING ARRAYS, IT
        #PROBABLY NEEDS TO STAY THE SAME VALUE AS COR AND JUST ALLOW SOME 0 VALUES.
    APP_prob_info@app_num_spp            = ret_vals_from_apply_errors$app_num_spp
        #THIS NEEDS TO MATCH COR_NUM_PUS DOESN'T IT?
    APP_prob_info@app_num_PUs            = ret_vals_from_apply_errors$app_num_PUs

        #  Set the values for the apparent problem structure.
    APP_prob_info@app_PU_spp_pair_indices      = ret_vals_from_apply_errors$app_PU_spp_pair_indices

#-------------------------------------------------------------------------

    Xu_bdprob_APP@APP_prob_info = APP_prob_info

        #NEEDS TO HAVE SAME DIMENSIONS AND ROW/COLUMN NAMES AS COR.
    Xu_bdprob_APP@bpm                      = ret_vals_from_apply_errors$app_spp_occupancy_data

    return (Xu_bdprob_APP)
    }

#===============================================================================

#' Generate a single biodiversity problem with error added to it
#'
#'  All values that are altered for the apparent problem should be assumed to
#'  be altered in the APP_prob_info_class object in the problem's APP_prob_info
#'  slot.
#'  Values in the main part of the Xu_bdprob_APP object should be the same
#'  as the values for the underlying COR problem that the APP problem is
#'  adding error to, with the exception of a few object-specific things
#'  like the UUID of the APP problem itself.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{app_dir_name_stem}{
#' \preformatted{
#' app_dir_name_stem :  chr "app"
#' }}
#' \subsection{APP_prob_info}{
#' \preformatted{
#' APP_prob_info : Formal class 'APP_prob_info_class' [package "bdpg"] with 9 slots
#' }}
#' \subsection{base_prob_name_stem}{
#' \preformatted{
#' base_prob_name_stem :  chr "base_prob"
#' }}
#' \subsection{bdpg_error_codes}{
#' \preformatted{
#' bdpg_error_codes : List of 6
#'  $ ERROR_STATUS_num_inside_or_within_group_links_less_than_one: num 1001
#'  $ ERROR_STATUS_optimal_solution_is_not_optimal               : num 1002
#'  $ ERROR_STATUS_num_nodes_per_group_must_be_at_least_2        : num 1003
#'  $ ERROR_STATUS_duplicate_spp_in_Xu_input_file                : num 1004
#'  $ ERROR_STATUS_unknown_spp_occ_FP_error_type                 : num 1005
#'  $ ERROR_STATUS_unknown_spp_occ_FN_error_type                 : num 1006
#' }}
#' \subsection{integerize}{
#' \preformatted{
#' integerize : function (x, digits = 0)
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1844_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1844_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{ret_vals_from_apply_errors}{
#' \preformatted{
#' ret_vals_from_apply_errors : List of 9
#'  $ original_FP_const_rate : num 0.1
#'  $ original_FN_const_rate : num 0.1
#'  $ match_error_counts     : logi TRUE
#'  $ FP_const_rate          : num 0.000588
#'  $ FN_const_rate          : num 0.1
#'  $ app_PU_spp_pair_indices:'data.frame':	3022 obs. of  2 variables:
#'  $ app_spp_occupancy_data : num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#'  $ app_num_spp            : int 1268
#'  $ app_num_PUs            : int 397
#' }}
#' \subsection{starting_dir}{
#' \preformatted{
#' starting_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1844_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{Xu_bdprob_APP}{
#' \preformatted{
#' Xu_bdprob_APP : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{Xu_bdprob_COR}{
#' \preformatted{
#' Xu_bdprob_COR : Formal class 'Xu_wrapped_bd_problem' [package "bdpg"] with 36 slots
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#' @param Xu_bdprob_COR correct Xu problem that is to have error added to it
#'
#' @return Returns apparent version of either a Xu_bd_problem or a
#'     Xu_wrapped_bd_problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_APP = function (Xu_bdprob_COR,
                            #compute_network_metrics_for_this_prob,
                                    #starting_dir,    #  not needed?  take from cor?
                                    parameters,
                                    bdpg_error_codes,
                        integerize,                           #  NOT USED ANYMORE?
                        base_prob_name_stem = "base_prob",    #  NOT USED ANYMORE?
                        app_dir_name_stem = "app"             #  NOT USED ANYMORE?
                                    )
    {
    Xu_bdprob_APP =
        create_and_init_APP_bdprob (Xu_bdprob_COR,
                                    parameters)

    Xu_bdprob_APP =
        create_APP_prob_info_by_adding_error_to_spp_occ_data (Xu_bdprob_COR,
                                                              Xu_bdprob_APP,
                                                              parameters,
                                                              bdpg_error_codes)

    starting_dir = parameters$fullOutputDir_NO_slash

    Xu_bdprob_APP =
        create_dirs_for_APP_prob (Xu_bdprob_APP,
                                  Xu_bdprob_COR@basic_or_wrapped_or_comb_str,
                                  starting_dir)

    Xu_bdprob_APP =
        compute_and_save_dist_and_network_metrics_for_prob (Xu_bdprob_APP,
                                                            Xu_bdprob_COR,
                                                            #compute_network_metrics_for_this_prob,
                                                            starting_dir,
                                                            parameters)

        #------------------------------------------------------------
        #  Everything seems to have worked.
        #  Save the bdprob to disk as a first cut at how to archive
        #  and retrieve problems in general.
        #  This particular bit of code may disappear later on, once
        #  it's clearer how to archive.
        #------------------------------------------------------------

    Xu_bdprob_APP@prob_is_ok = TRUE

    Xu_bdprob_APP <- save_rsprob (Xu_bdprob_APP, starting_dir)

#    save_rsprob_results_data_for_Xu_NOT_read_from_bench_file (Xu_bdprob_APP,
    save_rsprob_results_data (Xu_bdprob_APP, starting_dir, parameters)

#docaids::doc_vars_in_this_func_once ()
    return (Xu_bdprob_APP)
    }

#===============================================================================

