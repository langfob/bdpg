#===============================================================================

    #-------------------------------------------------------
    #  Generate an APPARENT Xu problem from a CORRECT one.
    #-------------------------------------------------------

#' Generate a single biodiversity problem with error added to it
#'
#' Create an apparent problem from a correct problem.
#'
#' @param Xu_bdprob_COR correct Xu problem that is to have error added to it
#' @param parameters named list of all parameters, generally from project.yaml file
#' @param bdpg_error_codes DESCRIPTION.
#' @param integerize DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @export

gen_single_bdprob_APP = function (Xu_bdprob_COR,
                                    #starting_dir,    #  not needed?  take from cor?
                                    parameters,
                                    bdpg_error_codes,
                                    integerize,
                        base_prob_name_stem = "base_prob",
                        app_dir_name_stem = "app"
                                    )
    {

        #------------------------------------------------------------
        #  Save data known so far for the newly created Xu problem.
        #------------------------------------------------------------

    Xu_bdprob_APP = new ("Xu_bd_problem")

        #---------------------------------------------------------------
        #  Assign a unique identifier to this newly generated problem.
        #  These IDs are useful when combining or adding error to
        #  problems so that you can identify exactly which problems
        #  were combined or used as a base when provenance might get
        #  confusing.
        #---------------------------------------------------------------

    Xu_bdprob_APP@UUID = uuid::UUIDgenerate()
    Xu_bdprob_APP@prob_is_ok                       = FALSE

        #------------------------------------------------------------------
        #  Save whatever information is known about the problem generator
        #  that produced this problem.
        #------------------------------------------------------------------

    Xu_bdprob_APP@prob_type     = Xu_bdprob_COR@prob_type
    Xu_bdprob_APP@prob_gen_info = Xu_bdprob_COR@prob_gen_info

    Xu_bdprob_APP@prob_generator_params_known      = Xu_bdprob_COR@prob_generator_params_known
    Xu_bdprob_APP@correct_solution_vector_is_known = Xu_bdprob_COR@correct_solution_vector_is_known

#CHANGE THESE?
    Xu_bdprob_APP@PU_spp_pair_indices       = Xu_bdprob_COR@PU_spp_pair_indices
# DO THE IDS NEED TO BE SAME AS COR THOUGH?
    Xu_bdprob_APP@all_PU_IDs                = Xu_bdprob_COR@all_PU_IDs
    Xu_bdprob_APP@all_spp_IDs               = Xu_bdprob_COR@all_spp_IDs

#CHANGE THESE?
    Xu_bdprob_APP@PU_col_name               = Xu_bdprob_COR@PU_col_name
    Xu_bdprob_APP@spp_col_name              = Xu_bdprob_COR@spp_col_name
    Xu_bdprob_APP@num_PUs                   = Xu_bdprob_COR@num_PUs
    Xu_bdprob_APP@num_spp                   = Xu_bdprob_COR@num_spp
    Xu_bdprob_APP@correct_solution_cost          = Xu_bdprob_COR@correct_solution_cost
#IN FUTURE ERROR MODELS, THESE COSTS COULD BE CHANGED.
    Xu_bdprob_APP@PU_costs                  = Xu_bdprob_COR@PU_costs

#WHAT IS STORED IN NODES?  NOT SURE IF IT NEEDS TO CHANGE OR NOT.
#I SUSPECT IT'S ONLY USED IN GENERATION OF THE ORIGINAL XU PROBLEM.
    Xu_bdprob_APP@nodes                     = Xu_bdprob_COR@nodes

        #-------------------------------------------------------------
        #  App data.
        #-------------------------------------------------------------


#===============================================================================
#===============================================================================
#===============================================================================
#                   Add error to the species occupancy data.
#===============================================================================

    APP_prob_info = new ("APP_prob_info_class")

    APP_prob_info@UUID_of_base_problem_that_has_err_added = Xu_bdprob_COR@UUID

    ret_vals_from_apply_errors =
        apply_error_to_spp_occupancy_data (parameters,
                                         Xu_bdprob_COR@bpm,     #  cor_bpm,
                                         Xu_bdprob_COR@num_PU_spp_pairs,     #  cor_num_PU_spp_pairs,
                                         Xu_bdprob_COR@num_PUs,     #  cor_num_PUs,
                                         Xu_bdprob_COR@num_spp,     #  cor_num_spp,
                                         bdpg_error_codes)

        #  Save the chosen error parameters to output later with results.

    APP_prob_info@original_FP_const_rate = ret_vals_from_apply_errors$original_FP_const_rate
    APP_prob_info@original_FN_const_rate = ret_vals_from_apply_errors$original_FN_const_rate
    APP_prob_info@match_error_counts     = ret_vals_from_apply_errors$match_error_counts
    APP_prob_info@FP_const_rate          = ret_vals_from_apply_errors$FP_const_rate
    APP_prob_info@FN_const_rate          = ret_vals_from_apply_errors$FN_const_rate

#THIS MAY DIFFER FROM COR IF A SPECIES IS MISSING IN APPARENT DATA?
#NOT SURE WHAT ALL IT'S USED FOR THOUGH.  IF DIMENSIONING ARRAYS, IT
#PROBABLY NEEDS TO STAY THE SAME VALUE AS COR AND JUST ALLOW SOME 0 VALUES.
    APP_prob_info@app_num_spp            = ret_vals_from_apply_errors$app_num_spp
#THIS NEEDS TO MATCH COR_NUM_PUS DOESN'T IT?
    APP_prob_info@app_num_PUs            = ret_vals_from_apply_errors$app_num_PUs

        #  Set the values for the apparent problem structure.
    APP_prob_info@app_PU_spp_pair_indices      = ret_vals_from_apply_errors$app_PU_spp_pair_indices

#===============================================================================

    Xu_bdprob_APP@APP_prob_info          = APP_prob_info

    #NEEDS TO HAVE SAME DIMENSIONS AND ROW/COLUMN NAMES AS COR.
    Xu_bdprob_APP@bpm                      = ret_vals_from_apply_errors$app_spp_occupancy_data

#browser()

#===============================================================================
#===============================================================================
#===============================================================================


        #-----------------------------------------------------------
        #  Convert PU/spp data structure into other formats needed
        #  downstream.
        #-----------------------------------------------------------

    # bpm =
        # create_adj_matrix_with_spp_rows_vs_PU_cols (Xu_bdprob_APP@num_spp,
        #                                             Xu_bdprob_APP@num_PUs,
        #                                             Xu_bdprob_APP@PU_spp_pair_indices,
        #                                     Xu_bdprob_APP@PU_costs,
        #                                             Xu_bdprob_APP@spp_col_name,
        #                                             Xu_bdprob_APP@PU_col_name,
        #                                             PU_spp_pair_info@dependent_node_IDs,
        #                                             PU_spp_pair_info@correct_solution_vector_is_known,
        #                                             bdpg_error_codes)

#    Xu_bdprob_APP@bpm = app_bpm


        #-------------------------------------------------------------
        #  Quit if there are any duplicate edges/spp in the problem.
        #-------------------------------------------------------------

# CHANGE THIS?
#  2017 02 10 - BTL
#  Getting error when I run this, but I don't think it should be an error
#  because if you add false positives, then I think that would allow you to get
#  duplicate links.  So, I'm going to remove this call for apparent, but not
#  for correct.

#     see_if_there_are_any_duplicate_links (Xu_bdprob_APP@bpm,
#
# #                                          Xu_bdprob_APP@num_spp,
#                                           Xu_bdprob_COR@num_spp,   #  correct?
#
#                                           bdpg_error_codes)


        #----------------------------------------
        #  Create directories for this problem.
        #----------------------------------------

    starting_dir = parameters$fullOutputDir_NO_slash
    create_RSprob_dir_and_subdirs (starting_dir, Xu_bdprob_APP)

        #-----------------------------------------------------------------
        #  Compute and save the distribution and network metrics for the
        #  problem.
        #-----------------------------------------------------------------

        #  Summarize and plot graph and distribution structure information.
    Xu_bdprob_APP@final_link_counts_for_each_node =
        summarize_and_plot_graph_and_distribution_structure_information (
                  Xu_bdprob_APP@PU_spp_pair_indices,
                  "APP",
                  Xu_bdprob_COR@all_PU_IDs,    #####!!!!!#####all_correct_node_IDs,

#                  Xu_bdprob_APP@derived_bdpg_dir_names$plot_output_dir,
                  get_RSprob_path_plots (Xu_bdprob_APP, starting_dir),

                  Xu_bdprob_APP@spp_col_name,
                  Xu_bdprob_APP@PU_col_name,
                  Xu_bdprob_APP@presences_col_name
                  )

        #  Compute network metrics.
    Xu_bdprob_APP <- init_object_graph_data (Xu_bdprob_APP,
                                             starting_dir,
                                             parameters$compute_network_metrics_APP,
                                             parameters$use_igraph_metrics,
                                             parameters$use_bipartite_metrics,
                                             parameters$bipartite_metrics_to_use)

#     Xu_bdprob_APP@compute_network_metrics = parameters$compute_network_metrics_APP
#     if (parameters$compute_network_metrics_APP)
#         {
#         Xu_bdprob_APP@bipartite_metrics_from_bipartite_package =
#           compute_network_measures_using_bipartite_package (Xu_bdprob_APP@bpm)
#
#         Xu_bdprob_APP@bipartite_metrics_from_igraph_package_df =
#           compute_igraph_related_network_measures (
#                                     Xu_bdprob_APP@PU_spp_pair_indices,
#
# #                                    Xu_bdprob_APP@derived_bdpg_dir_names$network_output_dir,
#                                     get_RSprob_path_networks (Xu_bdprob_APP, starting_dir),
#
#
#                                     Xu_bdprob_APP@PU_col_name,
#                                     Xu_bdprob_APP@spp_col_name
#                                                     )
#         }

        #------------------------------------------------------------
        #  Everything seems to have worked.
        #  Save the bdprob to disk as a first cut at how to archive
        #  and retrieve problems in general.
        #  This particular bit of code may disappear later on, once
        #  it's clearer how to archive.
        #------------------------------------------------------------

    Xu_bdprob_APP@prob_is_ok = TRUE

    Xu_bdprob_APP@basic_or_wrapped_str = Xu_bdprob_COR@basic_or_wrapped_str

#    Xu_bdprob_APP@full_saved_bdprob_path =
        save_bdprob (Xu_bdprob_APP@basic_or_wrapped_str, "APP",
                     Xu_bdprob_APP@UUID,
                     get_RSprob_path_topdir (Xu_bdprob_APP, starting_dir),
                     # Xu_bdprob_APP@prob_outdir,
                     Xu_bdprob_APP)

    return (Xu_bdprob_APP)
    }

#===============================================================================

