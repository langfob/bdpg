#===============================================================================

                #  gscp_15a_write_prob_results.R

#===============================================================================

load_results_output_values_common_to_all_Xu_problems <- function (results_list,
                                                                  rsprob,
                                                                  run_ID    #parameters
                                                                  )
    {
    #---------------------------------------------------------------------------

    results_list$rsp_tzar_run_ID                                                = run_ID    #parameters$run_id    #  tzar run ID, not RSrun ID

    results_list$rsp_UUID                                                       = rsprob@UUID
    results_list$rsp_checksum                                                   = rsprob@checksum
    results_list$rsp_rand_seed                                                  = rsprob@rand_seed
    results_list$rsp_cor_or_app_str                                             = rsprob@cor_or_app_str
    results_list$rsp_basic_or_wrapped_or_comb_str                               = rsprob@basic_or_wrapped_or_comb_str
    results_list$rsp_file_name_prefix                                           = rsprob@file_name_prefix

    results_list$rsp_prob_is_ok                                                 = rsprob@prob_is_ok
    results_list$rsp_prob_generator_params_known                                = rsprob@prob_generator_params_known

        #  prob_gen_info
    results_list$rsp_read_Xu_problem_from_Xu_file                               = rsprob@prob_gen_info@read_Xu_problem_from_Xu_file
    results_list$rsp_infile_name                                                = rsprob@prob_gen_info@infile_name

    #---------------------------------------------------------------------------

    results_list$rsp_num_PUs                                                        = rsprob@num_PUs
    results_list$rsp_num_spp                                                        = rsprob@num_spp
    results_list$rsp_num_spp_per_PU                                                 = rsprob@num_spp / rsprob@num_PUs#    results_list$rsp_seed                                                           = parameters$rsp_seed

    results_list$rsp_correct_solution_cost                                          = rsprob@correct_solution_cost
    results_list$rsp_correct_solution_vector_is_known                               = rsprob@correct_solution_vector_is_known

    #---------------------------------------------------------------------------

        #  Post-generation measures
    results_list$rsp_compute_network_metrics                                        = rsprob@compute_network_metrics
    results_list$rsp_use_igraph_metrics                                             = rsprob@use_igraph_metrics
    results_list$rsp_use_bipartite_metrics                                          = rsprob@use_bipartite_metrics
    results_list$rsp_bipartite_metrics_to_use                                       = rsprob@bipartite_metrics_to_use

    #---------------------------------------------------------------------------

    if (rsprob@cor_or_app_str == "APP")
        {
            #  This is an apparent problem, so retrieve APP controls.
        APP_prob_info = rsprob@APP_prob_info

        } else  #  This is a correct problem, so initialize empty APP controls.
        {
        APP_prob_info = new ("APP_prob_info_class")
        }

    #--------------------

            #  Error generation parameters
            #  Even though these values are meaningless for COR problems,
            #  I'm going to write something out for them anyway
            #  just to keep the output tables having the same size and
            #  column headings, which should make merging tables simpler.

    results_list$rsp_UUID_of_base_problem_that_has_err_added                        = APP_prob_info@UUID_of_base_problem_that_has_err_added

    results_list$rsp_cost_error_bound                                               = APP_prob_info@cost_error_bound

    results_list$rsp_original_FP_const_rate                                         = APP_prob_info@original_FP_const_rate
    results_list$rsp_original_FN_const_rate                                         = APP_prob_info@original_FN_const_rate
    results_list$rsp_match_error_counts                                             = APP_prob_info@match_error_counts
    results_list$rsp_FP_const_rate                                                  = APP_prob_info@FP_const_rate
    results_list$rsp_FN_const_rate                                                  = APP_prob_info@FN_const_rate

    results_list$rsp_realized_FP_rate                                               = APP_prob_info@realized_FP_rate
    results_list$rsp_realized_FN_rate                                               = APP_prob_info@realized_FN_rate

    results_list$rsp_app_num_spp                                                    = APP_prob_info@app_num_spp
    results_list$rsp_app_num_PUs                                                    = APP_prob_info@app_num_PUs

    #---------------------------------------------------------------------------

        #  Should we also be writing out the wrapping parameters?

    if (rsprob@basic_or_wrapped_or_comb_str == "Wrap")
        {
        results_list$rsp_UUID_of_base_problem_that_is_wrapped                       = rsprob@UUID_of_base_problem_that_is_wrapped

        } else  #  Not a wrapped problem, so just write a dummy value.
        {
        results_list$rsp_UUID_of_base_problem_that_is_wrapped                       = as.character (NA)
        }

    #---------------------------------------------------------------------------

    return (results_list)
    }

#===============================================================================

load_results_output_values_specific_to_Xu_benchmark_problems <-
    function (results_list, rsprob)
    {
        #------------------------------------------------------------------
        #  Load variables known for the benchmark results output but that
        #  come from a different source than for non-benchmark problems.
        #  The values are the same, but are not stored in an auxiliary
        #  structure (Xu_parameters) the way they are for non-benchmark
        #  problems, so they have to be set explicitly here so that the
        #  column names match in the results output file.
        #------------------------------------------------------------------

    results_list$rsp_tot_num_nodes                           = rsprob@num_PUs
    results_list$rsp_num_independent_set_nodes               = rsprob@num_PUs - rsprob@correct_solution_cost
    results_list$rsp_num_dependent_set_nodes                 = rsprob@correct_solution_cost
    results_list$rsp_opt_solution_as_frac_of_tot_num_nodes   = rsprob@correct_solution_cost / rsprob@num_PUs

    #---------------------------------------------------------------------------

        #-------------------------------------------------------------
        #  Load dummy values for all remaining data that is specific
        #  to non-benchmark Xu problems, i.e., ones created from
        #  scratch.
        #-------------------------------------------------------------

        #  Xu_base_params
    results_list$rsp_alpha__                         = NA
    results_list$rsp_n__num_groups                   = NA
    results_list$rsp_p__prop_of_links_between_groups = NA
    results_list$rsp_r__density                      = NA

    #-----------------------------

        #  Xu_bdpg_extended_params
    results_list$rsp_alpha___lower_bound                                            = NA
    results_list$rsp_alpha___upper_bound                                            = NA

    results_list$rsp_derive_alpha_from_n__num_groups_and_opt_frac_0.5               = NA
    results_list$rsp_use_unif_rand_alpha__                                          = NA

    results_list$rsp_n__num_groups                                                  = NA

    results_list$rsp_n__num_groups_lower_bound                                      = NA
    results_list$rsp_n__num_groups_upper_bound                                      = NA
    results_list$rsp_use_unif_rand_n__num_groups                                    = NA

    results_list$rsp_num_independent_nodes_per_group                                = NA

    results_list$rsp_use_unif_rand_p__prop_of_links_between_groups                  = NA
    results_list$rsp_p__prop_of_links_between_groups_lower_bound                    = NA
    results_list$rsp_p__prop_of_links_between_groups_upper_bound                    = NA
    results_list$rsp_base_for_target_num_links_between_2_groups_per_round           = NA
    results_list$rsp_at_least_1_for_target_num_links_between_2_groups_per_round     = NA  #  Not used?  See comment in gscp_5...R.

    results_list$rsp_use_unif_rand_r__density                                       = NA
    results_list$rsp_r__density_lower_bound                                         = NA
    results_list$rsp_r__density_upper_bound                                         = NA

    #-----------------------------

        #  Xu_derived_params
    results_list$rsp_num_nodes_per_group                                            = NA
    results_list$rsp_num_rounds_of_linking_between_groups                           = NA
    results_list$rsp_target_num_links_between_2_groups_per_round                    = NA
    results_list$rsp_num_links_within_one_group                                     = NA
    results_list$rsp_tot_num_links_inside_groups                                    = NA
    results_list$rsp_max_possible_num_links_between_groups                          = NA
    results_list$rsp_max_possible_tot_num_links                                     = NA
    results_list$rsp_max_possible_tot_num_node_link_pairs                           = NA

    #  Isn't this a DUPLICATE from Xu_bdpg_extended_params?
    #results_list$rsp_num_independent_nodes_per_group                                = NA

    #---------------------------------------------------------------------------

    return (results_list)
    }

#===============================================================================

load_results_output_values_specific_to_non_benchmark_Xu_problem <-
    function (results_list, rsprob)
    {
        #------------------------------------------------------------
        #  Load the auxiliary data structures that hold much of the
        #  information about a Xu problem created from scratch.
        #  These are empty for Xu benchmark problems.
        #------------------------------------------------------------

    Xu_parameters           = rsprob@prob_gen_info@Xu_parameters
    Xu_base_params          = Xu_parameters@base_params
    Xu_bdpg_extended_params = Xu_parameters@bdpg_extended_params
    Xu_derived_params       = Xu_parameters@derived_params

    #---------------------------------------------------------------------------

        #--------------------------------------------------------------------
        #  Load the variables that are also known for the benchmark results
        #  but from a different source.
        #--------------------------------------------------------------------

    results_list$rsp_tot_num_nodes                                                  = Xu_derived_params@tot_num_nodes
    results_list$rsp_num_independent_set_nodes                                      = Xu_derived_params@num_independent_set_nodes
    results_list$rsp_num_dependent_set_nodes                                        = Xu_derived_params@num_dependent_set_nodes
    results_list$rsp_opt_solution_as_frac_of_tot_num_nodes                          = Xu_derived_params@opt_solution_as_frac_of_tot_num_nodes

    #---------------------------------------------------------------------------

        #-------------------------------------------------------------
        #  Load all remaining data that is specific to non-benchmark
        #  Xu problems, i.e., ones created from scratch.
        #-------------------------------------------------------------

        #  Xu_base_params
    results_list$rsp_alpha__                         = Xu_base_params@alpha__
    results_list$rsp_n__num_groups                   = Xu_base_params@n__num_groups
    results_list$rsp_p__prop_of_links_between_groups = Xu_base_params@p__prop_of_links_between_groups
    results_list$rsp_r__density                      = Xu_base_params@r__density

    #-----------------------------

        #  Xu_bdpg_extended_params
    results_list$rsp_alpha___lower_bound                                            = Xu_bdpg_extended_params@alpha___lower_bound
    results_list$rsp_alpha___upper_bound                                            = Xu_bdpg_extended_params@alpha___upper_bound

    results_list$rsp_derive_alpha_from_n__num_groups_and_opt_frac_0.5               = Xu_bdpg_extended_params@derive_alpha_from_n__num_groups_and_opt_frac_0.5
    results_list$rsp_use_unif_rand_alpha__                                          = Xu_bdpg_extended_params@use_unif_rand_alpha__

    results_list$rsp_n__num_groups                                                  = Xu_bdpg_extended_params@n__num_groups

    results_list$rsp_n__num_groups_lower_bound                                      = Xu_bdpg_extended_params@n__num_groups_lower_bound
    results_list$rsp_n__num_groups_upper_bound                                      = Xu_bdpg_extended_params@n__num_groups_upper_bound
    results_list$rsp_use_unif_rand_n__num_groups                                    = Xu_bdpg_extended_params@use_unif_rand_n__num_groups

    results_list$rsp_num_independent_nodes_per_group                                = Xu_bdpg_extended_params@num_independent_nodes_per_group

    results_list$rsp_use_unif_rand_p__prop_of_links_between_groups                  = Xu_bdpg_extended_params@use_unif_rand_p__prop_of_links_between_groups
    results_list$rsp_p__prop_of_links_between_groups_lower_bound                    = Xu_bdpg_extended_params@p__prop_of_links_between_groups_lower_bound
    results_list$rsp_p__prop_of_links_between_groups_upper_bound                    = Xu_bdpg_extended_params@p__prop_of_links_between_groups_upper_bound
    results_list$rsp_base_for_target_num_links_between_2_groups_per_round           = Xu_bdpg_extended_params@base_for_target_num_links_between_2_groups_per_round
    results_list$rsp_at_least_1_for_target_num_links_between_2_groups_per_round     = Xu_bdpg_extended_params@at_least_1_for_target_num_links_between_2_groups_per_round  #  Not used?  See comment in gscp_5...R.

    results_list$rsp_use_unif_rand_r__density                                       = Xu_bdpg_extended_params@use_unif_rand_r__density
    results_list$rsp_r__density_lower_bound                                         = Xu_bdpg_extended_params@r__density_lower_bound
    results_list$rsp_r__density_upper_bound                                         = Xu_bdpg_extended_params@r__density_upper_bound

    #-----------------------------

        #  Xu_derived_params
    results_list$rsp_num_nodes_per_group                                            = Xu_derived_params@num_nodes_per_group
    results_list$rsp_num_rounds_of_linking_between_groups                           = Xu_derived_params@num_rounds_of_linking_between_groups
    results_list$rsp_target_num_links_between_2_groups_per_round                    = Xu_derived_params@target_num_links_between_2_groups_per_round
    results_list$rsp_num_links_within_one_group                                     = Xu_derived_params@num_links_within_one_group
    results_list$rsp_tot_num_links_inside_groups                                    = Xu_derived_params@tot_num_links_inside_groups
    results_list$rsp_max_possible_num_links_between_groups                          = Xu_derived_params@max_possible_num_links_between_groups
    results_list$rsp_max_possible_tot_num_links                                     = Xu_derived_params@max_possible_tot_num_links
    results_list$rsp_max_possible_tot_num_node_link_pairs                           = Xu_derived_params@max_possible_tot_num_node_link_pairs

    #  Isn't this a DUPLICATE from Xu_bdpg_extended_params?
    #results_list$rsp_num_independent_nodes_per_group                                = Xu_derived_params@num_independent_nodes_per_group

    #-----------------------------

    return (results_list)
    }

#===============================================================================

#' Save problem description results for a Xu_bd_problem
#'
#' Create a list of all problem description values for a Xu_bd_problem
#' and write that list to a file as a 1 line data frame with column
#' headers.
#'
#-------------------------------------------------------------------------------

#' @param run_ID integer ID for the run
#' @inheritParams std_param_defns

#-------------------------------------------------------------------------------

save_rsprob_results_data <- function (rsprob,
                                      exp_root_dir,
                                      run_ID    #parameters    #  only for tzar_run_ID?  replace w/ tzar_run_ID arg?
                                      )
    {
    # approx_number_of_list_entries = 60
    # results_list = vector ("list", approx_number_of_list_entries)

    results_list = list()

    #---------------------------------------------------------------------------

    results_list =
        load_results_output_values_common_to_all_Xu_problems (results_list,
                                                              rsprob,
                                                              run_ID    #parameters
                                                              )

    #---------------------------------------------------------------------------

    if (rsprob@prob_gen_info@read_Xu_problem_from_Xu_file)
        {
                  #  A Xu benchmark problem read from a file.
        results_list =
            load_results_output_values_specific_to_Xu_benchmark_problems (results_list, rsprob)

        } else    #  Not a Xu benchmark problem,
                  #  so many more values are known and can be written out.
        {
        results_list =
            load_results_output_values_specific_to_non_benchmark_Xu_problem (results_list, rsprob)
        }

    #---------------------------------------------------------------------------

    write_results_to_files ("prob_characteristics.csv",

            #as.data.frame (results_list),
            list_as_data_frame_with_0_length_vals_replaced_by_NA (results_list),

                            run_ID,    #parameters,
                            get_RSprob_path_topdir (rsprob, exp_root_dir),
                            "rsp_tzar_run_ID"
                            # , cur_result_row    #  Added 2016 03 28 - BTL.
                            )

    }  #  end function - save_rsprob_results_data

#===============================================================================

