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
#' @param compound_err_name string containing name of compound error condition
#' @inheritParams std_param_defns
#'
#' @return an apparent Xu_bd_problem
#-------------------------------------------------------------------------------

create_and_init_APP_bdprob <- function (Xu_bdprob_COR,
                                        parameters,
                                        compound_err_name)
    {
    new_seed_list =
        set_new_or_forced_rand_seed_if_necessary (is_rsrun = FALSE,
                                                  is_rsprob = TRUE,
                                                  parameters,
                                                  cor_or_app_str = "APP",
                                                  basic_or_wrapped_or_comb_str = Xu_bdprob_COR@basic_or_wrapped_or_comb_str,
                                                  location_string = paste0 ("Start of create_and_init_APP_bdprob(),APP,",
                                                                            Xu_bdprob_COR@basic_or_wrapped_or_comb_str))

cat ("\n@@@TRACKING rand_seed in create_and_init_APP_bdprob:: new_seed_list$seed_value = ", new_seed_list$seed_value, "\n")

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
cat ("\n\n>>>>> Creating APP_bdprob '",
     compound_err_name, "', uuid = ", Xu_bdprob_APP@UUID, "\n")

    Xu_bdprob_APP@prob_is_ok                       = FALSE
    Xu_bdprob_APP@rand_seed                        = new_seed_list$seed_value
cat ("\n@@@TRACKING rand_seed in create_and_init_APP_bdprob:: Xu_bdprob_APP@rand_seed = ", Xu_bdprob_APP@rand_seed, "\n")
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

##FixPUsppPairIndices-2018-02-17##    Xu_bdprob_APP@cor_PU_spp_pair_indices       = Xu_bdprob_COR@cor_PU_spp_pair_indices

    Xu_bdprob_APP@all_PU_IDs                = Xu_bdprob_COR@all_PU_IDs
    Xu_bdprob_APP@all_spp_IDs               = Xu_bdprob_COR@all_spp_IDs

    Xu_bdprob_APP@PU_col_name               = Xu_bdprob_COR@PU_col_name
    Xu_bdprob_APP@spp_col_name              = Xu_bdprob_COR@spp_col_name
    Xu_bdprob_APP@num_PUs                   = Xu_bdprob_COR@num_PUs
    Xu_bdprob_APP@num_spp                   = Xu_bdprob_COR@num_spp
    Xu_bdprob_APP@correct_solution_cost     = Xu_bdprob_COR@correct_solution_cost

    Xu_bdprob_APP@nodes                     = Xu_bdprob_COR@nodes

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

##FixPUsppPairIndices-2018-02-17##                  Xu_bdprob_APP@APP_prob_info@app_PU_spp_pair_indices,
                  Xu_bdprob_APP@PU_spp_pair_indices,

                  "APP",
                  Xu_bdprob_COR@all_PU_IDs,    #####!!!!!#####all_correct_node_IDs,

                  get_RSprob_path_plots (Xu_bdprob_APP, starting_dir),

                  Xu_bdprob_APP@spp_col_name,
                  Xu_bdprob_APP@PU_col_name,
                  Xu_bdprob_APP@presences_col_name
                  )

        #  Compute network metrics.

    if (class (Xu_bdprob_APP) == "Xu_wrapped_bd_problem")
        compute_network_metrics_APP = parameters$compute_network_metrics_wrapped_APP
    else
        compute_network_metrics_APP = parameters$compute_network_metrics_base_APP

    Xu_bdprob_APP =
        init_object_graph_data (
            Xu_bdprob_APP,
            starting_dir,
            parameters$compute_network_metrics,
            compute_network_metrics_APP,
            parameters$use_igraph_metrics,
            parameters$use_bipartite_metrics,
            parameters$bipartite_metrics_to_use)

    return (Xu_bdprob_APP)
    }

#===============================================================================

apply_unif_rand_error_to_PU_costs <- function (cor_PU_costs,
                                               cost_error_frac_bound    #parameters
                                               )
    {
    num_PUs = length (cor_PU_costs)
    if (is.null (cost_error_frac_bound))    #  Don't add any error to costs
        {
        cost_error_bound = 0
        cost_error_multipliers = rep (1, num_PUs)

        app_PU_costs = cor_PU_costs    #  No cost error specified so use COR costs

        realized_median_abs_cost_err_frac = 0
        realized_mean_abs_cost_err_frac   = 0
        realized_sd_abs_cost_err_frac     = 0

        } else    #  Add error to costs
        {
        cost_error_bound = vn (cost_error_frac_bound,
                               def_on_empty = FALSE,
                               range_lo = 0, range_hi = 1)

        PU_cost_lower_bound_frac = 1 - cost_error_bound
        PU_cost_upper_bound_frac = 1 + cost_error_bound

        cost_error_multipliers = runif (num_PUs,
                                        PU_cost_lower_bound_frac,
                                        PU_cost_upper_bound_frac)

        app_PU_costs = cor_PU_costs * cost_error_multipliers

            #--------------------------------------------------------
            #  Compute summary statistics for cost error fractions.
            #--------------------------------------------------------

        abs_cost_err_fracs       = abs (cost_error_multipliers - 1)
        realized_median_abs_cost_err_frac = median (abs_cost_err_fracs)
        realized_mean_abs_cost_err_frac   = mean (abs_cost_err_fracs)
        realized_sd_abs_cost_err_frac     = stats::sd (abs_cost_err_fracs)
        }

    return (list (app_PU_costs = app_PU_costs,
                  cost_error_bound = cost_error_bound,
                  cost_error_multipliers = cost_error_multipliers,

                  realized_median_abs_cost_err_frac = realized_median_abs_cost_err_frac,
                  realized_mean_abs_cost_err_frac = realized_mean_abs_cost_err_frac,
                  realized_sd_abs_cost_err_frac   = realized_sd_abs_cost_err_frac
                  ))
    }

#===============================================================================

compute_realized_euc_in_errors <- function (realized_FP_rate,
                                            realized_FN_rate,
                                            realized_Ftot_rate,
                                            realized_cost_err_frac)
    {
    euc_realized_FP_and_cost_in_err_frac   =
        sqrt (realized_FP_rate ^ 2 + realized_cost_err_frac ^ 2)

    euc_realized_FN_and_cost_in_err_frac   =
        sqrt (realized_FN_rate ^ 2 + realized_cost_err_frac ^ 2)

    euc_realized_Ftot_and_cost_in_err_frac =
        sqrt (realized_Ftot_rate ^ 2 + realized_cost_err_frac ^ 2)

    return (list (euc_realized_FP_and_cost_in_err_frac   =
                      euc_realized_FP_and_cost_in_err_frac,

                  euc_realized_FN_and_cost_in_err_frac   =
                      euc_realized_FN_and_cost_in_err_frac,

                  euc_realized_Ftot_and_cost_in_err_frac =
                      euc_realized_Ftot_and_cost_in_err_frac))
    }

#===============================================================================

#' Add error to the species occupancy data and save to APP_prob_info structure.
#'
#-------------------------------------------------------------------------------
#' @inheritParams std_param_defns
#' @param Xu_bdprob_COR a Xu_bd_problem
#' @param Xu_bdprob_APP an apparent Xu_bd_problem
#' @param gen_cost_errors boolean flag indicating whether to add cost error;
#'     TRUE implies add cost error and FALSE implies don't add cost error
#' @param gen_FP_FN_errors boolean flag indicating whether to add false positives
#'     and/or false negatives;
#'     TRUE implies add FP/FN error and FALSE implies don't add FP/FN error
#' @param ret_vals_from_build_const_err list of return values from building
#'     constant error values if already done, NULL if not already done
#' @param ret_vals_from_apply_cost_errors list of return values from building
#'     cost error values if already done, NULL if not already done
#'
#' @return an apparent Xu_bd_problem
#-------------------------------------------------------------------------------

create_APP_prob_info_by_adding_error_to_spp_occ_data <- function (Xu_bdprob_COR,
                                                                  Xu_bdprob_APP,
                                                                  parameters,
                                   gen_cost_errors,
                                   gen_FP_FN_errors,
                                                                  ret_vals_from_build_const_err = NULL,
                                                                  ret_vals_from_apply_cost_errors = NULL
                                                                  )
    {
    APP_prob_info = new ("APP_prob_info_class")

    APP_prob_info@UUID_of_base_problem_that_has_err_added = Xu_bdprob_COR@UUID

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

        #--------------------------------------------------------
        #  Compute PU cost errors and create apparent PU_costs.
        #--------------------------------------------------------

    if (gen_cost_errors)
        {
        if (is.null (ret_vals_from_apply_cost_errors))
            {
                #------------------------------------------------
                #  Cost errors have not already been generated,
                #  so generate them now.
                #------------------------------------------------

            ret_vals_from_apply_cost_errors =
                apply_unif_rand_error_to_PU_costs (Xu_bdprob_COR@PU_costs,
                                                   parameters$cost_error_frac_bound)
            }

            #-------------------------------------------------
            #  Save cost error values, regardless of whether
            #  they were passed in or generated here.
            #-------------------------------------------------

        Xu_bdprob_APP@PU_costs = ret_vals_from_apply_cost_errors$app_PU_costs

        APP_prob_info@cost_error_bound =
            ret_vals_from_apply_cost_errors$cost_error_bound
        APP_prob_info@realized_median_abs_cost_err_frac =
            ret_vals_from_apply_cost_errors$realized_median_abs_cost_err_frac
        APP_prob_info@realized_mean_abs_cost_err_frac =
            ret_vals_from_apply_cost_errors$realized_mean_abs_cost_err_frac
        APP_prob_info@realized_sd_abs_cost_err_frac =
            ret_vals_from_apply_cost_errors$realized_sd_abs_cost_err_frac

        } else
        {
            #-----------------------------------------------------
            #  Not generating FP/FN errors so set them all to 0.
            #  Use correct costs for apparent costs.
            #-----------------------------------------------------

        Xu_bdprob_APP@PU_costs = Xu_bdprob_COR@PU_costs

        APP_prob_info@cost_error_bound                  = 0.0
        APP_prob_info@realized_median_abs_cost_err_frac = 0.0
        APP_prob_info@realized_mean_abs_cost_err_frac   = 0.0
        APP_prob_info@realized_sd_abs_cost_err_frac     = 0.0
        }

        #-----------------------------------------------------
        #  Compute FP and FN errors and create apparent bpm.
        #-----------------------------------------------------

    if (gen_FP_FN_errors)
        {
        if (is.null (ret_vals_from_build_const_err))
            {
                #-------------------------------------------------
                #  FN/FP errors have not already been generated,
                #  so generate them now.
                #-------------------------------------------------

            ret_vals_from_build_const_err =
                build_const_err_FP_and_FN_matrices (sum (Xu_bdprob_COR@bpm),       #  num_TPs
                                                    length (Xu_bdprob_COR@bpm),    # num_TPs_and_TNs

                                                    Xu_bdprob_COR@num_PUs,
                                                    Xu_bdprob_COR@num_spp,

                                                    parameters$spp_occ_FP_error_type,
                                                    parameters$spp_occ_FP_const_rate,
                                                    parameters$spp_occ_FP_rate_lower_bound,
                                                    parameters$spp_occ_FP_rate_upper_bound,

                                                    parameters$spp_occ_FN_error_type,
                                                    parameters$spp_occ_FN_const_rate,
                                                    parameters$spp_occ_FN_rate_lower_bound,
                                                    parameters$spp_occ_FN_rate_upper_bound,

                                                    parameters$match_error_counts)
            }

            #--------------------------------------------------
            #  Save FP/FN error values, regardless of whether
            #  they were passed in or generated here.
            #--------------------------------------------------

        APP_prob_info@original_FP_const_rate = ret_vals_from_build_const_err$original_FP_const_rate
        APP_prob_info@original_FN_const_rate = ret_vals_from_build_const_err$original_FN_const_rate
        APP_prob_info@match_error_counts     = ret_vals_from_build_const_err$match_error_counts
        APP_prob_info@FP_const_rate          = ret_vals_from_build_const_err$FP_const_rate
        APP_prob_info@FN_const_rate          = ret_vals_from_build_const_err$FN_const_rate

            #----------------------------------------
            #  Ready to apply the FP/FN errors now.
            #----------------------------------------

        ret_vals_from_apply_errors =
            apply_const_error_to_spp_occupancy_data (Xu_bdprob_COR@num_PUs,
                                                     Xu_bdprob_COR@num_spp,
                                                     Xu_bdprob_COR@bpm,
                                                     ret_vals_from_build_const_err$FP_rates_matrix,
                                                     ret_vals_from_build_const_err$FN_rates_matrix)

            #----------------------------------------
            #  Save the realized FP/FN error rates.
            #----------------------------------------

        APP_prob_info@realized_FP_rate          = ret_vals_from_apply_errors$realized_FP_rate
        APP_prob_info@realized_FN_rate          = ret_vals_from_apply_errors$realized_FN_rate
        APP_prob_info@realized_Ftot_rate        = ret_vals_from_apply_errors$realized_Ftot_rate

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
    ##FixPUsppPairIndices-2018-02-17##    APP_prob_info@app_PU_spp_pair_indices      = ret_vals_from_apply_errors$app_PU_spp_pair_indices
        Xu_bdprob_APP@PU_spp_pair_indices      = ret_vals_from_apply_errors$app_PU_spp_pair_indices

            #NEEDS TO HAVE SAME DIMENSIONS AND ROW/COLUMN NAMES AS COR.
        Xu_bdprob_APP@bpm                      = ret_vals_from_apply_errors$app_spp_occupancy_data

                  #-----------------------------------------------------
        } else    #  Not generating FP/FN errors so set them all to 0.
        {         #-----------------------------------------------------


        APP_prob_info@original_FP_const_rate = 0.0
        APP_prob_info@original_FN_const_rate = 0.0
        APP_prob_info@match_error_counts     = FALSE
        APP_prob_info@FP_const_rate          = 0.0
        APP_prob_info@FN_const_rate          = 0.0

        APP_prob_info@realized_FP_rate       = 0.0
        APP_prob_info@realized_FN_rate       = 0.0
        APP_prob_info@realized_Ftot_rate     = 0.0

        APP_prob_info@app_num_spp            = Xu_bdprob_COR@num_spp
        APP_prob_info@app_num_PUs            = Xu_bdprob_COR@num_PUs
        Xu_bdprob_APP@PU_spp_pair_indices    = Xu_bdprob_COR@PU_spp_pair_indices
        Xu_bdprob_APP@bpm                    = Xu_bdprob_COR@bpm
        }

        #----------------------------------------------------------
        #  Compute and save euclidean error measures that combine
        #  FP/FN error with cost error into a single 1D error
        #  measure.
        #----------------------------------------------------------

    realized_euc_in_errors =
        compute_realized_euc_in_errors (
            APP_prob_info@realized_FP_rate,                     #ret_vals_from_apply_errors$realized_FP_rate,
            APP_prob_info@realized_FN_rate,                     #ret_vals_from_apply_errors$realized_FN_rate,
            APP_prob_info@realized_Ftot_rate,                   #ret_vals_from_apply_errors$realized_Ftot_rate,
            APP_prob_info@realized_median_abs_cost_err_frac)    #ret_vals_from_apply_cost_errors$realized_median_abs_cost_err_frac)

    APP_prob_info@euc_realized_FP_and_cost_in_err_frac =
        realized_euc_in_errors$euc_realized_FP_and_cost_in_err_frac

    APP_prob_info@euc_realized_FN_and_cost_in_err_frac =
        realized_euc_in_errors$euc_realized_FN_and_cost_in_err_frac

    APP_prob_info@euc_realized_Ftot_and_cost_in_err_frac =
        realized_euc_in_errors$euc_realized_Ftot_and_cost_in_err_frac

        #----------------------------------------------------------

    Xu_bdprob_APP@APP_prob_info = APP_prob_info

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

#' @inheritParams std_param_defns
#' @param Xu_bdprob_COR correct Xu problem that is to have error added to it
#' @param gen_cost_errors boolean flag indicating whether to add cost error;
#'     TRUE implies add cost error and FALSE implies don't add cost error
#' @param gen_FP_FN_errors boolean flag indicating whether to add false positives
#'     and/or false negatives;
#'     TRUE implies add FP/FN error and FALSE implies don't add FP/FN error
#' @param ret_vals_from_build_const_err list of return values from building
#'     constant error values if already done, NULL if not already done
#' @param ret_vals_from_apply_cost_errors list of return values from building
#'     cost error values if already done, NULL if not already done
#' @param compound_err_name string containing name of compound error condition
#'
#' @return Returns apparent version of either a Xu_bd_problem or a
#'     Xu_wrapped_bd_problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_APP <- function (Xu_bdprob_COR,
                                   parameters,
                           gen_cost_errors,
                           gen_FP_FN_errors,
                                   compound_err_name = NULL,
                                   ret_vals_from_build_const_err = NULL,
                                   ret_vals_from_apply_cost_errors = NULL)
    {
    Xu_bdprob_APP =
        create_and_init_APP_bdprob (Xu_bdprob_COR,
                                    parameters,
                                    compound_err_name)

    Xu_bdprob_APP =
        create_APP_prob_info_by_adding_error_to_spp_occ_data (Xu_bdprob_COR,
                                                              Xu_bdprob_APP,
                                                              parameters,
                                           gen_cost_errors,
                                           gen_FP_FN_errors,
                                                              ret_vals_from_build_const_err,
                                                              ret_vals_from_apply_cost_errors)

    starting_dir = parameters$fullOutputDir_NO_slash

    Xu_bdprob_APP =
        create_dirs_for_APP_prob (Xu_bdprob_APP,
                                  Xu_bdprob_COR@basic_or_wrapped_or_comb_str,
                                  starting_dir)

    Xu_bdprob_APP =
        compute_and_save_dist_and_network_metrics_for_prob (Xu_bdprob_APP,
                                                            Xu_bdprob_COR,
                                                            starting_dir,
                                                            parameters)

    Xu_bdprob_APP@combined_err_label = compound_err_name

        #------------------------------------------------------------
        #  Everything seems to have worked.
        #  Save the bdprob to disk as a first cut at how to archive
        #  and retrieve problems in general.
        #  This particular bit of code may disappear later on, once
        #  it's clearer how to archive.
        #------------------------------------------------------------

    Xu_bdprob_APP@prob_is_ok = TRUE

    Xu_bdprob_APP <- save_rsprob (Xu_bdprob_APP, starting_dir)

    save_rsprob_results_data (Xu_bdprob_APP, starting_dir, parameters$run_id)

    return (Xu_bdprob_APP)
    }

#===============================================================================

