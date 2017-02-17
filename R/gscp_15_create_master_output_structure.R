#===============================================================================

                #  gscp_15_create_master_output_structure.R

#===============================================================================

#' Create a master output structure for all problem attributes and reserve
#' selection results when evaluating a CORRECT problem.
#'
#'When evaluating performance on a CORRECT problem, pass that CORRECT problem
#'in as both the CORRECT and the APPARENT problem.  The code to create the
#'master output structure puts out various APPARENT values and this dummying
#'in of the CORRECT for the APPARENT allows the function to work identically
#'for CORRECT and APPARENT data.
#'
#' @param COR_bd_prob
#' @param marxan_control_values
#' @param marxan_output_values
#' @param parameters

create_COR_master_output_structure <- function (marxan_control_values,
                                                marxan_output_values,
                                                parameters,

                                                COR_bd_prob,
                                                COR_marxan_run
                                                )
    {
    create_master_output_structure (parameters            = parameters,
                                    marxan_output_values  = marxan_output_values,
                                    marxan_control_values = marxan_control_values,

                                    COR_bd_prob    = COR_bd_prob,
                                    APP_bd_prob    = COR_bd_prob,     #  <==
                                    APP_marxan_run = COR_marxan_run,  #  <==
                                    apply_error    = FALSE            #  <==
                                    )
    }

#===============================================================================

#' Create a master output structure for all problem attributes and reserve
#' selection results when evaluating an APPARENT problem.
#'
#' @param APP_bd_prob
#' @param COR_bd_prob
#' @param marxan_control_values
#' @param marxan_output_values
#' @param parameters

create_APP_master_output_structure <- function (marxan_control_values,
                                                marxan_output_values,
                                                parameters,

                                                COR_bd_prob,
                                                APP_bd_prob,
                                                APP_marxan_run
                                                )
    {
    create_master_output_structure (parameters            = parameters,
                                    marxan_output_values  = marxan_output_values,
                                    marxan_control_values = marxan_control_values,

                                    COR_bd_prob    = COR_bd_prob,
                                    APP_bd_prob    = APP_bd_prob,     #  <==
                                    APP_marxan_run = APP_marxan_run,  #  <==
                                    apply_error    = TRUE             #  <==
                                    )
    }

#===============================================================================

    #  Build a master table containing:
    {
        #  planning unit ID
                #  marxan_best_df_sorted$PUID
        #  correct (optimal) answer (as boolean flags on sorted planning units)
        #  best marxan guess
                #  marxan_best_df_sorted$SOLUTION
        #  marxan number of votes for each puid
                #  marxan_ssoln_df$number
        #  difference between correct and best (i.e., (cor - best), FP, FN, etc)
        #  absolute value of difference (to make counting them easier)
        #  number of species on each patch (i.e., simple richness)

    #  Need to bind together:
    #   problem setup
    #       - planning unit IDs (goes with all of these, even if they're split
    #         into separate tables)
    #       - number of species (simple richness) on patch as counts
    #   correct/optimal solution
    #       - correct/optimal solution as 0/1
    #   marxan solution(s)
    #       - marxan best solution as 0/1
    #       - marxan solution votes as counts
    #   performance measure(s)
    #       - difference between marxan best and optimal solution to represent
    #         error direction (e.g., FP, FN, etc.)
    #       - abs (difference) to represent error or no error

        #  *** Need to be sure that the puid column matches in the nodes data frame
        #  and the marxan data frames.  Otherwise, there could be a mismatch in
        #  the assignments for inclusion or exclusion of patches in the solutions.

        #  Create table holding all the information to compare solutions.
    }

#===============================================================================

#' Create a master output structure for all problem attributes and reserve
#' selection results.
#'
#'When evaluating performance on a CORRECT problem, pass that CORRECT problem
#'in as both the CORRECT and the APPARENT problem.  The code to create the
#'master output structure puts out various APPARENT values and this dummying
#'in of the CORRECT for the APPARENT allows the function to work identically
#'for CORRECT and APPARENT data.
#'
#' @param parameters
#' @param marxan_output_values
#' @param marxan_control_values
#' @param COR_bd_prob
#' @param APP_bd_prob
#' @param APP_marxan_run
#' @param apply_error

create_master_output_structure <- function (parameters,
                                            marxan_output_values,
                                            marxan_control_values,

                                            COR_bd_prob,
                                            APP_bd_prob,     #  <==  different for COR/APP
                                            APP_marxan_run,  #  <==  different for COR/APP
                                            apply_error      #  <==  different for COR/APP
                                            )
    {

#===============================================================================

#  2017 02 17 - BTL
#  MEANINGLESS, BUT NECESSARY, STARTUP CODE
#
#  NOTE THAT MANY OF THE ASSIGNMENTS DONE IN THIS SECTION DON'T REALLY NEED
#  TO BE DONE.  THEY'RE MAKING A LOCAL VARIABLE TO HOLD A VALUE THAT'S A SLOT
#  IN AN OBJECT OR LIST THAT WAS PASSED IN AND OFTEN, THEY'RE ONLY REFERENCED
#  ONE TIME, I.E., WHEN THEIR VALUE IS WRITTEN INTO THE RESULTS DATA FRAME.
#  SO, MUCH OF THIS COULD BE REPLACED BY JUST DIRECTLY REFERENCING THE SLOT.
#
#  THE OTHER THING IS THAT NEARLY ALL OF THESE ASSIGNMENTS WILL GO AWAY IF I
#  SWITCH TO THE CBIND STRATEGY FOR BUILDING A SPECIFIC RESULTS DATA FRAME
#  FOR EACH QUESTION OF INTEREST.

{
#------------------------------------------------------------------------
#  2017 02 17 - BTL
#  Moving these initializations up here to remind me to look at whether
#  these variables are just vestigial and should be removed altogether
#  from here and from the results df.
#  Not sure they will ever have any value other than 1 the way things
#  work now.
#------------------------------------------------------------------------
    {
    num_runs = 1

    cur_result_row = 0
    cur_result_row = cur_result_row + 1
    }

#===============================================================================

        #-------------------------------------------------------------
        #  Decode values that used to be explicitly in the arg list.
        #-------------------------------------------------------------
    {
            #---------------------------
            #  CORRECT values first...
            #---------------------------

    read_Xu_problem_from_Xu_file        = COR_bd_prob@prob_gen_info@read_Xu_problem_from_Xu_file

    Xu_parameters                       = COR_bd_prob@prob_gen_info@Xu_parameters
    derived_Xu_params                   = Xu_parameters@derived_params
    base_Xu_params                      = Xu_parameters@base_params

    num_PUs                             = COR_bd_prob@num_PUs
    num_spp                             = COR_bd_prob@num_spp
    correct_solution_cost               = COR_bd_prob@correct_solution_cost
    nodes                               = COR_bd_prob@nodes
    cor_link_counts_for_each_node       = COR_bd_prob@final_link_counts_for_each_node  #  See 3:04 pm note below
    cor_bpm                             = COR_bd_prob@bpm
    correct_solution_vector_is_known    = COR_bd_prob@correct_solution_vector_is_known

            #----------------------------------------------------------
            #  Now APPARENT values...
            #  However, when just evaluating performance on a correct
            #  problem, the CORRECT problem will be passed in as both
            #  CORRECT and APPARENT, so the APPARENT values below will
            #  be identical to their CORRECT counterparts in the case
            #  of evaluating performance on a CORRECT problem.
            #----------------------------------------------------------

    app_bpm                                  = APP_bd_prob@bpm
    compute_network_metrics                  = APP_bd_prob@compute_network_metrics
    bipartite_metrics_from_bipartite_package = APP_bd_prob@bipartite_metrics_from_bipartite_package
    bipartite_metrics_from_igraph_package_df = APP_bd_prob@bipartite_metrics_from_igraph_package_df

            #---------------------------------------
            #  Input parameters for error model...
            #---------------------------------------

    if (apply_error)
        {       #  APPARENT => there is an error model
        match_error_counts      = APP_bd_prob@APP_prob_info@match_error_counts    # FALSE
        FP_const_rate           = APP_bd_prob@APP_prob_info@FP_const_rate    # 0
        FN_const_rate           = APP_bd_prob@APP_prob_info@FN_const_rate    # 0
        original_FP_const_rate  = APP_bd_prob@APP_prob_info@original_FP_const_rate    # 0
        original_FN_const_rate  = APP_bd_prob@APP_prob_info@original_FN_const_rate    # 0

        } else
        {       #  CORRECT => no error model
        match_error_counts      = FALSE
        FP_const_rate           = 0
        FN_const_rate           = 0
        original_FP_const_rate  = 0
        original_FN_const_rate  = 0
        }

            #------------------------------------
            #  Now marxan_output_values values...
            #------------------------------------

    marxan_best_df_sorted = marxan_output_values$marxan_best_df_sorted
    marxan_ssoln_df       = marxan_output_values$marxan_ssoln_df
    marxan_mvbest_df      = marxan_output_values$marxan_mvbest_df

            #--------------------------------------------
            #  Finally, marxan_control_values values...
            #--------------------------------------------

    spf_const         = marxan_control_values$spf_const
    marxan_PROP       = marxan_control_values$marxan_PROP
    marxan_RANDSEED   = marxan_control_values$marxan_RANDSEED
    marxan_NUMREPS    = marxan_control_values$marxan_NUMREPS
    marxan_NUMITNS    = marxan_control_values$marxan_NUMITNS
    marxan_STARTTEMP  = marxan_control_values$marxan_STARTTEMP
    marxan_NUMTEMP    = marxan_control_values$marxan_NUMTEMP
    marxan_COSTTHRESH = marxan_control_values$marxan_COSTTHRESH
    marxan_THRESHPEN1 = marxan_control_values$marxan_THRESHPEN1
    marxan_THRESHPEN2 = marxan_control_values$marxan_THRESHPEN2
    marxan_RUNMODE    = marxan_control_values$marxan_RUNMODE
    marxan_MISSLEVEL  = marxan_control_values$marxan_MISSLEVEL
    marxan_ITIMPTYPE  = marxan_control_values$marxan_ITIMPTYPE
    marxan_HEURTYPE   = marxan_control_values$marxan_HEURTYPE
    marxan_CLUMPTYPE  = marxan_control_values$marxan_CLUMPTYPE
    }
}

#===============================================================================

#  2017 02 17 - BTL
#  ALL 3 OF THESE VALUES COULD BE COMPUTED LONG BEFORE COMING TO THIS ROUTINE,
#  THOUGH I DON'T KNOW IF THEY'RE ALL THAT NECESSARY.  IN ANY CASE, THEY
#  COULD EVEN BE ADDED TO THE PROBLEM OBJECT AS SOON AS THE PROBLEM IS BUILT.
#  THEY _ARE_ USED FURTHER DOWN IN HERE TO DO THINGS LIKE COMPUTE SOLUTION
#  SCORES, HOWEVER, EVEN THERE I WONDER IF THEY SHOULD BE REPLACED BY COSTS
#  INSTEAD OF PATCH COUNTS SINCE PATCH COUNTS ARE REALLY JUST SURROGATES FOR
#  COSTS WHERE EVERY PU COST = 1.  CHANGING ALL THESE THINGS TO COSTS MIGHT
#  BE NECESSARY FOR MAKING THIS CODE GENERALIZE TO NON-XU PROBLEMS.

    if (correct_solution_vector_is_known)
        {

        correct_solution_vector = nodes$dependent_set_member
        opt_solution_as_frac_of_tot_num_nodes = derived_Xu_params@opt_solution_as_frac_of_tot_num_nodes
        cor_num_patches_in_solution = sum (correct_solution_vector)
        }

#===============================================================================

#  2017 02 17 - BTL
#  THIS SECTION COULD BE COMPUTED AT END OF MARXAN RUN AND SAVED IN FILES THERE,
#  THEN READ BACK IN HERE IF USING THE CBIND STRATEGY FOR ASSEMBLING OUTPUT DF.

{
    #---------------------------------------------------------------------------
    #               Summarize marxan solution features.
    #---------------------------------------------------------------------------
    {
        #  Find which PUs marxan chose for its best solution.
    marxan_best_solution_PU_IDs = which (marxan_best_df_sorted$SOLUTION > 0)  #  solution has a 1 for each PU in the solution and 0 otherwise
    marxan_best_num_patches_in_solution = length (marxan_best_solution_PU_IDs)

        #  Compute error in cost of best marxan solution.
        #  Assumes equal cost for all patches, i.e., cost per patch = 1.
            #  BTL - 2017 02 17
            #  It's simple to fix this, i.e., these num_patches bits should
            #  be changed to costs instead of patch counts.
            #  Since patch costs are the same as patch counts in the Xu
            #  problems, it will work for now and for the future.

    marxan_best_solution_cost_err_frac = (marxan_best_num_patches_in_solution - cor_num_patches_in_solution) / cor_num_patches_in_solution
    abs_marxan_best_solution_cost_err_frac = abs (marxan_best_solution_cost_err_frac)

# #  2017 02 17 - BTL
# #  HOW I MIGHT WANT TO REWRITE THE STUFF ABOVE.
# #  NEED TO LOOK UP WHERE I'M STORING THE PU COSTS NOW SO THAT THE WAY IT'S
# #  STATED IN THE EXPRESSIONS BELOW IS CORRECT.
# #
# #  There are really 4 errors here:
# #  - an apparent cost error
# #  - a correct cost error and
# #  - an absolute error in the estimation of the cost (i.e., abs(app_cost - cor_cost)) and
# #  - a fractional error in the estimation of the cost (i.e., abs_cost_err/cor_cost)
#
#     marxan_best_sol_cost = sum (cost [marxan_best_df_sorted$SOLUTION > 0])
#     marxan_best_solution_cost_err_frac =
#         (marxan_best_sol_cost - cor_cost) / cor_cost
#     abs_marxan_best_solution_cost_err_frac =
#         abs (marxan_best_solution_cost_err_frac)

    }

    #===========================================================================
    #       Compute correct and apparent scores for marxan solution.
    #===========================================================================

{
    #---------------------------------------------------------------------------
    #               Apparent scores as computed by marxan
    #---------------------------------------------------------------------------
    {
    app_solution_NUM_spp_covered__fromMarxan  = sum (marxan_mvbest_df$MPM)
    app_solution_FRAC_spp_covered__fromMarxan = app_solution_NUM_spp_covered__fromMarxan / num_spp
    app_spp_rep_shortfall__fromMarxan         = 1 - app_solution_FRAC_spp_covered__fromMarxan
    }

    #---------------------------------------------------------------------------
    #               Apparent scores as computed by bdpg...
    #---------------------------------------------------------------------------
{
    app_results_list =
            compute_solution_vector_scores (app_bpm,
                                            num_PUs,
                                            marxan_best_solution_PU_IDs,
                                            marxan_best_num_patches_in_solution,
                                            cor_num_patches_in_solution,
                                            APP_marxan_run@targets,
                                            num_spp,
                                            FP_const_rate,
                                            FN_const_rate)
}
    #---------------------------------------------------------------------------
    #               Correct scores as computed by bdpg...
    #---------------------------------------------------------------------------
{
    cor_results_list =
            compute_solution_vector_scores (cor_bpm,
                                            num_PUs,
                                            marxan_best_solution_PU_IDs,
                                            marxan_best_num_patches_in_solution,
                                            cor_num_patches_in_solution,
                                            APP_marxan_run@targets,
                                            num_spp,
                                            FP_const_rate,
                                            FN_const_rate)
}
}
}

#===============================================================================

#  2017 02 17 - BTL
#  FROM HERE ON IS JUST BUSYWORK CODE FOR ASSEMBLING ALL THE PREVIOUS
#  RESULTS INTO A MASSIVE, POPULATED DATA FRAME.
#  NO COMPUTATIONS ARE DONE FROM HERE ON IN THIS FUNCTION.
{
    #-------------------------------------------------------------------------
    #  Create full results_df and initialize before populating it afterwards.
    #-------------------------------------------------------------------------

    {
    results_df =
        data.frame (
                    runset_abbrev = rep (NA, num_runs),
                    run_ID = rep (NA, num_runs),

                    exceeded_thresh_for_num_spp = rep (NA, num_runs),

                    num_PUs = rep (NA, num_runs),
                    num_spp = rep (NA, num_runs),
                    num_spp_per_PU = rep (NA, num_runs),
                    seed = rep (NA, num_runs),

                      #  Xu options
                    n__num_groups = rep (NA, num_runs),
                    alpha__ = rep (NA, num_runs),
                    p__prop_of_links_between_groups = rep (NA, num_runs),
                    r__density = rep (NA, num_runs),

                      #  Correct results as computed by bdpg
                    opt_solution_as_frac_of_tot_num_nodes = rep (NA, num_runs),
                    cor_num_patches_in_solution = rep (NA, num_runs),
                    marxan_best_num_patches_in_solution = rep (NA, num_runs),
                    abs_marxan_best_solution_cost_err_frac = rep (NA, num_runs),
                    marxan_best_solution_cost_err_frac = rep (NA, num_runs),

                    cor_spp_rep_shortfall = rep (NA, num_runs),
                    cor_NUM_spp_covered = rep (NA, num_runs),
                    cor_FRAC_spp_covered = rep (NA, num_runs),

                    cor_TP = rep (NA, num_runs),
                    cor_TN = rep (NA, num_runs),
                    cor_FP = rep (NA, num_runs),
                    cor_FN = rep (NA, num_runs),

                    cor_cSe = rep (NA, num_runs),
                    cor_cSp = rep (NA, num_runs),
                    cor_cPPV = rep (NA, num_runs),
                    cor_cNPV = rep (NA, num_runs),

                    cor_acc_frac = rep (NA, num_runs),
                    cor_acc_err_frac = rep (NA, num_runs),
                    cor_cost_savings = rep (NA, num_runs),

                    cor_opt_cost_savings = rep (NA, num_runs),

                    cor_TSS = rep (NA, num_runs),
                    cor_max_cSe_cSp = rep (NA, num_runs),
                    cor_min_cSe_cSp = rep (NA, num_runs),
                    cor_mean_cSe_cSp = rep (NA, num_runs),
                    cor_prod_cSe_cSp = rep (NA, num_runs),
                    cor_euc_cSe_cSp = rep (NA, num_runs),
                    cor_acc_err_mag = rep (NA, num_runs),

                      #  Apparent results as computed by bdpg
                    app_spp_rep_shortfall = rep (NA, num_runs),
                    app_solution_NUM_spp_covered = rep (NA, num_runs),
                    app_solution_FRAC_spp_covered = rep (NA, num_runs),

                    app_TP = rep (NA, num_runs),
                    app_TN = rep (NA, num_runs),
                    app_FP = rep (NA, num_runs),
                    app_FN = rep (NA, num_runs),

                    app_cSe = rep (NA, num_runs),
                    app_cSp = rep (NA, num_runs),
                    app_cPPV = rep (NA, num_runs),
                    app_cNPV = rep (NA, num_runs),

                    app_acc_frac = rep (NA, num_runs),
                    app_acc_err_frac = rep (NA, num_runs),
                    app_cost_savings = rep (NA, num_runs),

                    app_opt_cost_savings = rep (NA, num_runs),

                    app_TSS = rep (NA, num_runs),
                    app_max_cSe_cSp = rep (NA, num_runs),
                    app_min_cSe_cSp = rep (NA, num_runs),
                    app_mean_cSe_cSp = rep (NA, num_runs),
                    app_prod_cSe_cSp = rep (NA, num_runs),
                    app_euc_cSe_cSp = rep (NA, num_runs),
                    app_acc_err_mag = rep (NA, num_runs),

                      #  Apparent results as computed by Marxan
                    app_spp_rep_shortfall__fromMarxan = rep (NA, num_runs),
                    app_solution_NUM_spp_covered__fromMarxan = rep (NA, num_runs),
                    app_solution_FRAC_spp_covered__fromMarxan = rep (NA, num_runs),

                      #  Error generation parameters
                    apply_error = rep (NA, num_runs),
                    FP_const_rate = rep (NA, num_runs),
                    FN_const_rate = rep (NA, num_runs),
                    match_error_counts = rep (NA, num_runs),
                    original_FP_const_rate = rep (NA, num_runs),
                    original_FN_const_rate = rep (NA, num_runs),

                      #  Derived options
                    num_nodes_per_group = rep (NA, num_runs),
                    tot_num_nodes = rep (NA, num_runs),
                    num_independent_set_nodes = rep (NA, num_runs),
                    num_dependent_set_nodes = rep (NA, num_runs),
                    num_rounds_of_linking_between_groups = rep (NA, num_runs),
                    target_num_links_between_2_groups_per_round = rep (NA, num_runs),
                    num_links_within_one_group = rep (NA, num_runs),
                    tot_num_links_inside_groups = rep (NA, num_runs),
                    max_possible_num_links_between_groups = rep (NA, num_runs),
                    max_possible_tot_num_links = rep (NA, num_runs),

                      #  Marxan options
                    marxan_spf_const = rep (NA, num_runs),
                    marxan_PROP = rep (NA, num_runs),
                    marxan_RANDSEED = rep (NA, num_runs),
                    marxan_NUMREPS = rep (NA, num_runs),

                      #  Marxan Annealing Parameters
                    marxan_NUMITNS = rep (NA, num_runs),
                    marxan_STARTTEMP = rep (NA, num_runs),
                    marxan_NUMTEMP = rep (NA, num_runs),

                      #  Marxan Cost Threshold
                    marxan_COSTTHRESH = rep (NA, num_runs),
                    marxan_THRESHPEN1 = rep (NA, num_runs),
                    marxan_THRESHPEN2 = rep (NA, num_runs),

                      #  Marxan Program control
                    marxan_RUNMODE = rep (NA, num_runs),
                    marxan_MISSLEVEL = rep (NA, num_runs),
                    marxan_ITIMPTYPE = rep (NA, num_runs),
                    marxan_HEURTYPE = rep (NA, num_runs),
                    marxan_CLUMPTYPE = rep (NA, num_runs),

                      #  Full runset name
                    runset_name = rep (NA, num_runs)
                    )
    }

#===============================================================================

        #----------------------------------------------
        #  Fill in the full final results data frame.
        #----------------------------------------------

    {
        #  Filling in the runset_abbrev with the full runset name for the moment,
        #  because it's more reliably correct since it's automatically captured
        #  by tzar.  Not sure what I'll do in the long run.
        #  2015 03 09 - BTL
    results_df$runset_abbrev [cur_result_row]                                    = parameters$runset_name    #  parameters$runset_abbrev

    results_df$exceeded_thresh_for_num_spp                                       = FALSE

    results_df$num_PUs [cur_result_row]                                          = num_PUs
    results_df$num_spp [cur_result_row]                                          = num_spp
    results_df$num_spp_per_PU [cur_result_row]                                   = num_spp / num_PUs
    results_df$seed [cur_result_row]                                             = parameters$seed

        #  Xu options
    results_df$n__num_groups [cur_result_row]                                    = base_Xu_params@n__num_groups
    results_df$alpha__ [cur_result_row]                                          = base_Xu_params@alpha__
    results_df$p__prop_of_links_between_groups [cur_result_row]                  = base_Xu_params@p__prop_of_links_between_groups
    results_df$r__density [cur_result_row]                                       = base_Xu_params@r__density

        #  Correct results as computed by bdpg
    results_df$opt_solution_as_frac_of_tot_num_nodes [cur_result_row]            = opt_solution_as_frac_of_tot_num_nodes
    results_df$cor_num_patches_in_solution [cur_result_row]                      = cor_num_patches_in_solution
    results_df$marxan_best_num_patches_in_solution [cur_result_row]              = marxan_best_num_patches_in_solution
    results_df$abs_marxan_best_solution_cost_err_frac [cur_result_row]           = abs_marxan_best_solution_cost_err_frac
    results_df$marxan_best_solution_cost_err_frac [cur_result_row]               = marxan_best_solution_cost_err_frac

    results_df$cor_spp_rep_shortfall [cur_result_row]                = cor_results_list$spp_rep_shortfall
    results_df$cor_NUM_spp_covered [cur_result_row]                  = cor_results_list$num_spp_covered
    results_df$cor_FRAC_spp_covered [cur_result_row]                 = cor_results_list$frac_spp_covered

    results_df$cor_TP [cur_result_row]                               = cor_results_list$TP
    results_df$cor_TN [cur_result_row]                               = cor_results_list$TN
    results_df$cor_FP [cur_result_row]                               = cor_results_list$FP
    results_df$cor_FN [cur_result_row]                               = cor_results_list$FN

    results_df$cor_cSe [cur_result_row]                              = cor_results_list$cSe
    results_df$cor_cSp [cur_result_row]                              = cor_results_list$cSp
    results_df$cor_cPPV [cur_result_row]                             = cor_results_list$cPPV
    results_df$cor_cNPV [cur_result_row]                             = cor_results_list$cNPV

    results_df$cor_acc_frac [cur_result_row]                         = cor_results_list$acc_frac
    results_df$cor_acc_err_frac [cur_result_row]                     = cor_results_list$acc_err_frac
    results_df$cor_cost_savings [cur_result_row]                     = cor_results_list$cost_savings

    results_df$cor_opt_cost_savings [cur_result_row]                 = cor_results_list$opt_cost_savings

    results_df$cor_TSS [cur_result_row]                              = cor_results_list$TSS
    results_df$cor_max_cSe_cSp [cur_result_row]                      = cor_results_list$max_cSe_cSp
    results_df$cor_min_cSe_cSp [cur_result_row]                      = cor_results_list$min_cSe_cSp
    results_df$cor_mean_cSe_cSp [cur_result_row]                     = cor_results_list$mean_cSe_cSp
    results_df$cor_prod_cSe_cSp [cur_result_row]                     = cor_results_list$prod_cSe_cSp
    results_df$cor_euc_cSe_cSp [cur_result_row]                      = cor_results_list$euc_cSe_cSp
    results_df$cor_acc_err_mag [cur_result_row]                      = cor_results_list$acc_err_mag

        #  Apparent results as computed by bdpg
    results_df$app_spp_rep_shortfall [cur_result_row]                = app_results_list$spp_rep_shortfall
    results_df$app_solution_NUM_spp_covered [cur_result_row]         = app_results_list$num_spp_covered
    results_df$app_solution_FRAC_spp_covered [cur_result_row]        = app_results_list$frac_spp_covered

    results_df$app_TP [cur_result_row]                               = app_results_list$TP
    results_df$app_TN [cur_result_row]                               = app_results_list$TN
    results_df$app_FP [cur_result_row]                               = app_results_list$FP
    results_df$app_FN [cur_result_row]                               = app_results_list$FN

    results_df$app_cSe [cur_result_row]                              = app_results_list$cSe
    results_df$app_cSp [cur_result_row]                              = app_results_list$cSp
    results_df$app_cPPV [cur_result_row]                             = app_results_list$cPPV
    results_df$app_cNPV [cur_result_row]                             = app_results_list$cNPV

    results_df$app_acc_frac [cur_result_row]                         = app_results_list$acc_frac
    results_df$app_acc_err_frac [cur_result_row]                     = app_results_list$acc_err_frac
    results_df$app_cost_savings [cur_result_row]                     = app_results_list$cost_savings

    results_df$app_opt_cost_savings [cur_result_row]                 = app_results_list$opt_cost_savings

    results_df$app_TSS [cur_result_row]                              = app_results_list$TSS
    results_df$app_max_cSe_cSp [cur_result_row]                      = app_results_list$max_cSe_cSp
    results_df$app_min_cSe_cSp [cur_result_row]                      = app_results_list$min_cSe_cSp
    results_df$app_mean_cSe_cSp [cur_result_row]                     = app_results_list$mean_cSe_cSp
    results_df$app_prod_cSe_cSp [cur_result_row]                     = app_results_list$prod_cSe_cSp
    results_df$app_euc_cSe_cSp [cur_result_row]                      = app_results_list$euc_cSe_cSp
    results_df$app_acc_err_mag [cur_result_row]                      = app_results_list$acc_err_mag

        #  Apparent results as computed by Marxan
    results_df$app_spp_rep_shortfall__fromMarxan [cur_result_row]                = app_spp_rep_shortfall__fromMarxan
    results_df$app_solution_NUM_spp_covered__fromMarxan [cur_result_row]         = app_solution_NUM_spp_covered__fromMarxan
    results_df$app_solution_FRAC_spp_covered__fromMarxan [cur_result_row]        = app_solution_FRAC_spp_covered__fromMarxan

        #  Error generation parameters
    results_df$apply_error [cur_result_row]                                       = apply_error
    results_df$FP_const_rate [cur_result_row]                                   = FP_const_rate
    results_df$FN_const_rate [cur_result_row]                                   = FN_const_rate
    results_df$match_error_counts [cur_result_row]                              = match_error_counts
    results_df$original_FP_const_rate [cur_result_row]                          = original_FP_const_rate
    results_df$original_FN_const_rate [cur_result_row]                          = original_FN_const_rate

        #  Derived Xu options
    results_df$num_nodes_per_group [cur_result_row]                             = derived_Xu_params@num_nodes_per_group
    results_df$tot_num_nodes [cur_result_row]                                   = derived_Xu_params@tot_num_nodes
    results_df$num_independent_set_nodes [cur_result_row]                       = derived_Xu_params@num_independent_set_nodes
    results_df$num_dependent_set_nodes [cur_result_row]                         = derived_Xu_params@num_dependent_set_nodes
    results_df$num_rounds_of_linking_between_groups [cur_result_row]            = derived_Xu_params@num_rounds_of_linking_between_groups
    results_df$target_num_links_between_2_groups_per_round [cur_result_row]     = derived_Xu_params@target_num_links_between_2_groups_per_round
    results_df$num_links_within_one_group [cur_result_row]                      = derived_Xu_params@num_links_within_one_group
    results_df$tot_num_links_inside_groups [cur_result_row]                     = derived_Xu_params@tot_num_links_inside_groups
    results_df$max_possible_num_links_between_groups [cur_result_row]           = derived_Xu_params@max_possible_num_links_between_groups
    results_df$max_possible_tot_num_links [cur_result_row]                      = derived_Xu_params@max_possible_tot_num_links

        #  Marxan options
    results_df$marxan_spf_const [cur_result_row]                                 = spf_const
    results_df$marxan_PROP [cur_result_row]                                      = marxan_PROP
    results_df$marxan_RANDSEED [cur_result_row]                                  = marxan_RANDSEED
    results_df$marxan_NUMREPS [cur_result_row]                                   = marxan_NUMREPS

        #  Marxan Annealing Parameters
    results_df$marxan_NUMITNS [cur_result_row]                                   = marxan_NUMITNS
    results_df$marxan_STARTTEMP [cur_result_row]                                 = marxan_STARTTEMP
    results_df$marxan_NUMTEMP [cur_result_row]                                   = marxan_NUMTEMP

        #  Marxan Cost Threshold
    results_df$marxan_COSTTHRESH [cur_result_row]                                = marxan_COSTTHRESH
    results_df$marxan_THRESHPEN1 [cur_result_row]                                = marxan_THRESHPEN1
    results_df$marxan_THRESHPEN2 [cur_result_row]                                = marxan_THRESHPEN2

        #  Marxan Program control
    results_df$marxan_RUNMODE [cur_result_row]                                   = marxan_RUNMODE
    results_df$marxan_MISSLEVEL [cur_result_row]                                 = marxan_MISSLEVEL
    results_df$marxan_ITIMPTYPE [cur_result_row]                                 = marxan_ITIMPTYPE
    results_df$marxan_HEURTYPE [cur_result_row]                                  = marxan_HEURTYPE
    results_df$marxan_CLUMPTYPE [cur_result_row]                                 = marxan_CLUMPTYPE

        #  Full runset name
    results_df$runset_name [cur_result_row]                                      = parameters$runset_name

#--------------------------------------------------------------------------------
#  2017 02 17 - BTL
#  IS THIS STILL TRUE?
#  NEED TO INVESTIGATE.
#
#  Getting an error.  Not sure why...  Is it because the free variable names
#  like num_PUs, are the same as the list element names, like results_df$num_PUs?
#
#  Error in `$<-.data.frame`(`*tmp*`, "num_PUs", value = c(NA, 12L)) :
#    replacement has 2 rows, data has 1
#  Calls: source ... withVisible -> eval -> eval -> $<- -> $<-.data.frame
#  Execution halted
#--------------------------------------------------------------------------------

    if (compute_network_metrics)
        {
        #  Need to bind the network measures to the data frame too.
        results_df = cbind (results_df,
                      bipartite_metrics_from_bipartite_package,
                      bipartite_metrics_from_igraph_package_df
                      )
        }
    }

    write_results_to_files (results_df, parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
}

    }  #  end of function - create_master_output_structure()



#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#  THIS LAST SECTION IS A TEMPORARY DUMPING GROUND FOR CODE THAT'S BEEN
#  COMMENTED OUT BUT HAS SOME UTILITY IN BUILDING UP SOME DOCUMENTATION FOR
#  VARIOUS BITS OF DATA.  SEE MORE DETAILED EXPLANATION BELOW.
#-------------------------------------------------------------------------------
#  BTL - 2017 02 17
#  Need to document what some of the various marxan and bdpg vectors and lists
#  and data frame columns mean, since some of the names only made sense in
#  the context where they were created and now they're being used for other
#  things.  So here, I'm going to just collect a pile of code that's been
#  commented out but had some useful information in it before it was removed.
#  Later, you can get rid of all this, but not until some more documentation
#  is scavenged from it.  Not sure what all I need in that way though, so I
#  don't want to waste time on it right now.
#  solutions_df is no longer even referenced anywhere in bdpg.
#  Should I just get rid of it?
#  At the moment, it's slightly useful as a documentation of what some of the
#  different vectors mean (i.e., the data frame column names are more
#  meaningful than some of the vectors assigned to them.
#  Actually, that only applies to two of them:
#      - marxan_best_solution = marxan_best_df_sorted$SOLUTION, #  assumes already sorted by PU_ID
#      - marxan_votes         = marxan_ssoln_df$number
#  Would probably be better to just have a documentation section somewhere that
#  explains the different marxan output columns.

#   solutions_df = data.frame (puid = marxan_best_df_sorted$PUID,
#                              optimal_solution = correct_solution_vector,
#                              marxan_best_solution = marxan_best_df_sorted$SOLUTION, #  assumes already sorted by PU_ID
#                              marxan_votes = marxan_ssoln_df$number,
#                              cor_signed_diff = cor_signed_difference,
#                              cor_abs_val_diff = cor_abs_val_signed_difference,
#           #  2015 06 19 - BTL
#           #  Until now (3:04 pm), this said:
#           #       cor_num_spp_on_patch = final_link_counts_for_each_node$freq
#           #  That means that it was using the apparent, not the correct,
#           #  final link counts for each node.
#           #  I have now changed this to say cor_... instead.
#           #  Not sure if this was a source of unknown bugs before or will be
#           #  a source now.  Need to look more closely at this to see the
#           #  consequences where solutions_df$cor_num_spp_on_patch is used
#           #  downstream of here.  However, since this is the only place it
#           #  appears in this file and this file (gscp_15) is at the end of
#           #  the whole program, maybe it doesn't matter at all except being
#           #  echoed in some output file.
#           #  2017 02 11 - BTL
#           #  cor_final_counts_for_each_node seems to be the wrong name all over.
#           #  Trying cor_link_counts_for_each_node now.
#           #  All of this definitely needs some checking into...
# #                             cor_num_spp_on_patch = cor_final_counts_for_each_node$freq
#                              cor_num_spp_on_patch = cor_link_counts_for_each_node$freq
#                              )

#===============================================================================

#  cut from start of else clause of "if (read_Xu_problem_from_Xu_file)"

# cat ("\n\nJust before things requiring major fix in gscp_15:\n")
# #browser()
# # biodivprobgen_utilities.R:235:    #  like nodes$dependent_set_member.
# # biodivprobgen_utilities.R:238:    #       Error in marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member :
# # gen_bdprob.R:738:largest_PU_ID = max (Xu_nodes$node_ID)
# # generateSetCoverProblem.R:242:#all_correct_node_IDs = cor_nodes$node_ID
# # generateSetCoverProblem.R:243:all_correct_node_IDs = 1:max(cor_nodes$node_ID)
# # gscp_15_create_master_output_structure.R:131:2016 07 16 - nodes$dependent_set_member ONLY HAS THE NUMBER OF PLANNING UNITS
# # gscp_15_create_master_output_structure.R:159:      correct_solution_vector = nodes$dependent_set_member
# # gscp_15_create_master_output_structure.R:162:      cor_signed_difference = marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member
# #
# #
# # 2016 07 16 - nodes$dependent_set_member ONLY HAS THE NUMBER OF PLANNING UNITS
# #           THAT WERE IN THE ORIGINAL XU PROBLEM, NOT THE WRAPPED PROBLEM.
# #           MEANWHILE, THE MARXAN SOLUTION DOES HAVE THE WRAPPED PROBLEM PUs SO
# #           THE LENGTHS DO NOT MATCH.  NEED TO MAKE SURE THAT EVERYTHING IN THE
# #           WRAPPED PROBLEM HAS THE CORRECT DIMENSIONS AND VALUES.
# #             This is part of a larger problem of making sure that the problem
# #             returned by wrapping is correctly sized in every way to allow
# #             subsequent operations to act on it exactly as they would act on
# #             a base Xu problem.
# #
# #             One test of that is to make sure that all of
# #             the dimensions of the object elements include all planning units
# #             of the wrapped problem.  This may also be complicated by the
# #             application of error to generate an apparent problem.  That means
# #             you will also need to verify the problem dimensions and values
# #             again, after you have generated the apparent version.
# #
# #           ANOTHER PROBLEM HERE IS THAT THE XU SOLUTION IS NOT NECESSARILY
# #           THE ONLY CORRECT SOLUTION.  THIS MATCHING OF NODES TO A SOLUTION CAN
# #           BE WRONG IF MARXAN HAS FOUND A DIFFERENT CORRECT SOLUTION.
# #           NEED TO AT LEAST CHECK WHETHER
# #             A) MARXAN SOLUTION IS THE CORRECT SIZE (I.E., COST)
# #             AND
# #             B) IF IT IS THE CORRECT SIZE, THEN YOU ALSO NEED TO CHECK THAT
# #                IT REALLY DOES COVER THE SET, I.E., IT IS A CORRECT SOLUTION.

#===============================================================================

# #  2017 02 17 - BTL
# #  I don't think that this "Xu read from file" works anymore.
# #  When you go down and look at the loading of the final results dataframe,
# #  none of these variables are referenced.  In fact, none of them are ever
# #  referenced from here on, except for the first 3 and they're only used
# #  for calculations related to when not reading from a benchmark file.
# #  The results data frame is built using the Xu parameters structures that
# #  are attached to the problem itself, not from the variables below.
# #  So, either the building of the data frame needs to change or else the
# #  creation of the problem object from a benchmark file needs to do the
# #  empty assignment stuff below as part of its initialization.
# #  However, either way leaves the first 3 entries in the if section wrong.
# #  They look like they were only used in comparisons of marxan solution
# #  vectors with Xu solution vectors and that stuff never gets to the final
# #  results because the Xu solution vector might not be the only correct one
# #  and I think that all of the code and output that referenced that stuff
# #  has been removed.
# #  So, I'm going to remove this whole clause but leave it in the dead code
# #  section at the bottom of this file until I've got it all worked out.
# #
# #  Regardless of that, one important thing is to check to see how building
# #  the results data frame behaves when given a Xu problem that's read from
# #  a benchmark file.  For that matter, how does it behave when given ANY
# #  problem that lacks the Xu parameters that are currently being written
# #  to the data frame?
#
#       #---------------------------------------------------------------------
#
#     if (read_Xu_problem_from_Xu_file)
#         {
#         correct_solution_vector = rep (NA, num_PUs)
#         cor_signed_difference = rep (NA, num_PUs)
#         cor_abs_val_signed_difference = rep (NA, num_PUs)
#
#         #  Xu options
#         n__num_groups = NA
#         alpha__ = NA
#         p__prop_of_links_between_groups = NA
#         r__density = NA
#
#         #  Derived Xu options
#         num_nodes_per_group = NA
#         tot_num_nodes = num_PUs
#         num_independent_set_nodes = tot_num_nodes - correct_solution_cost
#         num_dependent_set_nodes = correct_solution_cost
#         num_rounds_of_linking_between_groups = NA
#         target_num_links_between_2_groups_per_round = NA
#         num_links_within_one_group = NA
#         tot_num_links_inside_groups = NA
#         max_possible_num_links_between_groups = NA
#         max_possible_tot_num_links = NA
#
#         opt_solution_as_frac_of_tot_num_nodes = correct_solution_cost / tot_num_nodes
#
#         } else  #  generated the problem

#===============================================================================

#-----------------------------------------------------
#  2017 02 17 12:17 am - BTL
#  Variables that RStudio had flagged as never used:
#-----------------------------------------------------

#unreferenced#bdpg_extended_params = Xu_parameters@bdpg_extended_params
#unreferenced#        cor_signed_difference = marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member
#unreferenced#        cor_abs_val_signed_difference = abs (cor_signed_difference)
#unreferenced#  app_solution_spp_rep_fracs              = app_results_list$spp_rep_fracs
#unreferenced#  cor_spp_rep_fracs              = cor_results_list$spp_rep_fracs

#-------------------------------------------------------------------------
#  2017 02 17 12:17 am - BTL
#  Variables that are commented out throughout the code and just getting
#  in the way at the moment but I might want to restore later on:
#-------------------------------------------------------------------------

  # app_solution_NUM_spp_covered            = app_results_list$num_spp_covered
  # app_solution_FRAC_spp_covered           = app_results_list$frac_spp_covered
  # app_spp_rep_shortfall                   = app_results_list$spp_rep_shortfall

  #         #  These should be the same as those computed via Marxan.
  #     app_solution_spp_rep_fracs =
  #         compute_rep_fraction (app_bpm, marxan_best_solution_PU_IDs, DEBUG_LEVEL, spp_rep_targets)
  #     app_solution_unmet_spp_rep_frac_indices = which (app_solution_spp_rep_fracs < 1)
  #     app_solution_NUM_spp_covered = num_spp - length (app_solution_unmet_spp_rep_frac_indices)
  #
  #     app_solution_FRAC_spp_covered = app_solution_NUM_spp_covered / num_spp
  #     app_spp_rep_shortfall = 1 - app_solution_FRAC_spp_covered

            #     cat ("\napp_solution_NUM_spp_covered =", app_solution_NUM_spp_covered)
            #     cat ("\napp_solution_FRAC_spp_covered =", app_solution_FRAC_spp_covered)
            #     cat ("\napp_spp_rep_shortfall =", app_spp_rep_shortfall)

#----------------------------------
#----------------------------------

  # cor_NUM_spp_covered            = cor_results_list$num_spp_covered
  # cor_FRAC_spp_covered           = cor_results_list$frac_spp_covered
  # cor_spp_rep_shortfall                   = cor_results_list$spp_rep_shortfall

  #     cor_spp_rep_fracs =
  #         compute_rep_fraction (cor_bpm, marxan_best_solution_PU_IDs, DEBUG_LEVEL, spp_rep_targets)
  #     cor_unmet_spp_rep_frac_indices = which (cor_spp_rep_fracs < 1)
  #     cor_NUM_spp_covered = num_spp - length (cor_unmet_spp_rep_frac_indices)
  #
  #     cor_FRAC_spp_covered = cor_NUM_spp_covered / num_spp
  #     cor_spp_rep_shortfall = 1 - cor_FRAC_spp_covered



  #     cat ("\ncor_NUM_spp_covered =", cor_NUM_spp_covered)
  #     cat ("\ncor_FRAC_spp_covered =", cor_FRAC_spp_covered)
  #     cat ("\ncor_spp_rep_shortfall =", cor_spp_rep_shortfall)

#===============================================================================

      #------------------------------------------------------------------------
      #  2017 02 17 - BTL
      #  I HAVE NO IDEA WHAT THIS COMMENT IS ABOUT OR WHETHER IT EVEN APPLIES
      #  ANYMORE.
      #  FOR EXAMPLE, THE LAST BIT ABOUT FP AND FN RATES IS DEFINITELY OUT
      #  OF DATE NOW, SINCE THOSE _ARE_ INCLUDED IN THE OUTPUT.
      #------------------------------------------------------------------------

  #  Supporting data not in binding
  #   species vs planning units (database?) to allow computation of performance
  #   measures related to which species are covered in solutions
  #   (e.g., SELECT species WHERE planning unit ID == curPlanningUnitID)
  #   (e.g., SELECT planning unit ID WHERE species == curSpeciesID))
  #       - planning unit IDs
  #       - set of species on planning unit

  #     ALSO STILL NEED TO ADD THE FP AND FN RATES AND OTHER ERROR MODEL
  #     ATTRIBUTES TO THE OUTPUT DATA FRAME AND CSV FILE.

#===============================================================================

#------------------------------------------------------------------------------
#  BTL - 2017 02 17
#  I'm not sure whether "apparent" in all of this means apparent in
#  the sense of the added error in the problem itself or in the sense
#  of something being marxan's result instead of the correct result.
#
#  Is this stuff being done for all of marxan's solutions in its output or
#  is it only being done for marxan's choice of best solution?
#  Would be interesting to see it for all of marxan's solutions and in
#  particular, it would be interesting to see how well the ranking across
#  solutions compares between correct and apparent, i.e., do apparent rank
#  solutions hold when you look at what they're really getting in the correct
#  solutions?
#
#  Would it be useful to do a rank correlation between correct and apparent
#  or among the top 10 or 25 percent of solutions or whatever set is most
#  likely to be what people are paying attention to.
#------------------------------------------------------------------------------


#===============================================================================

            # cat ("\n\n----------------------------------")
            # cat ("\nAPP_ VALUES AS COMPUTED BY MARXAN:")
            # cat ("\n----------------------------------")
            # cat ("\napp_solution_NUM_spp_covered__fromMarxan  =", app_solution_NUM_spp_covered__fromMarxan)
            # cat ("\napp_solution_FRAC_spp_covered__fromMarxan =", app_solution_FRAC_spp_covered__fromMarxan)
            # cat ("\napp_spp_rep_shortfall__fromMarxan         =", app_spp_rep_shortfall__fromMarxan)

# #unreferenced#    app_solution_unmet_spp_rep_frac_indices = app_results_list$indices_of_spp_with_unmet_rep_frac
#
#             cat ("\n\n-----------------------------------------")
#             cat ("\nAPP_ VALUES AS COMPUTED BY BIODIVPROBGEN:")
#             cat ("\n-----------------------------------------")
#             cat ("\nlength (app_solution_unmet_spp_rep_frac_indices) = ",
#                 length (app_solution_unmet_spp_rep_frac_indices))
#
# #unreferenced#    cor_unmet_spp_rep_frac_indices = cor_results_list$indices_of_spp_with_unmet_rep_frac
#
#             cat ("\n\n-----------------------------------------")
#             cat ("\nCOR_ VALUES AS COMPUTED BY BIODIVPROBGEN:")
#             cat ("\n-----------------------------------------")
#             cat ("\nlength (cor_unmet_spp_rep_frac_indices) = ",
#                 length (cor_unmet_spp_rep_frac_indices))

#===============================================================================

    #-------------------------------------------------------------------------
    #  Create full results_df.
    #
    #  I.e., just allocate it and initialize it before going on to populate
    #  it with the results.
    #
    #  Would it make more sense to be building up partial data frames
    #  elsewhere and then here, just cbind the partial frames together?
    #  That seems like it could be written as a more flexible routine that
    #  builds results frames to suit particular questions rather than
    #  building a mammoth frame that holds every possible thing and has to
    #  choose dummy values to put into places that don't apply to some data.
    #-------------------------------------------------------------------------

#===============================================================================

#===============================================================================

#===============================================================================

#===============================================================================

#===============================================================================

