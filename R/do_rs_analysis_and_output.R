#===============================================================================
#
#                       do_rs_analysis_and_output.R
#
#  Run code to run reserve selector and dump results to file.
#
#  Used to do graph analysis here too, but no longer.
#
#===============================================================================

ensemble <- function (APP_bd_prob,
                      parameters,
                      ens_probs_starting_dir,
                      RS_specific_params)
    {
    cat ("\nInside ensemble function.\n")

        #  Modify parameters to block graph calculations and to set
        #  set reserve selectors to block infinite recursion (i.e, don't
        #  call ensemble again) and to only call reserve selectors that
        #  are meant to be used to build the ensemble.

#  NOTE:  2018 12 16 - BTL
#         THERE'S A LIKELY FUTURE PROBLEM HERE BECAUSE THIS LIST
#         OF RESERVE SELECTORS TO BE TURNED OFF REQUIRES YOU TO KNOW
#         AND KEEP CURRENT, ALL POSSIBLE RESERVE SELECTORS, WHICH
#         MEANS THAT ADDING A NEW ONE ELSEWHERE WOULD MEAN YOU'D HAVE
#         TO BE SURE TO UPDATE THIS LIST.  NOT A GOOD IDEA.
#         WILL LEAVE IT FOR THE MOMENT WHILE I GET THIS WORKING, BUT
#         DEFINITELY NEED TO FIGURE OUT A MORE ROBUST STRATEGY, EVEN
#         IF IT'S JUST TO PASS THE LIST OF RESERVE SELECTORS TO TURN
#         OFF IN TO THIS ROUTINE.

    parameters$compute_network_metrics          = FALSE

    parameters$run_marxan                       = TRUE

    parameters$do_ensemble                      = FALSE

    parameters$do_gurobi                        = FALSE
    parameters$do_simple_richness_forward       = FALSE
    parameters$do_simple_richness_backward      = FALSE
    parameters$do_unprotected_richness_forward  = FALSE
    parameters$do_unprotected_richness_backward = FALSE
    parameters$do_zonation_like_forward         = FALSE
    parameters$do_zonation_like_backward        = FALSE


#  2018 12 16 - BTL - For quick testing at the moment...
FP_err_amt = 0.001
FN_err_amt = 0.01

#  2018 12 16 - BTL - No cost errors for the moment...
gen_combined_cost_and_FP_FN_errors = FALSE
cost_err_amt = 0

    for (cur_prob_idx in 1:RS_specific_params$num_probs_in_ensemble)
        {
                #-------------------------------------------
                #  Variant 3:  FP & FN, counts NOT matched
                #-------------------------------------------

                        # (list (bd_prob_topdir = APP_bd_prob_topdir,
                        #       RS_topDirs_list = RS_topDirs_list))
        prob_and_rsruns_topdirs_list =
            gen_1_app_variant (APP_bd_prob,    #base_bd_prob,
                               parameters,
                               ens_probs_starting_dir,    #starting_dir,

                               gen_cost_errors = gen_combined_cost_and_FP_FN_errors,
                               cost_error_frac_bound = cost_err_amt,    #err_amt,

                               gen_FP_FN_errors = TRUE,

                               spp_occ_FP_const_rate = FP_err_amt,    #err_amt,
                               spp_occ_FN_const_rate = FN_err_amt,    #err_amt,

                               match_error_counts = FALSE)

        cat ("\n\n=============================================\n\n")
        cat ("After gen_1_app_variant() for cur_prob_idx = ",
             cur_prob_idx, ", prob_and_rsruns_topdirs_list = \n")
        print (prob_and_rsruns_topdirs_list)
        cat ("\n\n=============================================\n\n")
        }
    }

#-------------------------------------------------------------------------------

do_ensemble <- function (APP_bd_prob,    #  <<<<<-----------------
                         COR_bd_prob,    #  <<<<<-----------------
                         parameters,
                         starting_dir,

                         rs_method_name,
                         resSel_func,

                         RS_specific_params,

                         src_rds_file_dir,
                         spp_rep_targets)
    {
    cat ("\nInside do_ensemble():", sep='')
    cat ("\n        starting_dir = '", starting_dir, "'", sep='')
    cat ("\n        rs_method_name = '", rs_method_name, "'", sep='')
    cat ("\n        RS_specific_params = \n", sep='')
    print (RS_specific_params)
    cat ("\n        src_rds_file_dir = '", src_rds_file_dir, "'", sep='')
    cat ("\n        spp_rep_targets = \n", sep='')
    print (spp_rep_targets)
    cat ("\n")

    ResSel_run <- create_RSrun (APP_bd_prob@UUID,
                                spp_rep_targets,
                                parameters,
                                starting_dir,
                                APP_bd_prob@cor_or_app_str,
                                APP_bd_prob@basic_or_wrapped_or_comb_str,
                                rs_method_name)

    base_outdir = get_RSrun_path_topdir (ResSel_run, starting_dir)

    ens_probs_starting_dir = file.path (base_outdir, "Ens_probs")
    dir.create (ens_probs_starting_dir)
    cat ("\n\nens_probs_starting_dir = '", ens_probs_starting_dir, "'\n")

    ensemble (APP_bd_prob,
              parameters,
              ens_probs_starting_dir,
              RS_specific_params)

    RSrun_topdir = get_RSrun_path_topdir (ResSel_run, starting_dir)

    return (RSrun_topdir)
    }

#===============================================================================

#' Run reserve selector(s) on bdproblem and write output from all analyses
#'
#' Note that the COR and APP arguments are the same if running on a correct
#' problem.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns named list containing top directory for each reserve
#' selector that was run
#' @export

#-------------------------------------------------------------------------------

do_rs_analysis_and_output <- function (APP_bd_prob,
                                           COR_bd_prob,
                                           parameters,
                                       starting_dir,
                                           src_rds_file_dir = NULL,
                                           spp_rep_targets =
                                               rep(1,COR_bd_prob@num_spp))
    {
    RS_topDirs_list = list ()

    #---------------------------------------------------------------------------

    do_ensemble = vb (parameters$do_ensemble,
                      def_on_empty = TRUE, def = FALSE)

    if (do_ensemble)
        {
        num_probs_in_ensemble = vn (parameters$num_probs_in_ensemble,
                                    range_lo=2)

        RS_specific_params = list (ensemble_sub_rs_list = c ("marxan"),
                                   num_probs_in_ensemble = num_probs_in_ensemble)
        rs_method_name = "Ensemble"

        ensemble_topDir =
            do_ensemble (APP_bd_prob,    #  <<<<<-----------------
                         COR_bd_prob,    #  <<<<<-----------------
                         parameters,
                         starting_dir,

                         rs_method_name,
                         resSel_func     = ensemble,

                         RS_specific_params,

                         src_rds_file_dir,
                         spp_rep_targets)

        RS_topDirs_list$ensemble = ensemble_topDir
        }

    #-----------------------------------

    do_simple_richness_forward = vb (parameters$do_simple_richness_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_simple_richness_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "SR_Forward"

        srforward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = simple_richness,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$srforward = srforward_topDir
        }

    #-----------------------------------

    do_simple_richness_backward = vb (parameters$do_simple_richness_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_simple_richness_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "SR_Backward"

        srbackward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = simple_richness,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$srbackward = srbackward_topDir
        }

    #---------------------------------------------------------------------------

    do_unprotected_richness_forward = vb (parameters$do_unprotected_richness_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_unprotected_richness_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "UR_Forward"

        urforward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = unprotected_richness,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$urforward = urforward_topDir
        }

    #-----------------------------------

    do_unprotected_richness_backward = vb (parameters$do_unprotected_richness_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_unprotected_richness_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "UR_Backward"

        urbackward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = unprotected_richness,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$urbackward = urbackward_topDir
        }

    #---------------------------------------------------------------------------

    do_zonation_like_forward = vb (parameters$do_zonation_like_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_zonation_like_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "ZL_Forward"

        zlforward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = zonation_like,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$zlforward = zlforward_topDir
        }

    #-----------------------------------

    do_zonation_like_backward = vb (parameters$do_zonation_like_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_zonation_like_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "ZL_Backward"

        zlbackward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = zonation_like,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$zlbackward = zlbackward_topDir
        }

    #---------------------------------------------------------------------------

    marxan_elapsed_time = NA
    run_marxan = vb (parameters$run_marxan, def_on_empty = TRUE, def = FALSE)
    if (run_marxan)
        {
        rs_method_name = "Marxan_SA"
        # marxan_elapsed_time =
        marxan_ret_vals =
            do_marxan_analysis_and_output (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                           starting_dir,
                                               rs_method_name,
                                               src_rds_file_dir,
                                               spp_rep_targets)

        marxan_elapsed_time = marxan_ret_vals$marxan_elapsed_time
        RS_topDirs_list$marxan = marxan_ret_vals$marxan_topdir
        }

    #---------------------------------------------------------------------------

    do_gurobi = vb (parameters$do_gurobi, def_on_empty = TRUE, def = FALSE)
    if (do_gurobi)
        {
        rs_method_name = "Gurobi"
        gurobi_topDir =
            do_gurobi_analysis_and_output (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                           starting_dir,
                                               rs_method_name,
                                               marxan_elapsed_time,
                                               src_rds_file_dir,
                                               spp_rep_targets)

        RS_topDirs_list$gurobi = gurobi_topDir
        }

    return (RS_topDirs_list)
    }

#===============================================================================



