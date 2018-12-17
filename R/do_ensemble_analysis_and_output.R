#===============================================================================
#
#                       do_ensemble_analysis_and_output.R
#
#  Run code to run ensemble and dump results to file.
#
#===============================================================================
#===============================================================================
#===============================================================================

collect_all_ens_cand_sols <- function (ens_RSrun_dirs,
                                       ens_prob_dirs,
                                       num_PUs
                                       #,
                                       #COR_bd_prob    #  unnecessary?
                                       )
    {
    num_ensemble_elements      = length (ens_prob_dirs)
    all_ens_marxan_best_sols   = vector (mode = "list", length = num_ensemble_elements)
    all_ens_marxan_summed_sols = vector (mode = "list", length = num_ensemble_elements)

    for (cur_idx in 1:num_ensemble_elements)
        {
        cur_RSrun_dir = ens_RSrun_dirs [cur_idx]
#         fileName = paste0 ("saved.", basename (cur_RSrun_dir), ".rds")
#         APP_marxan_run = readRDS (file.path (cur_RSrun_dir, fileName))

#         cur_prob_dir = ens_prob_dirs [cur_idx]
#         fileName = paste0 ("saved.", basename (cur_prob_dir), ".rds")
#         APP_bd_prob = readRDS (file.path (cur_prob_dir, fileName))
# browser()
#         rs_best_and_summed_solution_PU_IDs =
#                 get_marxan_best_and_summed_solution_PU_IDs (APP_marxan_run,
#                                                             ens_starting_dir,
#
#                                                             #COR_bd_prob,    #  unnecessary?  just reuse APP_bd_prob?
#                                                             APP_bd_prob,
#                                                             APP_bd_prob)
# browser()

                #-------------------------
                #  Best OVERALL solution
                #-------------------------

        Marxan_SA_best_solution_file_name =
                "Marxan_SA_best_solution_PU_IDs.csv"
        Marxan_SA_best_solution_file_path =
                file.path (cur_RSrun_dir, Marxan_SA_best_solution_file_name)
        rs_best_solution_PU_IDs =
                scan (Marxan_SA_best_solution_file_path, sep=",")

        all_ens_marxan_best_sols [[cur_idx]] = rs_best_solution_PU_IDs
            # rs_best_and_summed_solution_PU_IDs$rs_best_solution_PU_IDs
            #        rs_best_and_summed_solution_PU_IDs$marxan_best_solution_PU_IDs

                #-------------------
                #  SUMMED solution
                #-------------------

        Marxan_SA_SS_summed_solution_file_name =
                "Marxan_SA_SS_summed_solution_PU_IDs.csv"
        Marxan_SA_SS_summed_solution_file_path =
                file.path (cur_RSrun_dir, Marxan_SA_SS_summed_solution_file_name)
        marxan_summed_solution_PU_IDs =
                scan (Marxan_SA_SS_summed_solution_file_path, sep=",")

        all_ens_marxan_summed_sols [[cur_idx]] = marxan_summed_solution_PU_IDs
            # rs_best_and_summed_solution_PU_IDs$marxan_best_summed_solution_PU_IDs


cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
cat ("At bottom of loop COLLECTING CANDIDATE SOLUTIONS.")
cat ("cur_idx = ", cur_idx, "\n")
cat ("\n\nall_ens_marxan_best_sols = \n")
print (all_ens_marxan_best_sols)
cat ("\n\nall_ens_marxan_summed_sols = \n")
print (all_ens_marxan_summed_sols)
cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
#browser()
        }

cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
cat ("About to return from COLLECTING CANDIDATE SOLUTIONS.")
cat ("cur_idx = ", cur_idx, "\n")
cat ("\n\nall_ens_marxan_best_sols = \n")
print (all_ens_marxan_best_sols)
cat ("\n\nall_ens_marxan_summed_sols = \n")
print (all_ens_marxan_summed_sols)
cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
#browser()
    return (list (best_sols   = all_ens_marxan_best_sols,
                  summed_sols = all_ens_marxan_summed_sols))
    }

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
        #
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

            #  Need to make sure that marxan (or in the future, whatever RS is
            #  used by the ensemble) is turned on.
            #  If it's not turned on, then even though it will be run on all
            #  the ensemble subproblems, it won't be run on the APP problem
            #  they'are all derived from.  That would be an issue because
            #  that problem is also included in the ensemble and therefore,
            #  needs to have the same reserve selector run on it that is run
            #  on all the other ensemble subproblems.

    run_marxan = vb (parameters$run_marxan, def_on_empty=TRUE)
    if (! run_marxan)
        stop_bdpg ("run_marxan must be TRUE when running ensemble.")

    parameters$do_ensemble                      = FALSE  #  To avoid infinite recursion

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

    num_probs_in_ensemble = RS_specific_params$num_probs_in_ensemble

# SHOULD I ALSO ADD (or subtract) AN ELEMENT IN THESE ARRAYS SO THAT THE ORIGINAL
# APP PROBLEM CAN BE PART OF THE ENSEMBLE?
# OR AT LEAST MAKE THAT AN OPTION?
# SEEMS LIKE IT SHOULD DEFINITELY BE PART OF THE ENSEMBLE.
# COULD SUBTRACT ONE FROM THE ENSEMBLE SIZE IN THE LOOPING AND JUST
# LOAD THE ORIGINAL APP PROBLEM INTO THE FIRST ELEMENT OF THE ARRAYS.
# NOTE THAT IF YOU DO THIS, THEN YOU HAVE TO BE SURE THAT MARXAN OR WHATEVER
# THE ENSEMBLE RS IS GETS RUN ON THE ORIGINAL APP PROBLEM, SINCE YOU COULD
# SET THE YAML FILE RS SELECTIONS TO NOT INCLUDE MARXAN EVEN THOUGH IT IS USED
# IN THE ENSEMBLE.

    prob_dirs = vector (mode="character", length=num_probs_in_ensemble)
    marxan_rsrun_dirs = vector (mode="character", length=num_probs_in_ensemble)

    for (cur_prob_idx in 1:num_probs_in_ensemble)
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

        prob_dirs [cur_prob_idx] = prob_and_rsruns_topdirs_list$bd_prob_topdir
        marxan_rsrun_dirs [cur_prob_idx] = prob_and_rsruns_topdirs_list$RS_topDirs_list$marxan

        cat ("\n=============================================\n")
        cat ("After gen_1_app_variant() for cur_prob_idx = ",
             cur_prob_idx, ", prob_and_rsruns_topdirs_list = \n")
        print (prob_and_rsruns_topdirs_list)
        cat ("=============================================\n")
        }

    cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
    cat ("Finished generating ensemble.")
    cat ("prob_dirs = \n")
    print (prob_dirs)
    cat ("\n\nmarxan_rsrun_dirs = \n")
    print (marxan_rsrun_dirs)
    cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")

    num_PUs = APP_bd_prob@num_PUs

    all_ens_cand_sols = collect_all_ens_cand_sols (marxan_rsrun_dirs,
                                                   prob_dirs,
                                                   num_PUs)

    all_ens_marxan_best_sols   = all_ens_cand_sols$best_sols
    all_ens_marxan_summed_sols = all_ens_cand_sols$summed_sols

    cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
    cat ("FINISHED COLLECTING CANDIDATE SOLUTIONS.")
    cat ("all_ens_marxan_best_sols = \n")
    print (all_ens_marxan_best_sols)
    cat ("\n\nall_ens_marxan_summed_sols = \n")
    print (all_ens_marxan_summed_sols)
    cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
    }

#===============================================================================

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

