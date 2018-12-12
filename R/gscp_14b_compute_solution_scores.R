#===============================================================================

                    #  gscp_14b_compute_solution_scores.R

#===============================================================================

#  History:

#  2015 05 15 - BTL - Created by factoring out of gscp_15 as
#                     compute_solutions_scores.R.
#  2017 02 18 - BTL - Renamed to gscp_14b_compute_solution_scores.R.

#===============================================================================

#-------------------------------------------------------------------------------

#' Compute representation scores for candidate solution
#'
#' Scores are computed with respect to a reference spp occupancy matrix
#' (i.e., COR or APP)
#'
#' The species occupancy matrix is referred to as the "ref_spp_occ_matrix" to
#' indicate that we're computing scores with respect to a given reference, i.e.,
#' either correct or apparent, rather than always computing the correct score.
#'
#' This is to allow us to see the difference between how well a method is
#' actually doing and how it might appear to be doing when its performance is
#' measured against data of unknown correctness, which is how nearly all results
#' are presented in the literature.
#'
#-------------------------------------------------------------------------------

#' @param ref_spp_occ_matrix reference species occupancy matrix, e.g.,
#'     correct or apparent species occupancy matrix
#' @param cand_sol_PU_IDs vector of only the planning unit IDs that are
#'     included in the candidate solution
#' @inheritParams std_param_defns
#'
#' @return list of solution vector scores
#' @export
#'
#-------------------------------------------------------------------------------

#compute_and_verify_APP_rep_scores_according_to_bdpg <-
compute_and_verify_rep_scores_wrt <- function (ref_spp_occ_matrix,
                                               cand_sol_PU_IDs,
                                               spp_rep_targets,
                                               num_spp)
    {
        #----------------------------------------------------------------------
        #  For each species, compute what fraction of its representation
        #  target has been met by the candidate solution's vector of PU_IDs.
        #  If the ref_spp_occ_matrix is the app_bpm, then the result will
        #  be the representation fraction that the candidate solution APPEARS
        #  to meet.  If the ref_spp_occ_matrix is the cor_bpm, then the
        #  result will be the representation fraction actually achieved by
        #  the candidate solution.
        #------------------------------------------------------------------

    num_spp_covered = compute_num_spp_covered_by_solution (cand_sol_PU_IDs,
                                                           ref_spp_occ_matrix,
                                                           spp_rep_targets)
    frac_spp_covered = num_spp_covered  /  num_spp
    spp_rep_shortfall = 1  -  frac_spp_covered

        #----------

    results_list = list (spp_rep_shortfall = spp_rep_shortfall,
                         num_spp_covered   = num_spp_covered,
                         frac_spp_covered  = frac_spp_covered)

    return (results_list)
    }

#===============================================================================
#  2018 12 12 - BTL -
#  This section moved here from gscp_15_create_master_output_structure.R.
#-------------------------------------------------------------------------------

compute_euc_out_err_frac <- function (cor_or_app_str,
                                        solution_cost_err_frac,
                                        frac_spp_covered)
    {
    euc_out_err_frac = sqrt ((solution_cost_err_frac ^ 2) + ((1 - frac_spp_covered) ^2))

    if (cor_or_app_str == "COR")
        {
        results_list = list (rsr_COR_euc_out_err_frac = euc_out_err_frac)

        } else if (cor_or_app_str == "APP")
        {
        results_list = list (rsr_APP_euc_out_err_frac = euc_out_err_frac)

        } else
        {
        stop_bdpg (paste0 ("cor_or_app_str = '", cor_or_app_str,
                           "'.  Must be 'COR' or 'APP'"))
        }

    return (results_list)
    }

#===============================================================================

compute_RS_solution_cost_scores_wrt_COR_costs_vec <-
                                            function (rs_solution_PU_IDs_vec,
                                                      cor_optimum_cost,
                                                      PU_costs_vec)
    {
    #---------------------------------------------------------------------------
    #         Compute error in cost of reserve selector's solution.
    #---------------------------------------------------------------------------

    rs_solution_cost = compute_solution_cost (rs_solution_PU_IDs_vec,
                                              PU_costs_vec)
    rs_solution_cost_err_frac = (rs_solution_cost - cor_optimum_cost) /
                                cor_optimum_cost
    abs_rs_solution_cost_err_frac = abs (rs_solution_cost_err_frac)

    #---------------------------------------------------------------------
    #  Giving errors as a fraction of the correct optimum cost
    #  may be misleading sometimes, e.g., when the correct optimum cost
    #  is nearly the cost of the full landscape.
    #  Even just guessing the cost of the whole landscape would not give
    #  much percentage error in that case.
    #  So, we'll also compute the error as a fraction of the maximum
    #  possible over-optimum or under-optimum error to see if that is
    #  more informative/predictable than guessing the usual percentage
    #  error.
    #---------------------------------------------------------------------

    rs_over_opt_cost_err_frac_of_possible_overcost = NA
    total_landscape_cost = sum (PU_costs_vec)
    rs_max_overcost = total_landscape_cost - cor_optimum_cost
    if (rs_solution_cost_err_frac > 0)
        rs_over_opt_cost_err_frac_of_possible_overcost =
            (rs_solution_cost - cor_optimum_cost) / rs_max_overcost

    rs_under_opt_cost_err_frac_of_possible_undercost = NA
    if (rs_solution_cost_err_frac < 0)
        rs_under_opt_cost_err_frac_of_possible_undercost = abs_rs_solution_cost_err_frac

    #---------------------------------------------------------------------

    return (list (cor_optimum_cost = cor_optimum_cost,
                  rs_solution_cost = rs_solution_cost,
                  rs_solution_cost_err_frac = rs_solution_cost_err_frac,
                  abs_rs_solution_cost_err_frac = abs_rs_solution_cost_err_frac,
                  rs_over_opt_cost_err_frac_of_possible_overcost = rs_over_opt_cost_err_frac_of_possible_overcost,
                  rs_under_opt_cost_err_frac_of_possible_undercost = rs_under_opt_cost_err_frac_of_possible_undercost
                 ))
    }

#===============================================================================

#' Compute confusion matrix-based scores for candidate solution
#'
#' Computes error measures related to confusion matrix, etc. For the purpose of
#' computing a performance score, start by treating the problem as if it's a
#' classification problem, where a selected patch is classified as 1 and an
#' unselected patch is classified as 0. This allows us to use any of the many
#' existing measures developed for classifiers.
#'
#' Computations are done with respect to a reference spp occupancy matrix
#' (i.e., COR or APP)
#'
#' Choice of measures
#'
#' Doesn't matter too much which measures we use, since this is mostly about
#' demonstrating how to generate and evaluate problems and users will have to
#' choose which measure best aligns with their own goals.
#'
#' However, it may be that some of these measures are easier to learn to predict
#' than others, so it's good to provide several different ones until we know
#' more. The base case would be to provide the ones that are the simplest and
#' most direct measures over the confusion matrix. Confusion matrix fractions
#'
#' Computation of confusion matrix elements (TP,TN,FP,FN)
#'
#' Note that the TP and TN values are computed as the min of the candidate and
#' correct values. This is because the number of "trues" for the candidate can't
#' exceed the number of "trues" in the correct solution by definition.
#'
#' Similarly, any count of TP or TN that falls short of the corresponding counts
#' in the correct solution represents the number of TP or TN that the candidate
#' got right and using the number of TP or TN from the correct would overstate
#' the candidate's performance.
#'
#' This all seems a bit odd in the normal classification context because in
#' classification, you would have to know _which_ PUs the classifier got right
#' and count them up. In reserve selection, there could be many ways to get the
#' same final optimal count of PUs in the solution and we don't care _which_
#' ones are chosen to get that count. However, the same kind of a scoring system
#' can work because we know that anything short of the optimal number represents
#' the existance of False Negatives, i.e., _some_ PUs who should have been
#' included.  Similarly, any count greater than the optimal count implies the
#' existance of False Positives, i.e., _some_ PUs who should NOT have been
#' included.
#'
#' Since nearly all classification performance scores are based on some
#' combination of the 4 values from the confusion matrix (TP,TN,FP,FN), choosing
#' those 4 values sets us up to compute all these scores.
#'
#-------------------------------------------------------------------------------

#' Source of formulas for measures
#'
#' I got nearly all of these measures from one paper a while ago and I can't
#' remember exactly what paper it was at the moment.  Need to look this up
#' again.  I think it might have been the 2011 Liu et al. paper in the
#' references section below.
#'
#'@references
#'
#' Measuring and comparing the accuracy of species distribution models
#' with presence–absence data. C Liu, M White, G Newell - Ecography, 2011

#-------------------------------------------------------------------------------

#' @param cor_or_app_str string
#' @param num_PUs_in_cand_solution integer
#' @param num_PUs_in_optimal_solution integer
#' @param frac_spp_covered float
#' @param input_err_FP float
#' @param input_err_FN float
#' @inheritParams std_param_defns
#'
#' @return list of solution vector scores based on confusion matrix
#' @export
#'
#-------------------------------------------------------------------------------

compute_confusion_matrix_based_scores <- function (cor_or_app_str,
                                                   num_PUs_in_cand_solution,
                                                    num_PUs,
                                                    num_PUs_in_optimal_solution,
                                                  frac_spp_covered,
                                                    input_err_FP = 0,
                                                    input_err_FN = 0
                                                    )
    {
        #-------------------------------------------------------------
        #  Classification counts to base confusion matrix on
        #-------------------------------------------------------------

    num_cand_1s = num_PUs_in_cand_solution
    num_cand_0s = num_PUs - num_cand_1s

    num_optimum_1s = num_PUs_in_optimal_solution
    num_optimum_0s = num_PUs - num_optimum_1s

        #-------------------------------------------------------------
        #  Confusion matrix fractions
        #
        #  Note that the TP and TN values are computed as the min
        #  of the candidate and correct values.
        #  This is because the number of "trues" for the candidate
        #  can't exceed the number of "trues" in the correct
        #  solution by definition.
        #  Similarly, any count of TP or TN that falls short of the
        #  corresponding counts in the correct solution represents
        #  the number of TP or TN that the candidate got right and
        #  using the number of TP or TN from the correct would
        #  overstate the candidate's performance.
        #-------------------------------------------------------------

    TP = min (num_cand_1s, num_optimum_1s) / num_PUs
    TN = min (num_cand_0s, num_optimum_0s) / num_PUs
    FP = max (0, num_cand_1s - num_optimum_1s) / num_PUs
    FN = max (0, num_cand_0s - num_optimum_0s) / num_PUs

        #-------------------------------------------------------------
        #  Base evaluation measures over the confusion matrix
        #-------------------------------------------------------------
        #  These, particularly sensitivity and specificity, are
        #  the ingredients for many other compound measures
        #  such as TSS.
        #  I'm preceding their names with "c" to indicate that
        #  they are with respect to 0/1 classification of the
        #  PUs.
        #  I'm doing this because I am also experimenting with
        #  some other compound measures that have the same algebraic
        #  form, but different constituents, e.g., a pseudo-TSS
        #  based on cost savings and species representation
        #  shortfall instead of classifications.
        #  I will precede this experimental measures with
        #-------------------------------------------------------------

        #  cSe = sensitivity = fraction of correct presences (1's) predicted
    cSe = TP / (TP + FN)
        #  cSp = specificity = fraction of correct absences (0's) predicted
    cSp = TN / (TN + FP)
        #  cPPV = fraction of predicted presences (1's) that are correct
    cPPV = TP / (TP + FP)
        #  cNPV = fraction of predicted absences (0's) that are correct
    cNPV = TN / (TN + FN)

        #-------------------------------------------------------------
        #  Common, simple compound measures
        #-------------------------------------------------------------

    acc_frac = TP + TN
    acc_err_frac = 1 - acc_frac

    cost_savings = 1 - (num_cand_1s / num_PUs)
    opt_cost_savings = 1 - (num_optimum_1s / num_PUs)

    TSS = cSe + cSp - 1
    max_cSe_cSp = max (cSe, cSp)
    min_cSe_cSp = min (cSe, cSp)
    mean_cSe_cSp = (cSe + cSp) / 2
    prod_cSe_cSp = cSe * cSp
    euc_cSe_cSp = sqrt (cSe^2 + cSp^2) / sqrt (2)

        #-------------------------------------------------------------
        #  Error magnification with respect to input errors
        #  added in experiments.
        #  I base the magnfication on the larger of the two input
        #  errors to make the magnification more conservative,
        #  a little less sensational.
        #-------------------------------------------------------------

    mag_base = max (input_err_FP, input_err_FN)
    if (mag_base == 0)
        {
        acc_err_mag = NA

        } else
        {
        acc_err_mag = acc_err_frac / mag_base
        }

        #-------------------------------------------------------------
        #  Experimental compound measures
        #-------------------------------------------------------------

    pseudoTSS = TSS + frac_spp_covered - 1
    pseudo2TSS = TSS + (frac_spp_covered^2) - 1

    acc_TSS = acc_frac + frac_spp_covered - 1
    acc2_TSS = acc_frac + (frac_spp_covered^2) - 1

    savings_TSS = cost_savings + frac_spp_covered - 1
    savings2_TSS = cost_savings + (frac_spp_covered^2) - 1

    savings_TSS_opt = opt_cost_savings
    savings2_TSS_opt = opt_cost_savings

    diff_savings_TSS = savings_TSS_opt - savings_TSS
    diff_savings2_TSS = savings2_TSS_opt - savings2_TSS

    ratio_savings_TSS = savings_TSS_opt / savings_TSS
    ratio_savings2_TSS = savings2_TSS_opt / savings2_TSS

        #-------------------------------------------------------------
        # Measures still missing:
        #-------------------------------------------------------------
        # error magnifications
        # magspp = spp_shortfall_frac / max (FP, FN)
        # magPU = cost_error_frac / max (FP, FN)
        # where cost_error_frac is max (0 error, 1 error) ?
        # thresholded scores, i.e., score is 0 if not all spp covered
        # Confusion matrix and other supporting values
        #-------------------------------------------------------------

    #-------------------------------------------------------------

    if (cor_or_app_str == "COR")
        {
        results_list =
            list (
                rsr_COR_TP = TP,
                rsr_COR_TN = TN,
                rsr_COR_FP = FP,
                rsr_COR_FN = FN,

                rsr_COR_cSe  = cSe,
                rsr_COR_cSp  = cSp,
                rsr_COR_cPPV = cPPV,
                rsr_COR_cNPV = cNPV,

                rsr_COR_acc_frac     = acc_frac,
                rsr_COR_acc_err_frac = acc_err_frac,
                rsr_COR_cost_savings = cost_savings,

                rsr_COR_opt_cost_savings = opt_cost_savings,

                rsr_COR_TSS          = TSS,
                rsr_COR_max_cSe_cSp  = max_cSe_cSp,
                rsr_COR_min_cSe_cSp  = min_cSe_cSp,
                rsr_COR_mean_cSe_cSp = mean_cSe_cSp,
                rsr_COR_prod_cSe_cSp = prod_cSe_cSp,
                rsr_COR_euc_cSe_cSp  = euc_cSe_cSp,
                rsr_COR_acc_err_mag  = acc_err_mag
            )

        } else if (cor_or_app_str == "APP")
        {
        results_list =
            list (
                rsr_APP_TP = TP,
                rsr_APP_TN = TN,
                rsr_APP_FP = FP,
                rsr_APP_FN = FN,

                rsr_APP_cSe  = cSe,
                rsr_APP_cSp  = cSp,
                rsr_APP_cPPV = cPPV,
                rsr_APP_cNPV = cNPV,

                rsr_APP_acc_frac     = acc_frac,
                rsr_APP_acc_err_frac = acc_err_frac,
                rsr_APP_cost_savings = cost_savings,

                rsr_APP_opt_cost_savings = opt_cost_savings,

                rsr_APP_TSS          = TSS,
                rsr_APP_max_cSe_cSp  = max_cSe_cSp,
                rsr_APP_min_cSe_cSp  = min_cSe_cSp,
                rsr_APP_mean_cSe_cSp = mean_cSe_cSp,
                rsr_APP_prod_cSe_cSp = prod_cSe_cSp,
                rsr_APP_euc_cSe_cSp  = euc_cSe_cSp,
                rsr_APP_acc_err_mag  = acc_err_mag
            )

        } else
        {
        stop_bdpg (paste0 ("cor_or_app_str = '", cor_or_app_str,
                           "'.  Must be 'COR' or 'APP'"))
        }

    return (results_list)
    }

#===============================================================================
#           Compute error measures related to confusion matrix, etc.
#===============================================================================

#  Confusion matrix for match of 0/1 classification of PUs to optimal PUs.

# Se = sensitivity = fraction of correct presences (1's) predicted
# Sp = specificity = fraction of correct absences (0's) predicted
# error magnifications
# magspp = spp_shortfall_frac / max (FP, FN)
# magPU = cost_error_frac / max (FP, FN)
# where cost_error_frac is max (0 error, 1 error) ?
# TSS and ED for 0/1 PU counts
# TSS and ED for 0/1 PU_IDs
# pTSS for savings, num_spp
# pED ?
# Confusion matrix and other supporting values

#===============================================================================

