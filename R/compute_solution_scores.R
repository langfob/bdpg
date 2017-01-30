#===============================================================================

                    #  compute_solution_scores.R

#===============================================================================

#  History:

#  2015 05 15 - BTL - Created by factoring out of gscp_15...

#===============================================================================

    #  In this routine, cand_sol stands for "candidate solution"

compute_solution_vector_scores <- function (ref_spp_occ_matrix,
                                            num_PUs,
                                            cand_sol_PU_IDs,
                                            num_PUs_in_cand_solution,
                                            num_PUs_in_optimal_solution,
                                            spp_rep_targets,
                                            num_spp,
                                            input_err_FP = 0,
                                            input_err_FN = 0)
    {
    spp_rep_fracs = compute_rep_fraction (ref_spp_occ_matrix,
                                                   cand_sol_PU_IDs,
                                                   spp_rep_targets)

    indices_of_spp_with_unmet_rep_frac =  which (spp_rep_fracs < 1)

    num_spp_covered = num_spp  -  length (indices_of_spp_with_unmet_rep_frac)

    frac_spp_covered = num_spp_covered  /  num_spp

    spp_rep_shortfall = 1  -  frac_spp_covered

        cat ("\n\n-------------------------------------------------------------------------")
        cat ("\nIn compute_solution_vector_scores(), SCORES AS COMPUTED BY BIODIVPROBGEN:")
        cat ("\n-------------------------------------------------------------------------")
        cat ("\nlength (indices_of_spp_with_unmet_rep_frac) = ",
           length (indices_of_spp_with_unmet_rep_frac))
        cat ("\nnum_spp_covered =", num_spp_covered)
        cat ("\nfrac_spp_covered =", frac_spp_covered)
        cat ("\nspp_rep_shortfall =", spp_rep_shortfall)

        #------------------------------------------------------------
        #  Compute error measures related to confusion matrix, etc.
        #------------------------------------------------------------
        #  Doesn't matter too much which measures we use, since
        #  this is mostly about demonstrating how to generate and
        #  evaluate problems and users will have to choose which
        #  measure best aligns with their own goals.
        #  However, it may be that some of these measures are easier
        #  to learn to predict than others, so it's good to provide
        #  several different ones until we know more.
        #  The base case would be to provide the ones that are the
        #  simplest and most direct measures over the confusion
        #  matrix.
        #------------------------------------------------------------

#-------------------------------------------------------------

        #-------------------------------------------------------------
        #  Test code - will extract and re-enable later
        #-------------------------------------------------------------

# 2015 05 17 - BTL
# Test code part 1:            i.e., start of a function wrapper for the EFs
# build_confusion_matrix <- function (num_PUs,
#                                     num_PUs_in_optimal_solution,
#                                     num_PUs_in_marxan_solution,
#                                     frac_spp_covered,
#                                     input_err_FP = 0,
#                                     input_err_FN = 0
#                                     )
#     {

#-------------------------------------------------------------

        #-------------------------------------------------------------
        #  Classification counts to base confusion matrix on
        #-------------------------------------------------------------

    num_marxan_1s = num_PUs_in_cand_solution
    num_marxan_0s = num_PUs - num_marxan_1s

    num_optimum_1s = num_PUs_in_optimal_solution
    num_optimum_0s = num_PUs - num_optimum_1s

        #-------------------------------------------------------------
        #  Confusion matrix fractions
        #-------------------------------------------------------------

    TP = min (num_marxan_1s, num_optimum_1s) / num_PUs
    TN = min (num_marxan_0s, num_optimum_0s) / num_PUs
    FP = max (0, num_marxan_1s - num_optimum_1s) / num_PUs
    FN = max (0, num_marxan_0s - num_optimum_0s) / num_PUs

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

    cost_savings = 1 - (num_marxan_1s / num_PUs)
    opt_cost_savings = 1 - (num_optimum_1s / num_PUs)

    TSS = cSe + cSp - 1
    max_cSe_cSp = max (cSe, cSp)
    min_cSe_cSp = min (cSe, cSp)
    mean_cSe_cSp = (cSe + cSp) / 2
    prod_cSe_cSp = cSe * cSp
    euc_cSe_cSp = sqrt (cSe^2 + cSp^2) / sqrt (2)

    #--------------------

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

    #--------------------

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

        #-------------------------------------------------------------
        #  Test code - will extract and re-enable later
        #-------------------------------------------------------------

# 2015 05 17 - BTL
# Test code part 2:            i.e., end of a function wrapper for the EFs
#                                    plus a test call to that function
#     }
#
# num_PUs = 100
# num_PUs_in_marxan_solution = 25
# num_PUs_in_optimal_solution = 90
# frac_spp_covered = 0.3
# input_err_FP = 0.1
# input_err_FN = 0.01
#
# build_confusion_matrix (num_PUs,
#                         num_PUs_in_optimal_solution,
#                         num_PUs_in_marxan_solution,
#                         frac_spp_covered,
#                         input_err_FP,
#                         input_err_FN)

#-------------------------------------------------------------

    return (list (spp_rep_fracs = spp_rep_fracs,
                  indices_of_spp_with_unmet_rep_frac = indices_of_spp_with_unmet_rep_frac,

                  num_spp_covered = num_spp_covered,
                  frac_spp_covered = frac_spp_covered,
                  spp_rep_shortfall = spp_rep_shortfall,

                    TP = TP,
                    TN = TN,
                    FP = FP,
                    FN = FN,

                    cSe = cSe,
                    cSp = cSp,
                    cPPV = cPPV,
                    cNPV = cNPV,

                    acc_frac = acc_frac,
                    acc_err_frac = acc_err_frac,
                    cost_savings = cost_savings,

                    opt_cost_savings = opt_cost_savings,

                    TSS = TSS,
                    max_cSe_cSp = max_cSe_cSp,
                    min_cSe_cSp = min_cSe_cSp,
                    mean_cSe_cSp = mean_cSe_cSp,
                    prod_cSe_cSp = prod_cSe_cSp,
                    euc_cSe_cSp = euc_cSe_cSp,
                    acc_err_mag = acc_err_mag
                  ))
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

