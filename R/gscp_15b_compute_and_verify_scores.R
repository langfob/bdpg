#===============================================================================

                #  gscp_15b_compute_and_verify_scores.R

#===============================================================================

# compute_and_verify_APP_rep_scores_according_to_RS <-
#     function (rs_solution, num_spp, bpm, spp_rep_targets)
#     {
#     app_solution_NUM_spp_covered__fromRS =
#         compute_num_spp_covered_by_solution (rs_solution,
#                                              bpm,
#                                              spp_rep_targets)
#
#     app_solution_FRAC_spp_covered__fromRS = app_solution_NUM_spp_covered__fromRS / num_spp
#     app_spp_rep_shortfall__fromRS = 1 - app_solution_FRAC_spp_covered__fromRS
#
#     return (list (rsr_app_spp_rep_shortfall__fromRS         = app_spp_rep_shortfall__fromRS,
#                   rsr_app_solution_NUM_spp_covered__fromRS  = app_solution_NUM_spp_covered__fromRS,
#                   rsr_app_solution_FRAC_spp_covered__fromRS = app_solution_FRAC_spp_covered__fromRS))
#     }

#===============================================================================

compute_num_spp_covered_by_solution <- function (rs_solution,
                                                 bpm,
                                                 spp_rep_targets)
    {
        #  Reduce the spp/PU adjacency matrix to only include PUs that
        #  are in the set to test (e.g., in the proposed reserve set).
        #  Once that's done, summing each spp row will tell you how much
        #  representation each spp achieves in the proposed solution.

    spp_reps_in_sol = rowSums (bpm [, rs_solution, drop=FALSE])
    num_spp_covered = length (which (spp_reps_in_sol >= spp_rep_targets))

    return (num_spp_covered)
    }

#===============================================================================

