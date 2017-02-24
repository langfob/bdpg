#===============================================================================

                #  gscp_10b_compute_solution_rep_levels_and_costs.R

#===============================================================================

#  History:

#  2015 02 18 - BTL - Created.

#===============================================================================

    #  Compute representation match (shortfall or overrep) given a spp rows by
    #  PU columns adjacency matrix.

compute_rep_fraction =
    function (spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU,
              PU_set_to_test,
              spp_rep_targets = 1   #  replace with vector if not all 1s
              )
    {
        #  Reduce the spp/PU adjacency matrix to only include PUs that
        #  are in the set to test (e.g., in the proposed reserve set).
        #  Once that's done, summing each spp row will tell you how much
        #  representation each spp achieves in the proposed solution.

    selected_PUs_matrix_of_spp_cts_per_PU =
        spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU [ , PU_set_to_test, drop=FALSE]
    spp_rep_cts = apply (selected_PUs_matrix_of_spp_cts_per_PU, 1, sum)

            #  2015 04 28 - BTL
            #  Moved this statement out of the debug if statement below.
            #  It looks like it was a bug to have it in there instead of
            #  here before the spp_rep_fracs computation.
            #  I only noticed this today when I started getting the
            #  following warning/error message:
            #       Error in spp_rep_cts - spp_rep_targets :
            #       (converted from warning) longer object length is not
            #       a multiple of shorter object length

#     if (length (spp_rep_targets) == 1)
#         spp_rep_targets = rep (spp_rep_targets,
#                                dim (selected_PUs_matrix_of_spp_cts_per_PU) [1])

    spp_rep_fracs = 1 + ((spp_rep_cts - spp_rep_targets) / spp_rep_targets)

    if (getOption ("bdpg.DEBUG_LEVEL", default=0) > 0)
        {
        if (length (spp_rep_targets) == 1)
            spp_rep_targets = rep (spp_rep_targets,
                                   dim (selected_PUs_matrix_of_spp_cts_per_PU) [1])
        display_matrix =
            cbind (selected_PUs_matrix_of_spp_cts_per_PU, spp_rep_cts, spp_rep_targets, spp_rep_fracs)
        cat ("\nIn compute_rep_fraction():\nselected_PUs_matrix_of_spp_cts_per_PU with cts, targets, and fracs appended = \n")
        print (display_matrix)
        }

doc_vars_in_this_func ()

    return (spp_rep_fracs)
    }

#===============================================================================

    #  Compute cost of given solution vector of PUs to include.

compute_solution_cost =
    function (PU_set_to_test, PU_costs)
    {
    solution_cost <- sum (PU_costs [PU_set_to_test])

doc_vars_in_this_func ()
    return (solution_cost)
    }

#===============================================================================

