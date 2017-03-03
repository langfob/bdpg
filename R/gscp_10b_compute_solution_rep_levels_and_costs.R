#===============================================================================

                #  gscp_10b_compute_solution_rep_levels_and_costs.R

#===============================================================================

#  History:

#  2015 02 18 - BTL - Created.

#===============================================================================

    #  Compute representation match (shortfall or overrep) given a spp rows by
    #  PU columns adjacency matrix.

#' Compute species representation fractions achieved
#'
#' Compute fractions of species representation targets achieved by a given
#' set of planning units.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{PU_set_to_test}{
#' \preformatted{
#' PU_set_to_test :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{selected_PUs_matrix_of_spp_cts_per_PU}{
#' \preformatted{
#' selected_PUs_matrix_of_spp_cts_per_PU :  num [1:814, 1:61] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{spp_rep_cts}{
#' \preformatted{
#' spp_rep_cts :  num [1:814] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{spp_rep_fracs}{
#' \preformatted{
#' spp_rep_fracs :  num [1:814] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{spp_rep_targets}{
#' \preformatted{
#' spp_rep_targets :  num [1:814] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU}{
#' \preformatted{
#' spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU :  num [1:814, 1:122] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns spp_rep_fracs numeric vector of fractions of species
#'     representation targets achieved by a given set of planning units

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

#docaids::doc_vars_in_this_func_once ()
    return (spp_rep_fracs)
    }

#===============================================================================

#' Compute cost of given solution vector of PUs to include
#'
#' Given a set of planning units to include in a solution, compute the total
#' cost for that set.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{PU_costs}{
#' \preformatted{
#' PU_costs :  num [1:122] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{PU_set_to_test}{
#' \preformatted{
#' PU_set_to_test :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{solution_cost}{
#' \preformatted{
#' solution_cost :  num 61
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns numeric cost of the given solution

compute_solution_cost =
    function (PU_set_to_test, PU_costs)
    {
    solution_cost <- sum (PU_costs [PU_set_to_test])

#docaids::doc_vars_in_this_func_once ()
    return (solution_cost)
    }

#===============================================================================

