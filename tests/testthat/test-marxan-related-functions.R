#===============================================================================

                    #  test-marxan-related-functions.R

#===============================================================================

    #  Cut from gscp_14_read_marxan_output_files.R
    #  2017 02 15 - BTL
test_get_marxan_solution_choice_string <- function ()
    {
    get_marxan_solution_choice_string (10, 1, 10, 1)    #  OK_marxan_solution_IS_apparent_best
    get_marxan_solution_choice_string (20, 1, 10, 1)    #  BAD_HALF_marxan_solution_NOT_apparent_best_cost_and_IS_apparent_best_rep
    get_marxan_solution_choice_string (10, 0.8, 10, 1)  #  BAD_HALF_marxan_solution_IS_apparent_best_cost_and_NOT_apparent_best_rep
    get_marxan_solution_choice_string (11, 0.5, 10, 1)  #  BAD_marxan_solution_NEITHER_best
    }

#===============================================================================

