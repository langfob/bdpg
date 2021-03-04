#===============================================================================

#                           test_gen_single_bdprob_app.R

#===============================================================================

library (bdpg)

context ("gen_single_bdprob_app")

#===============================================================================

# test_gen_single_bdprob_app.R:117: failure: gen_single_bdprob: test_gen_single_bdprob_APP_BASE
# correct_values$cor_dependent_node_IDs not equal to Xu_bdprob_COR_BASE@dependent_node_IDs.
# Lengths differ: 4 is not 1


testsFor_Xu_bdprob_APP_BASE_1 <- function (Xu_bdprob_APP_BASE)
    {
    correct_values = set_correct_values_for_test_1 ()

    expect_false (is.null (Xu_bdprob_APP_BASE))

    expect_equal (6,  Xu_bdprob_APP_BASE@num_PUs)
    expect_equal (10, Xu_bdprob_APP_BASE@num_spp)
    expect_equal (4,  Xu_bdprob_APP_BASE@correct_solution_cost)
    expect_equal ("PU_ID",  Xu_bdprob_APP_BASE@PU_col_name)
    expect_equal ("spp_ID",  Xu_bdprob_APP_BASE@spp_col_name)
    expect_true (Xu_bdprob_APP_BASE@correct_solution_vector_is_known)
    expect_true (Xu_bdprob_APP_BASE@prob_generator_params_known)

    #  Most likely, these 2 are NOT the same after adding error.
    # expect_equal (correct_values$cor_PU_spp_pair_indices,
    #               Xu_bdprob_APP_BASE@PU_spp_pair_indices)

#browser()
    expect_equal (correct_values$cor_dependent_node_IDs,
                  Xu_bdprob_APP_BASE@dependent_node_IDs)

    expect_equal (correct_values$cor_nodes,
                  Xu_bdprob_APP_BASE@nodes)

    expect_equal (correct_values$cor_PU_costs,
                  Xu_bdprob_APP_BASE@PU_costs)
    }

#-----------------------------------

testsFor_Xu_bdprob_APP_WRAP_1 <- function (Xu_bdprob_COR_WRAP)
    {
    correct_values = setup__correct_values_for_wrap_test_1 ()

    expect_false (is.null (Xu_bdprob_COR_WRAP))

    expect_equal (12,  Xu_bdprob_COR_WRAP@num_PUs)
    expect_equal (20, Xu_bdprob_COR_WRAP@num_spp)
    expect_equal (4,  Xu_bdprob_COR_WRAP@correct_solution_cost)
    expect_equal ("PU_ID",  Xu_bdprob_COR_WRAP@PU_col_name)
    expect_equal ("spp_ID",  Xu_bdprob_COR_WRAP@spp_col_name)
    expect_true (Xu_bdprob_COR_WRAP@correct_solution_vector_is_known)
    expect_true (Xu_bdprob_COR_WRAP@prob_generator_params_known)

    expect_equal (correct_values$cor_PU_spp_pair_indices,
                  Xu_bdprob_COR_WRAP@PU_spp_pair_indices)

    expect_equal (correct_values$cor_dependent_node_IDs,
                  Xu_bdprob_COR_WRAP@dependent_node_IDs)

    expect_equal (correct_values$cor_nodes,
                  Xu_bdprob_COR_WRAP@nodes)

    expect_equal (correct_values$cor_PU_costs,
                  Xu_bdprob_COR_WRAP@PU_costs)

    expect_equal (correct_values$cor_bpm,
                  Xu_bdprob_COR_WRAP@bpm)
    }

#-----------------------------------
#  APP base
#-----------------------------------

test_that("gen_single_bdprob: test_gen_single_bdprob_APP_BASE", {

    parameters = init_test_parameters_1_COR_BASE ()

    Xu_bdprob_COR_BASE =
        gen_single_bdprob_COR (parameters,
                               starting_dir          = parameters$starting_dir,  #"~/Downloads",
                               parameters$integerize,
                               base_prob_name_stem   = "base_prob",
                               cor_dir_name_stem     = "cor"
                               )

    #--------------------

    parameters$gen_cost_errors = FALSE
    parameters$gen_FP_FN_errors = TRUE

    parameters$err_amt = 0.10
    parameters$spp_occ_FP_error_type = "CONSTANT"
    parameters$spp_occ_FP_const_rate = 0.1  #0.1

    parameters$spp_occ_FN_error_type = "CONSTANT"
    parameters$spp_occ_FN_const_rate = 0.1  #0.1

    parameters$match_error_counts = FALSE
    parameters$compound_err_name  = "05-FP_and_FN_not_matched_NO_cost_err"

    #----------

    Xu_bdprob_APP_BASE = gen_single_bdprob_APP (Xu_bdprob_COR_BASE,
                                                parameters,
                                                parameters$starting_dir,
                                                parameters$gen_cost_errors,
                                                parameters$gen_FP_FN_errors,
                                                compound_err_name = parameters$compound_err_name,
                                                ret_vals_from_build_const_err = NULL,
                                                ret_vals_from_apply_cost_errors = NULL)

    #--------------------

    testsFor_Xu_bdprob_APP_BASE_1 (Xu_bdprob_APP_BASE)
    })


#===============================================================================
#===============================================================================

#-----------------------------------
#  APP wrap
#-----------------------------------

# test_that("gen_single_bdprob: test_gen_single_bdprob_APP_WRAP", {
#
#     parameters = init_test_parameters_1_COR_BASE ()
#
#     Xu_bdprob_COR_BASE = test_gen_single_bdprob_COR (parameters)
#
#     Xu_bdprob_cor_wrap = gen_single_bdprob_WRAP (Xu_bdprob_COR_BASE,
#                                                  parameters,
#                                                  parameters$starting_dir)
#
#     Xu_bdprob_app_wrap = gen_single_bdprob_APP (Xu_bdprob_cor_wrap,
#                                                 parameters,
#                                                 parameters$starting_dir,
#                                                 gen_cost_errors,
#                                                 gen_FP_FN_errors,
#                                                 compound_err_name = NULL,
#                                                 ret_vals_from_build_const_err = NULL,
#                                                 ret_vals_from_apply_cost_errors = NULL)
#
#     testsFor_Xu_bdprob_app_wrap_1 (Xu_bdprob_cor_wrap)
#
#     })

#===============================================================================

