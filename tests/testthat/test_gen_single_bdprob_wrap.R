#===============================================================================

#                           test_gen_single_bdprob_wrap.R

#===============================================================================

library (bdpg)

context ("gen_single_bdprob_wrap")

#===============================================================================

testsFor_Xu_bdprob_COR_WRAP_1 <- function (Xu_bdprob_cor_wrap)
    {
    correct_values = setup__correct_values_for_wrap_test_1 ()

    expect_false (is.null (Xu_bdprob_cor_wrap))

    expect_equal (12,  Xu_bdprob_cor_wrap@num_PUs)
    expect_equal (20, Xu_bdprob_cor_wrap@num_spp)
    expect_equal (4,  Xu_bdprob_cor_wrap@correct_solution_cost)
    expect_equal ("PU_ID",  Xu_bdprob_cor_wrap@PU_col_name)
    expect_equal ("spp_ID",  Xu_bdprob_cor_wrap@spp_col_name)
    expect_true (Xu_bdprob_cor_wrap@correct_solution_vector_is_known)
    expect_true (Xu_bdprob_cor_wrap@prob_generator_params_known)

    expect_equal (correct_values$cor_PU_spp_pair_indices,
                  Xu_bdprob_cor_wrap@PU_spp_pair_indices)

    expect_equal (correct_values$cor_dependent_node_IDs,
                  Xu_bdprob_cor_wrap@dependent_node_IDs)

    expect_equal (correct_values$cor_nodes,
                  Xu_bdprob_cor_wrap@nodes)

    expect_equal (correct_values$cor_PU_costs,
                  Xu_bdprob_cor_wrap@PU_costs)

    expect_equal (correct_values$cor_bpm,
                  Xu_bdprob_cor_wrap@bpm)
    }

#-----------------------------------

test_that("gen_single_bdprob: test_gen_single_bdprob_COR_WRAP", {

    parameters = init_test_parameters_1_COR_WRAP ()
    # Xu_bdprob_cor = test_gen_single_bdprob_cor (parameters)
    Xu_bdprob_COR_BASE =
        gen_single_bdprob_COR (parameters,
                               starting_dir          = parameters$starting_dir,  #"~/Downloads",
                               parameters$integerize,
                               base_prob_name_stem   = "base_prob",
                               cor_dir_name_stem     = "cor"
                               )

    Xu_bdprob_COR_WRAP = gen_single_bdprob_WRAP (Xu_bdprob_COR_BASE,
                                                 parameters,
                                                 parameters$starting_dir)

    testsFor_Xu_bdprob_COR_WRAP_1 (Xu_bdprob_COR_WRAP)

    })

#===============================================================================

