#===============================================================================

#                           test_gen_single_bdprob_wrap.R

#===============================================================================

library (bdpg)

context ("gen_single_bdprob_wrap")

#===============================================================================

init_test_parameters_1_cor_wrap <- function ()
    {
    parameters = init_test_parameters_1_cor_base ()

    parameters$wrap_lognormal_dist_around_Xu = TRUE
    parameters$allow_imperfect_wrap = TRUE
    parameters$desired_Xu_spp_frac_of_all_spp = 0.5

    parameters$solution_frac_of_landscape = 0.5    #0.15    #0.3
    parameters$desired_max_abundance_frac = 0.7
    parameters$dep_set_PUs_eligible = FALSE
    parameters$add_one_to_lognormal_abundances = FALSE
    parameters$max_search_iterations = 500
    parameters$plot_rounded_abundances = FALSE    #TRUE

    return (parameters)
    }

#-----------------------------------

setup__correct_values_for_wrap_test_1 <- function ()
    {
    cor_PU_IDs = c(1,2,1,3,2,3,4,5,4,6,
                   5,6,2,6,3,5,3,5,3,5,
                   5,8,5,11,2,9,3,12,6,12,
                   5,12,5,10,8,3,7,9,6,7,
                   12,2,11,10)
    cor_spp_IDs = c(1,1,2,2,3,3,4,4,5,5,
                    6,6,7,7,8,8,9,9,10,10,
                    11,11,12,12,13,13,14,14,15,15,
                    16,16,17,17,17,18,18,18,19,19,
                    19,20,20,20)
    cor_PU_spp_pair_indices = data.frame (PU_ID  = cor_PU_IDs,
                                          spp_ID = cor_spp_IDs)

    cor_dependent_node_IDs = c (2,3,5,6)
    cor_node_IDs = c(1,2,3,4,5,
                     6,7,8,9,10,
                     11,12)
    cor_group_IDs = c(1,1,1,2,2,
                      2,NA,NA,NA,NA,
                      NA,NA)
    cor_dependent_set_members = c(FALSE,TRUE,TRUE,FALSE,TRUE,
                                  TRUE,FALSE,FALSE,FALSE,FALSE,
                                  FALSE,FALSE)
    cor_nodes = data.frame (node_ID  = cor_node_IDs,
                            group_ID = cor_group_IDs,
                            dependent_set_member =
                            cor_dependent_set_members)

    cor_PU_costs = rep (1, 12)

cor_bpm = matrix (c(1,1,0,0,0,0,0,0,0,0,0,0,
                    1,0,1,0,0,0,0,0,0,0,0,0,
                    0,1,1,0,0,0,0,0,0,0,0,0,
                    0,0,0,1,1,0,0,0,0,0,0,0,
                    0,0,0,1,0,1,0,0,0,0,0,0,
                    0,0,0,0,1,1,0,0,0,0,0,0,
                    0,1,0,0,0,1,0,0,0,0,0,0,
                    0,0,1,0,1,0,0,0,0,0,0,0,
                    0,0,1,0,1,0,0,0,0,0,0,0,
                    0,0,1,0,1,0,0,0,0,0,0,0,
                    0,0,0,0,1,0,0,1,0,0,0,0,
                    0,0,0,0,1,0,0,0,0,0,1,0,
                    0,1,0,0,0,0,0,0,1,0,0,0,
                    0,0,1,0,0,0,0,0,0,0,0,1,
                    0,0,0,0,0,1,0,0,0,0,0,1,
                    0,0,0,0,1,0,0,0,0,0,0,1,
                    0,0,0,0,1,0,0,1,0,1,0,0,
                    0,0,1,0,0,0,1,0,1,0,0,0,
                    0,0,0,0,0,1,1,0,0,0,0,1,
                    0,1,0,0,0,0,0,0,0,1,1,0), nrow=20, ncol=12, byrow=TRUE)

    return (list (
                cor_PU_IDs                = cor_PU_IDs,
                cor_spp_IDs               = cor_spp_IDs,
                cor_PU_spp_pair_indices   = cor_PU_spp_pair_indices,
                cor_dependent_node_IDs    = cor_dependent_node_IDs,
                cor_node_IDs              = cor_node_IDs,
                cor_group_IDs             = cor_group_IDs,
                cor_dependent_set_members = cor_dependent_set_members,
                cor_nodes                 = cor_nodes,
                cor_PU_costs              = cor_PU_costs,
                cor_bpm                   = cor_bpm
                ))
    }

#-----------------------------------

testsFor_Xu_bdprob_wrap_1 <- function (Xu_bdprob_cor_wrap)
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

    parameters = init_test_parameters_1_cor_wrap ()
    Xu_bdprob_cor = test_gen_single_bdprob_COR (parameters)

    Xu_bdprob_cor_wrap = gen_single_bdprob_WRAP (Xu_bdprob_cor,
                                                 parameters,
                                                 parameters$starting_dir)

    testsFor_Xu_bdprob_wrap_1 (Xu_bdprob_cor_wrap)

    })

#===============================================================================

