#===============================================================================

#                            test_gen_single_bdprob.R

#===============================================================================

library (bdpg)

context ("gen_single_bdprob")

#===============================================================================

test_that("gen_single_bdprob: compute_various_link_cts", {

    link_cts_list = setup__compute_various_link_cts ()

    expect_false (is.null (link_cts_list))

    expect_equal (3,  link_cts_list$num_links_within_one_group)
    expect_equal (6,  link_cts_list$tot_num_links_inside_groups)
    expect_equal (4,  link_cts_list$max_possible_num_links_between_groups)
    expect_equal (10, link_cts_list$max_possible_tot_num_links)
    expect_equal (20, link_cts_list$max_possible_tot_num_node_link_pairs)
    })

#-----------------------------------

testsFor_PU_spp_pair_info_1 <- function (PU_spp_pair_info)
    {
    correct_values = set_correct_values_for_test_1 ()

    expect_false (is.null (PU_spp_pair_info))

    expect_equal (6,  PU_spp_pair_info@num_PUs)
    expect_equal (10, PU_spp_pair_info@num_spp)
    expect_equal (4,  PU_spp_pair_info@correct_solution_cost)
    expect_equal ("PU_ID",  PU_spp_pair_info@PU_col_name)
    expect_equal ("spp_ID",  PU_spp_pair_info@spp_col_name)
    expect_true (PU_spp_pair_info@correct_solution_vector_is_known)
    expect_true (PU_spp_pair_info@prob_generator_params_known)

    expect_equal (correct_values$cor_PU_spp_pair_indices,
                  PU_spp_pair_info@PU_spp_pair_indices)

    expect_equal (correct_values$cor_dependent_node_IDs,
                  PU_spp_pair_info@dependent_node_IDs)

    expect_equal (correct_values$cor_nodes,
                  PU_spp_pair_info@nodes)

    expect_equal (correct_values$cor_PU_costs,
                  PU_spp_pair_info@PU_costs)
    }

#-----------------------------------

testsFor_Xu_bdprob_COR_BASE_1 <- function (Xu_bdprob_cor)
    {
    correct_values = set_correct_values_for_test_1 ()

    expect_false (is.null (Xu_bdprob_cor))

    expect_equal (6,  Xu_bdprob_cor@num_PUs)
    expect_equal (10, Xu_bdprob_cor@num_spp)
    expect_equal (4,  Xu_bdprob_cor@correct_solution_cost)
    expect_equal ("PU_ID",  Xu_bdprob_cor@PU_col_name)
    expect_equal ("spp_ID",  Xu_bdprob_cor@spp_col_name)
    expect_true (Xu_bdprob_cor@correct_solution_vector_is_known)
    expect_true (Xu_bdprob_cor@prob_generator_params_known)

    expect_equal (correct_values$cor_PU_spp_pair_indices,
                  Xu_bdprob_cor@PU_spp_pair_indices)

    expect_equal (correct_values$cor_dependent_node_IDs,
                  Xu_bdprob_cor@dependent_node_IDs)

    expect_equal (correct_values$cor_nodes,
                  Xu_bdprob_cor@nodes)

    expect_equal (correct_values$cor_PU_costs,
                  Xu_bdprob_cor@PU_costs)
    }

#-----------------------------------
#-----------------------------------

test_that("gen_single_bdprob: test_create_Xu_problem_from_scratch_given_params", {

    parameters = init_test_parameters_1_COR_BASE ()
    # PU_spp_pair_info = test_create_Xu_problem_from_scratch_given_params (parameters)
    PU_spp_pair_info =
        create_Xu_problem_from_scratch_given_params (
            parameters$tot_num_nodes,
            parameters$num_nodes_per_group,
            parameters$n__num_groups,
            parameters$num_independent_nodes_per_group,
            parameters$max_possible_tot_num_links,
            parameters$target_num_links_between_2_groups_per_round,
            parameters$num_rounds_of_linking_between_groups,
            parameters$duplicate_links_allowed)

    testsFor_PU_spp_pair_info_1 (PU_spp_pair_info)
    })

#-----------------------------------

test_that("gen_single_bdprob: test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams", {

    parameters = init_test_parameters_1_COR_BASE ()
    # PU_spp_pair_info = test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams (parameters)
    PU_spp_pair_info =
        create_Xu_problem_from_scratch_not_using_4_Xu_metaparams (parameters$max_allowed_num_spp,
                                                                  parameters$duplicate_links_allowed,
                                                                  parameters,
                                                                  parameters$integerize)

    testsFor_PU_spp_pair_info_1 (PU_spp_pair_info)
    })

#-----------------------------------

test_that("gen_single_bdprob: test_create_allowable_size_Xu_problem_from_scratch", {

    parameters = init_test_parameters_1_COR_BASE ()
    # PU_spp_pair_info = test_create_allowable_size_Xu_problem_from_scratch (parameters)
    PU_spp_pair_info =
        create_allowable_size_Xu_problem_from_scratch (
                                    parameters$max_allowed_num_spp,
                                    parameters,
                                    parameters$integerize,
                                    default_num_prob_size_tries_allowed = 3)

    testsFor_PU_spp_pair_info_1 (PU_spp_pair_info)
    })

#-----------------------------------

test_that("gen_single_bdprob: test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file", {

    parameters = init_test_parameters_1_COR_BASE ()
    # Xu_bdprob_cor = test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (parameters)
    Xu_bdprob_cor_base =
        gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (
                                        exp_root_dir = parameters$starting_dir,  #"~/Downloads",
                                        parameters,
                                        parameters$given_correct_solution_cost,
                                        parameters$max_allowed_num_spp,
                                        parameters$integerize,
                                        base_prob_name_stem = "base_prob",
                                        cor_dir_name_stem = "cor")

    testsFor_Xu_bdprob_COR_BASE_1 (Xu_bdprob_cor_base)
    })

#-----------------------------------

test_that("gen_single_bdprob: test_gen_single_bdprob_COR_BASE", {

    parameters = init_test_parameters_1_COR_BASE ()
    # Xu_bdprob_cor = test_gen_single_bdprob_COR (parameters)
    Xu_bdprob_cor_base =
        gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (
                                        exp_root_dir = parameters$starting_dir,  #"~/Downloads",
                                        parameters,
                                        parameters$given_correct_solution_cost,
                                        parameters$max_allowed_num_spp,
                                        parameters$integerize,
                                        base_prob_name_stem = "base_prob",
                                        cor_dir_name_stem = "cor")

    testsFor_Xu_bdprob_COR_BASE_1 (Xu_bdprob_cor_base)
    })

#===============================================================================

