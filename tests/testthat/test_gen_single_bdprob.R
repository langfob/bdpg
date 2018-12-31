#===============================================================================

#                            test_gen_single_bdprob.R

#===============================================================================

library (bdpg)

context ("gen_single_bdprob")

#===============================================================================

test_compute_various_link_cts <- function ()
    {
    set.seed (123)

    num_nodes_per_group                         = 3
    n__num_groups                               = 2
    num_independent_nodes_per_group             = 1
    target_num_links_between_2_groups_per_round = 2
    num_rounds_of_linking_between_groups        = 2
    integerize                                  = round

    link_cts_list =
        compute_various_link_cts (num_nodes_per_group,
                                  num_independent_nodes_per_group,
                                  n__num_groups,
                                  target_num_links_between_2_groups_per_round,
                                  num_rounds_of_linking_between_groups,
                                  integerize)

    num_links_within_one_group            = link_cts_list$num_links_within_one_group
    tot_num_links_inside_groups           = link_cts_list$tot_num_links_inside_groups
    max_possible_num_links_between_groups = link_cts_list$max_possible_num_links_between_groups
    max_possible_tot_num_links            = link_cts_list$max_possible_tot_num_links
    max_possible_tot_num_node_link_pairs  = link_cts_list$max_possible_tot_num_node_link_pairs

    return (link_cts_list)
    }

#===============================================================================

init_test_parameters_1 <- function ()
    {
    parameters  = list (bdpg_run_init_rand_seed                      = 123,

                        tot_num_nodes                                = 6,

                        num_nodes_per_group                          = 3,
                        n__num_groups                                = 2,
                        num_independent_nodes_per_group              = 1,
                        target_num_links_between_2_groups_per_round  = 2,
                        num_rounds_of_linking_between_groups         = 2,

                        max_allowed_num_spp                          = 20,
                        integerize                                   = round,

                        duplicate_links_allowed                      = TRUE,  #  Should fail if you set this to FALSE

                        dont_derive_prob_params_from_4_Xu_metaparams = TRUE,
                        read_Xu_problem_from_Xu_file                 = FALSE,
                        Xu_bench_infile_name                         = NULL,
                        given_correct_solution_cost                  = TRUE
                        )

    #----------

    link_cts_list =
        compute_various_link_cts (parameters$num_nodes_per_group,
                                  parameters$num_independent_nodes_per_group,
                                  parameters$n__num_groups,
                                  parameters$target_num_links_between_2_groups_per_round,
                                  parameters$num_rounds_of_linking_between_groups,
                                  parameters$integerize)

    # num_links_within_one_group            = link_cts_list$num_links_within_one_group
    # tot_num_links_inside_groups           = link_cts_list$tot_num_links_inside_groups
    # max_possible_num_links_between_groups = link_cts_list$max_possible_num_links_between_groups
    max_possible_tot_num_links            = link_cts_list$max_possible_tot_num_links
    # max_possible_tot_num_node_link_pairs  = link_cts_list$max_possible_tot_num_node_link_pairs

    parameters$max_possible_tot_num_links = max_possible_tot_num_links

    #----------

    set.seed (parameters$bdpg_run_init_rand_seed)

    #----------

    return (parameters)
    }

#===============================================================================

test_create_Xu_problem_from_scratch_given_params <- function (parameters)
    {
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

#print (PU_spp_pair_info)

    return (PU_spp_pair_info)
    }

#===============================================================================

test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams <- function (parameters)
    {
    PU_spp_pair_info =
        create_Xu_problem_from_scratch_not_using_4_Xu_metaparams (parameters$max_allowed_num_spp,
                                                                  parameters$duplicate_links_allowed,
                                                                  parameters,
                                                                  parameters$integerize)

    return (PU_spp_pair_info)
    }

#===============================================================================

test_create_allowable_size_Xu_problem_from_scratch <- function (parameters)
    {
    PU_spp_pair_info =
        create_allowable_size_Xu_problem_from_scratch (
                                    parameters$max_allowed_num_spp,
                                    parameters,
                                    parameters$integerize,
                                    default_num_prob_size_tries_allowed = 3)

    return (PU_spp_pair_info)
    }

#===============================================================================

test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file <- function (parameters)
    {
    Xu_bdprob_cor =
        gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (
                                        exp_root_dir = "~/Downloads",
                                        parameters,
                                        parameters$given_correct_solution_cost,
                                        parameters$max_allowed_num_spp,
                                        parameters$integerize,
                                        base_prob_name_stem = "base_prob",
                                        cor_dir_name_stem = "cor")

    return (Xu_bdprob_cor)
    }

#===============================================================================

#' @export
#'
test_gen_single_bdprob_COR <- function (parameters)
    {
    Xu_bdprob_cor =
        gen_single_bdprob_COR (parameters,
                               starting_dir          = "~/Downloads",
                               parameters$integerize,
                               base_prob_name_stem   = "base_prob",
                               cor_dir_name_stem     = "cor"
                               )

    return (Xu_bdprob_cor)
    }

#===============================================================================

test_that("gen_single_bdprob: compute_various_link_cts", {

    link_cts_list = test_compute_various_link_cts ()

    expect_false (is.null (link_cts_list))

    expect_equal (3,  link_cts_list$num_links_within_one_group)
    expect_equal (6,  link_cts_list$tot_num_links_inside_groups)
    expect_equal (4,  link_cts_list$max_possible_num_links_between_groups)
    expect_equal (10, link_cts_list$max_possible_tot_num_links)
    expect_equal (20, link_cts_list$max_possible_tot_num_node_link_pairs)
    })

#-----------------------------------

set_correct_values_for_test_1 <- function ()
    {
    cor_PU_IDs = c(1,2,1,3,2,3,4,5,4,6,5,6,2,6,3,5,3,5,3,5)
    cor_spp_IDs = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)
    cor_PU_spp_pair_indices = data.frame (PU_ID  = cor_PU_IDs,
                                          spp_ID = cor_spp_IDs)

    cor_dependent_node_IDs = c (2,3,5,6)
    cor_node_IDs = c(1,2,3,4,5,6)
    cor_group_IDs = c(1,1,1,2,2,2)
    cor_dependent_set_members = c(FALSE, TRUE, TRUE,
                                  FALSE, TRUE, TRUE)
    cor_nodes = data.frame (node_ID  = cor_node_IDs,
                            group_ID = cor_group_IDs,
                            dependent_set_member =
                            cor_dependent_set_members)

    cor_PU_costs = rep (1, 6)

    return (list (
                cor_PU_IDs = cor_PU_IDs,
                cor_spp_IDs = cor_spp_IDs,
                cor_PU_spp_pair_indices = cor_PU_spp_pair_indices,
                cor_dependent_node_IDs = cor_dependent_node_IDs,
                cor_node_IDs = cor_node_IDs,
                cor_group_IDs = cor_group_IDs,
                cor_dependent_set_members = cor_dependent_set_members,
                cor_nodes = cor_nodes,
                cor_PU_costs = cor_PU_costs
                ))
    }

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

testsFor_Xu_bdprob_cor_1 <- function (Xu_bdprob_cor)
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

    parameters = init_test_parameters_1 ()
    PU_spp_pair_info = test_create_Xu_problem_from_scratch_given_params (parameters)

    testsFor_PU_spp_pair_info_1 (PU_spp_pair_info)

    })

#-----------------------------------

test_that("gen_single_bdprob: test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams", {

    parameters = init_test_parameters_1 ()
    PU_spp_pair_info = test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams (parameters)

    testsFor_PU_spp_pair_info_1 (PU_spp_pair_info)

    })

#-----------------------------------

test_that("gen_single_bdprob: test_create_allowable_size_Xu_problem_from_scratch", {

    parameters = init_test_parameters_1 ()
    PU_spp_pair_info = test_create_allowable_size_Xu_problem_from_scratch (parameters)

    testsFor_PU_spp_pair_info_1 (PU_spp_pair_info)

    })

#-----------------------------------

test_that("gen_single_bdprob: test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file", {

    parameters = init_test_parameters_1 ()
    Xu_bdprob_cor = test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (parameters)

    testsFor_Xu_bdprob_cor_1 (Xu_bdprob_cor)

    })

#-----------------------------------

test_that("gen_single_bdprob: test_gen_single_bdprob_COR", {

    parameters = init_test_parameters_1 ()
    Xu_bdprob_cor = test_gen_single_bdprob_COR (parameters)

    testsFor_Xu_bdprob_cor_1 (Xu_bdprob_cor)

    })

#===============================================================================

# test_gen_single_bdprob <- function ()
#     {
#     parameters = init_test_parameters_1 ()
#
#     test_compute_various_link_cts ()
#     test_create_Xu_problem_from_scratch_given_params (parameters)
#     test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams (parameters)
#     test_create_allowable_size_Xu_problem_from_scratch (parameters)
#     test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (parameters)
#     test_gen_single_bdprob_COR (parameters)
#     }

#===============================================================================

