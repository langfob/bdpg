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

init_test_parameters <- function ()
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

test_gen_single_bdprob <- function ()
    {
    parameters = init_test_parameters ()

    test_compute_various_link_cts ()
    test_create_Xu_problem_from_scratch_given_params (parameters)
    test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams (parameters)
    test_create_allowable_size_Xu_problem_from_scratch (parameters)
    test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (parameters)
    test_gen_single_bdprob_COR (parameters)
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

test_that("gen_single_bdprob: test_create_Xu_problem_from_scratch_given_params", {

    parameters = init_test_parameters ()
    PU_spp_pair_info = test_create_Xu_problem_from_scratch_given_params (parameters)

    expect_false (is.null (PU_spp_pair_info))

    # expect_equal (3,  PU_spp_pair_info$num_links_within_one_group)
    # expect_equal (6,  PU_spp_pair_info$tot_num_links_inside_groups)
    # expect_equal (4,  PU_spp_pair_info$max_possible_num_links_between_groups)
    # expect_equal (10, PU_spp_pair_info$max_possible_tot_num_links)
    # expect_equal (20, PU_spp_pair_info$max_possible_tot_num_node_link_pairs)
    })

#-----------------------------------

test_that("gen_single_bdprob: duplicate link failure", {

    parameters = init_test_parameters ()
    parameters$duplicate_links_allowed = FALSE

    expect_error (test_create_Xu_problem_from_scratch_given_params (parameters),
                   "Can't set both use_given_time_as_limit and use_marxan_time_as_limit to TRUE.",
                  fixed=TRUE)
    })

#===============================================================================

