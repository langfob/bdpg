#===============================================================================

#                            test_gen_single_bdprob.R

#===============================================================================

#' @export
#'
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

    print (link_cts_list)
    }

#===============================================================================

#' @export
#'
init_test_parameters <- function ()
    {
    tot_num_nodes = 6
    num_nodes_per_group = 3
    n__num_groups = 2
    num_independent_nodes_per_group = 1
    target_num_links_between_2_groups_per_round = 2
    num_rounds_of_linking_between_groups = 2

    duplicate_links_allowed = TRUE
    max_allowed_num_spp = 20
    integerize = round


    link_cts_list =
        compute_various_link_cts (num_nodes_per_group,
                                  num_independent_nodes_per_group,
                                  n__num_groups,
                                  target_num_links_between_2_groups_per_round,
                                  num_rounds_of_linking_between_groups,
                                  integerize)

    # num_links_within_one_group            = link_cts_list$num_links_within_one_group
    # tot_num_links_inside_groups           = link_cts_list$tot_num_links_inside_groups
    # max_possible_num_links_between_groups = link_cts_list$max_possible_num_links_between_groups
    max_possible_tot_num_links            = link_cts_list$max_possible_tot_num_links
    # max_possible_tot_num_node_link_pairs  = link_cts_list$max_possible_tot_num_node_link_pairs

    parameters = list (

        tot_num_nodes                               = tot_num_nodes,
        num_nodes_per_group                         = num_nodes_per_group,
        n__num_groups                               = n__num_groups,
        num_independent_nodes_per_group             = num_independent_nodes_per_group,
        target_num_links_between_2_groups_per_round = target_num_links_between_2_groups_per_round,
        num_rounds_of_linking_between_groups        = num_rounds_of_linking_between_groups,

        duplicate_links_allowed                     = duplicate_links_allowed,
        max_allowed_num_spp                         = max_allowed_num_spp,
        integerize                                  = integerize,

        max_possible_tot_num_links                  = max_possible_tot_num_links
        )

    return (parameters)
    }

#===============================================================================

#' @export
#'
test_create_Xu_problem_from_scratch_given_params <- function ()
    {
    set.seed (123)

    parameters = init_test_parameters ()

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

#' @export
#'
test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams <- function ()
    {
    set.seed (123)

    parameters = init_test_parameters ()

    PU_spp_pair_info =
        create_Xu_problem_from_scratch_not_using_4_Xu_metaparams (parameters$max_allowed_num_spp,
                                                                  parameters$duplicate_links_allowed,
                                                                  parameters,
                                                                  parameters$integerize)

    return (PU_spp_pair_info)
    }

#===============================================================================

#' @export
#'
test_create_allowable_size_Xu_problem_from_scratch <- function ()
    {
    set.seed (123)

    parameters = init_test_parameters ()
    parameters$dont_derive_prob_params_from_4_Xu_metaparams = TRUE


    PU_spp_pair_info =
        create_allowable_size_Xu_problem_from_scratch (
                                    parameters$max_allowed_num_spp,
                                    parameters,
                                    parameters$integerize,
                                    default_num_prob_size_tries_allowed = 3)

    return (PU_spp_pair_info)
    }

#===============================================================================

#' @export
#'
test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file <- function ()
    {
    parameters = init_test_parameters ()

    set.seed (123)
    # parameters$bdpg_run_init_rand_seed = 123

    parameters$duplicate_links_allowed = TRUE    #  Should fail if you set this to FALSE

    parameters$dont_derive_prob_params_from_4_Xu_metaparams = TRUE
    parameters$read_Xu_problem_from_Xu_file                 = FALSE
    parameters$Xu_bench_infile_name                         = NULL
    parameters$given_correct_solution_cost                  = TRUE

    exp_root_dir = "~/Downloads"

    Xu_bdprob_cor =
        gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (
                                        exp_root_dir,
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
test_gen_single_bdprob <- function ()
    {
    test_compute_various_link_cts ()
    test_create_Xu_problem_from_scratch_given_params ()
    test_create_Xu_problem_from_scratch_not_using_4_Xu_metaparams ()
    test_create_allowable_size_Xu_problem_from_scratch ()
    test_gen_single_bdprob_COR_from_scratch_or_Xu_bench_file()
    }

#===============================================================================

