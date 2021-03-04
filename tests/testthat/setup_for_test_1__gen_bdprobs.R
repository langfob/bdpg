#===============================================================================

#                       setup_for_test_1__gen_bdprobs.R

#===============================================================================

library (bdpg)

#===============================================================================

setup__compute_various_link_cts <- function ()
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

init_test_parameters_1_COR_BASE <- function ()
    {
    parameters  = list (starting_dir                                 = tempdir(),

                        bdpg_run_init_rand_seed                      = 123,

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

#===============================================================================

