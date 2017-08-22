#===============================================================================

            #  gscp_11aa_write_abbreviated_results_to_files.R

#===============================================================================

write_abbreviated_results_to_files =
            function (cur_result_row,
                      parameters,
                      num_PUs,
                      num_spp,
                      n__num_groups,
                      alpha__,
                      p__prop_of_links_between_groups,
                      r__density,
                      num_nodes_per_group,
                      tot_num_nodes,
                      num_independent_set_nodes,
                      num_dependent_set_nodes,
                      num_rounds_of_linking_between_groups,
                      target_num_links_between_2_groups_per_round,
                      num_links_within_one_group,
                      tot_num_links_inside_groups,
                      max_possible_num_links_between_groups,
                      max_possible_tot_num_links)
    {
    all_output_col_names =
        c (
            "runset_abbrev",
            "run_ID",

            "exceeded_thresh_for_num_spp",

                #  Derived network parameters (after running Xu algorithm)
            "num_PUs",
            "num_spp",
            "num_spp_per_PU",

            "seed",

                #  Xu input parameters
            "n__num_groups",
            "alpha__",
            "p__prop_of_links_between_groups",
            "r__density",

                #  Correct answers
            "opt_solution_as_frac_of_tot_num_nodes",
            "cor_num_patches",

                #  Marxan results
            "marxan_best_num_patches",
            "abs_marxan_best_solution_cost_err_frac",
            "marxan_best_solution_cost_err_frac",
            "spp_rep_shortfall",
            "marxan_best_solution_NUM_spp_covered",
            "marxan_best_solution_FRAC_spp_covered",

                #  Derived input parameters (before running Xu algorithm)
            "num_nodes_per_group",
            "tot_num_nodes",
            "num_independent_set_nodes",
            "num_dependent_set_nodes",
            "num_rounds_of_linking_between_groups",
            "target_num_links_between_2_groups_per_round",
            "num_links_within_one_group",
            "tot_num_links_inside_groups",
            "max_possible_num_links_between_groups",
            "max_possible_tot_num_links",

                #  Marxan input parameters
            "marxan_spf_const",
            "marxan_PROP",
            "marxan_RANDSEED",
            "marxan_NUMREPS",
            "marxan_NUMITNS",
            "marxan_STARTTEMP",
            "marxan_NUMTEMP",
            "marxan_COSTTHRESH",
            "marxan_THRESHPEN1",
            "marxan_THRESHPEN2",
            "marxan_RUNMODE",
            "marxan_MISSLEVEL",
            "marxan_ITIMPTYPE",
            "marxan_HEURTYPE",
            "marxan_CLUMPTYPE",

            "runset_name",

                #  Bipartite measures
            "connectance",
            "web_asymmetry",
            "links_per_species",
            "number_of_compartments",
            "compartment_diversity",
            "cluster_coefficient",
            "nestedness",
            "weighted_nestedness",
            "weighted_NODF",
            "interaction_strength_asymmetry",
            "specialisation_asymmetry",
            "linkage_density",
            "weighted_connectance",
            "Fisher_alpha",
            "Shannon_diversity",
            "interaction_evenness",
            "Alatalo_interaction_evenness",
            "H2","number.of.species.HL",
            "number.of.species.LL",
            "mean.number.of.shared.partners.HL",
            "mean.number.of.shared.partners.LL",
            "cluster.coefficient.HL",
            "cluster.coefficient.LL",
            "weighted.cluster.coefficient.HL",
            "weighted.cluster.coefficient.LL",
            "niche.overlap.HL",
            "niche.overlap.LL",
            "togetherness.HL",
            "togetherness.LL",
            "C.score.HL",
            "C.score.LL",
            "V.ratio.HL",
            "V.ratio.LL",
            "discrepancy.HL",
            "discrepancy.LL",
            "extinction.slope.HL",
            "extinction.slope.LL",
            "robustness.HL",
            "robustness.LL",
            "functional.complementarity.HL",
            "functional.complementarity.LL",
            "partner.diversity.HL",
            "partner.diversity.LL",
            "generality.HL",
            "vulnerability.LL",

                #  igraph Latapy measures
            "ig_top",
            "ig_bottom",
            "ig_num_edges_m",
            "ig_ktop",
            "ig_kbottom",
            "ig_bidens",
            "ig_lcctop",
            "ig_lccbottom",
            "ig_distop",
            "ig_disbottom",
            "ig_cctop",
            "ig_ccbottom",
            "ig_cclowdottop",
            "ig_cclowdotbottom",
            "ig_cctopdottop",
            "ig_cctopdotbottom",
            "ig_mean_bottom_bg_redundancy",
            "ig_median_bottom_bg_redundancy",
            "ig_mean_top_bg_redundancy",
            "ig_median_top_bg_redundancy"
            )


    all_output_initialized_cols = rep (NA, length (all_output_col_names))

    initialized_output_df = as.data.frame (t (all_output_initialized_cols))
    names (initialized_output_df) = all_output_col_names

    initialized_output_df$runset_abbrev [cur_result_row]                                    = parameters$runset_name    #  parameters$runset_abbrev

    initialized_output_df$exceeded_thresh_for_num_spp                                       = TRUE

    initialized_output_df$num_PUs [cur_result_row]                                          = num_PUs
    initialized_output_df$num_spp [cur_result_row]                                          = num_spp
    initialized_output_df$num_spp_per_PU [cur_result_row]                                   = num_spp / num_PUs
    initialized_output_df$seed [cur_result_row]                                             = parameters$seed

        #  Xu options
    initialized_output_df$n__num_groups [cur_result_row]                                   = n__num_groups
    initialized_output_df$alpha__ [cur_result_row]                                          = alpha__
    initialized_output_df$p__prop_of_links_between_groups [cur_result_row]                 = p__prop_of_links_between_groups
    initialized_output_df$r__density [cur_result_row]                                       = r__density

        #  Derived Xu options
    initialized_output_df$num_nodes_per_group [cur_result_row]                             = num_nodes_per_group
    initialized_output_df$tot_num_nodes [cur_result_row]                                    = tot_num_nodes
    initialized_output_df$num_independent_set_nodes [cur_result_row]                        = num_independent_set_nodes
    initialized_output_df$num_dependent_set_nodes [cur_result_row]                          = num_dependent_set_nodes
    initialized_output_df$num_rounds_of_linking_between_groups [cur_result_row]            = num_rounds_of_linking_between_groups
    initialized_output_df$target_num_links_between_2_groups_per_round [cur_result_row]     = target_num_links_between_2_groups_per_round
    initialized_output_df$num_links_within_one_group [cur_result_row]                      = num_links_within_one_group
    initialized_output_df$tot_num_links_inside_groups [cur_result_row]                     = tot_num_links_inside_groups
    initialized_output_df$max_possible_num_links_between_groups [cur_result_row]           = max_possible_num_links_between_groups
    initialized_output_df$max_possible_tot_num_links [cur_result_row]                       = max_possible_tot_num_links

        #  Full runset name
    initialized_output_df$runset_name [cur_result_row]                                      = parameters$runset_name

        #  Write data frame to file.
    write_results_to_files ("abbreviated_results.csv",
                            initialized_output_df,
                            parameters,
                            get_RSprob_path_topdir (),
                            "rsp_tzar_run_ID"
                            )
    }

#===============================================================================



