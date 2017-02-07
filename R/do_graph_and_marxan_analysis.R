#===============================================================================
#
#                       do_graph_and_marxan_analysis.R
#
#   Run code to compute graph metrics and to run marxan and dump results
#   to file.
#
#===============================================================================

        #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
        #                       reading Xu file, e.g., prob_generator_params_known.
        #                       This is because learning alg downstream needs to
        #                       know things like whether the generator's params
        #                       are even known, so that it doesn't try to learn
        #                       something from missing data.

do_graph_and_marxan_analysis <- function (parameters,
                                                #  parameters from gen prob.
                                                #  They're only used in creating
                                                #  the master output structure.
                                          Xu_parameters,
                                          read_Xu_problem_from_Xu_file,

                                                #  From bdprob structure, i.e.,
                                                #  results of gen prob routine
                                          cor_num_spp,
                                          cor_num_PUs,
                                          cor_PU_spp_pair_indices,
                                          cor_PU_IDs, #####!!!!!#####
                                          cor_spp_IDs,  #####!!!!!#####
                                          cor_bpm,

                                          cor_PU_costs,
                                          cor_optimum_cost,
                                          cor_nodes,
                                          spp_col_name,
                                          PU_col_name,

                                          presences_col_name, #  hard-coded as "freq"

                                                #  Results of adding error.
                                          app_num_spp,
                                          app_num_PUs,
                                          app_PU_spp_pair_indices,
                                          app_bpm,

                                                #  input parameters for error model.
                                                #  Only used to create master output structure?
                                          add_error,
                                          match_error_counts,
                                          FP_const_rate,
                                          FN_const_rate,
                                          original_FP_const_rate,
                                          original_FN_const_rate,



                        #  THESE MARXAN DIRECTORIES NEED TO BE CREATED AND
                        #  STORED USING THE WAY OF ALLOCATING DIRECTORIES
                        #  TO A RESERVE SELECTOR.

                                          derived_bdpg_dir_names
                                         )
{
#===============================================================================
#                                   Run marxan.
#===============================================================================

    marxan_control_values =
        set_up_for_and_run_marxan (app_PU_spp_pair_indices,
                                     cor_PU_IDs, #####!!!!!#####
                                     cor_spp_IDs,  #####!!!!!#####

                                     PU_col_name,
                                     spp_col_name,

                        #  THESE MARXAN DIRECTORIES NEED TO BE CREATED AND
                        #  STORED USING THE WAY OF ALLOCATING DIRECTORIES
                        #  TO A RESERVE SELECTOR.

                                     derived_bdpg_dir_names$marxan_input_dir,
                                     derived_bdpg_dir_names$marxan_output_dir,
                                     derived_bdpg_dir_names$marxan_IO_dir,

                                     parameters
                                    )

    cat("\njust after set_up_for_and_run_marxan()")

  #---------------------------------------------------------------------------

    app_marxan_output_values =
        read_marxan_output_files (

                        #  THESE MARXAN DIRECTORIES NEED TO BE CREATED AND
                        #  STORED USING THE WAY OF ALLOCATING DIRECTORIES
                        #  TO A RESERVE SELECTOR.

                                        #  This directory may need to change if there is more than
                                        #  one run of marxan, i.e., a cor_ and an app_.
                                    derived_bdpg_dir_names$marxan_output_dir,





                                    cor_PU_IDs,    #####!!!!!#####all_correct_node_IDs,

                                        #  These used to say just num_spp and num_PUs.
                                        #  For the moment, I'm going to make them cor_...
                                        #  since there is neither num_spp nor app_num_spp, etc.
                                        #  This needs fixing though.
                                                #num_PUs,  #  cor_num_PUs?  app_num_PUs?
                                                #num_spp,  #  cor_num_spp?  app_num_spp?
                                    cor_num_PUs,  #  cor_num_PUs?  app_num_PUs?
                                    cor_num_spp,  #  cor_num_spp?  app_num_spp?

                                    cor_bpm,
                                    derived_bdpg_dir_names$plot_output_dir,
                                    parameters,
                                    app_bpm,

                                    cor_PU_costs,  #  May also need to pass cor_ and app_PU_costs.
                                    cor_optimum_cost,
                                    cor_optimum_cost  #  Should probably be app_optimum_cost, but doesn't exist at the moment so just passing cor_ to get it to run.
                                   )

    cat("\njust after read_marxan_output_files()")

#===============================================================================
#                   Dump all of the different kinds of results.
#===============================================================================

        #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
        #                       reading Xu file, e.g., prob_generator_params_known.
        #                       This is because learning alg downstream needs to
        #                       know things like whether the generator's params
        #                       are even known, so that it doesn't try to learn
        #                       something from missing data.

    create_master_output_structure (read_Xu_problem_from_Xu_file,
                                    Xu_parameters,

                                        #  These used to say just num_spp and num_PUs.
                                        #  For the moment, I'm going to make them cor_...
                                        #  since there is neither num_spp nor app_num_spp, etc.
                                        #  This needs fixing though.
                                                #num_PUs,  #  cor_num_PUs?  app_num_PUs?
                                                #num_spp,  #  cor_num_spp?  app_num_spp?
                                    cor_num_PUs,  #  cor_num_PUs?  app_num_PUs?
                                    cor_num_spp,  #  cor_num_spp?  app_num_spp?

                                    cor_optimum_cost,

                                        #  Using cor_nodes for now
                                              #nodes,  #  cor_nodes?  app_nodes?
                                    cor_nodes,

                                    cor_final_link_counts_for_each_node,
                                    app_bpm,
                                    cor_bpm,
                                    parameters,
                                    add_error,
                                    match_error_counts,
                                    FP_const_rate,
                                    FN_const_rate,
                                    original_FP_const_rate,
                                    original_FN_const_rate,

                                    marxan_control_values$spf_const,
                                    #                        spf_const,

                                    app_bipartite_metrics_from_bipartite_package,
                                    app_bipartite_metrics_from_igraph_package_df,

                                    app_marxan_output_values$marxan_best_df_sorted,
                                    app_marxan_output_values$marxan_ssoln_df,
                                    app_marxan_output_values$marxan_mvbest_df,

                                    marxan_control_values$marxan_PROP,
                                    marxan_control_values$marxan_RANDSEED,
                                    marxan_control_values$marxan_NUMREPS,
                                    marxan_control_values$marxan_NUMITNS,
                                    marxan_control_values$marxan_STARTTEMP,
                                    marxan_control_values$marxan_NUMTEMP,
                                    marxan_control_values$marxan_COSTTHRESH,
                                    marxan_control_values$marxan_THRESHPEN1,
                                    marxan_control_values$marxan_THRESHPEN2,
                                    marxan_control_values$marxan_RUNMODE,
                                    marxan_control_values$marxan_MISSLEVEL,
                                    marxan_control_values$marxan_ITIMPTYPE,
                                    marxan_control_values$marxan_HEURTYPE,
                                    marxan_control_values$marxan_CLUMPTYPE
                                   )

    }  #  end function - do_graph_and_marxan_analysis

#===============================================================================



