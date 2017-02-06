#===============================================================================
#
#                       do_graph_and_marxan_analysis.R
#
#   Run code to compute graph metrics and to run marxan and dump results
#   to file.
#
#===============================================================================

create_base_dir_structure <- function (base_outdir = ".",
                                       cor_or_app_subdir_name = "cor")
    {
        #  Create list of directory names.
    derived_bdpg_dir_names = list()

        #  Clean up base_outdir into usable path if necessary,
        #  e.g., change "." or "~somebody/x" into fully qualified path name.
    base_outdir = normalizePath (base_outdir)

        #  Create PLOT OUTPUT directory.
    derived_bdpg_dir_names$plot_output_dir =
                file.path (base_outdir, cor_or_app_subdir_name, "plots")
    dir.create (derived_bdpg_dir_names$plot_output_dir,
                showWarnings = TRUE, recursive = TRUE)

        #  Create NETWORK OUTPUT directory.
    derived_bdpg_dir_names$network_output_dir =
                file.path (base_outdir, cor_or_app_subdir_name,
                           "networks")
    dir.create (derived_bdpg_dir_names$network_output_dir,
                showWarnings = TRUE, recursive = TRUE)

        #  Create RES_SEL directory.
    derived_bdpg_dir_names$res_sel_dir =
                file.path (base_outdir, cor_or_app_subdir_name, "res_sel")
    dir.create (derived_bdpg_dir_names$res_sel_dir,
                showWarnings = TRUE, recursive = TRUE)

    return (derived_bdpg_dir_names)
    }

#===============================================================================

create_one_res_sel_dir_structure <- function (res_sel_dir_name = "marxan",
                                              base_outdir = ".")
    {
    res_sel_dir_names = vector ("list", 2)
    names (res_sel_dir_names) <- c("input_dir", "output_dir")

        #  Clean up base_outdir into usable path if necessary,
        #  e.g., change "." or "~somebody/x" into fully qualified path name.
    base_outdir = normalizePath (base_outdir)

        #  Create reserve selector INPUT directory.
    res_sel_dir_names$input_dir = file.path (base_outdir, "input")
    dir.create (res_sel_dir_names$input_dir,
                showWarnings = TRUE, recursive = TRUE)

        #  Create reserve selector OUTPUT directory.
    res_sel_dir_names$output_dir = file.path (base_outdir, "output")
    dir.create (res_sel_dir_names$output_dir,
                showWarnings = TRUE, recursive = TRUE)

    return (res_sel_dir_names)
    }

#===============================================================================

create_multiple_res_sel_dir_structures <- function (res_sel_dir_names,
                                                    base_outdir)
    {
    res_sel_dir_structures <- lapply (res_sel_dir_names,
                                      create_one_res_sel_dir_structure,
                                      base_outdir)

    names (res_sel_dir_structures) <- res_sel_dir_names

    return (res_sel_dir_structures)
    }

#===============================================================================

        #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
        #                       reading Xu file, e.g., prob_generator_params_known.
        #                       This is because learning alg downstream needs to
        #                       know things like whether the generator's params
        #                       are even known, so that it doesn't try to learn
        #                       something from missing data.

do_graph_and_marxan_analysis <- function (
#XXX 2017 02 06    cor_or_app_subdir_name,

                                            #  input parameters
                                          parameters,

#XXX 2017 02 06    current_os,   #### derived_bdpg_parameters$current_os
                                          #derived_bdpg_parameters,  # only used to get dirs and current_os, so should separate those out !!

                                            #  parameters from gen prob.
                                            #  They're only used in creating
                                            #  the master output structure.
                                          Xu_parameters,
                                          read_Xu_problem_from_Xu_file,

                                            #  From bdprob structure, i.e., results of gen prob routine
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

                                            #  Immediately after bdprob struct vars above.
                                          presences_col_name, #  hard-coded as "freq"
#####!!!!!#####                                          all_correct_node_IDs,

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


derived_bdpg_dir_names
                                          )
{
#===============================================================================
#                                   Run marxan.
#===============================================================================

  marxan_control_values = set_up_for_and_run_marxan (
            #  This could (should?) also be applied to the cor_PU_spp_pair_indices...
            #  Does cor_PU_spp_pair_indices exist or is that just PU_spp_pair_indices?
            #  Also, the output directories may need to change if there is more
            #  than one run of marxan done.
        app_PU_spp_pair_indices,
    cor_PU_IDs, #####!!!!!#####
    cor_spp_IDs,  #####!!!!!#####
                                                      PU_col_name,
                                                      spp_col_name,
#XXX 2017 02 06    current_os,    #  derived_bdpg_parameters$current_os,
                                                      derived_bdpg_dir_names$marxan_input_dir,
        derived_bdpg_dir_names$marxan_output_dir,
                                                      parameters,
        derived_bdpg_dir_names$marxan_IO_dir
                                                      )
cat("\njust after set_up_for_and_run_marxan()")

  #---------------------------------------------------------------------------

  app_marxan_output_values = read_marxan_output_files (

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

  create_master_output_structure (
                          read_Xu_problem_from_Xu_file,
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



