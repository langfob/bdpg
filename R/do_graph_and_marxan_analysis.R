#===============================================================================
#
#                       do_graph_and_marxan_analysis.R
#
#   Run code to compute graph metrics and to run marxan and dump results
#   to file.
#
#===============================================================================
#
#  2017 02 08 6:39 pm - BTL
#
#---------------------------
#  CURRENT STATE OF THINGS
#---------------------------
#
#  Code in bdpgxupaper is able to run through wrapping a problem and then
#  feeding it to marxan many different times under many different names
#  such as "other" and "simpleRichness" (just to see if the directory
#  creation and file writing works correctly.
#
#  The next step is to be able to collect the results of those runs and the
#  graph metrics, etc. and write them out.  However, that's currently a big
#  ugly mess of code (though I think it Does work correctly).  The one other
#  thing that will take a fair bit of work is to clean up the generation of
#  apparent problems.  Among other things, this will probably have a similar
#  directory naming task to the one I did today for different reserve selectors.
#
#  Here are the main current tasks and issues:
#
#----------------------------------------------
#  REFACTORING do_graph_and_marxan_analysis()
#----------------------------------------------
#
#  do_graph_and_marxan_analysis() no longer seems like a good way to handle
#  graph + reserve selection + assembling output structures + writing output.
#
#  Not only is it unwieldy, but it's also difficult to figure out what belongs
#  where in the arg list when running with correct data vs. with apparent data.
#
#  Moreover, apparent data adds a whole lot more information that is completely
#  irrelevant for the correct case, so in the correct case, the arg list gets
#  longer than necessary and even less clear about what should go in certain
#  args that don't even apply.
#
#  So, I think I'm going to split up the correct and apparent cases and
#  reorganize/refactor the whole calling process where do_graph...() was
#  called in the past.
#
#  GRAPH/NETWORK
#  -------------
#  One part of this is already done, since I've already moved the network
#  (graph) metric calculations back into the problem creation section since
#  they're relevant only to the problem itself and have nothing to do with
#  running reserve selectors.
#
#  OUTPUT ASSEMBLY & WRITING/ARCHIVING OUTPUT
#  ------------------------------------------
#  The output assembly needs lots of work to be more flexible in building and
#  storing subsets of attributes such as graph metrics, marxan results, etc.
#  I also need to make it more flexible to dealing with multiple runs inside a
#  single tzar run, e.g., if you had 100 apparents for a particular wrapped
#  problem and ran marxan with 2 different parameterizations that allowed each
#  allowed a different number of runs.  You might also be running a different
#  reserve selector in the same run.
#
#  Another issue with the output assembly is that it's not just reading of
#  files and assembly of results.  It also contains some computation of EF
#  scores, etc.  Need to separate those things out.  The code to read marxan
#  output files is particulary messy.
#
#  RESERVE SELECTION
#  -----------------
#  Still not sure how to handle running multiple reserve selectors over the
#  same problem in a single run.
#
#-------------------------------------------------------------------------------
#
#-------------------
#  ANOTHER PROBLEM
#-------------------
#  How to handle doing partial runs that make use of previous runs, e.g.,
#  suppose you generated a bunch of problems in one big batch, then came
#  back and ran marxan over those in another batch, then generated a bunch of
#  apparents for the original set and ran a bunch of different reserve
#  selectors over those, etc.
#
#  The question is, how is this best done to make it easy to keep doing these
#  "second thought" kinds of analyses after pieces of analyses have already
#  been done?  What does it imply needs to be archived and how?  What does it
#  imply about directory structures?  What does it imply about what is
#  temporarily stored where when you're in the process of doing one of these
#  second thoughts, e.g., where does the copy of the original problem go
#  while you're doing the new analysis?  Etc.
#
#===============================================================================

do_COR_graph_and_marxan_analysis <- function (COR_bd_prob,  #  NEWLY ADDED TO ARG LIST
                                              parameters
                        #                       ,
                        #                         #  parameters from gen prob.
                        #                         #  They're only used in creating
                        #                         #  the master output structure.
                        #                   Xu_parameters,
                        #                   read_Xu_problem_from_Xu_file,
                        #
                        #                         #  From bdprob structure, i.e.,
                        #                         #  results of gen prob routine
                        #                   cor_num_spp,
                        #                   cor_num_PUs,
                        #                   cor_PU_spp_pair_indices,
                        #                   cor_PU_IDs, #####!!!!!#####
                        #                   cor_spp_IDs,  #####!!!!!#####
                        #                   cor_bpm,
                        #
                        #                   cor_PU_costs,
                        #                   cor_optimum_cost,
                        #                   cor_nodes,
                        #                   spp_col_name,
                        #                   PU_col_name,
                        #
                        #                   presences_col_name, #  hard-coded as "freq"
                        #
                        #                         #  Results of adding error.
                        #                   app_num_spp,
                        #                   app_num_PUs,
                        #                   app_PU_spp_pair_indices,
                        #                   app_bpm,
                        #
                        #                         #  input parameters for error model.
                        #                         #  Only used to create master output structure?
                        #                   add_error,
                        #                   match_error_counts,
                        #                   FP_const_rate,
                        #                   FN_const_rate,
                        #                   original_FP_const_rate,
                        #                   original_FN_const_rate,
                        #
                        #
                        #
                        # #  THESE MARXAN DIRECTORIES NEED TO BE CREATED AND
                        # #  STORED USING THE WAY OF ALLOCATING DIRECTORIES
                        # #  TO A RESERVE SELECTOR.
                        #
                        #                   derived_bdpg_dir_names
                                         )
    {
#===============================================================================
#                                   Run marxan.
#===============================================================================

    set_up_and_run_return_COR_values =
        set_up_for_and_run_marxan_COR (COR_bd_prob, parameters)

        # set_up_and_run_return_values =
        # bdpg::set_up_for_and_run_marxan (app_PU_spp_pair_indices,
        #                                     cor_PU_IDs, #####!!!!!#####
        #                                     cor_spp_IDs,  #####!!!!!#####
        #                                     PU_col_name,
        #                                     spp_col_name,
        #                                     derived_bdpg_dir_names,
        #                                     parameters
        #                                     )

    marxan_control_values  = set_up_and_run_return_COR_values$marxan_control_values
    COR_bd_prob            = set_up_and_run_return_COR_values$COR_bd_prob

    cat("\njust after set_up_for_and_run_marxan_COR()")

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

        #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
        #                       reading Xu file, e.g., prob_generator_params_known.
        #                       This is because learning alg downstream needs to
        #                       know things like whether the generator's params
        #                       are even known, so that it doesn't try to learn
        #                       something from missing data.

do_APP_graph_and_marxan_analysis <- function (parameters,
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

    set_up_and_run_return_values =
        bdpg::set_up_for_and_run_marxan (app_PU_spp_pair_indices,
                                            cor_PU_IDs, #####!!!!!#####
                                            cor_spp_IDs,  #####!!!!!#####
                                            PU_col_name,
                                            spp_col_name,
                                            derived_bdpg_dir_names,
                                            parameters
                                            )

    marxan_control_values  = set_up_and_run_return_values$marxan_control_values
    derived_bdpg_dir_names = set_up_and_run_return_values$bdpg_dir_names

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

        #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
        #                       reading Xu file, e.g., prob_generator_params_known.
        #                       This is because learning alg downstream needs to
        #                       know things like whether the generator's params
        #                       are even known, so that it doesn't try to learn
        #                       something from missing data.

OLD_do_graph_and_marxan_analysis <- function (parameters,
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

    set_up_and_run_return_values =
        bdpg::set_up_for_and_run_marxan (app_PU_spp_pair_indices,
                                            cor_PU_IDs, #####!!!!!#####
                                            cor_spp_IDs,  #####!!!!!#####
                                            PU_col_name,
                                            spp_col_name,
                                            derived_bdpg_dir_names,
                                            parameters
                                            )

    marxan_control_values  = set_up_and_run_return_values$marxan_control_values
    derived_bdpg_dir_names = set_up_and_run_return_values$bdpg_dir_names

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



