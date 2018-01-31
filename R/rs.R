#===============================================================================

                                    #  rs.R

#===============================================================================

do_ResSel_analysis_and_output <-
                        function (APP_bd_prob,
                                  COR_bd_prob,
                                  parameters,

                                  rs_method_name,
                                  resSel_func,                    #  function for reserve selector, e.g., simple_richness
                                  run_ResSel_func,    #  function to call when running reserve selector, e.g., run_simple_richness
                                  RS_specific_params,

                                  src_rds_file_dir = NULL,
                                  spp_rep_targets = rep (1,COR_bd_prob@num_spp))
    {
        #---------------------------------------------
        #  Create a new reserve selector run object.
        #---------------------------------------------

    ResSel_run <- create_RSrun (APP_bd_prob@UUID,
                                spp_rep_targets,
                                parameters,
                                APP_bd_prob@cor_or_app_str,
                                APP_bd_prob@basic_or_wrapped_or_comb_str,
                                rs_method_name)

        #-------------------------
        #  Run reserve selector.
        #-------------------------

    ResSel_control_values =
        run_ResSel_func (bpm      = APP_bd_prob@bpm,
                         PU_costs = APP_bd_prob@PU_costs,
                         num_spp  = COR_bd_prob@num_spp,
                         num_PUs  = COR_bd_prob@num_PUs,
                         spp_rep_targets,
                         resSel_func,                    #  function for reserve selector, e.g., simple_richness
                         RS_specific_params,    #forward = TRUE,

                         rsrun = ResSel_run,
                         top_dir = parameters$fullOutputDir_NO_slash,
                         save_inputs = TRUE,
                         save_outputs = TRUE)

        #---------------------------------------------------------------------
        #  Need to strip the solution vector out of the result.
        #  The list returned from here will be added to the bdpg results
        #  as part of a single line in a data frame and therefore,
        #  can only contain single scalar values.
        #  If the list contains a vector, then each element is added on a
        #  new line in the rsrun results file with all scalar values in the
        #  list copied on the new line.  In other words, if there are 100
        #  PUs in the solution vector, there will be 100 identical lines
        #  (plus a header line) in the resulting output file.
        #
        #  We don't want to remove the solution vector from the return in
        #  the run_ResSel() function itself because it can be useful to
        #  have the returned solution vector directly available rather than
        #  having to read it back in from disk, e.g., in testing.
        #---------------------------------------------------------------------

    ResSel_control_values$ResSel_solution_vector = NULL

        #-------------------------------------
        #  Collect reserve selector results.
        #-------------------------------------

    save_rsrun_results_data_for_one_rsrun (
                            tzar_run_ID  = parameters$run_ID,
                            exp_root_dir = parameters$fullOutputDir_NO_slash,
                            ResSel_run,
                            COR_bd_prob,
                            APP_bd_prob,
                            rs_method_name,
                            ResSel_control_values,
                            src_rds_file_dir)

    }  #  end function - do_APP_rs_analysis_and_output

#===============================================================================

run_greedy_ResSel <- function (bpm,
                               PU_costs,
                               num_spp,
                               num_PUs,
                               spp_rep_targets,

                               resSel_func,           #  function for reserve selector, e.g., simple_richness
                               RS_specific_params,    #forward = TRUE,

                               rsrun,
                               top_dir = NULL,
                               save_inputs = FALSE,
                               save_outputs = FALSE)
    {

        #-------------------------------------------------------------------
        #  ResSel() returns a 2 element named list containing:
        #    - short_ranked_solution_PU_IDs_vec
        #    - full_ranked_solution_PU_IDs_vec
        #  where the short element contains just the PUs required to cover
        #  the representation targets while the full element contains the
        #  rank ordering of all PUs in the landscape.
        #-------------------------------------------------------------------

    ResSel_timings = system.time (
        {
        ResSel_results = resSel_func (num_spp,
                                      num_PUs,
                                      bpm,
                                      RS_specific_params$forward,
                                      spp_rep_targets)
        })

    ResSel_control_values = RS_specific_params

    ResSel_control_values$RS_user_time       = ResSel_timings["user.self"]
    ResSel_control_values$RS_system_time     = ResSel_timings["sys.self"]
    ResSel_control_values$RS_elapsed_time    = ResSel_timings["elapsed"]
    ResSel_control_values$RS_user_child_time = ResSel_timings["user.child"]
    ResSel_control_values$RS_sys_child_time  = ResSel_timings["sys.child"]

    if (save_inputs)
        {
        ResSel_input_dir  = get_RSrun_path_input (rsrun, top_dir)

        saveRDS (ResSel_control_values,
                 file.path (ResSel_input_dir, "input_params.rds"))
        }

    if (save_outputs)
        {
        ResSel_output_dir = get_RSrun_path_output (rsrun, top_dir)

        saveRDS (ResSel_results,
                 file.path (ResSel_output_dir, "results.rds"))
        }

    ResSel_control_values_and_results = ResSel_control_values
    ResSel_control_values_and_results$ResSel_solution_vector =
        ResSel_results$short_ranked_solution_PU_IDs_vec

    return (ResSel_control_values_and_results)
    }

#===============================================================================

