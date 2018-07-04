#===============================================================================

                    #  do_Greedy_analysis_and_output.R

#===============================================================================

get_Greedy_resSel_func <- function (rs_method_name)
    {
    return (switch (rs_method_name,
                    "SR_Forward"  = simple_richness,
                    "SR_Backward" = simple_richness,

                    "UR_Forward"  = unprotected_richness,
                    "UR_Backward" = unprotected_richness,

                    "ZL_Forward"  = zonation_like,
                    "ZL_Backward" = zonation_like
                    ))
    }

#===============================================================================

    #  This should behave exactly as before.
    #  It just pulls the create_RSrun up into here and calls the
    #  repackaged second half of the code as the ..._core().

#===============================================================================

#' Run greedy reserve selector on bd problem and write output from all analysis
#'
#' Note that the COR and APP arguments are the same if running on a correct
#' problem.
#'
#-------------------------------------------------------------------------------

#' @param resSel_func greedy reserve selection function, e.g.,
#'     simpleRichness
#' @param RS_specific_params list of parameters specific to the given
#'     reserve selector
#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_Greedy_analysis_and_output <-
                        function (APP_bd_prob,
                                  COR_bd_prob,
                                  parameters,

                                  rs_method_name,
                                  resSel_func,                    #  function for reserve selector, e.g., simple_richness
                                  # run_ResSel_func,    #  function to call when running reserve selector, e.g., run_simple_richness
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

    return (do_Greedy_analysis_and_output_core (APP_bd_prob,
                                                  COR_bd_prob,
                                ResSel_run,
                                                  parameters,

                                                  rs_method_name,
                                    resSel_func,        #  function for reserve selector, e.g., simple_richness
                                    # run_ResSel_func,    #  function to call when running reserve selector, e.g., run_simple_richness
                                                  RS_specific_params,

                                                  src_rds_file_dir = NULL,
                                                  spp_rep_targets = rep (1,COR_bd_prob@num_spp))

                        )
    }

#===============================================================================

    #  This needs to decode the old run to recover the objects that it used
    #  and to recover and reinstate the random seed
    #  and create a new RSrun object based on that same seed.
    #  After all that, then it should be able to call the ..._core() just
    #  like the regular (non-reproducing) version finishes up.

#===============================================================================

#' Reproduce greedy reserve selection run
#'
#-------------------------------------------------------------------------------


#' @param repro_RDS_file_loc string containing path to rds file to reproduce
#'     from
#' @param fullOutputDir_NO_slash string containing full path to directory
#'     where reproduction output should go
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------



repro_do_Greedy_analysis_and_output <- function (repro_RDS_file_loc,
                                                 fullOutputDir_NO_slash = NULL)    #"~/Downloads")
    {
    repro = load_saved_obj_from_file (repro_RDS_file_loc)

    APP_bd_prob        = repro$APP_bd_prob
    COR_bd_prob        = repro$COR_bd_prob
    ResSel_run         = repro$ResSel_run

    rs_method_name     = repro$rs_method_name

    resSel_func        = repro$resSel_func
    # run_ResSel_func    = repro$run_ResSel_func

    RS_specific_params = repro$RS_specific_params
    src_rds_file_dir   = repro$src_rds_file_dir
    spp_rep_targets    = repro$spp_rep_targets

        #---------------------------------------------
        #  Create a new reserve selector run object.
        #---------------------------------------------

# ResSel_run <- repro_RSrun (repro_RDS_file_loc,
#                            fullOutputDir_NO_slash = "~/Downloads")

        #-----------------------------------------------------------------------
        #  If a different output area has been provided,
        #  reset the slot for the output area in the original parameters list.
        #-----------------------------------------------------------------------

    parameters         = repro$parameters
    if (! is.null (fullOutputDir_NO_slash) & ! anyNA (fullOutputDir_NO_slash))    #is.na (fullOutputDir_NO_slash))
        parameters$fullOutputDir_NO_slash = fullOutputDir_NO_slash

#    rsrun = repro$rsrun
    prob_UUID                    = ResSel_run@run_on_prob_UUID
    spp_rep_targets              = ResSel_run@targets
    cor_or_app_str               = ResSel_run@cor_or_app_str
    basic_or_wrapped_or_comb_str = ResSel_run@basic_or_wrapped_or_comb_str
    rs_method_name               = ResSel_run@rs_method_name
    new_seed_list                =
        list (seed_value = ResSel_run@rand_seed,
              R_internal_seed_array = ResSel_run@R_internal_seed_array)

cat ("\n@@@TRACKING rand_seed in repro_do_Greedy_analysis_and_output:: new_seed_list$seed_value = ", new_seed_list$seed_value, "\n")

        #  Reset the random seed to match the previous run.
#    set.seed (new_seed_list$R_internal_seed_array)
.Random.seed <<- new_seed_list$R_internal_seed_array

ResSel_run = create_RSrun_core (prob_UUID,
                                 spp_rep_targets,
                                 parameters,
                                 cor_or_app_str,
                                 basic_or_wrapped_or_comb_str,
                                 rs_method_name,
                                 new_seed_list
                                 )
    # ResSel_run <- create_RSrun (APP_bd_prob@UUID,
    #                             spp_rep_targets,
    #                             parameters,
    #                             APP_bd_prob@cor_or_app_str,
    #                             APP_bd_prob@basic_or_wrapped_or_comb_str,
    #                             rs_method_name)

    # res_sel_funcs_list = get_resSel_func (rs_method_name)
    # resSel_func     = res_sel_funcs_list$resSel_func
    # run_ResSel_func = res_sel_funcs_list$run_ResSel_func
resSel_func = get_Greedy_resSel_func (rs_method_name)

    return (do_Greedy_analysis_and_output_core (APP_bd_prob,
                                                  COR_bd_prob,
                                ResSel_run,
                                                  parameters,

                                                  rs_method_name,
                                resSel_func,        #  function for reserve selector, e.g., simple_richness
                                # run_ResSel_func,    #  function to call when running reserve selector, e.g., run_simple_richness
                                                  RS_specific_params,

                                                  src_rds_file_dir,
                                                  spp_rep_targets))
    }

#===============================================================================

do_Greedy_analysis_and_output_core <-
                        function (APP_bd_prob,
                                  COR_bd_prob,
                ResSel_run,
                                  parameters,

                                  rs_method_name,
                                  resSel_func,                    #  function for reserve selector, e.g., simple_richness
                                  # run_ResSel_func,    #  function to call when running reserve selector, e.g., run_simple_richness
                                  RS_specific_params,

                                  src_rds_file_dir = NULL,
                                  spp_rep_targets = rep (1,COR_bd_prob@num_spp))
    {
    #     #---------------------------------------------
    #     #  Create a new reserve selector run object.
    #     #---------------------------------------------
    #
    # ResSel_run <- create_RSrun (APP_bd_prob@UUID,
    #                             spp_rep_targets,
    #                             parameters,
    #                             APP_bd_prob@cor_or_app_str,
    #                             APP_bd_prob@basic_or_wrapped_or_comb_str,
    #                             rs_method_name)

    repro = list (APP_bd_prob        = APP_bd_prob,
                  COR_bd_prob        = COR_bd_prob,
                  ResSel_run         = ResSel_run,
                  parameters         = parameters,
                  rs_method_name     = rs_method_name,
          resSel_func        = resSel_func,
          # run_ResSel_func    = run_ResSel_func,
                  RS_specific_params = RS_specific_params,
                  src_rds_file_dir   = src_rds_file_dir,
                  spp_rep_targets    = spp_rep_targets)

    starting_dir = parameters$fullOutputDir_NO_slash
    base_outdir = get_RSrun_path_topdir (ResSel_run, starting_dir)
#    saveRDS (repro, parameters$fullOutputDir_NO_slash)
    saveRDS (repro, file.path (base_outdir, "repro.rds"))

        #-------------------------
        #  Run reserve selector.
        #-------------------------

    ResSel_control_values =
        run_greedy_ResSel (bpm      = APP_bd_prob@bpm,
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
                            tzar_run_ID  = parameters$run_id,
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

