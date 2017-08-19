#===============================================================================

                    #  single_action_using_tzar_reps.R

#===============================================================================

get_bdprob_from_rds_file <- function (prob_src,
                                      cur_input_prob_idx,
                                      rds_file_set_path,
                                      rds_file_set_yaml_array,
                                      rds_file_path,
                                      bdpg_error_codes
                                      )
    {
#    prob_from_generator                    = "generator"
    prob_from_rds_file                     = "rds_file"
    prob_from_rds_file_set_from_file       = "rds_file_set_from_file"
    prob_from_rds_file_set_from_yaml_array = "rds_file_set_from_yaml_array"
#    prob_from_Xu_bench_file                = "Xu_bench_file"

##    prob_src = parameters$prob_src

    if (is.null (prob_src))
        {
        cat ("\n\nERROR: no prob_src given.\n", sep='')
        quit (save="no", bdpg_error_codes$ERROR_STATUS_no_prob_src_given)

        } else if (prob_src == prob_from_rds_file)
        {
##            rds_file_path = parameters$rds_file_path

            Xu_bdprob =
                load_saved_obj_from_file (normalizePath (rds_file_path))

        } else if (prob_src == prob_from_rds_file_set_from_file)
        {
##            rds_file_set_path = parameters$rds_file_set_path
            rds_file_set = readLines (rds_file_set_path)

##            cur_input_prob_idx = parameters$cur_input_prob_idx
            rds_file_path = rds_file_set [cur_input_prob_idx]

            Xu_bdprob =
                load_saved_obj_from_file (normalizePath (rds_file_path))

        } else if (prob_src == prob_from_rds_file_set_from_yaml_array)
        {
##            rds_file_set = parameters$rds_file_set_yaml_array

##            cur_input_prob_idx = parameters$cur_input_prob_idx
            rds_file_path = rds_file_set [cur_input_prob_idx]

            Xu_bdprob =
                load_saved_obj_from_file (normalizePath (rds_file_path))

        } else
        {
        cat ("\n\nERROR: unknown prob_src = '", prob_src, "'.\n", sep='')
        quit (save="no", bdpg_error_codes$ERROR_STATUS_unknown_prob_src)
        }

    return (Xu_bdprob)
    }

#===============================================================================

#' Execute a single action based on tzar repetition values in project.yaml
#'
#' This is the main workhorse function for bdpg.  It's intended to generate
#' a single bdproblem of a chosen type (COR, WRAP, APP) or a single reserve
#' selector run based on inputs specified in the yaml file and its
#' repetitions section.  In general, tzar will spawn a process that calls this
#' function across a set of problems, e.g., generating 10 apparent problems
#' for each correct problem whose path is given in an input file.
#'
#' @inheritParams std_param_defns
#'
#' @return Returns nothing.
#' @export

single_action_using_tzar_reps <- function (parameters,
                                           bdpg_error_codes,
                                           integerize)
    {
        #------------------------------------------------------
        #  Make sure that exactly one action has been chosen.
        #  Quit if none or more than one chosen.
        #------------------------------------------------------

    gen_COR_prob  = value_or_FALSE_if_null (parameters$gen_COR_prob)
    gen_WRAP_prob = value_or_FALSE_if_null (parameters$gen_WRAP_prob)
    gen_APP_prob  = value_or_FALSE_if_null (parameters$gen_APP_prob)
    do_rsrun      = value_or_FALSE_if_null (parameters$do_rsrun)

    num_actions_chosen = gen_COR_prob + gen_WRAP_prob + gen_APP_prob +
                         do_rsrun

    if (num_actions_chosen != 1)
        stop (paste0 ("\nMust set 1 and only 1 of these variables to TRUE: ",
                      "gen_COR_prob (", gen_COR_prob, "), ",
                      "gen_WRAP_prob (", gen_WRAP_prob, "), ",
                      "gen_APP_prob (", gen_APP_prob, "), ",
                      "do_rsrun (", do_rsrun, "), ",
                      "\n"))

    #---------------------------------------------------------------------------

        #--------------------------------------------
        #  Generate a correct problem if requested.
        #--------------------------------------------

    if (gen_COR_prob)
        {
        bdpg::gen_single_bdprob_COR (parameters,
                                     bdpg_error_codes,
                                     integerize,
                                     base_prob_name_stem = "base_prob",
                                     cor_dir_name_stem = "cor")
        }

    #---------------------------------------------------------------------------

        #--------------------------------------------
        #  Generate a wrapped problem if requested.
        #--------------------------------------------

    if (gen_WRAP_prob)
        {
        bdprob_to_wrap =
            get_bdprob_from_rds_file (parameters$WRAP_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$WRAP_input_rds_file_set_path,
                                      parameters$WRAP_input_rds_file_set_yaml_array,
                                      parameters$WRAP_rds_file_path
                                     )

        bdpg::gen_single_bdprob_WRAP (bdprob_to_wrap,
                                      parameters,
                                      bdpg_error_codes)
        }

    #---------------------------------------------------------------------------

        #----------------------------------------------
        #  Generate an apparent problem if requested,
        #  i.e., add error to a correct problem.
        #----------------------------------------------

    if (gen_APP_prob)
        {
        bdprob_to_add_error_to =
            get_bdprob_from_rds_file (parameters$APP_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$APP_input_rds_file_set_path,
                                      parameters$APP_input_rds_file_set_yaml_array,
                                      parameters$APP_rds_file_path
                                      )

        bdpg::gen_single_bdprob_APP (bdprob_to_add_error_to,
                                     parameters$compute_network_metrics_APP,
                                     parameters,
                                     bdpg_error_codes,
                                     integerize
                                     )
        }

    #---------------------------------------------------------------------------

        #----------------------------------------
        #  Run a reserve selector if requested.
        #----------------------------------------

    if (do_rsrun)
        {
        num_actions_chosen = run_rs_on_COR_prob + run_rs_on_APP_prob

        if (num_actions_chosen != 1)
            stop (paste0 ("\nMust set 1 and only 1 of these variables to TRUE: ",
                          "run_rs_on_COR_prob (", run_rs_on_COR_prob, "), ",
                          "run_rs_on_APP_prob (", run_rs_on_APP_prob, "), ",
                          "\n"))

        #-----------------------------------------------------------------------

        cor_bdprob =
            get_bdprob_from_rds_file (parameters$RS_cor_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$RS_cor_input_rds_file_set_path,
                                      parameters$RS_cor_input_rds_file_set_yaml_array,
                                      parameters$RS_cor_rds_file_path
                                      )

        if (run_rs_on_COR_prob)
            {
            bdpg::do_COR_marxan_analysis_and_output (cor_bdprob,
                                                     parameters)

            } else if (run_rs_on_APP_prob)
            {
            app_bdprob =
                get_bdprob_from_rds_file (parameters$RS_app_input_prob_src,
                                          parameters$cur_input_prob_idx,
                                          parameters$RS_app_input_rds_file_set_path,
                                          parameters$RS_app_input_rds_file_set_yaml_array,
                                          parameters$RS_app_rds_file_path
                                          )

            bdpg::do_APP_marxan_analysis_and_output (app_bdprob,
                                                     cor_bdprob,
                                                     parameters)
            }
        }

    #---------------------------------------------------------------------------

    return()
    }

#===============================================================================

