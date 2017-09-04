#===============================================================================

                    #  single_action_using_tzar_reps.R

#===============================================================================

#' Load a bdprob from an R rds output file, possibly specified in an array
#'
#' Takes a file name for an R rds output file that contains a saved Xu_bd_problem
#' object and loads the bdprob from the file.  The location of the file comes
#' from the parameters list and can be given in several ways.  The source
#' can be an rds file path or a text file containing multiple rds file paths or
#' it can be a yaml array containing multiple rds file paths.  If the source is
#' either of the multiple path sources, then the parameters list will contain
#' a variable called cur_input_prob_idx, giving the location of the path in
#' the text file or yaml array.
#'
#'
#' @param app_bdprob an apparent bdprob
#' @param cor_bdprob a correct bdprob
#' @inheritParams std_param_defns
#'
#' @return Returns a list containing named elements related to the source
#'         problem, i.e. src_Xu_bd_problem and src_rds_file_dir
#'
#-------------------------------------------------------------------------------

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

#' Make sure that the correct bdprob is the base for the given apparent bdprob
#'
#' If the given correct Xu_bd_problem is not the problem that the apparent has
#' added error to, this is a fatal error, so give an error message and quit.
#'
#' @param app_bdprob an apparent bdprob
#' @param cor_bdprob a correct bdprob
#' @inheritParams std_param_defns
#'
#' @return Returns nothing.
#'
#-------------------------------------------------------------------------------

make_sure_that_cor_bdprob_is_base_of_app_bdprob <- function (app_bdprob,
                                                             cor_bdprob,
                                                             parameters)
    {
    cor_bdprob_UUID      = cor_bdprob@UUID
    app_bdprob_base_UUID = app_bdprob@APP_prob_info@UUID_of_base_problem_that_has_err_added

    if (cor_bdprob_UUID != app_bdprob_base_UUID)
        {
        app_bdprob_UUID = app_bdprob@UUID

        err_msg = paste0 ("\n\nIn single_action_using_tzar_reps() doing run_rs_on_APP_prob: ",
                        "\n    current COR problem is not base for current APP problem.",

                        "\n    cor_bdprob_UUID      = '", cor_bdprob_UUID, "'",
                        "\n    app_bdprob_base_UUID = '", app_bdprob_base_UUID, "'",
                        "\n    app_bdprob_UUID = '", app_bdprob_UUID, "'",

                        "\n    cur_input_prob_idx = '", parameters$cur_input_prob_idx, "'",
                        "\n    rds_file_set_element_idx = '", parameters$rds_file_set_element_idx, "'")

        if (parameters$prob_src == "rds_file_set_from_file")
            {
            err_msg =
                paste0 (err_msg,
                        "\n    RS_cor_input_rds_file_set_path = '", parameters$RS_cor_input_rds_file_set_path, "'",
                        "\n    RS_app_input_rds_file_set_path = '", parameters$RS_app_input_rds_file_set_path, "'"
                        )
            }

        stop (err_msg)

        }  #  end if - UUIDs don't match
    }

#===============================================================================

#' Get the probable UUID of an object from its file name
#'
#' Problems and reserve selections saved from bdpg in rds files are given a
#' file name with a specific syntax, i.e., "saved", object type, UUID, "rds".
#' For example,
#'     saved.RSprob-COR-Base.489a016d-14fd-40af-89bb-3e1edf38f14f.rds
#' This function decodes that file name and returns the UUID.
#'
#' @param file_path rds file name (possibly full path) containing a UUID
#'
#' @return Returns UUID section of the given file name
#' @export
#'
#' @examples
#'  get_UUID_from_file_path ("saved.RSprob-COR-Base.489a016d-14fd-40af-89bb-3e1edf38f14f.rds")
#'  get_UUID_from_file_path ("/Users/bill/tzar/outputdata/bdpgxupaper_single_action_COR_prob/default_runset/1_marxan_simulated_annealing/RSprob-COR-Base.489a016d-14fd-40af-89bb-3e1edf38f14f/saved.RSprob-COR-Base.489a016d-14fd-40af-89bb-3e1edf38f14f.rds")

#-------------------------------------------------------------------------------

get_UUID_from_file_path <- function (f)
    {
    x = strsplit (basename(f),"\\.") [[1]]
    UUID = x[3]

    return (UUID)
    }

#===============================================================================

#' Search the set of files for a file name containing the UUID and load that file
#'
#' Given a vector of file names (possibly including their paths), find the
#' file name that contains the given UUID and load that file
#'
#' @param UUID_to_find a UUID string to look for in file names
#' @param rds_file_set vector of file names (possibly including paths)
#'
#' @return Returns the object loaded from the rds file if found, NULL if no
#' file name containing given UUID was found
#' @export

#-------------------------------------------------------------------------------

look_up_object_by_UUID <- function (UUID_to_find, rds_file_set)
    {
    obj = NULL
    for (cur_rds_file_path in rds_file_set)
        {
        cur_UUID = get_UUID_from_file_path (cur_rds_file_path)
        if (cur_UUID == UUID_to_find)
            {
            obj =
                load_saved_obj_from_file (normalizePath (cur_rds_file_path))
            break
            }
        }

    return (obj)
    }

#===============================================================================

#' Get the COR bdprob whose UUID is the base UUID for the given APP bdprob
#'
#' @param app_bdprob an apparent Xu_bd_problem
#' @inheritParams std_param_defns
#'
#' @return Returns the COR Xu_bd_problem that the apparent problem added error
#' to

#-------------------------------------------------------------------------------

get_base_cor_bdprob_for_given_app_bdprob <- function (app_bdprob, parameters)
    {
    app_bdprob_base_UUID =
        app_bdprob@APP_prob_info@UUID_of_base_problem_that_has_err_added

    rds_file_set =
        readLines (parameters$RS_app_base_input_rds_file_set_path)

cat ("\n\n---------- START get_base_cor_bdprob_for_given_app_bdprob() ----------")
cat ("\napp_bdprob@UUID = '", app_bdprob@UUID, "'")
cat ("\napp_bdprob_base_UUID = '", app_bdprob_base_UUID, "'")

    cor_bdprob =
        look_up_object_by_UUID (app_bdprob_base_UUID, rds_file_set)

cat ("\ncor_bdprob@UUID = '", cor_bdprob@UUID, "'")
cat ("\n\n---------- END get_base_cor_bdprob_for_given_app_bdprob() ----------")

    make_sure_that_cor_bdprob_is_base_of_app_bdprob (app_bdprob,
                                                     cor_bdprob,
                                                     parameters)

    return (cor_bdprob)
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

#-------------------------------------------------------------------------------

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
    run_rs_on_COR_prob = value_or_FALSE_if_null (parameters$run_rs_on_COR_prob)
    run_rs_on_APP_prob = value_or_FALSE_if_null (parameters$run_rs_on_APP_prob)

    num_actions_chosen = gen_COR_prob + gen_WRAP_prob + gen_APP_prob +
                         run_rs_on_COR_prob + run_rs_on_APP_prob

    if (num_actions_chosen != 1)
        stop (paste0 ("\nMust set 1 and only 1 of these variables to TRUE: ",
                      "gen_COR_prob (", gen_COR_prob, "), ",
                      "gen_WRAP_prob (", gen_WRAP_prob, "), ",
                      "gen_APP_prob (", gen_APP_prob, "), ",
                      "run_rs_on_COR_prob (", run_rs_on_COR_prob, "), ",
                      "run_rs_on_APP_prob (", run_rs_on_APP_prob, "), ",
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
                                        #  never used?
                                      parameters$WRAP_input_rds_file_set_yaml_array,
                                      parameters$WRAP_rds_file_path,
                                      bdpg_error_codes
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
                                        #  never used?
                                      parameters$APP_input_rds_file_set_yaml_array,
                                      parameters$APP_rds_file_path,
                                      bdpg_error_codes
                                      )

        bdpg::gen_single_bdprob_APP (bdprob_to_add_error_to,
                                     parameters$compute_network_metrics_APP,
                                     parameters,
                                     bdpg_error_codes,
                                     integerize
                                     )
        }

    #---------------------------------------------------------------------------

        #------------------------------------------------------------------
        #  Run a reserve selector on a correct problem (base or wrapped),
        #  if requested.
        #------------------------------------------------------------------

    if (run_rs_on_COR_prob)
        {
        cor_bdprob =
            get_bdprob_from_rds_file (parameters$RS_cor_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$RS_cor_input_rds_file_set_path,
                                        #  never used?
                                      parameters$RS_cor_input_rds_file_set_yaml_array,
                                      parameters$RS_cor_rds_file_path,
                                      bdpg_error_codes
                                      )

        bdpg::do_COR_marxan_analysis_and_output (cor_bdprob, parameters)
        }

    #---------------------------------------------------------------------------

        #---------------------------------------------------------------
        #  Run a reserve selector on an apparent problem if requested.
        #---------------------------------------------------------------

    if (run_rs_on_APP_prob)
        {
        app_bdprob =
            get_bdprob_from_rds_file (parameters$RS_app_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$RS_app_input_rds_file_set_path,
                                        #  never used?
                                      parameters$RS_app_input_rds_file_set_yaml_array,
                                      parameters$RS_app_rds_file_path,
                                      bdpg_error_codes
                                      )

        cor_bdprob = get_base_cor_bdprob_for_given_app_bdprob (app_bdprob,
                                                               parameters)

        bdpg::do_APP_marxan_analysis_and_output (app_bdprob,
                                                 cor_bdprob,
                                                 parameters)
        }

    #---------------------------------------------------------------------------

    return()
    }

#===============================================================================

