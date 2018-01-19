#===============================================================================

                    #  single_action_using_tzar_reps.R

#===============================================================================

#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

#' @param prob_src character string
#' @param cur_input_prob_idx integer
#' @param rds_file_set_path character string
#' @param rds_file_set_yaml_array vector of character strings
#' @param rds_file_path character string
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
                                      rds_file_path)
    {
#    prob_from_generator                    = "generator"
    prob_from_rds_file                     = "rds_file"
    prob_from_rds_file_set_from_file       = "rds_file_set_from_file"
    prob_from_rds_file_set_from_yaml_array = "rds_file_set_from_yaml_array"
#    prob_from_Xu_bench_file                = "Xu_bench_file"

##    prob_src = parameters$prob_src

    if (is.null (prob_src))
        {
        stop_bdpg ("No prob_src given")

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
        stop_bdpg (paste0 ("Unknown prob_src = '", prob_src, "'"))
        }

    src_rds_file_dir = dirname (rds_file_path)

    return (list (src_Xu_bd_problem = Xu_bdprob,
                  src_rds_file_dir = src_rds_file_dir))
    }

#===============================================================================

#' Make sure that the correct bdprob is the base for the given apparent bdprob
#'
#' If the given correct Xu_bd_problem is not the problem that the apparent has
#' added error to, this is a fatal error, so give an error message and quit.
#'
#-------------------------------------------------------------------------------

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

        stop_bdpg (err_msg)

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
#-------------------------------------------------------------------------------

#' @param f rds file name (possibly full path) containing a UUID
#'
#' @return Returns UUID section of the given file name
#' @export
#'
#' @examples
#'  get_UUID_from_file_path ("saved.RSprob-COR-Base.489a016d-14fd-40af-89bb-3e1edf38f14f.rds")
#'
#'  long_path_part1 = "/Users/bill/tzar/outputdata/bdpgxupaper_single_action_COR_prob/"
#'  long_path_part2 = "default_runset/1_marxan_simulated_annealing/RSprob-COR-Base."
#'  long_path_part3 = "489a016d-14fd-40af-89bb-3e1edf38f14f"
#'  long_path_part4 = "/saved.RSprob-COR-Base.489a016d-14fd-40af-89bb-3e1edf38f14f.rds"
#'  long_path = paste0 (long_path_part1, long_path_part2, long_path_part3, long_path_part4)
#'  get_UUID_from_file_path (long_path)

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
#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

#' @param app_bdprob an apparent Xu_bd_problem
#' @inheritParams std_param_defns
#'
#' @return Returns the COR Xu_bd_problem that the apparent problem added error
#' to

#-------------------------------------------------------------------------------

#' Get COR problem that given APP problem is based on
#'
#'  The parameters list needs to show locations for both the APP problems
#'  and the COR problems that they're based on, so the project.yaml file
#'  needs to have a section like this:
#'
#'    run_rs_on_APP_prob: TRUE
#'
#'    RS_app_input_prob_src: "rds_file_set_from_file"
#'    RS_app_input_rds_file_set_path: "/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpgxupaper/inst/extdata/input_files/Tzar_input_files/APP_rds_input_file_paths.txt"
#'
#'    RS_cor_input_prob_src: "rds_file_set_from_file"
#'    RS_cor_input_rds_file_set_path: "/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpgxupaper/inst/extdata/input_files/Tzar_input_files/COR_rds_input_file_paths.txt"

#-------------------------------------------------------------------------------

get_base_cor_bdprob_for_given_app_bdprob <- function (app_bdprob, parameters)
    {
    app_bdprob_base_UUID =
        app_bdprob@APP_prob_info@UUID_of_base_problem_that_has_err_added

        #---------------------------------------------------------------------
        #  Need a list of COR problem locations to use in finding the
        #  COR problem that the APP problem added error to.
        #  For the moment, only allow the list to come from a file
        #  since that's the most likely and I just want to get this working.
        #---------------------------------------------------------------------

    if (parameters$RS_cor_input_prob_src == "rds_file_set_from_file")
        {
        COR_rds_file_set =
            readLines (parameters$RS_cor_input_rds_file_set_path)

        } else
        {
        stop_bdpg (paste0 ("\n\nERROR:  parameters$RS_cor_input_prob_src = '", parameters$RS_cor_input_prob_src, "'.",
                        "\n        Must be 'rds_file_set_from_file'.\n\n"))
        }

cat ("\n\n---------- START get_base_cor_bdprob_for_given_app_bdprob() ----------")
cat ("\napp_bdprob@UUID = '", app_bdprob@UUID, "'")
cat ("\napp_bdprob_base_UUID = '", app_bdprob_base_UUID, "'")

    cor_bdprob =
        look_up_object_by_UUID (app_bdprob_base_UUID, COR_rds_file_set)

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
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing.
#' @export

#-------------------------------------------------------------------------------

single_action_using_tzar_reps <- function (parameters, integerize)
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
        stop_bdpg (paste0 ("\nMust set 1 and only 1 of these variables to TRUE: ",
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
#        bdprob_to_wrap =
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$WRAP_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$WRAP_input_rds_file_set_path,
                                        #  never used?
                                      parameters$WRAP_input_rds_file_set_yaml_array,
                                      parameters$WRAP_rds_file_path)

        src_bdprob_to_wrap = src_prob_and_path_list$src_Xu_bd_problem
        src_rds_file_dir   = src_prob_and_path_list$src_rds_file_dir

        bdpg::gen_single_bdprob_WRAP (src_bdprob_to_wrap,
                                      parameters
                                      # ,
                                      # src_rds_file_dir    #  NO LONGER USED?
                                      )
        }

    #---------------------------------------------------------------------------

        #----------------------------------------------
        #  Generate an apparent problem if requested,
        #  i.e., add error to a correct problem.
        #----------------------------------------------

    if (gen_APP_prob)
        {
#        bdprob_to_add_error_to =
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$APP_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$APP_input_rds_file_set_path,
                                        #  never used?
                                      parameters$APP_input_rds_file_set_yaml_array,
                                      parameters$APP_rds_file_path)

        bdprob_to_add_error_to = src_prob_and_path_list$src_Xu_bd_problem
#        src_rds_file_dir       = src_prob_and_path_list$src_rds_file_dir

        bdpg::gen_single_bdprob_APP (bdprob_to_add_error_to,
                                     #value_or_FALSE_if_null (parameters$compute_network_metrics_APP),
                                     parameters
                                     #,
                                     #integerize
                                     )
        }

    #---------------------------------------------------------------------------

        #------------------------------------------------------------------
        #  Run a reserve selector on a correct problem (base or wrapped),
        #  if requested.
        #------------------------------------------------------------------

    if (run_rs_on_COR_prob)
        {
#        cor_bdprob =
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$RS_cor_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$RS_cor_input_rds_file_set_path,
                                        #  never used?
                                      parameters$RS_cor_input_rds_file_set_yaml_array,
                                      parameters$RS_cor_rds_file_path)

        cor_bdprob       = src_prob_and_path_list$src_Xu_bd_problem
        src_rds_file_dir = src_prob_and_path_list$src_rds_file_dir

        # bdpg::do_COR_marxan_analysis_and_output (cor_bdprob,
        #                                          parameters,
        #                                          src_rds_file_dir)
        do_COR_rs_analysis_and_output (cor_bdprob, parameters, src_rds_file_dir)
        }

    #---------------------------------------------------------------------------

        #---------------------------------------------------------------
        #  Run a reserve selector on an apparent problem if requested.
        #---------------------------------------------------------------

    if (run_rs_on_APP_prob)
        {
#        app_bdprob =
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$RS_app_input_prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$RS_app_input_rds_file_set_path,
                                        #  never used?
                                      parameters$RS_app_input_rds_file_set_yaml_array,
                                      parameters$RS_app_rds_file_path)

        app_bdprob       = src_prob_and_path_list$src_Xu_bd_problem
        src_rds_file_dir = src_prob_and_path_list$src_rds_file_dir

        cor_bdprob = get_base_cor_bdprob_for_given_app_bdprob (app_bdprob,
                                                               parameters)

        # bdpg::do_APP_marxan_analysis_and_output (app_bdprob,
        #                                          cor_bdprob,
        #                                          parameters,
        #                                          src_rds_file_dir)
        do_APP_rs_analysis_and_output (app_bdprob,
                                       cor_bdprob,
                                       parameters,
                                       src_rds_file_dir)
        }

    #---------------------------------------------------------------------------

    return()
    }

#===============================================================================

