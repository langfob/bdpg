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

    if (is.null (prob_src))
        {
        stop_bdpg ("No prob_src given")

        } else if (prob_src == prob_from_rds_file)
        {
        Xu_bdprob = load_saved_obj_from_file (normalizePath (rds_file_path))

        } else if (prob_src == prob_from_rds_file_set_from_file)
        {
        rds_file_set  = readLines (rds_file_set_path)
        rds_file_path = rds_file_set [cur_input_prob_idx]
        Xu_bdprob     = load_saved_obj_from_file (normalizePath (rds_file_path))

        } else if (prob_src == prob_from_rds_file_set_from_yaml_array)
        {
        rds_file_set  = rds_file_set_yaml_array
        rds_file_path = rds_file_set [cur_input_prob_idx]
        Xu_bdprob     = load_saved_obj_from_file (normalizePath (rds_file_path))

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

single_action_using_tzar_reps <- function (parameters, starting_dir, integerize)
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
    run_network_metrics_on_prob = value_or_FALSE_if_null (parameters$run_network_metrics_on_prob)

    num_actions_chosen = gen_COR_prob + gen_WRAP_prob + gen_APP_prob +
                         run_rs_on_COR_prob + run_rs_on_APP_prob +
                         run_network_metrics_on_prob

    if (num_actions_chosen != 1)
        stop_bdpg (paste0 ("\nMust set 1 and only 1 of these variables to TRUE: ",
                      "gen_COR_prob (", gen_COR_prob, "), ",
                      "gen_WRAP_prob (", gen_WRAP_prob, "), ",
                      "gen_APP_prob (", gen_APP_prob, "), ",
                      "run_rs_on_COR_prob (", run_rs_on_COR_prob, "), ",
                      "run_rs_on_APP_prob (", run_rs_on_APP_prob, "), ",
                      "run_network_metrics_on_prob (", run_network_metrics_on_prob, "), ",
                      "\n"))

    #---------------------------------------------------------------------------

        #--------------------------------------------
        #  Generate a correct problem if requested.
        #--------------------------------------------

    if (gen_COR_prob)
        {
        gen_single_bdprob_COR (parameters,
                               starting_dir,
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
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$rds_file_set_path,
                                      parameters$rds_file_set_yaml_array,
                                      parameters$rds_file_path)

        src_bdprob_to_wrap = src_prob_and_path_list$src_Xu_bd_problem
        src_rds_file_dir   = src_prob_and_path_list$src_rds_file_dir

        gen_single_bdprob_WRAP (src_bdprob_to_wrap, parameters, starting_dir)
        }

    #---------------------------------------------------------------------------

        #----------------------------------------------
        #  Generate an apparent problem if requested,
        #  i.e., add error to a correct problem.
        #----------------------------------------------

    if (gen_APP_prob)
        {
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$rds_file_set_path,
                                      parameters$rds_file_set_yaml_array,
                                      parameters$rds_file_path)

        bdprob_to_add_error_to = src_prob_and_path_list$src_Xu_bd_problem

# MODIFY gen_single_bdprob_APP() TO SET THE COMPOUND ERROR NAME IF IT COMES
# INTO THE FUNCTION LIST AS A NULL?
# ALSO NOTE THAT THE gen_1_app_variant() FUNCTION CALLS THE RESERVE SELECTORS
# TOO AND WE DON'T WANT THAT HERE, SO WE CAN'T USE THAT FUNCTION HERE.
#  ALSO NEED TO ADD BOOLEAN ARGUMENTS ABOUT WHETHER TO GENERATE COST AND/OR
#  FP/FN ERRORS SINCE THAT HAS BEEN ADDED TO THE CALL FOR gen_single_bdprob_APP()
#  ON 2018 03 07.

        gen_single_bdprob_APP (bdprob_to_add_error_to, parameters, starting_dir)
        }

    #---------------------------------------------------------------------------

        #------------------------------------------------------------------
        #  Run a reserve selector on a correct problem (base or wrapped),
        #  if requested.
        #------------------------------------------------------------------

    if (run_rs_on_COR_prob)
        {
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$rds_file_set_path,
                                      parameters$rds_file_set_yaml_array,
                                      parameters$rds_file_path)

        cor_bdprob       = src_prob_and_path_list$src_Xu_bd_problem
        src_rds_file_dir = src_prob_and_path_list$src_rds_file_dir

        do_rs_analysis_and_output (cor_bdprob,
                                       cor_bdprob,
                                       parameters,
                                   starting_dir,
                                       src_rds_file_dir)
        }

    #---------------------------------------------------------------------------

        #---------------------------------------------------------------
        #  Run a reserve selector on an apparent problem if requested.
        #---------------------------------------------------------------

    if (run_rs_on_APP_prob)
        {
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$rds_file_set_path,
                                      parameters$rds_file_set_yaml_array,
                                      parameters$rds_file_path)

        app_bdprob       = src_prob_and_path_list$src_Xu_bd_problem
        src_rds_file_dir = src_prob_and_path_list$src_rds_file_dir

        cor_bdprob = get_base_cor_bdprob_for_given_app_bdprob (app_bdprob,
                                                               parameters)

        do_rs_analysis_and_output (app_bdprob,
                                       cor_bdprob,
                                       parameters,
                                   starting_dir,
                                       src_rds_file_dir)
        }

    #---------------------------------------------------------------------------

        #--------------------------------------------------
        #  Run network metrics on a problem if requested.
        #--------------------------------------------------
        #  Note that we're not using the existing parameter
        #  called compute_network_metrics to control this
        #  process because if that was turned on for
        #  generating a problem above, it would have already
        #  caused the generation of network metrics.
        #  We need to separate that process from this one
        #  where we're trying to take a set of existing
        #  COR or APP problems that don't already have their
        #  network metrics computed and do those computations
        #  in a separate batch from the original problem
        #  creation.  The reason for this is that some of
        #  the network metrics are really slow and aren't
        #  needed for just measuring reserve selector
        #  performance.  They're intended for use as
        #  performance prediction features later on in the
        #  process.  Separating them out like this allows
        #  running lots of experiments to generate problems
        #  and do reserve selection to get results for those
        #  first.  These slow network metrics can then be
        #  run while those initial results are being
        #  analyzed.  Similarly, if you only ran a small
        #  subset of the metrics at some point and want to
        #  run a bigger set of metrics later, you can use
        #  this to do that.
        #--------------------------------------------------

    if (run_network_metrics_on_prob)
        {
        src_prob_and_path_list =
            get_bdprob_from_rds_file (parameters$prob_src,
                                      parameters$cur_input_prob_idx,
                                      parameters$rds_file_set_path,
                                      parameters$rds_file_set_yaml_array,
                                      parameters$rds_file_path)

        net_bdprob       = src_prob_and_path_list$src_Xu_bd_problem
        src_rds_file_dir = src_prob_and_path_list$src_rds_file_dir

            #------------------------------------------------------------
            #  Creating network metric output is a bit different from
            #  creating rsprobs or rsruns because the network output
            #  is attached to the problem and usually goes into a
            #  subdirectory of the problem's general directory.
            #  When running these in batch like this, the problem's
            #  directory probably no longer exists.
            #  At a minimum, we want to attach the metric outputs back
            #  onto the problem object in its slots for network output
            #  and then write the modified problem object back out.
            #  If we do that, we want to be careful not to mess up the
            #  original object if something goes wrong.  We may also
            #  want to preserve that original object anyway for some
            #  other reason (e.g., it had a different set of network
            #  metrics attached to it).
            #  So, we will want to write the problem back out into a
            #  different spot instead of just writing over the top of
            #  the input problem.
            #------------------------------------------------------------

#        exp_root_dir = file.path (normalizePath (parameters$full_output_dir_with_slash))
        exp_root_dir = file.path (normalizePath (starting_dir))

        create_RSprob_dir_and_subdirs (exp_root_dir, net_bdprob)

        net_bdprob = init_object_graph_data (net_bdprob,
                                             exp_root_dir,
                                             TRUE,
                                             TRUE,
                                             parameters$use_igraph_metrics,
                                             parameters$use_bipartite_metrics,
                                             parameters$bipartite_metrics_to_use,
                                             write_to_disk = TRUE
                                             )

        net_bdprob = save_rsprob (net_bdprob, exp_root_dir)
        }

    #---------------------------------------------------------------------------

    return()
    }

#===============================================================================

