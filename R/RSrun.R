#===============================================================================
#
#                               RSrun.R
#
#  Functions related to RSrun class.
#
#===============================================================================

    #  Shortcut functions to always build paths to RSrun dirs reliably.

get_RSrun_path_topdir <- function (rsrun, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsrun))
    }

get_RSrun_path_IO <- function (rsrun, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsrun))
    }

get_RSrun_path_input <- function (rsrun, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsrun), rsrun@input_dir_name)
    }

get_RSrun_path_output <- function (rsrun, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsrun), rsrun@output_dir_name)
    }

get_RSrun_path_plots <- function (rsrun, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsrun), rsrun@plot_dir_name)
    }

#===============================================================================

#' Create directories for an RS run
#'
#' Create directory and subdirectory structure for an RS run inside an
#' experiment.  Creates the highest level directory for the run and the
#' input, output, and plots directories under that directory.
#'
#' @inheritParams std_param_defns
#'
#' @return Doesn't return anything.

#-------------------------------------------------------------------------------

create_RSrun_dir_and_subdirs <- function (rsrun,
                                          exp_root_dir)  #  usually parameters$fullOutputDir_NO_slash
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path    <- get_RSrun_path_topdir (rsrun, exp_root_dir)
    input_dir_path  <- get_RSrun_path_input (rsrun, exp_root_dir)
    output_dir_path <- get_RSrun_path_output (rsrun, exp_root_dir)
    plot_dir_path   <- get_RSrun_path_plots (rsrun, exp_root_dir)

    cat ("\ncreate_RSrun_dir_and_subdirs:")
    cat ("\n    top_dir_path = '",    top_dir_path, "'",    sep='')
    cat ("\n    input_dir_path = '",  input_dir_path, "'",  sep='')
    cat ("\n    output_dir_path = '", output_dir_path, "'", sep='')
    cat ("\n    plot_dir_path = '", plot_dir_path, "'", sep='')

        #------------------------------------------------------------------
        #  Create the directories if not just testing the creation of the
        #  path strings.
        #------------------------------------------------------------------

    create_dirs = TRUE    #  manually set this to FALSE if testing
    if (create_dirs)
        {
        dir.create (top_dir_path,    showWarnings = TRUE, recursive = TRUE)
        dir.create (input_dir_path,  showWarnings = TRUE, recursive = TRUE)
        dir.create (output_dir_path, showWarnings = TRUE, recursive = TRUE)
        dir.create (plot_dir_path,   showWarnings = TRUE, recursive = TRUE)
        }
    }

#===============================================================================
#===============================================================================
#===============================================================================

#' Create an RSrun
#'
#' Create a run of a reserve selector
#'
#-------------------------------------------------------------------------------

#' @param prob_UUID UUID for the biodiversity problem the reserve selector is
#'     run over
#' @param cor_or_app_str character string
#' @param basic_or_wrapped_or_comb_str character string
#' @inheritParams std_param_defns
#'
#' @return Returns an RSrun object

#-------------------------------------------------------------------------------

create_RSrun <- function (prob_UUID,
                          spp_rep_targets,
                          parameters,
                          cor_or_app_str,
                          basic_or_wrapped_or_comb_str,
                          rs_method_name
                          )
    {
    location_string = paste0 ("Start of create_RSrun(),",
                              cor_or_app_str, ",", basic_or_wrapped_or_comb_str)
    new_seed_list =
        set_new_or_forced_rand_seed_if_necessary (is_rsrun = TRUE,
                                                  is_rsprob = FALSE,
                                                  parameters,
                                                  cor_or_app_str,
                                                  basic_or_wrapped_or_comb_str,
                                                  location_string)

    return (create_RSrun_core (prob_UUID,
                                 spp_rep_targets,
                                 parameters,
                                 cor_or_app_str,
                                 basic_or_wrapped_or_comb_str,
                                 rs_method_name,
                                 new_seed_list
                                 ))
    }

#===============================================================================

#' Create an RSrun
#'
#' Create a run of a reserve selector
#'
#-------------------------------------------------------------------------------

#' @param repro_RDS_file_loc path to RDS file to be loaded
#' @param fullOutputDir_NO_slash full path to directory where output should go
#'     (with no slash on the end of the path)
#'
#' @return Returns an RSrun object
#'
#' @export

#-------------------------------------------------------------------------------

repro_RSrun <- function (repro_RDS_file_loc,
                         fullOutputDir_NO_slash = NULL)    #"~/Downloads")
    {
    repro = load_saved_obj_from_file (repro_RDS_file_loc)

        #-----------------------------------------------------------------------
        #  If a different output area has been provided,
        #  reset the slot for the output area in the original parameters list.
        #-----------------------------------------------------------------------

    parameters = repro$parameters
    if (! is.null (fullOutputDir_NO_slash) & ! is.na (fullOutputDir_NO_slash))
        parameters$fullOutputDir_NO_slash = fullOutputDir_NO_slash

    rsrun = repro$rsrun
    prob_UUID                    = rsrun@run_on_prob_UUID
    spp_rep_targets              = rsrun@targets
    cor_or_app_str               = rsrun@cor_or_app_str
    basic_or_wrapped_or_comb_str = rsrun@basic_or_wrapped_or_comb_str
    rs_method_name               = rsrun@rs_method_name
    new_seed_list                =
        list (seed_value = rsrun@rand_seed,
              R_internal_seed_array = rsrun@R_internal_seed_array)

#  Reset the random seed to match the previous run.
#    set.seed (new_seed_list$R_internal_seed_array)
.Random.seed <<- new_seed_list$R_internal_seed_array

    return (create_RSrun_core (prob_UUID,
                                 spp_rep_targets,
                                 parameters,
                                 cor_or_app_str,
                                 basic_or_wrapped_or_comb_str,
                                 rs_method_name,
                                 new_seed_list
                                 ))
    }

#===============================================================================

create_RSrun_core <- function (prob_UUID,
                                 spp_rep_targets,
                                 parameters,
                                 cor_or_app_str,
                                 basic_or_wrapped_or_comb_str,
                                 rs_method_name,
                                 new_seed_list
                                 )
    {
    #------------------------------------------------------------------

    rsrun <- new ("RSrun")

    rsrun@UUID             <- uuid::UUIDgenerate()
cat ("\n\n>>>>> Creating rsrun ", rsrun@UUID)

    rsrun@run_on_prob_UUID <- prob_UUID

    rsrun@rand_seed             = new_seed_list$seed_value
    rsrun@R_internal_seed_array = new_seed_list$R_internal_seed_array

    rsrun@targets  <- spp_rep_targets

    rsrun@obj_type_str   = "RSrun_"
    rsrun@rs_method_name = rs_method_name
    rsrun@cor_or_app_str = cor_or_app_str
    rsrun@basic_or_wrapped_or_comb_str = basic_or_wrapped_or_comb_str


    rsrun@file_name_prefix =
                            paste (rsrun@obj_type_str,
                                   rsrun@cor_or_app_str,
                                   rsrun@basic_or_wrapped_or_comb_str,
                                   rsrun@rs_method_name,
                                   sep='-')

    starting_dir = parameters$fullOutputDir_NO_slash

    create_RSrun_dir_and_subdirs (rsrun, starting_dir)

    rsrun <- save_RSrun (rsrun, starting_dir)

    return (rsrun)
    }

#===============================================================================

save_RSrun <- function (rsrun, starting_dir)
    {
    base_outdir = get_RSrun_path_topdir (rsrun, starting_dir)
    rsrun       = save_obj_with_checksum (rsrun,
                                          base_outdir)
    return (rsrun)
    }

#===============================================================================

