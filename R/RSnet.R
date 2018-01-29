#===============================================================================
#
#                               RSnet.R
#
#  Functions related to RSnet class.
#
#===============================================================================

    #  Shortcut functions to always build paths to RSnet dirs reliably.

get_RSnet_path_topdir <- function (rsnet, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsnet))
    }

get_RSnet_path_IO <- function (rsnet, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsnet))
    }

get_RSnet_path_input <- function (rsnet, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsnet), rsnet@input_dir_name)
    }

get_RSnet_path_output <- function (rsnet, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsnet), rsnet@output_dir_name)
    }

get_RSnet_path_plots <- function (rsnet, exp_root_dir)
    {
    file.path (exp_root_dir, build_topdir_name (rsnet), rsnet@plot_dir_name)
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

create_RSnet_dir_and_subdirs <- function (rsnet,
                                          exp_root_dir)  #  usually parameters$fullOutputDir_NO_slash
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path    <- get_RSnet_path_topdir (rsnet, exp_root_dir)
    input_dir_path  <- get_RSnet_path_input (rsnet, exp_root_dir)
    output_dir_path <- get_RSnet_path_output (rsnet, exp_root_dir)
    plot_dir_path   <- get_RSnet_path_plots (rsnet, exp_root_dir)

    cat ("\ncreate_RSnet_dir_and_subdirs:")
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

#' Create an RSnet
#'
#' Create a run of a reserve selector
#'
#-------------------------------------------------------------------------------

#' @param prob_UUID UUID for the biodiversity problem the network metrics are
#'     run over
#' @param cor_or_app_str character string
#' @param basic_or_wrapped_or_comb_str character string
#' @inheritParams std_param_defns
#'
#' @return Returns an RSnet object

#-------------------------------------------------------------------------------

create_RSnet <- function (prob_UUID,
                          parameters,
                          cor_or_app_str,
                          basic_or_wrapped_or_comb_str,
                          net_method_name
                          )
    {
    rsnet <- new ("RSnet")

    rsnet@UUID             <- uuid::UUIDgenerate()
    rsnet@run_on_prob_UUID <- prob_UUID

    rsnet@obj_type_str   = "RSnet_"
    rsnet@net_method_name = net_method_name
    rsnet@cor_or_app_str = cor_or_app_str
    rsnet@basic_or_wrapped_or_comb_str = basic_or_wrapped_or_comb_str

    rsnet@file_name_prefix =
                            paste (rsnet@obj_type_str,
                                   rsnet@cor_or_app_str,
                                   rsnet@basic_or_wrapped_or_comb_str,
                                   rsnet@net_method_name,
                                   sep='-')

    starting_dir = parameters$fullOutputDir_NO_slash

    create_RSnet_dir_and_subdirs (rsnet, starting_dir)

    rsnet <- save_RSnet (rsnet, starting_dir)

    return (rsnet)
    }

#===============================================================================

save_RSnet <- function (rsnet, starting_dir)
    {
    base_outdir = get_RSnet_path_topdir (rsnet, starting_dir)
    rsnet       = save_obj_with_checksum (rsnet,
                                          base_outdir)
    return (rsnet)
    }

#===============================================================================

