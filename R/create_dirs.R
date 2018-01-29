#===============================================================================

                            #  create_dirs.R

#===============================================================================

#' Build top directory name
#'
#' Builds a directory name for the highest level directory for a bdpg-related
#' object such as a reserve selection problem.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns a directory name (not a path)

#-------------------------------------------------------------------------------

build_topdir_name <- function (obj)
    {
    topdir_name <- paste0 (obj@file_name_prefix, ".", obj@UUID)

    return (topdir_name)
    }

#===============================================================================
    #  Shortcut functions to always build paths to RSprob dirs reliably.
#===============================================================================

#' Build top directory path for RS problem
#'
#' Build a full path to the highest level directory for a reserve selection
#' problem.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns character string path to highest level directory for a
#'     reserve selection problem

#-------------------------------------------------------------------------------

get_RSprob_path_topdir <- function (rsprob, exp_root_dir)
    {
    topdir <- file.path (exp_root_dir, build_topdir_name (rsprob))

    return (topdir)
}

#-------------------------------------------------------------------------------

#' Get path to plots directory of RS problem
#'
#' Build a full path to the directory where plots are written for a reserve
#' selection problem.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns character string path to directory where plots are written
#'     for a reserve selection problem

#-------------------------------------------------------------------------------

get_RSprob_path_plots <- function (rsprob, exp_root_dir)
    {
    plotsdir <- file.path (exp_root_dir, build_topdir_name (rsprob), rsprob@plot_output_dir)

    return (plotsdir)
    }

#-------------------------------------------------------------------------------

#' Get path to networks directory of RS problem
#'
#' Build a full path to the directory where networks are written for a reserve
#' selection problem.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns character string path to directory where networks are written
#'     for a reserve selection problem

#-------------------------------------------------------------------------------

get_RSprob_path_networks <- function (rsprob, exp_root_dir)
    {
    networksdir <- file.path (exp_root_dir, build_topdir_name (rsprob), rsprob@network_output_dir)

    return (networksdir)
    }

#===============================================================================

#' Create directories for an RS problem
#'
#' Create directory and subdirectory structure for an RS problem inside an
#' experiment.  Creates the highest level directory for the problem and the
#' plots and networks directories under that directory.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Doesn't return anything.

#-------------------------------------------------------------------------------

create_RSprob_dir_and_subdirs <- function (top_dir,  #  usually parameters$fullOutputDir_NO_slash
                                           rsprob)
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path     <- get_RSprob_path_topdir (rsprob, top_dir)
    plot_dir_path   <- get_RSprob_path_plots (rsprob, top_dir)
    network_dir_path <- get_RSprob_path_networks (rsprob, top_dir)

    cat ("\ncreate_RSprob_dir_and_subdirs:")
    cat ("\n    top_dir_path = '",     top_dir_path, "'",     sep='')
    cat ("\n    plot_dir_path = '",   plot_dir_path, "'",   sep='')
    cat ("\n    network_dir_path = '", network_dir_path, "'", sep='')

        #------------------------------------------------------------------
        #  Create the directories if not just testing the creation of the
        #  path strings.
        #------------------------------------------------------------------

    create_dirs = TRUE    #  manually set this to FALSE if testing
    if (create_dirs)
        {
        dir.create (top_dir_path,     showWarnings = TRUE, recursive = TRUE)
        dir.create (plot_dir_path,    showWarnings = TRUE, recursive = TRUE)
        dir.create (network_dir_path, showWarnings = TRUE, recursive = TRUE)
        }
    }

#===============================================================================

