#===============================================================================

                            #  create_dirs.R

#===============================================================================

build_topdir_name <- function (obj)
    {
    topdir_name <- paste0 (obj@file_name_prefix, ".", obj@UUID)
doc_vars_in_this_func ()

    return (topdir_name)
    }

#===============================================================================

    #  Shortcut functions to always build paths to RSprob dirs reliably.

get_RSprob_path_topdir <- function (rsprob, exp_root_dir)
    {
    topdir <- file.path (exp_root_dir, build_topdir_name (rsprob))
doc_vars_in_this_func ()

    return (topdir)
    }

get_RSprob_path_plots <- function (rsprob, exp_root_dir)
    {
    plotsdir <- file.path (exp_root_dir, build_topdir_name (rsprob), rsprob@plot_output_dir)
doc_vars_in_this_func ()

    return (plotsdir)
    }

get_RSprob_path_networks <- function (rsprob, exp_root_dir)
    {
    networksdir <- file.path (exp_root_dir, build_topdir_name (rsprob), rsprob@network_output_dir)
doc_vars_in_this_func ()

    return (networksdir)
    }

#===============================================================================

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
#browser()
    create_dirs = TRUE    #  manually set this to FALSE if testing
    if (create_dirs)
        {
        dir.create (top_dir_path,     showWarnings = TRUE, recursive = TRUE)
        dir.create (plot_dir_path,    showWarnings = TRUE, recursive = TRUE)
        dir.create (network_dir_path, showWarnings = TRUE, recursive = TRUE)
        }
doc_vars_in_this_func ()
    }

#===============================================================================
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

create_RSrun_dir_and_subdirs <- function (rsrun,
                                          top_dir)  #  usually parameters$fullOutputDir_NO_slash
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path    <- get_RSrun_path_topdir (rsrun, top_dir)
    input_dir_path  <- get_RSrun_path_input (rsrun, top_dir)
    output_dir_path <- get_RSrun_path_output (rsrun, top_dir)
    plot_dir_path   <- get_RSrun_path_plots (rsrun, top_dir)

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
doc_vars_in_this_func ()
    }

#===============================================================================

