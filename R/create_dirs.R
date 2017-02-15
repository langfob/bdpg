#===============================================================================

                            #  create_dirs.R

#===============================================================================

    #  Shortcut functions to always build paths to RSprob dirs reliably.

get_RSprob_path_topdir <- function (rsprob, top_dir)
    {
    file.path (top_dir, rsprob@UUID)
    }

get_RSprob_path_plots <- function (rsprob, top_dir)
    {
    file.path (top_dir, rsprob@UUID, rsprob@plot_output_dir)
    }

get_RSprob_path_networks <- function (rsprob, top_dir)
    {
    file.path (top_dir, rsprob@UUID, rsprob@network_output_dir)
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

    create_dirs = TRUE    #  manually set this to FALSE if testing
    if (create_dirs)
        {
        dir.create (top_dir_path,     showWarnings = TRUE, recursive = TRUE)
        dir.create (plot_dir_path,    showWarnings = TRUE, recursive = TRUE)
        dir.create (network_dir_path, showWarnings = TRUE, recursive = TRUE)
        }
    }

#===============================================================================
#===============================================================================

    #  Shortcut functions to always build paths to RSrun dirs reliably.

get_RSrun_path_topdir <- function (rsrun, top_dir)
    {
    file.path (top_dir, rsrun@UUID)
    }

get_RSrun_path_IO <- function (rsrun, top_dir)
    {
    file.path (top_dir, rsrun@UUID)
    }

get_RSrun_path_input <- function (rsrun, top_dir)
    {
    file.path (top_dir, rsrun@UUID, rsrun@input_dir_name)
    }

get_RSrun_path_output <- function (rsrun, top_dir)
    {
    file.path (top_dir, rsrun@UUID, rsrun@output_dir_name)
    }

get_RSrun_path_plots <- function (rsrun, top_dir)
    {
    file.path (top_dir, rsrun@UUID, rsrun@plot_dir_name)
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
    }

#===============================================================================

