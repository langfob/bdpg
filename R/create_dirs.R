#===============================================================================

                            #  create_dirs.R

#===============================================================================

get_RSprob_path_topdir <- function (rsprob, topdir)
    {
    file.path (topdir, rsprob@UUID)
    }

get_RSprob_path_plots <- function (rsprob, topdir)
    {
    file.path (topdir, rsprob@UUID, rsprob@plots_dir_name)
    }

get_RSprob_path_networks <- function (rsprob, topdir)
    {
    file.path (topdir, rsprob@UUID, rsprob@networks_dir_name)
    }

#===============================================================================

create_RSprob_dir_and_subdirs <- function (top_dir,  #  usually parameters$fullOutputDir_NO_slash
                                           rsprob)
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path     <- get_RSprob_path_topdir (rsprob, topdir)
    plots_dir_path   <- get_RSprob_path_plots (rsprob, topdir)
    network_dir_path <- get_RSprob_path_networks (rsprob, topdir)

    cat ("\ncreate_RSprob_dir_and_subdirs:")
    cat ("\n    top_dir_path = '",     top_dir_path, "'",     sep='')
    cat ("\n    plots_dir_path = '",   plots_dir_path, "'",   sep='')
    cat ("\n    network_dir_path = '", network_dir_path, "'", sep='')

        #------------------------------------------------------------------
        #  Create the directories if not just testing the creation of the
        #  path strings.
        #------------------------------------------------------------------

    create_dirs = TRUE    #  manually set this to FALSE if testing
    if (create_dirs)
        {
        dir.create (top_dir_path,     showWarnings = TRUE, recursive = TRUE)
        dir.create (plots_dir_path,   showWarnings = TRUE, recursive = TRUE)
        dir.create (network_dir_path, showWarnings = TRUE, recursive = TRUE)
        }
    }

#===============================================================================

get_RSrun_path_topdir <- function (rsrun, topdir)
    {
    file.path (topdir, rsrun@UUID)
    }

get_RSrun_path_IO <- function (rsrun, topdir)
    {
    file.path (topdir, rsrun@UUID)
    }

get_RSrun_path_input <- function (rsrun, topdir)
    {
    file.path (topdir, rsrun@UUID, rsrun@input_dir_name)
    }

get_RSrun_path_output <- function (rsrun, topdir)
    {
    file.path (topdir, rsrun@UUID, rsrun@output_dir_name)
    }

#===============================================================================

create_RSrun_dir_and_subdirs <- function (rsrun,
                                          top_dir)  #  usually parameters$fullOutputDir_NO_slash
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path    <- get_RSrun_path_topdir (rsrun, topdir)
    input_dir_path  <- get_RSrun_path_input (rsrun, topdir)
    output_dir_path <- get_RSrun_path_output (rsrun, topdir)

    cat ("\ncreate_RSrun_dir_and_subdirs:")
    cat ("\n    top_dir_path = '",    top_dir_path, "'",    sep='')
    cat ("\n    input_dir_path = '",  input_dir_path, "'",  sep='')
    cat ("\n    output_dir_path = '", output_dir_path, "'", sep='')

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
        }
    }

#===============================================================================

