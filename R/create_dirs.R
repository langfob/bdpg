#===============================================================================

                            #  create_dirs.R

#===============================================================================

create_RSprob_dir_and_subdirs <- function (top_dir,        #  tzar out dir
                                           prob_dir_name,  #  e.g., the problem's UUID
                                           RSprob_plot_dir_name,
                                           RSprob_network_dir_name,
                                           create_dirs = TRUE)
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path     <- file.path (top_dir, prob_dir_name)

    plots_dir_path   <- file.path (top_dir, prob_dir_name, RSprob_plot_dir_name)
    network_dir_path <- file.path (top_dir, prob_dir_name, RSprob_network_dir_name)

    cat ("\ncreate_RSprob_dir_and_subdirs:")
    cat ("\n    top_dir_path = '",     top_dir_path, "'",     sep='')
    cat ("\n    plots_dir_path = '",   plots_dir_path, "'",   sep='')
    cat ("\n    network_dir_path = '", network_dir_path, "'", sep='')

        #------------------------------------------------------------------
        #  Create the directories if not just testing the creation of the
        #  path strings.
        #------------------------------------------------------------------

    if (create_dirs)
        {
        dir.create (top_dir_path,     showWarnings = TRUE, recursive = TRUE)
        dir.create (plots_dir_path,   showWarnings = TRUE, recursive = TRUE)
        dir.create (network_dir_path, showWarnings = TRUE, recursive = TRUE)
        }
    }

#===============================================================================

create_RSrun_dir_and_subdirs <- function (top_dir,
                                          run_dir_name,  #  e.g., the run's UUID
                                          RSprob_input_dir_name,
                                          RSprob_output_dir_name,
                                          create_dirs = TRUE)
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path    <- file.path (top_dir, run_dir_name)

    input_dir_path  <- file.path (top_dir, run_dir_name, RSprob_input_dir_name)
    output_dir_path <- file.path (top_dir, run_dir_name, RSprob_output_dir_name)

    cat ("\ncreate_RSrun_dir_and_subdirs:")
    cat ("\n    top_dir_path = '",    top_dir_path, "'",    sep='')
    cat ("\n    input_dir_path = '",  input_dir_path, "'",  sep='')
    cat ("\n    output_dir_path = '", output_dir_path, "'", sep='')

        #------------------------------------------------------------------
        #  Create the directories if not just testing the creation of the
        #  path strings.
        #------------------------------------------------------------------

    if (create_dirs)
        {
        dir.create (top_dir_path,    showWarnings = TRUE, recursive = TRUE)
        dir.create (input_dir_path,  showWarnings = TRUE, recursive = TRUE)
        dir.create (output_dir_path, showWarnings = TRUE, recursive = TRUE)
        }
    }

#===============================================================================

