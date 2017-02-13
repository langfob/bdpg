#===============================================================================

                            #  create_dirs.R

#===============================================================================

create_RSprob_dir_and_subdirs <- function (top_dir,          #  tzar out dir
                                           dir_name_stem,    #  obj uuid
                                           RSprob_dir_names,
                                           create_dirs = TRUE)
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path     <- file.path (top_dir, dir_name_stem)

    plots_dir_path   <- file.path (top_dir, dir_name_stem,
                                   RSprob_dir_names$plot_output_dir)
    network_dir_path <- file.path (top_dir, dir_name_stem,
                                   RSprob_dir_names$network_output_dir)

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
                                          dir_name_stem,
                                          RSrun_dir_names,
                                          create_dirs = TRUE)
    {
        #------------------------
        #  Create path strings.
        #------------------------

    top_dir_path    <- file.path (top_dir, dir_name_stem)

    input_dir_path  <- file.path (top_dir, dir_name_stem,
                                  RSrun_dir_names$input_dir)
    output_dir_path <- file.path (top_dir, dir_name_stem,
                                  RSrun_dir_names$output_dir)

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

