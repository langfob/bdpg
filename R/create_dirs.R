#===============================================================================

                            #  create_dirs.R

#===============================================================================

create_RSprob_dir_and_subdirs <- function (starting_dir,
                                           dir_name_stem,
                                           create_dirs = TRUE)
    {
    top_dir_path = file.path (starting_dir, dir_name_stem)

    cat ("\ncreate_topdir:", "\n    top_dir_path = '", top_dir_path, "'", sep='')

    if (create_dirs & !dir.exists (top_dir_path))
        dir.create (top_dir_path, showWarnings = TRUE, recursive = TRUE)

        #  Create directory names.
        #  Creating as a list now to hold the place of storing these
        #  names somewhere rather than hard-coding them.
        #  Will figure out where to put them later and then pass that
        #  structure in as an argument to this routine.

    RSprob_dir_names = list()
    RSprob_dir_names$plot_output_dir    = file.path (top_dir_path, "plots")
    RSprob_dir_names$network_output_dir = file.path (top_dir_path, "networks")

    if (create_dirs)
        {
            #  Create PLOT OUTPUT directory.
        dir.create (RSprob_dir_names$plot_output_dir,
                    showWarnings = TRUE, recursive = TRUE)

            #  Create NETWORK OUTPUT directory.
        dir.create (RSprob_dir_names$network_output_dir,
                    showWarnings = TRUE, recursive = TRUE)
        }

    }

#===============================================================================

create_RSrun_dir_and_subdirs <- function (starting_dir,
                                           dir_name_stem,
                                           create_dirs = TRUE)
    {
    top_dir_path = file.path (starting_dir, dir_name_stem)

    cat ("\ncreate_topdir:", "\n    top_dir_path = '", top_dir_path, "'", sep='')

    if (create_dirs & !dir.exists (top_dir_path))
        dir.create (top_dir_path, showWarnings = TRUE, recursive = TRUE)

        #  Create directory names.
        #  Creating as a list now to hold the place of storing these
        #  names somewhere rather than hard-coding them.
        #  Will figure out where to put them later and then pass that
        #  structure in as an argument to this routine.

    RSrun_dir_names = list()
    RSrun_dir_names$rsrun_input_dir    = file.path (top_dir_path, "input")
    RSrun_dir_names$rsrun_output_dir = file.path (top_dir_path, "output")

    if (create_dirs)
        {
            #  Create PLOT OUTPUT directory.
        dir.create (RSrun_dir_names$rsrun_input_dir,
                    showWarnings = TRUE, recursive = TRUE)

            #  Create NETWORK OUTPUT directory.
        dir.create (RSrun_dir_names$rsrun_output_dir,
                    showWarnings = TRUE, recursive = TRUE)
        }

    }

#===============================================================================

