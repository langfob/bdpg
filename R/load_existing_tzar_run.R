#===============================================================================

                        #  load_existing_tzar_run.R

#  Load existing tzar run(s) from a different machine using scp and then
#  perform new action(s) on the old run inside a new tzar run.

#  The point of this it to first allow creation of problems and fast reserve
#  selections that don't use a lot of memory to run on nectar using many
#  small machines and then come back later and run memory-intensive and/or
#  cpu intensive things like graph calculations or gurobi reserve selection
#  to be done in a separate pass on a new, smaller set of bigger memory/cpu
#  nectar machines.
#
#  See daily log for 2017-08-19 Saturday for earlier work related to something
#  similar to this in file single_action_using_tzar_reps.R.  That work only
#  loads specific files from the same disk rather than loading entire
#  tzar runs from a different machine.  It also has some other restrictions
#  like only allowing one action to be done at a time, etc.

#===============================================================================

#' Load an existing tzar run from glass

#' @export

#-------------------------------------------------------------------------------

load_existing_tzar_run_from_glass <- function (file_of_runs_to_load,
                                               prev_run_idx,
                                               tgt_filename_or_dirname_for_scp)
    {
    cat ("\nStarting in load_existing_tzar_run_from_glass().\n")

        #-------------------------------------------------------
        #  Load array of file names for old tzar runs to load
        #  in the current run set.
        #-------------------------------------------------------

    cat ("\n\nFile name for runs to load = '", file_of_runs_to_load, "'\n")

    runs_to_load = readLines (parameters$file_of_runs_to_load)
    for (iii in 1:length(runs_to_load))
        {
        cat ("\n", iii, " : ", runs_to_load [iii])
        }
    cat ("\n")

        #-------------------------------------------------------
        #  Get the file name of the one old tzar run to be
        #  acted on in the current tzar run inside the current
        #  tzar run set.
        #-------------------------------------------------------

    cat ("\nprev_run_idx = '", prev_run_idx, "'\n")
    prev_run_idx = vn (prev_run_idx, range_lo=1, range_hi=length(runs_to_load))

    cat ("\nruns_to_load [prev_run_idx] = '", runs_to_load [prev_run_idx], "'\n")
    src_filename_or_dirname_to_scp = runs_to_load [prev_run_idx]


        #---------------------------------------------------------
        #  scp that run into the current tzar run's output area.
        #---------------------------------------------------------

    tgt_filename_or_dirname_for_scp = parameters$full_output_dir_with_slash

    scp_one_existing_tzar_run (src_filename_or_dirname_to_scp,
                               tgt_filename_or_dirname_for_scp)

        #---------------------------------------------------------
        #  At this point, the old run's data is inside a directory
        #  named for the old run, but it needs to be moved up
        #  into the current run's output area where it can be
        #  acted on as if it had been created in the current run.
        #  So, move all of the files in the copy of the old run's
        #  directory up into the current run's output area and
        #  get rid of the old directory copy area that is now
        #  empty.
        #  There is one complication.  The old run is likely to
        #  have a tzar metatdata subdirectory that would end up
        #  being copied over the current run's metadata directory.
        #  Instead, move that old metadata directory into the
        #  current metadata directory, but put it there under
        #  a new directory whose name is that of the old run.
        #  This will allow you to identify where that metadata
        #  came from as well as allow loading this run again
        #  later if other actions need to be performed on it
        #  without getting all the metadata mixed up between
        #  the various runs.
        #
        #  To summarize what happens after the scp has finished:
        #
        #  STARTING DIR STRUCTURE BEFORE THIS ROUTINE:
        #  ------------------------------------------
        #      cur_run_output_dir
        #          metadata
        #          old_run_copy
        #              metadata
        #              RSprob1
        #              RSprob2
        #              ...
        #  BECOMES AFTER THIS ROUTINE:
        #  --------------------------
        #      cur_run_output_dir
        #          metadata
        #               old_run_copy
        #                  metadata
        #          RSprob1
        #          RSprob2
        #          ...
        #---------------------------------------------------------

    move_old_tzar_run_files_to_cur_run_locations (src_filename_or_dirname_to_scp,
                                                  tgt_filename_or_dirname_for_scp)

    cat ("\nAbout to return from load_existing_tzar_run_from_glass().\n")
    }

#===============================================================================

scp_one_existing_tzar_run <- function (src_filename_or_dirname_to_scp,
                                       tgt_filename_or_dirname_for_scp)
    {
    #---------------------------------------------------------------------
    #  To test:

        #  Create a file listing all tzar runs to reload.
        #
        #  Need this because we want each scheduled tzar run to grab a
        #  different old tzar run, so we can't just specify it as a tzar
        #  library (since you don't know which one it will be when you're
        #  inside the currently scheduled tzar run).

    #  Create the file by hand or by a script (e.g., on glass).

        #  Add line to the library section of project.yaml to force
        #  download of that file to the nectar machine at runtime.

    #  Add the line by hand.

        #  Add iterator to project.yaml to iterate through lines in the
        #  file of runs to download.

    #  Add the line/section by hand.

    #---------------------------------------------------------------------

    scpcmd = paste0 ("scp    -r    rdv@glass.eres.rmit.edu.au:",
                     src_filename_or_dirname_to_scp,
                     "    ", tgt_filename_or_dirname_for_scp)
    cat ("\n\nscpcmd = '", scpcmd, "'\n")

    retval = system (scpcmd)

    cat ("\nAfter scp cmd, retval = '", retval, "'\n")
    }

#===============================================================================

move_old_tzar_run_files_to_cur_run_locations <-
                                    function (src_filename_or_dirname_to_scp,
                                              tgt_filename_or_dirname_for_scp)
    {
        #-----------------------------------------------------------------------
        #  Move old metadata directory to the current tzar run's metadata area
        #  as a new subdirectory with the name of the old run and the old
        #  metadata directory inside that.
        #-----------------------------------------------------------------------

    name_of_old_run = basename (src_filename_or_dirname_to_scp)
    old_metadata_path = file.path (tgt_filename_or_dirname_for_scp, name_of_old_run, "metadata")

    if (dir.exists (old_metadata_path))
        {
        dir_for_old_metadata = file.path (tgt_filename_or_dirname_for_scp, "metadata", name_of_old_run)
        dir.create (dir_for_old_metadata)
        file.rename (old_metadata_path, file.path (dir_for_old_metadata, "metadata"))
        }

        #-----------------------------------------------------------------------
        #  Move remaining old data from landing area to current tzar run's
        #  working area.
        #  I don't know a graceful way to do this in R since it seems like
        #  it would require some kind of awkward fooling with lists of files
        #  and regular expressions and getting all that to work with R's
        #  file.rename command.
        #  However, it's trivial to do in the shell, so I'll just do it with
        #  a system call.
        #-----------------------------------------------------------------------

    path_to_copy_of_old_run = file.path (tgt_filename_or_dirname_for_scp, name_of_old_run)
    mvcmd = paste0 ("mv    ", path_to_copy_of_old_run, "/*",
                    "    ", tgt_filename_or_dirname_for_scp)
    cat ("\n\nmvcmd = '", mvcmd, "'\n")

    retval = system (mvcmd)

    cat ("\nAfter mv cmd, retval = '", retval, "'\n")

        #-----------------------------------------------------------------------
        #  Remove the now-empty directory that contained the copy of the
        #  old tzar run.
        #-----------------------------------------------------------------------

    unlink (path_to_copy_of_old_run, recursive = TRUE)

    #-----------------------------------------------

        #  Add code to cd (if necessary) or point to correct position to
        #  allow graph and/or gurobi calls as if done inside the original
        #  tzar run in the new tzar run's directory.
        #  May have to be careful about any fully specified paths in the
        #  existing output for the original run, though I think that everything
        #  is originally done from a relative path.

        #  Possibly cd to current run's working area if not there already.

            #  After that, you can begin doing whatever operations you want to do
            #  on each problem directory from the old problem.
            #  That will require a bunch of logic like what is already in
            #  single_action_using_tzar_reps.R.  Calls in there might work as is,
            #  but I suspect they will need modification...
    }

#===============================================================================

    #  Test code for command line.
if (FALSE)
    {
    getwd()
    load_existing_tzar_run_from_glass()
    getwd()
    }

#===============================================================================

