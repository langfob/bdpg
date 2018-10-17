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

#-------------------------------------------------------------------------------

load_existing_tzar_run_from_glass <- function ()
    {
    dir_to_scp_into = "/Users/bill/Downloads/test"
#    cat ("\nDir to scp into = ", dir_to_scp_into, "\n")

    setwd (dir_to_scp_into)
#    cat ("\nAbout to scp, sitting in: '", getwd(), "'\n")

if (FALSE)
{
        #  Test using a single source file instead of a directory.
    src_filename_or_dirname_to_scp = "/home/rdv/tzar_output/bdpg_nectar/Old_runs/20321_easy/RSprob-APP-Base.0649e4f9-d5c7-44d4-8481-3abd7ce7f666/saved.RSprob-APP-Base.0649e4f9-d5c7-44d4-8481-3abd7ce7f666.rds"
#    cat ("\n\nsrc_filename_or_dirname_to_scp to scp from = '", src_filename_or_dirname_to_scp, "'\n")
}

        #  Test using a source directory instead of a single file.
    src_filename_or_dirname_to_scp = "/home/rdv/tzar_output/bdpg_nectar/Old_runs/20321_easy"
#    cat ("\n\nsrc_filename_or_dirname_to_scp to scp from = '", src_filename_or_dirname_to_scp, "'\n")

    scp_one_existing_tzar_run (src_filename_or_dirname_to_scp,
                               dir_to_scp_into)
    }

#-------------------------------------------------------------------------------

scp_one_existing_tzar_run <- function (src_filename_or_dirname_to_scp,
                                       dir_to_scp_into)
    {
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

        #  Find or add code to scp the directory to the right place in the
        #  current run.
        #  May need to scp it into a temporary area of its own on the
        #  nectar machine and then copy from that area into the new tzar run
        #  in progress on the nectar machine.
        #  However, there might be some conflict between the old log file and
        #  overwriting the new log file etc.
        #  Make an appropriated named copy of the original log file from the
        #  original run so that can be tracked.
            #  Not sure, will tzar append to it already or do I need to do
            #  some fiddling when I scp (and unzip?) the original directories
            #  so that the current log doesn't get clobbered (overwritten)?

    original_dir = getwd()
    #dir_to_scp_into = "/Users/bill/Downloads/test"
    cat ("\nDir to scp into = ", dir_to_scp_into, "\n")

    setwd (dir_to_scp_into)
    cat ("\nAbout to scp, sitting in: '", getwd(), "'\n")

    #src_filename_or_dirname_to_scp = "/home/rdv/tzar_output/bdpg_nectar/Old_runs/20321_easy/RSprob-APP-Base.0649e4f9-d5c7-44d4-8481-3abd7ce7f666/saved.RSprob-APP-Base.0649e4f9-d5c7-44d4-8481-3abd7ce7f666.rds"
    cat ("\n\nsrc_filename_or_dirname_to_scp to scp from = '", src_filename_or_dirname_to_scp, "'\n")

    scpcmd = paste0 ("scp    -r    rdv@glass.eres.rmit.edu.au:", src_filename_or_dirname_to_scp, "    .")
    cat ("\n\nscpcmd = '", scpcmd, "'\n")

    retval = -555
    retval = system (scpcmd)
    cat ("\nAfter scp cmd, retval = '", retval, "'\n")

    setwd (original_dir)
    cat ("After finishing scp, sitting in: '", getwd(), "'\n")

    #  Move old metadata directory to the
    #  current tzar run's metadata area as an appropriately named subdirectory.

    #  Move remaining old data from landing area to current tzar run's
    #  working area.

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

    #  Test
if (FALSE)
    {
    getwd()
    load_existing_tzar_run_from_glass()
    getwd()
    }

#===============================================================================

