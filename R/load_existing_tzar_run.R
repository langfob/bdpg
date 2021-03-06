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
#  similar to this in file single_action_using_tzar_reps.R.  Much of the code
#  in here was initially cloned from that earlier file.  That work only
#  loads specific files from the same disk rather than loading entire
#  tzar runs from a different machine.  It also has some other restrictions
#  like only allowing one action to be done at a time, etc.

#===============================================================================

#' Load an existing tzar run from glass

#' @export

#-------------------------------------------------------------------------------

load_existing_tzar_run_from_glass <- function (parameters,
                                               file_of_runs_to_load,
                                               prev_run_idx,
                                               tgt_filename_or_dirname_for_scp)
    {
    cat ("\nStarting in load_existing_tzar_run_from_glass().\n")
    cat ("\ntgt_filename_or_dirname_for_scp = '", tgt_filename_or_dirname_for_scp, "'\n")

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

#    tgt_filename_or_dirname_for_scp = parameters$full_output_dir_with_slash

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







    act_on_loaded_existing_tzar_run (tgt_filename_or_dirname_for_scp,
                                     parameters)





    cat ("\nAbout to return from load_existing_tzar_run_from_glass().\n")
    }

#===============================================================================

scp_one_existing_tzar_run <- function (src_filename_or_dirname_to_scp,
                                       tgt_filename_or_dirname_for_scp)
    {
    cat ("\nStarting scp_one_existing_tzar_run().\n")
    cat ("\nsrc_filename_or_dirname_to_scp = '", src_filename_or_dirname_to_scp, "'\n")
    cat ("\ntgt_filename_or_dirname_for_scp = '", tgt_filename_or_dirname_for_scp, "'\n")

    #---------------------------------------------------------------------
    #  To test this routine:

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
    cat ("\nStarting move_old_tzar_run_files_to_cur_run_locations().\n")
    cat ("\nsrc_filename_or_dirname_to_scp = '", src_filename_or_dirname_to_scp, "'\n")
    cat ("\ntgt_filename_or_dirname_for_scp = '", tgt_filename_or_dirname_for_scp, "'\n")

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

get_list_of_RSprob_dirs <- function (lookindir)
    {
    return (list.files (lookindir, pattern="RSprob*"))
    }

#===============================================================================

get_list_of_RSprob_COR_dirs <- function (lookindir)
    {
    return (list.files (lookindir, pattern="RSprob-COR*"))
    }

#===============================================================================

get_list_of_RSprob_APP_dirs <- function (lookindir)
    {
    return (list.files (lookindir, pattern="RSprob-APP*"))
    }

#===============================================================================
#===============================================================================

get_full_path_for_loaded_bdprob <- function (tgt_filename_or_dirname_for_scp,
                                             cur_prob_dir)
    {
    cur_prob_dir = file.path (tgt_filename_or_dirname_for_scp, cur_prob_dir)

    rds_file_name = list.files (cur_prob_dir, pattern="saved.RSprob*")
    rds_file_path = file.path (cur_prob_dir, rds_file_name [1])  #  There should only be one element in the rds_file_name vector.

    full_path_for_loaded_bdprob = normalizePath (rds_file_path)

    return (full_path_for_loaded_bdprob)
    }

#-------------------------------------------------------------------------------

get_loaded_bdprob <- function (tgt_filename_or_dirname_for_scp,
                               cur_prob_dir)
    {
# cur_prob_dir = file.path (tgt_filename_or_dirname_for_scp, cur_prob_dir)
#
# rds_file_name = list.files (cur_prob_dir, pattern="saved.RSprob*")
# rds_file_path = file.path (cur_prob_dir, rds_file_name [1])  #  There should only be one element in the rds_file_name vector.
#
# bdprob = load_saved_obj_from_file (normalizePath (rds_file_path))

    full_path_for_loaded_bdprob =
        get_full_path_for_loaded_bdprob (tgt_filename_or_dirname_for_scp,
                                         cur_prob_dir)

    bdprob = load_saved_obj_from_file (full_path_for_loaded_bdprob)

    return (bdprob)
    }

#-------------------------------------------------------------------------------

get_loaded_cor_bdprob_for_loaded_app_bdprob <-
    function (tgt_filename_or_dirname_for_scp,
              app_bdprob,
              parameters)
    {
    cat ("\nStarting get_loaded_cor_bdprob_for_loaded_app_bdprob().\n")
    cat ("\ntgt_filename_or_dirname_for_scp = '", tgt_filename_or_dirname_for_scp, "'\n")

    app_bdprob_base_UUID =
            app_bdprob@APP_prob_info@UUID_of_base_problem_that_has_err_added

    cor_rds_file_list_file_name = paste0 (tgt_filename_or_dirname_for_scp,
                                        "/tmp_cor_file_list.txt")

    lscmd = paste0 ("ls ", tgt_filename_or_dirname_for_scp,
                  "/RSprob-COR*/saved* > ",
                  cor_rds_file_list_file_name)

    retval = system (lscmd)
    cat ("\nAfter ls cmd, retval = '", retval, "'\n")


    # list_of_RSprob_COR_rds_files = readlines ()
    #     get_list_of_RSprob_COR_dirs (tgt_filename_or_dirname_for_scp)
    #
    # list.files (lookindir, pattern="RSprob-COR*/saved.RSprob-COR*")

    list_of_RSprob_COR_rds_files = readLines (cor_rds_file_list_file_name)
    file.remove (cor_rds_file_list_file_name)

    cor_bdprob =
      look_up_object_by_UUID (app_bdprob_base_UUID,
                              list_of_RSprob_COR_rds_files)

    make_sure_that_cor_bdprob_is_base_of_app_bdprob (app_bdprob,
                                                   cor_bdprob,
                                                   parameters)

    return (cor_bdprob)
    }

#===============================================================================
#===============================================================================

run_graph_metrics_on_RSprob <- function (tgt_filename_or_dirname_for_scp,
                                         cur_prob_dir,
                                         parameters)
    {

    full_path_for_loaded_bdprob =
        get_full_path_for_loaded_bdprob (tgt_filename_or_dirname_for_scp,
                                         cur_prob_dir)

    net_bdprob = load_saved_obj_from_file (full_path_for_loaded_bdprob)


#  NEED TO CLEAR ANY OLD NETWORK RESULTS IN
#      - NETWORK DIRECTORY?
#      - OLD OBJECT'S NETWORK SLOTS?

    net_bdprob = init_object_graph_data (net_bdprob,
                                         tgt_filename_or_dirname_for_scp,    #  exp_root_dir,
                                         TRUE,
                                         TRUE,
                                         parameters$use_igraph_metrics,
                                         parameters$use_bipartite_metrics,
                                         parameters$bipartite_metrics_to_use,
                                         write_to_disk = TRUE
                                         )

        #  Remove old saved problem and replace it with the new one.

#  Also, are the UUID and the checksum of the problem correct?
#  Need to make sure that it stays the same as the original UUID but has a
#  new checksum since the graph metrics have changed.

    if (file.exists(full_path_for_loaded_bdprob))
        {
        file.remove(full_path_for_loaded_bdprob)
        }

    net_bdprob = save_rsprob (net_bdprob, tgt_filename_or_dirname_for_scp)
    }

#===============================================================================
#===============================================================================

#' Execute a single action based on tzar repetition values in project.yaml
#'
#' This is the main workhorse function for bdpg.  It's intended to generate
#' a single bdproblem of a chosen type (COR, WRAP, APP) or a single reserve
#' selector run based on inputs specified in the yaml file and its
#' repetitions section.  In general, tzar will spawn a process that calls this
#' function across a set of problems, e.g., generating 10 apparent problems
#' for each correct problem whose path is given in an input file.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing.
#' @export

#-------------------------------------------------------------------------------

act_on_loaded_existing_tzar_run <- function (tgt_filename_or_dirname_for_scp,
                                             parameters)
    {
    cat ("\nStarting act_on_loaded_existing_tzar_run().\n")
    cat ("\ntgt_filename_or_dirname_for_scp = '", tgt_filename_or_dirname_for_scp, "'\n")

        #------------------------------------------------------
        #  Make sure that exactly one action has been chosen.
        #  Quit if none or more than one chosen.
        #------------------------------------------------------

    run_rs_on_ALL_prob = value_or_FALSE_if_null (parameters$run_rs_on_ALL_prob)
    run_rs_on_COR_prob = value_or_FALSE_if_null (parameters$run_rs_on_COR_prob)
    run_rs_on_APP_prob = value_or_FALSE_if_null (parameters$run_rs_on_APP_prob)

    run_network_metrics_on_ALL_prob = value_or_FALSE_if_null (parameters$run_network_metrics_on_ALL_prob)
    run_network_metrics_on_COR_prob = value_or_FALSE_if_null (parameters$run_network_metrics_on_COR_prob)
    run_network_metrics_on_APP_prob = value_or_FALSE_if_null (parameters$run_network_metrics_on_APP_prob)

    #---------------------------------------------------------------------------

        #------------------------------------------------------------------
        #  Load COR and/or APP problem file lists according to the input
        #  options selected.
        #------------------------------------------------------------------

    if (run_rs_on_ALL_prob)
        {
        run_rs_on_COR_prob = TRUE
        run_rs_on_APP_prob = TRUE
        }

    if (run_network_metrics_on_ALL_prob)
        {
        run_network_metrics_on_COR_prob = TRUE
        run_network_metrics_on_APP_prob = TRUE
        }

    if (run_rs_on_COR_prob | run_network_metrics_on_COR_prob)
        {
        list_of_RSprob_COR_dirs =
            get_list_of_RSprob_COR_dirs (tgt_filename_or_dirname_for_scp)

        cat ("\n\nIn act_on_loaded_existing_tzar_run():\n",
             "list_of_RSprob_COR_dirs = \n")
        print (list_of_RSprob_COR_dirs)
        cat ("\n")
        }

    if (run_rs_on_APP_prob | run_network_metrics_on_APP_prob)
        {
        list_of_RSprob_APP_dirs =
            get_list_of_RSprob_APP_dirs (tgt_filename_or_dirname_for_scp)

        cat ("\n\nIn act_on_loaded_existing_tzar_run():\n",
             "list_of_RSprob_APP_dirs = \n")
        print (list_of_RSprob_APP_dirs)
        cat ("\n")
        }


    #---------------------------------------------------------------------------

        #------------------------------------------------------------------
        #  Run a reserve selector on CORRECT problems (base or wrapped),
        #  if requested.
        #------------------------------------------------------------------

    if (run_rs_on_COR_prob)
        {
        for (cur_prob_dir in list_of_RSprob_COR_dirs)
            {
            # cur_prob_dir = file.path (tgt_filename_or_dirname_for_scp, cur_prob_dir)
            #
            # rds_file_name = list.files (cur_prob_dir, pattern="saved.RSprob*")
            # rds_file_path = file.path (cur_prob_dir, rds_file_name [1])  #  There should only be one element in the rds_file_name vector.
            #
            # cor_bdprob = load_saved_obj_from_file (normalizePath (rds_file_path))

            cor_bdprob = get_loaded_bdprob (tgt_filename_or_dirname_for_scp,
                                            cur_prob_dir)



            do_rs_analysis_and_output (cor_bdprob,
                                       cor_bdprob,
                                       parameters,
                                       cur_prob_dir)
            }  #  end for - COR dirs
        }  #  end if - run rs on COR dirs

    #---------------------------------------------------------------------------

        #---------------------------------------------------------------
        #  Run a reserve selector on APPARENT problems if requested.
        #---------------------------------------------------------------

#  NOTE:
#      THIS REQUIRES THE COR PROBLEM FOR THE APP PROBLEM TO BE AVAILABLE
#      IN THE OLD TZAR RUN TOO.
#      I THINK THAT'S BECAUSE IT'S NEEDED IN COMPUTING THE COR SCORE FOR THE
#      APP RS RESULTS.

    if (run_rs_on_APP_prob)
        {
        for (cur_prob_dir in list_of_RSprob_APP_dirs)
            {
            # cur_prob_dir = file.path (tgt_filename_or_dirname_for_scp, cur_prob_dir)
            #
            # rds_file_name = list.files (cur_prob_dir, pattern="saved.RSprob*")
            # rds_file_path = file.path (cur_prob_dir, rds_file_name [1])  #  There should only be one element in the rds_file_name vector.
            #
            # app_bdprob = load_saved_obj_from_file (normalizePath (rds_file_path))

            app_bdprob = get_loaded_bdprob (tgt_filename_or_dirname_for_scp,
                                            cur_prob_dir)





            cor_bdprob =
                get_loaded_cor_bdprob_for_loaded_app_bdprob (
                                                tgt_filename_or_dirname_for_scp,
                                                app_bdprob,
                                                parameters)

            do_rs_analysis_and_output (app_bdprob,
                                       cor_bdprob,
                                       parameters,
                                       cur_prob_dir)
            }  #  end for - APP dirs
        }  #  end if - run rs on APP dirs

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
    #---------------------------------------------------------------------------

        #------------------------------------------------------------
        #  Run graph metrics on CORRECT problems (base or wrapped),
        #  if requested.
        #------------------------------------------------------------

    if (run_network_metrics_on_COR_prob)
        {
        for (cur_prob_dir in list_of_RSprob_COR_dirs)
            {
            run_graph_metrics_on_RSprob (tgt_filename_or_dirname_for_scp,
                                         cur_prob_dir,
                                         parameters)

            }  #  end for - COR dirs
        }  #  end if - run rs on COR dirs

    #---------------------------------------------------------------------------

        #------------------------------------------------------------
        #  Run graph metrics on APPARENT problems (base or wrapped),
        #  if requested.
        #------------------------------------------------------------

    if (run_network_metrics_on_APP_prob)
        {
        for (cur_prob_dir in list_of_RSprob_APP_dirs)
            {
            run_graph_metrics_on_RSprob (tgt_filename_or_dirname_for_scp,
                                         cur_prob_dir,
                                         parameters)

            }  #  end for - APP dirs
        }  #  end if - run rs on APP dirs

    #---------------------------------------------------------------------------

    return()
    }

#===============================================================================
#===============================================================================

    #  Test code for command line.
# if (FALSE)
#     {
#     getwd()
#     load_existing_tzar_run_from_glass (parameters,
#                                        file_of_runs_to_load,
#                                        prev_run_idx,
#                                        tgt_filename_or_dirname_for_scp)
#     getwd()
#     }

#===============================================================================

