#===============================================================================

                                #  create_dirs.R

#===============================================================================

create_one_res_sel_dir_structure <- function (res_sel_dir_name = "marxan",
                                              base_outdir = ".",
                                              create_dirs=TRUE)
    {
    res_sel_dir_names = vector ("list", 2)
    names (res_sel_dir_names) <- c("input_dir", "output_dir")

    res_sel_dir_names$input_dir = file.path (base_outdir, "input")
    res_sel_dir_names$output_dir = file.path (base_outdir, "output")

    if (create_dirs)
        {
            #  Create reserve selector INPUT directory.
        dir.create (res_sel_dir_names$input_dir,
                    showWarnings = TRUE, recursive = TRUE)

            #  Create reserve selector OUTPUT directory.
        dir.create (res_sel_dir_names$output_dir,
                    showWarnings = TRUE, recursive = TRUE)
        }

    return (res_sel_dir_names)
    }

#===============================================================================

create_new_replicate_dirs <- function (bdpg_dir_names,
                                        rep = 1,
                                        res_sel_name = "marxan",
                                        dir_names,
                                        base_outdir = ".",
                                        create_dirs = TRUE)
    {
    top_dir_name = stringr::str_c (res_sel_name, ".", rep)
    top_dir_base_outdir = file.path (normalizePath (base_outdir, mustWork=FALSE),
                                       res_sel_name,
                                       top_dir_name)

    res_sel_dir_names =
        create_one_res_sel_dir_structure (res_sel_dir_name = res_sel_name,
                                          base_outdir = top_dir_base_outdir,
                                          create_dirs = create_dirs
                                          )

    bdpg_dir_names$res_sel[[res_sel_name]][[top_dir_name]] = res_sel_dir_names

    dir_names = vector ("list", 4)    #  Create final structure to be returned.
    names (dir_names) <- c("IO_dir", "input_dir", "output_dir",
                           "bdpg_dir_names")

    dir_names$IO_dir         = top_dir_base_outdir
    dir_names$input_dir      = res_sel_dir_names$input_dir
    dir_names$output_dir     = res_sel_dir_names$output_dir
    dir_names$bdpg_dir_names = bdpg_dir_names

#    return (bdpg_dir_names)
    return (dir_names)
    }

#===============================================================================

#' Create a directory subtree to hold output from a run of a reserve selector
#'
#'  This function is meant to deal with the fact that for a given
#'  reserve selection problem (whether correct or apparent, wrapped
#'  or simple, etc.), we may want to run a given reserve selector
#'  multiple times, e.g., with a different set of parameters each time.
#'
#'  A good example of when to use this function would be running marxan on the
#'  same problem with different numbers of iterations allowed, to get some idea
#'  of run time or problem difficulty.  For example, you might want to run it
#'  the first time only allowing 1000 iterations to get an idea of how long that
#'  takes and how good is the solution. You might also want to run it allowing 1
#'  million iterations to get a better solution when you have more run time
#'  available. Both of these runs might be done inside the same tzar run or they
#'  might be done in separate tzar runs.
#'
#'  In the case of doing both marxan runs inside the same tzar run, you'd like
#'  to have the directory structure under res_sel/marxan allow for marxan.1 and
#'  marxan.2, etc.  One way to do this is to have the code check for the largest
#'  marxan directory number in the current tzar run whenever it calls the marxan
#'  code and then create a new subdirectory tree with a higher number for the
#'  new run.
#'
#'  (Note that you don't have to be running tzar here.  The phrase "tzar run" is
#'  just a convenient term for the whole big run as opposed to just one call to
#'  marxan inside the whole big run.)
#'
#'  Creates a dir tree for this reserve selector parameterization and run. For
#'  example, if the reserve selector is marxan and this is the first
#'  parameterization and run of marxan for this problem, create
#'  res_sel/marxan/marxan.1 and its 2 subdirs, input and output. If this is the
#'  second parameterization and run for marxan, then create the same structure
#'  using marxan.2 instead of marxan.1.  Similarly, if you're using a different
#'  reserve selector, e.g., simpleRichness instead of marxan, this routine will
#'  create an analogous subtree res_sel/simpleRichness/simpleRichness.1, etc.

#' @param bdpg_dir_names nested list of paths to directories of bdpg output
#' @param res_sel_name character string giving name of reserve selector to add a subtree for, e.g., "marxan"
#' @param base_outdir character string giving path to base directory of bdpg output tree for this run
#' @param create_dirs boolean flag indicating whether to create the directories or, e.g, for testing, just create the list structure but not the directories
#'
#' @return the input bdpg_dir_names list with the new reserve selection run subtree added

create_new_res_sel_replicate_subtree <- function (bdpg_dir_names,
                                                    res_sel_name = "marxan",
                                                    base_outdir = ".",
                                                    create_dirs = TRUE)
    {
    base_outdir = normalizePath (base_outdir, mustWork=FALSE)
    base_outdir = bdpg_dir_names$res_sel_dir

    existing_res_sel_list = bdpg_dir_names$res_sel[[res_sel_name]]
    if (is.null (existing_res_sel_list))
        {
            #  No reserve selector has been added yet.
            #  Do the whole startup bit for a new reserve selector.

        dir_names = create_new_replicate_dirs (bdpg_dir_names,
                                                1,
                                                res_sel_name,
                                                dir_names,
                                                base_outdir,
                                                create_dirs)

        } else  #  This reserve selector has already been added.
        {       #  Add new replicate dir structure, e.g., marxan.2.

        num_of_existing_replicates = length (existing_res_sel_list)

        dir_names = create_new_replicate_dirs (bdpg_dir_names,
                                                num_of_existing_replicates + 1,
                                                res_sel_name,
                                                dir_names,
                                                base_outdir,
                                                create_dirs)
        }

    return (dir_names)
    }

#===============================================================================

create_base_dir_structure <- function (base_outdir, create_dirs=TRUE)
    {
    base_outdir = normalizePath (base_outdir, mustWork=FALSE)

        #  Create list of directory names.
    derived_bdpg_dir_names = list()
    derived_bdpg_dir_names$plot_output_dir    = file.path (base_outdir, "plots")
    derived_bdpg_dir_names$network_output_dir = file.path (base_outdir, "networks")
    derived_bdpg_dir_names$res_sel_dir        = file.path (base_outdir, "res_sel")

        #  Create head of list to hold reserve selector directory subtree.
    derived_bdpg_dir_names$res_sel            = list()

    if (create_dirs)
        {
            #  Create PLOT OUTPUT directory.
        dir.create (derived_bdpg_dir_names$plot_output_dir,
                    showWarnings = TRUE, recursive = TRUE)

            #  Create NETWORK OUTPUT directory.
        dir.create (derived_bdpg_dir_names$network_output_dir,
                    showWarnings = TRUE, recursive = TRUE)

            #  Create RES_SEL directory.
        dir.create (derived_bdpg_dir_names$res_sel_dir,
                    showWarnings = TRUE, recursive = TRUE)
        }

    return (derived_bdpg_dir_names)
    }

#===============================================================================

#' Build a new numbered file name whose extension is 1 higher than the largest
#' extension already in the given directory.
#'
#' If there are no numbered directories (e.g., app.1, app.2, etc.) in the
#' directory to be searched, then this routine will return the stem with ".1"
#' appended, e.g., "marxan.1".
#'
#' This routine is a little bit loose in that it doesn't check for errors
#' such as whether there are any non integer file extensions.
#'
#' @param dir_to_search character string giving the path to the directory to
#'     search for numbered directory names
#' @param name_stem character string giving just the stem of the subdirectory
#'     names, e.g,. "app" or "marxan", etc.
#'
#' @return character string giving the name of the next highest number
#'     directory, e.g., if the dir_to_search was "tzarout/exp1/base_prob/app"
#'     and the name_stem was "app" and there were directories
#'     "tzarout/exp1/base_prob/app/app.1" and "tzarout/exp1/base_prob/app/app.2"
#'     then "app.3" would be returned.
#' @export

get_next_numbered_dir_name <- function (dir_to_search,  #  e.g., "tzaroutdir/base_prob"
                                        name_stem       #  e.g., "base_prob"
                                        )
    {
        #  Build a wildcard search in the dir_to_search, looking for
        #  the name stem followed by anything.
    base_name = file.path (dir_to_search, stringr::str_c(name_stem, ".*"))
    numbered_files_found = Sys.glob (file.path (base_name))

        #  If there were any files there, find the largest extension number
        #  and add 1 to it to get the new file name's extension.
        #  If there weren't any there, set the new extension to 1.
    biggest_ext = 0
    if (length (numbered_files_found) > 0)
        biggest_ext = max (as.integer (lapply (numbered_files_found,
                                               tools::file_ext)))
    new_ext = biggest_ext + 1

        #  Tack the new extension onto give the original stem.
    new_name = stringr::str_c (name_stem, ".", new_ext)

    return (new_name)
    }

#----------

    #  Imitate unix touch function to create an empty file.
    #  I couldn't find an existing function for this, but did find a
    #  stack overflow question that suggested using write.table() on an
    #  empty table, so that's what I've done.

touch <- function (file_path_to_touch)
    {
    file_path_to_touch = normalizePath (file_path_to_touch, mustWork=FALSE)
#    cat ("\nfile_path_to_touch = '", file_path_to_touch, "'\n")
    write.table (data.frame(),
                 file = file_path_to_touch,
                 col.names=FALSE)
    }

#----------

test_get_next_numbered_dir_name <- function ()
    {
    dir_to_search = "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1646_marxan_simulated_annealing.inprogress/wrap_prob.1/cor/res_sel/marxan"

    #  should return new_name == "marxan.3"
    name_stem = "marxan"
    new_name = get_next_numbered_dir_name (dir_to_search,  #  e.g., "tzaroutdir/base_prob"
                                            name_stem       #  e.g., "base_prob"
                                            )
    if (new_name == "marxan.3") cat (".") else cat ("NO_3 ")

    #  should return new_name == "app.1"
    name_stem = "app"
    new_name = get_next_numbered_dir_name (dir_to_search,  #  e.g., "tzaroutdir/base_prob"
                                            name_stem       #  e.g., "base_prob"
                                            )
    if (new_name == "app.1") cat (".") else cat ("NO_1 ")
    }

#===============================================================================

get_or_create_prob_topdir_path <- function (starting_dir, base_prob_name_stem,
                                            create_dirs = TRUE)
    {
        #------------------------------------------------------------
        #  If it doesn't exist yet, create top directory for
        #  current problems type, i.e., base or wrapped.
        #  Example:  tzar_outdir/base_prob or tzar_outdir/wrap_prob
        #------------------------------------------------------------

            #  Strip off trailing slash of starting_dir if there is one.
            #  This is necessary because file.path() only strips the trailing
            #  slash off its LAST argument.  If a previous argument has a
            #  trailing slash, it gets doubled up in the output.
    last_char = stringr::str_sub (starting_dir, nchar (starting_dir), nchar (starting_dir))
    if (last_char == .Platform$file.sep)
        starting_dir = stringr::str_sub (starting_dir,1,nchar(starting_dir)-1)
    base_probs_dir = normalizePath (file.path (starting_dir,
                                               base_prob_name_stem),
                                    mustWork=FALSE)
    cat ("\nget_or_create_prob_topdir_path:",
         "\n    base_probs_dir = '", base_probs_dir, "'", sep='')
    if (create_dirs & !dir.exists (base_probs_dir))
        dir.create (base_probs_dir, showWarnings = TRUE, recursive = TRUE)

        #-----------------------------------------------------
        #  Create top directory for the current new problem,
        #  e.g., tzar_outdir/base_prob/base_prob.1
        #-----------------------------------------------------

    prob_topdir_name =
        get_next_numbered_dir_name (base_probs_dir, base_prob_name_stem)

    prob_topdir = file.path (base_probs_dir, prob_topdir_name)
    cat ("\n    prob_topdir = '", prob_topdir, "'", sep='')
    if (create_dirs)
        dir.create (prob_topdir, showWarnings = TRUE, recursive = TRUE)

    return (prob_topdir)
    }

#===============================================================================

