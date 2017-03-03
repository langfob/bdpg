#===============================================================================

                            #  create_dirs.R

#===============================================================================

#' Build top directory name
#'
#' Builds a directory name for the highest level directory for a bdpg-related
#' object such as a reserve selection problem.
#'
#'#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{obj}{
#' \preformatted{
#' obj : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{topdir_name}{
#' \preformatted{
#' topdir_name :  chr "RSprob-COR-Base.d0729e1c-eadc-4899-a382-8cb7ac2c08d7"
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns a directory name (not a path)

build_topdir_name <- function (obj)
    {
    topdir_name <- paste0 (obj@file_name_prefix, ".", obj@UUID)

#docaids::doc_vars_in_this_func_once ()
    return (topdir_name)
    }

#===============================================================================
    #  Shortcut functions to always build paths to RSprob dirs reliably.
#===============================================================================

#' Build top directory path for RS problem
#'
#' Build a full path to the highest level directory for a reserve selection
#' problem.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{exp_root_dir}{
#' \preformatted{
#' exp_root_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{rsprob}{
#' \preformatted{
#' rsprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{topdir}{
#' \preformatted{
#' topdir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns character string path to highest level directory for a
#'     reserve selection problem

get_RSprob_path_topdir <- function (rsprob, exp_root_dir)
    {
    topdir <- file.path (exp_root_dir, build_topdir_name (rsprob))

#docaids::doc_vars_in_this_func_once ()
    return (topdir)
}

#-------------------------------------------------------------------------------

#' Get path to plots directory of RS problem
#'
#' Build a full path to the directory where plots are written for a reserve
#' selection problem.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{exp_root_dir}{
#' \preformatted{
#' exp_root_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{plotsdir}{
#' \preformatted{
#' plotsdir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{rsprob}{
#' \preformatted{
#' rsprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns character string path to directory where plots are written
#'     for a reserve selection problem

get_RSprob_path_plots <- function (rsprob, exp_root_dir)
    {
    plotsdir <- file.path (exp_root_dir, build_topdir_name (rsprob), rsprob@plot_output_dir)

#docaids::doc_vars_in_this_func_once ()
    return (plotsdir)
    }

#-------------------------------------------------------------------------------

#' Get path to networks directory of RS problem
#'
#' Build a full path to the directory where networks are written for a reserve
#' selection problem.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{exp_root_dir}{
#' \preformatted{
#' exp_root_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{networksdir}{
#' \preformatted{
#' networksdir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{rsprob}{
#' \preformatted{
#' rsprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns character string path to directory where networks are written
#'     for a reserve selection problem

get_RSprob_path_networks <- function (rsprob, exp_root_dir)
    {
    networksdir <- file.path (exp_root_dir, build_topdir_name (rsprob), rsprob@network_output_dir)

#docaids::doc_vars_in_this_func_once ()
    return (networksdir)
    }

#===============================================================================

#' Create directories for an RS problem
#'
#' Create directory and subdirectory structure for an RS problem inside an
#' experiment.  Creates the highest level directory for the problem and the
#' plots and networks directories under that directory.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{create_dirs}{
#' \preformatted{
#' create_dirs :  logi TRUE
#' }}
#' \subsection{network_dir_path}{
#' \preformatted{
#' network_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{plot_dir_path}{
#' \preformatted{
#' plot_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{rsprob}{
#' \preformatted{
#' rsprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{top_dir}{
#' \preformatted{
#' top_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{top_dir_path}{
#' \preformatted{
#' top_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Doesn't return anything.

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

#docaids::doc_vars_in_this_func_once ()
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

#' Create directories for an RS run
#'
#' Create directory and subdirectory structure for an RS run inside an
#' experiment.  Creates the highest level directory for the run and the
#' input, output, and plots directories under that directory.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{create_dirs}{
#' \preformatted{
#' create_dirs :  logi TRUE
#' }}
#' \subsection{input_dir_path}{
#' \preformatted{
#' input_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{output_dir_path}{
#' \preformatted{
#' output_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{plot_dir_path}{
#' \preformatted{
#' plot_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{rsrun}{
#' \preformatted{
#' rsrun : Formal class 'RSrun' [package "bdpg"] with 12 slots
#' }}
#' \subsection{top_dir}{
#' \preformatted{
#' top_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{top_dir_path}{
#' \preformatted{
#' top_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Doesn't return anything.

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

#docaids::doc_vars_in_this_func_once ()
    }

#===============================================================================

