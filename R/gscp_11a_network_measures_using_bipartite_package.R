#===============================================================================

            #  gscp_11a_network_measures_using_bipartite_package.R

#===============================================================================

                    #  Code for using bipartite package.

#===============================================================================

#'Compute bipartite network measures using bipartite package
#'
#'The bdpg package computes bipartite measures over the PU/spp network in
#'two ways, one using the igraph package and the other using the bipartite
#'package.  This function computes values using the bipartite package.
#'
#'@section Cautions about metric name confusion:
#'
#'The bipartite package looks at bipartite networks through the lens of
#'plant-pollinator or parasitoid-prey networks.  Specifically, in the
#'bipartite documentation, it breaks a web into 2 groups as follows:
#'
#'"Web is a matrix representing the interactions observed between higher trophic
#'level species (columns) and lower trophic level species (rows). Usually this
#'will be number of pollinators on each species of plants or number of
#'parasitoids on each species of prey."
#'
#'\itemize{
#'    \item{HL=PUs=columns}
#'    \item{LL=Spp=rows}
#'    \item{species=PUsAndSpp}
#'}
#'
#'This means that measures coming out of the bipartite package are named as if
#'the graph was a plant-pollinator network.  This, in turn, means that some
#'metric names need to be re-interpreted for the reserve selection problem.
#'
#'In bdpg, species are rows and planning units are columns, i.e., in bdpg,
#'planning units correspond to the higher trophic level species and reserve
#'selection "species" correspond to bipartite lower trophic level species. For
#'example, when bipartite says something like "number of species", that means
#'the total number of both plants and pollinators together (i.e., both
#'groups/levels in the network, since plants and pollinators are both species.
#'In the reserve selection problem, that means the total number of both planning
#'units and species together, rather than just the species involved in the
#'reserve selection problem.
#'
#'@section Choice of metrics vs. speed of computation:
#'
#'Information about which indices are slow is taken from the R documentation for
#'the networklevel function in the bipartite package in its section called
#'"Reducting computation time".  Here is that section to help understand
#'the choices made in the compute_network_measures_using_bipartite_package()
#'function.
#'
#'@section Reducing computation time:
#'
#'Some indices require rather long computation times on large webs. If you want
#'to increase the speed by omitting some indices, here a rough guide: Ask only
#'for the indices you are interested in! Otherwise, here is the sequence of most
#'time-consuming indices:
#'
#'\enumerate{
#'    \item{The slowest function is related to extinction slopes and robustness. Excluding
#'both makes the function faster.}
#'
#'    \item{weighted cluster coefficient is also very time consuming (an exhaustive search
#'for 4-loops in the one-mode projection of the network). Omitting it can
#'dramatically boost speed.}
#'
#'    \item{Degree distributions are somewhat time consuming.}
#'
#'    \item{Fisher's alpha is computed iteratively and hence time consuming.}
#'
#'    \item{Nestedness and weighted nestedness are not the fastest of routines.}
#'
#'    \item{Number (and diversity) of compartments calls a recursive and hence relatively
#'slow algorithm.}
#'
#'    \item{H2 and specialisation asymmetry require an iterative, heuristic search
#'algorithm. Finally, excluding discrepancy can also moderately decrease
#'computation time.}
#'}
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{all_except_slow_indices}{
#' \preformatted{
#' all_except_slow_indices :  chr [1:22] "number of species" "connectance" "web asymmetry" ...
#' }}
#' \subsection{bipartite_metrics_csv_file_name}{
#' \preformatted{
#' bipartite_metrics_csv_file_name :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{bipartite_metrics_from_bipartite_package}{
#' \preformatted{
#' bipartite_metrics_from_bipartite_package : 'data.frame':	1 obs. of  4 variables:
#'  $ prob_UUID    : Factor w/ 1 level "d0729e1c-eadc-4899-a382-8cb7ac2c08d7": 1
#'  $ connectance  : num 0.0164
#'  $ number.of.PUs: num 122
#'  $ number.of.Spp: num 814
#' }}
#' \subsection{bpm}{
#' \preformatted{
#' bpm :  num [1:814, 1:122] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{exp_root_dir}{
#' \preformatted{
#' exp_root_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{metrics_col_names}{
#' \preformatted{
#' metrics_col_names :  chr [1:3] "connectance" "number.of.species.HL" "number.of.species.LL"
#' }}
#' \subsection{metrics_to_use}{
#' \preformatted{
#' metrics_to_use :  chr [1:2] "number of species" "connectance"
#' }}
#' \subsection{new_metrics_col_names}{
#' \preformatted{
#' new_metrics_col_names :  chr [1:3] "connectance" "number.of.species.HL" "number.of.species.LL"
#' }}
#' \subsection{quick_test}{
#' \preformatted{
#' quick_test :  chr [1:2] "number of species" "connectance"
#' }}
#' \subsection{rsprob}{
#' \preformatted{
#' rsprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{uuid_col}{
#' \preformatted{
#' uuid_col : 'data.frame':	1 obs. of  1 variable:
#'  $ prob_UUID: Factor w/ 1 level "d0729e1c-eadc-4899-a382-8cb7ac2c08d7": 1
#' }}
#' \subsection{x}{
#' \preformatted{
#' x :  chr [1:3] "connectance" "number.of.PUs" "number.of.Spp"
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns bipartite_metrics_from_bipartite_package

compute_network_measures_using_bipartite_package = function (rsprob,
                                                             exp_root_dir)
    {
    cat ("\n\nAbout to create all_except_slow_indices.")

    bpm = rsprob@bpm

    quick_test <-
      c(
          "number of species",
          "connectance"
          )

    all_except_slow_indices <-
      c(
          "number of species",
          "connectance",
          "web asymmetry",
          "links per species",
    #           "number of compartments",           #  #6a slowest ("calls a recursive and hence relatively slow algorithm.")
    #           "compartment diversity",            #  #6b slowest ("calls a recursive and hence relatively slow algorithm.")
          "cluster coefficient",
    #            "degree distribution",              #  #3 slowest ("somewhat time consuming")
          "mean number of shared partners",
          "togetherness",
          "C score",
          "V ratio",
    #            "discrepancy",                      #  #7c slowest ("excluding discrepancy can also moderately decrease computation time")
    #            "nestedness",                       #  #5a slowest ("not the fastest of routines")
    #            "weighted nestedness",              #  #5b slowest ("not the fastest of routines")
          "ISA",
          "SA",
    #            "extinction slope",                 #  #1a slowest
    #            "robustness",                       #  #1b slowest
          "niche overlap",
    #            "weighted cluster coefficient",     #  #2 slowest
          "weighted NODF",
          "partner diversity",
          "generality",
          "vulnerability",
          "linkage density",
          "weighted connectance",
    #            "Fisher alpha",                     #  #4 slowest ("computed iteratively and hence time consuming")
          "interaction evenness",
          "Alatalo interaction evenness",
          "Shannon diversity",
          "functional complementarity"
    )
    #            , "H2                                 #  #7a slowest ("require an iterative, heuristic search algorithm", #7b - same for specialisation asymmetry but it's not in this list)
    #        )


        #-----------------------------------------------------------------------
        #  On a small network, using "ALLBUTDD" as the metric choice took a
        #  miniscule amount of extra time compared to all_except_slow_indices.
        #  However, on larger problems (e.g., wrapped problems), the bipartite
        #  package's metric computations are pretty slow (5 or 10 minutes at
        #  the moment) even for the all_except_slow_indices so it's better
        #  not to use "ALLBUTDD" there.
        #-----------------------------------------------------------------------

    cat ("\nIn compute_network_measures_using_bipartite_package():  metrics_to_use = '",
         parameters$bipartite_metrics_to_use, "'", sep='')

    if (parameters$bipartite_metrics_to_use == "ALLBUTDD")
        {
        metrics_to_use = "ALLBUTDD"

        } else if (parameters$bipartite_metrics_to_use == "quick_test")
        {
        metrics_to_use = quick_test

        } else if (parameters$bipartite_metrics_to_use == "all_except_slow_indices")
        {
        metrics_to_use = all_except_slow_indices

        } else
        {
        stop (paste0 ("\n\ncompute_network_measures_using_bipartite_package(): ",
                      "parameters$bipartite_metrics_to_use must be one of {",
                      "ALLBUTDD, quick_test, all_except_slow_indices}\n"))
        }

    bipartite_metrics_from_bipartite_package <-
                        bipartite::networklevel (bpm, index=metrics_to_use)

        #-----------------------------------------------------------------------
        #  The outputs will be used in a data frame, but they've been returned
        #  here as a vector, so they need to be transposed.
        #-----------------------------------------------------------------------

    bipartite_metrics_from_bipartite_package <-
        t (bipartite_metrics_from_bipartite_package)


        #-----------------------------------------------------------------------
        #  Clean up metric names...
        #
        #  The bipartite metrics output has spaces in some of the metric names.
        #  This can cause problems sometimes for downstream uses of the output,
        #  so we need to replace the spaces with something other than white
        #  space.
        #  Here, we'll replace them with underscores.  Some of the metrics
        #  already use periods instead of spaces, but using underscore helps
        #  identify which ones have been modified when trying to find them
        #  in the bipartite package documentation.
        #-----------------------------------------------------------------------

    metrics_col_names = colnames (bipartite_metrics_from_bipartite_package)
    cat ("\n\nmetrics_col_names = \n")
    print (metrics_col_names)

    new_metrics_col_names =
      stringr::str_replace_all (string=metrics_col_names,
                                pattern=" ",
                                replacement="_")
    cat ("\n\nnew_metrics_col_names = \n")
    print (new_metrics_col_names)
    cat ("\n\n")

    colnames (bipartite_metrics_from_bipartite_package) = new_metrics_col_names

    cat ("\n\nbipartite_metrics_from_bipartite_package with spaces removed from column names = \n")
    print (bipartite_metrics_from_bipartite_package)
    cat ("\n\n")

        #--------------------------------------------------------------------
        #  Translate all column names from bipartite's naming scheme to
        #  using PUs and Spp instead of HL and LL, etc.
        #  This could have been done just once and hard-coded in here, but
        #  the string substitution doesn't take long and I want to make it
        #  clear exactly what was changed and how, so the code for doing it
        #  is included here.
        #--------------------------------------------------------------------

    x = new_metrics_col_names
    x = gsub("HL",                "PUs",       x, fixed=TRUE)
    x = gsub("LL",                "Spp",       x, fixed=TRUE)
    x = gsub("number.of.species", "number.of", x, fixed=TRUE)
    x = gsub("species",           "PUsAndSpp", x, fixed=TRUE)

    cat ("\n\ntranslated_col_names = \n")
    print (x)
    cat ("\n\n")

    colnames (bipartite_metrics_from_bipartite_package) = x

    cat ("\n\nbipartite_metrics_from_bipartite_package with column names translated to use PUs & Spp, etc. instead of bipartite HL, LL, etc. = \n")
    print (bipartite_metrics_from_bipartite_package)
    cat ("\n\n")

        #-----------------------------------------------------------------
        #  Add UUID of the problem as the first column and then save the
        #  graph results data frame to disk.
        #-----------------------------------------------------------------

    uuid_col = data.frame (bip_rsp_UUID=rsprob@UUID)
    bipartite_metrics_from_bipartite_package =
        cbind (uuid_col, bipartite_metrics_from_bipartite_package)

    cat ("\n\nfinal bipartite_metrics_from_bipartite_package including prob_UUID column = \n")
    print (bipartite_metrics_from_bipartite_package)
    cat ("\n\n")

    bipartite_metrics_csv_file_name =
        file.path (get_RSprob_path_networks (rsprob, exp_root_dir),
                   paste0 (rsprob@bipartite_metrics_file_name_stem, ".csv"))
#                   "bipartite_metrics_from_bipartite_package.csv")

    write.csv (bipartite_metrics_from_bipartite_package,
               file = bipartite_metrics_csv_file_name,
    #            col.names=TRUE,
                row.names=FALSE
                )

# Error in (function (cl, name, valueClass)  :
#   assignment of an object of class “data.frame” is not valid for @‘bipartite_metrics_from_bipartite_package’ in an object of class “Xu_bd_problem”; is(value, "matrix") is not TRUE

#docaids::doc_vars_in_this_func_once ()
    return (bipartite_metrics_from_bipartite_package)
    }

#===============================================================================

