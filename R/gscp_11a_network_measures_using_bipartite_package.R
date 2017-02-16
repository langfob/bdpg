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
#'Cautions about metric name confusion
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
#'Choice of metrics vs. speed of computation
#'
#'Information about which indices are slow is taken from the R documentation for
#'the networklevel function in the bipartite package in its section called
#'"Reducting computation time".  Here is that section to help understand
#'the choices made in the compute_network_measures_using_bipartite_package()
#'function.
#'
#'Reducing computation time:
#'
#'Some indices require rather long computation times on large webs. If you want
#'to increase the speed by omitting some indices, here a rough guide: Ask only
#'for the indices you are interested in! Otherwise, here is the sequence of most
#'time-consuming indices:
#'
#'1.  The slowest function is related to extinction slopes and robustness. Excluding
#'both makes the function faster.
#'
#'2.  weighted cluster coefficient is also very time consuming (an exhaustive search
#'for 4-loops in the one-mode projection of the network). Omitting it can
#'dramatically boost speed.
#'
#'3.  Degree distributions are somewhat time consuming.
#'
#'4.  Fisher's alpha is computed iteratively and hence time consuming.
#'
#'5.  Nestedness and weighted nestedness are not the fastest of routines.
#'
#'6.  Number (and diversity) of compartments calls a recursive and hence relatively
#'slow algorithm.
#'
#'7.  H2 and specialisation asymmetry require an iterative, heuristic search
#'algorithm. Finally, excluding discrepancy can also moderately decrease
#'computation time.
#'
#' @param bpm
#'
#' @return
#' @export

compute_network_measures_using_bipartite_package = function (bpm)
    {
    cat ("\n\nAbout to create all_except_slow_indices.")

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


      bipartite_metrics_from_bipartite_package <-
    # #         t (networklevel (bpm, index=c ("links per species",
    # #                                      "H2",
    # #                                      "interaction evenness")))

                #  BTL - 2015 01 07
                #  On a small network, ALLBUTDD took a miniscule amount
                #  of extra time compared to all_except_slow_indices,
                #  so I'm going to use ALLBUTDD for now.  If bigger
                #  networks show this is too slow, then can revert to
                #  using all_except_slow_indices.

                #  BTL - 2017 02 16
                #  I've been using larger, wrapped problems lately, so the
                #  graph computations are going pretty slowly, so I'm going
                #  to go back to using the faster metrics.
                #  However, I think that most of the speed problems are coming
                #  from igraph, not bipartite.  Need to check that out.
                #  In any case, the choice of metrics here should be an
                #  option in the yaml file rather than hard-coded here.
                #  I'll get to that later.  For now, while I'm testing,
                #  I'll just use the faster ones.

        t (networklevel (bpm, index=all_except_slow_indices))
#        t (bipartite::networklevel (bpm, index="ALLBUTDD"))

    cat ("\n\nbipartite_metrics_from_bipartite_package = \n")
    print (bipartite_metrics_from_bipartite_package)

#  BTL - 2017 02 06 - POSSIBLE BUG
#  Documentation for networklevel() says that it returns:
#      "Depending on the selected indices, some or all of the
#       below (returned as vector if “degree distribution” was
#       not requested, otherwise as list):"
#  So, when I have index="ALLBUTDD", shouldn't it be returning a vector and
#  all of this stuff about column names should be failing?

cat ("\n>>>>>  class of return from bipartite::networklevel = ")
print (class (bipartite_metrics_from_bipartite_package))
cat ("\n>>>>>  dim (bipartite_metrics_from_bipartite_package) = ")
print (bipartite_metrics_from_bipartite_package)
cat ("\n\n")
#browser()

      #  Clean up metric names...
      #
      #  The bipartite metrics output has spaces in some of the metric names.
      #  This can cause problems sometimes for downstream uses of the output, so
      #  you need to replace the spaces with something other than white space.
      #  Here, I'll replace them with underscores.  Some of the metrics already
      #  use periods instead of spaces, but I won't mess with those for now in
      #  case it helps match them up with some other expectation from a paper or
      #  something.

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

    return (bipartite_metrics_from_bipartite_package)
    }

#===============================================================================

