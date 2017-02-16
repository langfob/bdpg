#===============================================================================

                    #  gscp_11c_init_object_graph_data.R

#===============================================================================

#' Compute graph metrics for an RSprob if requested and set the corresponding
#' flags in the RSprob.
#'
#' This function looks at various flags that have generally been set in the
#' parameters object built from project.yaml and determines which, if any,
#' graph metrics should be computed for the problem.  It also sets the
#' corresponding slot's value for each of the flags in the RSprob object.
#'
#' @param rsprob An RSprob reserve selection problem object to run graph metrics
#'     over if desired
#' @param compute_network_metrics boolean flag indicating whether any graph
#'     metrics should be computed for this problem; TRUE implies metrics
#'     should be computed, FALSE implies not
#' @param use_igraph_metrics boolean flag indicating whether metrics based
#'     on the igraph package should be computed for this problem; TRUE implies
#'     they should be computed, FALSE implies not
#' @param use_bipartite_metrics boolean flag indicating whether metrics based
#'     on the bipartite package should be computed for this problem; TRUE
#'     implies they should be computed, FALSE implies not
#' @param bipartite_metrics_to_use character string indication which set of
#'     bipartite package metrics should be computed if any from that package
#'     are to be computed; current choices are "ALLBUTDD" or
#'     "all_except_slow_indices"
#'
#' The bipartite package has a range of measures that can be computed and some
#' of them are quite slow, so the argument called "bipartite_metrics_to_use" is
#' provided for this function to allow some control over the amount of time
#' spent in the bipartite package computations.
#'
#' "ALLBUTDD" runs all of the
#' metrics (including the slow ones) except for fitting parameters of the
#' degree distribution, which is even slower than all the rest.
#'
#' "all_except_slow_indices" runs all metrics other than the slow ones, where
#' "slow" is defined to be the degree distribution fitting and all of the
#' metrics that are listed in the bipartite::networklevel() function's
#' help page in its section called "Reducing computation time".  Currently,
#' the metrics identified in that routine as being somewhat slow are:
#'
#'           "extinction slope",                 #  #1a slowest
#'           "robustness",                       #  #1b slowest
#'           "weighted cluster coefficient",     #  #2 slowest
#'           "degree distribution",              #  #3 slowest ("somewhat time consuming")
#'           "Fisher alpha",                     #  #4 slowest ("computed iteratively and hence time consuming")
#'           "nestedness",                       #  #5a slowest ("not the fastest of routines")
#'           "weighted nestedness",              #  #5b slowest ("not the fastest of routines")
#'           "number of compartments",           #  #6a slowest ("calls a recursive and hence relatively slow algorithm.")
#'           "compartment diversity",            #  #6b slowest ("calls a recursive and hence relatively slow algorithm.")
#'           "H2                                 #  #7a slowest ("require an iterative, heuristic search algorithm", #7b - same for specialisation asymmetry but it's not in this list)
#'           "discrepancy",                      #  #7c slowest ("excluding discrepancy can also moderately decrease computation time")
#'
#' It's easy to specify which metrics to include and exclude when calling
#' bipartite::networklevel, so it would be easy to add other options of metrics
#' to compute by making small modifcations to the bdpg function
#' compute_network_measures_using_bipartite_package().
#'
#' @return modified version of the rsrun input variable, i.e., with graph-related
#'     flags set
#'
#' @examples \dontrun{
#'     Xu_bdprob_cor <- init_object_graph_data (Xu_bdprob_cor,
#'                                              parameters$compute_network_metrics_COR,
#'                                              parameters$use_igraph_metrics,
#'                                              parameters$use_bipartite_metrics,
#'                                              parameters$bipartite_metrics_to_use)
#'
#' }

init_object_graph_data <- function (rsprob,
                                    compute_network_metrics,
                                    use_igraph_metrics,
                                    use_bipartite_metrics,
                                    bipartite_metrics_to_use = "ALLBUTDD")
    {
    rsprob@compute_network_metrics  = compute_network_metrics
    rsprob@use_igraph_metrics       = use_igraph_metrics
    rsprob@use_bipartite_metrics    = use_bipartite_metrics
    rsprob@bipartite_metrics_to_use = bipartite_metrics_to_use

    if (compute_network_metrics)
        {
        if (use_bipartite_metrics)
            rsprob@bipartite_metrics_from_bipartite_package =
              compute_network_measures_using_bipartite_package (rsprob@bpm)

        if (use_igraph_metrics)
            rsprob@bipartite_metrics_from_igraph_package_df =
                compute_igraph_related_network_measures (rsprob@PU_spp_pair_indices,
                                                         get_RSprob_path_networks (rsprob, starting_dir),
                                                         rsprob@PU_col_name,
                                                         rsprob@spp_col_name)
        }

    return (rsprob)
    }

#===============================================================================



