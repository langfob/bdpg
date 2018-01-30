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
#-------------------------------------------------------------------------------

#' @param rsprob An RSprob reserve selection problem object to run graph metrics
#'     over if desired
#' @param top_dir character string
#' @param compute_network_metrics boolean flag indicating whether any graph
#'     metrics should be computed for this problem; TRUE implies metrics
#'     should be computed, FALSE implies not
#' @param compute_network_metrics_COR_APP_WRAP boolean flag indicating whether
#'     this type of problem (base_cor, base_app, wrapped_cor, or wrapped_app)
#'     should have metrics computed for it; TRUE implies metrics should be
#'     computed, FALSE implies not
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
#' @param write_to_disk boolean flag indicating whether metrics should be
#'     written to disk before being returned; TRUE implies write to disk,
#'     FALSE implies not
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

#-------------------------------------------------------------------------------

init_object_graph_data <- function (rsprob,
                                    top_dir,
                                    compute_network_metrics,
                                    compute_network_metrics_COR_APP_WRAP,
                                    use_igraph_metrics,
                                    use_bipartite_metrics,
                                    bipartite_metrics_to_use = "ALLBUTDD",
                                    write_to_disk = TRUE)
    {
        #-----------------------------------------------------------------------
        #  Many of the input values are taken directly from the parameters list
        #  and may have NULL values, so check for that and clean them up if
        #  necessary to avoid throwing errors later.
        #-----------------------------------------------------------------------

    compute_network_metrics              = value_or_FALSE_if_null (compute_network_metrics)

    compute_network_metrics_COR_APP_WRAP =
        vb (compute_network_metrics_COR_APP_WRAP, def_on_empty = TRUE, def = TRUE)

    use_igraph_metrics                   = value_or_FALSE_if_null (use_igraph_metrics)
    use_bipartite_metrics                = value_or_FALSE_if_null (use_bipartite_metrics)

        #-----------------------------------------------------------------------
        #  This flag is used to allow easily toggling off all network metrics
        #  during testing without having to reset each of the flags
        #  individually.
        #  If it is set to FALSE, all other network flags are ignored.
        #-----------------------------------------------------------------------

    rsprob@compute_network_metrics              = compute_network_metrics

        #-----------------------------------------------------------------------
        #  Flags for individual network metrics and graph packages that
        #  are evaluated only if the top level flag is turned on.
        #-----------------------------------------------------------------------

    rsprob@compute_network_metrics_COR_APP_WRAP = compute_network_metrics_COR_APP_WRAP
    rsprob@use_igraph_metrics                   = use_igraph_metrics
    rsprob@use_bipartite_metrics                = use_bipartite_metrics

    if (is.null (bipartite_metrics_to_use)) bipartite_metrics_to_use = "ALLBUTDD"
    rsprob@bipartite_metrics_to_use             = bipartite_metrics_to_use

    if (compute_network_metrics & compute_network_metrics_COR_APP_WRAP)
        {
        if (use_bipartite_metrics)
            rsprob@bipartite_metrics_from_bipartite_package =
                compute_network_measures_using_bipartite_package (rsprob,
                                                                  top_dir,
                                                                  bipartite_metrics_to_use,
                                                                  write_to_disk)

        if (use_igraph_metrics)
            rsprob@bipartite_metrics_from_igraph_package_df =
                compute_igraph_related_network_measures (rsprob,
                                                         top_dir,
                                                         write_to_disk)
        }

    return (rsprob)
    }

#===============================================================================




