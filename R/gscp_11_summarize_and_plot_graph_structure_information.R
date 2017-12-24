#===============================================================================

            #  gscp_11_summarize_and_plot_graph_structure_information.R

#===============================================================================

#  Hack to quiet CHECK command for data frame column names that CHECK flags
#  as "no visible binding for global variable".
#  This is the officially sanctioned hack for doing this, e.g., see
#  https://github.com/STAT545-UBC/Discussion/issues/451
#  https://github.com/tidyverse/magrittr/issues/29
#  http://r.789695.n4.nabble.com/globalVariables-td4593980.html

if(getRversion() >= "2.15.1")  utils::globalVariables(c("freq"))

#-------------------------------------------------------------------------------

#  Compute and plot the degree distribution of the node graph.
#  It may be that we can use metrics over this graph as features of the
#  problem that give information about its difficulty.
#  For example, people are always going off about power laws in the
#  degree distribution.  Would something like that explain anything?

#  What about other measures like various forms of centrality?

#  Also, need to plot the graph using eric's software (or something similar)
#  to see if the visual layout gives any information.

#  After plotting, I realized that this degree distribution has the same
#  shape as a rank abundance curve, however, it's the opposite of a rank
#  abundance curve in that it's saying how many spp per patch, not how
#  many patches per spp.  I need to plot that now to see how it compares
#  to a typical rank abundance curve.  However, because of the way that
#  this problem generator currently works, the distribution will be
#  perfectly flat, i.e. 2 patches for every species, one for each end of
#  the link.  I need to start adding copies of the link IDs to other
#  patches after this generator finishes and see if you're still able to
#  have a deceptive problem even without the flat rank abundance
#  distribution?  In fact, can you add link IDs/species to non-independent
#  patches that still preserves the correct solution but purposely drives
#  the optimizer toward a wrong solution by knowing what kinds of things
#  it values?  Could you use annealing (or even Marxan itself) to search
#  for better "plate stackings" that lie over the hidden solution and
#  the optimizer finds some feature that lets it drive toward finding
#  difficult problems?  (Need good change operators too though.  However,
#  Marxan's own change operator is mindlessly simple and might work for
#  this as well if you do enough iterations the way Marxan does.)

#  What about cooccurrence matrices for the species?  Is there any measure
#  or visual display over those (e.g., something about their degree
#  distribution) that can be used as a predictive feature?

#-------------------------------------------------------------------------------

#' Plot final degree distribution for node graph
#'
#' Plot the final degree distribution for the node graph, which is essentially
#' the distribution of number of species per planning unit.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{cor_or_app_label}{
#' \preformatted{
#' cor_or_app_label :  chr "COR"
#' }}
#' \subsection{final_degree_dist}{
#' \preformatted{
#' final_degree_dist : 'data.frame':	122 obs. of  2 variables:
#'  $ PU_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq : int  33 33 33 31 31 31 30 29 29 29 ...
#' }}
#' \subsection{final_link_counts_for_each_node}{
#' \preformatted{
#' final_link_counts_for_each_node : 'data.frame':	122 obs. of  2 variables:
#'  $ PU_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq : int  1 25 1 27 1 29 1 28 1 24 ...
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{PU_col_name}{
#' \preformatted{
#' PU_col_name :  chr "PU_ID"
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Doesn't return anything

plot_final_degree_dist_for_node_graph =
        function (final_link_counts_for_each_node,
                  PU_col_name,
                  plot_output_dir,
                  cor_or_app_label
                  )
    {
    cat ("\n\n--------------------  Computing and plotting degree distribution of node graph.\n")

    final_degree_dist = plyr::arrange (final_link_counts_for_each_node, -freq)
    final_degree_dist[,PU_col_name] = 1:dim(final_degree_dist)[1]

    pdf (file.path (plot_output_dir,
                    paste0 (cor_or_app_label, "_", "final_degree_dist.pdf")),
         width=6, height=6)
    plot (final_degree_dist,
            main=paste0 ("Richness (", cor_or_app_label, ")"),
            #sub="subtitle",
            xlab="PU ID",
            ylab="num spp on PU")
    dev.off()

#docaids::doc_vars_in_this_func_once ()
    }

#-------------------------------------------------------------------------------

#' Plot rank abundance distribution for node graph
#'
#' Plot number of planning units for each species, sorted in decreasing order
#' of number of planning units.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{cor_or_app_label}{
#' \preformatted{
#' cor_or_app_label :  chr "COR"
#' }}
#' \subsection{final_node_counts_for_each_link}{
#' \preformatted{
#' final_node_counts_for_each_link : 'data.frame':	814 obs. of  2 variables:
#'  $ spp_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq  : int  2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{final_rank_abundance_dist}{
#' \preformatted{
#' final_rank_abundance_dist : 'data.frame':	814 obs. of  2 variables:
#'  $ spp_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq  : int  2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{spp_col_name}{
#' \preformatted{
#' spp_col_name :  chr "spp_ID"
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Doesn't return anything

plot_rank_abundance_dist_for_node_graph =
        function (final_node_counts_for_each_link,
                  spp_col_name,
                  plot_output_dir,
                  cor_or_app_label
                  )
    {
    final_rank_abundance_dist = plyr::arrange (final_node_counts_for_each_link, -freq)
    final_rank_abundance_dist[,spp_col_name] = 1:dim(final_rank_abundance_dist)[1]

    pdf (file.path (plot_output_dir,
                    paste0 (cor_or_app_label, "_",
                            "final_rank_abundance_dist.pdf")),
         width=6, height=6)
    plot (final_rank_abundance_dist,
            main=paste0 ("Rank abundance curve (", cor_or_app_label, ")"),
            # sub="subtitle",
            xlab="spp ID",
            ylab="abundance:  num PUs occupied by spp")
    dev.off()

#docaids::doc_vars_in_this_func_once ()
    }

#===============================================================================

#' Plot degree and abundance distributions for node graph
#'
#' Plot the final degree distribution for the node graph, which is essentially
#' the distribution of number of species per planning unit.  Similarly,
#' plot number of planning units for each species, sorted in decreasing order
#' of number of planning units.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{cor_or_app_label}{
#' \preformatted{
#' cor_or_app_label :  chr "COR"
#' }}
#' \subsection{final_link_counts_for_each_node}{
#' \preformatted{
#' final_link_counts_for_each_node : 'data.frame':	122 obs. of  2 variables:
#'  $ PU_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq : int  1 25 1 27 1 29 1 28 1 24 ...
#' }}
#' \subsection{final_rank_abundance_dist}{
#' \preformatted{
#' final_rank_abundance_dist : 'data.frame':	814 obs. of  2 variables:
#'  $ spp_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq  : int  2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{PU_col_name}{
#' \preformatted{
#' PU_col_name :  chr "PU_ID"
#' }}
#' \subsection{spp_col_name}{
#' \preformatted{
#' spp_col_name :  chr "spp_ID"
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Doesn't return anything

plot_degree_and_abundance_dists_for_node_graph =
        function (final_link_counts_for_each_node,
                  final_rank_abundance_dist,
                  PU_col_name,
                  plot_output_dir,
                  cor_or_app_label,
                  spp_col_name
                  )
    {
    plot_final_degree_dist_for_node_graph (final_link_counts_for_each_node,
                                            PU_col_name,
                                            plot_output_dir,
                                            cor_or_app_label
                                            )

    plot_rank_abundance_dist_for_node_graph (final_rank_abundance_dist,
                                            spp_col_name,
                                            plot_output_dir,
                                            cor_or_app_label
                                            )

#docaids::doc_vars_in_this_func_once ()
    }

#===============================================================================

