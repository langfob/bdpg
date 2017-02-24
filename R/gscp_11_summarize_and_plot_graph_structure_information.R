#===============================================================================

            #  gscp_11_summarize_and_plot_graph_structure_information.R

#===============================================================================

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

doc_vars_in_this_func ()
    }

#-------------------------------------------------------------------------------

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

doc_vars_in_this_func ()
    }

#===============================================================================

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

doc_vars_in_this_func ()
    }

#===============================================================================

