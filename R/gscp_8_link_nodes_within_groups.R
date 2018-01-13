#===============================================================================

                        #  gscp_8_link_nodes_within_groups.R

#===============================================================================

#' Link nodes within groups
#'
#' Create species in a Xu problem by creating links between nodes in different
#' groups.  Each link will represent a species that appears on the planning
#' units at each end of the link and those planning units are in separate
#' groups.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns list containing edge_list and cur_row

#-------------------------------------------------------------------------------

link_nodes_within_groups =
    function (num_nodes_per_group,
              n__num_groups,
              nodes,
              edge_list)
    {
    cat ("\n\n--------------------  Linking nodes WITHIN each group.\n")

    if (num_nodes_per_group < 2)
        {
        # cat ("\n\n***  num_nodes_per_group (", num_nodes_per_group,
        #      ") must be at least 2.\n\n")

        if (getOption ("bdpg.emulating_tzar", default=FALSE))  browser ()

        stop_bdpg (paste0 ("\n\n***  num_nodes_per_group (", num_nodes_per_group,
             ") must be at least 2."))
        }

    num_nodes_per_group_minus_1 = num_nodes_per_group - 1
    cur_row = 1

    for (cur_group_ID in 1:n__num_groups)
        {
            #  NOTE:  I think that the ordering of the node IDs within
            #           each pair is important later on.  That is, when
            #           trying to identify duplicated links, the unique()
            #           call that is used will only work if the pairs are
            #           ordered within pair, i.e., if all "from" nodes
            #           have a value less than or equal to the "to" value.
            #           That wouldn't be necessary if these were directed links,
            #           but undirected, you couldn't recognize duplicates if
            #           the order was allowed to occur both ways, i.e., (3,5) and
            #           (5,3) would not be flagged as being duplicates.


            #  NOTE:  The code in this loop assumes the group nodes are sorted.
            #         These group nodes are probably already sorted,
            #         but this just makes sure, as a safeguard against
            #         some future change.
        cur_group_nodes_sorted =
            sort (nodes [nodes$group_ID == cur_group_ID, "node_ID"])
#        cat ("\n\ncur_group_nodes_sorted for group ", cur_group_ID, " = ")
#        print (cur_group_nodes_sorted)

            #  Link each node in the group to all nodes with a higher node ID in
            #  the same group.
            #  Doing it this way insures that all nodes in the group are linked to
            #  all other nodes in the group but that the linking action is only done
            #  once for each pair.

        for (cur_idx in 1:num_nodes_per_group_minus_1)
            {
            for (other_node_idx in (cur_idx+1):num_nodes_per_group)
                {
                edge_list [cur_row, 1] = cur_group_nodes_sorted [cur_idx]
                edge_list [cur_row, 2] = cur_group_nodes_sorted [other_node_idx]
                cur_row = cur_row + 1
                }
            }
        }

    if (getOption ("bdpg.DEBUG_LEVEL", default=0) > 0)
        {
        cat ("\n\nedge_list (with last lines NA to hold intergroup links to be loaded in next step):\n\n")
        print (edge_list)
        cat ("\n\n")
        }

    return (list (edge_list=edge_list, cur_row=cur_row))
    }

#===============================================================================

    #  edge_list gives the edge list.
        #  However, can't use it until it's completely finished, i.e.,
        #  close to when it's handed to Marxan.
    #  igraph needs an edge list.
    #  The row number is also the edge/link ID.
    #  The two columns are the nodes that are connected.
    #  This is the edge list the igraph needs, I think.
    #  However, I need more than one graph.

#===============================================================================

