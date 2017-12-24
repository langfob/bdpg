#===============================================================================

                        #  gscp_6_create_data_structures.R

#  The "nodes" data structure is the main store for data about each node in
#  the problem (where nodes correspond to "planning units" in the reserve
#  selection phrasing of the problem and "sets" in the minimum set cover
#  phrasing of the problem).

#  For each node, the structure contains its:
#       - node_ID
#           - an integer
#       - group_ID
#           - integer identifying what clique it belongs to
#       - dependent_set_member
#           - boolean flag indicating whether it's in the independent set or not
#           - FALSE means it IS in the independent set
#           - TRUE means it is NOT in the independent set and therefore,
#             it IS in the optimal solution

#===============================================================================

#' Create nodes data structure
#'
#-------------------------------------------------------------------------------

#' @param tot_num_nodes integer
#' @param num_nodes_per_group integer
#' @param n__num_groups integer
#' @param num_independent_nodes_per_group integer
#'
#' @return data frame containing node_ID, group_ID, dependent_set_member

#-------------------------------------------------------------------------------

create_nodes_data_structure =
        function (tot_num_nodes,
                  num_nodes_per_group,
                  n__num_groups,
                  num_independent_nodes_per_group)
    {
    cat ("\n\n--------------------  Creating and populating nodes structure.\n")

        #--------------------------------------------------------------
        #  For each group ID, assign a consecutive set of node IDs.
        #  For example, if there are 8 nodes total and 2 node groups,
        #  assign nodes 1:4 to group 1 and nodes 5:8 to group 2.
        #--------------------------------------------------------------

    node_IDs = 1:tot_num_nodes
    group_IDs = 1 + (0:(tot_num_nodes - 1) %/% num_nodes_per_group)

        #--------------------------------------------------------------------
        #  Assign lowest node IDs in each group to be the independent nodes
        #  in that group.
        #--------------------------------------------------------------------

    independent_node_ID_starts = seq (from=1,
                                        by=num_nodes_per_group,
                                        length.out=n__num_groups)
    independent_node_IDs = c()
    for (idx in 0:(num_independent_nodes_per_group-1))
        {
        independent_node_IDs = c(independent_node_IDs,
                                 (idx + independent_node_ID_starts))
        }
    independent_node_IDs = sort (independent_node_IDs)

    #--------------------

        #  For each node ID, flag whether it is in the dependent set or not.

    dependent_set_members = rep (TRUE, tot_num_nodes)
    dependent_set_members [independent_node_IDs] = FALSE

        #  Collect the IDs of just the dependent nodes.

    dependent_node_IDs = node_IDs [-independent_node_IDs]


        #------------------------------------------------------------
        #  Build an overall data frame that shows for each node,
        #  its node ID and group ID, plus a flag indicating whether
        #  it's in the dependent set or not.  For example, if there
        #  are 3 nodes per group:
        #
        #       node_ID     group_ID       dependent_set_member
        #         1            1                 TRUE
        #         2            1                 TRUE
        #         3            1                 FALSE
        #         4            2                 TRUE
        #         5            2                 TRUE
        #         6            2                 FALSE
        #        ...          ...                 ...
        #------------------------------------------------------------

    nodes = data.frame (node_ID = node_IDs,
                        group_ID = group_IDs,
                        dependent_set_member = dependent_set_members)

    #-------------------------------------------------------------------------------

    return (nodes)
    }

#===============================================================================

get_num_nodes = function (nodes) { dim (nodes)[1] }

#===============================================================================

get_independent_node_IDs = function (nodes)
    {
    return (which (! nodes [,"dependent_set_member"]))
    }

#===============================================================================

get_dependent_node_IDs = function (nodes)
    {
    return (which (nodes [,"dependent_set_member"]))
    }

#===============================================================================

