#===============================================================================

                #  gscp_10a_clean_up_completed_graph_structures.R

#===============================================================================

    #  All node pairs should be loaded into the edge_list table now
    #  and there should be no NA lines left in the table.

    #  However no duplicate links are allowed, so need to go through all
    #  node pairs and remove non-unique ones.
                #  BTL - 2015 01 03 - Is this "no duplicates allowed" taken
                #                       from the original algorithm?
                #                       Need to be sure about that since
                #                       it affects things downstream.

        #  NOTE:  I think that this unique() call only works if the
        #           pairs are ordered within pair, i.e., if all from
        #           nodes have a value less than or equal to the to value.
        #           That wouldn't be necessary if these were directed links,
        #           but undirected, you couldn't recognize duplicates if
        #           the order was allowed to occur both ways, i.e., (3,5) and
        #           (5,3) would not be flagged as being duplicates.

#===============================================================================

#' Convert edge list to PU/spp table
#'
#'  Converts an edge list to a PU/spp table to give to Marxan and to network
#'  functions for bipartite networks:
#'
#'  Now that we have the edge list, we need to go through and
#'  create a table where every link attached to a node appears on a
#'  separate line of the table and is labelled with the node ID.
#'  So, that means that we have to go through the whole edge list and
#'  create 2 new table entries for each link in the edge list.
#'  Each of those entries gives the ID of one of the end nodes of the
#'  link plus the link's ID.
#'
#'  This table needs to be built because it's the form that Marxan expects
#'  the spp and PU data to be in, i.e., node=PU and link=spp and every
#'  line in the table is specifying a PU and one of the species in it.
#'  If we weren't feeding Marxan, there would be no need for this kind
#'  of a table since we already have an edge list.
#'
#'  However, there is one other useful byproduct of building this table.
#'  It makes it easy to compute rank-abundance information and information
#'  about the distribution of species across patches.
#'
#'  I just realized that this structure is also something like the
#'  description of a bipartite network, so I may need to modify or use
#'  it in doing the bipartite network analyses too.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns a PU_spp_pair_info_class object

#-------------------------------------------------------------------------------

create_PU_spp_pair_indices = function (edge_list,
                                        nodes,
                                        dependent_node_IDs,
                                        PU_costs,
                                       num_PUs)
    {
    num_edge_list = get_num_edge_list (edge_list)

    num_PU_spp_pairs = 2 * num_edge_list
    PU_spp_pair_indices =
        data.frame (PU_ID = rep (NA, num_PU_spp_pairs),
                    spp_ID = rep (NA, num_PU_spp_pairs))

    PU_col_name = names (PU_spp_pair_indices)[1]
    spp_col_name = names (PU_spp_pair_indices)[2]

    next_PU_spp_pair_row = 1

    for (cur_spp_ID in 1:num_edge_list)
        {
        PU_spp_pair_indices [next_PU_spp_pair_row, PU_col_name] = edge_list [cur_spp_ID, 1]  #  smaller_PU_ID
        PU_spp_pair_indices [next_PU_spp_pair_row, spp_col_name] = cur_spp_ID  #  next_spp_ID
        next_PU_spp_pair_row = next_PU_spp_pair_row + 1

        PU_spp_pair_indices [next_PU_spp_pair_row, PU_col_name] = edge_list [cur_spp_ID, 2]  #  larger_PU_ID
        PU_spp_pair_indices [next_PU_spp_pair_row, spp_col_name] = cur_spp_ID  #  next_spp_ID
        next_PU_spp_pair_row = next_PU_spp_pair_row + 1

        }  #  end for - cur_spp_ID

    correct_solution_cost = sum (PU_costs [dependent_node_IDs])

    #-----------------------------------------------------------------

    PU_spp_pair_info <- new ("PU_spp_pair_info_class")

    PU_spp_pair_info@PU_spp_pair_indices   <- PU_spp_pair_indices
    PU_spp_pair_info@PU_col_name           <- PU_col_name
    PU_spp_pair_info@spp_col_name          <- spp_col_name
    PU_spp_pair_info@num_PUs               <- num_PUs
    PU_spp_pair_info@num_spp               <- get_num_edge_list (edge_list)
    PU_spp_pair_info@correct_solution_cost <- correct_solution_cost

    return (PU_spp_pair_info)

    # return (list (PU_spp_pair_indices = PU_spp_pair_indices,
    #               PU_col_name = PU_col_name,
    #               spp_col_name = spp_col_name,
    #               num_PUs = num_PUs,
    #               num_spp = get_num_edge_list (edge_list),
    #               correct_solution_cost = correct_solution_cost
    #               ))
    }

#===============================================================================

