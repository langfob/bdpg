#===============================================================================

                        #  gscp_9_link_nodes_between_groups.R

#===============================================================================

    #  Now all groups and their within group links have been built.  
    #  Ready to start doing rounds of intergroup linking.

link_nodes_between_groups = 
    function (target_num_links_between_2_groups_per_round, 
              num_rounds_of_linking_between_groups, 
              n__num_groups, 
              nodes, 
              edge_list, 
              cur_row, 
              DEBUG_LEVEL
              ) 
    {
    cat ("\n\n--------------------  Doing rounds of intergroup linking.\n")
    
        #  The loop below crashes if the target number of links between groups 
        #  in each round has not been given a positive value, so skip the loop 
        #  if the target is not at least 1.
    if (target_num_links_between_2_groups_per_round >= 1)
        {
        for (cur_round in 1:num_rounds_of_linking_between_groups)
            {
            if (DEBUG_LEVEL > 0)
                cat ("\nRound", cur_round)
            
                #  Draw a random pair of groups to link in this round.
            cur_group_pair = safe_sample (1:n__num_groups, 2, replace=FALSE)
            
                #  Find all dependent set nodes in each group of the current 
                #  group pair.  
                #  Note that ONLY dependent set nodes are allowed.  Otherwise, 
                #  you might get links between members of the independent set. 
                #  Also, if you allowed a link from the independent set to a 
                #  node outside its group, you could violate the constraint 
                #  that insures that every node in the dependent set is 
                #  necessary in the solution.
            
                #  I'm using min and max here because smaller group IDs were 
                #  filled with smaller node IDs, so every node ID in the 
                #  smaller group ID should be the smaller node ID of any pairing 
                #  of nodes between the groups and the linking routine 
                #  expcts the smaller node ID to come before the larger one 
                #  in the linking argument list.  This may be a vestigial thing 
                #  from earlier schemes that doesn't matter any more, but 
                #  it's easy to maintain here for the moment, just in case it 
                #  does still matter in some way.  In any case, it doesn't 
                #  hurt anything to do this now other than the little bit of 
                #  extra execution time to compute the min and max.
            
            group_1 = min (cur_group_pair)
            group_1_nodes = nodes [(nodes$group_ID == group_1) & (nodes$dependent_set_member), 
                              "node_ID"]
            
            group_2 = max (cur_group_pair)
            group_2_nodes = nodes [(nodes$group_ID == group_2) & (nodes$dependent_set_member), 
                              "node_ID"]

            if (DEBUG_LEVEL > 0)
                {
                cat ("\n\n-----\ngroup_1_nodes = : ")
                print (group_1_nodes)
                cat ("\ngroup_2_nodes = : ")
                print (group_2_nodes)
                }

            #***----------------------------------------------------------------------------
            
#             group_1_sampled_nodes = 
#                 safe_sample (group_1_nodes, target_num_links_between_2_groups_per_round, 
#                       replace=TRUE)
#             group_2_sampled_nodes = 
#                 safe_sample (group_2_nodes, target_num_links_between_2_groups_per_round, 
#                       replace=TRUE)
# 
#             if (DEBUG_LEVEL > 0)
#                 {
#                 cat ("\n\n-----\ngroup_1_sampled_nodes = : ")
#                 print (group_1_sampled_nodes)
#                 cat ("\ngroup_2_sampled_nodes = : ")
#                 print (group_2_sampled_nodes)
#                 cat ("\ntarget_num_links_between_2_groups_per_round = : ")
#                 print (target_num_links_between_2_groups_per_round)
#                 }

            for (cur_node_pair_idx in 1:target_num_links_between_2_groups_per_round)
                {                
#                edge_list [cur_row, 1] = group_1_sampled_nodes [cur_node_pair_idx]
                edge_list [cur_row, 1] = safe_sample (group_1_nodes, 1)
                
#                edge_list [cur_row, 2] = group_2_sampled_nodes [cur_node_pair_idx]
                edge_list [cur_row, 2] = safe_sample (group_2_nodes, 1)
                
                if (DEBUG_LEVEL > 0)
                    {
                    cat ("\n\n-----\ncur_node_pair_idx = : ")
                    print (cur_node_pair_idx)
                    cat ("\ncur_row = : ")
                    print (cur_row)
                    cat ("\nedge_list [cur_row, 1] = : ")
                    print (edge_list [cur_row, 1])
                    cat ("\nedge_list [cur_row, 2] = : ")
                    print (edge_list [cur_row, 2])
                    }
                cur_row = cur_row + 1
                }  #  end for - cur_node_pair_idx
            }  #  end for - cur_round      
        }  #  end if - (target_num_links_between_2_groups_per_round >= 1)
    
    if (DEBUG_LEVEL > 0)
        {
        cat ("\n\nnodes (in gscp_9...):\n\n")
        print (nodes)
        cat ("\n\nedge_list (fully loaded at end of gscp_9...):\n\n")
        print (edge_list)
        cat ("\n\n")
        }
    
    
#     return (list (edge_list=edge_list, 
#                   cur_row=cur_row))
    return (edge_list)
    }

#===============================================================================

