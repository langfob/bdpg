#===============================================================================

                        #  gscp_9a_create_Xu_graph.R

#===============================================================================

assert_edge_list_does_not_violate_assumptions =
    function (edge_list, first_row_of_intergroup_links, nodes)
    {
          #--------------------------------------------------------------------
          #  Set a flag to indicate whether any error at all has occurred in
          #  this routine.  Check it at the end of the function.
          #  Any error that is caught in here will set this flag to TRUE but
          #  not cause the routine to fail until it has looked at all edges.
          #  Doing it this way because I want to catch as many errors as
          #  possible in one pass, so that you don't have to keep rerunning
          #  the code to get all the errors if there are lots of them.
          #--------------------------------------------------------------------

      edge_list_error = FALSE

          #---------------------------------------------------------------
          #  First make sure that you don't have a completely degenerate
          #  edge_list, i.e., one where there are no within group links.
          #  Since all within group links are expected to precede all
          #  between group links, the first row of intergroup links must
          #  be at least 2 if there are some within group links.
          #---------------------------------------------------------------

      if (first_row_of_intergroup_links < 2)
          {
          edge_list_error = TRUE
          cat ("\nERROR:  Degenerate edge_list.  ",
               "first_row_of_intergroup_links = ",
               first_row_of_intergroup_links,
               ", i.e., < 2.")

          } else  #  Not a degenerate edge_list.
          {
              #-------------------------------------------------------------
              #  Verify that WITHIN group links don't violate assumptions.
              #-------------------------------------------------------------

          for (cur_row in seq (1, (first_row_of_intergroup_links - 1)))
              {
              from_node = edge_list [cur_row, 1]
              to_node   = edge_list [cur_row, 2]

                  #-----------------------------------------------
                  #  Nodes in an edge must be in the same group.
                  #-----------------------------------------------

              if (! (nodes$group_ID [[from_node]] == nodes$group_ID [[to_node]]))
                  {
                  edge_list_error = TRUE
                  cat ("\nERROR:  At edge_list row ", cur_row,
                       ", within group edge endpoints [",
                       from_node, ", ", to_node,
                       "] are in different groups [",
                       nodes$group_ID [[from_node]], ", ",
                       nodes$group_ID [[to_node]],
                       "].")
                  }

                  #---------------------------------------------------------------
                  #  Independent set nodes really are independent, i.e.,
                  #  no edge connected to an independent set node can connect to
                  #  another independent set node.
                  #---------------------------------------------------------------

              if ((! nodes$dependent_set_member [[from_node]]) &
                  (! nodes$dependent_set_member [[to_node]]))
                  {
                  edge_list_error = TRUE
                  cat ("\nERROR:  At edge_list row ", cur_row,
                       ", within group edge endpoints [",
                       from_node, ", ", to_node,
                       "] are both in the independent set.")
                  }
              }  #  end for - WITHIN group links

          #-----------------------------------------------------------------------

              #------------------------------------------------------------------
              #  Verify that BETWEEN group links don't violate assumptions.
              #
              #  It's unusual but legally possible for no intergroup links to
              #  be generated, so only try to verify correctness of intergroup
              #  links if there are some.
              #------------------------------------------------------------------

          num_rows_in_edge_list = dim (edge_list) [1]
          if (first_row_of_intergroup_links <= num_rows_in_edge_list)
              {
              for (cur_row in seq (first_row_of_intergroup_links, num_rows_in_edge_list))
                  {
                  from_node = edge_list [cur_row, 1]
                  to_node   = edge_list [cur_row, 2]

                      #----------------------------------------------------------
                      #  Nodes in intergroup edges are not allowed to be in the
                      #  independent set.
                      #----------------------------------------------------------

                  if (! nodes$dependent_set_member [[from_node]])
                      {
                      edge_list_error = TRUE
                      cat ("\nERROR:  At edge_list row ", cur_row,
                           ", between group edge's FROM node [",
                           from_node,
                           "] is in the independent set.")
                      }

                  if (! nodes$dependent_set_member [[to_node]])
                      {
                      edge_list_error = TRUE
                      cat ("\nERROR:  At edge_list row ", cur_row,
                           ", between group edge's TO node [",
                           to_node,
                           "] is in the independent set.")
                      }

                  }  #  end for - BETWEEN group links
              }  #  end if - (first_row_of_intergroup_links <= num_rows_in_edge_list)
          }  #  end else - (first_row_of_intergroup_links >= 2)

doc_vars_in_this_func ()
      if (edge_list_error)
          stop ("\n\nOne or more fatal errors in building edge_list.\n\n")

    return (TRUE)
    }

#===============================================================================

sort_within_rows = function (a_2_col_matrix, decreasing=FALSE)
    {
    for (row in 1:dim(a_2_col_matrix)[1])
        {
        a_2_col_matrix [row,] = sort (a_2_col_matrix [row,], decreasing)
        }

doc_vars_in_this_func ()
    return (a_2_col_matrix)
    }

#===============================================================================

create_Xu_graph = function (num_nodes_per_group,
                            n__num_groups,
                            nodes,
                            max_possible_tot_num_links,
                            target_num_links_between_2_groups_per_round,
                            num_rounds_of_linking_between_groups,
                            duplicate_links_allowed=FALSE,
                            bdpg_error_codes
                            )
    {
    DEBUG_LEVEL = getOption ("bdpg.DEBUG_LEVEL", default=0)

    edge_list = matrix (NA,
                        nrow = max_possible_tot_num_links,
                        ncol = 2,
                        byrow = TRUE)

    colnames (edge_list) = c("from_node", "to_node")

    #---------------------------------------------------------------------------

        #-----------------------------
        #  Link nodes WITHIN groups.
        #-----------------------------

    edge_list_and_cur_row =
        link_nodes_within_groups (num_nodes_per_group,
                                  n__num_groups,
                                  nodes,
                                  edge_list,
                                  bdpg_error_codes)

    #---------------------------------------------------------------------------

        #------------------------------
        #  Link nodes BETWEEN groups.
        #------------------------------

    first_row_of_intergroup_links = edge_list_and_cur_row$cur_row

    edge_list =
        link_nodes_between_groups (target_num_links_between_2_groups_per_round,
                                   num_rounds_of_linking_between_groups,
                                   n__num_groups,
                                   nodes,
                                   edge_list_and_cur_row$edge_list,
                                   edge_list_and_cur_row$cur_row)

    #---------------------------------------------------------------------------

        #---------------------------------------------
        #  Remove duplicate links, if there are any.
        #---------------------------------------------

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
            #           nodes have a "from" value less than or equal to the "to"
            #           value.
            #           That wouldn't be necessary if these were directed links,
            #           but undirected, you couldn't recognize duplicates if
            #           the order was allowed to occur both ways, i.e., (3,5) and
            #           (5,3) would not be flagged as being duplicates.

    if (! duplicate_links_allowed)
        {
            #  Sort each of the rows to be sure that from-to pairs are in
            #  sorted order.  They're probably already in sorted order,
            #  but this is safer (e.g., in case the earlier code changes
            #  or I am wrong in my assumption about them already being sorted).

        edge_list = sort_within_rows (edge_list)  #  be sure pairs are sorted
        num_non_unique_edge_list = dim (edge_list)[1]

        edge_list = unique (edge_list)
        num_unique_edge_list = dim (edge_list)[1]

        if (DEBUG_LEVEL > 0)
            {
            cat ("\n\nnum_non_unique_edge_list =", num_non_unique_edge_list)
            cat ("\nnum_unique_edge_list =", num_unique_edge_list)
            cat ("\n")
            }
        }

    if (DEBUG_LEVEL > 0)
        {
        cat ("\n\nedge_list (at end of gscp_9a):\n\n")
        print (edge_list)
        cat ("\n\n")
        }

    #---------------------------------------------------------------------------

        #------------------------------------
        #  Verify that all edges are legal.
        #------------------------------------

        #  All edges added to the edge_list SHOULD be legal at this point
        #  if the code is working correctly.  Make sure that this is true
        #  and fail if it's not.

    assert_edge_list_does_not_violate_assumptions (edge_list,
                                                   first_row_of_intergroup_links,
                                                   nodes)

    #---------------------------------------------------------------------------

doc_vars_in_this_func ()

    return (edge_list)
    }

#===============================================================================

get_num_edge_list = function (edge_list)
    {
    return (dim (edge_list)[1])
    }

#===============================================================================

