#===============================================================================

                        #  gscp_8_link_nodes_within_groups.R

#===============================================================================

#' Link nodes within groups
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{bdpg_error_codes}{
#' \preformatted{
#' bdpg_error_codes : List of 6
#'  $ ERROR_STATUS_num_inside_or_within_group_links_less_than_one: num 1001
#'  $ ERROR_STATUS_optimal_solution_is_not_optimal               : num 1002
#'  $ ERROR_STATUS_num_nodes_per_group_must_be_at_least_2        : num 1003
#'  $ ERROR_STATUS_duplicate_spp_in_Xu_input_file                : num 1004
#'  $ ERROR_STATUS_unknown_spp_occ_FP_error_type                 : num 1005
#'  $ ERROR_STATUS_unknown_spp_occ_FN_error_type                 : num 1006
#' }}
#' \subsection{cur_group_ID}{
#' \preformatted{
#' cur_group_ID :  int 61
#' }}
#' \subsection{cur_group_nodes_sorted}{
#' \preformatted{
#' cur_group_nodes_sorted :  int [1:2] 121 122
#' }}
#' \subsection{cur_idx}{
#' \preformatted{
#' cur_idx :  int 1
#' }}
#' \subsection{cur_row}{
#' \preformatted{
#' cur_row :  num 62
#' }}
#' \subsection{edge_list}{
#' \preformatted{
#' edge_list :  int [1:1003, 1:2] 1 3 5 7 9 11 13 15 17 19 ...
#' }}
#' \subsection{n__num_groups}{
#' \preformatted{
#' n__num_groups :  num 61
#' }}
#' \subsection{nodes}{
#' \preformatted{
#' nodes : 'data.frame':	122 obs. of  3 variables:
#'  $ node_ID             : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ group_ID            : num  1 1 2 2 3 3 4 4 5 5 ...
#'  $ dependent_set_member: logi  FALSE TRUE FALSE TRUE FALSE TRUE ...
#' }}
#' \subsection{num_nodes_per_group}{
#' \preformatted{
#' num_nodes_per_group :  num 2
#' }}
#' \subsection{num_nodes_per_group_minus_1}{
#' \preformatted{
#' num_nodes_per_group_minus_1 :  num 1
#' }}
#' \subsection{other_node_idx}{
#' \preformatted{
#' other_node_idx :  int 2
#' }}
#'
#' @param num_nodes_per_group
#' @param n__num_groups
#' @param nodes
#' @param edge_list
#' @inheritParams std_param_defns
#'
#' @return Returns list containing edge_list and cur_row

link_nodes_within_groups =
    function (num_nodes_per_group,
              n__num_groups,
              nodes,
              edge_list,
              bdpg_error_codes
              )
    {
    cat ("\n\n--------------------  Linking nodes WITHIN each group.\n")

    if (num_nodes_per_group < 2)
        {
        cat ("\n\n***  num_nodes_per_group (", num_nodes_per_group,
             ") must be at least 2.\n\n")

        if (getOption ("bdpg.emulatingTzar", default=FALSE))  browser ()

        quit (save="no", status=bdpg_error_codes$ERROR_STATUS_num_nodes_per_group_must_be_at_least_2)
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

#docaids::doc_vars_in_this_func_once ()
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

