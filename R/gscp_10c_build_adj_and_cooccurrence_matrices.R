#===============================================================================

                #  gscp_10c_build_adj_and_cooccurrence_matrices.R

#===============================================================================

#  History:

#  2015 02 19 - BTL
#       Created by cutting creation of adj matrix out of
#       gscp_11a_network_measures_using_bipartite_package.R.

#===============================================================================
#       Verify that the generated optimal solution really is a solution.
#===============================================================================

#' Verify that solution does cover all species
#'
#'  Theoretically, this should not be necessary, but checking it here to
#'  make sure that the implementation is working correctly.
#'  Correct solutions will have every species attaining a representation
#'  fraction of their target of at least 1 (where 1 means exactly meeting
#'  their target).
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
#' \subsection{bpm}{
#' \preformatted{
#' bpm :  num [1:814, 1:122] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{dependent_node_IDs}{
#' \preformatted{
#' dependent_node_IDs :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{num_PUs}{
#' \preformatted{
#' num_PUs :  int 122
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 814
#' }}
#' \subsection{PU_costs}{
#' \preformatted{
#' PU_costs :  num [1:122] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{solution_cost}{
#' \preformatted{
#' solution_cost :  num 61
#' }}
#' \subsection{spp_rep_fracs}{
#' \preformatted{
#' spp_rep_fracs :  num [1:814] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{spp_rep_targets}{
#' \preformatted{
#' spp_rep_targets :  num [1:814] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{unmet_spp_rep_frac_indices}{
#' \preformatted{
#' unmet_spp_rep_frac_indices :  int(0)
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns TRUE if given solution does cover all species targets;
#'     quits otherwise

verify_that_generated_solution_really_is_a_solution =
        function (bpm,
                  dependent_node_IDs,
                  num_spp,
                  num_PUs,
            PU_costs,
                  bdpg_error_codes
                  )
    {
    spp_rep_targets = rep (1, num_spp)
    spp_rep_fracs = compute_rep_fraction (bpm,
                                            dependent_node_IDs,
                                            spp_rep_targets)

    unmet_spp_rep_frac_indices = which (spp_rep_fracs < 1)

    if (length (unmet_spp_rep_frac_indices) > 0)
        {
        cat ("\n\nSERIOUS ERROR: The generated optimal solution is not a solution.",
           "\n               Species at the following indices in spp_rep_fracs have representation < 1:\n",
           "\n               ")
        print (spp_rep_fracs [unmet_spp_rep_frac_indices])
        cat ("\n\nAll spp rep fracs = \n")
        print (spp_rep_fracs)
        cat ("\n\ndependent_node_IDs = \n")
        print (dependent_node_IDs)
        cat ("\n\nbpm = \n")
        print (bpm)

        if (getOption ("bdpg.emulating_tzar", default=FALSE))  browser ()
        quit (save="no", status=bdpg_error_codes$ERROR_STATUS_optimal_solution_is_not_optimal)
        }

    solution_cost = compute_solution_cost (dependent_node_IDs, PU_costs)    #rep (1, num_PUs))

        #  If an error message needs to be written too, then one of these
        #  might be a more appropriate call.
        #assertError(expr, verbose = FALSE)
        #assertWarning(expr, verbose = FALSE)
        #assertCondition(expr, ..., .exprString = , verbose = FALSE)
        #warning(..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
        #        domain = NULL)
        #stop(..., call. = TRUE, domain = NULL)

    stopifnot (all.equal (solution_cost, length (dependent_node_IDs)))

#docaids::doc_vars_in_this_func_once ()
    return (TRUE)
    }

#===============================================================================
#  Create a data frame using NAMES of the elements rather than their INDICES.
#===============================================================================

#' Create PU spp pair indices
#'
#' Create planning unit / species pair indices.
#'
#'  Though the two data frames carry identical information, it looks
#'  like both of them are necessary because different network packages
#'  expect different inputs.  The bipartite package creates an
#'  adjacency matrix based on the indices, but the igraph package
#'  creates a bipartite graph using either the indices or the vertex names.
#'
#'  However, if I later decide to use the vertex indices, I need to go
#'  back and renumber either the spp vertices or the PU vertices so
#'  that they don't overlap the other set.  That may end up being the
#'  better strategy when graphs get big, but at the moment, giving them
#'  names seems less likely to introduce some kind of indexing bug.
#'
#'  Will create names for PU vertices by prepending the vertex ID with a "p".
#'  Similarly, spp vertices will be named by prepending with an "s".
#'
#'  NOTE: We have to either uniquely name the vertices or we have to
#'  renumber either the spp or the PUs.  This is because the numbering of
#'  both sets of vertices starts at 1 and that means the vertex IDs are
#'  not unique when the two sets are combined.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{PU_col_name}{
#' \preformatted{
#' PU_col_name :  chr "PU_ID"
#' }}
#' \subsection{PU_spp_pair_indices}{
#' \preformatted{
#' PU_spp_pair_indices : 'data.frame':	1628 obs. of  2 variables:
#'  $ PU_ID : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ spp_ID: int  1 1 2 2 3 3 4 4 5 5 ...
#' }}
#' \subsection{PU_spp_pair_names}{
#' \preformatted{
#' PU_spp_pair_names : 'data.frame':	1628 obs. of  2 variables:
#'  $ PU_ID : chr  "p1" "p2" "p3" "p4" ...
#'  $ spp_ID: chr  "s1" "s1" "s2" "s2" ...
#' }}
#' \subsection{PU_spp_pair_names_triple}{
#' \preformatted{
#' PU_spp_pair_names_triple : List of 3
#'  $ PU_spp_pair_names:'data.frame':	1628 obs. of  2 variables:
#'  $ PU_vertex_names  : chr [1:122] "p1" "p2" "p3" "p4" ...
#'  $ spp_vertex_names : chr [1:814] "s1" "s2" "s3" "s4" ...
#' }}
#' \subsection{PU_vertex_indices}{
#' \preformatted{
#' PU_vertex_indices :  int [1:122] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{PU_vertex_names}{
#' \preformatted{
#' PU_vertex_names :  chr [1:122] "p1" "p2" "p3" "p4" "p5" "p6" "p7" "p8" "p9" "p10" ...
#' }}
#' \subsection{spp_col_name}{
#' \preformatted{
#' spp_col_name :  chr "spp_ID"
#' }}
#' \subsection{spp_vertex_indices}{
#' \preformatted{
#' spp_vertex_indices :  int [1:814] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{spp_vertex_names}{
#' \preformatted{
#' spp_vertex_names :  chr [1:814] "s1" "s2" "s3" "s4" "s5" "s6" "s7" "s8" "s9" "s10" ...
#' }}
#'
#' @param PU_spp_pair_indices data frame
#' @param PU_col_name character string
#' @param spp_col_name character string
#'
#' @return PU_spp_pair_names_triple plist

create_PU_spp_pair_names =
        function (
#                   cor_num_PUs,
#                   cor_num_spp,
                  PU_spp_pair_indices,
                  PU_col_name,
                  spp_col_name
                  )
    {
    cat ("\n\nAbout to create PU_spp_pair_names...")

        #  First, create vectors of just the PU names and spp names alone.
        #  These are used later to build tables.
        #  NOTE:  I think that both the num_PUs and num_spp need to be cor_
        #           in case there is some kind of shrinkage in the counts
        #           when error is added to create app_ values.
        #           (Could the app_ counts ever be bigger than the cor_counts?)

#    PU_vertex_indices = 1:cor_num_PUs    #  I think this has to always be cor_
    PU_vertex_indices = sort(unique(PU_spp_pair_indices[,PU_col_name]))
    PU_vertex_names = stringr::str_c ("p", PU_vertex_indices)

#    spp_vertex_indices = 1:cor_num_spp    #  I think this has to always be cor_
    spp_vertex_indices = sort(unique(PU_spp_pair_indices[,spp_col_name]))
    spp_vertex_names = stringr::str_c ("s", spp_vertex_indices)

        #  Now, create a near copy of the PU_spp_pair_indices table
        #  but using the names of the PUs and species instead of their
        #  indices.

    PU_spp_pair_names =
        data.frame (PU_ID = stringr::str_c ("p", PU_spp_pair_indices [,PU_col_name]),
                    spp_ID = stringr::str_c ("s", PU_spp_pair_indices [,spp_col_name]),
                    stringsAsFactors = FALSE)

    PU_spp_pair_names_triple <- list (PU_spp_pair_names=PU_spp_pair_names,
                  PU_vertex_names=PU_vertex_names,
                  spp_vertex_names=spp_vertex_names)

#docaids::doc_vars_in_this_func_once ()

    return (PU_spp_pair_names_triple)
    }

#===============================================================================
#               Build input matrix for bipartite package...
#===============================================================================

#' Create adjacency matrix with species rows vs planning unit columns
#'
#' Create numeric adjacency matrix with one row for each species and one
#' column for each planning unit and each matrix entry specifying whether
#' that species occupies that planning unit.  A 1 indicates the species
#' does occupy the planning unit and 0 indicates it does not.
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
#' \subsection{bpm}{
#' \preformatted{
#' bpm :  num [1:814, 1:122] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{correct_solution_vector_is_known}{
#' \preformatted{
#' correct_solution_vector_is_known :  logi TRUE
#' }}
#' \subsection{cur_col}{
#' \preformatted{
#' cur_col :  int 112
#' }}
#' \subsection{cur_row}{
#' \preformatted{
#' cur_row :  int 814
#' }}
#' \subsection{dependent_node_IDs}{
#' \preformatted{
#' dependent_node_IDs :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{edge_idx}{
#' \preformatted{
#' edge_idx :  int 1628
#' }}
#' \subsection{num_PU_cols}{
#' \preformatted{
#' num_PU_cols :  int 122
#' }}
#' \subsection{num_PU_spp_pairs}{
#' \preformatted{
#' num_PU_spp_pairs :  int 1628
#' }}
#' \subsection{num_PUs}{
#' \preformatted{
#' num_PUs :  int 122
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 814
#' }}
#' \subsection{num_spp_rows}{
#' \preformatted{
#' num_spp_rows :  int 814
#' }}
#' \subsection{PU_col_name}{
#' \preformatted{
#' PU_col_name :  chr "PU_ID"
#' }}
#' \subsection{PU_costs}{
#' \preformatted{
#' PU_costs :  num [1:122] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{PU_spp_pair_indices}{
#' \preformatted{
#' PU_spp_pair_indices : 'data.frame':	1628 obs. of  2 variables:
#'  $ PU_ID : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ spp_ID: int  1 1 2 2 3 3 4 4 5 5 ...
#' }}
#' \subsection{spp_col_name}{
#' \preformatted{
#' spp_col_name :  chr "spp_ID"
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns bpm; integer matrix with one row for each species and one
#'     column for each planning unit.  Each matrix entry specifies whether
#'     that species occupies that planning unit; 1 indicates the species
#'     does occupy the planning unit and 0 indicates it does not.

create_adj_matrix_with_spp_rows_vs_PU_cols =
    function (num_spp,
              num_PUs,
## 2015 05 01 ##               spp_vertex_names,
## 2015 05 01 ##               PU_vertex_names,
              PU_spp_pair_indices,
        PU_costs,
              spp_col_name,
              PU_col_name,
              dependent_node_IDs,
              correct_solution_vector_is_known,
              bdpg_error_codes
              )

    {
    cat ("\n\nAbout to create bpm matrix.")

    num_spp_rows = num_spp    #max (PU_spp_pair_indices [ , spp_col_name])
    num_PU_cols  = num_PUs    #max (PU_spp_pair_indices [ , PU_col_name])

        #  Create the adjacency matrix that will be viewed as a
        #  bipartite matrix (bpm) by the bipartite network routines
        #  with species as rows and planning units as columns.
    bpm = matrix (0,
                  nrow=num_spp_rows,    #  num_spp,
                  ncol=num_PU_cols,    #  num_PUs,
                  byrow=TRUE

                      #  Not sure whether to have dimnames or not.
                      #  Doesn't seem to hurt anything at the moment...
## 2015 05 01 ##                   ,
## 2015 05 01 ##                   dimnames=list (spp_vertex_names,
## 2015 05 01 ##                                  PU_vertex_names)
                  )

    cat ("\n\nAbout to fill in bpm matrix.")
#browser()
    num_PU_spp_pairs = dim (PU_spp_pair_indices)[1]
    for (edge_idx in 1:num_PU_spp_pairs)
        {
        cur_row = PU_spp_pair_indices [edge_idx, spp_col_name]
        cur_col = PU_spp_pair_indices [edge_idx, PU_col_name]

            #  I'm making this be "1 + ..." instead of just "1",
            #  in case at some point, I need to keep counts of
            #  duplicates instead of just presence/absence.
            #  With unique values, you get the same matrix
            #  either way.
        bpm [cur_row, cur_col] = 1 + bpm [cur_row, cur_col]
        }

        #  If you've generated the problem, then you know the correct
        #  solution vector and can verify that it does get the representation
        #  that is required.
        #  However, if you've just read the Xu problem in from a file,
        #  you probably don't know what is the correct solution, so
        #  you can't verify it.

    if (correct_solution_vector_is_known)
        {
        verify_that_generated_solution_really_is_a_solution (bpm,
                                                        dependent_node_IDs,
                                                        num_spp,  #  num_spp_rows?
                                                        num_PUs,  #  num_PU_cols?
                                            PU_costs,
                                                        bdpg_error_codes)
        }

#docaids::doc_vars_in_this_func_once ()
    return (bpm)
    }

#===============================================================================

