#===============================================================================

                        #  generateSetCoverProblem.R

#===============================================================================

#  History:

#  2017 01 22 - BTL
#       Removed from old biodivprobgen project as part of splitting that
#       project into separate packages for the code specific to papers and
#       the general library of problem generator code.

#===============================================================================

#####source (paste0 (sourceCodeLocationWithSlash, "external_startup_code.R"))

#===============================================================================

#' Base function for generating set cover problems
#'
#' The main function that is to be called when generating a biodiversity
#' problem generator.
#'
#' @param parameters List of parameters controlling the current run (usually
#'   decoded from project.yaml by tzar)
#'
#' @return returns nothing
#'
#' @examples \dontrun{
#' old_gen_set_cover_problem (parameters)
#'}

old_gen_set_cover_problem = function (parameters)
{
#===============================================================================
#       Initialize.
#===============================================================================

    #  Initialize variables that have to be derived from values
    #  set in the parameters structure (which is generally built
    #  from project.yaml).

derived_bdpg_parameters = initialize_and_derive_parameters (parameters)   #  BUG?  UNKNOWN FOR XU FROM FILE?
bdpg_error_codes = derived_bdpg_parameters$bdpg_error_codes

#===============================================================================
#       Generate a problem, i.e, create the Xu graph nodes and edge_list.
#===============================================================================

EF_num <<- 0    #  2016 06 12 - BTL - Only used for debugging in searching for a lognormal...  Can remove if that search gets axed.

bdprob = gen_bdprob (parameters, bdpg_error_codes,
                     derived_bdpg_parameters$integerize)

if (!bdprob@prob_is_ok)
    {
    clean_up (timepoints_df, cur_timepoint_num, parameters, emulatingTzar,
              "\n\n>>>>>  gen_bdprob() failed.  <<<<<\n\n")
    stop ()
    }

#===============================================================================
#                   Save the values for the "correct" problem.
#===============================================================================

    #  The problem structures built so far represent the correct values.
    #  Adding error to the problem structure will create an apparent
    #  problem structure that is probably different from the correct
    #  structure.
    #  When we compute scores at the end of all this, we need to compute
    #  them with respect to the correct problem rather than the apparent.
    #  So, before we add error, we need to save the values defining the
    #  correct structure.

cor_PU_spp_pair_indices = bdprob@PU_spp_pair_indices
cor_bpm                 = bdprob@bpm
cor_num_PUs             = bdprob@num_PUs
cor_num_spp             = bdprob@num_spp
cor_nodes               = bdprob@nodes
cor_optimum_cost        = bdprob@cor_optimum_cost  #  BUG?  HOW IS THIS LOADED FOR XU FROM FILE?
cor_PU_costs            = bdprob@PU_costs
PU_col_name             = bdprob@PU_col_name
spp_col_name            = bdprob@spp_col_name

cor_PU_IDs              = bdprob@all_PU_IDs
cor_spp_IDs             = bdprob@all_spp_IDs

cat ("\n\nAbout to set all_correct_node_IDs.\n")
#browser()
    #  Some nodes may be unusued in cor_nodes, particularly if building a
    #  compound problem, e.g., if wrapping a distribution around a Xu problem.
    #  Need to add them into this list since it will be used to index array
    #  entries, you can't have any missing indices.
#all_correct_node_IDs = cor_nodes$node_ID
all_correct_node_IDs = 1:max(cor_nodes$node_ID)

presences_col_name = "freq"

#===============================================================================

cor_or_app_subdir_name = "cor"

        #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
        #                       reading Xu file, e.g., prob_generator_params_known.
        #                       This is because learning alg downstream needs to
        #                       know things like whether the generator's params
        #                       are even known, so that it doesn't try to learn
        #                       something from missing data.

do_graph_and_marxan_analysis (cor_or_app_subdir_name,

                                            #  input parameters
                                          parameters,
                                          emulatingTzar,
                                          DEBUG_LEVEL,
                                          #derived_bdpg_parameters,   #  BUG?  UNKNOWN FOR XU FROM FILE?
                                          derived_bdpg_parameters$current_os,

                                            #  parameters from gen prob.
#                                          bdprob$derived_Xu_params,
                                          bdprob@Xu_parameters,
                                          bdprob@read_Xu_problem_from_Xu_file,

#            PU_spp_pair_names,  #NO

                                            #  From bdprob structure, i.e., results of gen prob routine
                                          cor_num_spp,
                                          cor_num_PUs,
                                          cor_PU_spp_pair_indices,
    cor_PU_IDs, #####!!!!!#####
    cor_spp_IDs,  #####!!!!!#####
                                          cor_bpm,

                            cor_PU_costs,
                                          cor_optimum_cost,
                                          cor_nodes,
                                          spp_col_name,
                                          PU_col_name,

                                            #  Immediately after bdprob struct vars above.
                                          presences_col_name, #  hard-coded as "freq"
#####!!!!!#####                                          all_correct_node_IDs,

                                            #  Results of adding error.
                                            cor_num_spp,
                                            cor_num_PUs,
                                          cor_PU_spp_pair_indices,
                                          cor_bpm,

                                            #  input parameters for error model.
                                          add_error=FALSE,
                                          match_error_counts=FALSE,
                                          FP_const_rate=0,
                                          FN_const_rate=0,
                                          original_FP_const_rate=0,
                                          original_FN_const_rate=0
                                          )

#===============================================================================
#                   Add error to the species occupancy data.
#===============================================================================

add_error = FALSE
if (! is.null (parameters$add_error_to_spp_occupancy_data))
    add_error = parameters$add_error_to_spp_occupancy_data

if (add_error)
    {
    ret_vals_from_add_errors =
        add_error_to_spp_occupancy_data (parameters, cor_bpm,
                                         cor_num_PU_spp_pairs,
                                         cor_num_PUs, cor_num_spp,
                                         bdpg_error_codes)

        #  Save the chosen error parameters to output later with results.
    original_FP_const_rate = ret_vals_from_add_errors$original_FP_const_rate
    original_FN_const_rate = ret_vals_from_add_errors$original_FN_const_rate
    match_error_counts     = ret_vals_from_add_errors$match_error_counts
    FP_const_rate          = ret_vals_from_add_errors$FP_const_rate
    FN_const_rate          = ret_vals_from_add_errors$FN_const_rate
    app_num_spp            = ret_vals_from_add_errors$app_num_spp
    app_num_PUs            = ret_vals_from_add_errors$app_num_PUs

        #  Set the values for the apparent problem structure.
    app_PU_spp_pair_indices      = ret_vals_from_add_errors$app_PU_spp_pair_indices
    app_bpm                      = ret_vals_from_add_errors$app_spp_occupancy_data

#=================================

        #  Create subdirectory name for this apparent problem.
        #  In the future, we may allow more than 1 app per cor, so
        #  I'll add an app count to the end of the subdirectory name and
        #  nest it under a more general "app" directory that corresponds to
        #  the "cor" directory.

    cur_app_num = 1
    cor_or_app_subdir_name = paste0 ("app", .Platform$file.sep, "app.", cur_app_num)

    do_graph_and_marxan_analysis (cor_or_app_subdir_name,

                                                #  input parameters
                                              parameters,
                                              emulatingTzar,
                                              DEBUG_LEVEL,
                                              #derived_bdpg_parameters,   #  BUG?  UNKNOWN FOR XU FROM FILE?
                                              derived_bdpg_parameters$current_os,

                                                #  parameters from gen prob.
#                                              bdprob$derived_Xu_params,
                                              bdprob@Xu_parameters,
                                              bdprob@read_Xu_problem_from_Xu_file,

    #            PU_spp_pair_names,  #NO

                                                #  From bdprob structure, i.e., results of gen prob routine
                                              cor_num_spp,
                                              cor_num_PUs,
                                              cor_PU_spp_pair_indices,
    cor_PU_IDs, #####!!!!!#####
    cor_spp_IDs,  #####!!!!!#####
                                              cor_bpm,

                            cor_PU_costs,
                                              cor_optimum_cost,
                                              cor_nodes,
                                              spp_col_name,
                                              PU_col_name,

                                                #  Immediately after bdprob struct vars above.
                                              presences_col_name, #  hard-coded as "freq"
#####!!!!!#####                                              all_correct_node_IDs,

                                                #  Results of adding error.
                                                app_num_spp,
                                                app_num_PUs,
                                              app_PU_spp_pair_indices,
                                              app_bpm,

                                                #  input parameters for error model.
                                              add_error,
                                              match_error_counts,
                                              FP_const_rate,
                                              FN_const_rate,
                                              original_FP_const_rate,
                                              original_FN_const_rate
                                              )
    } #####else    #  Don't add error.
#     {         #  Since no error is being added, correct and apparent are the same.
#
#     app_PU_spp_pair_indices      = cor_PU_spp_pair_indices
#     app_bpm                      = cor_bpm
#     }


#===============================================================================

clean_up (timepoints_df, cur_timepoint_num, parameters, emulatingTzar,
          "\n\n>>>>>  Ran to completion.  <<<<<\n\n")

#===============================================================================

}

#===============================================================================


