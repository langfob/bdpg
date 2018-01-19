#===============================================================================

                    #  gscp_15c_read_partial_results_files.R

#===============================================================================

# Pseudocode for creating csv (Excel) output files
#
# - One for each reserve selection run on a given problem:
#     - problem could be correct or apparent
#     - problem could be wrapped or not
#
# - In all cases below:
#     - values should have been saved inside of the corresponding directory for the constituent object
#
# Composed of:
# - tzar run ID information for this RSrun
#     - tzar run IDs for each constituent object can be derived from their UUIDs,
#       assuming those have been saved in the objects.
#         - Are they saved in the objects?
# - Xu characteristics for the given problem (where known, NA otherwise)
#     - if problem is Xu-generated
#         - Xu base parameters
#         - Xu derived parameters
#     - if problem is read from Xu file or not generated from Xu
#         - NA
# - non-Xu characteristics for the given problem (num_PUs, num_spp, anything else?)
#     - correct non-Xu characteristics
#     - apparent non-Xu characteristics
# - error generator characteristics
#     - if apparent, from object
#     - if correct, 0 or NA
# - classification scores for the given problem
#     - correct classification scores
#     - apparent classification scores
# - representation scores for the given problem
#     - correct representation scores
#     - apparent representation scores
# - graph measures for the given problem
#     - igraph measures
#     - bipartite measures

#===============================================================================

read_results_list_from_csv_file <- function (csv_file)
    {
    if (file.exists (csv_file))
        {
        results_df = read.csv (csv_file, header=TRUE)
        results_list = as.list (results_df)

        } else
        {
        results_list = NULL
        cat ("\n\ncsv file does not exist, so not read.\n    File name = '",
             csv_file, "'\n")
        }

    return (results_list)
    }

#-------------------------------------------------------------------------------

read_prob_characteristics_list <- function (rsprob,
                                            src_rds_file_dir,
                                            exp_root_dir,
                                            use_src_rds_file_dir  #,
                                            #parameters
                                            )
    {
        #-----------------------------------------------------------------------
        # - Xu characteristics for the given problem (where known, NA otherwise)
        #     - if problem is Xu-generated
        #         - Xu base parameters
        #         - Xu derived parameters
        #     - if problem is read from Xu file or not generated from Xu
        #         - NA
        # - non-Xu characteristics for the given problem (num_PUs, num_spp,
        #   anything else?)
        #     - correct non-Xu characteristics
        #     - apparent non-Xu characteristics
        # - error generator characteristics
        #     - if apparent, from object
        #     - if correct, 0 or NA
        #-----------------------------------------------------------------------

    if (use_src_rds_file_dir)
        {
            #  BD problem came from an rds file.
        prob_characteristics_file =
            file.path (src_rds_file_dir, "prob_characteristics.csv")

        } else
        {
            #  BD problem not from an rds file.
        prob_characteristics_file =
            file.path (get_RSprob_path_topdir (rsprob, exp_root_dir),
    #                   parameters$summary_filename)
                       "prob_characteristics.csv")
        }

    results_list = read_results_list_from_csv_file (prob_characteristics_file)

    return (results_list)
    }

#-------------------------------------------------------------------------------

read_bipartite_measures_list <- function (rsprob,
                                          src_rds_file_dir,
                                          exp_root_dir,
                                          use_src_rds_file_dir)
    {
    if (use_src_rds_file_dir)
        {
            #  BD problem came from an rds file.
        bipartite_file =
            file.path (src_rds_file_dir,
                       paste0 (rsprob@bipartite_metrics_file_name_stem, ".csv"))

        } else
        {
            #  BD problem not from an rds file.
        bipartite_file =
            file.path (get_RSprob_path_networks (rsprob, exp_root_dir),
                       paste0 (rsprob@bipartite_metrics_file_name_stem, ".csv"))
        }

    results_list = read_results_list_from_csv_file (bipartite_file)

    return (results_list)
    }

#-------------------------------------------------------------------------------

read_igraph_measures_list <- function (rsprob,
                                       src_rds_file_dir,
                                       exp_root_dir,
                                       use_src_rds_file_dir)
    {
    if (use_src_rds_file_dir)
        {
            #  BD problem came from an rds file.
        igraph_file =
            file.path (src_rds_file_dir,
                       paste0 (rsprob@igraph_metrics_file_name_stem, ".csv"))

        } else
        {
            #  BD problem not from an rds file.
        igraph_file =
            file.path (get_RSprob_path_networks (rsprob, exp_root_dir),
                       paste0 (rsprob@igraph_metrics_file_name_stem, ".csv"))
        }

    results_list = read_results_list_from_csv_file (igraph_file)

    return (results_list)
    }

#===============================================================================

build_and_write_scores_list <-
    function (rsrun,
        app_bpm,    #bpm,
              rs_best_solution_PU_IDs,
              spp_rep_targets,
              num_spp,
              marxan_best_num_patches_in_solution,     #num_PUs_in_cand_solution,
              num_PUs,
              cor_num_patches_in_solution,             #num_PUs_in_optimal_solution,
              FP_const_rate,
              FN_const_rate
              )

        # rsrun,
        #         marxan_best_num_patches_in_solution,    #num_PUs_in_cand_solution,
        #         num_PUs,
        #         cor_num_patches_in_solution,            #num_PUs_in_optimal_solution,
        #         app_frac_spp_covered,                   #app_rep_scores_list$frac_spp_covered,  #frac_spp_covered,
        #         FP_const_rate,                          #input_err_FP = 0,
        #         FN_const_rate
                # )
    {
  #    results_list = list (cls1=400, cls2=401)

      #----------

      app_rep_scores_list_according_to_bdpg =
          compute_and_verify_APP_rep_scores_according_to_bdpg (app_bpm,
                                                               rs_best_solution_PU_IDs,
                                                               spp_rep_targets,
                                                               num_spp)

      #----------

  # app_results_list = compute_solution_vector_scores (app_bpm = ref_spp_occ_matrix, ...

      app_confusion_matrix_based_error_measures_list =
          compute_confusion_matrix_based_scores (marxan_best_num_patches_in_solution,                      #num_PUs_in_cand_solution,
                                                  num_PUs,
                                                  cor_num_patches_in_solution,                             #num_PUs_in_optimal_solution,
                                                  app_rep_scores_list_according_to_bdpg$frac_spp_covered,  #frac_spp_covered,
                                                  FP_const_rate,                                           #input_err_FP = 0,
                                                  FN_const_rate                                            #input_err_FN = 0,
                                                 )

    #----------

    results_list = c (app_rep_scores_list_according_to_bdpg,
                      app_confusion_matrix_based_error_measures_list
                     )

    return (results_list)
    }

#===============================================================================


