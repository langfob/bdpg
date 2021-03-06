#===============================================================================

                        #  testscript.R

#===============================================================================

#  testthat::test_file() fails when you're testing functions that are not
#  exported from the package when you're running the tests outside of CHECK.
#  The following stackoverflow question suggests that you source the R file(s)
#  containing the function(s) you want to test.  That way, they end up in the
#  namespace of the session where you're working and they can be called.

#-------------------------------------------------------------------------------

# https://stackoverflow.com/questions/6041079/how-can-we-test-functions-that-arent-exposed-when-building-r-packages
#
# You can test during development with the usual testthat functions because
# you're probably just sourcing in all your R code and not worrying about
# namespaces (at least that's how I develop). You then use R CMD check in
# conjunction with test_package to ensure the tests still work at build time -
# test_packages runs the tests in the package namespace so they can test
# non-exported functions.
# answered May 18 '11 at 12:36
# hadley
#

#===============================================================================

library (here)

proj_dir = here()
cat ("\n\nproj_dir = here() = ", proj_dir, "\n", sep='')

#===============================================================================

    #  Here's what worked for me AFTER DOING A BUILD ON BDPG:
library(bdpg)
library(testthat)


source(file.path (proj_dir, 'R/BDProb.R'))
source(file.path (proj_dir, 'R/apply_error_to_spp_occupancy_data.R'))
source(file.path (proj_dir, 'R/biodivprobgen_initialization.R'))
source(file.path (proj_dir, 'R/biodivprobgen_utilities.R'))
source(file.path (proj_dir, 'R/compute_checksums.R'))
source(file.path (proj_dir, 'R/compute_pu_and_spp_histograms_from_marxan_pusvpr_file.R'))
source(file.path (proj_dir, 'R/create_dirs.R'))
source(file.path (proj_dir, 'R/do_marxan_analysis_and_output.R'))
source(file.path (proj_dir, 'R/do_gurobi_analysis_and_output.R'))
source(file.path (proj_dir, 'R/gen_4_basic_variants.R'))
source(file.path (proj_dir, 'R/gen_20_basic_variants.R'))
source(file.path (proj_dir, 'R/gen_bdprob.R'))
source(file.path (proj_dir, 'R/gen_lognormal_overlay.R'))
#source(file.path (proj_dir, 'R/gen_multi_bdprob.R'))
source(file.path (proj_dir, 'R/gen_single_bdprob.R'))
source(file.path (proj_dir, 'R/gen_single_bdprob_APP.R'))
source(file.path (proj_dir, 'R/gen_wrapped_bdprob.R'))
#source(file.path (proj_dir, 'R/gmailr_test.R'))
source(file.path (proj_dir, 'R/gscp_10a_clean_up_completed_graph_structures.R'))
source(file.path (proj_dir, 'R/gscp_10b_compute_solution_rep_levels_and_costs.R'))
source(file.path (proj_dir, 'R/gscp_10c_build_adj_and_cooccurrence_matrices.R'))
source(file.path (proj_dir, 'R/gscp_11_summarize_and_plot_graph_structure_information.R'))
source(file.path (proj_dir, 'R/gscp_11a_network_measures_using_bipartite_package.R'))
source(file.path (proj_dir, 'R/gscp_11b_network_measures_using_igraph_package.R'))
source(file.path (proj_dir, 'R/gscp_11c_init_object_graph_data.R'))
source(file.path (proj_dir, 'R/gscp_12_write_network_to_marxan_files.R'))
source(file.path (proj_dir, 'R/gscp_13_write_marxan_control_file_and_run_marxan.R'))
source(file.path (proj_dir, 'R/gscp_13_write_gurobi_control_file_and_run_gurobi.R'))
source(file.path (proj_dir, 'R/gscp_14_read_marxan_output_files.R'))
source(file.path (proj_dir, 'R/gscp_14a_compute_marxan_solution_scores.R'))
source(file.path (proj_dir, 'R/gscp_14b_compute_solution_scores.R'))
source(file.path (proj_dir, 'R/gscp_14c_plot_incremental_marxan_summed_solution_reps.R'))
source(file.path (proj_dir, 'R/gscp_14d_see_if_marxan_best_was_actually_best.R'))
source(file.path (proj_dir, 'R/gscp_15_create_master_output_structure.R'))
source(file.path (proj_dir, 'R/gscp_15a_write_prob_results.R'))
source(file.path (proj_dir, 'R/gscp_15b_compute_and_verify_scores.R'))
source(file.path (proj_dir, 'R/gscp_15c_read_partial_results_files.R'))
source(file.path (proj_dir, 'R/gscp_5_derive_control_parameters.R'))
source(file.path (proj_dir, 'R/gscp_6_create_data_structures.R'))
source(file.path (proj_dir, 'R/gscp_8_link_nodes_within_groups.R'))
source(file.path (proj_dir, 'R/gscp_9_link_nodes_between_groups.R'))
source(file.path (proj_dir, 'R/gscp_9a_create_Xu_graph.R'))
source(file.path (proj_dir, 'R/load_and_parse_Xu_set_cover_problem_file.R'))
source(file.path (proj_dir, 'R/random_seed_setting_utilities.R'))
source(file.path (proj_dir, 'R/single_action_using_tzar_reps.R'))
source(file.path (proj_dir, 'R/std_param_defns.R'))
source(file.path (proj_dir, 'R/write_marxan_input_files.R'))

#-------------------------------------------------------------------------------

#test_file(file.path (proj_dir, "tests/testthat/test_random_seed_setting_utilities.R"))
#test_file(file.path (proj_dir, "tests/testthat/test_biodivprobgen_utilities.R"))
#test_file(file.path (proj_dir, "tests/testthat/test_gen_single_bdprob.R"))
test_file(file.path (proj_dir, "tests/testthat/test_gen_wrapped_bdprob.R"))
#test_file(file.path (proj_dir, "tests/testthat/test_vb.R"))
#test_file(file.path (proj_dir, "tests/testthat/test_vn.R"))
#test_file(file.path (proj_dir, "tests/testthat/test_do_gurobi_analysis_and_output.R"))
#test_file(file.path (proj_dir, "tests/testthat/test_gen_20_basic_variants.R"))

#===============================================================================

