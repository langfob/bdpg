#===============================================================================

                #  gscp_15_create_master_output_structure.R

#===============================================================================

#' Create a master output structure for all problem attributes and reserve
#' selection results when evaluating a CORRECT problem.
#'
#'When evaluating performance on a CORRECT problem, pass that CORRECT problem
#'in as both the CORRECT and the APPARENT problem.  The code to create the
#'master output structure puts out various APPARENT values and this dummying
#'in of the CORRECT for the APPARENT allows the function to work identically
#'for CORRECT and APPARENT data.
#'
#' @param COR_bd_prob
#' @param marxan_control_values
#' @param marxan_output_values
#' @param parameters

create_COR_master_output_structure <- function (marxan_control_values,
                                                marxan_output_values,
                                                parameters,

                                                COR_bd_prob,
                                                COR_marxan_run
                                                )
    {
    create_master_output_structure (parameters            = parameters,
                                    marxan_output_values  = marxan_output_values,
                                    marxan_control_values = marxan_control_values,

                                    COR_bd_prob    = COR_bd_prob,
                                    APP_bd_prob    = COR_bd_prob,     #  <==
                                    APP_marxan_run = COR_marxan_run,  #  <==
                                    apply_error    = FALSE            #  <==
                                    )
    }

#===============================================================================

#' Create a master output structure for all problem attributes and reserve
#' selection results when evaluating an APPARENT problem.
#'
#' @param APP_bd_prob
#' @param COR_bd_prob
#' @param marxan_control_values
#' @param marxan_output_values
#' @param parameters

create_APP_master_output_structure <- function (marxan_control_values,
                                                marxan_output_values,
                                                parameters,

                                                COR_bd_prob,
                                                APP_bd_prob,
                                                APP_marxan_run
                                                )
    {
    create_master_output_structure (parameters            = parameters,
                                    marxan_output_values  = marxan_output_values,
                                    marxan_control_values = marxan_control_values,

                                    COR_bd_prob    = COR_bd_prob,
                                    APP_bd_prob    = APP_bd_prob,     #  <==
                                    APP_marxan_run = APP_marxan_run,  #  <==
                                    apply_error    = TRUE             #  <==
                                    )
    }

#===============================================================================

#' Create a master output structure for all problem attributes and reserve
#' selection results.
#'
#'When evaluating performance on a CORRECT problem, pass that CORRECT problem
#'in as both the CORRECT and the APPARENT problem.  The code to create the
#'master output structure puts out various APPARENT values and this dummying
#'in of the CORRECT for the APPARENT allows the function to work identically
#'for CORRECT and APPARENT data.
#'
#' @param marxan_output_values
#' @param COR_bd_prob
#' @param APP_bd_prob
#' @param APP_marxan_run
#' @param apply_error

create_master_output_structure <- function (marxan_output_values,
                                            COR_bd_prob,
                                            APP_bd_prob,     #  <==  different for COR/APP
                                            APP_marxan_run,  #  <==  different for COR/APP
                                            apply_error      #  <==  different for COR/APP
                                            )
    {
    num_PUs                             = COR_bd_prob@num_PUs
    num_spp                             = COR_bd_prob@num_spp

    app_bpm                             = APP_bd_prob@bpm

    marxan_mvbest_df                    = marxan_output_values$marxan_mvbest_df

    if (apply_error)
        {       #  APPARENT => there is an error model
        FP_const_rate           = APP_bd_prob@APP_prob_info@FP_const_rate    # 0
        FN_const_rate           = APP_bd_prob@APP_prob_info@FN_const_rate    # 0

        } else
        {       #  CORRECT => no error model
        FP_const_rate           = 0
        FN_const_rate           = 0
        }

{

  #===============================================================================

  #  2017 02 17 - BTL
  #  ALL 3 OF THESE VALUES COULD BE COMPUTED LONG BEFORE COMING TO THIS ROUTINE,
  #  THOUGH I DON'T KNOW IF THEY'RE ALL THAT NECESSARY.  IN ANY CASE, THEY
  #  COULD EVEN BE ADDED TO THE PROBLEM OBJECT AS SOON AS THE PROBLEM IS BUILT.
  #  THEY _ARE_ USED FURTHER DOWN IN HERE TO DO THINGS LIKE COMPUTE SOLUTION
  #  SCORES, HOWEVER, EVEN THERE I WONDER IF THEY SHOULD BE REPLACED BY COSTS
  #  INSTEAD OF PATCH COUNTS SINCE PATCH COUNTS ARE REALLY JUST SURROGATES FOR
  #  COSTS WHERE EVERY PU COST = 1.  CHANGING ALL THESE THINGS TO COSTS MIGHT
  #  BE NECESSARY FOR MAKING THIS CODE GENERALIZE TO NON-XU PROBLEMS.

    if (COR_bd_prob@correct_solution_vector_is_known)
        {
        derived_Xu_params                     = COR_bd_prob@prob_gen_info@Xu_parameters@derived_params
        opt_solution_as_frac_of_tot_num_nodes = derived_Xu_params@opt_solution_as_frac_of_tot_num_nodes

        correct_solution_vector               = COR_bd_prob@nodes$dependent_set_member
        cor_num_patches_in_solution           = sum (correct_solution_vector)
        }

  #===============================================================================

  #  2017 02 17 - BTL
  #  THIS SECTION COULD BE COMPUTED AT END OF MARXAN RUN AND SAVED IN FILES THERE,
  #  THEN READ BACK IN HERE IF USING THE CBIND STRATEGY FOR ASSEMBLING OUTPUT DF.

  {
      #---------------------------------------------------------------------------
      #               Summarize marxan solution features.
      #---------------------------------------------------------------------------
      {
          #  Find which PUs marxan chose for its best solution.
      marxan_best_df_sorted = marxan_output_values$marxan_best_df_sorted
      marxan_best_solution_PU_IDs = which (marxan_best_df_sorted$SOLUTION > 0)  #  solution has a 1 for each PU in the solution and 0 otherwise
      marxan_best_num_patches_in_solution = length (marxan_best_solution_PU_IDs)

          #  Compute error in cost of best marxan solution.
          #  Assumes equal cost for all patches, i.e., cost per patch = 1.
              #  BTL - 2017 02 17
              #  It's simple to fix this, i.e., these num_patches bits should
              #  be changed to costs instead of patch counts.
              #  Since patch costs are the same as patch counts in the Xu
              #  problems, it will work for now and for the future.

      marxan_best_solution_cost_err_frac = (marxan_best_num_patches_in_solution - cor_num_patches_in_solution) / cor_num_patches_in_solution
      abs_marxan_best_solution_cost_err_frac = abs (marxan_best_solution_cost_err_frac)

  # #  2017 02 17 - BTL
  # #  HOW I MIGHT WANT TO REWRITE THE STUFF ABOVE.
  # #  NEED TO LOOK UP WHERE I'M STORING THE PU COSTS NOW SO THAT THE WAY IT'S
  # #  STATED IN THE EXPRESSIONS BELOW IS CORRECT.
  # #
  # #  There are really 4 errors here:
  # #  - an apparent cost error wrt the correct cost
  # #  - a correct cost error  wrt the correct cost and
  # #  - an absolute error in the estimation of the cost (i.e., abs(app_cost - cor_cost)) and
  # #  - a fractional error in the estimation of the cost (i.e., abs_cost_err/cor_cost)
  #
  #     marxan_best_sol_cost = sum (cost [marxan_best_df_sorted$SOLUTION > 0])
  #     marxan_best_solution_cost_err_frac =
  #         (marxan_best_sol_cost - cor_cost) / cor_cost
  #     abs_marxan_best_solution_cost_err_frac =
  #         abs (marxan_best_solution_cost_err_frac)

      }

      #===========================================================================
      #       Compute correct and apparent scores for marxan solution.
      #===========================================================================

  {
      #---------------------------------------------------------------------------
      #               Apparent scores as computed by marxan
      #---------------------------------------------------------------------------
      {
      app_solution_NUM_spp_covered__fromMarxan  = sum (marxan_mvbest_df$MPM)
      app_solution_FRAC_spp_covered__fromMarxan = app_solution_NUM_spp_covered__fromMarxan / num_spp
      app_spp_rep_shortfall__fromMarxan         = 1 - app_solution_FRAC_spp_covered__fromMarxan
      }

      #---------------------------------------------------------------------------
      #               Apparent scores as computed by bdpg...
      #---------------------------------------------------------------------------
  {
      app_results_list =
              compute_solution_vector_scores (app_bpm,

                                                  #  Identical args from here down
                                                  #  for cor and app.
                                              num_PUs,
                                              marxan_best_solution_PU_IDs,
                                              marxan_best_num_patches_in_solution,
                                              cor_num_patches_in_solution,
                                              APP_marxan_run@targets,
                                              num_spp,
                                              FP_const_rate,
                                              FN_const_rate)
  }
      #---------------------------------------------------------------------------
      #               Correct scores as computed by bdpg...
      #---------------------------------------------------------------------------
  {
      cor_results_list =
              compute_solution_vector_scores (cor_bpm,

                                                  #  Identical args from here down
                                                  #  for cor and app.
                                              num_PUs,
                                              marxan_best_solution_PU_IDs,
                                              marxan_best_num_patches_in_solution,
                                              cor_num_patches_in_solution,
                                              APP_marxan_run@targets,
                                              num_spp,
                                              FP_const_rate,
                                              FN_const_rate)
  }
  }  #  end - Compute correct and apparent scores for marxan solution.
  }

}


    }  #  end of function - create_master_output_structure()

#===============================================================================

