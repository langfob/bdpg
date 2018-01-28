#===============================================================================

                    #  do_zonation_analysis_and_output.R

#===============================================================================

#  2018 01 28 - BTL
#  Have just replaced z() with zonation_like() but z() still appears in many
#  of the old tests, so I'm just going to make a dummy z() with the same
#  interface that just calls the new function zonation_like().
#  Will change the tests in testthat/greedy later and then can get rid of this
#  function.

z <- function (num_spp,
               num_PUs,
               wt_spp_vec,
               c_PU_vec,
               bpm,
               forward = FALSE,  #  Normally true for zonation.
               z_meth = "funcs",
               spp_rep_targets = rep (1, num_spp))
    {
    return (zonation_like  (num_spp,
                            num_PUs,
                            bpm,
                            forward,
                            spp_rep_targets,
                            wt_spp_vec,
                            c_PU_vec,
                            z_meth))
    }

#===============================================================================

zonation_like <- function (num_spp,
                           num_PUs,
                           bpm,
                           forward = TRUE,
                           spp_rep_targets = rep (1, num_spp),
                           wt_spp_vec      = rep (1, num_spp),
                           c_PU_vec        = rep (1,num_PUs),
                           z_meth          = "funcs")
    {
    if ("funcs" %in% z_meth)
        {
        ranked_solution_PU_IDs_vec = z_using_funcs (num_spp,
                                                    num_PUs,
                                                    wt_spp_vec,
                                                    c_PU_vec,
                                                    bpm,
                                                    forward)
        } else if ("inline" %in% z_meth)
        {
        ranked_solution_PU_IDs_vec = z_using_inline (num_spp,
                                                     num_PUs,
                                                     wt_spp_vec,
                                                     c_PU_vec,
                                                     bpm,
                                                     forward)
        } else if ("for" %in% z_meth)
        {
        ranked_solution_PU_IDs_vec = z_using_for (num_spp,
                                                  num_PUs,
                                                  wt_spp_vec,
                                                  c_PU_vec,
                                                  bpm,
                                                  forward)
        } else
        {
        stop_bdpg (paste0 ("Unrecognized z_meth = '", z_meth, "'.  ",
                           "Must be one of 'funcs', 'inline', or 'for'."))
        }


    short_ranked_solution_PU_IDs_vec =
        find_first_solution_with_all_rep_tgts_met (bpm,
                                                   ranked_solution_PU_IDs_vec,
                                                   spp_rep_targets)

    return (list (short_ranked_solution_PU_IDs_vec =
                      short_ranked_solution_PU_IDs_vec,
                  full_ranked_solution_PU_IDs_vec = ranked_solution_PU_IDs_vec))
    }

#===============================================================================


