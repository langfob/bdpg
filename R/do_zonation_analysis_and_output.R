#===============================================================================

                    #  do_zonation_analysis_and_output.R

#===============================================================================

#-------------------------------------------------------------------------------
#  NOTE:  You could swap the 2 criteria here and choose first on min(sum(loss))
#         and break ties on min(max(loss)) instead of zonation's normal EF
#         which is the other way around.
#-------------------------------------------------------------------------------
#  This also makes me wonder if there is a theoretically reasoned way to choose
#  a criteria that is most robust to errors in the input data.
#  I'm thinking about this in relation to the A-B/A+B stuff, where some kinds of
#  arithmetic expressions are more stable than others.
#  What might that look like here?
#  Summing seems more stable than max-ing.
#  min ((I * Sum(x_i + err(x_i))) == I*Sum(x_i) + I*(err(x_i))
#      vs.
#  min (Max (x_i + err(x_i))) ?
#
#  Also, does it matter whether you're going forward or backwards in the search?
#  That would be quite easy to phrase in a test, though it means reversing
#  max() and min() calls inside the code in several places.
#  Could those by localized to some very small bit of code that is all you need
#  to swap out?
#  For that matter, you could just replace max and min with inner and outer
#  and in one case, set inner=max, outer=min and in the other, vice versa
#  and then be able to use exactly the same base code?
#-------------------------------------------------------------------------------

#===============================================================================

z <- function (num_spp,
               num_PUs,
               wt_spp_vec,
               c_PU_vec,
               bpm,
               forward = FALSE,  #  Normally true for zonation.
               z_meth = "funcs",
               spp_rep_targets = rep (1, num_spp)
               )
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


