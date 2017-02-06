#===============================================================================
#       Generate a multi problem, i.e, wrap or merge problems.
#===============================================================================

    #  Need to do some sanity checks on various wrap parameters
    #  to make sure that they can't generate nonsense or a crash
    #
    #  Just a dummy placeholder routine until I know what needs checking.

#' Do sanity checks on parameters for wrapping a biodiversity problem
#'
#' Currently just a placeholder...

do_sanity_checks <- function ()
    {
    cat ("\n\nDummy sanity check for wrap_...().\n\n")
    }

#===============================================================================

#' Create set of planning units eligible for adding species into
#'
#' When wrapping a species distribution around a Xu biodiversity problem,
#' the first instance of any new species is added to the solution set of the
#' original Xu problem that is being wrapped.  However, if more than one
#' instance of the new species is being added in the wrapped problem, the
#' extra instances could go in a) just the new planning units outside the
#' original Xu problem added for the wrapper, or b) in both the original
#' solution set and the newly added planning units.  This function builds
#' that target set of planning units based on the value of the
#' dep_set_PUs_eligible flag passed to it.
#'
#' @param Xu_dep_set integer vector of planning unit IDs for the solution set
#'     of the original Xu problem being wrapped around
#' @param extra_PUs integer vector of plannning unit IDs being added to the
#'     original Xu problem to make the wrapped problem
#' @param dep_set_PUs_eligible boolean indicating whether new species instances
#'     can be added to the original Xu problem's solution set in addition to
#'     the adding them to the newly added planning units; TRUE implies new
#'     species instances can be added to both sets; FALSE implies new species
#'     instances can only be added to the newly added planning units
#'
#' @return set of planning unit IDs where new species can be placed

create_eligible_PU_set <- function (Xu_dep_set,
                                    extra_PUs,
                                    dep_set_PUs_eligible
                                    )
    {
    if (dep_set_PUs_eligible)
        eligible_PUs = c(Xu_dep_set, extra_PUs)
    else
        eligible_PUs = extra_PUs

    return (eligible_PUs)
    }

#===============================================================================


#'  Remove species from an abundance distribution if they fall on either
#'  too many or too few patches.
#'
#'  This function is primarily to get rid of species that occur on
#'  only 0 or 1 patches, however, some distribution generator might also
#'  generate species that are too common as well, so this routine will
#'  axe those too.
#'
#'  Abundances are expressed here as a vector of count values where each value
#'  is the count of the number of patches a particular species occurs on.
#'  For example, [1 1 1 2 6 6] would represent 3 species that occur
#'  on exactly one patch, 1 species that occurs on 2 patches, and 2 species
#'  that occur on 6 patches each.  Calling this routine with a min_abund of 2
#'  and a max_abund of 5 would return a 1 element vector, i.e., the vector [2].
#'
#'  This doesn't really need to be a function since it's only one line
#'  long, but I built an earlier version that was more verbose so that
#'  I could monitor how many things were being cut out.
#'  That version is in the git repository in case it needs to be
#'  resurrected for some reason.
#'
#' @param rounded_abundances vector of abundances to be trimmed
#' @param min_abund lowest abundance to allow in the trimmed set
#' @param max_abund largest abundance to allow in the trimmed set
#'
#' @return vector of abundances whose values lie in the specified range

trim_abundances = function (rounded_abundances,
                            min_abund=2,
                            max_abund=.Machine$double.xmax
                            )
    {
    return (rounded_abundances [(rounded_abundances <= max_abund) &
                                (rounded_abundances >= min_abund), drop=FALSE])
    }

#===============================================================================

#  New version:

#' Remove base problem's species from the wrapping problems set of species to
#' be distributed over the planning units
#'
#' When wrapping one distribution around another, the outside distribution
#' needs to contain the inside distribution as a proper subset, however,
#' when it's time to distribute the wrapping distribution over the landscape,
#' the inside distribution has already been distributed.  That means that the
#' inside distribution needs to be removed from full wrapping distribution
#' before its instances are spread across the landscape.  This function
#' strips that inside distribution out of the full wrapped distribution and
#' returns a set of abundances ready for spreading.
#'
#' @param Xu_PU_spp_table PU_spp table for original Xu problem being wrapped
#'     around
#' @param trimmed_rounded_abund_per_spp vector of abundances of all species in
#'     the full wrapped distribution, i.e., including the original Xu problem
#'     abundances
#' @param spp_col_name string containing the name of the column in the PU_spp
#'     table that contains the species IDs
#'
#' @return vector of abundances in the wrapping abundance distribution that are
#'     not also in the original Xu problem

remove_base_spp_abundances_from_wrapping_distribution <-
    function (Xu_PU_spp_table,
              trimmed_rounded_abund_per_spp,
              spp_col_name)
    {
                    verbose_remove_base = FALSE    #  just for debugging now...

                    if (verbose_remove_base)
                            {
                            cat ("\n\nStarting ",
                                 "remove_base_spp_abundances_from_wrapping_distribution():")
                                            cat ("\n    Xu_PU_spp_table = \n")
                                            show (Xu_PU_spp_table)
                                            cat ("\n    trimmed_rounded_abund_per_spp = \n")
                                            show (trimmed_rounded_abund_per_spp)
                                            cat ("\n    spp_col_name = \n")
                                            show (spp_col_name)
                            }

        #  Count the number of occurrences of each species in the base problem.
    base_abund_by_spp = plyr::count (Xu_PU_spp_table [,spp_col_name])
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    base_abund_by_spp = \n")
                            show (base_abund_by_spp)
                            }

        #  Count how many times each number of occurrences appears in that set
        #  (e.g., how many species occur on 2 patches, on 3 patches, etc.).
    base_abund_hist         = plyr::count (base_abund_by_spp [,"freq"])
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    base_abund_hist = \n")
                            show (base_abund_hist)
                            }

        #  Do the same for the wrapping abundances.
        #  They are given as a vector of numbers of occurrences for each
        #  species in the wrapping distribution, but unlike the base problem
        #  abundances, no species ID has been assigned yet to these wrapping
        #  counts.
    wrapping_abund_hist     = plyr::count (trimmed_rounded_abund_per_spp)
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    wrapping_abund_hist = \n")
                            show (wrapping_abund_hist)
                            }

        #  There will probably be some different elements in the two histograms.
        #  For example, the original Xu histogram will only have one entry,
        #  i.e., the count for abundance = 2, since all species occur on
        #  exactly 2 patches.  The wrapping histogram will probably (though
        #  not necessarily) have entries for other patch counts and may have
        #  a different value than the Xu histogram for the number of species
        #  occurring on 2 patches.
        #  We need to make a single histogram that merges the two sets of
        #  possible counts.  The merge() function does this by making a single
        #  data.frame with a column for each of the 2 input histograms,
        #  labelling one column as x and the other as y.  It adds a row for
        #  each matching entry in the inputs (e.g., a row for abundance = 2,
        #  with the x column giving the count for the wrapping histogram and
        #  the y column giving the count for the Xu histogram).
        #  It also adds a row for each entry in either input that doesn't
        #  appear in the other input (e.g., a row for every abundance other
        #  than 2 in the wrapping histogram when it's wrapping around an
        #  original Xu histogram that only has the value for abundance = 2).

    wrapped_extra_spp_abund_merge = merge (x=wrapping_abund_hist,
                                                y=base_abund_hist,
                                                by="x", all=TRUE)
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    wrapped_extra_spp_abund_merge = \n")
                            show (wrapped_extra_spp_abund_merge)
                            }

        #  Now we need to clean up this data frame so that NAs are replaced
        #  with 0's and so that any missing abundance values are added to
        #  the data.frame (e.g., if the highest abundance value was 10 but
        #  neither input histogram had any species that occurred on 3, 4, or
        #  9 PUs).




            #  Replace NA counts with 0s.
    wrapped_extra_spp_abund_merge [is.na (wrapped_extra_spp_abund_merge)] = 0
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    After NA replacement with 0, wrapped_extra_spp_abund_merge = \n")
                            show (wrapped_extra_spp_abund_merge)
                            }

    wrapped_extra_spp_abund_hist =
        as.data.frame (cbind (wrapped_extra_spp_abund_merge [,"x"],
                              wrapped_extra_spp_abund_merge [, "freq.x"] - wrapped_extra_spp_abund_merge [, "freq.y"]
                              ))
    names (wrapped_extra_spp_abund_hist) = c("abund","freq")
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    Final wrapped_extra_spp_abund_hist = \n")
                            show (wrapped_extra_spp_abund_hist)
                            }


    num_extra_spp = sum (wrapped_extra_spp_abund_hist [,"freq"])
    extra_spp_abund = rep (NA, num_extra_spp)
    num_abund_rows = length (wrapped_extra_spp_abund_hist[,"abund"])
    start_idx = 1
    for (cur_idx in 1:num_abund_rows)
        {
        if (wrapped_extra_spp_abund_hist [cur_idx, "freq"] > 0)    #  Should always be true, but just in case...
            {
            end_idx = start_idx + wrapped_extra_spp_abund_hist [cur_idx, "freq"] - 1
            extra_spp_abund [start_idx:end_idx] = wrapped_extra_spp_abund_hist [cur_idx, "abund"]

                        if (verbose_remove_base)
                            {
                            cat ("\nAt bottom of cur_idx = ", cur_idx, ",
                                 start_idx = ", start_idx,
                                 "end_idx = ", end_idx,
                                 "extra_spp_abund = \n")
                            print (extra_spp_abund)
                        }

            start_idx = end_idx + 1
            }
        }

    return (extra_spp_abund)
    }

#===============================================================================

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dep_set DESCRIPTION.
#' @param eligible_set DESCRIPTION.
#' @param rounded_abund_per_spp DESCRIPTION.
#' @param num_base_spp DESCRIPTION.
#' @param Xu_PU_spp_table DESCRIPTION.
#' @param min_allowed_abundance DESCRIPTION.
#' @param PU_col_name DESCRIPTION.
#' @param spp_col_name DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
wrap_abundances_around_eligible_set <- function (dep_set,
                                                    eligible_set,
                                                    rounded_abund_per_spp,
                                                    num_base_spp,
                                                    Xu_PU_spp_table,
                                                    min_allowed_abundance = 2,
                                                    PU_col_name = "PU_ID",
                                                    spp_col_name = "spp_ID"
                                                   )
    {
    cat ("\n\nStarting wrap_abundances_around_eligible_set()\n", sep='')

    #----------------------------------



        #  2016 06 21 - BTL
        #  NEED TO REMOVE ALL SPP WHOSE ABUNDANCE IS 1,
        #  BECAUSE THEY MAKE THE PROBLEM MUCH EASIER, I.E., ALL PUs CONTAINING
        #  ANY SPECIES WITH ABUNDANCE = 1 MUST AUTOMATICALLY BE INCLUDED IN
        #  CORRECT SOLUTION.
        #  THEREFORE, ANY PROBLEM CAN AUTOMATICALLY BE REDUCED TO A DIFFERENT
        #  AND HARDER PROBLEM BY REMOVING ALL SINGLETON SPECIES.
        #  The same would apply to any other target value if the target
        #  was > 1.  For example, if a species had a target of 3 and it only
        #  occurred on 3 patches or less, that species would need to be
        #  removed.  This doesn't happen in these initial Xu problems since
        #  the generator currently only works for having all targets = 1.


    trimmed_rounded_abund_per_spp = trim_abundances (rounded_abund_per_spp,
                                                     min_abund=min_allowed_abundance)
    plot (sort(trimmed_rounded_abund_per_spp, decreasing = TRUE))

    #----------------------------------

#  FIXED NOW?
 # BUG: THIS SECTION OF CODE ASSUMES THAT extra_abund IS A VECTOR CONTAINING
 #      THE ABUNDANCE OF EACH EXTRA SPECIES.
 #      AT THE MOMENT, extra_abund ACTUALLY CONTAINS THE NUMBER OF SPECIES
 #      HAVING EACH GIVEN ABUNDANCE.
 #      NEED TO CHANGE EXPAND WHAT'S RETURNED INTO THE EXPECTED VECTOR.
 #      CAN THAT BE DONE USING ONE OF THE PLYR THINGS LIKE MELT()?

                    extra_abund =
                        # remove_base_spp_abundances_from_wrapping_distribution (trimmed_rounded_abund_per_spp,
                        #                                                        num_base_spp,
                        #                                                        spp_col_name)
                        remove_base_spp_abundances_from_wrapping_distribution (Xu_PU_spp_table,
                                                                               trimmed_rounded_abund_per_spp,
                                                                               spp_col_name)

                    num_extra_occurrences = sum (extra_abund)
                    num_extra_spp         = length (extra_abund)

#  Should these be initialized to NA instead of 0?  Does it matter?
    PU_spp_table = data.frame (PU_ID = rep (0, num_extra_occurrences),
                               spp_ID = rep (0, num_extra_occurrences))

#browser()
    cur_row = 1

    for (cur_spp_idx in 1:num_extra_spp)
        {
#        cat ("\n\ncur_spp_idx = ", cur_spp_idx, "\n", sep='')

        num_PUs_to_draw = extra_abund [cur_spp_idx]

            #  Draw the one mandatory PU in the dependent set for this species.

        dep_set_PU = safe_sample (dep_set, 1, replace=FALSE)
        PU_spp_table [cur_row, "PU_ID"] = dep_set_PU
        PU_spp_table [cur_row, "spp_ID"] = cur_spp_idx
        cur_row = cur_row + 1

            #  If that PU was part of the overall eligible set
            #  (e.g., the eligible set was the union of the dependent set and
            #   the extra set rather than just being the extra set),
            #  remove it from the eligible set of PUs.

        x = which (eligible_set == dep_set_PU)
        cur_eligible_set =
            if (length (x) > 0) eligible_set [-x] else eligible_set

        num_PUs_to_draw = num_PUs_to_draw - 1
    # cat ("\nAbout to check num_PUs_to_draw > 0 for cur_spp_idx = ",
    #      cur_spp_idx, sep='')
#browser()
        if (num_PUs_to_draw > 0)
            {
            extra_PUs_for_cur_spp = safe_sample (cur_eligible_set,
                                                 num_PUs_to_draw,
                                                 replace=FALSE)

            end_row = cur_row + num_PUs_to_draw - 1
            PU_spp_table [cur_row:end_row, "PU_ID"]  = extra_PUs_for_cur_spp
            PU_spp_table [cur_row:end_row, "spp_ID"] = cur_spp_idx

            cur_row = end_row + 1

            }  #  end if - num_PUs_to_draw
        }  #  end for - cur_spp_idx

    cat ("\n\n--------------------------------------\n\n", "PU_spp_table = \n")
#    print (PU_spp_table)    #  usually too long to print...
    print (head (PU_spp_table))
    cat ("\n\n")

        #-------------------------------------------------------------------
        #  Add the occurrences from the original problem to the table
        #  just created for the wrapped problem.
        #  Since both the extra species and the base species started their
        #  species numbering at 1, need to offset the numbering of one or
        #  the other of these two groups.  Will change the numbering of
        #  the extra species to begin just after the last of the base
        #  species.
        #  This is a fairly arbitrary choice, but it may help debugging
        #  sometime since the base species will have the same spp IDs in
        #  the wrapped set as in the base set.
        #-------------------------------------------------------------------

    PU_spp_table [, "spp_ID"] = PU_spp_table [, "spp_ID"] + num_base_spp
    PU_spp_table = rbind (Xu_PU_spp_table, PU_spp_table)

    cat ("\n\nAfter rbind(), PU_spp_table = \n")
#    print (PU_spp_table)    #  usually too long to print...
    print (head (PU_spp_table))
    cat ("\n\n")

#browser()

    return (PU_spp_table)

    }  #  end function - wrap_abundances_around_eligible_set

#===============================================================================

    #  Note that this function doesn't care where you got the abundances,
    #  i.e., they don't have to have come from the lognormal generator.
    #  It can be any abundance set that you want, as long as it contains
    #  at least as many species sitting on exactly 2 patches as the base
    #  Xu problem has.  It can have more than the Xu problem, but not less.

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param rounded_abundances DESCRIPTION.
#' @param Xu_bdprob DESCRIPTION.
#' @param dep_set_PUs_eligible DESCRIPTION.
#' @param tot_num_PUs_in_landscape DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @export

wrap_abundance_dist_around_Xu_problem = function (rounded_abundances,
                                                  Xu_bdprob,
                                                  dep_set_PUs_eligible,
                                                  tot_num_PUs_in_landscape
                                                  )
    {
    Xu_nodes = Xu_bdprob@nodes
    Xu_dep_set = get_dependent_node_IDs (Xu_nodes)
    Xu_dep_and_indep_set = c(get_independent_node_IDs (Xu_nodes), Xu_dep_set)

largest_PU_ID = max (Xu_nodes$node_ID)
#    extra_PUs = (Xu_bdprob@num_PUs + 1) : tot_num_PUs_in_landscape
    extra_PUs = (largest_PU_ID + 1) : tot_num_PUs_in_landscape

    eligible_PUs =
        create_eligible_PU_set (Xu_dep_set, extra_PUs, dep_set_PUs_eligible)

    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------

        #  Now have the rounded distribution of species abundances to
        #  wrap around the given Xu problem, so do the wrap to generate a
        #  table of pairs of indices of PUs and species.

    wrapped_PU_spp_indices =
        wrap_abundances_around_eligible_set (Xu_dep_set,
                                             eligible_PUs,
                                             rounded_abundances,
                                             Xu_bdprob@num_spp,  #Xu_tot_num_spp,
                                             Xu_bdprob@PU_spp_pair_indices,
                                             min_allowed_abundance = 2,
                                             Xu_bdprob@PU_col_name,
                                             Xu_bdprob@spp_col_name)

      #----------------------------------

    wrapped_PU_vector = wrapped_PU_spp_indices [, Xu_bdprob@PU_col_name]
    unique_wrapped_PUs = sort (unique (wrapped_PU_vector))
            #  Highest PU_ID may not be occupied, so old counts like
            #  wrapped_highest_PU_ID and wrapped_num_PUs are wrong.
            #  This is especially important because these are the numbers that
            #  drives the dimensions of the bpm and costs and
            #  the length of marxan boolean solution indicator vectors.
    wrapped_highest_PU_ID = tot_num_PUs_in_landscape    #max (unique_wrapped_PUs)
    wrapped_num_PUs = tot_num_PUs_in_landscape          #length (unique_wrapped_PUs)

    if (wrapped_num_PUs != wrapped_highest_PU_ID)
        {
        cat ("\n\nXu_bdprob@num_PUs = \n", Xu_bdprob@num_PUs)
        cat ("\nsetdiff (unique_wrapped_PUs, 1:wrapped_highest_PU_ID) = \n")
        print (setdiff (unique_wrapped_PUs, 1:wrapped_highest_PU_ID))
        missing_PU_IDs = setdiff (1:wrapped_highest_PU_ID, unique_wrapped_PUs)
        cat ("\nMissing ", length (missing_PU_IDs),
           " PU_IDs = setdiff (1:wrapped_highest_PU_ID, unique_wrapped_PUs) = \n")
        print (setdiff (1:wrapped_highest_PU_ID, unique_wrapped_PUs))
        cat ("\n")
        message (paste0 ("\n\nwrapped_num_PUs (", wrapped_num_PUs,
                         ") != wrapped_highest_PU_ID (", wrapped_highest_PU_ID,
                         ")")
                )
        if (min (missing_PU_IDs) > Xu_bdprob@num_PUs)
            {
            message (paste0 ("\nProbably just didn't draw some PU_IDs in ",
                             "extra set when placing wrapping species, since ",
                             "lowest missing PU_ID (", min (missing_PU_IDs),
                             " is larger than highest PU_ID in base problem (",
                             Xu_bdprob@num_PUs, ").\n\n"))
            } else
            {
            stop (paste0 ("\nlowest missing PU_ID (", min (missing_PU_IDs),
                             " is <= highest PU_ID in base problem (",
                             Xu_bdprob@num_PUs, ").\n\n"))
            }
        }

    used_extra_PUs = setdiff (unique_wrapped_PUs, Xu_dep_and_indep_set)
    num_used_extra_PUs = length (used_extra_PUs)
    #    unused_extra_PUs = setdiff (extra_PUs, used_extra_PUs)

num_extra_PUs = length (extra_PUs)

    extra_nodes =
            data.frame (node_ID = extra_PUs,                                 #used_extra_PUs,
                      group_ID = rep (NA, num_extra_PUs),                    #num_used_extra_PUs),
                      dependent_set_member = rep (FALSE, num_extra_PUs))     #num_used_extra_PUs))
    wrapped_nodes = rbind (Xu_bdprob@nodes, extra_nodes)

    all_PU_IDs = 1:wrapped_highest_PU_ID

      #----------------------------------

cat ("\n\nJust after loading wrapped_nodes:\n")
#browser()
# nodes = data.frame (node_ID = node_IDs,
#                     group_ID = group_IDs,
#                     dependent_set_member = dependent_set_members)

      #----------------------------------

    wrapped_spp_vector = wrapped_PU_spp_indices [, Xu_bdprob@spp_col_name]
    unique_wrapped_spp = sort (unique (wrapped_spp_vector))
    wrapped_num_spp = length (unique_wrapped_spp)
    wrapped_highest_spp_ID = max (unique_wrapped_spp)

    if (wrapped_num_spp != wrapped_highest_spp_ID)
        {
        cat ("\n\nWARNING: wrapped_num_spp (", wrapped_num_spp,
           ") != wrapped_highest_spp_ID (", wrapped_highest_spp_ID, ")")
        cat ("\n    setdiff (unique_wrapped_spp, 1:wrapped_highest_spp_ID) = ",
           setdiff (unique_wrapped_spp, 1:wrapped_highest_spp_ID))
        cat ("\n    setdiff (1:wrapped_highest_spp_ID, unique_wrapped_spp) = ",
           setdiff (1:wrapped_highest_spp_ID, unique_wrapped_spp))
        cat ("\n\n")
        }

    all_spp_IDs = 1:wrapped_highest_spp_ID

      #---------------------------------------------------------------------------

              #-----------------------------------------------------------
              #  Convert PU/spp data structure into other formats needed
              #  downstream.
              #
              #  NOTE:  I'm using wrapped_highest_???_ID instead of
              #         wrapped_num_??? in this call because I think
              #         that there may be runs where not all of the
              #         extra_PUs have something land on them.
              #         If that's the case, then the bpm adjacency
              #         matrix might end up being too small if it's
              #         dimensioned from the counts instead of from the
              #         highest ID number.  Probably won't matter for
              #         spp, but I'm doing the same for species just
              #         in case.
              #-----------------------------------------------------------

    wrapped_PU_costs = get_PU_costs (wrapped_num_PUs)
    wrapped_bpm =
        create_adj_matrix_with_spp_rows_vs_PU_cols (
                                    wrapped_highest_spp_ID, #wrapped_num_spp,
                                    wrapped_highest_PU_ID,  #wrapped_num_PUs,
                                    wrapped_PU_spp_indices,  #wrapped_PU_spp_pair_indices,
                        wrapped_PU_costs,
                                    Xu_bdprob@spp_col_name,
                                    Xu_bdprob@PU_col_name,
                                    Xu_dep_set,
                                    Xu_bdprob@correct_solution_vector_is_known,
                                    Xu_bdprob@bdpg_error_codes
                                    )

      #---------------------------------------------------------------------------

          #  Assign a unique identifier to this newly generated problem.
          #  These IDs are useful when combining or adding error to
          #  problems so that you can identify exactly which problems
          #  were combined or used as a base when provenance might get
          #  confusing.

      UUID = uuid::UUIDgenerate()    #  ***  Need to generate this for Xu too in another routine.  ***  #

      #---------------------------------------------------------------------------

          #  Now have a completed problem, so build the structure describing it
          #  for return to the caller.

    wrapped_bdprob = new ("Xu_wrapped_bd_problem")

#    wrapped_bdprob@base_bd_prob                 = Xu_bdprob
    wrapped_bdprob@UUID_of_base_problem_that_is_wrapped = Xu_bdprob@UUID

    wrapped_bdprob@UUID                         = UUID
    wrapped_bdprob@read_Xu_problem_from_Xu_file = Xu_bdprob@read_Xu_problem_from_Xu_file

    wrapped_bdprob@PU_spp_pair_indices          = wrapped_PU_spp_indices
    wrapped_bdprob@all_PU_IDs                   = all_PU_IDs
    wrapped_bdprob@all_spp_IDs                  = all_spp_IDs

    wrapped_bdprob@PU_col_name                  = Xu_bdprob@PU_col_name
    wrapped_bdprob@spp_col_name                 = Xu_bdprob@spp_col_name
    wrapped_bdprob@num_PUs                      = wrapped_num_PUs
    wrapped_bdprob@num_spp                      = wrapped_num_spp
    wrapped_bdprob@PU_costs                     = wrapped_PU_costs    #get_PU_costs (wrapped_num_PUs)

    wrapped_bdprob@Xu_parameters                = Xu_bdprob@Xu_parameters
    wrapped_bdprob@prob_is_ok                   = TRUE

    wrapped_bdprob@nodes                        = wrapped_nodes
    wrapped_bdprob@bpm                          = wrapped_bpm

          #  Since the problem is wrapped around the Xu problem,
          #  the correct optimum cost is the same as for the Xu problem.
          #  The vector of costs is longer though, since there are more PUs
          #  in the wrapped problem.
          #  At the moment, those PU costs are all dummied to 1 anyway.
          #  If that ever changes, then get_PU_costs() is going to have to change.

    wrapped_bdprob@cor_optimum_cost             = Xu_bdprob@cor_optimum_cost  #correct_solution_cost

      #---------------------------------------------------------------------------

    return (wrapped_bdprob)  #  end function - wrap_abundance_dist_around_Xu_problem
    }

#===============================================================================

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param bdprob_1 a BD_Prob to be combined
#' @param bdprob_2 a BD_Prob to be combined
#' @param parameters list of control parameters, generally from project.yaml
#' @param bdpg_error_codes list of error names and corresponding error codes
#' @param integerize function to use in converting floats to ints

combine_2_bdprobs = function (bdprob_1, bdprob_2,
                              parameters, bdpg_error_codes, integerize)
    {
    stop ("\n\ncombine_2_bdprobs() is NOT IMPLEMENTED yet.\n\n")
    }

#===============================================================================

#' Generate a new biodiversity problem by modifying or combining existing
#' problem(s)
#'
#'  Sometimes it may be useful to combine 2 or more bd problems into
#'  one bigger problem since it may have the potential to produce a
#'  more difficult compound problem.
#'  If nothing else, it bears some resemblance to having a larger
#'  landscape with subregion characteristics.
#'
#'  This function is intended to allow either wrapping a distribution around
#'  an existing problem or combining two problems.  However, at the moment,
#'  it only allows wrapping.  Combining two problems is pretty straightforward
#'  but has not been implemented yet and probably won't be implemented unless
#'  there is a demand for it.
#'
#'  Dummy code that was in here to demonstrate high-level parts of that
#'  combination (i.e., combine_2_bdprobs()) have been removed but can be
#'  found in github versions of the code up until around commit 1c0fbba6
#'  on Feb 4, 2017.
#'
#' @param parameters named list of all parameters, generally from project.yaml file
#' @param wrap_lognormal_dist_around_Xu boolean indicating whether to wrap a
#'     lognormal distribution around a base Xu problem; TRUE implies wrapping
#'     should be done; FALSE implies not
#' @param read_Xu_problem_from_Xu_file boolean indicating whether to read a
#'     Xu problem from one of Xu's benchmark problem files; TRUE implies that
#'     the problem should be read from one of those files; FALSE implies that
#'     the problem should be generated from scratch
#' @param infile_name string containing the name of the Xu benchmark file to
#'     read a problem from if reading from a Xu benchmark file
#' @param given_correct_solution_cost boolean indicating whether the correct
#'     cost of the correct optimum solution is known; TRUE implies that it is
#'     known
#' @param max_allowed_num_spp maximum number of species allowed in generating
#'     a problem from scratch (particularly of use when trying to do smaller,
#'     faster tests in development)
#' @param bdpg_error_codes named list of error codes and their corresponding
#'     numeric return values
#' @param integerize function to use when converting floats to integers
#'
#' @return a multi-BD_Prob
#' @export

gen_multi_bdprob = function (parameters,
                             wrap_lognormal_dist_around_Xu,
                             read_Xu_problem_from_Xu_file,
                             infile_name,
                             given_correct_solution_cost,
                             max_allowed_num_spp,
                             bdpg_error_codes,
                             integerize)  #parameters, bdpg_error_codes, integerize)
    {
        #----------------------------------------------------------------------
        #  Make sure that the base problem for the multiproblem is not one of
        #  Xu's benchmark problems read in from a file, since they do not
        #  contain the correct solution set.  They only contain the correct
        #  solution cost.
        #----------------------------------------------------------------------

    if (wrap_lognormal_dist_around_Xu &   #(parameters$wrap_lognormal_around_Xu &
        read_Xu_problem_from_Xu_file)   # parameters$read_Xu_problem_from_Xu_file)
        {
        stop (paste0 ("\n\nParameters wrap_lognormal_dist_around_Xu and ",
                      "read_Xu_problem_from_Xu_file ",
                      "\nare both true.",
                      "\nCannot wrap around Xu problem read from file ",
                      "because dependent node IDs ",
                      "\nare never given with the file.",
                      "\nQuitting.\n\n")
              )
        }

        #--------------------------------------------------------------------
        #  Base problem is not a Xu benchmark read from a file, so go ahead
        #  and generate the problem.
        #--------------------------------------------------------------------

    cat ("\n\n>>>>>>>>>>>>>>>>>>>>>>  ABOUT TO base Xu problem for multi-problem  <<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n")
    bdprob_1 = gen_single_bdprob_COR (parameters,
                                  read_Xu_problem_from_Xu_file,
                                  infile_name,
                                  given_correct_solution_cost,
                                  max_allowed_num_spp,
                                  bdpg_error_codes,
                                  integerize)

    if (! bdprob_1@prob_is_ok)
        {
        stop ("\n\nGenerating base BD_Problem for multi-problem failed.\n\n")

                #--------------------------------------------------------------
        } else  #  Base problem generation worked, so build multiproblem now.
                #--------------------------------------------------------------
        {
        if (wrap_lognormal_dist_around_Xu)  #parameters$wrap_lognormal_around_Xu)
            {
                #---------------------------------
                #  Control parameters from user.
                #---------------------------------

            desired_Xu_spp_frac_of_all_spp  = parameters$desired_Xu_spp_frac_of_all_spp
            solution_frac_of_landscape      = parameters$solution_frac_of_landscape
            desired_max_abundance_frac      = parameters$desired_max_abundance_frac
            dep_set_PUs_eligible            = parameters$dep_set_PUs_eligible
            add_one_to_lognormal_abundances = parameters$add_one_to_lognormal_abundances
            seed_value_for_search           = parameters$seed_value_for_search
            max_search_iterations           = parameters$max_search_iterations

                #-----------------------
                #  Derived parameters.
                #-----------------------

            tot_num_PUs_in_landscape = round (get_num_nodes (bdprob_1@nodes) /
                                              solution_frac_of_landscape)
            search_outfile_name      = paste0 (parameters$fullOutputDirWithSlash,
                                               "outfile.csv")

                #-----------------------------------------------------------
                #  Search for a set of lognormal parameters that fit the
                #  user's constraints while including the base Xu problem.
                #-----------------------------------------------------------

            rounded_abundances =
                find_lognormal_to_wrap_around_Xu (bdprob_1,
                                                  parameters,
                                                  desired_Xu_spp_frac_of_all_spp,
                                                  solution_frac_of_landscape,
                                                  desired_max_abundance_frac,
                                                  seed_value_for_search,
                                                  max_search_iterations,
                                                  add_one_to_lognormal_abundances,
                                                  search_outfile_name)


                #--------------------------------------------------------------
                #  Wrap the generated lognormal around the Xu base problem.
                #
                #  Note that the wrap_abundance_dist_around_Xu_problem()
                #  function doesn't care where you got the abundances, i.e.,
                #  they don't have to have come from the lognormal generator.
                #
                #  It can be any abundance set that you want, as long as it
                #  contains at least as many species sitting on exactly
                #  2 patches as the base Xu problem has.
                #
                #  It can have more species that sit on exactly 2 patches
                #  than the base Xu problem, but not less.
                #--------------------------------------------------------------

            combined_bdprob =
                wrap_abundance_dist_around_Xu_problem (rounded_abundances,
                                                       bdprob_1,
                                                       dep_set_PUs_eligible,
                                                       tot_num_PUs_in_landscape)

            }
        }

    return (combined_bdprob)
    }

#===============================================================================


