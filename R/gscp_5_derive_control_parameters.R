#===============================================================================

                        #  gscp_5_derive_control_parameters.R

#===============================================================================

derive_Xu_control_parameters = function (parameters, 
                                         emulatingTzar, 
                                         bdpg_error_codes, 
                                         integerize
                                         ) 
    {
        #  NOTE:  The runif() documentation says:
        #           "runif will not generate either of the extreme values unless 
        #            max = min or max-min is small compared to min, and in 
        #            particular not for the default arguments."
    
    n__num_groups = parameters$n__num_groups    
    if (parameters$use_unif_rand_n__num_groups)
        {
        n__num_groups = 
        integerize (runif (1, 
                         min = parameters$n__num_groups_lower_bound,
                         max = parameters$n__num_groups_upper_bound
                         ))
        }
    
    alpha__ = parameters$alpha__
    if (parameters$derive_alpha_from_n__num_groups_and_opt_frac_0.5)
        {
            #  BTL - 2015 04 08
            #  This is a special case to summarize the conditions for the 
            #  main large set of experiments for the biodivprobgen paper.
            #  In this case, there will be 1 dependent node and 1 independent 
            #  node for each group, so the optimal solution will use 50% of 
            #  the total number of nodes (i.e., planning units).  
            #  I also want to randomly choose from a range of n__num_groups, 
            #  which will in turn dictate the number of planning units to be 
            #  twice the number of groups since there are 2 planning units 
            #  per group in this case.
            #  Since alpha, n, and num_nodes_per_group are all interlinked, 
            #  choosing 2 of them will force the value of the third.  
            #  In this case, I will choose num_nodes_per_group and n__num_groups, 
            #  so the alpha value will be forced.
        
        num_nodes_per_group = 2
        desired_num_PUs = num_nodes_per_group * n__num_groups
        alpha__ = log (desired_num_PUs, n__num_groups) - 1
        
        } else
        {
        if (parameters$use_unif_rand_alpha__)
            {
            alpha__ = runif (1, 
                           min = parameters$alpha___lower_bound,
                           max = parameters$alpha___upper_bound
                           )
            }
        }
    
    p__prop_of_links_between_groups = parameters$p__prop_of_links_between_groups
    if (parameters$use_unif_rand_p__prop_of_links_between_groups)
        {
        p__prop_of_links_between_groups = 
        runif (1, 
             min = parameters$p__prop_of_links_between_groups_lower_bound,
             max = parameters$p__prop_of_links_between_groups_upper_bound
             )
        }
    
    r__density = parameters$r__density
    if (parameters$use_unif_rand_r__density)
        {
            #  BTL - 2015 03 19
            #  Not sure why these bounds on r__density said p__r__... instead 
            #  of just r__...
            #  So, replacing the p__r__... in gscp_3..., gscp_5..., and in 
            #  project.yaml.
        
        r__density = runif (1, 
                #                   min = parameters$p__r__density_lower_bound,
                              min = parameters$r__density_lower_bound,
                #                   max = parameters$p__r__density_upper_bound
                              max = parameters$r__density_upper_bound
                              )
        }
    
    #--------------------
    
        #  2014 12 11 - BTL - Adding to the original set of parameters
        #  Originally, there was only 1 independent node per group.  
        #  I'm going to try allowing more than that to see if it will still 
        #  build hard problems but allow the size of the solution set to drop 
        #  below 50% of the node set.
    
    num_independent_nodes_per_group = 1
    
    #-------------------------------------------------------------------------------
    
        #  Derived control parameters.
    
    cat ("\n\n--------------------  Building derived control parameters.\n")
    
        #  Originally, this parameter was set assuming that there was only 
        #  1 independent node per group.  
        #  Now allowing there to be more than 1, so need to add to the 
        #  number of nodes per group.  However, we only want to add the 
        #  the excess independent nodes that are beyond the original 1, 
        #  so we have to subtract 1 from the number we're adding on.
        
        #    num_nodes_per_group = integerize (n__num_groups ^ alpha__)
    num_nodes_per_group = integerize (n__num_groups ^ alpha__) - 
                                        (num_independent_nodes_per_group - 1)
    
        #    num_independent_set_nodes = n__num_groups
    num_independent_set_nodes = n__num_groups * num_independent_nodes_per_group
    
    tot_num_nodes = n__num_groups * num_nodes_per_group
    
    num_dependent_set_nodes = tot_num_nodes - num_independent_set_nodes
    opt_solution_as_frac_of_tot_num_nodes = 
                            num_dependent_set_nodes / tot_num_nodes
    
    num_rounds_of_linking_between_groups = 
            integerize (r__density * n__num_groups * log (n__num_groups))
    
        #  Is this right?  Even in the old version?  
        #  i.e., this count would allow links to ind. nodes too.
        #  Should it be "* num_dependent_nodes_per_group" instead of 
        #  "* num_nodes_per_group"?
        #  How is it defined in Xu?
        #  The interlinking code DOES make a check though and only allows 
        #  linking to dependent nodes.  
        #  The only thing that the code here seems like it would do is to 
        #  overallocate space.
        #  One weird thing though is that if you only have one dependent node, 
        #  how is a proportion of 0.2x going to cause anything at all to be 
        #  interlinked - unless, the integerize function is always giving a 
        #  value of at least 1?  But integerize() is currently round() and 
        #  with 2 nodes per group (to get  things down to 50% as the solution 
        #  fraction), anything less than p=0.5 should yield no interlinking?
        #  Actually, with rounding and 2 nodes per group, anything >= 0.25 will 
        #  yield at least one interlink.  So, should I just leave this alone?  
        #  Still, it's going to get very weird (and blow up?) if you have 
        #  4 or 5 independent nodes in a group and only 1 dependent node 
        #  because this is going to tell you that you have to have something 
        #  like p=0.3 times 5 or 6 instead of times 2.  That would lead to 
        #  many duplicate links, but does that really matter?  Seems like it 
        #  would in a predictive sense, i.e., the value assigned to p would 
        #  not have the same meaning in these lower bound saturating kinds of 
        #  circumstances compared to when there larger values that it could 
        #  take a real proportion of.  Even in the old version, there will be 
        #  an odd threshold effect in what p means, e.g., when it falls below 
        #  0.25 in the example above.  Still, isn't that always going to be the 
        #  case because the theory uses continuous values but the problem sizes 
        #  have to be integers and you will always have to map from continuous 
        #  to integer?
        #  Maybe the best solution here is to create an option that allows you 
        #  to choose the behavior you want and records that in the output.
        #  What would be the possible variants of this option?
        #  Compute target... from:
        #       a) num_nodes_per_group
        #       b) num_dependent_nodes_per_group
        #       c) at least 1, 
        #               i.e., max (1, [a or b above]) so that you always 
        #               get at least 1
        #  So, option c) would mean that you need two options instead of 1, 
        #  i.e., [a) or b)] and [max or actual value].  
        #  Another thing that should probably be an option is the choice of the 
        #  integerize function, since that also affects this.
    
    base_for_target_num_links_between_2_groups_per_round = 
            parameters$base_for_target_num_links_between_2_groups_per_round
    
        #  BTL - 2015 04 08
        #  Is this variable still used somewhere?
        #  Can't find it appearing when I grep all of the R files right now...
    at_least_1_for_target_num_links_between_2_groups_per_round = 
        parameters$at_least_1_for_target_num_links_between_2_groups_per_round
    
    target_num_links_between_2_groups_per_round = 
        integerize (p__prop_of_links_between_groups * num_nodes_per_group)  
    
    
    
    #  Compute how many links there will be within each group.  
    #  If there is more than one independent node, then not all possible 
    #  combinations of links will be made.  Have to subtract off 
    #  the number of possible links between independent nodes in 
    #  the group.
    #    num_links_within_one_group = choose (num_nodes_per_group, 2)
    num_links_within_one_group = choose (num_nodes_per_group, 2) - 
                                 choose (num_independent_nodes_per_group, 2)
    
    tot_num_links_inside_groups = n__num_groups * num_links_within_one_group
    
    max_possible_num_links_between_groups = 
                integerize (target_num_links_between_2_groups_per_round * 
                            num_rounds_of_linking_between_groups)
    
    max_possible_tot_num_links = 
                integerize (tot_num_links_inside_groups + 
                            max_possible_num_links_between_groups)
    max_possible_tot_num_node_link_pairs = 2 * max_possible_tot_num_links
    
    cat ("\n\nInput variable settings")
    cat ("\n\t\t n__num_groups = ", n__num_groups)
    cat ("\n\t\t alpha__ = ", alpha__)
    cat ("\n\t\t p__prop_of_links_between_groups = ", p__prop_of_links_between_groups)
    cat ("\n\t\t r__density = ", r__density)

    cat ("\n\nDerived variable settings")
    cat ("\n\t\t num_nodes_per_group = ", num_nodes_per_group)
    cat ("\n\t\t num_rounds_of_linking_between_groups = ", num_rounds_of_linking_between_groups)
    cat ("\n\t\t target_num_links_between_2_groups_per_round = ", target_num_links_between_2_groups_per_round)
    cat ("\n\t\t num_links_within_one_group = ", num_links_within_one_group)
    cat ("\n\t\t tot_num_links_inside_groups = ", tot_num_links_inside_groups)
    cat ("\n\t\t max_possible_num_links_between_groups = ", max_possible_num_links_between_groups)
    cat ("\n\t\t max_possible_tot_num_links = ", max_possible_tot_num_links)
    cat ("\n\t\t max_possible_tot_num_node_link_pairs = ", max_possible_tot_num_node_link_pairs)
    cat ("\n\n")
    
    #===============================================================================
    
        #  Create and load a structure holding all of the derived parameters 
        #  so that they can be returned to the caller.
    
    # derived_Xu_params = list()
    # 
    # derived_Xu_params$num_nodes_per_group                                        = num_nodes_per_group
    # derived_Xu_params$num_rounds_of_linking_between_groups                       = num_rounds_of_linking_between_groups
    # derived_Xu_params$target_num_links_between_2_groups_per_round                = target_num_links_between_2_groups_per_round
    # derived_Xu_params$num_links_within_one_group                                 = num_links_within_one_group
    # derived_Xu_params$tot_num_links_inside_groups                                = tot_num_links_inside_groups
    # derived_Xu_params$max_possible_num_links_between_groups                      = max_possible_num_links_between_groups
    # derived_Xu_params$max_possible_tot_num_links                                 = max_possible_tot_num_links
    # derived_Xu_params$max_possible_tot_num_node_link_pairs                       = max_possible_tot_num_node_link_pairs
    # 
    # derived_Xu_params$n__num_groups                                              = n__num_groups
    # derived_Xu_params$alpha__                                                    = alpha__
    # derived_Xu_params$p__prop_of_links_between_groups                            = p__prop_of_links_between_groups
    # derived_Xu_params$r__density                                                 = r__density
    # derived_Xu_params$num_independent_nodes_per_group                            = num_independent_nodes_per_group
    # derived_Xu_params$num_independent_set_nodes                                  = num_independent_set_nodes
    # derived_Xu_params$tot_num_nodes                                              = tot_num_nodes
    # derived_Xu_params$num_dependent_set_nodes                                    = num_dependent_set_nodes
    # derived_Xu_params$opt_solution_as_frac_of_tot_num_nodes                      = opt_solution_as_frac_of_tot_num_nodes
    # derived_Xu_params$base_for_target_num_links_between_2_groups_per_round       = base_for_target_num_links_between_2_groups_per_round
    # derived_Xu_params$at_least_1_for_target_num_links_between_2_groups_per_round = at_least_1_for_target_num_links_between_2_groups_per_round

    #===============================================================================
    
        #  "Too small" problem
        #
        #  Running under tzar, this code crashes when it tries to allocate 
        #  arrays of size 0 and tzar marks that crash by renaming the output 
        #  directory to end in ".failed".  This is good enough for what I"m 
        #  doing because it tells me to skip over the directory in compiling 
        #  results and no harm is done.  I could imagine doing something fancier 
        #  and trying to choose a new set of parameters, but how to choose them 
        #  would be different for different situations (e.g., when they're being 
        #  generated randomly vs. single experiments where one specific 
        #  configuration is being tested).  Also, changing the input parameters 
        #  would have to mean also modifying the input yaml file if the yaml 
        #  was to be a reproducible and correct reflection of the parameters 
        #  used to do the experiment.  So, I think that what I'll do instead 
        #  is just make the failure be slightly more graceful by checking for 
        #  0 array sizes and giving an error message about it to the log file 
        #  before quitting.  
        #
        #  The one other issue here is that I could try to write out the various 
        #  results data frames with some kind of indicator that the run failed 
        #  so that they could still be read when all good results data frames 
        #  are collected.  That might be useful in providing information about 
        #  what parameter ranges end up being out of bounds or were not sampled 
        #  when trying to learn prediction functions.  However, it would end up 
        #  requiring more specialized code in the collection routines to deal 
        #  with failed runs instead of just being able to assume all went well.  
        #  At this point, simpler code seems like a bigger benefit than the 
        #  small amount of added and seldom-used information about failures, so 
        #  I'm going with the simpler solution here for now.  
    
    if ((num_links_within_one_group < 1) | (tot_num_links_inside_groups < 1))
        {
        cat ("\n\nFailing:  num_links_within_one_group (", 
             num_links_within_one_group, 
             ") < 1  OR  tot_num_links_inside_groups (", 
             tot_num_links_inside_groups, 
             ") < 1.\n\n")
        
        if (emulatingTzar)  browser ()
        
        quit (status=bdpg_error_codes$ERROR_STATUS_num_inside_or_within_group_links_less_than_one, 
              save="no")
        }
    
    #-------------------------------------------------------------------------------
    
        #  "Too big" problem
        #
        #  No longer handled here.
        #  Now handled in generate_set_cover_problem.R
    
    #-------------------------------------------------------------------------------
    
    Xu_base_parameters <- new ("Xu_base_params", 
                               n__num_groups = n__num_groups, 
                               alpha__ = alpha__, 
                               p__prop_of_links_between_groups = p__prop_of_links_between_groups, 
                               r__density = r__density
                               )
    
    Xu_derived_parameters <- new ("Xu_derived_params", 
                                    num_nodes_per_group                                        = num_nodes_per_group, 
                                    num_rounds_of_linking_between_groups                       = num_rounds_of_linking_between_groups, 
                                    target_num_links_between_2_groups_per_round                = target_num_links_between_2_groups_per_round, 
                                    num_links_within_one_group                                 = num_links_within_one_group, 
                                    tot_num_links_inside_groups                                = tot_num_links_inside_groups, 
                                    max_possible_num_links_between_groups                      = max_possible_num_links_between_groups, 
                                    max_possible_tot_num_links                                 = max_possible_tot_num_links, 
                                    max_possible_tot_num_node_link_pairs                       = max_possible_tot_num_node_link_pairs, 
                                    
#                                    num_independent_nodes_per_group                            = num_independent_nodes_per_group, 
                                    num_independent_set_nodes                                  = num_independent_set_nodes, 
                                    tot_num_nodes                                              = tot_num_nodes, 
                                    num_dependent_set_nodes                                    = num_dependent_set_nodes, 
                                    opt_solution_as_frac_of_tot_num_nodes                      = opt_solution_as_frac_of_tot_num_nodes  #, 
#                                    base_for_target_num_links_between_2_groups_per_round       = base_for_target_num_links_between_2_groups_per_round, 
#                                    at_least_1_for_target_num_links_between_2_groups_per_round = at_least_1_for_target_num_links_between_2_groups_per_round
                                  )
    
#     Xu_bdpg_extended_parameters <- new ("Xu_bdpg_extended_params", 
#                                     alpha___lower_bound                                        = parameters$alpha___lower_bound, 
#                                     alpha___upper_bound                                        = parameters$alpha___upper_bound, 
#                                     derive_alpha_from_n__num_groups_and_opt_frac_0.5           = parameters$derive_alpha_from_n__num_groups_and_opt_frac_0.5, 
#                                     use_unif_rand_alpha__                                      = parameters$use_unif_rand_alpha__, 
#                                     
# #                                    n__num_groups                                              = n__num_groups, 
#                                     n__num_groups_lower_bound                                  = parameters$n__num_groups_lower_bound, 
#                                     n__num_groups_upper_bound                                  = parameters$n__num_groups_upper_bound, 
#                                     use_unif_rand_n__num_groups                                = parameters$use_unif_rand_n__num_groups, 
#                 
#                                     num_independent_nodes_per_group                            = num_independent_nodes_per_group, 
#                                                                 
#                                     use_unif_rand_p__prop_of_links_between_groups              = parameters$use_unif_rand_p__prop_of_links_between_groups, 
#                                     p__prop_of_links_between_groups_lower_bound                = parameters$p__prop_of_links_between_groups_lower_bound, 
#                                     p__prop_of_links_between_groups_upper_bound                = parameters$p__prop_of_links_between_groups_upper_bound, 
#                                     base_for_target_num_links_between_2_groups_per_round       = base_for_target_num_links_between_2_groups_per_round,  #  Correct type?
#                                     at_least_1_for_target_num_links_between_2_groups_per_round = at_least_1_for_target_num_links_between_2_groups_per_round,  #  Not used?  See comment in gscp_5...R.
#                                     
#                                     use_unif_rand_r__density                                   = parameters$use_unif_rand_r__density, 
#                                     r__density_lower_bound                                     = parameters$r__density_lower_bound, 
#                                     r__density_upper_bound                                     = parameters$r__density_upper_bound, 
#                                     
#                                     integerize                                                 = integerize 
#                             )
    
    Xu_bdpg_extended_parameters <- new ("Xu_bdpg_extended_params") 
    if (!is.null (parameters$xxx))  Xu_bdpg_extended_parameters@xxx = parameters$xxx
    if (!is.null (parameters$alpha___lower_bound))  Xu_bdpg_extended_parameters@alpha___lower_bound = parameters$alpha___lower_bound
    if (!is.null (parameters$alpha___upper_bound))  Xu_bdpg_extended_parameters@alpha___upper_bound = parameters$alpha___upper_bound
    if (!is.null (parameters$derive_alpha_from_n__num_groups_and_opt_frac_0.5))  Xu_bdpg_extended_parameters@derive_alpha_from_n__num_groups_and_opt_frac_0.5 = parameters$derive_alpha_from_n__num_groups_and_opt_frac_0.5
    if (!is.null (parameters$use_unif_rand_alpha__))  Xu_bdpg_extended_parameters@use_unif_rand_alpha__ = parameters$use_unif_rand_alpha__
    if (!is.null (parameters$n__num_groups_lower_bound))  Xu_bdpg_extended_parameters@n__num_groups_lower_bound = parameters$n__num_groups_lower_bound
    if (!is.null (parameters$n__num_groups_upper_bound))  Xu_bdpg_extended_parameters@n__num_groups_upper_bound = parameters$n__num_groups_upper_bound
    if (!is.null (parameters$use_unif_rand_n__num_groups))  Xu_bdpg_extended_parameters@use_unif_rand_n__num_groups = parameters$use_unif_rand_n__num_groups
    if (!is.null (parameters$num_independent_nodes_per_group))  Xu_bdpg_extended_parameters@num_independent_nodes_per_group = num_independent_nodes_per_group
    if (!is.null (parameters$use_unif_rand_p__prop_of_links_between_groups))  Xu_bdpg_extended_parameters@use_unif_rand_p__prop_of_links_between_groups = parameters$use_unif_rand_p__prop_of_links_between_groups
    if (!is.null (parameters$p__prop_of_links_between_groups_lower_bound))  Xu_bdpg_extended_parameters@p__prop_of_links_between_groups_lower_bound = parameters$p__prop_of_links_between_groups_lower_bound
    if (!is.null (parameters$p__prop_of_links_between_groups_upper_bound))  Xu_bdpg_extended_parameters@p__prop_of_links_between_groups_upper_bound = parameters$p__prop_of_links_between_groups_upper_bound
    if (!is.null (parameters$base_for_target_num_links_between_2_groups_per_round))  Xu_bdpg_extended_parameters@base_for_target_num_links_between_2_groups_per_round = parameters$base_for_target_num_links_between_2_groups_per_round
    if (!is.null (parameters$at_least_1_for_target_num_links_between_2_groups_per_round))  Xu_bdpg_extended_parameters@at_least_1_for_target_num_links_between_2_groups_per_round = parameters$at_least_1_for_target_num_links_between_2_groups_per_round
    if (!is.null (parameters$use_unif_rand_r__density))  Xu_bdpg_extended_parameters@use_unif_rand_r__density = parameters$use_unif_rand_r__density
    if (!is.null (parameters$r__density_lower_bound))  Xu_bdpg_extended_parameters@r__density_lower_bound = parameters$r__density_lower_bound
    if (!is.null (parameters$r__density_upper_bound))  Xu_bdpg_extended_parameters@r__density_upper_bound = parameters$r__density_upper_bound
    if (!is.null (parameters$integerize))  Xu_bdpg_extended_parameters@integerize = integerize

    Xu_parameters <- new ("Xu_params", 
                          base_params = Xu_base_parameters,
                          derived_params = Xu_derived_parameters,
                          bdpg_extended_params = Xu_bdpg_extended_parameters
                        )
    
    #-------------------------------------------------------------------------------
    
#    return (derived_Xu_params)
    return (Xu_parameters)
    }

#===============================================================================

