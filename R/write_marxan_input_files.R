#===============================================================================

                #  source ('write_marxan_input_files.R')

#  The functions in this file write the 3 input files required by marxan.
#
#  There are 2 forms provided for the functions, one that requires full
#  vectors of values for inputs and the other that defaults some of those
#  vectors to a replicated constant value, e.g., a universal species target
#  of 2.
#
#  The names of the functions that take the full vectors all end in
#  "_from_vectors", e.g., write_marxan_pu.dat_input_file_from_vectors()
#  rather than write_marxan_pu.dat_input_file().

#===============================================================================

#' Write marxan planning units input file (pu.dat)  given vectors of cost and status values.
#'
#' @param PU_IDs A vector of planning unit IDs
#' @param cost_values A vector of costs
#' @param status_values A vector of status values
#' @export
#' @return nothing.
#' @examples
#' \dontrun{
#' PU_IDs = 1:5
#' cost_values = c(2,3,8,1,4)
#' status_values = c(0,0,0,0,0)
#' write_marxan_spec.dat_input_file_from_vectors (PU_IDs, cost_values, status_values)
#'          }

write_marxan_pu.dat_input_file_from_vectors = function (PU_IDs,
                                                         cost_values,
                                                         status_values)
    {
    num_PUs = length (PU_IDs)
    pu_cost_status_table = data.frame (id = PU_IDs,
                                       cost = cost_values,
                                       status = status_values)

    write.table (pu_cost_status_table,
                 file="./pu.dat",
                 sep=",",
                 quote=FALSE,
                 row.names=FALSE)
########################################################################################
#  TEMPORARY:  Echo information about data structures of all currently active variables.
cat("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
print (sys.call())
v=ls();
sapply (v,function(x){cat ("\n", x, "\n", sep='');str(get(x),vec.len=1,max.level=1)});
cat("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
########################################################################################

    }

#-------------------------------------------------------------------------------

#' Write marxan planning units input file (pu.dat) using constant values for cost and status.
#'
#' @param PU_IDs A vector of planning unit IDs
#' @param cost_const A constant cost to be applied to all species
#' @param status_const A constant status to be applied to all species
#' @export
#' @return nothing.
#' @examples
#' \dontrun{
#' PU_IDs = c(3,5,6,7,21,32)
#' write_marxan_pu.dat_input_file (PU_IDs)
#'
#' PU_IDs = c(3,5,6,7,21,32)
#' write_marxan_pu.dat_input_file (PU_IDs, 3, 0)
#'          }

write_marxan_pu.dat_input_file = function (PU_IDs,
                                           cost_const = 1,
                                           status_const = 0)
    {
    num_PUs = length (PU_IDs)

    cost_values = rep (1, num_PUs)
    status_values = rep (0, num_PUs)

    write_marxan_pu.dat_input_file_from_vectors (PU_IDs,
                                                 cost_values,
                                                 status_values)
########################################################################################
#  TEMPORARY:  Echo information about data structures of all currently active variables.
cat("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
print (sys.call())
v=ls();
sapply (v,function(x){cat ("\n", x, "\n", sep='');str(get(x),vec.len=1,max.level=1)});
cat("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
########################################################################################

    }

#===============================================================================

#' Write species input file (spec.dat) given vectors of target and spf values.
#'
#' @param spp_IDs A vector of species IDs
#' @param spf_values A vector of species penalty factors
#' @param target_values A vector of target values
#' @export
#' @return nothing.
#' @examples
#' \dontrun{
#' spp_IDs = 1:5
#' spf_values = c(2,3,8,1,4)
#' target_values = c(20,20,10,100,5)
#' write_marxan_spec.dat_input_file_from_vectors (spp_IDs, spf_values, target_values)
#'          }

write_marxan_spec.dat_input_file_from_vectors =
                            function (spp_IDs, spf_values, target_values)
    {
    spp_target_spf_table = data.frame (id = spp_IDs,
                                       target = target_values,
                                       spf = spf_values)

    write.table (spp_target_spf_table,
                 file="./spec.dat",
                 sep=",",
                 quote=FALSE,
                 row.names=FALSE)
########################################################################################
#  TEMPORARY:  Echo information about data structures of all currently active variables.
cat("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
print (sys.call())
v=ls();
sapply (v,function(x){cat ("\n", x, "\n", sep='');str(get(x),vec.len=1,max.level=1)});
cat("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
########################################################################################

    }

#-------------------------------------------------------------------------------

#' Write species input file (spec.dat) using constant values for species penalties and targets.
#'
#' @param spp_IDs A vector of species IDs
#' @param spf_const A constant species penalty factor to be applied to all species
#' @param target_const A constant target representation level to be applied to all species
#' @export
#' @return nothing.
#' @examples
#' \dontrun{
#' spp_IDs = 1:10
#' write_marxan_spec.dat_input_file (spp_IDs)
#' #'
#' spp_IDs = 1:10
#' write_marxan_spec.dat_input_file (spp_IDs, 10, 1)
#'          }

write_marxan_spec.dat_input_file = function (spp_IDs,
                                             spf_const = 1,
                                             target_const = 1)
    {
    num_spp = length (spp_IDs)

    spf_values = rep (spf_const, num_spp)
    target_values = rep (target_const, num_spp)

    write_marxan_spec.dat_input_file_from_vectors (spp_IDs,
                                                   spf_values,
                                                   target_values)
########################################################################################
#  TEMPORARY:  Echo information about data structures of all currently active variables.
cat("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
print (sys.call())
v=ls();
sapply (v,function(x){cat ("\n", x, "\n", sep='');str(get(x),vec.len=1,max.level=1)});
cat("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
########################################################################################

    }

#===============================================================================

#' Write planning unit vs. species input file (puvspr.dat).
#'
#' @param spp_PU_amount_table A data frame of species IDs vs planning unit IDs
#' @export
#' @return nothing.
#' @details
#' The spp_PU_amount_table is expected to have 3 columns called (in left to
#' right order) "species",
#' "pu", and "amount".  Marxan requires this table to be sorted in increasing
#' order on the planning unit column.  I don't think the order within planning
#' unit matters on the
#' other columns.  See source code for the test function
#' \code{gen_random_spp_PU_amount_table} for an example of creating
#' this table.
#' @examples
#' \dontrun{
#' num_PUs = 100
#' num_spp = 3
#' spp_PU_amount_table = gen_random_spp_PU_amount_table (num_PUs, num_spp)
#' write_marxan_puvspr.dat_input_file (spp_PU_amount_table)
#'          }

write_marxan_puvspr.dat_input_file = function (spp_PU_amount_table)
    {
    write.table (spp_PU_amount_table,
                 file="./puvspr.dat",
                 sep=",",
                 quote=FALSE,
                 row.names=FALSE)
########################################################################################
#  TEMPORARY:  Echo information about data structures of all currently active variables.
cat("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
print (sys.call())
v=ls();
sapply (v,function(x){cat ("\n", x, "\n", sep='');str(get(x),vec.len=1,max.level=1)});
cat("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
########################################################################################

    }

#===============================================================================

#' Write all marxan input files except bound.dat and input.dat using constant values for individual elements.
#'
#' @param PU_IDs A vector of planning unit IDs
#' @param spp_IDs A vector of species IDs
#' @param spp_PU_amount_table A data frame of species IDs vs planning unit IDs
#' @param spf_const A constant species penalty factor to be applied to all species
#' @param target_const A constant target representation level to be applied to all species
#' @param cost_const A constant cost to be applied to all species
#' @param status_const A constant status to be applied to all species
#' @export
#' @return nothing.
#' @details
#' The spp_PU_amount_table is expected to have 3 columns called (in left to
#' right order) "species",
#' "pu", and "amount".  Marxan requires this table to be sorted in increasing
#' order on the planning unit column.  I don't think the order within planning
#' unit matters on the
#' other columns.  See source code for the test function
#' \code{gen_random_spp_PU_amount_table} for an example of creating
#' this table.
#' @examples
#' \dontrun{
#' num_PUs = 100
#' num_spp = 3
#' PU_IDs = 1:num_PUs
#' spp_IDs = 1:num_spp
#'
#' spp_PU_amount_table = gen_random_spp_PU_amount_table (num_PUs, num_spp)
#'
#'     #  Write all input files at once.
#' write_all_marxan_input_files (PU_IDs, spp_IDs, spp_PU_amount_table)
#'
#'     #  Write each input file individually.
#' write_marxan_pu.dat_input_file (PU_IDs)
#' write_marxan_spec.dat_input_file (spp_IDs)
#' write_marxan_puvspr.dat_input_file (spp_PU_amount_table)
#'          }

write_all_marxan_input_files = function (PU_IDs,
                                         spp_IDs,
                                         spp_PU_amount_table,

                                         targets = rep (1, length (spp_IDs)),
                                         costs = rep (1, length (PU_IDs)),

                                         spf_const = 1,
                                         status_const = 0)
    {
    write_marxan_pu.dat_input_file_from_vectors (PU_IDs, costs, status_const)
    write_marxan_spec.dat_input_file_from_vectors (spp_IDs, spf_const, targets)
    write_marxan_puvspr.dat_input_file (spp_PU_amount_table)
########################################################################################
#  TEMPORARY:  Echo information about data structures of all currently active variables.
cat("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
print (sys.call())
v=ls();
sapply (v,function(x){cat ("\n", x, "\n", sep='');str(get(x),vec.len=1,max.level=1)});
cat("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
########################################################################################

    }

#-------------------------------------------------------------------------------

#' Write all marxan input files except bound.dat and input.dat given vectors for individual elements.
#'
#' @param PU_IDs A vector of planning unit IDs
#' @param spp_IDs A vector of species IDs
#' @param spp_PU_amount_table A data frame of species IDs vs planning unit IDs
#' @param spf_values A vector of species penalty factors
#' @param target_values A vector of target values
#' @param cost_values A vector of costs
#' @param status_values A vector of status values
#' @export
#' @return nothing.
#' @details
#' The spp_PU_amount_table is expected to have 3 columns called (in left to
#' right order) "species",
#' "pu", and "amount".  Marxan requires this table to be sorted in increasing
#' order on the planning unit column.  I don't think the order within planning
#' unit matters on the
#' other columns.  See source code for the test function
#' \code{gen_random_spp_PU_amount_table} for an example of creating
#' this table.
#' @examples
#' \dontrun{
#' num_PUs = 100
#' num_spp = 3
#' PU_IDs = 1:num_PUs
#' spp_IDs = 1:num_spp
#' spf_values = rep (10, num_spp)
#' target_values = rep (1, num_spp)
#' cost_values = rep (1000, num_PUs)
#' status_values = rep (0, num_PUs)
#'
#' spp_PU_amount_table = gen_random_spp_PU_amount_table (num_PUs, num_spp)
#'
#'     #  Write all input files at once.
#' write_all_marxan_input_files_from_vectors (PU_IDs, spp_IDs, spp_PU_amount_table,
#'                                            spf_values, target_values,
#'                                            cost_values, status_values)
#'
#'     #  Write each input file individually.
#' write_marxan_pu.dat_input_file_from_vectors (PU_IDs, cost_values, status_values)
#' write_marxan_spec.dat_input_file_from_vectors (spp_IDs, spf_values, target_values)
#' write_marxan_puvspr.dat_input_file (spp_PU_amount_table)
#'          }

write_all_marxan_input_files_from_vectors = function (PU_IDs,
                                                      spp_IDs,
                                                      spp_PU_amount_table,
                                                      spf_values,
                                                      target_values,
                                                      cost_values,
                                                      status_values
                                                     )
    {
    write_marxan_pu.dat_input_file_from_vectors (PU_IDs, cost_values, status_values)
    write_marxan_spec.dat_input_file_from_vectors (spp_IDs, spf_values, target_values)
    write_marxan_puvspr.dat_input_file (spp_PU_amount_table)
########################################################################################
#  TEMPORARY:  Echo information about data structures of all currently active variables.
cat("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
print (sys.call())
v=ls();
sapply (v,function(x){cat ("\n", x, "\n", sep='');str(get(x),vec.len=1,max.level=1)});
cat("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
########################################################################################

    }

#===============================================================================

