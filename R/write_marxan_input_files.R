#===============================================================================

                #  source ('write_marxan_input_files.R')

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
#' write_marxan_spec.dat_input_file (PU_IDs, cost_values, status_values)
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
#' write_marxan_spec.dat_input_file (spp_IDs, spf_values, target_values)
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
#' other columns.  See source code for the function 
#' \code{\link{gen_random_spp_PU_amount_table}} for an example of creating 
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
#' other columns.  See source code for the function 
#' \code{\link{gen_random_spp_PU_amount_table}} for an example of creating 
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

write_all_marxan_input_files = function (PU_IDs, spp_IDs, spp_PU_amount_table, 
                                         spf_const = 1, 
                                         target_const = 1, 
                                         cost_const = 1, 
                                         status_const = 0)
    {
    write_marxan_pu.dat_input_file (PU_IDs, cost_const, status_const)
    write_marxan_spec.dat_input_file (spp_IDs, spf_const, target_const)    
    write_marxan_puvspr.dat_input_file (spp_PU_amount_table)
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
#' other columns.  See source code for the function 
#' \code{\link{gen_random_spp_PU_amount_table}} for an example of creating 
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

write_all_marxan_input_files_from_vectors = 
    function (PU_IDs, spp_IDs, spp_PU_amount_table, 
              spf_values, target_values, 
              cost_values, status_values)
    {
    write_marxan_pu.dat_input_file_from_vectors (PU_IDs, cost_values, status_values)
    write_marxan_spec.dat_input_file_from_vectors (spp_IDs, spf_values, target_values)    
    write_marxan_puvspr.dat_input_file (spp_PU_amount_table)
    }

#===============================================================================
#                              Testing
#===============================================================================

# choose_random_spp_for_patch = function (num_spp)
#     {
#     num_spp_for_patch = sample (1:num_spp, 1)
#     spp_for_patch = sample (1:num_spp, num_spp_for_patch, replace=FALSE)
#
#     return (spp_for_patch)
#     }

#-------------------------------------------------------------------------------

#' Generate a random species vs. planning unit table.
#'
#' @param num_PUs An integer number of planning units to generate.
#' @param num_spp An integer number of species to generate.
#' @param sppAmount An integer amount of each species to generate on each patch.
#' @export
#' @return A data frame with column headings "species", "pu", and "amount".
#' @examples
#' \dontrun{
#' num_PUs = 10
#' num_spp = 10
#' gen_random_spp_PU_amount_table (num_PUs, num_spp)
#'          }

gen_random_spp_PU_amount_table =
        function (num_PUs, num_spp,
                  sppAmount = 1  #  Use same amount for every species
                 )
    {
    num_spp_in_PU = sample (1:num_spp, num_PUs, replace=TRUE)
    total_num_spp_PU_pairs = sum (num_spp_in_PU)

    spp_PU_amount_table =
        data.frame (species = rep (NA, total_num_spp_PU_pairs),
                    pu      = rep (NA, total_num_spp_PU_pairs),
                    amount  = rep (sppAmount, total_num_spp_PU_pairs))

    cur_table_row = 0
    for (cur_PU in 1:num_PUs)
        {
        spp_for_cur_PU = sample (1:num_spp,
                                 num_spp_in_PU [cur_PU],
                                 replace=FALSE)

        for (cur_spp in spp_for_cur_PU)
            {
            cur_table_row = cur_table_row + 1

            spp_PU_amount_table [cur_table_row, 1] = cur_spp
            spp_PU_amount_table [cur_table_row, 2] = cur_PU
            }
        }

    return (spp_PU_amount_table)
    }

#===============================================================================

test_write_marxan_input_files <- function ()
    {
    seed = 1
    set.seed (seed)

    num_PUs = 100
    num_spp = 3

    PU_IDs = 1:num_PUs
    spp_IDs = 1:num_spp

    spp_PU_amount_table = gen_random_spp_PU_amount_table (num_PUs, num_spp)
    write_all_marxan_input_files (PU_IDs, spp_IDs, spp_PU_amount_table)
    }

#===============================================================================

