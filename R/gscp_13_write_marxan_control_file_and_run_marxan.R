#===============================================================================

            #  gscp_13_write_marxan_control_file_and_run_marxan.R

#===============================================================================

    #  TODO:

    #  NOTE:  Many of the entries below have to do with reading marxan output
    #         and loading it into this program and doing something with it.
    #         I should build functions for doing those things and add them to
    #         the marxan package.  Should talk to Ascelin about this too and
    #         see if there is any overlap with what he's doing.

#===============================================================================

    #  Build structures holding:
        #  Make a function to automatically do these subtotalling actions
        #  since I need to do it all the time.
            #  May want one version for doing these give a table or dataframe
            #  and another for doing them given a list or list of lists
            #  since those are the most common things I do (e.g, in
            #  distSppOverPatches()).
        #  Species richness for each patch.
        #  Number of patches for each spp.
        #  Correct solution.
        #  Things in dist spp over patches?
            #  Patch list for each species.
            #  Species list for each patch.
    #  Are some of these already built for the plotting code above?

    #  *** Maybe this should be done using sqlite instead of lists of lists and
    #  tables and data frames?


    #  http://stackoverflow.com/questions/1660124/how-to-group-columns-by-sum-in-r
    #  Is this counting up the number of species on each patch?
# x2  <-  by (spp_PU_amount_table$amount, spp_PU_amount_table$pu, sum)
# do.call(rbind,as.list(x2))
#
# cat ("\n\nx2 =\n")
# print (x2)

#===============================================================================

    #  Aside:  The mention of distSppOverPatches() above reminds me that I
    #           found something the other day saying that copulas were
    #           a way to generate distributions with specified marginal
    #           distributions.  I think that I saved a screen grab and
    #           named the image using copula in the title somehow...

#===============================================================================

#' Run marxan
#'
#' Run marxan program
#'
#-------------------------------------------------------------------------------

#' @param marxan_dir character string
#' @param marxan_executable_name character string
#'
#' @return Returns system return code from running marxan
#' @export

#-------------------------------------------------------------------------------

run_marxan <- function (marxan_dir, marxan_executable_name)
    {
#    marxan_dir = "/Users/bill/D/Marxan/"    #  replaced in yaml file

    original_dir = getwd()
    cat ("\n\noriginal_dir =", original_dir)

    cat ("\n\nImmediately before calling marxan, marxan_dir = ", marxan_dir)
    setwd (marxan_dir)

    cat("\n =====> The current wd is", getwd() )

        #  The -s deals with the problem of Marxan waiting for you to hit
        #  return at the end of the run when you're running in the background.
        #  Without it, the system() command never comes back.
        #       (From p. 24 of marxan.net tutorial:
        #        http://marxan.net/tutorial/Marxan_net_user_guide_rev2.1.pdf
        #        I'm not sure if it's even in the normal user's manual or
        #        best practices manual for marxan.)

        #  BTL - 2015 03 27
        #  Marxan mailing list recently pointed to some new marxan materials
        #  on github and one of them has an example of some R code that
        #  calls marxan with what looks like a specification of the location
        #  of the input.dat file.  So, it looks like I can just add the
        #  filespec of the input.dat file as an argument after the "-s"
        #  argument without having any kind of other dash option specifying
        #  that you're giving the path to the input.dat file.

    system.command.run.marxan = paste0 ("./", marxan_executable_name, " -s")
    cat( "\n\n>>>>>  The system command to run marxan will be:\n'",
         system.command.run.marxan, "'\n>>>>>\n\n", sep='')

    retval = system (system.command.run.marxan)    #  , wait=FALSE)
    cat ("\n\nmarxan retval = '", retval, "'.\n\n", sep='')

    setwd (original_dir)
    cat ("\n\nAfter setwd (original_dir), sitting in:", getwd(), "\n\n")

    return (retval)
    }

#===============================================================================

#' Set marxan controls and run marxan
#'
#' Set marxan controls and run marxan
#'
#-------------------------------------------------------------------------------

#' @param marxan_input_dir character string
#' @param marxan_output_dir character string
#' @param parameters list
#' @param marxan_IO_dir character string
#' @param rand_seed integer
#'
#' @return Returns list
#' @export

#-------------------------------------------------------------------------------

set_marxan_controls_and_run_marxan <- function (marxan_input_dir,
                                                marxan_output_dir,
                                                marxan_IO_dir,
                                                parameters,
                                                rand_seed
                                               )
    {
        #-----------------------------------------------------------------------
        #  Set the marxan executable name to default to the mac,
        #  but check for linux as well.
        #  I don't know the name for Windows, so I'll just
        #  let the system command crash on Windows for the moment since I'm
        #  not doing anything at all with Windows right now and
        #  can look that up later if necessary.
        #-----------------------------------------------------------------------
    marxan_executable_name = "MarOpt_v243_Mac64"
    current_os = get_current_os ()

    if (current_os == "linux-gnu")
        marxan_executable_name = "MarOpt_v243_Linux64"

        #-----------------------------------------------------------------------
        #  General Marxan Parameters
        #-----------------------------------------------------------------------

        #  Set default marxan values here, but allow for overrides below if
        #  the marxan_use_default_input_parameters flag is turned off in the
        #  parameters set.

    marxan_BLM = 1
    marxan_PROP  = 0.5

        #*******
        #  NOTE:  Random seed is set to -1 in the cplan input.dat.
        #           I think that means to use a different seed each time.
        #           I probably need to change this to any positive number,
        #           at least in the default input.dat that I'm using now
        #           so that I get reproducible results.
        #*******

    marxan_RANDSEED  = rand_seed    #parameters$seed    #  Default to same seed as the R code.
    marxan_NUMREPS  = 10

        #  Annealing Parameters
        #  It looks like Marxan chokes if input.dat has the number of
        #  iterations expressed in scientific notation (e.g., 1e+06).
        #  Somewhere along the path between here and writing the value
        #  out to the input.dat file, values around 1 million do get
        #  converted to scientific notation, so I'm putting them in
        #  quotes so that they are written out to marxan's liking.
    marxan_NUMITNS  = "1000000"
    marxan_STARTTEMP  = -1
    marxan_NUMTEMP  = 10000

        #  Cost Threshold
    marxan_COSTTHRESH   = "0.00000000000000E+0000"
    marxan_THRESHPEN1   = "1.40000000000000E+0001"
    marxan_THRESHPEN2   = "1.00000000000000E+0000"

        #  Input Files
    marxan_INPUTDIR  = marxan_input_dir    #  "input"
    marxan_PUNAME  = "pu.dat"
    marxan_SPECNAME  = "spec.dat"
    marxan_PUVSPRNAME  = "puvspr.dat"

        #  Save Files
    marxan_SCENNAME  = "output"
    marxan_SAVERUN  = 3
    marxan_SAVEBEST  = 3
    marxan_SAVESUMMARY  = 3
    marxan_SAVESCEN  = 3
    marxan_SAVETARGMET  = 3
    marxan_SAVESUMSOLN  = 3
    marxan_SAVEPENALTY  = 3
    marxan_SAVELOG  = 2
    marxan_OUTPUTDIR  = marxan_output_dir    #  "output"

        #  Program control

        #  From Marxan user's manual v. 1.8.10, pp. 25-6
        #  3.2.1.2.1 Run Options
        #  Variable – ‘RUNMODE’ Required: Yes
        #  Description: This is an essential variable that defines the method Marxan
        #  will use to locate good reserve solutions . As discussed in the introduction,
        #  the real strength of Marxan lies in its use of Simulated Annealing to find
        #  solutions to the reserve selection problem. Marxan, however, is also capable
        #  of using simpler, but more rapid, methods to locate potential solutions ,
        #  such as heuristic rules and iterative improvement (see Appendix B -2.2 for
        #  more details on these methods). Because heuristic rules can be applied
        #  extremely quickly and produce reasonable results they are included for use on
        #  extremely large data sets. Modern computers are now so powerful that
        #  heuristics are less necessary as a time saving device, a lthough they are
        #  still useful as research tools. Running Iterative Improvement on itsown gives
        #  very poor solutions. As well as using any of these three methods on their
        #  own, Marxan can also use them in concert with each. If more than one are
        #  selected they will be applied in the following order: Simulated Annealing,
        #  Heuristic, Iterative Improvement. This means that there are seven different
        #  run options:
        #     0  Apply Simulated Annealing followed by a Heuristic
        #     1  Apply Simulated Annealing followed by Iterative Improvement
        #     2  Apply Simulated Annealing followed by a Heuristic, followed by
        #        Iterative Improvement
        #     3  Use only a Heuristic
        #     4  Use only Iterative Improvement
        #     5  Use a Heuristic followed by Iterative Improvement
        #     6  Use only Simulated Annealing
        #  ... each of the above running combinations can be set with a single
        #  number in the ‘input.dat’ file ...
    marxan_RUNMODE  = 1

        #  From Marxan user's manual v. 1.8.10, p. 27
        #  3.2.1.2.3 Heuristic
        #  Variable – ‘HEURTYPE’ Required: No
        #  Description: If you are using a n optional heuristic to find reserve
        #  solutions, this variable defines what type of heuristic algorithm will be
        #  applied. Details of the different Heuristics listed below are given in
        #  Appendix B-2 .3.
        #   0  Richness
        #   1  Greedy
        #   2  Max Rarity
        #   3  Best Rarity
        #   4  Average Rarity
        #   5  Sum Rarity
        #   6  Product Irreplaceability
        #   7  Summation Irreplaceability
    marxan_HEURTYPE  = -1

    marxan_MISSLEVEL  = 1
    marxan_ITIMPTYPE  = 0
    marxan_CLUMPTYPE  = 0
    marxan_VERBOSITY  = 3

    marxan_SAVESOLUTIONSMATRIX  = 3

        #-------------------

    #     #  Need to pull the random seed out of this and always set it myself.
    #     #  A good default would be to use the same random seed as my R code is using
    #     #  here.
    #
    # if (! is.null (parameters$marxan_seed))
    #     { marxan_RANDSEED  = parameters$marxan_seed }

        #  If not using default input parameters, use any values that are
        #  specified in the yaml file for the following variables.
        #  Anything that's not in the yaml file, just fall back to the defaults.
        #  The way to know whether the yaml file has specified a value is that
        #  asking for its slot in the parameters list will return NULL if it
        #  was not specified.

    if (! parameters$marxan_use_default_input_parameters)
        {
        if (! is.null (parameters$marxan_prop))
            { marxan_PROP  = parameters$marxan_prop }

        if (! is.null (parameters$marxan_num_reps))
            { marxan_NUMREPS  = parameters$marxan_num_reps }

        if (! is.null (parameters$marxan_num_iterations))
            { marxan_NUMITNS  = parameters$marxan_num_iterations }

        if (! is.null (parameters$marxan_runmode))
            { marxan_RUNMODE  = parameters$marxan_runmode }

        if (! is.null (parameters$marxan_heurtype))
            { marxan_HEURTYPE  = parameters$marxan_heurtype }
        }

    #-------------------

    #marxan_input_parameters_file_name = "/Users/bill/D/Marxan/input.dat"
    #####marxan_input_parameters_file_name = parameters$marxan_input_parameters_file_name
    # marxan_input_parameters_file_name =
    #     paste0 (marxan_input_dir, .Platform$file.sep,
    #             parameters$marxan_input_parameters_file_name)
    marxan_input_parameters_file_name = parameters$marxan_input_parameters_file_name

    cat ("\n\n>>>>>  IN 13, marxan_input_dir = ", marxan_input_dir)
    cat ("\n>>>>>  parameters$marxan_input_parameters_file_name = ", parameters$marxan_input_parameters_file_name)
    cat ("\n>>>>>  marxan_input_parameters_file_name = ", marxan_input_parameters_file_name, "\n")
    #stop ("is it inputinput?")
    #####rm_cmd = paste ("rm", marxan_input_parameters_file_name)
    #####system (rm_cmd)

    #marxan_input_file_conn = file (marxan_input_parameters_file_name)
    marxan_input_file_conn = marxan_input_parameters_file_name

    #cat ("Marxan input file", file=marxan_input_file_conn, append=TRUE)
    cat ("Marxan input file", file=marxan_input_file_conn)

    cat ("\n\nGeneral Parameters", file=marxan_input_file_conn, append=TRUE)

    cat ("\nBLM", marxan_BLM, file=marxan_input_file_conn, append=TRUE)
    cat ("\nPROP", marxan_PROP, file=marxan_input_file_conn, append=TRUE)
    cat ("\nRANDSEED", marxan_RANDSEED, file=marxan_input_file_conn, append=TRUE)
    cat ("\nNUMREPS", marxan_NUMREPS, file=marxan_input_file_conn, append=TRUE)

    cat ("\n\nAnnealing Parameters", file=marxan_input_file_conn, append=TRUE)
    cat ("\nNUMITNS", marxan_NUMITNS, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSTARTTEMP", marxan_STARTTEMP, file=marxan_input_file_conn, append=TRUE)
    cat ("\nNUMTEMP", marxan_NUMTEMP, file=marxan_input_file_conn, append=TRUE)

    cat ("\n\nCost Threshold", file=marxan_input_file_conn, append=TRUE)
    cat ("\nCOSTTHRESH", marxan_COSTTHRESH, file=marxan_input_file_conn, append=TRUE)
    cat ("\nTHRESHPEN1", marxan_THRESHPEN1, file=marxan_input_file_conn, append=TRUE)
    cat ("\nTHRESHPEN2", marxan_THRESHPEN2, file=marxan_input_file_conn, append=TRUE)

    cat ("\n\nInput Files", file=marxan_input_file_conn, append=TRUE)
    cat ("\nINPUTDIR", marxan_INPUTDIR, file=marxan_input_file_conn, append=TRUE)
    cat ("\nPUNAME", marxan_PUNAME, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSPECNAME", marxan_SPECNAME, file=marxan_input_file_conn, append=TRUE)
    cat ("\nPUVSPRNAME", marxan_PUVSPRNAME, file=marxan_input_file_conn, append=TRUE)

    cat ("\n\nSave Files", file=marxan_input_file_conn, append=TRUE)
    cat ("\nSCENNAME", marxan_SCENNAME, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSAVERUN", marxan_SAVERUN, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSAVEBEST", marxan_SAVEBEST, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSAVESUMMARY", marxan_SAVESUMMARY, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSAVESCEN", marxan_SAVESCEN, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSAVETARGMET", marxan_SAVETARGMET, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSAVESUMSOLN", marxan_SAVESUMSOLN, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSAVEPENALTY", marxan_SAVEPENALTY, file=marxan_input_file_conn, append=TRUE)
    cat ("\nSAVELOG", marxan_SAVELOG, file=marxan_input_file_conn, append=TRUE)
    cat ("\nOUTPUTDIR", marxan_OUTPUTDIR, file=marxan_input_file_conn, append=TRUE)

    cat ("\n\nProgram control.", file=marxan_input_file_conn, append=TRUE)
    cat ("\nRUNMODE", marxan_RUNMODE, file=marxan_input_file_conn, append=TRUE)
    cat ("\nMISSLEVEL", marxan_MISSLEVEL, file=marxan_input_file_conn, append=TRUE)
    cat ("\nITIMPTYPE", marxan_ITIMPTYPE, file=marxan_input_file_conn, append=TRUE)
    cat ("\nHEURTYPE", marxan_HEURTYPE, file=marxan_input_file_conn, append=TRUE)
    cat ("\nCLUMPTYPE", marxan_CLUMPTYPE, file=marxan_input_file_conn, append=TRUE)
    cat ("\nVERBOSITY", marxan_VERBOSITY, file=marxan_input_file_conn, append=TRUE)

    cat ("\nSAVESOLUTIONSMATRIX", marxan_SAVESOLUTIONSMATRIX, file=marxan_input_file_conn, append=TRUE)

    #  When I turn this on, I get the following error message:
    #       Error in UseMethod("close") :
    #       no applicable method for 'close' applied to an object of class "character"
    #  Not sure what I should be handing to the close() function...
    #close (marxan_input_file_conn)

    input_dat_cp_cmd = paste0 ("cp ", marxan_input_parameters_file_name, " ", marxan_IO_dir)
    cat ("\n\ninput_dat_cp_cmd = ", input_dat_cp_cmd, "\n")
    system (input_dat_cp_cmd)

    #---------------------------------------------------------------------------

    system (paste0 ("chmod +x ", parameters$marxan_dir, marxan_executable_name))

    #stop("Testing - just finished marxan input file writing...")


# Run marxan and time measure how long it takes. system.time() returns
# "An object of class "proc_time" which is a numeric vector of length 5,
# containing the
#     - user,
#     - system, and
#     - total elapsed times
# for the currently running R process, and the
#     - cumulative sum of user and
#     - system times
# of any child processes spawned by it on which it has waited.
# (The print method uses the summary method to combine the child times with
# those of the main process.)
#
# The definition of ‘user’ and ‘system’ times is from your OS. Typically it is
# something like
#
# The ‘user time’ is the CPU time charged for the execution of user instructions
# of the calling process. The ‘system time’ is the CPU time charged for
# execution by the system on behalf of the calling process.
#
# Times of child processes are not available on Windows and will always be given
# as NA.
#
# The resolution of the times will be system-specific and on Unix-alikes times
# are rounded down to milliseconds. On modern systems they will be that
# accurate, but on older systems they might be accurate to 1/100 or 1/60 sec.
# They are typically available to 10ms on Windows."

marxan_timings =
system.time ({
    run_marxan (parameters$marxan_dir, marxan_executable_name)
            })

marxan_elapsed_time = marxan_timings["elapsed"]
cat ("\n\nMarxan SA elapsed time = '", marxan_elapsed_time, "'")

    #---------------------------------------------------------------------------

    retVal = list ()
    retVal$marxan_PROP           = marxan_PROP
    retVal$marxan_RANDSEED       = marxan_RANDSEED
    retVal$marxan_NUMREPS        = marxan_NUMREPS
    retVal$marxan_NUMITNS        = marxan_NUMITNS
    retVal$marxan_STARTTEMP      = marxan_STARTTEMP
    retVal$marxan_NUMTEMP        = marxan_NUMTEMP
    retVal$marxan_COSTTHRESH     = marxan_COSTTHRESH
    retVal$marxan_THRESHPEN1     = marxan_THRESHPEN1
    retVal$marxan_THRESHPEN2     = marxan_THRESHPEN2
    retVal$marxan_RUNMODE        = marxan_RUNMODE
    retVal$marxan_MISSLEVEL      = marxan_MISSLEVEL
    retVal$marxan_ITIMPTYPE      = marxan_ITIMPTYPE
    retVal$marxan_HEURTYPE       = marxan_HEURTYPE
    retVal$marxan_CLUMPTYPE      = marxan_CLUMPTYPE

    retVal$RS_user_time          = marxan_timings["user.self"]
    retVal$RS_system_time        = marxan_timings["sys.self"]
    retVal$RS_elapsed_time       = marxan_elapsed_time
    retVal$RS_user_child_time    = marxan_timings["user.child"]
    retVal$RS_sys_child_time     = marxan_timings["sys.child"]

    #---------------------------------------------------------------------------

    return (retVal)
    }

#===============================================================================

#' Set up for and run marxan for COR problem
#'
#' Convenience function to call set_up_for_and_run_marxan with proper
#' arguments for a correct Xu_bd_problem.
#'
#-------------------------------------------------------------------------------

#' @param COR_bd_prob a correct Xu_bd_problem (or subclass)
#' @param marxan_run an RSrun object (or subclass)
#' @param parameters parameters list for the run, usually derived from project.yaml
#'
#' @return list containing marxan_control_values and updated COR_bd_prob
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_marxan_COR <- function (COR_bd_prob,
                                           marxan_run,
                                           parameters)
    {
    marxan_control_values =
       set_up_for_and_run_marxan (COR_bd_prob@cor_PU_spp_pair_indices,
                                    COR_bd_prob@all_PU_IDs,
                                    COR_bd_prob@all_spp_IDs,
                                    COR_bd_prob@PU_col_name,
                                    COR_bd_prob@spp_col_name,

                                    marxan_run,

                                    COR_bd_prob@num_spp,
                                    parameters
                                    )

    return (marxan_control_values)
    }

#===============================================================================

#' Set up for and run marxan for APP problem
#'
#' Convenience function to call set_up_for_and_run_marxan with proper
#' arguments for an apparent Xu_bd_problem
#'
#-------------------------------------------------------------------------------

#' @param APP_bd_prob an apparent Xu_bd_problem (or subclass)
#' @param COR_bd_prob the correct Xu_bd_problem (or subclass) that the apparent problem is derived from
#' @param marxan_run an RSrun object (or subclass)
#' @param parameters parameters list for the run, usually derived from project.yaml
#'
#' @return list containing marxan_control_values and updated APP_bd_prob
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_marxan_APP <- function (APP_bd_prob,
                                           COR_bd_prob,
                                           marxan_run,
                                           parameters)
    {
    marxan_control_values =
        set_up_for_and_run_marxan (APP_bd_prob@APP_prob_info@app_PU_spp_pair_indices,
                                    COR_bd_prob@all_PU_IDs,
                                    COR_bd_prob@all_spp_IDs,
                                    COR_bd_prob@PU_col_name,
                                    COR_bd_prob@spp_col_name,

                                    marxan_run,

                                    APP_bd_prob@num_spp,
                                    parameters
                                    )

    return (marxan_control_values)
    }

#===============================================================================

#' Set up for and run marxan
#'
#' Set up for and run marxan for either COR or APP
#'
#-------------------------------------------------------------------------------

#' @param PU_spp_pair_indices data frame
#' @param PU_IDs integer vector
#' @param spp_IDs integer vector
#' @param PU_col_name character string
#' @param spp_col_name character string
#' @param rsrun RSrun object
#' @param num_spp integer
#' @param parameters list
#'
#' @return Returns marxan control values list
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_marxan = function (PU_spp_pair_indices,       #  app values if running on app
                                      PU_IDs, #####!!!!!#####    #  All values, i.e., cor values?
                                      spp_IDs,  #####!!!!!#####  #  All values, i.e., cor values?
                                      PU_col_name,
                                      spp_col_name,

                                      rsrun,

                                      num_spp,
                                      parameters
                                      )
    {
        #  Get paths to the marxan IO subdirectories.

    topdir            = parameters$fullOutputDir_NO_slash
    marxan_IO_dir     = get_RSrun_path_IO (rsrun, topdir)
    marxan_input_dir  = get_RSrun_path_input (rsrun, topdir)
    marxan_output_dir = get_RSrun_path_output (rsrun, topdir)

    #--------------------

    spf_const =
        compute_marxan_species_penalty_factor (parameters$marxan_spf_rule,
                                               num_spp,
                                               parameters)

    #--------------------

        #  Write the network of species and planning units to marxan input
        #  files.

    write_network_to_marxan_files (PU_spp_pair_indices,       #  app values if running on app
                                   PU_IDs, #####!!!!!#####    #  All values, i.e., cor values?
                                   spp_IDs,  #####!!!!!#####  #  All values, i.e., cor values?

                                   PU_col_name,
                                   spp_col_name,
                                   parameters,
                                   marxan_input_dir,
                                   marxan_output_dir,

                                   spf_const,

                                   rsrun@targets
                                  )

    #--------------------

    marxan_control_values =
        set_marxan_controls_and_run_marxan (marxan_input_dir,
                                            marxan_output_dir,
                                            marxan_IO_dir,
                                            parameters,
                                            rsrun@rand_seed
                                           )

        #  Document what spf_const value was computed before the run of marxan
        #  (above) and then used in the run of marxan.
        #  Want to document the value in case it is useful as an input feature
        #  when trying to learn to predict error.

    marxan_control_values$spf_const = spf_const

    #--------------------

    return (marxan_control_values)
    }

#===============================================================================

