#===============================================================================


#===============================================================================

#  Suggests gtools

#library (gtools)

#  From:  http://www.numbertheory.nl/2013/03/24/parsing-complex-text-files-using-regular-expressions-and-vectorization/



#===============================================================================

# check_for_end_of_file <- function (cur_idx, num_doc_lines, cur_data_line,
#                                    cur_state, prev_state,
#                                    legal_line_starts)
#     {
#         #  If past the last line then prepare to quit.
#         #  Otherwise, determine whether the current line
#         #  starts with a legal kind of value and modify the
#         #  current state if necessary.
#
#     if (cur_idx > num_doc_lines)
#         {
#         cur_state <- "state__finished"
#
#         } else
#         {
#         cur_line_tokens <- tokenize_string (cur_data_line)
#         first_token <- cur_line_tokens [1, "type"]
#
#             #  Check for legal type of start to the line, i.e,
#             #  it needs to start with a symbol (func name or var name) or
#             #  whitespace of an operator (i.e., ">").
#         cur_state <- switch (first_token,
#
#                     #  If you find whitespace, then you're either continuing
#                     #  a variable declaration or a function declaration.
#
#                 whitespace = set_continuation_state (),
#
#                 symbol = set_symbol_state (),
#
#                 operator = state__block_start,
#
#                 ... = stop (paste0 ("\n\nUnexpected token at start of line ",
#                               cur_data_line_num,
#                               ".   Token = '", first_token,
#                               "'.  Line = \n'", cur_data_line,
#                               "'\n\n"))
#
#                 )  #  end switch - first_token
#         }  #  end else - not past last line yet
#
#     return (cur_state)
#     }

#===============================================================================
#===============================================================================

get_first_token_type_of_line <- function (data_line)
    {
    line_tokens <- sourcetools::tokenize_string (data_line)
    first_token_type_of_line <- line_tokens [1, "type"]

    return (first_token_type_of_line)
    }

#===============================================================================

close_open_subsection_if_necessary <- function (prev_state)
    {
    if (prev_state == "state__var_first_line_of_desc" |
        prev_state == "state__var_desc_cont_line")
        {
            #  Variables are written out as subsections and that means
            #  they start with brackets and finding a function name
            #  means that the var subsection needs to be closed.
        cat ("\n#\' }}")
        }
    }

#===============================================================================

finish_up <- function (prev_state)
    {
    close_open_subsection_if_necessary (prev_state)

    stop ("\n\n-----  FINISHED  -----\n\n")
    }


#===============================================================================

stop_due_to_unexpected_token <- function (token,
                                          cur_data_line_num,
                                          cur_data_line)
    {
    stop (paste0 ("\n\nUnexpected token at start of line ",
                  cur_data_line_num,
                  ".   Token = '", first_token,
                  "'.  Line = \n'", cur_data_line,
                  "'\n\n"))
    }

#===============================================================================

decode_block_start <- function (prev_state)
    {
    close_open_subsection_if_necessary (prev_state)

    cat ("\n\n\n\n")

    next_state <- "state__func_first_line_of_decl"
    return (next_state)
    }

#===============================================================================

decode_func_first_line_of_decl <- function (cur_data_line,
                                            cur_data_line_num,
                                            prev_state,
                                            first_token_of_next_line)
    {
        #  If the last thing you were doing before this was working on
        #  a variable's subsection, that needs to be closed out before
        #  doing anything else.

    close_open_subsection_if_necessary (prev_state)

    cat ("\n\n\n\n\n#\'@section Local Variable Structures and examples:")
    cat ("\n#\'Here is the output of str() for each variable visible in the function.")
    cat ("\n#\'Note that the particular counts and values given are just examples to show")
    cat ("\n#\'what the data might look like.")
    cat ("\n#\'")

        #  First thing on the line is a symbol, so treat it as a
        #  function name.
        #  Display function names in bold, so insert the latex
        #  "strong" command before it and put opening and closing
        #  brackets for that command around the name, then
        #  echo the rest of the line not in bold.

    cur_line_tokens <- sourcetools::tokenize_string (cur_data_line)
    func_name <- cur_line_tokens [1, "value"]

    cat ("\n#\' \\strong{FUNCTION:  ",
         func_name,
         "}",
         sep='')

    cat (stringr::str_sub (cur_data_line,
                           cur_line_tokens [2, "column"],
                           stringr::str_length (cur_data_line)))

    next_state <-
        switch (first_token_of_next_line,

                whitespace = "state__func_decl_cont_line",
                symbol     = "state__var_first_line_of_desc",
                operator   = "state__block_start",
                ...        = stop_due_to_unexpected_token (
                                                    first_token_of_next_line,
                                                    cur_data_line_num,
                                                    cur_data_line)
                )

    return (next_state)
    }

#===============================================================================

decode_func_decl_cont_line <- function (cur_data_line,
                                        cur_data_line_num,
                                        first_token_of_next_line)
    {
        #  First thing on the line is whitespace and the previous line
        #  was either a function declaration line or a function declaration
        #  continuation line.

    cat ("\n#\' ", cur_data_line, sep='')  #  Just echo this line.

    next_state <-
        switch (first_token_of_next_line,

                whitespace = "state__func_decl_cont_line",
                symbol     = "state__var_first_line_of_desc",
                operator   = "state__block_start",
                ...        = stop_due_to_unexpected_token (
                                                    first_token_of_next_line,
                                                    cur_data_line_num,
                                                    cur_data_line)
                )

    return (next_state)
    }

#===============================================================================

decode_var_first_line_of_desc <- function (cur_data_line,
                                           cur_data_line_num,
                                           prev_state,
                                           first_token_of_next_line)
    {
        #  If the last thing you were doing before this was working on
        #  a variable's subsection, that needs to be closed out before
        #  doing anything else.

    close_open_subsection_if_necessary (prev_state)

        #  First thing on the line is a symbol, so treat it as a
        #  variable name.
        #  Display variable name as a subsection header.

    cur_line_tokens <- sourcetools::tokenize_string (cur_data_line)
    variable_name <- cur_line_tokens [1, "value"]

    cat ("\n#\' \\subsection{", variable_name, "}{", sep='')
    cat ("\n#\' \\preformatted{")

    cat ("\n#\' ", cur_data_line, sep='')  #  Echo the whole line in subsection.

    next_state <-
        switch (first_token_of_next_line,

                whitespace = "state__var_desc_cont_line",
                symbol     = "state__var_first_line_of_desc",
                operator   = "state__block_start",
                ...        = stop_due_to_unexpected_token (
                                                    first_token_of_next_line,
                                                    cur_data_line_num,
                                                    cur_data_line)
                )

    return (next_state)
    }

# state__start_of_file
# state__block_start
# state__finished
# state__func_decl_cont_line
# state__func_first_line_of_decl
# state__var_desc_cont_line
# state__var_first_line_of_desc

#===============================================================================

decode_var_desc_cont_line <- function (cur_data_line,
                                       cur_data_line_num,
                                       first_token_of_next_line)
        {
        #  First thing on the line is whitespace and the previous line
        #  was either a variable description line or a variable description
        #  continuation line.

    cat ("\n#\' ", cur_data_line, sep='')  #  Just echo this line.

    next_state <-
        switch (first_token_of_next_line,

                whitespace = "state__var_desc_cont_line",
                symbol     = "state__var_first_line_of_desc",
                operator   = "state__block_start",
                ...        = stop_due_to_unexpected_token (
                                                    first_token_of_next_line,
                                                    cur_data_line_num,
                                                    cur_data_line)
                )

    return (next_state)
    }

#===============================================================================

state_transition <- function (all_data, doc_line_numbers)
    {
    num_doc_lines     <- length (doc_line_numbers)

    prev_state        <- "state__start_of_file"
    cur_state         <- "state__block_start"

#    legal_line_starts <- c ("symbol", "whitespace", "operator", "EOF")

    for (cur_idx in 1:num_doc_lines)
        {
        cur_data_line_num <- doc_line_numbers [cur_idx]
        cur_data_line <- all_data [cur_data_line_num]

        # cur_state <- check_for_end_of_file (cur_idx, num_doc_lines,
        #                                     cur_state, prev_state,
        #                                     cur_data_line, legal_line_starts)

            #---------------------------------------------------------
            #  Get first token of next line since it will affect the
            #  setting of the state at the end of operating on the
            #  current line.
            #---------------------------------------------------------

        next_idx <- cur_idx + 1
        if (next_idx <= num_doc_lines)
            {
                #  Not at end of file yet.

            next_line_num <- doc_line_numbers [next_idx]
            next_data_line <- all_data [next_line_num]

            first_token_of_next_line <-
                get_first_token_type_of_line (next_data_line)

            } else  #  cur line is last line of file
            {
            first_token_of_next_line <- "EOF"
            }

                #------------------------------------------------
                #  Ready now to take action on the current line
                #  based on what state we're in.
                #------------------------------------------------

if(FALSE)
{
cat ("\n\n::::::::::::::  Just before main switch:")
cat ("\n    prev_state = '", prev_state, "'")
cat ("\n    cur_state = '", cur_state, "'")
cat ("\n    first_token_of_next_line = '", first_token_of_next_line, "'")
cat ("\n    cur_data_line_num = ", cur_data_line_num)
cat ("\n    cur_data_line = '", cur_data_line, "'")
cat ("\n::::::::::::::\n")
browser()
}

        next_state <-
            switch (cur_state,

                    state__var_first_line_of_desc =
                        decode_var_first_line_of_desc (
                                                    cur_data_line,
                                                    cur_data_line_num,
                                                    prev_state,
                                                    first_token_of_next_line),

                    state__var_desc_cont_line =
                        decode_var_desc_cont_line (cur_data_line,
                                                   cur_data_line_num,
                                                   first_token_of_next_line),

                    state__func_first_line_of_decl =
                        decode_func_first_line_of_decl (cur_data_line,
                                                        cur_data_line_num,
                                                        prev_state,
                                                        first_token_of_next_line),

                    state__func_decl_cont_line =
                        decode_func_decl_cont_line (cur_data_line,
                                                    cur_data_line_num,
                                                    first_token_of_next_line),

                    state__block_start =
                        decode_block_start (prev_state),

                    state__finished =
                        finish_up (prev_state),

                    ... = stop (paste0 ("\n\nFailed at line ", cur_data_line_num,
                              ", i.e., no matching state found for state '",
                              cur_state, "' at line ", cur_data_line, "'\n\n"))

                    )  #  end switch (cur_state)

        prev_state <- cur_state
        cur_state <- next_state

        }  #  end for - (cur_line_num in 1:num_doc_lines)

    }

#===============================================================================

#' Title
#'
#' @export

test_do_it <- function ()
    {
    infile = "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1836_marxan_simulated_annealing.completedTzarEmulation/consoleSinkOutput.temp.txt"

    do_it (infile)
    }

#===============================================================================

#' Title
#'
#' @return
#' @export

do_it <- function (infile)
    {
    print (getwd())
    sinkFilePath = "./funcvars.doc.txt"

        #  Open a file to echo console to.
    tempConsoleOutFile <- file (sinkFilePath, open="wt")

    	#  Redirect console output to the file.
    sink (tempConsoleOutFile, split=TRUE)

    all_data <- readLines (infile)

#                all_data <- all_data [1:100]  # for testing only

#    lines_starting_with_blank <- grep ("^ ", all_data)
    lines_containing_start_or_end <- grep (" doc_vars_in_this_func  ", all_data)
#    lines_containing_start <- grep ("<  START doc_vars_in_this_func", all_data)

    original_line_numbers <- 1:length (all_data)
    interval_nums <- findInterval (original_line_numbers,
                                              lines_containing_start_or_end)
    interval_containing_line <-
        cbind (original_line_numbers, interval_nums)

    doc_line_nums_and_intervals <- interval_containing_line [gtools::odd (interval_containing_line [,2]),]
    colnames(doc_line_nums_and_intervals) <- c("line_num", "interval_num")

doc_line_numbers <- doc_line_nums_and_intervals [,"line_num"]
state_transition (all_data, doc_line_numbers)


#  REPLACE THIS SECTION WITH SOMETHING THAT FINDS THESE LOCATIONS AND
#  MODIFIES THE LINE RIGHT AFTER IT TO START WITH A SPACE AND THEN
#  HAVE THE \strong ETC, ALL REPLACING THAT LINE.
#  - STARTING WITH A SPACE MEANS THAT THE NEXT PASS WILL IGNORE THE LINE AND
#    PASS IT STRAIGHT THROUGH.
#  - SIMILARLY, ANY LINE THAT DOESN'T START WITH A SPACE COULD HAVE THE DOUBLE
#    BRACKET ADDED TO THE FRONT OF IT?  STILL DOESN'T WORK RIGHT THOUGH FOR
#    THE FIRST VARIABLE IN A FUNCTION.
#  - COULD SPIT OUT A DUMMY {{ AT THE END OF THE SECTION HEADER THOUGH AND
#    MAYBE THAT WOULD PAIR UP WITH THE ODD }} AND END UP BEING IGNORED.
#    - ACTUALLY, IF THIS PAIR ENDED UP ON A LINE OF ITS OWN, YOU COULD MAKE
#      ANOTHER PASS THAT GREPPED FOR LINES LIKE THAT AND REMOVED THEM.
#  - ALSO STILL NEED TO STRIP OFF FROM THE COLON TO END OF LINE ON VAR LINES.
    # doc_line_numbers <- doc_line_nums_and_intervals [,"line_num"]
    #
    # doc_line_nums_and_intervals <-
    #     doc_line_nums_and_intervals [! (doc_line_numbers %in% lines_containing_start),]




    reduced_data <- all_data [doc_line_nums_and_intervals [,"line_num"]]
#browser()
    sapply (reduced_data, write_the_line)
#    invisible()



#  ...

        sink ()
        close (tempConsoleOutFile)




    return (NULL)
}

#do_it()

#===============================================================================
#===============================================================================

write_the_line <- function (cur_line_text)
    {
#    browser()
    x <- grep ("<  START doc_vars_in_this_func", cur_line_text)
    if (length (x) == 0)
        {
        if (startsWith (cur_line_text, " "))
            {
                #  It's a detail or continuation line.
                #  Just echo the line.
            cat ("\n#\' ", cur_line_text, sep='')

            } else
            {
                #  It's a function name or a variable name.
                #  Start a subsection.
            cat ("\n#\' }}\n#\'")
            cat ("\n#\' \\subsection{", cur_line_text, "}{", sep='')
            cat ("\n#\' \\preformatted{")
            }
        } else
        {
        cat ("\n\n\n\n\n#\'@section Local Variable Structures and examples:")
        cat ("\n#\'Here is the output of str() for each variable visible in the function.")
        cat ("\n#\'Note that the particular counts and values given are just examples to show")
        cat ("\n#\'what the data might look like.")
        cat ("\n#\'")
        }
    }

#===============================================================================
#===============================================================================

# set_symbol_state <- function (cur_state, prev_state)
#     {
#     if (prev_state == "state__block_start")
#         {
#         cur_state <- "state__first_line_of_func_decl"
#
#         } else if (prev_state == "state__first_line_of_func_decl")
#         {
#         cur_state <- "state__var_name_line"
#
#         } else if (prev_state == "state__var_name_line")
#         {
#             #  E.g., "stuffvar1" line followed by "stuffvar2..." line
#
#         cur_state <- "state__var_name_line"
#         }
#
#
#     state__var_name_line = xxx(),
#
#     state__var_continuation_line = xxx(),
#
#     return (cur_state)
#     }

#===============================================================================


