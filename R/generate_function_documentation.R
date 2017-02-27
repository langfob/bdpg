#===============================================================================
#
#                   generate_function_documentation.R
#
#===============================================================================

#  Library references:
#      - Uses gtools::odd()
#      - Uses sourcetools::tokenize_string()

#  Part of the mainline was derived by reading:
#  From:  http://www.numbertheory.nl/2013/03/24/parsing-complex-text-files-using-regular-expressions-and-vectorization/

#===============================================================================

#' Get type of first token on the line
#'
#' Tokenizes the whole line and then returns the type associated with the
#' first token on the line.  This is useful because the first token is what
#' flags whether the line is the start of a chunk, variable, or function or
#' whether it's a continuation line of a function or a variable.
#'
#' This doesn't really need to tokenize the whole line since it only uses the
#' first token.  Not sure if there's a way to ask for just one token.
#'
#' @inheritParams decode_var_first_line_of_desc
#'
#' @return character string containing the first token type of the line,
#'     e.g., "whitespace" or "symbol"

get_first_token_type_of_line <- function (data_line)
    {
    line_tokens <- sourcetools::tokenize_string (data_line)
    first_token_type_of_line <- line_tokens [1, "type"]

    return (first_token_type_of_line)
    }

#===============================================================================

#' Write closing brackets for previous subsection if open brackets remain
#'
#' Variables are written out as subsections and that means they start with
#' brackets.  Finding a function name means that the var subsection brackets
#' need to be closed and this function does that by just spitting out two
#' brackets.
#'
#' @inheritParams decode_var_first_line_of_desc
#'
#' @return Returns nothing

close_open_subsection_if_necessary <- function (prev_state)
    {
    if (prev_state == "state__var_first_line_of_desc" |
        prev_state == "state__var_desc_cont_line")
        {
        cat ("\n#\' }}")
        }
    }

#===============================================================================

#' Clean up any subsection left open when end of file is found
#'
#' @inheritParams decode_var_first_line_of_desc
#' @seealso \code{\link{close_open_subsection_if_necessary}}
#' @return Returns nothing

finish_up <- function (prev_state)
    {
    close_open_subsection_if_necessary (prev_state)

    stop ("\n\n-----  FINISHED  -----\n\n")
    }


#===============================================================================

#' Quit since an unexpected token was found
#'
#' Emits an error message containint the unexpected token, the input line
#' number where it occurred, and the line itself; then it quits.
#'
#' @inheritParams decode_var_first_line_of_desc
#' @param token character string indicating the token type, e.g.,
#'     "whitespace" or "symbol"
#'
#' @return Returns nothing

stop_due_to_unexpected_token <- function (token,
                                          data_line_num,
                                          data_line)
    {
    stop (paste0 ("\n\nUnexpected token at start of line ",
                  data_line_num,
                  ".   Token = '", first_token,
                  "'.  Line = \n'", data_line,
                  "'\n\n"))
    }

#===============================================================================

#' Start new function block and clean up previous block if necessary
#'
#' At start of new block of variable descriptions for a new function,
#' write some output to visually separate the new output from the previous
#' block.  Also clean up any variable subsection that's still open from
#' the previous block, i.e., write closing brackets for it.
#'
#' @inheritParams decode_var_first_line_of_desc
#' @param visual_separator character string to write between the end of the
#'     previous block and the new block to make it easier to pick out where
#'     one ends and the other begins
#'
#' @seealso \code{\link{close_open_subsection_if_necessary}}
#' @inherit decode_var_first_line_of_desc return

decode_block_start <- function (prev_state,
                                visual_separator="\n\n\n\n")
    {
    close_open_subsection_if_necessary (prev_state)

    cat (visual_separator)

    next_state <- "state__func_first_line_of_decl"
    return (next_state)
    }

#===============================================================================

#' Title
#'
#' @inheritParams decode_var_first_line_of_desc
#'
#' @inherit decode_var_first_line_of_desc return

decode_func_first_line_of_decl <- function (data_line,
                                            data_line_num,
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

    cur_line_tokens <- sourcetools::tokenize_string (data_line)
    func_name <- cur_line_tokens [1, "value"]

    cat ("\n#\' \\strong{FUNCTION:  ",
         func_name,
         "}",
         sep='')

    cat (stringr::str_sub (data_line,
                           cur_line_tokens [2, "column"],
                           stringr::str_length (data_line)))

    next_state <-
        switch (first_token_of_next_line,

                whitespace = "state__func_decl_cont_line",
                symbol     = "state__var_first_line_of_desc",
                operator   = "state__block_start",
                ...        = stop_due_to_unexpected_token (
                                                    first_token_of_next_line,
                                                    data_line_num,
                                                    data_line)
                )

    return (next_state)
    }

#===============================================================================

#' Title
#'
#' @inheritParams decode_var_first_line_of_desc
#'
#' @inherit decode_var_first_line_of_desc return

decode_func_decl_cont_line <- function (data_line,
                                        data_line_num,
                                        first_token_of_next_line)
    {
        #  First thing on the line is whitespace and the previous line
        #  was either a function declaration line or a function declaration
        #  continuation line.

    cat ("\n#\' ", data_line, sep='')  #  Just echo this line.

    next_state <-
        switch (first_token_of_next_line,

                whitespace = "state__func_decl_cont_line",
                symbol     = "state__var_first_line_of_desc",
                operator   = "state__block_start",
                ...        = stop_due_to_unexpected_token (
                                                    first_token_of_next_line,
                                                    data_line_num,
                                                    data_line)
                )

    return (next_state)
    }

#===============================================================================

#' Title
#'
#' @param data_line character string containing one full line from the input
#'     file
#' @param data_line_num integer line number of data_line in original input file
#' @param prev_state character string indicating the previous state, e.g.,
#'     "state__block_start"
#' @param first_token_of_next_line character string containing the first
#'     token type of the next line in the input file, e.g., "whitespace" or
#'     "symbol"
#'
#' @return character string indicating the next state to transition to, e.g.,
#'     "state__block_start"

decode_var_first_line_of_desc <- function (data_line,
                                           data_line_num,
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

    cur_line_tokens <- sourcetools::tokenize_string (data_line)
    variable_name <- cur_line_tokens [1, "value"]

    cat ("\n#\' \\subsection{", variable_name, "}{", sep='')
    cat ("\n#\' \\preformatted{")

    cat ("\n#\' ", data_line, sep='')  #  Echo the whole line in subsection.

    next_state <-
        switch (first_token_of_next_line,

                whitespace = "state__var_desc_cont_line",
                symbol     = "state__var_first_line_of_desc",
                operator   = "state__block_start",
                ...        = stop_due_to_unexpected_token (
                                                    first_token_of_next_line,
                                                    data_line_num,
                                                    data_line)
                )

    return (next_state)
    }

#===============================================================================

#' Title
#'
#' @inheritParams decode_var_first_line_of_desc
#'
#' @return

decode_var_desc_cont_line <- function (data_line,
                                       data_line_num,
                                       first_token_of_next_line)
    {
        #  First thing on the line is whitespace and the previous line
        #  was either a variable description line or a variable description
        #  continuation line.

    cat ("\n#\' ", data_line, sep='')  #  Just echo this line.

    next_state <-
        switch (first_token_of_next_line,

                whitespace = "state__var_desc_cont_line",
                symbol     = "state__var_first_line_of_desc",
                operator   = "state__block_start",
                ...        = stop_due_to_unexpected_token (
                                                    first_token_of_next_line,
                                                    data_line_num,
                                                    data_line)
                )

    return (next_state)
    }

#===============================================================================

#' Parse reduced input text array and write outputs depending on state
#'
#' This function takes an input array of strings that were lines in the
#' input file and parses it into chunks corresponding to functions and
#' variables.  As it encounters each chunk, it writes out a properly
#' formatted roxygen subsection block that can be pasted into the function's
#' documentation.  It also writes a bit of header text for the section.
#' Each variable is written as a subsection.
#'
#' @section Assumptions:
#' \describe{
#'   \item{Operator begins START and END lines}{Each relevant section of the
#'       input file begins with a line containing
#'       ">>>>  START doc_vars_in_this_func  >>>>" and ends with a line
#'       containing "<<<<  END doc_vars_in_this_func  <<<<".  It doesn't matter
#'       how many "greater than" or "less than" characters are in the line.
#'       All that matters is that the line begins with an operator so that the
#'       tokenizer will return "operator" as the first token on the line to
#'       flag it as the start of a block. }
#' }
#'
#' @section Function declaration in output:
#' For each function block that is parsed, the function's name and argument list
#' are also written to the output, even though they are not intended to be
#' included in what is pasted into the function's documentation. (That
#' information is already in the documentation.) The information is only
#' included to make it easier to identify which function the variable list
#' belongs to when cutting it out of a big file full of outputs.
#'
#' @section Parameter declarations in output:
#' The output also includes descriptions for variables that are a part of the
#' function's parameter list since the output is for all variables known in
#' the function.  These variables should already appear in the @@params section
#' of the function's documentation, so they can be deleted from the output.
#' However, they're included here for 2 reasons.  First, this code is pretty
#' quick and dirty and it would take more coding to parse out the function's
#' argument list to determine overlapping variables.  Second, the information
#' written out here can be helpful in building the @@parames and @@return
#' sections of the function's documentation before removing them from the
#' sections generated here.
#'
#' @section States Used In The Parsing:
#' \describe{
#'   \item{state__start_of_file}{Before parsing begins.}
#'   \item{state__block_start}{Sitting on a ">>>  START..." line at the head
#'       of a new function block.}
#'   \item{state__func_first_line_of_decl}{Sitting on the first line of a
#'       function declaration.  Starts with a symbol, not whitespace.
#'       Immediately follows the block start line with no intervening lines.}
#'   \item{state__func_decl_cont_line}{Sitting on a continuation line of a
#'       multi-line function declaration.  Line starts with whitespace.}
#'   \item{state__var_first_line_of_desc}{Sitting on the first line of the
#'       description of a variable.  Starts with a symbol, not whitespace.
#'       Immediately follows the end of a function declaration or variable
#'       declaration  with no intervening lines.}
#'   \item{state__var_desc_cont_line}{Sitting on a continuation line of a
#'       multi-line description of a variable.  Line starts with whitespace.}
#'   \item{state__finished}{Found end of file, ready to do final cleanup.}
#' }
#'
#' @section Legal Line Starts In The Parsing:
#' \describe{
#'   \item{symbol}{An R function name or variable name with no preceding white
#'       space.}
#'   \item{whitespace}{Spaces and/or tabs.}
#'   \item{operator}{An R operator; in this case the only one that should
#'       occur is the ">" that is used on the block start lines.}
#'   \item{EOF}{End of file; not returned by the tokenizer, but set in this
#'       function and its subcalls.}
#'   }


#'
#' @param all_data Vector of character strings, each entry containing the text
#'     from the corresponding line in the original input file
#' @param doc_line_numbers Vector of integer line numbers serving as an index
#'     into the reduced set of lines of interest in the original data.  For
#'     example, lines 1 and 2 of the original input may be irrelevant, while
#'     line 3 of the original file is the first useful line for parsing, so
#'     doc_line_numbers[1] = 3.
#'
#' @return nothing

state_transition <- function (all_data, doc_line_numbers)
    {
    num_doc_lines     <- length (doc_line_numbers)

    prev_state        <- "state__start_of_file"
    cur_state         <- "state__block_start"

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

#' Test running generate_func_var_roxygen_comments() function
#'
#' Loads an existing input file and runs the function.
#'
#' @return Returns nothing (but in the future it should flag success/failure).
#' @export

test_generate_func_var_roxygen_comments <- function ()
    {
    infile = "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1836_marxan_simulated_annealing.completedTzarEmulation/consoleSinkOutput.temp.txt"
    sinkFilePath = "/Users/bill/D/Projects/ProblemDifficulty/ProbDiff_Notes/funcvars.doc.txt"

    generate_func_var_roxygen_comments (infile, sinkFilePath)
    }

#===============================================================================

#' Read text from doc_vars_in_this_func() and convert to roxygen comments
#'
#' @return Returns nothing
#' @export

generate_func_var_roxygen_comments <- function (infile, sinkFilePath)
    {
    print (getwd())

        #  Open a file to echo console to and redirect output there.
    tempConsoleOutFile <- file (sinkFilePath, open="wt")
    sink (tempConsoleOutFile, split=TRUE)

        #  Load the text file to be parsed.
    all_data <- readLines (infile)

        #  Sections of interest in the file are bracketed with a start line
        #  and an end line containing the " doc_vars_in_this_func  ", i.e.,
        #          ">>>>  START doc_vars_in_this_func  >>>>" and
        #          "<<<<  END doc_vars_in_this_func  <<<<".
        #  Find all of these lines and use them to construct intervals.
        #  We only have to search for "doc_vars_in_this_func" because that
        #  occurs in both the START and END lines.
        #
        #  Assign every line in the original file to an interval number.
        #  Then, every line with an odd interval number is a line of interest,
        #  so you can filter the file to only look at those lines.

    lines_containing_start_or_end <- grep (" doc_vars_in_this_func  ", all_data)

                #  Assign an interval number to each set of lines between
                #  the START and END markers and between each END markers
                #  and the next START marker.

    original_line_numbers <- 1:length (all_data)
    interval_nums <- findInterval (original_line_numbers,
                                   lines_containing_start_or_end)
    interval_containing_line <-
        cbind (original_line_numbers, interval_nums)

                #  Select all lines that fall inside the START/END pairs and
                #  ignore the ones between END/START pairs.  In other words,
                #  just select lines with an odd interval number.
                #  - findInterval() starts a new interval number each time that
                #    it encounters a line in the set of start/end pairs.
                #    This means that the START line is included in the intervals
                #    that we save and the END line is in the intervals that get
                #    tossed.
                #  - Note that choosing the odd intervals (rather than the even
                #    ones) still works even if there are no input lines before
                #    the first occurrence of a START line.
                #    It looks like the findInterval() routine starts numbering
                #    intervals at 0 until it finds the first line matching its
                #    search criteria and then sets the interval number to 1 when
                #    it finds that first match.  So, if that match occurs on the
                #    first line, no lines get assigned an interval number of 0
                #    and the intervals that we want still have odd numbers.

    doc_line_nums_and_intervals <- interval_containing_line [gtools::odd (interval_containing_line [,2]),]
    colnames(doc_line_nums_and_intervals) <- c("line_num", "interval_num")

                #  Build an index from the selected lines back into their
                #  corresponding line numbers in the original data so that
                #  you can work only the lines of interest but still retrieve
                #  their original text

    doc_line_numbers <- doc_line_nums_and_intervals [,"line_num"]

        #  Ready now to parse the selected lines and spit out the appropriate
        #  roxygen formatting of the data in those lines.
        #  Do the parsing using a simple state-transition switching mechanism.

    state_transition (all_data, doc_line_numbers)

        #  All done, so close out the file containing the roxygen output.

    sink ()
    close (tempConsoleOutFile)
    }

#===============================================================================


