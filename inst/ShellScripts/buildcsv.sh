#!/bin/bash

#                                   buildcsv.sh

#   ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh

#  Cloned from ~/.../testgit.sh.  Not sure where that file came from...

#  Don't forget to make executable.
#  chmod u+x buildcsv.sh

#-------------------------------------------------------------------------------

# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    Gurobi         Gurobi         rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    Marxan_SA      Marxan_SA      rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    ZL_Backward    ZL_Backward    rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    ZL_Forward     ZL_Forward     rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    SR_Forward     SR_Forward     rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    SR_Backward    SR_Backward    rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    UR_Backward    UR_Backward    rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    UR_Forward     UR_Forward     rsrun_results.csv    .
#
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    Marxan_SA_SS    Marxan_SA    rsrun_results_summed_solution.csv    .

#-------------------------------------------------------------------------------


echo

if [ "$#" -ne 4 ]; then
    echo
    echo "Number of arguments = '$#'"
    echo "Must have exactly 2 arguments."
    echo "Usage:   rsName   rsFileRoot   infilesName    outputDir"
    echo "    $0    Marxan_SA_SS    Marxan_SA    rsrun_results_summed_solution.csv    ."
    echo "OR"
    echo "    $0    Marxan_SA    Marxan_SA    rsrun_results.csv    ."
    echo "OR"
    echo "    $0    Gurobi    Gurobi    rsrun_results.csv    ."
    echo
fi

rsName=$1
echo "rsName: '$rsName'"
#echo "Reserve selector name: '$1'"
echo

rsFileRoot=$2
echo "rsFileRoot: '$rsFileRoot'"
#echo "Reserve selector file root: '$2'"
echo

infilesName=$3
echo "infilesName: '$infilesName'"
#echo "Infiles name: '$3'"
echo

start_dir=`pwd`
echo "Starting from dir: '$start_dir'"
echo


#cd ~/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/24_easy.completedTzarEmulation
cd $4
moved_to_dir=`pwd`
echo "Now in working dir: '$moved_to_dir'"
echo

#-------------------------------------------------------------------------------

###  nohup    time    find    ./1956?_easy    -name    rsrun_results.csv    >    easy_rsrun_results_1956.txt    &
#nohup    time
#find    ./198??_easy    -name    rsrun_results.csv    >    easy_rsrun_results_198.txt
#find    ./198??_easy    -name    rsrun_results.csv    >    rsrun_results_paths.txt
#&

echo
echo "Collect all rsrun file paths for all reserve selectors into one file."
echo "This will be all rsrun_results.csv files for most reserve selectors."
echo "This will be just be marxan rsrun_results_summed_solution.csv files"
echo "for Marxan_SA_SS."
echo
time    find    .    -name    $infilesName    >    rsrun_results_full_paths.txt

#-------------------------------------------------------------------------------

echo
echo "Select only the paths that are specific to the desired reserve selector."
echo "For example, only Gurobi rsrun_results.csv files."
echo "For Marxan_SA_SS, this will be Marxan_SA rsrun_results_summed_solution.csv"
echo "files since there are no Marxan_SA_SS directories because the summed "
echo "solution results are output alongside the Marxan_SA results, just in a"
echo "different file."
echo "Use -h option on grep so that file names don't appear in output."
echo
grep    -h    $rsFileRoot    rsrun_results_full_paths.txt    >    $rsFileRoot.txt

#-------------------------------------------------------------------------------

echo
echo "Aggregate contents of all outputs for given reserve selector into one file."
echo
cat    $rsFileRoot.txt    |     xargs    -n1 cat    >    $rsName.csv.with.many.headers
echo
wc $rsName.csv.with.many.headers

echo
echo "Since every run output file contains a header line and an output line,"
echo "the aggregated output file has an identical copy of the header for"
echo "every line of output data and the aggregated file is twice as long as"
echo "it needs to be."
echo "Strip out all but 1 of the many duplicated header lines from agg file."
echo
grep    -i    UUID    $rsName.csv.with.many.headers    |    head    -n 1    > $rsName.csv
echo
grep    -v    UUID    $rsName.csv.with.many.headers    >>    $rsName.csv
echo
wc $rsName.csv

#-------------------------------------------------------------------------------


