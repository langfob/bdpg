#!/bin/bash

#===============================================================================

#                               combine_csv.sh

#   ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/combine_csv.sh

#===============================================================================

#  Don't forget to make executable.
#  chmod u+x combine_csv.sh

if [ "$#" -ne 2 ]; then
    echo
    echo "Usage: $0    rsName     outputDir"
    echo
    echo "Number of arguments = '$#'"
    echo
    echo "Must give 2 arguments:  rs_name  outDir"
fi

#===============================================================================

echo
start_dir=`pwd`
echo "Starting from dir: '$start_dir'"
echo

rs_name=$1
echo "Reserve selector name: '$rs_name'"
echo

out_dir=$2
cd $out_dir
new_dir=`pwd`
echo "Now in output dir: '$new_dir'"
echo

target_file=${rs_name}.combined_results.csv
echo "target_file = $target_file"

#src_dir_start="/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_"
src_dir_start="/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base_2nd_attempt/bdpg_20_variants_all_rs_easy_base_"
echo "src_dir_start = $src_dir_start"

#===============================================================================

#----------  02

echo
echo "Add 02 errors"
src_dir_mid=${src_dir_start}02
                #runset_path=${src_dir_mid}_err_amt/default_runset
runset_path=${src_dir_mid}_err_amt_2nd_try
    echo "runset_path = $runset_path"
src_file=${runset_path}/$rs_name.csv
    echo "src_file = $src_file"
echo
echo "----->  Build $rs_name.csv file using find, etc."

#time ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    $rs_name    $runset_path

echo
echo "    Add $rs_name.csv contents to combined output file."

    #  Different for 02 because adding header line for full file
grep    -i    UUID    $src_file    |     head    -n 1    > $target_file

grep    -v    UUID    $src_file    >>    $target_file

#----------  05

echo
echo "Add 05 errors"
src_dir_mid=${src_dir_start}05
                #runset_path=${src_dir_mid}_err_amt/default_runset
runset_path=${src_dir_mid}_err_amt_2nd_try
    echo "runset_path = $runset_path"
src_file=${runset_path}/$rs_name.csv
    echo "src_file = $src_file"
echo
echo "----->  Build $rs_name.csv file using find, etc."

#time ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    $rs_name    $runset_path

echo
echo "    Add $rs_name.csv contents to combined output file."
grep    -v    UUID    $src_file    >>    $target_file

#----------  075

echo
echo "Add 075 errors"
src_dir_mid=${src_dir_start}075
                #runset_path=${src_dir_mid}_err_amt/default_runset
runset_path=${src_dir_mid}_err_amt_2nd_try
    echo "runset_path = $runset_path"
src_file=${runset_path}/$rs_name.csv
    echo "src_file = $src_file"
echo
echo "----->  Build $rs_name.csv file using find, etc."

#time ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    $rs_name    $runset_path

echo
echo "    Add $rs_name.csv contents to combined output file."
grep    -v    UUID    $src_file    >>    $target_file

#----------  10

echo
echo "Add 10 errors"
src_dir_mid=${src_dir_start}10
                #runset_path=${src_dir_mid}_err_amt/default_runset
runset_path=${src_dir_mid}_err_amt_2nd_try
    echo "runset_path = $runset_path"
src_file=${runset_path}/$rs_name.csv
    echo "src_file = $src_file"
echo
echo "----->  Build $rs_name.csv file using find, etc."

#time ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    $rs_name    $runset_path

echo
echo "    Add $rs_name.csv contents to combined output file."
grep    -v    UUID    $src_file    >>    $target_file

#----------  15

echo
echo "Add 15 errors"
src_dir_mid=${src_dir_start}15
                #runset_path=${src_dir_mid}_err_amt/default_runset
runset_path=${src_dir_mid}_err_amt_2nd_try
    echo "runset_path = $runset_path"
src_file=${runset_path}/$rs_name.csv
    echo "src_file = $src_file"
echo
echo "----->  Build $rs_name.csv file using find, etc."

#time ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    $rs_name    $runset_path

echo
echo "    Add $rs_name.csv contents to combined output file."
grep    -v    UUID    $src_file    >>    $target_file

# #===============================================================================
#
