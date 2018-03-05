#!/bin/bash

#                                   combine_csv.sh

start_dir=`pwd`
echo "Starting from dir: '$start_dir'"
echo

echo "dollar 1 = $1"

rs_name=$1
echo "Reserve selector name: '$rs_name'"
echo

target_dir="/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/"
#base_out_dir="/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/"
#target_dir=$base_out_dir$rs_name
echo $target_dir
cd $target_dir
new_dir=`pwd`
echo "Now in dir: '$new_dir'"
echo

target_file=${rs_name}.combined_results.csv
echo "target_file = $target_file"

src_dir_start="/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_"
echo "src_dir_start = $src_dir_start"

#----------  02

echo
echo "Add 02 errors"
src_dir_mid=${src_dir_start}02
src_file=${src_dir_mid}_err_amt/default_runset/$rs_name.csv
    echo "src_file = $src_file"
grep    -i    UUID    $src_file    |     head    -n 1    > $target_file
grep    -v    UUID    $src_file    >>    $target_file

#----------  05

# echo
# echo "Add 05 errors"
# src_dir_mid=${src_dir_start}05
# src_file=${src_dir_mid}_err_amt/default_runset/$rs_name.csv
#     echo "src_file = $src_file"
# grep    -v    UUID    $src_file    >>    $target_file
#
# #----------  075
#
# echo
# echo "Add 075 errors"
# src_dir_mid=${src_dir_start}075
# src_file=${src_dir_mid}_err_amt/default_runset/$rs_name.csv
#     echo "src_file = $src_file"
# grep    -v    UUID    $src_file    >>    $target_file
#
# #----------  10
#
# echo
# echo "Add 10 errors"
# src_dir_mid=${src_dir_start}10
# src_file=${src_dir_mid}_err_amt/default_runset/$rs_name.csv
#     echo "src_file = $src_file"
# grep    -v    UUID    $src_file    >>    $target_file
