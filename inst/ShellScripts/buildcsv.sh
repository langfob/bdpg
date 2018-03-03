#!/bin/bash

#                                   buildcsv.sh

#   ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh

#  Cloned from ~/.../testgit.sh.  Not sure where that file came from...

#  Don't forget to make executable.
#  chmod u+x buildcsv.sh

echo "Usage: rsName tzarOutDir"
echo "    $0  Marxan_SA  ./24_easy.completedTzarEmulation"
echo
echo "Number of arguments = '$#'"
echo

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters"
fi

echo "Reserve selector name: '$1'"
echo

start_dir=`pwd`
echo "Starting from dir: '$start_dir'"
echo


#cd ~/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/24_easy.completedTzarEmulation
cd $2
moved_to_dir=`pwd`
echo "Now in working dir: '$moved_to_dir'"
echo

###  nohup    time    find    ./1956?_easy    -name    rsrun_results.csv    >    easy_rsrun_results_1956.txt    &
#nohup    time
#find    ./198??_easy    -name    rsrun_results.csv    >    easy_rsrun_results_198.txt
#find    ./198??_easy    -name    rsrun_results.csv    >    rsrun_results_paths.txt
#&

        #  Collect rsrun output lines into one file that has 1 copy of the header line for each of the 20 output lines.
find    .    -name    rsrun_results.csv    >    rsrun_results_full_paths.txt
#find    $2??_easy    -name    rsrun_results.csv    >    rsrun_results_paths.txt

grep    -h    $1    rsrun_results_full_paths.txt    >    $1.txt
#grep    -h    $1    rsrun_results_paths.txt    >    $1.txt

cat    $1.txt    |     xargs    -n1 cat    >    $1.csv.with.many.headers
wc $1.csv.with.many.headers

#         #  Strip out the extra header lines so that there is only 20 data lines plus 1 header line left.
grep    -i    UUID          $1.csv.with.many.headers     |     head    -n 1    > $1.csv
grep    -v    UUID    $1.csv.with.many.headers    >>    $1.csv
wc $1.csv


 # 1006  find    ./198??_easy    -name    rsrun_results.csv    >    easy_rsrun_results_198.txt
 # 1007  ls -l easy_rsrun_results_198.txt
 # 1008  wc easy_rsrun_results_198.txt
 # 1009  mv easy_rsrun_results_198.txt rsrun_results_paths.txt
 # 1010  grep    -h    Marxan_SA    rsrun_results_paths.txt    >    Marxan_SA.txt
 # 1011  cat    Marxan_SA.txt     |     xargs    -n1 cat    >    Marxan_SA.csv.with.many.headers
 # 1012  wc Marxan_SA.csv.with.many.headers
 # 1013  grep    -i    UUID          Marxan_SA.csv.with.many.headers    |     head    -n 1    > Marxan_SA.csv
 # 1014  grep    -v    UUID          Marxan_SA.csv.with.many.headers   >>    Marxan_SA.csv
 # 1015  wc Marxan_SA.csv
 # 1016  history
 # 1017  mkdir aggregated-2018-03-02-bdpg-100-runs-easy-198xx
 # 1018  pwd
 # 1019  mv rsrun_results_paths.txt aggregated-2018-03-02-bdpg-100-runs-easy-198xx
 # 1020  ls -l Marxan_SA.*
 # 1021  mv Marxan_SA.* aggregated-2018-03-02-bdpg-100-runs-easy-198xx

# > pwd
# /Users/bill/Downloads/bdpgout
# ____________________________________________
# > ls -l
# total 1616
# drwxr-xr-x  11 bill  staff     352 26 Feb 10:41 195xx
# drwxr-xr-x  10 bill  staff     320 26 Feb 11:53 197xx
# -rw-r--r--@  1 bill  staff   16080  1 Mar 13:59 bdpg_output_analysis.Rmd
# -rw-r--r--   1 bill  staff  753180  1 Mar 13:59 bdpg_output_analysis.html
# -rw-r--r--@  1 bill  staff   15476 25 Feb 17:27 marxan_analysis.Rmd
# ____________________________________________
# >
# ____________________________________________
# > mkdir 198xx
# ____________________________________________
# > cd 198xx
# ____________________________________________
# >
# > scp    rdv@glass.eres.rmit.edu.au:tzar_output/aggregated-2018-03-02-bdpg-100-runs-easy-198xx/*    .
# Marxan_SA.csv                                                                                             100% 4034KB 596.7KB/s   00:06
# Marxan_SA.csv.with.many.headers                                                                           100%   14MB 854.9KB/s   00:16
# Marxan_SA.txt                                                                                             100%  184KB  44.6KB/s   00:04
# rsrun_results_paths.txt                                                                                   100% 1303KB 735.1KB/s   00:01
# ____________________________________________
