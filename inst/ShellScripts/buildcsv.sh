#!/bin/bash

#                                   buildcsv.sh

#  Cloned from ~/.../testgit.sh.  Not sure where that file came from...

#  Don't forget to make executable.
#  chmod u+x buildcsv.sh

echo "Usage: rsName tzarOutDir"
echo "    $0  Marxan_SA  ~/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/24_easy.completedTzarEmulation"
echo
echo "Number of arguments = '$#'"
echo

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters"
fi

echo "Reserve selector name in dollar 1: '$1'"
echo

start_dir=`pwd`
echo "Starting from dir: '$start_dir'"
echo


#cd ~/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/24_easy.completedTzarEmulation
cd $2
moved_to_dir=`pwd`
echo "Now in working dir: '$moved_to_dir'"
echo

        #  Collect rsrun output lines into one file that has 1 copy of the header line for each of the 20 output lines.
find    .    -name    rsrun_results.csv    >    rsrun_results_for_20_variants.txt
grep    -h    $1    rsrun_results_for_20_variants.txt    >    $1.txt
cat    $1.txt    |     xargs    -n1 cat    >    $1.csv.with.many.headers
wc $1.csv.with.many.headers

#         #  Strip out the extra header lines so that there is only 20 data lines plus 1 header line left.
grep    -i    UUID          $1.csv.with.many.headers     |     head    -n 1    > $1.csv
grep    -v    UUID    $1.csv.with.many.headers    >>    $1.csv
wc $1.csv




repodir=$3
#repodir="/Users/bill/D/Projects/Dummy"
echo "repodir = '$repodir'"
echo
cd $repodir
echo "Now in repo dir: '`pwd`'"
echo
ls -l
echo

reponame=$2
#reponame="Dummy"
echo "reponame = '$reponame'"
echo

username=$1
#username=langfob
echo "username is '$username'"
echo

github_PAT_token=$GITHUB_PAT
echo "github_PAT_token = '$github_PAT_token'"
echo


#############
#  Nothing here says private vs. public repository.
#  How to do that?
#############

##git add -A && git commit -m "Your Message"
git add -A
git commit -m "Initial commit"

echo "Creating Github repository '$reponame' ..."
curl -u "$username:$github_PAT_token" https://api.github.com/user/repos -d '{"name":"'$reponame'"}' > /dev/null 2>&1
echo " done."

echo "Pushing local code to remote ..."
echo "    First adding remote repository (i.e., github repository)..."
git remote add origin git@github.com:$username/$reponame.git > /dev/null 2>&1
echo "   done."
echo
echo "    Now pushing to that github remote repository..."
git push -u origin master > /dev/null 2>&1
echo "    done."



echo
cd $start_dir
echo "Moved back to starting dir: '`pwd`'"

echo
echo "all done now..."


#  From https://www.viget.com/articles/create-a-github-repo-from-the-command-line

github-create() {
 reponame=$1

 dir_name=`basename $(pwd)`

 if [ "$reponame" = "" ]; then
 echo "Repo name (hit enter to use '$dir_name')?"
 read reponame
 fi

 if [ "$reponame" = "" ]; then
 reponame=$dir_name
 fi

 username=`git config github.user`
 if [ "$username" = "" ]; then
 echo "No user name given"
 invalid_credentials=1
 fi

 github_PAT_token=`git config github.github_PAT_token`
 if [ "$github_PAT_token" = "" ]; then
 echo "Could not find github_PAT_token"
 invalid_credentials=1
 fi

 if [ "$invalid_credentials" == "1" ]; then
 return 1
 fi





 # create and push new repository
echo "Starting local git repository ..."
#git init
#git add .
#git commit -m "$message"
#  if [ $? -gt 1 ]; then echo "`basename $0`: could not commit local repository"; exit 8; fi

echo "Creating Github repository '$reponame' ..."
curl -s -u "$username:$github_PAT_token" https://$hostapi/user/repos -d '{"name":"'$reponame'"}' > /dev/null 2>&1
  if [ $? -ne 0 ]; then echo "`basename $0`: curl could not perform POST"; exit 5; fi

if $verbose; then echo "Pushing local code to remote server ..."; fi
git remote add origin git@$hostname:$username/$reponame.git 2>&1
  if [ $? -ne 0 ]; then echo "`basename $0`: could not add remote repository"; exit 8; fi
git push -u origin master 2>&1
  if [ $? -ne 0 ]; then echo "`basename $0`: could not push to new remote repository"; exit 8; fi

if $verbose; then echo "`basename $0`: finished"; fi
}
