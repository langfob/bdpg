<!-- README.md is generated from README.Rmd. Please edit that file -->
bdpg
====

This package provides functions for generating biodiversity problems (particularly reserve selection) with known solutions and testing algorithms for attempting to generate optimal solutions.

Installation
------------

You can install bdpg from github using devtools::install\_github().

However, the repo is currently private, which means that you have to be listed on github as a collaborator and pass a github personal access token to see it. If you don't have a token yet, you can get one by following the instructions at:

[Personal access tokens](https://github.com/settings/tokens)

or

[Creating a personal access token for the command line](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)

You can store this token in an environment variable called GITHUB\_PAT on your machine. This allows future use of the personal access token without having to hard-code it into functions calls. You can then retrieve it in a devtools::install\_github() call's "auth" argument by using devtools::github\_pat() as shown below.

While the repo is still private, the devtools call looks like:

``` r
devtools::install_github ("langfob/bdpg", auth_token = "INSERT YOUR TOKEN STRING HERE")
```

or

``` r
devtools::install_github ("langfob/bdpg", auth_token = devtools::github_pat())
```

If and when the repo becomes public, the call is simply:

``` r
devtools::install_github ("langfob/bdpg")
```

So, assuming that the repo is still private and you've stored a token in the GITHUB\_PAT environment variable, you can download the package as follows:

``` r
install.packages ("devtools")    #  if devtools not installed already
devtools::install_github ("langfob/bdpg", auth_token = devtools::github_pat())
```

Note that install\_github() also has an optional "ref" argument that allows you to specify a particular 'commit, tag, or branch name, or a call to github\_pull. Defaults to "master"'.

Example
-------

This is a basic example which shows you how to solve a common problem:
- The mainline code in the bdpgxupaper makes use of the bdpg library. To call that code using tzar emulation:

``` r
library(bdpgxupaper)
runtip()
```

Overview (still boilerplate)
----------------------------

-   (An overview that describes the main components of the package. For more complex packages, this will point to vignettes for more details.)
