# bdpg (development version)

## Bug fixes

Fixed three small bugs that had no effect on correctness of results but fixing them means previous experimental results will no longer be exactly reproduced by the changed code.  

* double_diff penalty calculation in EF() - bug fix

double_diff penalty calculation in EF() had absolute value parentheses in wrong place.  In very rare circumstances, this could have caused the optimizer to try to make the number of species on exactly 2 PUs in the lognormal be one less than the number in the correct solution.  Fixed in commit 8a9cf3b8.  

* solution fraction of landscape - bug fix

Was using full Base node set size to compute total wrapped landscape size instead of using just the size of the correct solution node set.  Consequently, solution set fraction of the full landscape was a little smaller than what it was supposed to be.   Fixed in commit 58418956.

* k__arity was set to 1 instead of 2 - bug fix

Fixed issue #61.  Added missing k__arity exponent to equation for target_num_links_between_2_groups_per_round.  Did not cause results to be incorrect but caused smaller range of problem difficulty than it was supposed to.  Fixed in commit e04483aa.  Changed default value from 1 to 2 in commit 31f018dc.  This means that to reproduce the behavior of bdpg 3.1.0 and earlier, you have to set parameters$k__arity to 1, though you probably won't want to reproduce that behavior since it's incorrect.

# bdpg 3.1.0  (2021-03-03)

## Release 3.1.0

* Close to what was used in experiments for bdpg papers

This represents the bdpg code that is fairly close to what was used in experiments for bdpg papers. Some small bugs have come up in the course of writing the papers and this release captures the code before fixing those bugs since these fixes were not there in the experiments. Even this state of the code is not exactly what was used in the papers though, since other bug fixes and small features were adding as they were discovered during experimentation.

