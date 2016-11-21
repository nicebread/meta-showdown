# meta-showdown

## Documentation about the most relevant result files

### Unaggregated data files
- res.wide.RData: This file contains the most fine-grained results of all meta-analytic (MA) techniques; each row is one MA technique for one simulated data set. No filters have been applied.
- res.wide.red.RData: A reduced version of res.wide. All further summaries have been computed based on this data set. Two filters have been applied:
	- Methods c("PET.rma", "PEESE.rma", "PETPEESE.rma", "pcurve.hack") have been removed
	- MAs with less than 4 significant studies in p-curve and p-uniform have been removed
- res.hyp: A reduced version of res.wide.red which only contains the p-values of each method in order to evaluate the hypothesis test.

### Aggregated data files
- summ.RData: This is the summary
