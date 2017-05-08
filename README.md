# meta-showdown

## Documentation about the most relevant result files

### Unaggregated data files (in /dataFiles)
- res.final.RData: This file contains the most fine-grained results of all meta-analytic (MA) techniques in long format; each row is one result of one MA technique for one simulated data set. No filters have been applied.
- res.wide.RData: This file contains the most fine-grained results of all meta-analytic (MA) techniques; each row is one MA technique for one simulated data set. No filters have been applied.
- res.wide.red.RData: A reduced version of res.wide. All further summaries have been computed based on this data set. Three filters have been applied:
	- MAs with less than 4 significant studies in p-curve and p-uniform have been removed for these two methods
	- Ignore 3PSM when it doesn't provide a p-value
	- Ignore p-uniform when it doesn't provide a lower CI (very rare cases)

### Aggregated data files (in /dataFiles)
- summ.RData / summ.csv: This is the summary file which contains ME, RMSE, coverage, etc. for each method and each condition.