### Columns
By default, only certain columns are shown : 
- ENSEMBL Gene ID, 
- mean of normalized expression across all the samples, the $log_{2}(FoldChange)$,
- the *p-value* adjusted for multiple comparison with the Benjamini & Hochberg method (no value indicates that either there was a count outlier, or that the gene was independently filtered out as it had a low chance of being significant, or that expression was null),
- name of the gene
- Description of the genes according to quoted sources.

But other columns are hidden in the **Column visibility** dropdown, such as stats of the statistical analysis (standard error, Wald statistic, raw *p-value*), as well as normalized counts for each sample.
The following video displays those possibilities :
