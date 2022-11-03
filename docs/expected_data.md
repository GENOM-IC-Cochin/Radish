# Expected data structure
This app has certain expectations towards the input data. As shown by the (constant) variable of `module_input.R`: 
```R
expected_data <- c(
    "dataMerged",
    "all_results",
    "configuration",
    "contrasteList",
    "rld",
    "txi.rsem"
)
```
the app expects precisely 6 variables (in a list, in a rds file), named this precise way, and with coherent sample/condition naming across all. They respectively contain:
 **Important** : the input data frame must not contain empty strings, only NAs! For instance, it might be the case that there is no gene name : in this case, the corresponding cell must contain NA. Below a simple code snippet to replace empty strings with NAs across a data frame : 
 ```r
 dataframe_with_NAs <- dataframe_with_empty_str %>%
   mutate(across(everything(), na_if, ""))
 ```

### dataMerged
A normalized matrix of gene counts. Posesses a column containing the genes' names, the column's name containing the word "symbol".

### all_results
Named list (`list`)  (with names of the type `Condition_vs_Control`) of the results dataframes, containing DESeq2 results (columns `baseMean`, `padj`, `log2FoldChange`, etc...), with a `Row.names` column containing the ENSEMBL genes IDs, and a (single) column of genes names which title contains the word "symbol". Ideally, also contains normalised expression values by sample. Expects in particular : 
- `Row.names` (Ensembl Gene IDS)
- `baseMean`
- `log2FoldChange`
- `padj`
- `symbol` (or a single column containing the word symbol, corresponding to gene names)
- `description`

### configuration
A dataframe with a column named File, another Name with the names of the samples, and a last "Condition" one with the coressponding conditions. The app does not support multiple conditions (what would be a multi-variable DESeq2 analysis, `~ Treatment + Cell type` for instance). File is necessary as it is removed by named in some places.

### contrasteList
Never actually used, as the app rather looks at the names of the all_results list to find the contrasts.

### rld
DESeqTransform, result of one of the two (`rlog` or `vst`) variance stabilizing transformation.

### txi.rsem
Result of the (tx)import of the results table, thus non-normalized.

