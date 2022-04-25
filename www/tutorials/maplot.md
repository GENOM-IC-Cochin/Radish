MA-plot is a very useful plot : it shows the $log_{2}(FoldChange)$ against the mean normalized expression over (currently) all the samples. If you have many conditions, it has to be taken with a grain of salt, as the mean expression might not be relevant to the current comparison from which the $log_{2}(FoldChange)$ comes.
This representation, compared with the Volcano plot, gives some insight related to the expression of the genes, which is not always transpiring from the adjusted *p-values*. It also shows that highly expressed genes are usually not varying a lot across conditions, in terms of fold change, a fact illustrated with the usual cone shape of this plot.

This app allows you to customize the look of this plot, from colors to annotations. It also allows different threshold of significance.

As for all the other plots, three formats are available for download : pdf and svg which are vectorized (made of paths and objects), and png which is rasterized (made of pixels). Svg is particularly useful, as it allows lossless, element-wise modification in free software such as Inkscape (tutorial [here](https://inkscape.org/learn/tutorials/)). It can in turn export to png.

The following video shows some features : 
