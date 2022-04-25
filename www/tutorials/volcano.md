The volcano plot is the most used representation to display differential expression results. It plots $-log_{10}(padj)$ against $log_{2}(FoldChange)$. Contrary to the MA-plot, it does not display genes lacking a *p-value*, genes filtered out by DESeq2 for different reasons.

This app allows you to customize its appearance, changing colors, labels, as well as "zooming" in the plot by squishing extreme values to the border. It also allows one to customize the thresholds in terms of Fold change or *p-value*. Gene labelling is also available.
A tab within this tab shows an interactive version of this plot. It is quite (very) slow, as it needs to plot around 15 000 points. If you are patient enough however, it is another way (outside of the table) to explore the differentially expressed genes.

As for all the other plots, three formats are available for download : pdf and svg which are vectorized (made of paths and objects), and png which is rasterized (made of pixels). Svg is particularly useful, as it allows lossless, element-wise modification in free software such as Inkscape (tutorial [here](https://inkscape.org/learn/tutorials/)). It can in turn export to png.

A short video demonstrating these features :
