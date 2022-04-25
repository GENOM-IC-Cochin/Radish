Alongside the volcano plot, the heatmap is the other heavyweight of differential expression representation. At its core only a colorized table, it summarizes the information in a more accessible way. It is heavily dependent on two characteristics : the ordering of columns and rows, as well as the color mapping to values.
The first one is almost always done with a hierarchical clustering algorithm, at least for the rows. This allows to bring similar genes/samples closer. Here, you can choose whether you want to cluster the columns. The rows are always clustered based on normalized expression (prior to z-score calculation).
The color mapping is done after [Z-scoring](https://en.wikipedia.org/wiki/Standard_score). This means that cells with the same color, across different lines *do not have the same underlying expression values*.

The app allows you to plot up to the top 2000 genes at given *p-values* and LFC thresholds.
In an alternative mode, it also allows you to plot represent selected genes, across selected conditions. 

A short video showing these features :
