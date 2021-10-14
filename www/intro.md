**Welcome to SHARE!**

SHARE is a GENOM'IC tool for RNA-seq exploration. It uses data processed with GENOM'IC's own RNA-seq pipeline, and allows you to interactively explore and visualise the data.

**Usage :**

-   In the **Input** tab, load the `results.RData` file, provided in the `NGSXX-XX_Resultats` folder given to you by GENOM'IC.

-   Assess the samples' quality in the **PCA** tab. You can remove a sample and the PCA plot gets redrawn, if you want to see if a specific sample might be an outlier. *However*, this does not impact subsequent figures and the analysis on the whole : if you want to re-analyse the data, while excluding a sample, please [contact us](mailto:ngs.u1016@inserm.fr).

-   Set the **contrast** you want to study : in the top left bar, you can select the contrast of conditions you want to study, for instance *Treatment\_vs\_Control*.

-   Explore the counts and fold changes (for the current contrast) in the **Interactive table** tab. You can search for your gene(s) of interest, by Ensembl ID or name. Log fold changes (and associated standard error), adjusted pvalues, expression means, normalised expression values for each sample, gene name and description (if available) are all accessible. By default, only differentially expressed genes are shown and searchable, but you can change the cutoffs! For instance, if you want to see all genes, you can set the pvalue cutoff at 1 and the logFoldChange cutoff at 0.

-   You can then visualise the results as a customisable **Volcano plot**. Many settings are available, most self-descriptive. Notable ones are :
    -   Maximum values of axis : allows you to "zoom in" the plot, squishing extreme values to a dashed line.
    -   Selecting genes to highlight : labels dots on the plot, with either Ensembl IDs or gene name. If a gene has an name, it will be the label chosen, even if you have selected its ID. You can also vary the size of the labels, in order to fit more of them.

-   A **Heatmap** is also available. You need to click on the *Draw Heatmap* button to visualise it. The values used for the plot are normalised counts values, but the ones displayed are Z-scores, to improve color visualisation and overall aesthetics. The clustering is done before the scaling. This means however that rows are not comparable, in terms of value at least.
    Two main modes are possible :
    -   Represent the top differentially expressed genes for the current contrast. You can choose the number. If you want to plot all the differentially expressed genes at a given cutoff, you can just match the number of genes in the **Interactive table**.
    -   Represent a choice of genes, across the conditions that interset you. As with the volcano plot, you can choose by IDs or names, but names take the precedence.

