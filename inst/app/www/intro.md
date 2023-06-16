**Welcome to Radish!**

Radish is a GENOM'IC tool for RNA-seq exploration. It uses data processed with GENOM'IC's own RNA-seq pipeline, and allows you to interactively explore and visualise the data. It is a projet in development, so do expect things to change, even things to break (I sure hope not!). Please do some sanity checks on the results and figures provided by this app !

If you encounter any issue, or have any suggestion, don't hesitate to [contact me](mailto:paul.etheimer@inserm.fr) (English or French)!


**Changes in version 1.1.0 !**

All filters (on *p values* or log Fold Change) on lists of Differentially Expressed Genes (DEGs) have been removed : as those list are False Discovery Rate (FDR)-corrected (the [wikipedia page](https://en.wikipedia.org/wiki/False_discovery_rate) is quite complete) for multiple testing, subsetting or intersecting those lists leads to losing the FDR control.


**Usage :**

- In the tutorial tab you can access a thorough tutorial on how to use the app.

-   In the **Data** tab, load the `results.rds` file, provided in the `NGSXX-XX_Resultats` folder given to you by GENOM'IC. Or try the app with some demo data kindly provided, by Juliette Paillet (Kroemer team, UMRS1138), from [this article](https://doi.org/10.1084/jem.20200853 "Autoimmunity affecting the biliary tract fuels the immunosurveillance of cholangiocarcinoma") by Paillet et al. (raw data otherwise available from the Gene Expression Omnibus, under accession no. [GSE180289](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE180289 "Gene Expression Omnibus")).
