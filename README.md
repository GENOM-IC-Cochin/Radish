
# Table of Contents

-   [Introduction](#orge3da5dc)
-   [Usage](#org7ffbcd4)
    -   [Installation](#org520eba4)
    -   [Using the app](#orga7464b0)
    -   [Demo data](#org453ed4a)
-   [Contact](#org22656d3)
-   [Acknowledgement](#orgd45dd1a)



<a id="orge3da5dc"></a>

# Introduction

Radish is a shiny application for the visualisation and analysis of pre-processed RNA-seq data, specifically differential expression analysis. Given data processed with GENOM'IC's pipeline, it is able to produce customisable plots, such as PCA plots, Volcano plots and Heatmaps. It also allows one to explore the counts and results table interactively.

Radish is an exploration tool, it relies on data already processed from raw reads, following a STAR -> RSEM -> DESeq2 pipeline, with gene name annotation with Biomart.


<a id="org7ffbcd4"></a>

# Usage


<a id="org520eba4"></a>

## Installation

Installation is only available through docker for now.

-   First, install Docker : <https://docs.docker.com/get-docker/>
-   Then run (on Windows, use the command prompt):
    
        docker pull paulimer/radish
        docker run -dp 80:3838 --rm paulimer/radish
-   Access the app through your usual web navigator (Chromium-based, there is an issue with Firefox and the tutorial videos), at the adress <http://localhost/>.
-   Load your `result.rds` file in the **Data** tab
-   Explore!


<a id="orga7464b0"></a>

## Using the app

Follow the instructions in the app. Load the `results.rds` file provided by GENOM'IC in the **Data** tab, and then proceed to explore the dataset and produce your figures!


<a id="org453ed4a"></a>

## Demo data

To experiment with the app, you can use demo data kindly provided by Juliette Paillet (Kroemer team, UMRS1138), from [this article](https://doi.org/10.1084/jem.20200853 "Autoimmunity affecting the biliary tract fuels the immunosurveillance of cholangiocarcinoma") by Paillet et al. (raw data otherwise available from Gene Expression Omnibus, under accession no. [GSE180289](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE180289 "Gene Expression Omnibus")). It is accessible by a simple button press!


<a id="org22656d3"></a>

# Contact

If you have any comment, suggestion or question, feel free to post an issue.


<a id="orgd45dd1a"></a>

# Acknowledgement

Main contributors to this project are Paul Etheimer (Developpement, documentation, testing) under the supervision of Juliette Hamroune (Initiator, evaluation, feature suggestions).

