
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

SHARE is a shiny application for the visualisation and analysis of pre-processed RNA-seq data. Given data processed with GENOM'IC's pipeline, it is able to produce customasible plots, such as PCA plots, Volcano plots and Heatmaps. It also allowes one to explore the counts and results table interactively.

SHARE is an exploration tool, it relies on data already processed from raw reads, following a STAR -> RSEM -> DESeq2 pipeline, with gene name annotation with Biomart.


<a id="org7ffbcd4"></a>

# Usage


<a id="org520eba4"></a>

## Installation

Installation is only available through docker for now.

-   First, install Docker : <https://docs.docker.com/get-docker/>
-   Then run:
    
        docker pull paulimer/share
        docker run -dp 80:3838 --rm paulimer/share
-   Access the app through your usual web navigator, at the adress <http://localhost/>.
-   Load your `result.Rdata` file in the **Data** tab
-   Explore!


<a id="orga7464b0"></a>

## Using the app

Follow the instructions in the app. Load the `results.Rdata` file provided by GENOM'IC in the **Data** tab, and then proceed to explore the dataset and produce your figures!


<a id="org453ed4a"></a>

## Demo data

To experiment with the app, you can use the `demo_data.Rdata` file, in the root directory. Just load it in the **Data** tab, and explore!


<a id="org22656d3"></a>

# Contact

If you have any comment, suggestion or question, feel free to post an issue.


<a id="orgd45dd1a"></a>

# Acknowledgement

Main contributors to this project are Paul Etheimer (Developpement, documentation, testing) under the supervision of Juliette Hamroune (Initiator, evaluation, feature suggestions).

