
# Table of Contents

-   [Introduction](#orgdb693bb)
-   [Usage](#org2f01349)
    -   [Installation](#org225762c)
    -   [Using the app](#org8b5da74)
-   [Contact](#org2db273a)
-   [Acknowledgement](#orga464028)



<a id="orgdb693bb"></a>

# Introduction

SHARE is a shiny application for the visualisation and analysis of pre-processed RNA-seq data. Given data processed with GENOM'IC's pipeline, it is able to produce customasible plots, such as PCA plots, Volcano plots and Heatmaps. It also allowes one to explore the counts and results table interactively.

SHARE is an exploration tool, it relies on data already processed from raw reads, following a STAR -> RSEM -> DESeq2 pipeline, with gene name annotation with Biomart.


<a id="org2f01349"></a>

# Usage


<a id="org225762c"></a>

## Installation

Installation is only available through docker for now.

-   First, install Docker : <https://docs.docker.com/get-docker/>
-   Then run
    
        sudo docker pull paulimer/share
        sudo docker run -dp 80:3838 --rm share
-   Finally, access the app through your usual web navigator, at the adress <http://localhost/>.


<a id="org8b5da74"></a>

## Using the app

Follow the instructions in the app. Load the results.RData file provided by GENOM'IC in the Data tab, and then proceed to explore the dataset and produce your figures!


<a id="org2db273a"></a>

# Contact

If you have any comment, suggestion or question, feel free to post an issue.


<a id="orga464028"></a>

# Acknowledgement

Main contributors to this project are Paul Etheimer (Developpement, documentation, testing) under the supervision of Juliette Hamroune (Initiator, evaluation, feature suggestions).

