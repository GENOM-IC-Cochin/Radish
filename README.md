
# Table of Contents

-   [Introduction](#org95b4e28)
-   [Usage](#org549a61c)
    -   [Installation](#org235841f)
    -   [Using the app](#orge273bec)
-   [Contact](#org316bf16)
-   [Acknowledgement](#org6b4e925)



<a id="org95b4e28"></a>

# Introduction

SHARE is a shiny application for the visualisation and analysis of pre-processed RNA-seq data. Given data processed with GENOM'IC's pipeline, it is able to produce customasible plots, such as PCA plots, Volcano plots and Heatmaps. It also allowes one to explore the counts and results table interactively.

SHARE is an exploration tool, it relies on data already processed from raw reads, following a STAR -> RSEM -> DESeq2 pipeline, with gene name annotation with Biomart.


<a id="org549a61c"></a>

# Usage


<a id="org235841f"></a>

## Installation

Installation is only available through docker for now.

-   First, install Docker : <https://docs.docker.com/get-docker/>
-   Then run:
    
        docker pull paulimer/share
        docker run -dp 80:3838 --rm paulimer/share
-   Finally, access the app through your usual web navigator, at the adress <http://localhost/>.


<a id="orge273bec"></a>

## Using the app

Follow the instructions in the app. Load the results.RData file provided by GENOM'IC in the Data tab, and then proceed to explore the dataset and produce your figures!


<a id="org316bf16"></a>

# Contact

If you have any comment, suggestion or question, feel free to post an issue.


<a id="org6b4e925"></a>

# Acknowledgement

Main contributors to this project are Paul Etheimer (Developpement, documentation, testing) under the supervision of Juliette Hamroune (Initiator, evaluation, feature suggestions).

