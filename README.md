# Replication: The Organization of Political Belief Networks

This repository provides the replication files for the paper *The Organization of Political Belief Networks: A Cross-Country Analysis*, published at [*Social Science Research*](https://www.sciencedirect.com/science/article/abs/pii/S0049089X22000485) journal.

The pre-print version of the paper is stored at [SocArXiv](https://osf.io/preprints/socarxiv/6s9rk/).

## Notes on Replication

The paper uses the joint dataset of World Values Survey and European Values Study, which covers the periods of 2017 and 2020. The datafiles and the corresponding codebooks and questionnaires can be found [here](https://www.worldvaluessurvey.org/WVSEVSjoint2017.jsp).

There are two macro files, [issue macro](./data/addsheets_issuemacro.csv) and [country macro](./data/addsheets_countrymacro.csv). The former lists the opinion items used in the analyses, while the latter provides information about the countries. Most notably, it features various additional variables gathered from outside sources: (a) the variables from V-Dem indices from 2000 to 2017 (see this [link](https://www.v-dem.net/en/data/data/) for the data file), and (b) the effective number of parties. I benefit from Michael Gallagher's files ([here](https://www.tcd.ie/Political_Science/people/michael_gallagher/ElSystems/index.php)) for this.

The article used the second version of the joint WVS/EVS data, stored [here](https://osf.io/rypq4/). Since the publication of the article, the WVS/EVS published a fourth version of the dataset, increasing the number of countries.

## Updates in the Replication Package

_2023, November_: In order to strengthen the future replicability of the study, I updated this folder by reorganizing the folder structure and adding {{renv}} configurations for package compatibility.
