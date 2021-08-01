# Replication: The Organization of Political Belief Networks

This repository provides the replication files for the paper *The Organization of Political Belief Networks: A Cross-Country Analysis*, published in the *X* journal.

The paper uses the joint dataset of World Values Survey and European Values Study, which covers the periods of 2017 and 2020. The datafiles and the corresponding codebooks and questionnaires can be found [here](https://www.worldvaluessurvey.org/WVSEVSjoint2017.jsp).

There are two macro files, [issue macro](docs/addsheets_issuemacro.csv.md) and [country macro](docs/addsheets_countrymacro.csv). The former lists the opinion items used in the analyses, while the latter provides information about the countries. Most notably, it features various additional variables gathered from outside sources: (a) the variables from V-Dem indices from 2000 to 2017 (see this [link](https://www.v-dem.net/en/data/data/) for the data), and (b) the effective number of parties. I mostly used Michael Gallagher's files ([here](https://www.tcd.ie/Political_Science/people/michael_gallagher/ElSystems/index.php)) for this, though I hand-calculated some of the missing countries.
