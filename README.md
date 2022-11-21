# Replication materials for *Characterizing patterns in police stops by race in Minneapolis from 2016 to 2021* by Onookome-Okome et al., 2022

This repository contains all materials needed to replicate the findings presented in 'Characterizing patterns in police stops by race in Minneapolis from 2016 to 2021' published in the peer-reviewed [*Journal of Ethnicity in Criminal Justice* on 15 June 2022](https://www.tandfonline.com/doi/abs/10.1080/15377938.2022.2086192).

# Abstract

The murder of George Floyd centered Minneapolis, Minnesota, in conversations on racial injustice in the US. We leverage open data from the Minneapolis Police Department to analyze individual, geographic, and temporal patterns in more than 170,000 police stops since 2016. We evaluate person and vehicle searches at the individual level by race using generalized estimating equations with neighborhood clustering, directly addressing neighborhood differences in police activity. Minneapolis exhibits clear patterns of disproportionate policing by race, wherein Black people are searched at higher rates compared to White people. Temporal visualizations indicate that police stops declined following the murder of George Floyd. This analysis provides contemporary evidence on the state of policing for a major metropolitan area in the United States.

# File explanation 

DATASETS

- `Police_Stop_Data.csv`: raw police stop data downloaded from the Minneapolis Open Data portal
downloaded in Spring 2021. Link to original dataset: https://opendata.minneapolismn.gov/datasets/police-stop-data/explore?showTable=true
This dataset is processed in 1_GEO_DATA_PREP.R

- `HennepinCountyInt2017through2019Counts.gpkg`: processed geospatial dataset that provides
counts of police-citizen interactions at the Census Tract level from 2017 through 2019. This data is
created in 1_GEO_DATA_PREP.R and visualized in 2_GEO_RATE_VIZ.R

- `MplsCleanedThrough2021.csv`: a cleaned version of the raw police stop data that is used for
regression modeling. This is used in GEE_Full_Analysis.R

ANALYSIS SCRIPTS

- `1_GEO_DATA_PREP.R`: R script that intakes and processes the raw police stop data. Provides
tabular counts found in the manuscript.
- `2_GEO_RATE_VIZ.R`: R script that intakes the .gpkg dataset to produce the spatio-temporal visualizations
found in the manuscript.
- `3_TEMPORAL_VIZ.R`: R script that intakes MplsCleanedThrough2021.csv to produce the temporal visualization of police stops
found in the manuscript.
- `4_GEE_Full_Analysis.R`: R script that intakes MplsCleanedThrough2021.csv to produce the regression results found in the manuscript.

# Author team and contact information

1. Tuviere Onookome-Okome, MSc (tuviere.onookome-okome[at]mail.mcgill.ca)
2. Jonah Gorondensky, MSc (jonah.gorodensky[at]mail.mcgill.ca )
3. Eric Rose, PhD (eric.rose2[at]mcgill.ca)
4. Jeffery Sauer, PhD (jcsauer[at]terpmail.umd.edu)
5. Kristian Lum, PhD (kristianlum[at]gmail.com)
6. Erica E.M. Moodie, PhD (erica.moodie[at]mcgill.ca ) 

*Last updated: 21 November 2022*
