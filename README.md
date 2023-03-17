# Basic human values and the adoption of cryptocurrency

This manuscript explores the associations between basic human values in the Theory of Basic Human Values by Shalom H. Schwartz and 
cryptocurrency at levels of awareness, intention to buy and behavior.

# Authors 

1) Adrian Stanciu
- https://orcid.org/0000-0001-8149-7829
- GESIS-Leibniz Institute for the Social Sciences, Mannheim, Germany

2) Mariana Bernardes
- University of Bremen, Bremen, Germany

3) Melanie V. Partsch 
- https://orcid.org/0000-0002-0216-0492
- GESIS-Leibniz Institute for the Social Sciences, Mannheim, Germany

4) Clemens M. Lechner
- https://orcid.org/0000-0003-3053-8701
- GESIS-Leibniz Institute for the Social Sciences, Mannheim, Germany

# Replicability

This repository contains raw data and scripts that can be used to replicate the reported results. 

## Data

The folder _data_ contains the raw data in spss format .sav "data.sav", as well as .Rdata objects for descriptive statistics and the prepared data for analyses. 
Also here are provided the case ids in excel format .xlsx that we identified as having straightlining (identical answers). More details in the manuscript. 

## Scripts

The folder _scripts_ contains three scripts that can be used to replicate our results entirely. 

- 00_data prep = contains the data prepration steps
- 01_descrpt = contains the codes for arriving at the descriptive stats 
- 02_logistic_imputeddata = contains the missing data imputations as well as the reported logistic regressions

The folder _scripts_ contains also a script called _helper functions_ which contains a series of functions that are used throughout the analyses. 
These functions are called in the scripts 00_ to 02_ (see above). 

## Plots

The folder _img_ contains plots generated with the script mentioned above. Running the scripts above will generate "fresh" plots. 
For that, one needs to activate the writting of these plots (see in those scripts the dev.off() ).
