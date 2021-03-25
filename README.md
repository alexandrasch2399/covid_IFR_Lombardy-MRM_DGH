# covid_IFR_Lombardy-MRM_DGH
Estimating the infection fatality rate of Covid-19 using demographics data and deaths records from Italy's hardest hit area

All code within this repository is published under the MIT license (see LICENSE file).  Original code from https://github.com/gianlucaRinaldi/covid_IFR_Lombardy Copyright (c) 2020 Gianluca Rinaldi. This code was published in relation to https://doi.org/10.1101/2020.04.18.20070912 "An empirical estimate of the infection fatality rate of COVID-19 from the first Italian outbreak" by Gianluca Rinaldi and Matteo Paradisi. 

Additions and alterations made by Matthew R. MacLeod are Copyright (c) Her Majesty the Queen in Right of Canada, Department of National Defence, 2021. The additional code and alterations are published in support of "The Impact of Age Demographics on Interpreting and Applying
Population-Wide Infection Fatality Rates for COVID-19" by Matthew R. MacLeod and D. Gregory Hunter, pending publication.

To replicate the exhibits in the Rinaldi and Paradisi paper, please set the working directory of the local downloaded project in "main.R" and then run it

If using a Mac, rjags installation might not work immediately: please download and install JAGS from 
https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Mac%20OS%20X/ 
and then run "main.R" again

The input for tables 1 and 2 is saved in the workspace as data.table objects table1Data and table2Data
