# covid_IFR_Lombardy-MRM_DGH
Estimating the infection fatality rate of Covid-19 using demographics data and deaths records from Italy's hardest hit area

All code within this repository is published under the MIT license (see LICENSE file).  Original code from https://github.com/gianlucaRinaldi/covid_IFR_Lombardy Copyright (c) 2020 Gianluca Rinaldi. Additions and alterations made by Matthew R. MacLeod are Copyright (c) Her Majesty the Queen in Right of Canada, Department of National Defence, 2021.

To replicate the exhibits in the paper, please set the workind directory of local downloaded project in "main.R" and then run it

If using a Mac, rjags installation might not work immediately: please download and install JAGS from 
https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Mac%20OS%20X/ 
and then run "main.R" again

The input for tables 1 and 2 is saved in the workspace as data.table objects table1Data and table2Data
