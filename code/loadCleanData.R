# MIT License
# 
# Original work Copyright (c) 2020 Gianluca Rinaldi
# Modified work Copyright (c) 2021 Her Majesty the Queen in Right of Canada, Department of National Defence
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


#################################################################
# Load and clean data
#################################################################
relevantTowns <- c("Bertonico", "Codogno", "Castiglione d'Adda", "Casalpusterlengo", "Fombio", "Maleo", "Somaglia", "Terranova dei Passerini", "Castelgerundo", "San Fiorano")
excludedTowns <- c("Bertonico") # Only Bertonico doesn't have death data for 2020 as of May 6 2020

# This is only to reconstruct comune_giorno_relevant.csv from the original data (not uploaded to github for size limitations)
# MRM - comment out below two lines that refer to a file that is not available
#deathsData <- fread("data/comuni_giornaliero-decessiUpTo15April.csv") 
#relevantTownsDeathsData <- deathsData[NOME_COMUNE %in% relevantTowns, ]
#write.csv(relevantTownsDeathsData, file = "data/comune_giorno_relevant.csv")

# Load deaths data
deathsData <- fread("data/comune_giorno_relevant.csv")

# Construct total deaths by age group, year, and town for the covid affected period
deathsData[, ageRange := cut(CL_ETA, c(0, 4, 8, 10, 12, 14, 16, 22), labels = c("0-20", "21-40", "41-50", "51-60", "61-70","71-80", "81+"), include.lowest = T)]

# Remove dates after april 30 because no 2020 data
deathsData <- deathsData[GE <= 430,]
deathsData[, covidAffectedPeriod := (GE %in% 221:430)]

# Demographics data
demographicData <- fread(input = "data/Lodi_2015_2019.csv")
# Castelgerundo was formed by merging Cavacurta and Camairago in 2018 the input data is already adjusted for this

# Display total population in the area and total population in towns without available deaths data
demographicData[Denominazione %in% relevantTowns, sum(tot2019)]
demographicData[Denominazione %in% excludedTowns, sum(tot2019)]

# We have deaths data for 9 towns
deathsData <- deathsData[!(NOME_COMUNE %in% excludedTowns), ]
demographicData <- demographicData[(Denominazione %in% setdiff(relevantTowns, excludedTowns))] 

# Look at total deaths by age cathegories to have an idea of variance
deathsData[, totDeathsDay15 := sum(as.numeric(T_15)), by = c("GE")]
deathsData[, totDeathsDay16 := sum(as.numeric(T_16)), by = c("GE")]
deathsData[, totDeathsDay17 := sum(as.numeric(T_17)), by = c("GE")]
deathsData[, totDeathsDay18 := sum(as.numeric(T_18)), by = c("GE")]
deathsData[, totDeathsDay19 := sum(as.numeric(T_19)), by = c("GE")]
deathsData[, totDeathsDay20 := sum(as.numeric(T_20)), by = c("GE")]

deathsData[covidAffectedPeriod == T, totDeathsTown15 := sum(as.numeric(T_15)), by = c("ageRange", "NOME_COMUNE")]
deathsData[covidAffectedPeriod == T, totDeathsTown16 := sum(as.numeric(T_16)), by = c("ageRange", "NOME_COMUNE")]
deathsData[covidAffectedPeriod == T, totDeathsTown17 := sum(as.numeric(T_17)), by = c("ageRange", "NOME_COMUNE")]
deathsData[covidAffectedPeriod == T, totDeathsTown18 := sum(as.numeric(T_18)), by = c("ageRange", "NOME_COMUNE")]
deathsData[covidAffectedPeriod == T, totDeathsTown19 := sum(as.numeric(T_19)), by = c("ageRange", "NOME_COMUNE")]
deathsData[covidAffectedPeriod == T, totDeathsTown20 := sum(as.numeric(T_20)), by = c("ageRange", "NOME_COMUNE")]

allDeathsByYearTown <- unique(deathsData[covidAffectedPeriod == T, c("NOME_COMUNE", "ageRange", "totDeathsTown15", "totDeathsTown16", "totDeathsTown17", "totDeathsTown18", "totDeathsTown19", "totDeathsTown20")])

deathsData[, meanDailyDeathsBeforeAll := (totDeathsDay15 + totDeathsDay16 + totDeathsDay17 + totDeathsDay18 + totDeathsDay19)/5, by = GE]
deathsData[, deaths2020All := totDeathsDay20]

plotDeaths <- unique(deathsData[, c("GE", "meanDailyDeathsBeforeAll", "deaths2020All")])
plotDeaths[, month := substr(as.character(GE), 1,1)]
plotDeaths[, day := substr(as.character(GE), 2,3)]
plotDeaths[, date := as.Date(paste0(month, "/", day, "/2020"), format = "%m/%d/%Y")]

# Construct age range shares from 2019 
demographicData[, totalPopulationAgeRange := sum(tot2019), by = ageRange]	
demographicData[, totalPopulation := sum(tot2019)]	
demographicData[, ageRangeShare := totalPopulationAgeRange/totalPopulation, ]
