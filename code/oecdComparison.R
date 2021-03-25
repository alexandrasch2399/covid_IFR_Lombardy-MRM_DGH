# MIT License
# 
# Original work Copyright (c) 2020 Gianluca Rinaldi
# Modified work Copyright (c) 2021 Her Majesty the Queen in Right of Canada, Department of National Defence, 2021.
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


## Estimate IFR for other nations using OECD Data

# Uncomment this to get the source data form OECD (slow)
#oecdDem <- get_dataset("HISTPOP", filter = NULL, start_time = NULL,
#            end_time = NULL, pre_formatted = FALSE)
#oecdDemCln <- as.data.table(oecdDem)
#oecdDemCln <- oecdDemCln[obsTime == 2018, ]
#oecdDemCln <- oecdDemCln[SEX == "T", ]
#write.csv(oecdDemCln,"data/oecdDemCln.csv")

# MRM - require additional packages

required_packages <- c("data.table","ggplot2","ggthemes","gghighlight","dplyr","purrr","tidyr")
not_installed <- required_packages[!(required_packages %in% installed.packages()[ , "Package"])]    # Determine missing packages
if(length(not_installed)) install.packages(not_installed)                                           # Install missing packages

suppressWarnings(lapply(required_packages, require, character.only = TRUE))



oecdDemCln <- fread("data/oecdDemCln.csv")
# One year difference compared to my data unfortunately (20-24 + 25-29 + 30-34 + 35-39 instead of 21-40 in ISTAT data)
# MRM - note that most countries/data sources use the OECD convention, not the ISTAT convention
oecdDemCln[AGE %in% c("0_4", "05_9", "10_14", "15_19"), ageRange := "0-20"]
oecdDemCln[AGE %in% c("20_24", "25_29", "30_34", "35_39"), ageRange := "21-40"]
oecdDemCln[AGE %in% c("40_44", "45_49"), ageRange := "41-50"]
oecdDemCln[AGE %in% c("50_54", "55_59"), ageRange := "51-60"]
oecdDemCln[AGE %in% c("60_64", "65_69"), ageRange := "61-70"]
oecdDemCln[AGE %in% c("70_74", "75_79"), ageRange := "71-80"]
oecdDemCln[AGE %in% c("80_84", "85_OVER"), ageRange := "81+"]

popByAgeRange <- oecdDemCln[! is.na(ageRange), sum(obsValue), by = c("LOCATION", "ageRange")]
popByAgeRange <- merge(popByAgeRange, oecdDemCln[AGE %in% c("TOTAL"), c("LOCATION", "obsValue")], by = "LOCATION")
popByAgeRange[, share := V1/obsValue]

# Pull out core elements of the table
popByAgeRangeInterim <- popByAgeRange %>% select(-V1,-obsValue)


# Add IFR estimates
ifrEstimates <- table2Data[20:26, c(1,3,4)]
ifrEstimates <- cbind(ifrEstimates, c("0-20", "21-40", "41-50", "51-60", "61-70", "71-80", "81+"))
names(ifrEstimates) <- c("mode","lower","upper","ageRange")

popByAgeRange <- merge(popByAgeRange, ifrEstimates, by = "ageRange")
popByAgeRange[, overallIFRest := sum(mode*share), by = LOCATION]
popByAgeRange[, overallIFRLower := sum(lower*share), by = LOCATION]
popByAgeRange[, overallIFRUpper := sum(upper*share), by = LOCATION]

popByAgeRange <- unique(popByAgeRange[, c("LOCATION", "overallIFRest", "overallIFRLower", "overallIFRUpper")])

# MRM - all code below this point specific to MacLeod and Hunter paper

# MRM - select out countries of interest to highlight in the plot, output
theseCountries <- c("CAN","FRA","ITA", "CHN")
popByAgeRangeOECD <- popByAgeRange
ggplot(data=popByAgeRange, aes(x= reorder(LOCATION, overallIFRest), y = overallIFRest, 
                               ymin=overallIFRLower, ymax=overallIFRUpper)) +
  geom_pointrange(colour="red") + xlab("Population") + ylab("Estimated overall IFR (Rinaldi age stratification)") + 
  coord_flip() + theme_tufte() + gghighlight(LOCATION %in% theseCountries, 
                                             unhighlighted_params= list(colour=alpha("grey",0.8)))
ggsave("output/oecdPlot.png")

#---------------
# Code in support of the 'Credible Intervals' section of the MacLeod and Hunter paper

# Set up quantiles
quants <- c(0.025,0.5,0.975)
q_names <- map_chr(quants, ~paste0(.x*100,"%"))
q_funs <- map(quants, ~partial(quantile, probs = .x, na.rm=TRUE)) %>% set_names(nm=q_names)

# pull out MCMC trace data
IFRcorrect <- cbind(IFRPlot,index=1:100000) 

# Weight and sum the individual IFR samples by the population fractional weights for the 
# population of interest and sum
letsPlot <- IFRcorrect %>% rename(ageRange = variable) %>% left_join(popByAgeRangeInterim) %>% 
  mutate(IFRcontrib = value*share) %>% select(-value,-share) %>% 
  pivot_wider(names_from = ageRange,values_from=IFRcontrib) %>% 
  mutate(lineIFR = Reduce("+",.[3:9])) %>% select(index,LOCATION,lineIFR) %>% 
  #mutate(lineIFR=100*lineIFR) %>%
  group_by(LOCATION) %>% summarize_at(vars(lineIFR),funs(!!!q_funs))

# Plot the IFR estimates generated above for OECD countries with countries of interest highlighted.
ggplot(data=letsPlot, aes(x= reorder(LOCATION, `50%`), y = `50%`,
                          ymin=`2.5%`, ymax=`97.5%`)) +
  geom_pointrange(colour="red", shape=5) + xlab("Population\n") + 
  ylab("Estimated overall infection fatality rate (%)") +
  coord_flip() + theme_tufte(base_family = "ArialMT") + 
  gghighlight(LOCATION %in% theseCountries, unhighlighted_params= list(colour="grey", shape=19))

# Added an additional line to the plot as per INFORMS formatting guidelines
ggplot(data=letsPlot, aes(x= reorder(LOCATION, `50%`), y = `50%`,
                          ymin=`2.5%`, ymax=`97.5%`)) +
  geom_pointrange(colour="red", shape=5) + xlab("Population\n") + 
  ylab("Estimated overall infection fatality rate (%)") +
  coord_flip() + theme_tufte(base_family="ArialMT") + theme(axis.line.x=element_line(),
                                                            #axis.line.y=element_line()
  ) +
  gghighlight(LOCATION %in% theseCountries, unhighlighted_params= list(colour="grey", shape=19))

# Save plot
ggsave("output/Figure4.pdf",device=cairo_pdf)
ggsave("output/Figure4.png")


# Age-adjusted density plot of COVID-19 IFRs for several countries of interest

letsPlotIt <- IFRcorrect %>% rename(ageRange = variable) %>% left_join(popByAgeRangeInterim) %>%
  mutate(IFRcontrib = value*share) %>% select(-value,-share) %>% 
  pivot_wider(names_from = ageRange,values_from=IFRcontrib) %>% 
  mutate(lineIFR = Reduce("+",.[3:9])) %>% select(index,LOCATION,lineIFR) %>% group_by(LOCATION)


letsPlotIt %>% rename(Country=LOCATION) %>%  group_by(Country) %>% 
  filter(Country %in% c("CAN","FRA","ITA", "CHN")) %>% 
  #mutate(lineIFR = lineIFR * 100) %>%
  ggplot(aes(lineIFR, colour=Country,linetype=Country)) + geom_density() + 
  xlab("Infection fatality rate (%)") + ylab("Density of Estimates") +
  theme_tufte(base_family = "ArialMT") + 
  theme(axis.text.y = element_blank()) + 
  scale_color_brewer(palette = "Set1")

# Adding in extra axes lines
letsPlotIt %>% rename(Country=LOCATION) %>%  group_by(Country) %>% 
  filter(Country %in% c("CAN","FRA","ITA", "CHN")) %>% 
  #mutate(lineIFR = lineIFR * 100) %>%
  ggplot(aes(lineIFR, colour=Country,linetype=Country)) + geom_density() + 
  xlab("Infection fatality rate (%)") + ylab("Density of Estimates") +
  theme_tufte(base_family="ArialMT") + scale_color_brewer(palette = "Set1") + theme(
    axis.line.x=element_line(),
    axis.line.y=element_line(),
    axis.text.y = element_blank(),
    axis.ticks.y=element_blank()
  ) +
  scale_y_continuous(expand=c(0,0)) 
ggsave("output/Figure2.pdf",device=cairo_pdf)
ggsave("output/Figure2.png")


# MRM - read in Canadian data massaged to match  format
# Note that these values were initially based on interim population estimates that
# have since been adjusted to final totals, so do not exactly match any values found
# Statistics Canada (2020b) Table 17-10-0005-01: Population estimates on July 1st, 
# by age and sex. Accessed May 28, 2020, http://dx.doi.org/https://doi.org/10.25318/1710000501-eng.
# The values for the Canadian Armed Forces (CAF) used in the table are rounded from the
# exact totals used in the published analysis. 


popByAgeRange <- fread("data/canInterim.csv")


# Add IFR estimates
ifrEstimates <- table2Data[20:26, 1:3]
ifrEstimates <- cbind(ifrEstimates, c("0-20", "21-40", "41-50", "51-60", "61-70", "71-80", "81+"))
names(ifrEstimates) <- c("median","lower","upper","ageRange")

popByAgeRange <- merge(popByAgeRange, ifrEstimates, by = "ageRange")
popByAgeRange[, overallIFRest := sum(median*share), by = LOCATION]
popByAgeRange[, overallIFRLower := sum(lower*share), by = LOCATION]
popByAgeRange[, overallIFRUpper := sum(upper*share), by = LOCATION]

popByAgeRange <- unique(popByAgeRange[, c("LOCATION", "overallIFRest", "overallIFRLower", "overallIFRUpper")])

# Plot IFR estimates for Canada, highlighting Canada overall and the CAF specifically

theseCountries <- c("CAN","CAF")
ggplot(data=popByAgeRange, aes(x= reorder(LOCATION, overallIFRest), y = overallIFRest, 
                               ymin=overallIFRLower, ymax=overallIFRUpper)) +
  geom_pointrange(colour="red") + xlab("Population") + 
  ylab("Estimated overall IFR (Rinaldi age stratification)") + 
  coord_flip() + theme_tufte(base_family="ArialMT") + 
  gghighlight(LOCATION %in% theseCountries, unhighlighted_params= list(colour=alpha("grey",0.8)))
ggsave("output/canPlot.png")

# Plot the infection rate prior distribution used in Rinaldi and Paradisi analysis
tibble(p=seq(0,1, length=100), density=dbeta(p, 3, 2)) %>% 
  ggplot(., aes(x=p,y=density)) + geom_line() + theme_tufte(base_family="ArialMT") +
  theme(
    axis.line.x=element_line(),
    axis.line.y=element_line(),
    # axis.ticks.y=element_blank()
  ) +  xlab("Infection rate") + ylab("Prior density") +
  scale_y_continuous(expand=c(0,0)) 

ggsave("output/Figure1.pdf",device=cairo_pdf)