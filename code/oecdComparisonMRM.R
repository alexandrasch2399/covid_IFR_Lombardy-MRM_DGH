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


## Estimate IFR for other nations using OECD Data

# Uncomment this to get the source data form OECD (slow)
#oecdDem <- get_dataset("HISTPOP", filter = NULL, start_time = NULL,
#            end_time = NULL, pre_formatted = FALSE)
#oecdDemCln <- as.data.table(oecdDem)
#oecdDemCln <- oecdDemCln[obsTime == 2018, ]
#oecdDemCln <- oecdDemCln[SEX == "T", ]
#write.csv(oecdDemCln,"data/oecdDemCln.csv")

# MRM - require additional packages

required_packages <- c("data.table","ggplot2","ggthemes","gghighlight","dplyr",
                       "purrr","tidyr","forcats","xtable","stringr","scales")
not_installed <- required_packages[!(required_packages %in% installed.packages()[ , "Package"])]    # Determine missing packages
if(length(not_installed)) install.packages(not_installed)                                           # Install missing packages

suppressWarnings(lapply(required_packages, require, character.only = TRUE))



oecdDemCln <- fread("data/oecdDemCln.csv")
# One year difference compared to my data unfortunately (20-24 + 25-29 + 30-34 + 35-39 instead of 21-40 in ISTAT data)
# MRM - note that most countries/data sources use the OECD convention, not the ISTAT convention,
# adjusted to reflect the more standard convention and also separate out additional  bins
oecdDemCln[AGE %in% c("0_4", "05_9"), ageRange := "0-9"]
oecdDemCln[AGE %in% c("10_14", "15_19"), ageRange := "10-19"]
oecdDemCln[AGE %in% c("20_24", "25_29"), ageRange := "20-29"]
oecdDemCln[AGE %in% c("30_34", "35_39"), ageRange := "30-39"]
oecdDemCln[AGE %in% c("40_44", "45_49"), ageRange := "40-49"]
oecdDemCln[AGE %in% c("50_54", "55_59"), ageRange := "50-59"]
oecdDemCln[AGE %in% c("60_64", "65_69"), ageRange := "60-69"]
oecdDemCln[AGE %in% c("70_74", "75_79"), ageRange := "70-79"]
oecdDemCln[AGE %in% c("80_84", "85_OVER"), ageRange := "80+"]


popByAgeRange <- oecdDemCln[! is.na(ageRange), sum(obsValue), by = c("LOCATION", "ageRange")]
popByAgeRange <- merge(popByAgeRange, oecdDemCln[AGE %in% c("TOTAL"), c("LOCATION", "obsValue")], by = "LOCATION")
popByAgeRange[, share := V1/obsValue]

# MRM - Pull out core elements of the table
popByAgeRangeInterim <- popByAgeRange %>% select(-V1,-obsValue)

# MRM - Read in IFR estimates from two other papers:
# Verity R, Okell LC, Dorigatti I, Winskill P, Whittaker C, Imai N, Cuomo-Dannenburg G, 
# et al. (2020a) Estimates of the severity of coronavirus disease 2019: A model-based analysis. 
# Lancet Infectious Diseases 20(6):669–677.  
# Salje H, Tran Kiem C, Lefrancq N, Courtejoie N, Bosetti P, Paireau J,
# Andronico A, et al. (2020a) Estimating the burden of SARS-CoV-2
# in France. Science 369(6500):208–211. (and its revision)


verSal <- fread("data/ifrVeritySalje.csv")

verSal[ageRange %in% c("0-10"), ageRange:=c("0-9")]
verSal[ageRange %in% c("11-20"), ageRange:=c("10-19")]
verSal[ageRange %in% c("21-30"), ageRange:=c("20-29")]
verSal[ageRange %in% c("31-40"), ageRange:=c("30-39")]
verSal[ageRange %in% c("41-50"), ageRange:=c("40-49")]
verSal[ageRange %in% c("51-60"), ageRange:=c("50-59")]
verSal[ageRange %in% c("61-70"), ageRange:=c("60-69")]
verSal[ageRange %in% c("71-80"), ageRange:=c("70-79")]
verSal[ageRange %in% c("81+"), ageRange:=c("80+")]


# MRM - Combine the IFR estimates from Rinali, Salje and Verity into a single table with
# consistent age bands and titling. "IFRSource" column notes the origin of the estimate.
ifrEstimates <- rbind(table2Data[20, 2:4],table2Data[20,2:4],table2Data[21,2:4],table2Data[21,2:4],table2Data[22:26,2:4])
ifrEstimates <- cbind(ifrEstimates, c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))
ifrEstimates <- cbind(ifrEstimates, "Rinaldi")
names(ifrEstimates) <- c("median","lower","upper","ageRange","IFRSource")
ifrEstimates <- rbind(ifrEstimates,verSal)

# -----------------
# MRM - all code below this point specific to MacLeod and Hunter paper

# MRM - select out countries of interest to highlight in the plot, output
theseCountries <- c("CAN","FRA","ITA", "CHN")

# Create a summary table of IFR estimates for all OECD countries for each source estimate.
popByAgeRangeOECD <- left_join(popByAgeRange,ifrEstimates) %>% group_by(LOCATION,IFRSource) %>% 
  summarize(overallIFRest = sum(median*share), overallIFRLower = sum(lower*share), 
            overallIFRUpper = sum(upper*share))


popOrder <- popByAgeRangeOECD %>% ungroup() %>% filter(IFRSource == "Rinaldi") %>% arrange(overallIFRest) %>% mutate(order=row_number()) %>% select(LOCATION,order)

# Full plot for all OECD countries for all four IFR sources - too busy a plot in practice.
popByAgeRangeOECD %>% ungroup() %>% left_join(popOrder) %>% arrange(desc(order)) %>% 
  mutate(LOCATION=factor(LOCATION,levels=(unique(LOCATION)))) %>% group_by(LOCATION) %>%
  ggplot(aes(x= IFRSource, y = overallIFRest, ymin=overallIFRLower, 
             ymax=overallIFRUpper,  col=IFRSource, shape = IFRSource)) + 
  geom_pointrange(aes(col=IFRSource)) + facet_wrap(~LOCATION,strip.position="left",
                                                   scales="free_y",ncol=1)  + 
  theme_tufte(base_family = "ArialMT") + xlab("Population") + ylab("Estimated population infection fatality rate (%)") + 
  scale_color_brewer(palette="Set1") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold")) + coord_flip()

# Plot of IFR estimates for each source for four countries of interest
popByAgeRangeOECD %>% ungroup() %>% left_join(popOrder) %>% arrange(desc(order)) %>% 
  mutate(LOCATION=factor(LOCATION,levels=(unique(LOCATION)))) %>% group_by(LOCATION) %>% 
  filter(LOCATION %in% theseCountries) %>%
  ggplot(aes(x= IFRSource, y = overallIFRest, ymin=overallIFRLower, 
             ymax=overallIFRUpper,  col=IFRSource, shape=IFRSource)) + 
  geom_pointrange(aes(col=IFRSource)) + facet_wrap(~LOCATION,strip.position="left",
                                                   scales="free_y",ncol=1)  + 
  theme_tufte(base_family = "ArialMT") + xlab("Population") + ylab("Estimated population infection fatality rate (%)") + 
  scale_color_brewer(palette="Set1") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        axis.line.x=element_line(),
        #axis.line.y=element_line(),
        legend.title = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold")) + coord_flip()

ggsave("output/Figure5.pdf",device=cairo_pdf)
ggsave("output/Figure5.png")


# MRM - read in Canadian data massaged to match interim format, complete set of bins
popByAgeRange <- fread("data/canInterim10.csv")

# Join the IFR estimates with the population (using weighted sum of quantile methods) 
popByAgeRange <- left_join(popByAgeRange,ifrEstimates) %>% group_by(LOCATION,IFRSource) %>% 
  summarize(overallIFRest = sum(median*share), overallIFRLower = sum(lower*share), 
            overallIFRUpper = sum(upper*share))

# Overall IFR estimates for Canadian sub-populations for each IFR source estimate
popByAgeRange %>% ungroup() %>% mutate(LOCATION = fct_reorder(LOCATION,desc(overallIFRest))) %>% 
  group_by(LOCATION) %>%
  filter(LOCATION != "P Res F") %>% filter(LOCATION != "Reg F") %>%
  ggplot(aes(x= IFRSource, y = overallIFRest, ymin=overallIFRLower, 
             ymax=overallIFRUpper,  col=IFRSource, shape=IFRSource)) + 
  geom_pointrange(aes(col=IFRSource)) + facet_wrap(~LOCATION,strip.position="left",
                                                   scales="free_y",ncol=1)  + 
  theme_tufte(base_family = "ArialMT") + xlab("Population") + ylab("Estimated population infection fatality rate (%)") + 
  scale_color_brewer(palette="Set1") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        legend.title = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold")) + coord_flip()


# Same figure as above, but with additional axis
popByAgeRange %>% ungroup() %>% mutate(LOCATION = fct_reorder(LOCATION,desc(overallIFRest))) %>% 
  group_by(LOCATION) %>%
  filter(LOCATION != "P Res F") %>% filter(LOCATION != "Reg F") %>%
  ggplot(aes(x= IFRSource, y = overallIFRest, ymin=overallIFRLower, 
             ymax=overallIFRUpper,  col=IFRSource, shape=IFRSource)) + 
  geom_pointrange(aes(col=IFRSource)) + facet_wrap(~LOCATION,strip.position="left",
                                                   scales="free_y",ncol=1)  + 
  theme_tufte(base_family = "ArialMT") + xlab("Population") + ylab("Estimated population infection fatality rate (%)") + 
  scale_color_brewer(palette="Set1") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        axis.line.x = element_line(),
        #axis.line.y = element_line(),
        legend.title = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold")) + coord_flip()
ggsave("output/Figure6.pdf",device=cairo_pdf)
ggsave("output/Figure6.png")


# Produce age-stratified IFR estimates from each source
# Note upgrading to R 4.x and packages has re-ordered the elements of the figure.
# Also, Rinaldi confidence intervals extend to zero when re-run on my new setup.
ifrEstimates %>% ggplot(aes(x=IFRSource, y = median, ymin = lower, ymax = upper, 
                            col = IFRSource, shape = IFRSource)) +
  geom_pointrange() +  facet_wrap(~ageRange, nrow = 1,strip.position = "bottom") + 
  theme_tufte(base_family = "ArialMT") + scale_color_brewer(palette="Set1") + 
  xlab("Age Band") +
  ylab("Infection fatality rate (%)") +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid.major.y = element_line(alpha("black",0.4)),
        # panel.grid.minor.y = element_line(alpha("black",0.2)),
        legend.title = element_blank(),
        panel.background = element_rect(fill=alpha("grey",0.3))) + 
  scale_y_log10(breaks=c(0.001,0.01,0.1,1,10), labels=comma)
ggsave("output/Figure3.pdf", width=11, height = 8.5, device=cairo_pdf)
ggsave("output/Figure3.png", width=11, height = 8.5)