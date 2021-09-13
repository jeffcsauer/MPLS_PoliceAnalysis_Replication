## ---------------------------
##
## Script name: Temporal Stops over time
##
## Purpose of script: Load in the cleaned Police Stop dataset
## and create time series visualizations of the stops
##
## Author: Okome et al. 20XX
##
## Date Created: 3/15/2021
##
## Email: jcsauer@terpmail.umd.edu
##
## Script template: https://timfarewell.co.uk/my-r-script-header-template/
## ---------------------------
##
## Notes: Code follows tidyverse R Style Guide. Autoformatted using
## the `styler` packing and addin.
##
## ---------------------------

## Ensure workspace is clean
rm(list = ls())

## Load packages ----
library(tidyverse)
library(tmap)
library(rgdal)

## Set wd ----

# Get the current user of the script
currentuser <- Sys.info()['effective_user']

# Run loop to set appropriate working directory based on teammate's computer.
if (currentuser == 'jeffe') {
  setwd("C:/Users/jeffe/Dropbox/Minneapolis Police Data/")
} else if (currentuser == 'USER') {
  setwd("D:/Dropbox/Minneapolis Police Data/")
} else {
  print("User not detected, please adjust path.")
}

# Double-check the path
getwd()

## Load relevant cleaned datasets ----
Mpls <-
  read.csv("./Data/processed/MplsCleanedThrough2021.csv")

## Create month-year counts ----

# Create month-year variable
Mpls$dateMonthYear <- format(as.Date(Mpls$date), "%Y-%m")

# Create a year month summarized data object
MplsMonthYear <- Mpls %>%
  # filter(dateMonthYear <= "2020-04") %>% if you want to focus on pre-george floyd
  group_by(dateMonthYear) %>%
  summarize(
    NStopsAsian = sum(raceRecode == 'Asian'),
    NStopsBlack = sum(raceRecode == 'Black'),
    NStopsLatin = sum(raceRecode == 'Latinx'),
    NStopsNatAm = sum(raceRecode == 'Indigenous'),
    NStopsOther = sum(raceRecode == 'Other'),
    NStopsUnkwn = sum(raceRecode == 'Unknown'),
    NStopsWhite = sum(raceRecode == 'White'),
    NStopsTotal = n(),
    NStopsAsianPct = (NStopsAsian/NStopsTotal)*100,
    NStopsBlackPct = (NStopsBlack/NStopsTotal)*100,
    NStopsLatinPct = (NStopsLatin/NStopsTotal)*100,
    NStopsNatAmPct = (NStopsNatAm/NStopsTotal)*100,
    NStopsOtherPct = (NStopsOther/NStopsTotal)*100,
    NStopsUnkwnPct = (NStopsUnkwn/NStopsTotal)*100,
    NStopsWhitePct = (NStopsWhite/NStopsTotal)*100,
  ) %>% 
  select(-NStopsTotal) %>% 
  filter(
    dateMonthYear!="2016-10" # only a few observations
  )

  #write to csv
  write.csv(MplsMonthYear, "C:/Users/jeffe/Dropbox/Minneapolis Police Data/Data/processed/summarized.csv")

  min(MplsMonthYear$NStopsBlackPct)
  max(MplsMonthYear$NStopsBlackPct)
  
  min(MplsMonthYear$NStopsWhitePct)
  max(MplsMonthYear$NStopsWhitePct)
  

# Convert wide data to long
MplsMonthYearLong <- stats::reshape(
  as.data.frame(MplsMonthYear),
  idvar = "dateMonthYear",
  direction = "long",
  varying = list(c(2:8), c(9:15)),
  v.names = c("NStops", "NStopsPct")
)

# Insert race groups back into Long dataset
MplsMonthYearLong$race <- rep(c("Asian", "Black", "Latinx", "Indigenous", "Other", "Unknown", "White"), each = 53)

## Create a few month-year longitudinal plots----

# Create new breaks - https://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

XLabs <- every_nth(MplsMonthYear$dateMonthYear, 2)

# Stacked bar chart with percentages
ggplot(MplsMonthYearLong, aes(
    x = dateMonthYear,
    y = NStops,
    fill = race,
    cumulative = T
  )) +
  scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
  scale_x_discrete(labels = XLabs) + 
  geom_col() +
  ylab("Total number of stops") +
  xlab("Year-Month") +
  ggtitle("Year-Month breakdown of Minneapolis police stops by race (Nov. 2016 - Mar. 2021)") +
  geom_text(aes(label = paste0(round(NStopsPct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 2) +
  # MN Annotate
  annotate(
    geom = "curve",
    x = 42.5,
    y = 4500,
    xend = 41,
    yend = 3150,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "text",
    x = 42.5,
    y = 4500,
    label = "MN COVID-19 Events \nFirst case: Mar. 6, 2020\nState of Emergency: Mar. 13, 2020\nStay at Home: Mar. 27, 2020",
    hjust = "left", size = 4
  ) +
  # George Floyd Annotate
  annotate(
    geom = "curve",
    x = 46,
    y = 3500,
    xend = 43.75,
    yend = 3200,
    curvature = 0,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "text",
    x = 46,
    y = 3500,
    label = "Killing of George Floyd\nMay 25, 2020",
    hjust = "left", size = 4
  ) +
  # Legend title
  labs(fill = "Race recorded\nat stop") + 
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 0
  ))

ggsave(
  "./Writing/Visualizations/Graphs/TemporalStackedBarStopsByRace.tiff",
  width = 14,
  height = 10
)

# Line graph
ggplot(data = MplsMonthYearLong,
       aes(
         x = dateMonthYear,
         y = NStops,
         group = race,
         color = race
       )) +
  geom_line() +
  ggtitle("Year-Month breakdown of Minneapolis police stops by race (Nov. 2016 - Mar. 2021)") +
  scale_y_continuous(breaks = seq(0, 2000, by = 250)) +
  scale_x_discrete(labels = XLabs) + 
  scale_color_discrete(name="Race recorded\nat stop") +
  ylab("Total number of stops") +
  xlab("Year-Month") +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 0
  )) +
  # MN Annotate
  geom_vline(xintercept = 41,
             linetype = 'dotted',
             col = 'blue') +
  annotate(
    "text",
    x = 41,
    y = 1700,
    label = "MN COVID-19 Events\nFirst case: Mar. 6, 2020\nState of Emergency: Mar. 13, 2020\nStay at Home: Mar. 27, 2020",
    hjust = 1.05
  ) +
  # George Floyd Annotate
  geom_vline(xintercept = 43,
             linetype = 'dotted',
             col = 'red') +
  annotate(
    "text",
    x = 43,
    y = 1450,
    label = "Killing of George Floyd\nMay 25, 2020",
    hjust = -0.05
  ) 
  
ggsave(
  "./Writing/Visualizations/Graphs/TemporalLineStopsByRace.tiff",
  width = 14,
  height = 10
)
