## ---------------------------
##
## Script name: Police Stop Dataset Clean
##
## Purpose of script: Load in the Police Stop dataset from MPLS open data
## and clean it for analysis.
##
## Author: Okome et al. 20XX
##
## Date Created: 12/13/2020
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
library(data.table)
library(sf)
library(lubridate)
library(suncalc)

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

## Begin cleaning police stops data ----

# Police data last downloaded on 12-13-2020
Mpls <- 
  read.csv('./Data/raw/MPLS/Police_Stop_Data.csv', stringsAsFactors = F)

# Drop first column (used for shapefiles)
Mpls <- Mpls[,-c(1)]

# Remove observations missing a neighborhood (also removes
# observations that are missing latlon)
Mpls <- Mpls[Mpls$neighborhood != "",]

# Clean data for stops that are for truancy or curfew violations
table(Mpls$problem)
Mpls <- Mpls[Mpls$problem != "Truancy (P)",]
Mpls <- Mpls[Mpls$problem != "Curfew Violations (P)",]

# Clean data for stops that are for gender as non-conforming
# Mpls <- Mpls[Mpls$gender != 'Gender Non-Conforming',]

# Code for arrest rates from searches
Mpls %>% 
  filter(vehicleSearch=="YES") %>% 
  group_by(race) %>% 
  summarize(
    total_searches_race = n(),
    total_arrests_race = sum(callDisposition=="BKG-Booking"),
    arrest_search_rate_race = total_arrests_race/total_searches_race
  )

# Reclassify the race variable such that the category 'East African' is now 'Black'
Mpls$preRaceRecode <- recode(Mpls$preRace, 'East African' = 'Black')
Mpls$raceRecode <- recode(Mpls$race, 'East African' = 'Black')

# Latino to Latinx
Mpls$preRaceRecode <- recode(Mpls$preRaceRecode, 'Latino' = 'Latinx')
Mpls$raceRecode <- recode(Mpls$raceRecode, 'Latino' = 'Latinx')

# Native American to Indigenous
Mpls$preRaceRecode <- recode(Mpls$preRaceRecode, 'Native American' = 'Indigenous')
Mpls$raceRecode <- recode(Mpls$raceRecode, 'Native American' = 'Indigenous')

# Reclassifying missing to unknown
Mpls$preRaceRecode <- sub("^$", "Unknown", Mpls$preRaceRecode)
Mpls$raceRecode <- sub("^$", "Unknown", Mpls$raceRecode)

# Remove the '+00' from the time column
Mpls$responseDate <- gsub("\\+00", "", Mpls$responseDate)

# Create two date columns, a 'full' date-time column, and a 'date' column
Mpls$dateFull <- ymd_hms(Mpls$responseDate)
Mpls$date <- as.Date.POSIXct(ymd_hms(Mpls$responseDate))

# Create an 'any' search variable
Mpls$anySearch <-
  ifelse(Mpls$personSearch == "YES" |
           Mpls$vehicleSearch == "YES", "YES", "NO")

# Determine sunrise and sunset times using suncalc package
# Create another lon column for function getSunlightTimes
Mpls$lon <- Mpls$long
SunsetTimes <- getSunlightTimes(data = Mpls, tz = 'UTC')
# From the Police_Stop_Data metadata page: "Stop Data for the Minneapolis Police Department. 
# Please note that the datetimes shown are in UTC time (not local time)."
# https://www.arcgis.com/home/item.html?id=215b4b543d894750aef86c725b56ee2a

# Use the sunset times to create a binary day/night variable based on
# sunrise and sunset (following nature article)
Mpls$veilBeforeSunset <-
  ifelse(Mpls$dateFull >= SunsetTimes$sunrise &
           Mpls$dateFull <= SunsetTimes$sunset,
         1,
         0)

Mpls$veilAfterSunset <-
  ifelse(Mpls$dateFull >= SunsetTimes$dusk |
           Mpls$dateFull <= SunsetTimes$sunrise,
         1,
         0)

# Create a generic inter twilight variable (17:00 - 22:00)
Mpls$hm <- hour(Mpls$dateFull) + minute(Mpls$dateFull) / 60
Mpls$interTwilight <-
  ifelse(Mpls$hm >= 17.0 &
           Mpls$hm <= 22.0,
         1,
         0)

# Create a year variable
Mpls$year <- year(Mpls$dateFull)

# Check data
table(Mpls$race)
table(Mpls$raceRecode)
table(Mpls$preRace)
table(Mpls$preRaceRecode)

# Pre and post race agreement
Mpls2 <- Mpls[Mpls$race!="",]
Mpls2 <- Mpls2[Mpls2$race!="Unknown",]
Mpls2 <- Mpls2[Mpls2$preRace!="Unknown",]
Mpls2 <- Mpls2[Mpls2$race!="Unknown",]
table(Mpls2$preRace, Mpls2$race)
sum(diag(table(Mpls2$preRace, Mpls2$race)))/nrow(Mpls2)

# Save data as is for use later
write.csv(Mpls, "./Data/processed/MplsCleanedThrough2021.csv")

### TABLE 1
# Tabulation of stops
(table(Mpls$raceRecode) / nrow(Mpls)) * 100

# Tabulation of stops by various demographic characteristics
m_Mpls <-
  melt(Mpls, measure.vars = c("preRaceRecode", "raceRecode", "problem", "gender"))

summarytable_personsearch <- m_Mpls %>%
  group_by(personSearch, variable, value) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n))*100)

summarytable_vehiclesearch <- m_Mpls %>%
  group_by(vehicleSearch, variable, value) %>%
  summarise (n = n()) %>%
  mutate(freq = (n / sum(n))*100)

### TABLE 2
# Write to csv to manually enter into word
write.csv(summarytable_personsearch, "D:/Dropbox/Minneapolis Police Data/Writing/Tables/Table2_personsearch.csv")
write.csv(summarytable_vehiclesearch, "D:/Dropbox/Minneapolis Police Data/Writing/Tables/Table2_vehiclesearch.csv")

### Figure 1

# A
summarytable_personsearch_f1 <- summarytable_personsearch %>% 
  filter(variable == "raceRecode" & personSearch!="")

TopA <- ggplot(summarytable_personsearch_f1, 
       aes(factor(value), n, fill = personSearch)) + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Determined Race") + 
  ylab("Number of events") +
  scale_fill_manual(name = "Person Search", values=c("#999999", "black")) + 
  theme_classic() + 
  theme(legend.position = "top")

# B
TopB <- ggplot(summarytable_personsearch_f1, 
       aes(fill=personSearch, y=n, x=value)) + 
  geom_bar(position="fill", stat="identity") +
  xlab("Determined Race") + 
  ylab("Percentage of stops") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(name = "Person Search", values=c("#999999", "black")) + 
  theme_classic() + 
  theme(legend.position = "top")

TopB

# Save as a single figure
library(ggpubr)

Figure1 <- ggarrange(TopA, TopB, labels = c("A", "B"), nrow = 2)

ggsave(Figure1, 
       path = "./Writing/Visualizations/",
       filename = "Figure1_Rebuild.png", height = 8, width = 7, dpi = 600,
       device = 'png') 

## Not all years have the same amount of data, and COVID started March 2020. 
## In addition, the data for 2016 is incomplete. Thus we drop data for 2016 
## and those before March 13, 2020. This only pertains to the 
## rate maps. 

# Drop if 2016 or 2021
Mpls <- Mpls[Mpls$year != 2016,]
Mpls <- Mpls[Mpls$year != 2021,]


### Gather census tract data ---- 
library(tidycensus)

# To use the tidycensus package you need a Census API key. Below is a 
# key I have generated. You may need to reinstall it. 

#census_api_key("432785a57d0f169bffd0590b2565dce1c3369050", install = T)

# Identify a number of census tract level demographic variables to collect. 
# We use 5-year American Community Survey estimates. 
acs5_vars_subset <- c(pop_tot = "B01003_001", 
                      pop_male = "B01001_002", 
                      pop_female = "B01001_026",
                      race_tot = "B02001_001", 
                      race_wht = "B02001_002",
                      race_blk = "B02001_003", 
                      #race_ind = "B02001_004", 
                      #race_asn = "B02001_005", 
                      #race_hwn = "B02001_006", 
                      #race_oth = "B02001_007",
                      #race_2more = "B02001_008", 
                      #race_2more_oth = "B02001_009", 
                      #race_2more_nonoth = "B02001_010",
                      hisp_tot = "B03002_001",
                      hisp_no_tot = "B03002_002",
                      hisp_no_wht = "B03002_003",
                      hisp_no_blk = "B03002_004",
                      hisp_no_ind = "B03002_005",
                      hisp_no_asn = "B03002_006",
                      hisp_no_hwn = "B03002_007",
                      hisp_no_oth = "B03002_008",
                      hisp_no_2more = "B03002_009",
                      hisp_no_2more_oth = "B03002_010",
                      hisp_no_2more_nonoth = "B03002_011",
                      hisp_yes_tot = "B03002_012",
                      hisp_yes_wht = "B03002_013",
                      hisp_yes_blk = "B03002_014",
                      hisp_yes_ind = "B03002_015",
                      hisp_yes_asn = "B03002_016",
                      hisp_yes_hwn = "B03002_017",
                      hisp_yes_oth = "B03002_018",
                      hisp_yes_2more = "B03002_019",
                      hisp_yes_2more_oth = "B03002_020",
                      hisp_yes_2more_nonoth = "B03002_021",
                      inc_med = "B06011_001", 
                      edu_tot = "B15002_001",
                      edu_tot_m = "B15002_002", 
                      edu_tot_m_none = "B15002_003", 
                      edu_tot_m_nursery = "B15002_004", 
                      edu_tot_m_gr_5_6 = "B15002_005", 
                      edu_tot_m_gr_7_8 = "B15002_006", 
                      edu_tot_m_gr_9 = "B15002_007", 
                      edu_tot_m_gr_10 = "B15002_008", 
                      edu_tot_m_gr_11 = "B15002_009", 
                      edu_tot_m_gr_12_nodip = "B15002_010", 
                      edu_tot_m_gr_hs_grad = "B15002_011", 
                      edu_tot_m_coll_1yrless = "B15002_012", 
                      edu_tot_m_coll_1yrmore = "B15002_013", 
                      edu_tot_m_assoc = "B15002_014", 
                      edu_tot_m_bach = "B15002_015", 
                      edu_tot_m_mast = "B15002_016", 
                      edu_tot_m_prof = "B15002_017", 
                      edu_tot_m_doct = "B15002_018", 
                      edu_tot_f = "B15002_019", 
                      edu_tot_f_none = "B15002_020", 
                      edu_tot_f_nursery = "B15002_021", 
                      edu_tot_f_gr_5_6 = "B15002_022", 
                      edu_tot_f_gr_7_8 = "B15002_023", 
                      edu_tot_f_gr_9 = "B15002_024", 
                      edu_tot_f_gr_10 = "B15002_025", 
                      edu_tot_f_gr_11 = "B15002_026", 
                      edu_tot_f_gr_12_nodip = "B15002_027", 
                      edu_tot_f_gr_hs_grad = "B15002_028", 
                      edu_tot_f_coll_1yrless = "B15002_029", 
                      edu_tot_f_coll_1yrmore = "B15002_030", 
                      edu_tot_f_assoc = "B15002_031", 
                      edu_tot_f_bach = "B15002_032", 
                      edu_tot_f_fast = "B15002_033", 
                      edu_tot_f_prof = "B15002_034", 
                      edu_tot_f_doct = "B15002_035",
                      pov = "B17001_002", 
                      pov_tot = "B17001_001", 
                      unemp1 = "C23002A_008", 
                      unemp3 = "C23002A_021",
                      unemp_tot1 = "C23002A_006",
                      unemp_tot3 = "C23002A_019",
                      build1 = "B25024_007",
                      build2 = "B25024_008",
                      build3 = "B25024_009",
                      build_tot = "B25024_001", 
                      vehicle = "B08141_002",
                      vehicle_tot = "B08141_001", 
                      # add age variables to get age 18-24, 25-34, 35-44, 45-54, 55-64, 65 plus)
                      age_tot_m_5below = "B01001_003",
                      age_tot_m_5_9 = "B01001_004",
                      age_tot_m_10_14 = "B01001_005",
                      age_tot_m_15_17 = "B01001_006",
                      age_tot_m_18_19 = "B01001_007",
                      age_tot_m_20 = "B01001_008",
                      age_tot_m_21 = "B01001_009", 
                      age_tot_m_22_24 = "B01001_010",
                      age_tot_m_25_29 = "B01001_011",
                      age_tot_m_30_34 = "B01001_012",
                      age_tot_m_35_39 = "B01001_013",
                      age_tot_m_40_44 = "B01001_014", 
                      age_tot_m_45_49 = "B01001_015", 
                      age_tot_m_50_54 = "B01001_016",
                      age_tot_m_55_59 = "B01001_017", 
                      age_tot_m_60_61 = "B01001_018", 
                      age_tot_m_62_64 = "B01001_019", 
                      age_tot_m_65_66 = "B01001_020",
                      age_tot_m_67_69 = "B01001_021",
                      age_tot_m_70_74 = "B01001_022", 
                      age_tot_m_75_79 = "B01001_023", 
                      age_tot_m_80_84 = "B01001_024",
                      age_tot_m_85_plus = "B01001_025",
                      age_tot_f_5below = "B01001_027",
                      age_tot_f_5_9 = "B01001_028",
                      age_tot_f_10_14 = "B01001_029",
                      age_tot_f_15_17 = "B01001_030",
                      age_tot_f_18_19 = "B01001_031",
                      age_tot_f_20 = "B01001_032",
                      age_tot_f_21 = "B01001_033", 
                      age_tot_f_22_24 = "B01001_034",
                      age_tot_f_25_29 = "B01001_035",
                      age_tot_f_30_34 = "B01001_036",
                      age_tot_f_35_39 = "B01001_037",
                      age_tot_f_40_44 = "B01001_038", 
                      age_tot_f_45_49 = "B01001_039", 
                      age_tot_f_50_54 = "B01001_040",
                      age_tot_f_55_59 = "B01001_041", 
                      age_tot_f_60_61 = "B01001_042", 
                      age_tot_f_62_64 = "B01001_043", 
                      age_tot_f_65_66 = "B01001_044",
                      age_tot_f_67_69 = "B01001_045",
                      age_tot_f_70_74 = "B01001_046", 
                      age_tot_f_75_79 = "B01001_047", 
                      age_tot_f_80_84 = "B01001_048",
                      age_tot_f_85_plus = "B01001_049",
                      # Add a few variables to construct Messer deprivation index
                      noHS_num = "B06009_002",
                      noHS_denom = "B06009_001",
                      #femhead_num = "B09008_011",
                      #femhead_denom = "B09008_001",
                      manageprof_num = "B08124_002",
                      manageprof_denom = "B08124_001",
                      crowd_num1 = "B25014_005",
                      crowd_num2 = "B25014_006",
                      crowd_num3 = "B25014_007",
                      crowd_num4 = "B25014_011",
                      crowd_num5 = "B25014_012",
                      crowd_num6 = "B25014_013",
                      crowd_denom = "B25014_001",
                      pubassist_num = "B19058_002",
                      pubassist_denom = "B19058_001",
                      incunder30_num1 = "B19001_002", 
                      incunder30_num2 = "B19001_003", 
                      incunder30_num3 = "B19001_004",
                      incunder30_num4 = "B19001_005",
                      incunder30_num5 = "B19001_006",
                      incunder30_denom =  "B19001_001"
)

# Collect the data from the API
options(tigris_use_cache = T)

acs5_data_2017_base <- get_acs(geography = "tract",
                               variables = acs5_vars_subset,
                               year = 2017,
                               survey = "acs5",
                               output = "wide",
                               state = "MN",
                               county = "Hennepin",
                               geometry = TRUE,
                               cache_table = T)

acs5_data_2018_base <- get_acs(geography = "tract",
                               variables = acs5_vars_subset,
                               year = 2018,
                               survey = "acs5",
                               output = "wide",
                               state = "MN",
                               county = "Hennepin",
                               geometry = TRUE,
                               cache_table = T)

acs5_data_2019_base <- get_acs(geography = "tract",
                               variables = acs5_vars_subset,
                               year = 2019,
                               survey = "acs5",
                               output = "wide",
                               state = "MN",
                               county = "Hennepin",
                               geometry = TRUE,
                               cache_table = T)


# As we are going to do some transformations we duplicate the data objects.
acs5_data_2017 <- acs5_data_2017_base
acs5_data_2018 <- acs5_data_2018_base
acs5_data_2019 <- acs5_data_2019_base

# Assign a year
acs5_data_2017$YEAR <- 2017
acs5_data_2018$YEAR <- 2018
acs5_data_2019$YEAR <- 2019

# Remove measures of error
acs5_data_2017 <- acs5_data_2017 %>% dplyr::select(-ends_with('M'))
acs5_data_2018 <- acs5_data_2018 %>% dplyr::select(-ends_with('M'))
acs5_data_2019 <- acs5_data_2019 %>% dplyr::select(-ends_with('M'))

# Carry out some variable transformations. 
acs5_data_2017 <- acs5_data_2017 %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(med_inc = inc_medE,
         tot_pop = pop_totE, 
         tot_male = pop_maleE,
         tot_female = pop_femaleE,
         pct_male = pop_maleE/pop_totE,
         pct_female = pop_femaleE/pop_totE,
         tot_white = hisp_no_whtE + hisp_yes_whtE,
         tot_black = hisp_no_blkE + hisp_yes_blkE,
         tot_other = tot_pop - (tot_white+tot_black),
         pct_white = (hisp_no_whtE + hisp_yes_whtE)/hisp_totE,
         pct_black = (hisp_no_blkE + hisp_yes_blkE)/hisp_totE,
         pct_other = 1-(pct_white+pct_black),
         #pct_other = hisp_yes_totE/hisp_totE, NEED TO CHANGE CALCULATION!
         edu_tot_hs_grad = edu_tot_m_gr_hs_gradE + edu_tot_f_gr_hs_gradE,
         edu_tot_bach = edu_tot_m_bachE + edu_tot_f_bachE,
         pct_highschool = edu_tot_hs_grad/edu_totE,
         pct_college = edu_tot_bach/edu_totE,
         pct_pov = povE/pov_totE,
         pct_unemp = (unemp1E + unemp3E)/(unemp_tot1E + unemp_tot3E),
         pct_build = (build1E + build2E + build3E)/build_totE,
         pct_vehicle = vehicleE/vehicle_totE,
         tot_age_0_14 = age_tot_m_5belowE + age_tot_m_5_9E + age_tot_m_10_14E + 
           age_tot_f_5belowE + age_tot_f_5_9E + age_tot_f_10_14E,
         tot_age_15_19 = age_tot_m_15_17E + age_tot_m_18_19E +
           age_tot_f_15_17E + age_tot_f_18_19E,
         tot_age_20_24 = age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E + 
           age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E,
         tot_age_25_34 = age_tot_m_25_29E + age_tot_m_30_34E + age_tot_f_25_29E + age_tot_f_30_34E,
         tot_age_35_44 = age_tot_m_35_39E + age_tot_m_40_44E + age_tot_f_35_39E + age_tot_f_40_44E,
         tot_age_45_54 = age_tot_m_45_49E + age_tot_m_50_54E + age_tot_f_45_49E + age_tot_f_50_54E,
         tot_age_55_64 = age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E + age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E,
         tot_age_65plus = age_tot_m_65_66E +  age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE +
           age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE,
         age_tot_mE = age_tot_m_5belowE + age_tot_m_5_9E + age_tot_m_10_14E + age_tot_m_15_17E + age_tot_m_18_19E + age_tot_m_20E + 
           age_tot_m_21E + age_tot_m_22_24E + age_tot_m_25_29E + age_tot_m_30_34E + age_tot_m_35_39E + age_tot_m_40_44E +
           age_tot_m_45_49E + age_tot_m_50_54E + age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E + age_tot_m_65_66E + 
           age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE,
         age_tot_fE = age_tot_f_5belowE + age_tot_f_5_9E + age_tot_f_10_14E + age_tot_f_15_17E + age_tot_f_18_19E + age_tot_f_20E + 
           age_tot_f_21E + age_tot_f_22_24E + age_tot_f_25_29E + age_tot_f_30_34E + age_tot_f_35_39E + age_tot_f_40_44E +
           age_tot_f_45_49E + age_tot_f_50_54E + age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E + age_tot_f_65_66E + 
           age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE,
         pct_m_18_24 = (age_tot_m_18_19E + age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E)/(age_tot_mE),
         pct_m_25_34 = (age_tot_m_25_29E + age_tot_m_30_34E)/age_tot_mE,
         pct_m_35_44 = (age_tot_m_35_39E + age_tot_m_40_44E)/age_tot_mE,
         pct_m_45_54 = (age_tot_m_45_49E + age_tot_m_50_54E)/age_tot_mE,
         pct_m_55_64 = (age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E)/age_tot_mE,
         pct_m_65plus = (age_tot_m_65_66E + age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE)/age_tot_mE,
         pct_f_18_24 = (age_tot_f_18_19E + age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E)/age_tot_fE,
         pct_f_25_34 = (age_tot_f_25_29E + age_tot_f_30_34E)/age_tot_fE,
         pct_f_35_44 = (age_tot_f_35_39E + age_tot_f_40_44E)/age_tot_fE,
         pct_f_45_54 = (age_tot_f_45_49E + age_tot_f_50_54E)/age_tot_fE,
         pct_f_55_64 = (age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E)/age_tot_fE,
         pct_f_65plus = (age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE)/age_tot_fE, 
         pct_18_24 = (age_tot_m_18_19E + age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E + 
                        age_tot_f_18_19E + age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E)/(age_tot_mE+age_tot_fE),
         pct_25_34 = (age_tot_m_25_29E + age_tot_m_30_34E + 
                        age_tot_f_25_29E + age_tot_f_30_34E)/(age_tot_mE+age_tot_fE),
         pct_35_44 = (age_tot_m_35_39E + age_tot_m_40_44E + 
                        age_tot_f_35_39E + age_tot_f_40_44E)/(age_tot_mE+age_tot_fE),
         pct_45_54 = (age_tot_m_45_49E + age_tot_m_50_54E + 
                        age_tot_f_45_49E + age_tot_f_50_54E)/(age_tot_mE+age_tot_fE),
         pct_55_64 = (age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E +
                        age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E)/(age_tot_mE+age_tot_fE),
         pct_65plus = (age_tot_m_65_66E + age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE +
                         age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE)/(age_tot_mE+age_tot_fE),
         # Make the additional deprivation index variables
         pct_noHS = noHS_numE/noHS_denomE,
         #pct_femhead = femhead_numE/femhead_denomE,
         pct_manageprof = manageprof_numE/manageprof_denomE,
         pct_crowd = (crowd_num1E + crowd_num2E + crowd_num3E + crowd_num4E + crowd_num5E + crowd_num6E)/crowd_denomE,
         pct_pubassist = pubassist_numE/pubassist_denomE,
         pct_incunder30 = (incunder30_num1E + incunder30_num2E + incunder30_num3E + incunder30_num4E + incunder30_num5E)/incunder30_denomE) %>% 
  dplyr::select(c(GEOID, NAME, YEAR, med_inc, tot_pop, pct_male, pct_female, tot_white, tot_black, tot_other,
                  pct_white, pct_black, pct_other, edu_tot_hs_grad, edu_tot_bach, age_tot_mE, age_tot_fE,
                  pct_highschool, pct_college, pct_pov, pct_unemp, pct_build, pct_vehicle, 
                  pct_m_18_24, pct_m_25_34, pct_m_35_44, pct_m_45_54, pct_m_55_64, pct_m_65plus,
                  pct_f_18_24, pct_f_25_34, pct_f_35_44, pct_f_45_54, pct_f_55_64, pct_f_65plus,
                  pct_18_24, pct_25_34, pct_35_44, pct_45_54, pct_55_64, pct_65plus, age_tot_mE, age_tot_fE,
                  pop_maleE, pop_femaleE, tot_age_0_14, tot_age_15_19, tot_age_20_24, tot_age_25_34, 
                  tot_age_35_44, tot_age_45_54, tot_age_55_64, tot_age_65plus, tot_male, tot_female,
                  pct_noHS, pct_manageprof, pct_crowd, pct_pubassist, pct_incunder30))

acs5_data_2018 <- acs5_data_2018 %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(med_inc = inc_medE,
         tot_pop = pop_totE, 
         tot_male = pop_maleE,
         tot_female = pop_femaleE,
         pct_male = pop_maleE/pop_totE,
         pct_female = pop_femaleE/pop_totE,
         tot_white = hisp_no_whtE + hisp_yes_whtE,
         tot_black = hisp_no_blkE + hisp_yes_blkE,
         tot_other = tot_pop - (tot_white+tot_black),
         pct_white = (hisp_no_whtE + hisp_yes_whtE)/hisp_totE,
         pct_black = (hisp_no_blkE + hisp_yes_blkE)/hisp_totE,
         pct_other = 1-(pct_white+pct_black),
         #pct_other = hisp_yes_totE/hisp_totE, NEED TO CHANGE CALCULATION!
         edu_tot_hs_grad = edu_tot_m_gr_hs_gradE + edu_tot_f_gr_hs_gradE,
         edu_tot_bach = edu_tot_m_bachE + edu_tot_f_bachE,
         pct_highschool = edu_tot_hs_grad/edu_totE,
         pct_college = edu_tot_bach/edu_totE,
         pct_pov = povE/pov_totE,
         pct_unemp = (unemp1E + unemp3E)/(unemp_tot1E + unemp_tot3E),
         pct_build = (build1E + build2E + build3E)/build_totE,
         pct_vehicle = vehicleE/vehicle_totE,
         tot_age_0_14 = age_tot_m_5belowE + age_tot_m_5_9E + age_tot_m_10_14E + 
           age_tot_f_5belowE + age_tot_f_5_9E + age_tot_f_10_14E,
         tot_age_15_19 = age_tot_m_15_17E + age_tot_m_18_19E +
           age_tot_f_15_17E + age_tot_f_18_19E,
         tot_age_20_24 = age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E + 
           age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E,
         tot_age_25_34 = age_tot_m_25_29E + age_tot_m_30_34E + age_tot_f_25_29E + age_tot_f_30_34E,
         tot_age_35_44 = age_tot_m_35_39E + age_tot_m_40_44E + age_tot_f_35_39E + age_tot_f_40_44E,
         tot_age_45_54 = age_tot_m_45_49E + age_tot_m_50_54E + age_tot_f_45_49E + age_tot_f_50_54E,
         tot_age_55_64 = age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E + age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E,
         tot_age_65plus = age_tot_m_65_66E +  age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE +
           age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE,
         age_tot_mE = age_tot_m_5belowE + age_tot_m_5_9E + age_tot_m_10_14E + age_tot_m_15_17E + age_tot_m_18_19E + age_tot_m_20E + 
           age_tot_m_21E + age_tot_m_22_24E + age_tot_m_25_29E + age_tot_m_30_34E + age_tot_m_35_39E + age_tot_m_40_44E +
           age_tot_m_45_49E + age_tot_m_50_54E + age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E + age_tot_m_65_66E + 
           age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE,
         age_tot_fE = age_tot_f_5belowE + age_tot_f_5_9E + age_tot_f_10_14E + age_tot_f_15_17E + age_tot_f_18_19E + age_tot_f_20E + 
           age_tot_f_21E + age_tot_f_22_24E + age_tot_f_25_29E + age_tot_f_30_34E + age_tot_f_35_39E + age_tot_f_40_44E +
           age_tot_f_45_49E + age_tot_f_50_54E + age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E + age_tot_f_65_66E + 
           age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE,
         pct_m_18_24 = (age_tot_m_18_19E + age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E)/(age_tot_mE),
         pct_m_25_34 = (age_tot_m_25_29E + age_tot_m_30_34E)/age_tot_mE,
         pct_m_35_44 = (age_tot_m_35_39E + age_tot_m_40_44E)/age_tot_mE,
         pct_m_45_54 = (age_tot_m_45_49E + age_tot_m_50_54E)/age_tot_mE,
         pct_m_55_64 = (age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E)/age_tot_mE,
         pct_m_65plus = (age_tot_m_65_66E + age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE)/age_tot_mE,
         pct_f_18_24 = (age_tot_f_18_19E + age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E)/age_tot_fE,
         pct_f_25_34 = (age_tot_f_25_29E + age_tot_f_30_34E)/age_tot_fE,
         pct_f_35_44 = (age_tot_f_35_39E + age_tot_f_40_44E)/age_tot_fE,
         pct_f_45_54 = (age_tot_f_45_49E + age_tot_f_50_54E)/age_tot_fE,
         pct_f_55_64 = (age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E)/age_tot_fE,
         pct_f_65plus = (age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE)/age_tot_fE, 
         pct_18_24 = (age_tot_m_18_19E + age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E + 
                        age_tot_f_18_19E + age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E)/(age_tot_mE+age_tot_fE),
         pct_25_34 = (age_tot_m_25_29E + age_tot_m_30_34E + 
                        age_tot_f_25_29E + age_tot_f_30_34E)/(age_tot_mE+age_tot_fE),
         pct_35_44 = (age_tot_m_35_39E + age_tot_m_40_44E + 
                        age_tot_f_35_39E + age_tot_f_40_44E)/(age_tot_mE+age_tot_fE),
         pct_45_54 = (age_tot_m_45_49E + age_tot_m_50_54E + 
                        age_tot_f_45_49E + age_tot_f_50_54E)/(age_tot_mE+age_tot_fE),
         pct_55_64 = (age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E +
                        age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E)/(age_tot_mE+age_tot_fE),
         pct_65plus = (age_tot_m_65_66E + age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE +
                         age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE)/(age_tot_mE+age_tot_fE),
         # Make the additional deprivation index variables
         pct_noHS = noHS_numE/noHS_denomE,
         #pct_femhead = femhead_numE/femhead_denomE,
         pct_manageprof = manageprof_numE/manageprof_denomE,
         pct_crowd = (crowd_num1E + crowd_num2E + crowd_num3E + crowd_num4E + crowd_num5E + crowd_num6E)/crowd_denomE,
         pct_pubassist = pubassist_numE/pubassist_denomE,
         pct_incunder30 = (incunder30_num1E + incunder30_num2E + incunder30_num3E + incunder30_num4E + incunder30_num5E)/incunder30_denomE) %>% 
  dplyr::select(c(GEOID, NAME, YEAR, med_inc, tot_pop, pct_male, pct_female, tot_white, tot_black, tot_other,
                  pct_white, pct_black, pct_other, edu_tot_hs_grad, edu_tot_bach, age_tot_mE, age_tot_fE,
                  pct_highschool, pct_college, pct_pov, pct_unemp, pct_build, pct_vehicle, 
                  pct_m_18_24, pct_m_25_34, pct_m_35_44, pct_m_45_54, pct_m_55_64, pct_m_65plus,
                  pct_f_18_24, pct_f_25_34, pct_f_35_44, pct_f_45_54, pct_f_55_64, pct_f_65plus,
                  pct_18_24, pct_25_34, pct_35_44, pct_45_54, pct_55_64, pct_65plus, age_tot_mE, age_tot_fE,
                  pop_maleE, pop_femaleE, tot_age_0_14, tot_age_15_19, tot_age_20_24, tot_age_25_34, 
                  tot_age_35_44, tot_age_45_54, tot_age_55_64, tot_age_65plus, tot_male, tot_female,
                  pct_noHS, pct_manageprof, pct_crowd, pct_pubassist, pct_incunder30))

acs5_data_2019 <- acs5_data_2019 %>% 
  group_by(GEOID, NAME, YEAR) %>% 
  mutate(med_inc = inc_medE,
         tot_pop = pop_totE, 
         tot_male = pop_maleE,
         tot_female = pop_femaleE,
         pct_male = pop_maleE/pop_totE,
         pct_female = pop_femaleE/pop_totE,
         tot_white = hisp_no_whtE + hisp_yes_whtE,
         tot_black = hisp_no_blkE + hisp_yes_blkE,
         tot_other = tot_pop - (tot_white+tot_black),
         pct_white = (hisp_no_whtE + hisp_yes_whtE)/hisp_totE,
         pct_black = (hisp_no_blkE + hisp_yes_blkE)/hisp_totE,
         pct_other = 1-(pct_white+pct_black),
         #pct_other = hisp_yes_totE/hisp_totE, NEED TO CHANGE CALCULATION!
         edu_tot_hs_grad = edu_tot_m_gr_hs_gradE + edu_tot_f_gr_hs_gradE,
         edu_tot_bach = edu_tot_m_bachE + edu_tot_f_bachE,
         pct_highschool = edu_tot_hs_grad/edu_totE,
         pct_college = edu_tot_bach/edu_totE,
         pct_pov = povE/pov_totE,
         pct_unemp = (unemp1E + unemp3E)/(unemp_tot1E + unemp_tot3E),
         pct_build = (build1E + build2E + build3E)/build_totE,
         pct_vehicle = vehicleE/vehicle_totE,
         tot_age_0_14 = age_tot_m_5belowE + age_tot_m_5_9E + age_tot_m_10_14E + 
           age_tot_f_5belowE + age_tot_f_5_9E + age_tot_f_10_14E,
         tot_age_15_19 = age_tot_m_15_17E + age_tot_m_18_19E +
           age_tot_f_15_17E + age_tot_f_18_19E,
         tot_age_20_24 = age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E + 
           age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E,
         tot_age_25_34 = age_tot_m_25_29E + age_tot_m_30_34E + age_tot_f_25_29E + age_tot_f_30_34E,
         tot_age_35_44 = age_tot_m_35_39E + age_tot_m_40_44E + age_tot_f_35_39E + age_tot_f_40_44E,
         tot_age_45_54 = age_tot_m_45_49E + age_tot_m_50_54E + age_tot_f_45_49E + age_tot_f_50_54E,
         tot_age_55_64 = age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E + age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E,
         tot_age_65plus = age_tot_m_65_66E +  age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE +
           age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE,
         age_tot_mE = age_tot_m_5belowE + age_tot_m_5_9E + age_tot_m_10_14E + age_tot_m_15_17E + age_tot_m_18_19E + age_tot_m_20E + 
           age_tot_m_21E + age_tot_m_22_24E + age_tot_m_25_29E + age_tot_m_30_34E + age_tot_m_35_39E + age_tot_m_40_44E +
           age_tot_m_45_49E + age_tot_m_50_54E + age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E + age_tot_m_65_66E + 
           age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE,
         age_tot_fE = age_tot_f_5belowE + age_tot_f_5_9E + age_tot_f_10_14E + age_tot_f_15_17E + age_tot_f_18_19E + age_tot_f_20E + 
           age_tot_f_21E + age_tot_f_22_24E + age_tot_f_25_29E + age_tot_f_30_34E + age_tot_f_35_39E + age_tot_f_40_44E +
           age_tot_f_45_49E + age_tot_f_50_54E + age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E + age_tot_f_65_66E + 
           age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE,
         pct_m_18_24 = (age_tot_m_18_19E + age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E)/(age_tot_mE),
         pct_m_25_34 = (age_tot_m_25_29E + age_tot_m_30_34E)/age_tot_mE,
         pct_m_35_44 = (age_tot_m_35_39E + age_tot_m_40_44E)/age_tot_mE,
         pct_m_45_54 = (age_tot_m_45_49E + age_tot_m_50_54E)/age_tot_mE,
         pct_m_55_64 = (age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E)/age_tot_mE,
         pct_m_65plus = (age_tot_m_65_66E + age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE)/age_tot_mE,
         pct_f_18_24 = (age_tot_f_18_19E + age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E)/age_tot_fE,
         pct_f_25_34 = (age_tot_f_25_29E + age_tot_f_30_34E)/age_tot_fE,
         pct_f_35_44 = (age_tot_f_35_39E + age_tot_f_40_44E)/age_tot_fE,
         pct_f_45_54 = (age_tot_f_45_49E + age_tot_f_50_54E)/age_tot_fE,
         pct_f_55_64 = (age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E)/age_tot_fE,
         pct_f_65plus = (age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE)/age_tot_fE, 
         pct_18_24 = (age_tot_m_18_19E + age_tot_m_20E + age_tot_m_21E + age_tot_m_22_24E + 
                        age_tot_f_18_19E + age_tot_f_20E + age_tot_f_21E + age_tot_f_22_24E)/(age_tot_mE+age_tot_fE),
         pct_25_34 = (age_tot_m_25_29E + age_tot_m_30_34E + 
                        age_tot_f_25_29E + age_tot_f_30_34E)/(age_tot_mE+age_tot_fE),
         pct_35_44 = (age_tot_m_35_39E + age_tot_m_40_44E + 
                        age_tot_f_35_39E + age_tot_f_40_44E)/(age_tot_mE+age_tot_fE),
         pct_45_54 = (age_tot_m_45_49E + age_tot_m_50_54E + 
                        age_tot_f_45_49E + age_tot_f_50_54E)/(age_tot_mE+age_tot_fE),
         pct_55_64 = (age_tot_m_55_59E + age_tot_m_60_61E + age_tot_m_62_64E +
                        age_tot_f_55_59E + age_tot_f_60_61E + age_tot_f_62_64E)/(age_tot_mE+age_tot_fE),
         pct_65plus = (age_tot_m_65_66E + age_tot_m_67_69E + age_tot_m_70_74E + age_tot_m_75_79E + age_tot_m_80_84E + age_tot_m_85_plusE +
                         age_tot_f_65_66E + age_tot_f_67_69E + age_tot_f_70_74E + age_tot_f_75_79E + age_tot_f_80_84E + age_tot_f_85_plusE)/(age_tot_mE+age_tot_fE),
         # Make the additional deprivation index variables
         pct_noHS = noHS_numE/noHS_denomE,
         #pct_femhead = femhead_numE/femhead_denomE,
         pct_manageprof = manageprof_numE/manageprof_denomE,
         pct_crowd = (crowd_num1E + crowd_num2E + crowd_num3E + crowd_num4E + crowd_num5E + crowd_num6E)/crowd_denomE,
         pct_pubassist = pubassist_numE/pubassist_denomE,
         pct_incunder30 = (incunder30_num1E + incunder30_num2E + incunder30_num3E + incunder30_num4E + incunder30_num5E)/incunder30_denomE) %>% 
  dplyr::select(c(GEOID, NAME, YEAR, med_inc, tot_pop, pct_male, pct_female, tot_white, tot_black, tot_other,
                  pct_white, pct_black, pct_other, edu_tot_hs_grad, edu_tot_bach, age_tot_mE, age_tot_fE,
                  pct_highschool, pct_college, pct_pov, pct_unemp, pct_build, pct_vehicle, 
                  pct_m_18_24, pct_m_25_34, pct_m_35_44, pct_m_45_54, pct_m_55_64, pct_m_65plus,
                  pct_f_18_24, pct_f_25_34, pct_f_35_44, pct_f_45_54, pct_f_55_64, pct_f_65plus,
                  pct_18_24, pct_25_34, pct_35_44, pct_45_54, pct_55_64, pct_65plus, age_tot_mE, age_tot_fE,
                  pop_maleE, pop_femaleE, tot_age_0_14, tot_age_15_19, tot_age_20_24, tot_age_25_34, 
                  tot_age_35_44, tot_age_45_54, tot_age_55_64, tot_age_65plus, tot_male, tot_female,
                  pct_noHS, pct_manageprof, pct_crowd, pct_pubassist, pct_incunder30))


### Limit Mpls stop data and census data to those inside the Mpls city boundary

# Load Mpls city boundary
MplsBoundary <-
  st_read(
    './data/raw/MPLS_CB/City_Boundary-shp/16cdbbfa-ad10-493c-afaf-52b61f2e76e42020329-1-180h9ap.whbo.shp'
  )

# Transform to match census tract information
MplsBoundary <- st_transform(MplsBoundary, st_crs(acs5_data_2019))

# Identify those census tracts that intersect with the city boundary
HennepinCountyInt2017 <-
  acs5_data_2017[st_intersects(acs5_data_2017, MplsBoundary, sparse = FALSE)[, 1],]
HennepinCountyInt2018 <-
  acs5_data_2018[st_intersects(acs5_data_2018, MplsBoundary, sparse = FALSE)[, 1],]
HennepinCountyInt2019 <-
  acs5_data_2019[st_intersects(acs5_data_2019, MplsBoundary, sparse = FALSE)[, 1],]

# After manual inspection we see that a few CT touch the city boundary,
# but they are not within the city limits and have very few or no stops.
# FID include: 102, 57, 128, 100, 59, 14, 5, 37, 54, 55, 94, 130, 95, 124,
# 101, 127, 131
badGEOID <- c(
  "27053020400",
  "27053021200",
  "27053021700",
  "27053021800",
  "27053022000",
  "27053022801",
  "27053022802",
  "27053022901",
  "27053023100",
  "27053023600",
  "27053024700",
  "27053024901",
  "27053980000",
  "27053020400",
  "27053020102",
  "27053020101",
  "27053020500"
)

# Remove these FID
HennepinCountyInt2017 <-
  subset(HennepinCountyInt2017, !(GEOID %in% badGEOID))
HennepinCountyInt2018 <-
  subset(HennepinCountyInt2018, !(GEOID %in% badGEOID))
HennepinCountyInt2019 <-
  subset(HennepinCountyInt2019, !(GEOID %in% badGEOID))

### Clip police stops as a few are geocoded outside the city ----

# Convert Mpls into spatial object
Mpls <- st_as_sf(Mpls, coords = c("lon", "lat"),
                 crs = 4326)

# Set CRS of police stops to match that of other Mpls datasets
Mpls <- st_transform(Mpls, st_crs(MplsBoundary))

# Clip the points
Mpls <- Mpls[MplsBoundary,]

### Intersect Mpls police stop points with the HennepinCountyInt object ----

# Separate MPLS object into each year
Mpls2017 <- Mpls %>%
  dplyr::filter(year == 2017)

Mpls2018 <- Mpls %>%
  dplyr::filter(year == 2018)

Mpls2019 <- Mpls %>%
  dplyr::filter(year == 2019)

# Create an intersected count of police stops in each census tract
MplsTempYearInt2017 <-
  st_intersection(HennepinCountyInt2017, Mpls2017)
MplsTempYearInt2018 <-
  st_intersection(HennepinCountyInt2018, Mpls2018)
MplsTempYearInt2019 <-
  st_intersection(HennepinCountyInt2019, Mpls2019)

# Sum the total number of stops by census tract for each year
MplsTempYearIntAgg2017 <- MplsTempYearInt2017 %>%
    group_by_('NAME') %>%
    summarize(
      # All stops
      NStopsAsian = sum(raceRecode == 'Asian'),
      NStopsBlack = sum(raceRecode == 'Black'),
      NStopsLatin = sum(raceRecode == 'Latino'),
      NStopsNatAm = sum(raceRecode == 'Native American'),
      NStopsOther = sum(raceRecode == 'Other'),
      NStopsUnkwn = sum(raceRecode == 'Unknown'),
      NStopsWhite = sum(raceRecode == 'White'),
      NStopsTotal = n(),
      # Before Sunset (BS)
      NStopsAsianBS = sum(raceRecode == 'Asian' & veilBeforeSunset == 1),
      NStopsBlackBS = sum(raceRecode == 'Black' & veilBeforeSunset == 1),
      NStopsLatinBS = sum(raceRecode == 'Latino' & veilBeforeSunset == 1),
      NStopsNatAmBS = sum(raceRecode == 'Native American' & veilBeforeSunset == 1),
      NStopsOtherBS = sum(raceRecode == 'Other' & veilBeforeSunset == 1),
      NStopsUnkwnBS = sum(raceRecode == 'Unknown' & veilBeforeSunset == 1),
      NStopsWhiteBS = sum(raceRecode == 'White' & veilBeforeSunset == 1),
      NStopsTotalBS = sum(veilBeforeSunset == 1),
      # After Sunset (AS)
      NStopsAsianAS = sum(raceRecode == 'Asian' & veilAfterSunset == 1),
      NStopsBlackAS = sum(raceRecode == 'Black' & veilAfterSunset == 1),
      NStopsLatinAS = sum(raceRecode == 'Latino' & veilAfterSunset == 1),
      NStopsNatAmAS = sum(raceRecode == 'Native American' & veilAfterSunset == 1),
      NStopsOtherAS = sum(raceRecode == 'Other' & veilAfterSunset == 1),
      NStopsUnkwnAS = sum(raceRecode == 'Unknown' & veilAfterSunset == 1),
      NStopsWhiteAS = sum(raceRecode == 'White' & veilAfterSunset == 1),
      NStopsTotalAS = sum(veilAfterSunset == 1),
      # Person Searches (IS)
      NStopsAsianPS = sum(raceRecode == 'Asian' & personSearch == 'YES'),
      NStopsBlackPS = sum(raceRecode == 'Black' & personSearch == 'YES'),
      NStopsLatinPS = sum(raceRecode == 'Latino' & personSearch == 'YES'),
      NStopsNatAmPS = sum(raceRecode == 'Native American' & personSearch == 'YES'),
      NStopsOtherPS = sum(raceRecode == 'Other' & personSearch == 'YES'),
      NStopsUnkwnPS = sum(raceRecode == 'Unknown' & personSearch == 'YES'),
      NStopsWhitePS = sum(raceRecode == 'White' & personSearch == 'YES'),
      NStopsTotalPS = sum(personSearch == 'YES'),
      # Vehicle Searches (VS)
      NStopsAsianVS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES'),
      NStopsBlackVS = sum(raceRecode == 'Black' & vehicleSearch == 'YES'),
      NStopsLatinVS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES'),
      NStopsNatAmVS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES'),
      NStopsOtherVS = sum(raceRecode == 'Other' & vehicleSearch == 'YES'),
      NStopsUnkwnVS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES'),
      NStopsWhiteVS = sum(raceRecode == 'White' & vehicleSearch == 'YES'),
      NStopsTotalVS = sum(vehicleSearch == 'YES'),
      # Person Searches (IS) Before Sunset (BS)
      NStopsAsianPSBS = sum(raceRecode == 'Asian' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsBlackPSBS = sum(raceRecode == 'Black' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsLatinPSBS = sum(raceRecode == 'Latino' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsNatAmPSBS = sum(raceRecode == 'Native American' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsOtherPSBS = sum(raceRecode == 'Other' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsUnkwnPSBS = sum(raceRecode == 'Unknown' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsWhitePSBS = sum(raceRecode == 'White' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsTotalPSBS = sum(personSearch == 'YES' & veilBeforeSunset == 1),
      # Vehicle Searches (VS) Before Sunset (BS)
      NStopsAsianVSBS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsBlackVSBS = sum(raceRecode == 'Black' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsLatinVSBS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsNatAmVSBS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsOtherVSBS = sum(raceRecode == 'Other' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsUnkwnVSBS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsWhiteVSBS = sum(raceRecode == 'White' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsTotalVSBS = sum(vehicleSearch == 'YES' & veilBeforeSunset == 1),
      # Person Searches (IS) After Sunset (AS)
      NStopsAsianPSAS = sum(raceRecode == 'Asian' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsBlackPSAS = sum(raceRecode == 'Black' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsLatinPSAS = sum(raceRecode == 'Latino' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsNatAmPSAS = sum(raceRecode == 'Native American' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsOtherPSAS = sum(raceRecode == 'Other' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsUnkwnPSAS = sum(raceRecode == 'Unknown' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsWhitePSAS = sum(raceRecode == 'White' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsTotalPSAS = sum(personSearch == 'YES' & veilAfterSunset == 1),
      # Vehicle Searches (VS) After Sunset (AS)
      NStopsAsianVSAS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsBlackVSAS = sum(raceRecode == 'Black' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsLatinVSAS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsNatAmVSAS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsOtherVSAS = sum(raceRecode == 'Other' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsUnkwnVSAS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsWhiteVSAS = sum(raceRecode == 'White' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsTotalVSAS = sum(vehicleSearch == 'YES' & veilAfterSunset == 1))

MplsTempYearIntAgg2018 <- MplsTempYearInt2018 %>%
    group_by_('NAME') %>%
    summarize(
      # All stops
      NStopsAsian = sum(raceRecode == 'Asian'),
      NStopsBlack = sum(raceRecode == 'Black'),
      NStopsLatin = sum(raceRecode == 'Latino'),
      NStopsNatAm = sum(raceRecode == 'Native American'),
      NStopsOther = sum(raceRecode == 'Other'),
      NStopsUnkwn = sum(raceRecode == 'Unknown'),
      NStopsWhite = sum(raceRecode == 'White'),
      NStopsTotal = n(),
      # Before Sunset (BS)
      NStopsAsianBS = sum(raceRecode == 'Asian' & veilBeforeSunset == 1),
      NStopsBlackBS = sum(raceRecode == 'Black' & veilBeforeSunset == 1),
      NStopsLatinBS = sum(raceRecode == 'Latino' & veilBeforeSunset == 1),
      NStopsNatAmBS = sum(raceRecode == 'Native American' & veilBeforeSunset == 1),
      NStopsOtherBS = sum(raceRecode == 'Other' & veilBeforeSunset == 1),
      NStopsUnkwnBS = sum(raceRecode == 'Unknown' & veilBeforeSunset == 1),
      NStopsWhiteBS = sum(raceRecode == 'White' & veilBeforeSunset == 1),
      NStopsTotalBS = sum(veilBeforeSunset == 1),
      # After Sunset (AS)
      NStopsAsianAS = sum(raceRecode == 'Asian' & veilAfterSunset == 1),
      NStopsBlackAS = sum(raceRecode == 'Black' & veilAfterSunset == 1),
      NStopsLatinAS = sum(raceRecode == 'Latino' & veilAfterSunset == 1),
      NStopsNatAmAS = sum(raceRecode == 'Native American' & veilAfterSunset == 1),
      NStopsOtherAS = sum(raceRecode == 'Other' & veilAfterSunset == 1),
      NStopsUnkwnAS = sum(raceRecode == 'Unknown' & veilAfterSunset == 1),
      NStopsWhiteAS = sum(raceRecode == 'White' & veilAfterSunset == 1),
      NStopsTotalAS = sum(veilAfterSunset == 1),
      # Person Searches (PS)
      NStopsAsianPS = sum(raceRecode == 'Asian' & personSearch == 'YES'),
      NStopsBlackPS = sum(raceRecode == 'Black' & personSearch == 'YES'),
      NStopsLatinPS = sum(raceRecode == 'Latino' & personSearch == 'YES'),
      NStopsNatAmPS = sum(raceRecode == 'Native American' & personSearch == 'YES'),
      NStopsOtherPS = sum(raceRecode == 'Other' & personSearch == 'YES'),
      NStopsUnkwnPS = sum(raceRecode == 'Unknown' & personSearch == 'YES'),
      NStopsWhitePS = sum(raceRecode == 'White' & personSearch == 'YES'),
      NStopsTotalPS = sum(personSearch == 'YES'),
      # Vehicle Searches (VS)
      NStopsAsianVS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES'),
      NStopsBlackVS = sum(raceRecode == 'Black' & vehicleSearch == 'YES'),
      NStopsLatinVS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES'),
      NStopsNatAmVS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES'),
      NStopsOtherVS = sum(raceRecode == 'Other' & vehicleSearch == 'YES'),
      NStopsUnkwnVS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES'),
      NStopsWhiteVS = sum(raceRecode == 'White' & vehicleSearch == 'YES'),
      NStopsTotalVS = sum(vehicleSearch == 'YES'),
      # Person Searches (IS) Before Sunset (BS)
      NStopsAsianPSBS = sum(raceRecode == 'Asian' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsBlackPSBS = sum(raceRecode == 'Black' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsLatinPSBS = sum(raceRecode == 'Latino' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsNatAmPSBS = sum(raceRecode == 'Native American' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsOtherPSBS = sum(raceRecode == 'Other' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsUnkwnPSBS = sum(raceRecode == 'Unknown' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsWhitePSBS = sum(raceRecode == 'White' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsTotalPSBS = sum(personSearch == 'YES' & veilBeforeSunset == 1),
      # Vehicle Searches (VS) Before Sunset (BS)
      NStopsAsianVSBS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsBlackVSBS = sum(raceRecode == 'Black' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsLatinVSBS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsNatAmVSBS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsOtherVSBS = sum(raceRecode == 'Other' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsUnkwnVSBS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsWhiteVSBS = sum(raceRecode == 'White' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsTotalVSBS = sum(vehicleSearch == 'YES' & veilBeforeSunset == 1),
      # Person Searches (IS) After Sunset (AS)
      NStopsAsianPSAS = sum(raceRecode == 'Asian' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsBlackPSAS = sum(raceRecode == 'Black' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsLatinPSAS = sum(raceRecode == 'Latino' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsNatAmPSAS = sum(raceRecode == 'Native American' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsOtherPSAS = sum(raceRecode == 'Other' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsUnkwnPSAS = sum(raceRecode == 'Unknown' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsWhitePSAS = sum(raceRecode == 'White' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsTotalPSAS = sum(personSearch == 'YES' & veilAfterSunset == 1),
      # Vehicle Searches (VS) After Sunset (AS)
      NStopsAsianVSAS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsBlackVSAS = sum(raceRecode == 'Black' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsLatinVSAS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsNatAmVSAS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsOtherVSAS = sum(raceRecode == 'Other' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsUnkwnVSAS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsWhiteVSAS = sum(raceRecode == 'White' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsTotalVSAS = sum(vehicleSearch == 'YES' & veilAfterSunset == 1))

MplsTempYearIntAgg2019 <- MplsTempYearInt2019 %>%
    group_by_('NAME') %>%
    summarize(
      # All stops
      NStopsAsian = sum(raceRecode == 'Asian'),
      NStopsBlack = sum(raceRecode == 'Black'),
      NStopsLatin = sum(raceRecode == 'Latino'),
      NStopsNatAm = sum(raceRecode == 'Native American'),
      NStopsOther = sum(raceRecode == 'Other'),
      NStopsUnkwn = sum(raceRecode == 'Unknown'),
      NStopsWhite = sum(raceRecode == 'White'),
      NStopsTotal = n(),
      # Before Sunset (BS)
      NStopsAsianBS = sum(raceRecode == 'Asian' & veilBeforeSunset == 1),
      NStopsBlackBS = sum(raceRecode == 'Black' & veilBeforeSunset == 1),
      NStopsLatinBS = sum(raceRecode == 'Latino' & veilBeforeSunset == 1),
      NStopsNatAmBS = sum(raceRecode == 'Native American' & veilBeforeSunset == 1),
      NStopsOtherBS = sum(raceRecode == 'Other' & veilBeforeSunset == 1),
      NStopsUnkwnBS = sum(raceRecode == 'Unknown' & veilBeforeSunset == 1),
      NStopsWhiteBS = sum(raceRecode == 'White' & veilBeforeSunset == 1),
      NStopsTotalBS = sum(veilBeforeSunset == 1),
      # After Sunset (AS)
      NStopsAsianAS = sum(raceRecode == 'Asian' & veilAfterSunset == 1),
      NStopsBlackAS = sum(raceRecode == 'Black' & veilAfterSunset == 1),
      NStopsLatinAS = sum(raceRecode == 'Latino' & veilAfterSunset == 1),
      NStopsNatAmAS = sum(raceRecode == 'Native American' & veilAfterSunset == 1),
      NStopsOtherAS = sum(raceRecode == 'Other' & veilAfterSunset == 1),
      NStopsUnkwnAS = sum(raceRecode == 'Unknown' & veilAfterSunset == 1),
      NStopsWhiteAS = sum(raceRecode == 'White' & veilAfterSunset == 1),
      NStopsTotalAS = sum(veilAfterSunset == 1),
      # Person Searches (PS)
      NStopsAsianPS = sum(raceRecode == 'Asian' & personSearch == 'YES'),
      NStopsBlackPS = sum(raceRecode == 'Black' & personSearch == 'YES'),
      NStopsLatinPS = sum(raceRecode == 'Latino' & personSearch == 'YES'),
      NStopsNatAmPS = sum(raceRecode == 'Native American' & personSearch == 'YES'),
      NStopsOtherPS = sum(raceRecode == 'Other' & personSearch == 'YES'),
      NStopsUnkwnPS = sum(raceRecode == 'Unknown' & personSearch == 'YES'),
      NStopsWhitePS = sum(raceRecode == 'White' & personSearch == 'YES'),
      NStopsTotalPS = sum(personSearch == 'YES'),
      # Vehicle Searches (VS)
      NStopsAsianVS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES'),
      NStopsBlackVS = sum(raceRecode == 'Black' & vehicleSearch == 'YES'),
      NStopsLatinVS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES'),
      NStopsNatAmVS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES'),
      NStopsOtherVS = sum(raceRecode == 'Other' & vehicleSearch == 'YES'),
      NStopsUnkwnVS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES'),
      NStopsWhiteVS = sum(raceRecode == 'White' & vehicleSearch == 'YES'),
      NStopsTotalVS = sum(vehicleSearch == 'YES'),
      # Person Searches (IS) Before Sunset (BS)
      NStopsAsianPSBS = sum(raceRecode == 'Asian' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsBlackPSBS = sum(raceRecode == 'Black' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsLatinPSBS = sum(raceRecode == 'Latino' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsNatAmPSBS = sum(raceRecode == 'Native American' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsOtherPSBS = sum(raceRecode == 'Other' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsUnkwnPSBS = sum(raceRecode == 'Unknown' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsWhitePSBS = sum(raceRecode == 'White' & personSearch == 'YES' & veilBeforeSunset == 1),
      NStopsTotalPSBS = sum(personSearch == 'YES' & veilBeforeSunset == 1),
      # Vehicle Searches (VS) Before Sunset (BS)
      NStopsAsianVSBS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsBlackVSBS = sum(raceRecode == 'Black' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsLatinVSBS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsNatAmVSBS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsOtherVSBS = sum(raceRecode == 'Other' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsUnkwnVSBS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsWhiteVSBS = sum(raceRecode == 'White' & vehicleSearch == 'YES' & veilBeforeSunset == 1),
      NStopsTotalVSBS = sum(vehicleSearch == 'YES' & veilBeforeSunset == 1),
      # Person Searches (IS) After Sunset (AS)
      NStopsAsianPSAS = sum(raceRecode == 'Asian' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsBlackPSAS = sum(raceRecode == 'Black' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsLatinPSAS = sum(raceRecode == 'Latino' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsNatAmPSAS = sum(raceRecode == 'Native American' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsOtherPSAS = sum(raceRecode == 'Other' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsUnkwnPSAS = sum(raceRecode == 'Unknown' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsWhitePSAS = sum(raceRecode == 'White' & personSearch == 'YES' & veilAfterSunset == 1),
      NStopsTotalPSAS = sum(personSearch == 'YES' & veilAfterSunset == 1),
      # Vehicle Searches (VS) After Sunset (AS)
      NStopsAsianVSAS = sum(raceRecode == 'Asian' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsBlackVSAS = sum(raceRecode == 'Black' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsLatinVSAS = sum(raceRecode == 'Latino' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsNatAmVSAS = sum(raceRecode == 'Native American' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsOtherVSAS = sum(raceRecode == 'Other' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsUnkwnVSAS = sum(raceRecode == 'Unknown' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsWhiteVSAS = sum(raceRecode == 'White' & vehicleSearch == 'YES' & veilAfterSunset == 1),
      NStopsTotalVSAS = sum(vehicleSearch == 'YES' & veilAfterSunset == 1))

# Add back a single geometry to the 2018 and 2019 data as there were
# no recorded stops there. Check the geoms and compare to see which are empty.
which(MplsTempYearIntAgg2017$NAME %in% MplsTempYearIntAgg2018$NAME == F)
MplsTempYearIntAgg2017$NAME[97]

# Remove this geom from 2017
MplsTempYearIntAgg2017 <- filter(MplsTempYearIntAgg2017, NAME != MplsTempYearIntAgg2017$NAME[97])

# Rejoin to the HennepinCountInt 20XX data

# 2017
HennepinCountyInt2017Counts <-
  inner_join(
    HennepinCountyInt2017 %>% as.data.frame(),
    MplsTempYearIntAgg2017 %>% as.data.frame(),
    by = "NAME"
  )

HennepinCountyInt2017Counts <-
  HennepinCountyInt2017Counts %>% st_sf(sf_column_name = 'geometry.x')

# 2018
HennepinCountyInt2018Counts <-
  inner_join(
    HennepinCountyInt2018 %>% as.data.frame(),
    MplsTempYearIntAgg2018 %>% as.data.frame(),
    by = "NAME"
  )

HennepinCountyInt2018Counts <-
  HennepinCountyInt2018Counts %>% st_sf(sf_column_name = 'geometry.x')

# 2019
HennepinCountyInt2019Counts <-
  inner_join(
    HennepinCountyInt2019 %>% as.data.frame(),
    MplsTempYearIntAgg2019 %>% as.data.frame(),
    by = "NAME"
  )

HennepinCountyInt2019Counts <-
  HennepinCountyInt2019Counts %>% st_sf(sf_column_name = 'geometry.x')

# Create rate variables by dividing by different denominators.
HennepinCountyInt2017Counts <- HennepinCountyInt2017Counts %>%
  mutate(
    # Per race (stratum) - ALL STOPS
    # Black
    NStopsBlackRatePerStratum = (NStopsBlack / tot_black) * 100,
    NStopsBlackASRatePerStratum = (NStopsBlackAS / tot_black) * 100,
    NStopsBlackBSRatePerStratum = (NStopsBlackBS / tot_black) * 100, 
    # White
    NStopsWhiteRatePerStratum = (NStopsWhite / tot_white) * 100,
    NStopsWhiteASRatePerStratum = (NStopsWhiteAS / tot_white) * 100,
    NStopsWhiteBSRatePerStratum = (NStopsWhiteBS / tot_white) * 100, 
    # Per capita (census tract total) - ALL STOPS
    # Total
    NStopsTotalRatePerCapita = (NStopsTotal / tot_pop) * 100,
    NStopsTotalASRatePerCapita = (NStopsTotalAS / tot_pop) * 100,
    NStopsTotalBSRatePerCapita = (NStopsTotalBS / tot_pop) * 100,
    # Black
    NStopsBlackRatePerCapita = (NStopsBlack / tot_pop) * 100,
    NStopsBlackASRatePerCapita = (NStopsBlackAS / tot_pop) * 100,
    NStopsBlackBSRatePerCapita = (NStopsBlackBS / tot_pop) * 100, 
    # White
    NStopsWhiteRatePerCapita = (NStopsWhite / tot_pop) * 100,
    NStopsWhiteASRatePerCapita = (NStopsWhiteAS / tot_pop) * 100,
    NStopsWhiteBSRatePerCapita = (NStopsWhiteBS / tot_pop) * 100,
    # Per city race total population - ALL STOPS
    # Black
    NStopsBlackRatePerCapitaMPLS = (NStopsBlack / sum(tot_black, na.rm = T)) * 100,
    NStopsBlackASRatePerCapitaMPLS = (NStopsBlackAS / sum(tot_black, na.rm = T)) * 100,
    NStopsBlackBSRatePerCapitaMPLS = (NStopsBlackBS / sum(tot_black, na.rm = T)) * 100, 
    # White
    NStopsWhiteRatePerCapitaMPLS = (NStopsWhite / sum(tot_white, na.rm = T)) * 100,
    NStopsWhiteASRatePerCapitaMPLS = (NStopsWhiteAS / sum(tot_white, na.rm = T)) * 100,
    NStopsWhiteBSRatePerCapitaMPLS = (NStopsWhiteBS / sum(tot_white, na.rm = T)) * 100,
    # Per race (stratum) - PERSON SEARCHES
    # Black
    NStopsBlackPSRatePerStratum = (NStopsBlackPS / tot_black) * 100,
    # White
    NStopsWhitePSRatePerStratum = (NStopsWhitePS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES
    # Black
    NStopsBlackVSRatePerStratum = (NStopsBlackVS / tot_black) * 100,
    # White
    NStopsWhiteVSRatePerStratum = (NStopsWhiteVS / tot_white) * 100,
    # Per race (stratum) - PERSON SEARCHES BEFORE SUNSET 
    # Black
    NStopsBlackPSBSRatePerStratum = (NStopsBlackPSBS / tot_black) * 100,
    # White
    NStopsWhitePSBSRatePerStratum = (NStopsWhitePSBS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES BEFORE SUNSET
    # Black
    NStopsBlackVSBSRatePerStratum = (NStopsBlackVSBS / tot_black) * 100,
    # White
    NStopsWhiteVSBSRatePerStratum = (NStopsWhiteVSBS / tot_white) * 100,
    # Per race (stratum) - PERSON SEARCHES AFTER SUNSET 
    # Black
    NStopsBlackPSASRatePerStratum = (NStopsBlackPSAS / tot_black) * 100,
    # White
    NStopsWhitePSASRatePerStratum = (NStopsWhitePSAS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES AFTER SUNSET
    # Black
    NStopsBlackVSASRatePerStratum = (NStopsBlackVSAS / tot_black) * 100,
    # White
    NStopsWhiteVSASRatePerStratum = (NStopsWhiteVSAS / tot_white) * 100
  )

HennepinCountyInt2018Counts <- HennepinCountyInt2018Counts %>%
  mutate(
    # Per race (stratum)
    # Black
    NStopsBlackRatePerStratum = (NStopsBlack / tot_black) * 100,
    NStopsBlackASRatePerStratum = (NStopsBlackAS / tot_black) * 100,
    NStopsBlackBSRatePerStratum = (NStopsBlackBS / tot_black) * 100, 
    # White
    NStopsWhiteRatePerStratum = (NStopsWhite / tot_white) * 100,
    NStopsWhiteASRatePerStratum = (NStopsWhiteAS / tot_white) * 100,
    NStopsWhiteBSRatePerStratum = (NStopsWhiteBS / tot_white) * 100, 
    # Per capita (census tract total)
    # Total
    NStopsTotalRatePerCapita = (NStopsTotal / tot_pop) * 100,
    NStopsTotalASRatePerCapita = (NStopsTotalAS / tot_pop) * 100,
    NStopsTotalBSRatePerCapita = (NStopsTotalBS / tot_pop) * 100,
    # Black
    NStopsBlackRatePerCapita = (NStopsBlack / tot_pop) * 100,
    NStopsBlackASRatePerCapita = (NStopsBlackAS / tot_pop) * 100,
    NStopsBlackBSRatePerCapita = (NStopsBlackBS / tot_pop) * 100, 
    # White
    NStopsWhiteRatePerCapita = (NStopsWhite / tot_pop) * 100,
    NStopsWhiteASRatePerCapita = (NStopsWhiteAS / tot_pop) * 100,
    NStopsWhiteBSRatePerCapita = (NStopsWhiteBS / tot_pop) * 100,
    # Per city race total population
    # Black
    NStopsBlackRatePerCapitaMPLS = (NStopsBlack / sum(tot_black, na.rm = T)) * 100,
    NStopsBlackASRatePerCapitaMPLS = (NStopsBlackAS / sum(tot_black, na.rm = T)) * 100,
    NStopsBlackBSRatePerCapitaMPLS = (NStopsBlackBS / sum(tot_black, na.rm = T)) * 100, 
    # White
    NStopsWhiteRatePerCapitaMPLS = (NStopsWhite / sum(tot_white, na.rm = T)) * 100,
    NStopsWhiteASRatePerCapitaMPLS = (NStopsWhiteAS / sum(tot_white, na.rm = T)) * 100,
    NStopsWhiteBSRatePerCapitaMPLS = (NStopsWhiteBS / sum(tot_white, na.rm = T)) * 100,
    # Per race (stratum) - INDIVIDUAL SEARCHES
    # Black
    NStopsBlackPSRatePerStratum = (NStopsBlackPS / tot_black) * 100,
    # White
    NStopsWhitePSRatePerStratum = (NStopsWhitePS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES
    # Black
    NStopsBlackVSRatePerStratum = (NStopsBlackVS / tot_black) * 100,
    # White
    NStopsWhiteVSRatePerStratum = (NStopsWhiteVS / tot_white) * 100,
    # Per race (stratum) - PERSON SEARCHES BEFORE SUNSET 
    # Black
    NStopsBlackPSBSRatePerStratum = (NStopsBlackPSBS / tot_black) * 100,
    # White
    NStopsWhitePSBSRatePerStratum = (NStopsWhitePSBS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES BEFORE SUNSET
    # Black
    NStopsBlackVSBSRatePerStratum = (NStopsBlackVSBS / tot_black) * 100,
    # White
    NStopsWhiteVSBSRatePerStratum = (NStopsWhiteVSBS / tot_white) * 100,
    # Per race (stratum) - PERSON SEARCHES AFTER SUNSET 
    # Black
    NStopsBlackPSASRatePerStratum = (NStopsBlackPSAS / tot_black) * 100,
    # White
    NStopsWhitePSASRatePerStratum = (NStopsWhitePSAS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES AFTER SUNSET
    # Black
    NStopsBlackVSASRatePerStratum = (NStopsBlackVSAS / tot_black) * 100,
    # White
    NStopsWhiteVSASRatePerStratum = (NStopsWhiteVSAS / tot_white) * 100
  )

HennepinCountyInt2019Counts <- HennepinCountyInt2019Counts %>%
  mutate(
    # Per race (stratum)
    # Black
    NStopsBlackRatePerStratum = (NStopsBlack / tot_black) * 100,
    NStopsBlackASRatePerStratum = (NStopsBlackAS / tot_black) * 100,
    NStopsBlackBSRatePerStratum = (NStopsBlackBS / tot_black) * 100, 
    # White
    NStopsWhiteRatePerStratum = (NStopsWhite / tot_white) * 100,
    NStopsWhiteASRatePerStratum = (NStopsWhiteAS / tot_white) * 100,
    NStopsWhiteBSRatePerStratum = (NStopsWhiteBS / tot_white) * 100, 
    # Per capita (census tract total)
    # Total
    NStopsTotalRatePerCapita = (NStopsTotal / tot_pop) * 100,
    NStopsTotalASRatePerCapita = (NStopsTotalAS / tot_pop) * 100,
    NStopsTotalBSRatePerCapita = (NStopsTotalBS / tot_pop) * 100,
    # Black
    NStopsBlackRatePerCapita = (NStopsBlack / tot_pop) * 100,
    NStopsBlackASRatePerCapita = (NStopsBlackAS / tot_pop) * 100,
    NStopsBlackBSRatePerCapita = (NStopsBlackBS / tot_pop) * 100, 
    # White
    NStopsWhiteRatePerCapita = (NStopsWhite / tot_pop) * 100,
    NStopsWhiteASRatePerCapita = (NStopsWhiteAS / tot_pop) * 100,
    NStopsWhiteBSRatePerCapita = (NStopsWhiteBS / tot_pop) * 100,
    # Per city race total population
    # Black
    NStopsBlackRatePerCapitaMPLS = (NStopsBlack / sum(tot_black, na.rm = T)) * 100,
    NStopsBlackASRatePerCapitaMPLS = (NStopsBlackAS / sum(tot_black, na.rm = T)) * 100,
    NStopsBlackBSRatePerCapitaMPLS = (NStopsBlackBS / sum(tot_black, na.rm = T)) * 100, 
    # White
    NStopsWhiteRatePerCapitaMPLS = (NStopsWhite / sum(tot_white, na.rm = T)) * 100,
    NStopsWhiteASRatePerCapitaMPLS = (NStopsWhiteAS / sum(tot_white, na.rm = T)) * 100,
    NStopsWhiteBSRatePerCapitaMPLS = (NStopsWhiteBS / sum(tot_white, na.rm = T)) * 100,
    # Per race (stratum) - PERSON SEARCHES
    # Black
    NStopsBlackPSRatePerStratum = (NStopsBlackPS / tot_black) * 100,
    # White
    NStopsWhitePSRatePerStratum = (NStopsWhitePS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES
    # Black
    NStopsBlackVSRatePerStratum = (NStopsBlackVS / tot_black) * 100,
    # White
    NStopsWhiteVSRatePerStratum = (NStopsWhiteVS / tot_white) * 100,
    # Per race (stratum) - PERSON SEARCHES BEFORE SUNSET 
    # Black
    NStopsBlackPSBSRatePerStratum = (NStopsBlackPSBS / tot_black) * 100,
    # White
    NStopsWhitePSBSRatePerStratum = (NStopsWhitePSBS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES BEFORE SUNSET
    # Black
    NStopsBlackVSBSRatePerStratum = (NStopsBlackVSBS / tot_black) * 100,
    # White
    NStopsWhiteVSBSRatePerStratum = (NStopsWhiteVSBS / tot_white) * 100,
    # Per race (stratum) - PERSON SEARCHES AFTER SUNSET 
    # Black
    NStopsBlackPSASRatePerStratum = (NStopsBlackPSAS / tot_black) * 100,
    # White
    NStopsWhitePSASRatePerStratum = (NStopsWhitePSAS / tot_white) * 100,
    # Per race (stratum) - VEHICLE SEARCHES AFTER SUNSET
    # Black
    NStopsBlackVSASRatePerStratum = (NStopsBlackVSAS / tot_black) * 100,
    # White
    NStopsWhiteVSASRatePerStratum = (NStopsWhiteVSAS / tot_white) * 100
  )

### Save datasets ----

###

# Create spatial points object (without 2020)
MplsTemp <- Mpls[which(Mpls$year != 2021), ]
# Save as Rdata object
saveRDS(MplsTemp, "./data/processed/MplsPoints.rds")

###

# Save cleaned MPLS stops data
outname <- "./data/processed/MplsCleaned.gpkg"
st_write(
  Mpls,
  dsn = outname,
  layer = "MplsStopsCleaned",
  driver = "GPKG",
  append = F
)

###

# Save joined census tract (CT) data for each year
outname <- "./data/processed/MplsStopsCTJoined2017.gpkg"
st_write(
  HennepinCountyInt2017Counts,
  dsn = outname,
  layer = "MplsCT2017",
  driver = "GPKG",
  append = F
)

outname <- "./data/processed/MplsStopsCTJoined2018.gpkg"
st_write(
  HennepinCountyInt2018Counts,
  dsn = outname,
  layer = "MplsCT2018",
  driver = "GPKG",
  append = F
)

outname <- "./data/processed/MplsStopsCTJoined2019.gpkg"
st_write(
  HennepinCountyInt2019Counts,
  dsn = outname,
  layer = "MplsCT2019",
  driver = "GPKG",
  append = F
)

# Combine the three joined census tract (CT) datasets into one
HennepinCountyInt2017through2019Counts <-
  rbind(
    HennepinCountyInt2017Counts,
    HennepinCountyInt2018Counts,
    HennepinCountyInt2019Counts
  )

outname <-
  "./data/processed/HennepinCountyInt2017through2019Counts.gpkg"

st_write(
  HennepinCountyInt2017through2019Counts,
  dsn = outname,
  layer = "MplsCT2017through2019",
  driver = "GPKG",
  append = F
)

### Identify those census tracts with very high stop rates ----

HennepinCountyHighCTs <- as.data.frame(subset(HennepinCountyInt2017through2019Counts, HennepinCountyInt2017through2019Counts$NStopsBlackRatePerStratum >= 100))

HennepinCountyHighCTs <- as.data.frame(HennepinCountyHighCTs[,c('NAME', 'YEAR')])

write.table(HennepinCountyHighCTs,
  file = "./Writing/ContextualInformation/VeryHighCensusTracts.txt",
  sep = "\t",
  row.names = TRUE,
  col.names = NA
)

