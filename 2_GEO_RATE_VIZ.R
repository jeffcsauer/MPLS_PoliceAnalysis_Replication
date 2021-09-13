## ---------------------------
## Script name: Visualizing rates by census track geography
##
## Purpose of script: Load in the claened Police Stop dataset 
## and map the rates for each year
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
library(sf)
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

## Load relevant datasets ----
MplsSpatial <-
  readOGR("./Data/processed/HennepinCountyInt2017through2019Counts.gpkg")

## Map rates by year by race ----

## All stops ----

# Create basemaps

  # Currently using mapbox option
  Sys.setenv(MAPBOX_API_KEY = 'pk.eyJ1IjoianNhdWVyIiwiYSI6ImNrNDBieDIyMTAwNDMza210NmFoaXRqaDMifQ.GN5XxS_SfKA_UJld3hhOZg')

  # Download basemaps for both areas - few different options
  MPLSOsm <-
    tmaptools::read_osm(c(
      xmin = -93.362448,
      ymin = 44.878627,
      xmax = -93.174307,
      ymax = 45.064193
    ),
    type = "osm")
  
# Describe stop rates
  
  mean(MplsSpatial$NStopsBlackPSRatePerStratum)
  
  # 95% CI
  mean(MplsSpatial$NStopsBlackPSRatePerStratum) - (1.96*(sd(MplsSpatial$NStopsBlackPSRatePerStratum)/nrow(MplsSpatial)))
  mean(MplsSpatial$NStopsBlackPSRatePerStratum) + (1.96*(sd(MplsSpatial$NStopsBlackPSRatePerStratum)/nrow(MplsSpatial)))
  
  mean(MplsSpatial$NStopsWhitePSRatePerStratum)
  
  # 95% CI
  mean(MplsSpatial$NStopsWhitePSRatePerStratum) - (1.96*(sd(MplsSpatial$NStopsWhitePSRatePerStratum)/nrow(MplsSpatial)))
  mean(MplsSpatial$NStopsWhitePSRatePerStratum) + (1.96*(sd(MplsSpatial$NStopsWhitePSRatePerStratum)/nrow(MplsSpatial)))

# Get CT IDs for highest stop rates 
    
# Black
  
  test1 <-
    top_n(as.data.frame(MplsSpatial), 10, NStopsBlackPSRatePerStratum)  %>% select(NAME, NStopsBlackPSRatePerStratum)
  
# White
  
  test2 <-
    top_n(as.data.frame(MplsSpatial), 10, NStopsWhitePSRatePerStratum) %>% select(NAME, NStopsWhitePSRatePerStratum)


# Create a simple loop to map the data for each year for each race rate
  
  # Per Stratum
  
    # Create unified breaks across all variables
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackRatePerStratum, 5, subset = NULL)
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhiteRatePerStratum, 5, subset = NULL)
    BreakList <- c(0, 5, 10, 25, 100, 500, 1250)
    
    # Generate maps
    for (i in 2017:2019) {
      for (j in c(
        "NStopsBlackRatePerStratum",
        "NStopsWhiteRatePerStratum"
      )) {
        # Create file path
        SavePath <- file.path("./Writing/Visualizations/Maps/0_AllStops/Stops Denominator Census Tract Race Pop/",
                              paste(j, i, ".tiff", sep = ""))
        
        # Create race variable
        if (j == "NStopsTotalRatePerStratum") {
          race = "Total"
        } else if (j == "NStopsBlackRatePerStratum") {
          race = "Black"
        } else if (j == "NStopsWhiteRatePerStratum") {
          race = "White"
        }
        
        # Create title
        title = paste0(race, " stop rates across Minneapolis census tracts in ", i)
        
        # Create the map
        TempMap <- tm_shape(MPLSOsm) +
          tm_rgb() +
          tm_shape(subset(MplsSpatial, YEAR == i)) +
          tm_fill(
            col = j,
            breaks = BreakList,
            palette = "PuRd",
            title = "Stop rate \nper 100 \npopulation (race)"
            #legend.format = list(list(digits = 2))
          ) +
          tm_borders("grey25") +
          tm_layout(
            legend.outside = TRUE,
            main.title =  title,
            main.title.position = c('left')
          )
        
        # Save the map
        tmap_save(
          tm = TempMap,
          filename = SavePath,
          width = 3200,
          height = 2540,
          units = "px",
          dpi = 300
        )
        
      }
    }
    
  
  # Per Capita (Census Tract total population)
  
    # Create unified breaks across all variables
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsTotalRatePerCapita, 5, subset = NULL)
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackRatePerCapita, 5, subset = NULL)
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhiteRatePerCapita, 5, subset = NULL)
    BreakList <- c(0, 5, 10, 25, 50, 85, 120)
    
    # Generate maps
    for (i in 2017:2019) {
      for (j in c(
        "NStopsTotalRatePerCapita",
        "NStopsBlackRatePerCapita",
        "NStopsWhiteRatePerCapita"
      )) {
        # Create file path
        SavePath <- file.path("./Writing/Visualizations/Maps/0_AllStops/Stops Denominator Census Tract Total Pop/",
                              paste(j, i, ".tiff", sep = ""))
        
        # Create race variable
        if (j == "NStopsTotalRatePerCapita") {
          race = "Total"
        } else if (j == "NStopsBlackRatePerCapita") {
          race = "Black"
        } else if (j == "NStopsWhiteRatePerCapita") {
          race = "White"
        }
        
        # Create title
        title = paste0(race, " stop rates across Minneapolis census tracts in ", i)
        
        # Create the map
        TempMap <- tm_shape(MPLSOsm) +
          tm_rgb() +
          tm_shape(subset(MplsSpatial, YEAR == i)) +
          tm_fill(
            col = j,
            breaks = BreakList,
            palette = "PuRd",
            title = "Stop rate \nper 100 \npopulation (total)"
            #legend.format = list(list(digits = 2))
          ) +
          tm_borders("grey25") +
          tm_layout(
            legend.outside = TRUE,
            main.title =  title,
            main.title.position = c('left')
          )
        
        # Save the map
        tmap_save(
          tm = TempMap,
          filename = SavePath,
          width = 3200,
          height = 2540,
          units = "px",
          dpi = 300
        )
        
      }
    }
    
    # Per Capita (MPLS Total population per race)
    
    # Create unified breaks across all variables
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackRatePerCapitaMPLS, 5, subset = NULL)
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhiteRatePerCapitaMPLS, 5, subset = NULL)
    BreakList <- c(0, 0.25, 0.50, 0.75, 1, 1.25, 1.50)
    
    # Generate maps
    for (i in 2017:2019) {
      for (j in c(
        "NStopsBlackRatePerCapitaMPLS",
        "NStopsWhiteRatePerCapitaMPLS"
      )) {
        # Create file path
        SavePath <- file.path("./Writing/Visualizations/Maps/0_AllStops/Stops Denominator MPLS Total Pop/",
                              paste(j, i, ".tiff", sep = ""))
        
        # Create race variable
        if (j == "NStopsTotalRatePerCapitaMPLS") {
          race = "Total"
        } else if (j == "NStopsBlackRatePerCapitaMPLS") {
          race = "Black"
        } else if (j == "NStopsWhiteRatePerCapitaMPLS") {
          race = "White"
        }
        
        # Create title
        title = paste0(race, " stop rates across Minneapolis census tracts in ", i)
        
        # Create the map
        TempMap <- tm_shape(MPLSOsm) +
          tm_rgb() +
          tm_shape(subset(MplsSpatial, YEAR == i)) +
          tm_fill(
            col = j,
            breaks = BreakList,
            palette = "PuRd",
            title = "Stop rate \nper 100 \npopulation (MPLS total)"
            #legend.format = list(list(digits = 2))
          ) +
          tm_borders("grey25") +
          tm_layout(
            legend.outside = TRUE,
            main.title =  title,
            main.title.position = c('left')
          )
        
        # Save the map
        tmap_save(
          tm = TempMap,
          filename = SavePath,
          width = 3200,
          height = 2540,
          units = "px",
          dpi = 300
        )
        
      }
    }
  
## Person search ----
    
# Only rendering stratum specific searches at the moment
    
    # Create unified breaks across all variables
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackPSRatePerStratum, 5, subset = NULL)
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhitePSRatePerStratum, 5, subset = NULL)
    BreakList <- c(0, 5, 10, 25, 75, 150)
    
    # Generate maps
    for (i in 2017:2019) {
      for (j in c(
        "NStopsBlackPSRatePerStratum",
        "NStopsWhitePSRatePerStratum"
      )) {
        # Create file path
        SavePath <- file.path("./Writing/Visualizations/Maps/0_PersonSearch/Stops Denominator Census Tract Race Pop/",
                              paste(j, i, ".tiff", sep = ""))
        
        # Create race variable
        if (j == "NStopsTotalRatePSPerStratum") {
          race = "Total"
        } else if (j == "NStopsBlackPSRatePerStratum") {
          race = "Black"
        } else if (j == "NStopsWhitePSRatePerStratum") {
          race = "White"
        }
        
        # Create title
        title = paste0(race, " stop rates (person search) across\nMinneapolis census tracts in ", i)
        
        # Create the map
        TempMap <- tm_shape(MPLSOsm) +
          tm_rgb() +
          tm_shape(subset(MplsSpatial, YEAR == i)) +
          tm_fill(
            col = j,
            breaks = BreakList,
            palette = "PuRd",
            title = "Stop rate \nper 100 \npopulation (race)"
            #legend.format = list(list(digits = 2))
          ) +
          tm_borders("grey25") +
          tm_layout(
            legend.outside = TRUE,
            main.title =  title,
            main.title.position = c('left')
          )
        
        # Save the map
        tmap_save(
          tm = TempMap,
          filename = SavePath,
          width = 3200,
          height = 2540,
          units = "px",
          dpi = 300
        )
        
      }
    }
    
    # Before and After Sunset
    
    # Before Sunset
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackPSBSRatePerStratum, 5, subset = NULL)
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhitePSBSRatePerStratum, 5, subset = NULL)
    BreakList <- c(0, 5, 10, 15, 30, 65)
    
    # Generate maps
    for (i in 2017:2019) {
      for (j in c(
        "NStopsBlackPSBSRatePerStratum",
        "NStopsWhitePSBSRatePerStratum"
      )) {
        # Create file path
        SavePath <- file.path("./Writing/Visualizations/Maps/0_PersonSearch/Stops Denominator Census Tract Race Pop/",
                              paste(j, i, ".tiff", sep = ""))
        
        # Create race variable
        if (j == "NStopsTotalRatePSBSPerStratum") {
          race = "Total"
        } else if (j == "NStopsBlackPSBSRatePerStratum") {
          race = "Black"
        } else if (j == "NStopsWhitePSBSRatePerStratum") {
          race = "White"
        }
        
        # Create title
        title = paste0(race, " stop rates (person search, before sunset)\nacross Minneapolis census tracts in ", i)
        
        # Create the map
        TempMap <- tm_shape(MPLSOsm) +
          tm_rgb() +
          tm_shape(subset(MplsSpatial, YEAR == i)) +
          tm_fill(
            col = j,
            breaks = BreakList,
            palette = "PuRd",
            title = "Stop rate \nper 100 \npopulation (race)"
            #legend.format = list(list(digits = 2))
          ) +
          tm_borders("grey25") +
          tm_layout(
            legend.outside = TRUE,
            main.title =  title,
            main.title.position = c('left')
          )
        
        # Save the map
        tmap_save(
          tm = TempMap,
          filename = SavePath,
          width = 3200,
          height = 2540,
          units = "px",
          dpi = 300
        )
      }
    }
    
    # After Sunset
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackPSASRatePerStratum, 5, subset = NULL)
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhitePSASRatePerStratum, 5, subset = NULL)
    BreakList <- c(0, 1, 3, 10, 20, 80)
    
    # Generate maps
    for (i in 2017:2019) {
      for (j in c(
        "NStopsBlackPSASRatePerStratum",
        "NStopsWhitePSASRatePerStratum"
      )) {
        # Create file path
        SavePath <- file.path("./Writing/Visualizations/Maps/0_PersonSearch/Stops Denominator Census Tract Race Pop/",
                              paste(j, i, ".tiff", sep = ""))
        
        # Create race variable
        if (j == "NStopsTotalRatePSASPerStratum") {
          race = "Total"
        } else if (j == "NStopsBlackPSASRatePerStratum") {
          race = "Black"
        } else if (j == "NStopsWhitePSASRatePerStratum") {
          race = "White"
        }
        
        # Create title
        title = paste0(race, " stop rates (person search, after sunset)\nacross Minneapolis census tracts in ", i)
        
        # Create the map
        TempMap <- tm_shape(MPLSOsm) +
          tm_rgb() +
          tm_shape(subset(MplsSpatial, YEAR == i)) +
          tm_fill(
            col = j,
            breaks = BreakList,
            palette = "PuRd",
            title = "Stop rate \nper 100 \npopulation (race)"
            #legend.format = list(list(digits = 2))
          ) +
          tm_borders("grey25") +
          tm_layout(
            legend.outside = TRUE,
            main.title =  title,
            main.title.position = c('left')
          )
        
        # Save the map
        tmap_save(
          tm = TempMap,
          filename = SavePath,
          width = 3200,
          height = 2540,
          units = "px",
          dpi = 300
        )
      }
    }
    
    
    
## Vehicle search ----
    
# Only rendering stratum specific searches at the moment
    
    # Create unified breaks across all variables
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackVSRatePerStratum, 5, subset = NULL)
    BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhiteVSRatePerStratum, 5, subset = NULL)
    BreakList <- c(0, 5, 10, 25, 75, 100)
    
    # Generate maps
    for (i in 2017:2019) {
      for (j in c(
        "NStopsBlackVSRatePerStratum",
        "NStopsWhiteVSRatePerStratum"
      )) {
        # Create file path
        SavePath <- file.path("./Writing/Visualizations/Maps/0_VehicleSearch/Stops Denominator Census Tract Race Pop/",
                              paste(j, i, ".tiff", sep = ""))
        
        # Create race variable
        if (j == "NStopsTotalRateVSPerStratum") {
          race = "Total"
        } else if (j == "NStopsBlackVSRatePerStratum") {
          race = "Black"
        } else if (j == "NStopsWhiteVSRatePerStratum") {
          race = "White"
        }
        
        # Create title
        title = paste0(race, " stop rates (vehicle search) across\nMinneapolis census tracts in ", i)
        
        # Create the map
        TempMap <- tm_shape(MPLSOsm) +
          tm_rgb() +
          tm_shape(subset(MplsSpatial, YEAR == i)) +
          tm_fill(
            col = j,
            breaks = BreakList,
            palette = "PuRd",
            title = "Stop rate \nper 100 \npopulation (race)"
            #legend.format = list(list(digits = 2))
          ) +
          tm_borders("grey25") +
          tm_layout(
            legend.outside = TRUE,
            main.title =  title,
            main.title.position = c('left')
          )
        
        # Save the map
        tmap_save(
          tm = TempMap,
          filename = SavePath,
          width = 3200,
          height = 2540,
          units = "px",
          dpi = 300
        )
      }
    }
    
    # Before and After Sunset
    
      # Before Sunset
      BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackVSBSRatePerStratum, 5, subset = NULL)
      BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhiteVSBSRatePerStratum, 5, subset = NULL)
      BreakList <- c(0, 5, 10, 15, 30, 65)
      
      # Generate maps
      for (i in 2017:2019) {
        for (j in c(
          "NStopsBlackVSBSRatePerStratum",
          "NStopsWhiteVSBSRatePerStratum"
        )) {
          # Create file path
          SavePath <- file.path("./Writing/Visualizations/Maps/0_VehicleSearch/Stops Denominator Census Tract Race Pop/",
                                paste(j, i, ".tiff", sep = ""))
          
          # Create race variable
          if (j == "NStopsTotalRateVSBSPerStratum") {
            race = "Total"
          } else if (j == "NStopsBlackVSBSRatePerStratum") {
            race = "Black"
          } else if (j == "NStopsWhiteVSBSRatePerStratum") {
            race = "White"
          }
          
          # Create title
          title = paste0(race, " stop rates (vehicle search, before sunset)\nacross Minneapolis census tracts in ", i)
          
          # Create the map
          TempMap <- tm_shape(MPLSOsm) +
            tm_rgb() +
            tm_shape(subset(MplsSpatial, YEAR == i)) +
            tm_fill(
              col = j,
              breaks = BreakList,
              palette = "PuRd",
              title = "Stop rate \nper 100 \npopulation (race)"
              #legend.format = list(list(digits = 2))
            ) +
            tm_borders("grey25") +
            tm_layout(
              legend.outside = TRUE,
              main.title =  title,
              main.title.position = c('left')
            )
          
          # Save the map
          tmap_save(
            tm = TempMap,
            filename = SavePath,
            width = 3200,
            height = 2540,
            units = "px",
            dpi = 300
          )
        }
      }
      
      # After Sunset
      BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackVSASRatePerStratum, 5, subset = NULL)
      BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhiteVSASRatePerStratum, 5, subset = NULL)
      BreakList <- c(0, 1, 3, 10, 20, 80)
      
      # Generate maps
      for (i in 2017:2019) {
        for (j in c(
          "NStopsBlackVSASRatePerStratum",
          "NStopsWhiteVSASRatePerStratum"
        )) {
          # Create file path
          SavePath <- file.path("./Writing/Visualizations/Maps/0_VehicleSearch/Stops Denominator Census Tract Race Pop/",
                                paste(j, i, ".tiff", sep = ""))
          
          # Create race variable
          if (j == "NStopsTotalRateVSASPerStratum") {
            race = "Total"
          } else if (j == "NStopsBlackVSASRatePerStratum") {
            race = "Black"
          } else if (j == "NStopsWhiteVSASRatePerStratum") {
            race = "White"
          }
          
          # Create title
          title = paste0(race, " stop rates (vehicle search, after sunset)\nacross Minneapolis census tracts in ", i)
          
          # Create the map
          TempMap <- tm_shape(MPLSOsm) +
            tm_rgb() +
            tm_shape(subset(MplsSpatial, YEAR == i)) +
            tm_fill(
              col = j,
              breaks = BreakList,
              palette = "PuRd",
              title = "Stop rate \nper 100 \npopulation (race)"
              #legend.format = list(list(digits = 2))
            ) +
            tm_borders("grey25") +
            tm_layout(
              legend.outside = TRUE,
              main.title =  title,
              main.title.position = c('left')
            )
          
          # Save the map
          tmap_save(
            tm = TempMap,
            filename = SavePath,
            width = 3200,
            height = 2540,
            units = "px",
            dpi = 300
          )
        }
      }

## Publication visualization ----
      
# Person search stop rates
      
BAMMtools::getJenksBreaks(MplsSpatial$NStopsBlackPSRatePerStratum, 5, subset = NULL)     
BAMMtools::getJenksBreaks(MplsSpatial$NStopsWhitePSRatePerStratum, 6, subset = NULL)     

BlackBreakList <- c(0, 5, 10, 25, 75, 150)
WhiteBreakList <- c(0, 0.5, 1, 3, 5, 9)

      
PS_Black_2017 <- tm_shape(MPLSOsm) +
  tm_rgb() +
  tm_shape(subset(MplsSpatial, YEAR == 2017)) +
  tm_fill(
    col = "NStopsBlackPSRatePerStratum",
    breaks = BlackBreakList,
    palette = "PuRd",
    title = "Person search rate \nper 100 \npopulation (race)",
    legend.show = F
    #legend.format = list(list(digits = 2))
  ) +
  tm_borders("grey25") +
  tm_layout(
  #  legend.outside = TRUE,
    main.title =  "A) Person search stop rate for \nBlack individuals, 2017",
    main.title.position = c('center'), main.title.size = 1,
    frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA
  )

PS_Black_2018 <- tm_shape(MPLSOsm) +
  tm_rgb() +
  tm_shape(subset(MplsSpatial, YEAR == 2018)) +
  tm_fill(
    col = "NStopsBlackPSRatePerStratum",
    breaks = BlackBreakList,
    palette = "PuRd",
    title = "Person search rate \nper 100 \npopulation (race)",
    legend.show = F
    #legend.format = list(list(digits = 2))
  ) +
  tm_borders("grey25") +
  tm_layout(
    #  legend.outside = TRUE,
    main.title =  "B) Person search stop rate for \nBlack individuals, 2018",
    main.title.position = c('center'), main.title.size = 1,
    frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA
  )


PS_Black_2019 <- tm_shape(MPLSOsm) +
  tm_rgb() +
  tm_shape(subset(MplsSpatial, YEAR == 2019)) +
  tm_fill(
    col = "NStopsBlackPSRatePerStratum",
    breaks = BlackBreakList,
    palette = "PuRd",
    title = "Person search rate \nper 100 \npopulation (race)",
    legend.show = F
    #legend.format = list(list(digits = 2))
  ) +
  tm_borders("grey25") +
  tm_layout(
    #  legend.outside = TRUE,
    main.title =  "C) Person search stop rate for \nBlack individuals, 2019",
    main.title.position = c('center'), main.title.size = 1,
    frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA
  )

PS_White_2017 <- tm_shape(MPLSOsm) +
  tm_rgb() +
  tm_shape(subset(MplsSpatial, YEAR == 2017)) +
  tm_fill(
    col = "NStopsWhitePSRatePerStratum",
    breaks = WhiteBreakList,
    palette = "OrRd",
    title = "Person search rate \nper 100 \npopulation (race)",
    legend.show = F
    #legend.format = list(list(digits = 2))
  ) +
  tm_borders("grey25") +
  tm_layout(
    #  legend.outside = TRUE,
    main.title =  "D) Person search stop rate for \nWhite individuals, 2017",
    main.title.position = c('center'), main.title.size = 1,
    frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA
  )

PS_White_2018 <- tm_shape(MPLSOsm) +
  tm_rgb() +
  tm_shape(subset(MplsSpatial, YEAR == 2018)) +
  tm_fill(
    col = "NStopsWhitePSRatePerStratum",
    breaks = WhiteBreakList,
    palette = "OrRd",
    title = "Person search rate \nper 100 \npopulation (race)",
    legend.show = F
    #legend.format = list(list(digits = 2))
  ) +
  tm_borders("grey25") +
  tm_layout(
    #  legend.outside = TRUE,
    main.title =  "E) Person search stop rate for \nWhite individuals, 2018",
    main.title.position = c('center'), main.title.size = 1,
    frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA
  )

PS_White_2019 <- tm_shape(MPLSOsm) +
  tm_rgb() +
  tm_shape(subset(MplsSpatial, YEAR == 2019)) +
  tm_fill(
    col = "NStopsWhitePSRatePerStratum",
    breaks = WhiteBreakList,
    palette = "OrRd",
    title = "Person search rate \nper 100 \npopulation (race)",
    legend.show = F
    #legend.format = list(list(digits = 2))
  ) +
  tm_borders("grey25") +
  tm_layout(
    legend.outside = TRUE,
    main.title =  "F) Person search stop rate for \nWhite individuals, 2019",
    main.title.position = c('center'), main.title.size = 1,
    frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA
  )

Arranged <- tmap_arrange(
  PS_Black_2017,
  PS_Black_2018,
  PS_Black_2019,
  PS_White_2017,
  PS_White_2018,
  PS_White_2019,
  ncol = 3,
  nrow = 2, 
  heights = c(5,5)
) 

Arranged

tmap_save(Arranged, 
          filename = "./Writing/Visualizations/Maps/1_CombinedFigures/BlackWhite_PS_Combined.jpeg",
          width = 3200,
          height = 2540,
          units = "px",
          dpi = 300)

# Render legend and save as separate object - add manually in Word
BlackLegendMap <- tm_shape(MPLSOsm) +
  tm_rgb() +
  tm_shape(subset(MplsSpatial, YEAR == 2019)) +
  tm_fill(
    col = "NStopsWhitePSRatePerStratum",
    breaks = BlackBreakList,
    palette = "PuRd",
    title = "Person search rate \nper 100 people",
    #legend.format = list(list(digits = 2))
  ) +
  tm_borders("grey25") +
  tm_layout(
    legend.only = T,
    legend.title.size = 2,
    legend.text.size = 1.6,
    main.title =  "F) Person search stop rate for \nWhite individuals, 2019",
    main.title.position = c('center'),
    main.title.size = 1,
    frame = FALSE,
    frame.lwd = NA,
    panel.label.bg.color = NA,
    design.mode=TRUE, scale=2, asp=0
  )

BlackLegendMap

tmap_save(BlackLegendMap, 
          filename = "./Writing/Visualizations/Maps/1_CombinedFigures/Combined_LEGEND_Black.jpeg",
          #width = 320,
          #height = 254,
          #units = "px",
          dpi = 300)

WhiteLegendMap <- tm_shape(MPLSOsm) +
  tm_rgb() +
  tm_shape(subset(MplsSpatial, YEAR == 2019)) +
  tm_fill(
    col = "NStopsWhitePSRatePerStratum",
    breaks = WhiteBreakList,
    palette = "OrRd",
    title = "Person search rate \nper 100 people",
    #legend.format = list(list(digits = 2))
  ) +
  tm_borders("grey25") +
  tm_layout(
    legend.only = T,
    legend.title.size = 2,
    legend.text.size = 1.6,
    main.title =  "F) Person search stop rate for \nWhite individuals, 2019",
    main.title.position = c('center'),
    main.title.size = 1,
    frame = FALSE,
    frame.lwd = NA,
    panel.label.bg.color = NA,
    design.mode=TRUE, scale=2, asp=0
  )

WhiteLegendMap

tmap_save(WhiteLegendMap, 
          filename = "./Writing/Visualizations/Maps/1_CombinedFigures/BlackWhite_PS_Combined_LEGEND_White.jpeg",
          #width = 320,
          #height = 254,
          #units = "px",
          dpi = 300)

# UPDATE BREAKS ON WHITE MAPS

