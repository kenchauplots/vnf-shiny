# Pkgs --------------------------------------------------------------------
library(data.table)
library(leaflet)
library(reactable)
library(sf)
library(shiny)
library(tidyverse)

# pkgs = c("data.table", "leaflet", "reactable", "sf", "shiny", "tidyverse")
# for (p in pkgs) require(p, character.only = TRUE)


# Data --------------------------------------------------------------------
vnf = readRDS("data/vnf-2012-2020.rds") %>% 
  mutate(basin = str_to_title(basin),
         county = paste0(str_to_title(county), ", ", stusps),
         county = str_replace(county, "Mclean", "McLean"),
         county = str_replace(county, "Mckenzie", "McKenzie"),
         county = str_replace(county, "Mcmullen", "McMullen"),
         county = str_replace(county, "Dewitt", "DeWitt")) %>% 
  arrange(basin, county)
counties = st_read("data/counties.shp", quiet = TRUE) %>% 
  mutate(county = paste0(str_to_title(county), ", ", stusps),
         county = str_replace(county, "Mclean", "McLean"),
         county = str_replace(county, "Mckenzie", "McKenzie"),
         county = str_replace(county, "Mcmullen", "McMullen"),
         county = str_replace(county, "Dewitt", "DeWitt"),
         state = str_to_title(state))
zips = st_read("data/zips.shp", quiet = TRUE) %>% 
  mutate(county = str_replace(county, "Mclean", "McLean"),
         county = str_replace(county, "Mckenzie", "McKenzie"),
         county = str_replace(county, "Mcmullen", "McMullen"),
         county = str_replace(county, "Dewitt", "DeWitt"))

permian.counties = vnf %>% filter(basin == "Permian") %>% 
  pull(county) %>% unique() %>% sort() %>% as.list()
westerngulf.counties = vnf %>% filter(basin == "Western Gulf") %>% 
  pull(county) %>% unique() %>% sort() %>% as.list()
williston.counties = vnf %>% filter(basin == "Williston") %>% 
  pull(county) %>% unique() %>% sort() %>% as.list()

mt.zips = zips %>% filter(state == "Montana") %>% 
  pull(zip) %>% unique() %>% sort() %>% as.list()
nd.zips = zips %>% filter(state == "North Dakota") %>% 
  pull(zip) %>% unique() %>% sort() %>% as.list()
nm.zips = zips %>% filter(state == "New Mexico") %>% 
  pull(zip) %>% unique() %>% sort() %>% as.list()
tx.zips = zips %>% filter(state == "Texas") %>% 
  pull(zip) %>% unique() %>% sort() %>% as.list()

# UI ----------------------------------------------------------------------
navbarPage(
  "UOG Flares",
  id = "nav",
  tabPanel(
    "County view",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        selectizeInput(inputId = "county", label = "County",
                       choices = list(`Permian` = permian.counties,
                                      `Western Gulf` = westerngulf.counties,
                                      `Williston` = williston.counties),
                       selected = "La Salle, TX",
                       multiple = TRUE),
        dateRangeInput(inputId = "cty_dates", label = "Date range",
                       start = "2012-03-01", end = "2012-12-31"),
        submitButton("Find UOG flares")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Map",
            tags$head(# Include our custom CSS
              includeCSS("styles.css"),
              includeScript("gomap.js")),
            
            leafletOutput("cty_map", width = "100%", height = "600")
          ),
          tabPanel(
            "Summary",
            # textOutput("sum_cty_flares"),
            tableOutput("sum_cty_tab"),
            plotOutput("cty_bars", height = "300px")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "ZIP view",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        # textInput(inputId= "zip", label = "ZIP Code", value = ""),
        selectizeInput(inputId = "zip", label = "ZIP Code",
                       choices = list(`Montana` = mt.zips,
                                      `New Mexico` = nm.zips,
                                      `North Dakota` = nd.zips,
                                      `Texas` = tx.zips),
                       selected = "78014",
                       multiple = TRUE),
        dateRangeInput(inputId = "zip_dates", label = "Date range",
                       start = "2012-03-01", end = "2012-12-31"),
        sliderInput(inputId = "distance", label = "Distance (km)", min = 1, max = 100,
                    value = 10, step = 1),
        submitButton("Find UOG flares")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Map",
            tags$head(# Include our custom CSS
              includeCSS("styles.css"),
              includeScript("gomap.js")),
            
            leafletOutput("zip_map", width = "100%", height = "600")
          ),
          tabPanel(
            "Summary",
            # textOutput("sum_zip_flares"),
            tableOutput("sum_zip_tab"),
            plotOutput("zip_bars", height = "300px")
          ),
          tabPanel(
            "Reference",
            width = 12, align = "center",
            h4("Table of ZIP codes within study area"),
            reactableOutput("zips_tab")
          )
        )
      )
    )
  )
)
