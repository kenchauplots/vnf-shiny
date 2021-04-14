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

# Server ------------------------------------------------------------
function(input, output, session) {
  
  output$zips_tab = renderReactable({
    reactable(
      data = zips %>% st_drop_geometry() %>% 
        mutate(county = substr(county, 1, nchar(county) - 4)) %>% 
        select(zip, basin, state, county) %>% 
        data.table(),
      searchable = TRUE,
      defaultSorted = "zip",
      pagination = FALSE,
      height = 500, fullWidth = FALSE,
      highlight = TRUE, showSortIcon = TRUE,
      columns = list(
        zip = colDef(name = "ZIP"),
        basin = colDef(name = "Basin"),
        state = colDef(name = "State"),
        county = colDef(name = "County", minWidth = 120)
      )
    )
  })
  
  cty_vnf = reactive({
    vnf %>% 
      filter(date >= input$cty_dates[1], date <= input$cty_dates[2]) %>% 
      filter(county %in% input$county)
  })
  
  zip_vnf = reactive({
    vnf %>% 
      filter(date >= input$zip_dates[1], date <= input$zip_dates[2]) %>% 
      st_as_sf(coords = c("vnf_lon", "vnf_lat"), crs = 4326, remove = FALSE) %>% 
      st_transform(crs = 32613) %>% 
      st_join(
        filter(zips, zip %in% input$zip) %>% 
          st_transform(crs = 32613) %>% 
          st_buffer(dist = input$distance * 1e3) %>% 
          select(zip, geometry),
        join = st_intersects, left = FALSE) %>% 
      st_drop_geometry() %>% data.table()
  })
  
  filtered_county = reactive({
    counties %>% filter(county %in% input$county)
  })
  
  filtered_zip = reactive({
    zips %>% filter(zip %in% input$zip)
  })
  
  output$cty_map = renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(noWarp = TRUE)) %>%
      # addProviderTiles("CartoDB.DarkMatter") %>% 
      flyToBounds(lng1 = -127.8, lat1 = 23.8,
                  lng2 = -64.8, lat2 = 49.8)
  })
  
  output$zip_map = renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(noWarp = TRUE)) %>%
      # addProviderTiles("CartoDB.DarkMatter") %>% 
      flyToBounds(lng1 = -127.8, lat1 = 23.8,
                  lng2 = -64.8, lat2 = 49.8)
  })
  
  output$sum_cty_tab = renderTable({
    cty_vnf() %>% 
      group_by(County = county) %>% 
      summarize(Flares = n(), .groups = "drop")
  })
  
  output$sum_zip_tab = renderTable({
    zip_vnf() %>% 
      group_by(ZIP = zip) %>% 
      summarize(Flares = n(), .groups = "drop")
  })
  
  output$cty_bars = renderPlot({
    req(nrow(cty_vnf()) > 0)
    
    if(input$cty_dates[2] - input$cty_dates[1] < 181){
      dat = cty_vnf() %>% 
        group_by(date) %>% 
        summarize(n = n(), .groups = "drop") %>% 
        mutate(time = as.Date(cut(date, "week")))
      ggplot() +
        geom_bar(data = dat, aes(x = time, weight = n), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%b"),
                     breaks = "2 month", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Weekly # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    } else if(input$cty_dates[2] - input$cty_dates[1] < 732) {
      dat = cty_vnf() %>% 
        group_by(date) %>% 
        summarize(n = n(), .groups = "drop") %>% 
        mutate(time = as.Date(cut(date, "month")))
      ggplot() +
        geom_bar(data = dat, aes(x = time, weight = n), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%b-%Y"),
                     breaks = "3 month", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Monthly # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      dat = cty_vnf() %>% 
        group_by(date) %>% 
        summarize(n = n(), .groups = "drop") %>% 
        mutate(time = as.Date(cut(date, "year")))
      ggplot() +
        geom_bar(data = dat, aes(x = time, weight = n), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%Y"),
                     breaks = "1 year", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Annual # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  output$zip_bars = renderPlot({
    req(input$zip %in% zips$zip, nrow(zip_vnf()) > 0)
    
    if(input$zip_dates[2] - input$zip_dates[1] < 181){
      dat = zip_vnf() %>% 
        select(-zip) %>% unique() %>% 
        group_by(date) %>% 
        summarize(n = n(), .groups = "drop") %>% 
        mutate(time = as.Date(cut(date, "week")))
      ggplot() +
        geom_bar(data = dat, aes(x = time, weight = n), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%b"),
                     breaks = "2 month", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Weekly # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    } else if(input$zip_dates[2] - input$zip_dates[1] < 732) {
      dat = zip_vnf() %>% 
        select(-zip) %>% unique() %>% 
        group_by(date) %>% 
        summarize(n = n(), .groups = "drop") %>% 
        mutate(time = as.Date(cut(date, "month")))
      ggplot() +
        geom_bar(data = dat, aes(x = time, weight = n), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%b-%Y"),
                     breaks = "3 month", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Monthly # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      dat = zip_vnf() %>% 
        select(-zip) %>% unique() %>% 
        group_by(date) %>% 
        summarize(n = n(), .groups = "drop") %>% 
        mutate(time = as.Date(cut(date, "year")))
      ggplot() +
        geom_bar(data = dat, aes(x = time, weight = n), fill = "firebrick") +
        scale_x_date(labels = scales::date_format("%Y"),
                     breaks = "1 year", expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        labs(x = NULL, y = NULL, title = "Annual # of UOG flares") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  observe({
    curr_vnf = cty_vnf()
    
    curr_cty = filtered_county()
    cty_bbox = curr_cty %>% st_bbox() %>% as.list()
    lon_min = min(curr_vnf$vnf_lon, cty_bbox$xmin)
    lat_min = min(curr_vnf$vnf_lat, cty_bbox$ymin)
    lon_max = max(curr_vnf$vnf_lon, cty_bbox$xmax)
    lat_max = max(curr_vnf$vnf_lat, cty_bbox$ymax)
    
    leafletProxy("cty_map", data = curr_vnf) %>% 
      clearShapes() %>% clearMarkerClusters() %>% 
      addPolygons(data = filtered_county(),
                  weight = 1, opacity = 1) %>% 
      addCircleMarkers(
        lng = ~vnf_lon, lat = ~vnf_lat,
        color = "#d62728", radius = 5,
        fillOpacity = 0.8, stroke = FALSE,
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          spiderfyOnMaxZoom = TRUE
        )) %>% 
      flyToBounds(lng1 = lon_min, lat1 = lat_min,
                  lng2 = lon_max, lat2 = lat_max)
  })
  
  observe({
    req(input$zip %in% zips$zip)
    
    curr_vnf = zip_vnf() %>% select(-zip) %>% unique()
    
    zip_bbox = st_bbox(filtered_zip()) %>% as.list()
    lon_min = min(curr_vnf$vnf_lon, zip_bbox$xmin)
    lat_min = min(curr_vnf$vnf_lat, zip_bbox$ymin)
    lon_max = max(curr_vnf$vnf_lon, zip_bbox$xmax)
    lat_max = max(curr_vnf$vnf_lat, zip_bbox$ymax)
    
    curr_zip_buff = filtered_zip() %>% 
      st_transform(crs = 32613) %>% 
      st_buffer(dist = input$distance * 1e3) %>% 
      st_transform(crs = 4326)
    
    leafletProxy("zip_map", data = curr_vnf) %>% 
      clearShapes() %>% clearMarkerClusters() %>%
      addPolygons(data = curr_zip_buff,
                  opacity = .1, fillOpacity = .1,
                  weight = 1, color = "red") %>% 
      addPolygons(data = filtered_zip(),
                  weight = 1, opacity = 1) %>% 
      addCircleMarkers(
        lng = ~vnf_lon, lat = ~vnf_lat,
        color = "#d62728", radius = 5,
        fillOpacity = 0.8, stroke = FALSE,
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          spiderfyOnMaxZoom = TRUE
        )) %>% 
      flyToBounds(lng1 = lon_min, lat1 = lat_min,
                  lng2 = lon_max, lat2 = lat_max)
  })
}