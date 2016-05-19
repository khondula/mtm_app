
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)


source("load_data.R")

shinyServer(function(input, output) {
  
  data <- reactiveValues(clickedMarker=NULL) 
  observeEvent(input$map_marker_click, 
               {data$clickedMarker <- input$map_marker_click}) 
  observeEvent(input$map_click, 
               {data$clickedMarker <- NULL}) 

  click_mitt <- reactive({data$clickedMarker$id})
  
  output$map <- renderLeaflet({
    pal <- colorFactor(c("red", "green", "yellow"), 
                       domain = c("creation", "enhancement", "restoration"))
    
    leaflet(data = mitt_points) %>% 
      addProviderTiles("Esri.WorldImagery", group = "Esri") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OSM") %>%
      addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
      addProviderTiles("Stamen.TopOSMFeatures", group = "StamenFeatures") %>%
      
      addWMSTiles(
        "http://basemap.nationalmap.gov/arcgis/services/USGSHydroNHD/MapServer/WMSServer?",
        layers = "0",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "USGS National Map", group = "USGSNationalMap")  %>%
      
      addCircleMarkers(lng = ~x, lat = ~y, layerId = ~Project_Name,
                       popup = ~paste("<b>",Project_Name,"</b><br/>Mitigation Location: ",Project_site,"<br/> Stream Type: ",StreamType,"<br/>Mitigation Type: ",Project_Type),
                       color = ~pal(Project_Type), 
                       radius = 6, fillOpacity = .5, clusterOptions = markerClusterOptions()) %>%
    
    addLayersControl(
      baseGroups = c("Esri", "OSM", "OpenTopoMap", "USGSNationalMap"),
      overlayGroups = c("StamenFeatures"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
      
      addLegend("bottomright", colors = c("red", "green", "yellow"), 
                labels = c("Stream Creation", "Stream Enhancement", "Stream Restoration"))
    
    
  })
  
  click_data <- reactive({
    if(!is.null(data$clickedMarker$id)){ 
    hdata <- filter(habitat, mittID == click_mitt()) %>% 
      select(permit_year, HAV) %>%
      gather(parameter, value, HAV) %>% filter(!is.na(value))
    
    wqdata <- filter(wq, mittID == click_mitt()) %>% 
      select(permit_year, Conductivity_uScm) %>%
      gather(parameter, value, Conductivity_uScm) %>% filter(!is.na(value))
    
    biodata <- filter(biol, mittID == click_mitt()) %>% 
      select(permit_year, WVSCI, EKSAP_MBI) %>%
      gather(parameter, value, WVSCI:EKSAP_MBI) %>% filter(!is.na(value))
    
    as.data.frame(rbind(hdata, wqdata, biodata)) } else NULL
  })
  
  output$click_data <- renderDataTable({
    click_data()
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$click_data_summary <- renderDataTable({
    if(!is.null(data$clickedMarker$id)){ 
    click_data() %>% group_by(permit_year, parameter) %>% 
      summarise_each(funs(mean)) %>% spread(parameter, value) } else NULL
    
  })
  
  output$report_PDF <- renderUI({
    PDFfile=input$report_select
    tags$iframe(
      src=PDFfile,
      width="100%",
      height="600px")
  })
  
  parameter_data <- reactive({
    monitoring_data[which(monitoring_data$parameter == input$parameter_select),] %>%
      filter(permit_year %in% c(1:5))
  })
  
  # output$parameter_data <- renderDataTable({
  #   parameter_data() %>% group_by(permit_year, Project_Type, StreamType) %>%
  #     summarise(parameter_avg = mean(value, na.rm = TRUE)) %>%
  #     spread(permit_year, parameter_avg)
  #   
  # })
  # 
  # output$parameter_histogram <- renderPlot({
  #   ggplot(parameter_data(), aes(x = value)) + 
  #     geom_histogram() + facet_wrap(~permit_year, ncol = 1)
  # })
  
})
