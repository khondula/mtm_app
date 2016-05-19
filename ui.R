
# features to add
# download link for google earth KML file
# summary data by restoration type (creation, restoration, enhancement)
# summary data by parameter (nested parameter list - habitat, wq, biota)
# display all data for selected site in a table
# link to display monitoring report for selected site
# link to ES&T paper

library(shiny)
source("load_data.R")

shinyUI(navbarPage("MTM Mitigation data", theme = shinytheme("spacelab"),
  tabPanel("Data by Project",
           fluidPage(
             fluidRow(column(5,
               h4("Click on a map marker for a project's basic monitoring data"),
               # verbatimTextOutput("clickInfo"),
               tabsetPanel(
                 tabPanel("all data", dataTableOutput("click_data")),
                 tabPanel("avg per year", dataTableOutput("click_data_summary")))
               ),
             column(7, leafletOutput("map", height = "600px"))
           ))),
  # tabPanel("Summary Data",
  #          sidebarLayout(
  #            sidebarPanel(
  #              selectInput("parameter_select", label = h4("Select a monitoring parameter"),
  #                          choices = unique(data$parameter), selected = "HAV")
  #            ),
  #            mainPanel(tabsetPanel(
  #              tabPanel("Histogram by year", plotOutput("parameter_histogram", height = "600px")),
  #              tabPanel("Data", h3("Average value reported by year of monitoring"),
  #                       dataTableOutput("parameter_data"))))
  #          )),
  tabPanel("View Reports",
           sidebarLayout(
             sidebarPanel(
              selectInput("report_select", label = h4("Select a document to view"),
                          choices = list.files("www/"), selected = list.files("www/")[1]),
              h4("link here to download zip of all reports")
             ),
             mainPanel(
               uiOutput("report_PDF")
             )
           )
  )
))
