
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#

#

library(shiny)
library(shinyjs)
library(leaflet)
library(plotly)
library(crosstalk)
source("R/translator.R")
source("R/utilities.R")

load("internazionalization/translation.bin")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


shinyUI(fluidPage(
   includeCSS("www/DO.css"),
   tags$script(src="https://cdn.plot.ly/plotly-latest.min.js"),
   includeScript("www/js/redraw.js"),
   useShinyjs(),
   extendShinyjs(text = "shinyjs.resetProfileClick = function() { Shiny.onInputChange('.clientValue-plotly_click-sex_pie', 'null'); }"),
   extendShinyjs(text = "shinyjs.resetProvenienceClick = function() { Shiny.onInputChange('.clientValue-plotly_click-prov_pie', 'null'); }"),
   extendShinyjs(text = "shinyjs.resetProvByRegionClick = function() { Shiny.onInputChange('.clientValue-plotly_click-region_bar', 'null'); }"),
   extendShinyjs(text = "shinyjs.resetProvByNationClick = function() { Shiny.onInputChange('.clientValue-plotly_click-nation_bar', 'null'); }"),
 
  div( 
       br(),          
       uiOutput("radio")         
     ), br(),
  fluidRow(
          id = "page_bar",
       column(
               width = 8,
               htmlOutput("map_bar", container = tags$p, class="map_bar")
               
       ),
       column(
               width = 4,
               uiOutput("map_filter_button")
       )

  ),
  fluidRow(
        id = "coverage",
        column(width = 5,
               class = "coverage-container",
                 textOutput("current_coverage"), 
                 div(id='details', uiOutput("details_button")),                 
                 div(id='demo', class='collapse', dataTableOutput("coverage"))
                )),
  
  fluidRow(
          column( 
                  width = 6,
                  class = "main_map",
                  leafletOutput("province_map", height = "600")
                                           
          ),
          column(
                  width = 6,
                  class = 'border_map',
                  leafletOutput("municipalities_map", height = "280")
                        
                
          ),
          column(
                  width = 6,
                  class = 'border_map',
                  leafletOutput("structure_map", height = "280")
          )
  ),
  fluidRow(
          id='pie_bar',
          column(
                  width = 8,
                  htmlOutput("provenience_bar", container = tags$p, class="map_bar")
          ),
          column(
                  width = 4,
                  uiOutput("provenience_filter_button")
          )
  ), br(), br(),        
  fluidRow(
        column(
                offset = 4,
                width = 4,
                plotlyOutput("proveniences",  height = '600px')
        )),br(), br(), br(),
  fluidRow(
          column(
                  width = 12, 
                  div(id="provenience_by_nation",
                      plotlyOutput("prov_by_nation")    
                  )
                  
          )
  ), br(), br(), br(),br(), br(),
  fluidRow(
          column(
                  width = 12,
                  div(id="provenience_by_region",
                      plotlyOutput("prov_by_region"))

          )
  ),br(), br(), br(), br(),
  
  fluidRow(
          width = 12,
          plotlyOutput("trend_comparison")
          
  ),br(), br(), br(),
  
  
  fluidRow(
          id='profiling_bar',
          column(
                  width = 8,
                  htmlOutput("profiling_bar_title", container = tags$p, class='map_bar')
          ),
          column(
                  width = 4,
                  uiOutput("profiling_filter_button")
                  
          )

  ), br(), br(), br(),
  fluidRow(
          column(
                  offset = 4,
                  width = 4,
                  plotlyOutput("sex",  height = '600px')                  
                  
          )
  ), br(), br(), br(),
  fluidRow(
          id='type_bar',
          column(
                  width = 8,
                  htmlOutput("type_bar_title", container = tags$p, class='map_bar')
          ),
          column(
                  width = 4,
                  uiOutput("type_filter_button")

          )

  ), br(), br(), br(),
  fluidRow(
          column(
                  width = 12, 
                  plotlyOutput("accomodated_type")
          )
  ), br(), br(), br(),
  fluidRow(
          column(
                  width = 12, 
                  plotlyOutput("age_range")
          )
  )
                  
))
