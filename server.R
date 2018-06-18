
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(plotly)
library(data.table)
library(dplyr)
library(htmltools)
library(crosstalk)
library(V8)
library(RColorBrewer)
source("R/arrivals_presences.R")
source("R/proveniences.R")
source("R/profiling.R")
source("R/utilities.R")
source("R/update_translation.R")
source("R/translator.R")
source("R/layoutBlocks.R")
source("internazionalization/translation_functions.R")


provinces <- readOGR("shapes/Prov2016_rprj.shp", encoding = "UTF-8", use_iconv = T)
sardinian_provinces <- subset(provinces, provinces$PROVINCIA %in% c("Sassari", "Nuoro", "Cagliari", "Oristano", "Olbia-Tempio", "Ogliastra",
                                                                    "Medio Campidano", "Carbonia-Iglesias"))
#municipalities <- readOGR("shapes/Com2016_rprj.shp", encoding = "UTF-8", use_iconv = T)
municipalities <- readOGR("shapes/SardCom2016_rprj.shp", encoding = "UTF-8", use_iconv = T)
aggregate_movements <- read.csv("data/agg_citt_line_20xx.csv", encoding = "UTF-8", stringsAsFactors = F, colClasses = c("codicecomune" = "character", "mese" = "character", "codicenazione" = "character"))
aggregate_web_data <- read.csv("data/agg_citt_web.csv", encoding = "UTF-8", stringsAsFactors = F, colClasses = c("codicecomune" = "character", "codiceluogo" = "character", "sesso" = "character", "tipoalloggiato" = "character"))
map_threshold <- read.csv("data/soglia_map_prov_com_citt.csv", encoding = "UTF-8", colClasses = c("codicecomune" = "character", "anno" = "character"))
structures <- read.csv("data/struttura_info_citt.csv", stringsAsFactors = F, colClasses = c("id_struttura" = "character", "cod_com" = "character", "lat" = "character", "lon" = "character"))
coverage <- fread("data/copertura_sardegna.csv", colClasses = c("codicecomune" = "character"))
download_com <- read.csv("data/comunali.csv")
download_prov <- read.csv("data/provinciali.csv")

### load translation file ###
load("internazionalization/translation.bin")

### load time interval file
load("r_data/time_interval.RData")


shinyServer(function(input, output, session) {
        
        ######## Map filters elimination ########
        ### reactive variables ###
        data <- reactiveValues(clickedProvince = NULL, clickedMunicipality = NULL)
        change <- reactiveValues(language = "it")
        prov_pie <- reactiveValues(reset = FALSE, ev = NULL)
        sex_pie <- reactiveValues(reset = FALSE, ev = NULL)
        nation_bar <- reactiveValues(reset = FALSE, ev = NULL)
        region_bar <- reactiveValues(reset = FALSE, ev = NULL)
        accomodated_bar <- reactiveValues(ev = NULL)
        age_bar <- reactiveValues(ev = NULL)
        province_map_clicked <- reactiveValues(selected = NULL)
        province_map <- reactiveValues(proxy = NULL)
        municipality_map_clicked <- reactiveValues(selected = NULL)
        map <- reactiveValues(selected_params = NULL)
        bcc <- reactiveVal(value = 0)

        ##### Observers ###################
        observeEvent(input$province_map_shape_click,
                     {data$clickedProvince <- input$province_map_shape_click

                     ####### highlight selection ####
                     #define leaflet proxy for second regional level map
                       click <-input$province_map_shape_click
                       province_map$proxy <- leafletProxy("province_map")
                       selected_province <- sardinian_provinces[sardinian_provinces$COD_PRO == click$id, ]

                     if(!is.null(province_map_clicked$selected)){
                       print(paste("previous province selected", province_map_clicked$selected))
                       province_map$proxy %>% clearGroup("Selected")
                       
                     }   
                     
                     province_map$proxy %>% addPolylines(data = selected_province, layerId = selected_province$COD_PRO, color = "black", weight = 4, group = "Selected")
                     province_map_clicked$selected <- selected_province$COD_PRO 

                     })
        observeEvent(input$municipalities_map_shape_click,
                     {data$clickedMunicipality <- input$municipalities_map_shape_click
                     ####### highlight selection ####
                     #define leaflet proxy for second regional level map
                     click <-input$municipalities_map_shape_click
                     proxy <- leafletProxy("municipalities_map")

                     selected_municipality <- municipalities[municipalities$PRO_COM == click$id, ]
                     print("+++selected municipality++++")
                     print(selected_municipality$PRO_COM)
                     
                     
                     if(!is.null(municipality_map_clicked$selected)){
                       print(paste("+++ previous municipality selected", municipality_map_clicked$selected))
                       proxy  %>% clearGroup("Selected")

                     }
                     proxy %>% addPolylines(data = selected_municipality, layerId = selected_municipality, color = "black", weight = 4, group = "Selected")
                     municipality_map_clicked$selected <- selected_municipality$PRO_COM
                     })
        
        observeEvent(input$stop_map_filters,
                     {data$clickedProvince <- NULL
                      data$clickedMunicipality <- NULL
                      if (!is.null(province_map$proxy)){
                          province_map$proxy %>% clearGroup("Selected")  
                      }})
        
        observeEvent(input$it, {
                change$language <- "it"
        })
        
        observe({
          query <- parseQueryString(session$clientData$url_search)
          if ((!is.null(query$language)) && (query$language == "en")){
            change$language <- "en"
          }else{
            change$language <- "it"
          }
          
        })
        
        observeEvent(input$en, {
                change$language <- "en"
        })
        
        observe({map$selected_params <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)})
        observe({prov_pie$ev <- event_data("plotly_click", source = 'prov_pie')})
        observe({sex_pie$ev <- event_data("plotly_click", source = "sex_pie" )})
        observe({nation_bar$ev <- event_data("plotly_click", source = "nation_bar")})
        observe({region_bar$ev <- event_data("plotly_click", source = "region_bar")})
        observe({accomodated_bar$ev <- event_data("plotly_click", source = "accomodated_bar")})
        observe({age_bar$ev <- event_data("plotly_click", source = "age_bar")})
        observe({
                   shinyjs::onclick("dett_button", function(){
                           if(bcc() == 0){
                                   bcc(1)
                                   html("dett_button", tr("chiudi", change$language))
                           }else{
                                   bcc(0)
                                   html("dett_button", tr("dettagli", change$language))                                   
                           }
                            
                   })
                })
                 
        observeEvent(input$stop_provenience_filter, {
                prov_pie$reset <- TRUE
                prov_pie$ev <- NULL
                nation_bar$ev <- NULL
                region_bar$ev <- NULL
                js$resetProvenienceClick()
                js$resetProvByNationClick()
                js$resetProvByRegionClick()
                shinyjs::show("prov_by_nation", anim = TRUE)
                shinyjs::show("prov_by_region", anim = TRUE)

        })
        observeEvent(input$stop_profiling_filter, {
          sex_pie$reset <- TRUE
          sex_pie$ev <- NULL
          js$resetProfileClick()
          
        })
        observeEvent(input$stop_accomodated_type_filter, {
           accomodated_bar$ev <- NULL
           age_bar$ev <- NULL                
        })

        #################################################
        #### UI multilingual element generation ##### 
        output$header <- renderText({
                generateHeaderTitle(change$language)
        })

        output$radio <- renderUI({
                generateRadio(change$language)
        })
        
        output$map_filter_button <-renderUI({
                generateMapFilterButton(change$language)
        })
        
        output$provenience_bar <- renderUI({
                bar_title <- tr("provenience_bar", change$language)
                bar_title
        })
        
        output$provenience_filter_button <- renderUI({
                generateFilterButton(change$language, "stop_provenience_filter", "elimina_filtri_provenienza")
        })
        
        output$export_prov_button <- renderUI({
                generateExportButton(change$language, "downloadProv", "esporta_prov")
        })
        
        output$export_com_button <- renderUI({
                generateExportButton(change$language, "downloadCom", "esporta_com")
        })
        
        output$profiling_bar_title <- renderUI({
                bar_title <- tr("profiling_bar_title", change$language)
                bar_title
        })

        output$profiling_filter_button <- renderUI({
                generateFilterButton(change$language, "stop_profiling_filter", "elimina_filtri_profilazione")
        })
        
        output$type_bar_title <- renderUI({
                br()
        })
        
        output$type_filter_button <- renderUI({
                generateFilterButton(change$language, "stop_accomodated_type_filter", "elimina_filtri_alloggiato")
        })
        
        output$details_button <- renderUI({
          tags$button(id="dett_button", tr("dettagli", change$language), class="btn btn-info", `data-toggle`="collapse", `data-target`="#demo")
        })
         
        
        output$province_map <- renderLeaflet({
                measure = input$measure
                print(paste("measure: ", measure))
                measure_selected = get_arrivals(aggregate_movements)
                legend_title = tr("numero_arrivi", change$language)
                if ((!is.null(measure)) && ((measure == "Presenze") || (measure == "Presences"))){
                        measure_selected = get_presences((aggregate_movements))
                        legend_title = tr("numero_presenze", change$language)
                }
                 
                ### add arrivals to sardinian_provinces dataset
                sardinian_provinces$measure <- sapply(sardinian_provinces$SIGLA, function(x) measure_selected[[2]][measure_selected[[1]] == x])
                print("Sardinian provinces measures")
                print(sardinian_provinces$measure)
                
                #pal <- colorQuantile("Blues", domain = sardinian_provinces$measure, n = 8, )
                blues <- colorRampPalette(brewer.pal(9,"Blues"))(100)
                pal <- colorNumeric(blues[30:100], domain = sardinian_provinces$measure)
                #pal <- colorFactor("Blues", domain = as.factor(sardinian_provinces$measure))
                #blues <- sample(x = colorRampPalette(brewer.pal(9,"Blues"))(100), 8)
                #pal <- colorBin("Blues", domain = sardinian_provinces$measure, bins = c(10000, 15000, 20000, 40000, 50000, 60000, 70000, 100000,350000), pretty = FALSE)
            
                #~colorQuantile("YlOrRd", arriv)(arriv)
                m <- leaflet(data=sardinian_provinces) %>%
                        setView(lng=8.981, lat=40.072, zoom=8) %>%
                        addTiles() %>%
                        addPolygons(layerId = sardinian_provinces$COD_PRO, color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5, 
                                    fillColor = ~pal(measure),
                                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                        bringToFront = TRUE), label = sardinian_provinces$PROVINCIA, labelOptions = labelOptions(clickable = FALSE, noHide = TRUE)) %>%
                        
                        addLegend("bottomright", pal = pal, values = ~measure, title = legend_title, opacity = 1)
              m        
                
        })
        

        output$map_bar <- renderText({
                
                selected_area = "Sardegna"
                out <- NULL
                print("clickMarker")
                print(data$clickedProvince)
                
                out <- tr("map_bar_region", change$language)
                if(!is.null(data$clickedProvince[[1]])){
                        prov_code <- data$clickedProvince[[1]]
                        selected_area = as.character(sardinian_provinces$PROVINCIA[sardinian_provinces$COD_PRO == prov_code])
                        area <- tr(selected_area, change$language)
                        out <- paste(tr("map_bar_province1", change$language), "<strong>", area, "</strong>", tr("map_bar_province2", change$language))
                        
                        if(!is.null(data$clickedMunicipality[["id"]])){
                            municipality_code <- data$clickedMunicipality[["id"]]
                            selected_area <- municipalities$COMUNE[municipalities$PRO_COM == municipality_code]
                            out <- paste(tr("map_bar_mun1", change$language), "<strong>", selected_area, "</strong>", tr("map_bar_mun2", change$language))
                        }
  
                }
                out <- paste(out, "<p class='disclaimer'>", tr("disclaimer", change$language), "</p>")
                out
        })
        

        
        output$municipalities_map <- renderLeaflet({
                r <- NULL
                allowed_municipalities_code <- map_threshold %>% mutate(codicecomune = gsub("^0", "", codicecomune)) %>% filter(esito_unita == 1) %>% select(codicecomune)
                allowed_municipalities_code <- as.integer(allowed_municipalities_code[[1]]) %>% unique(.)
                if (!is.null(data$clickedProvince[[1]])){

                        province_code <- input$province_map_shape_click[["id"]]
                        province_symbol <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
                        
                        print("**** province symbol *****")
                        #print(province_symbol)
                        latitude <- input$province_map_shape_click[["lat"]] 
                        longitude <- input$province_map_shape_click[["lng"]]
                        print(input$province_map_shape_click)
                        
                        sardinian_municipalities <- subset(municipalities, (municipalities$COD_PRO %in% c(province_code)) & (municipalities$PRO_COM %in% allowed_municipalities_code))
                        measure = input$measure
                        measure_selected = get_arrivals_by_municipal_code(aggregate_movements)
                        legend_title = tr("numero_arrivi", change$language)
                        if ((measure == "Presenze") || (measure == "Presences")){
                                print(paste("radio: ", measure))
                                measure_selected = get_presences_by_municipal_code((aggregate_movements))
                                legend_title = tr("numero_presenze", change$language)
                        }
                        print("measure_selected")
                        sardinian_municipalities$measure <- sapply(sardinian_municipalities$PRO_COM, function(x){
                          if(x %in% measure_selected$municipal_code){
                            measure_selected[[2]][measure_selected[[1]] == x]                            
                          }else{
                            0
                          }
                        })
                        
                        print(measure_selected)
                        pal <- colorNumeric("Blues", domain = sardinian_municipalities$measure)
                        
                        print(sardinian_municipalities$measure)
                        r <- leaflet() %>%
                                setView(lng=longitude, lat=latitude, zoom=8) %>%
                                addTiles() %>%
                                addPolygons(data = sardinian_municipalities, color = "#444444", weight = 1, layerId = sardinian_municipalities$PRO_COM, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.8,
                                            fillColor = ~pal(measure),
                                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                bringToFront = TRUE), label = sardinian_municipalities$COMUNE, labelOptions = labelOptions(clickable = FALSE, noHide = TRUE)) %>%
                                             addLegend("bottomright", pal = pal, values = sardinian_municipalities$measure, title = legend_title, opacity = 1)                                        
                }
                #session$userData[["mmap"]] = r
                r
        })
        
        output$structure_map <- renderLeaflet({
                r <- NULL
                allowed_municipalities <- map_threshold %>% mutate(codicecomune = gsub("^0", "", codicecomune)) %>% filter(esito_unita == 1)
                allowed_municipalities_code <- allowed_municipalities %>% select(codicecomune)
                allowed_municipalities_code <- as.integer(allowed_municipalities_code[[1]])
                struct <- structures %>% mutate(cod_com = gsub("^0", "", cod_com))

                province_code <- data$clickedProvince[["id"]]
                municipal_code <- data$clickedMunicipality[["id"]]
                
                
                if (!is.null(data$clickedMunicipality[["id"]]) && sameProvince(municipal_code, province_code)){
                        province_symbol <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
                        
                        print("**** municipal parameters *****")
   
                        latitude <- data$clickedMunicipality[["lat"]] 
                        longitude <- data$clickedMunicipality[["lng"]]
                        
                        municipal_structures <- struct %>% filter(cod_com == data$clickedMunicipality[["id"]])
                        municipal_structures <- municipal_structures %>% filter(!(lat == '' | lon == '')) %>% mutate(lat = as.numeric(lat), .) %>% mutate(lon = as.numeric(lon), .)
                        
                      if(nrow(municipal_structures) != 0){
                        municipal_structures$lat <- round(municipal_structures$lat, 3)
                        municipal_structures$lon <- round(municipal_structures$lon, 3)
                        
                        print(paste("id comune: ", data$clickedMunicipality[["id"]]))
                        
                        print("municipal structures: ")
                        print(municipal_structures$denominazione)
                        
                        m = municipal_structures
                        
                        r <- leaflet(municipal_structures)  %>%
                          setView(lng=longitude, lat=latitude, zoom=11) %>%
                          addTiles() %>%
                          addMarkers(~lon, ~lat, popup = ~build_marker_popup(denominazione, tipologia, classificazione, mesi_apertura, tot_letti, change$language), label = ~as.character(denominazione))
                      }else{
                          r <- leaflet(municipal_structures)  %>%
                          setView(lng=longitude, lat=latitude, zoom=11) %>%
                          addTiles()
                      }
                      r  
                }
                r
        })
        
        
        output$current_coverage <- renderText({
          province_abbreviation <- NULL
          municipality_code <- NULL
          
          selections <- map$selected_params
          print("current coverage selections")
          print(selections)
          province_abbreviation <- selections[[1]]
          municipality_code <- selections[[2]]
          coverage = get_average_coverage(coverage, province_abbreviation, municipality_code, (current_year))
          # mese = tr(tolower(coverage[[1]]), change$language)
          anno <- current_year
          copertura = coverage[[2]]
          print(paste("copertura is null: ", is.null(copertura)))
          if (!is.null(copertura) & copertura != '' & !is.na(copertura)){
            copertura = as.numeric(copertura)*100
            copertura = paste(copertura, "%")
          }else{
            copertura = "ND"
          }
          res <- paste(tr("copertura_anno", change$language), current_year, ":", copertura)
        })
        
        
        output$coverage <- renderDataTable({
            province_abbreviation <- NULL
            municipality_code <- NULL
            #selections <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)
            selections <- map$selected_params
            province_abbreviation <- selections[[1]]
            municipality_code <- selections[[2]]
            coverage <- get_coverage(coverage, province_abbreviation, municipality_code, (current_year))
            coverage$mese <- sapply(coverage$mese, FUN = function(x){tr(tolower(x), change$language)})
            names(coverage) = c(tr("anno", change$language), tr("mese", change$language), tr("copertura", change$language))
            coverage
        }, options = list(lengthMenu = c(3, 6, 12), pageLength = 3, columnDefs = list(list(targets = "_all", searchable = FALSE)), ordering = F, 
                          autoWidth = FALSE, language = list(search = paste(tr("cerca", change$language)), lengthMenu = tr("mostra_elementi", change$language),
                          info = "", paginate = list("next" = tr("successivo", change$language), previous = tr("precedente", change$language)))))
        
        output$proveniences <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
                
                #selections <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)
                selections <- map$selected_params
                province_abbreviation <- selections[[1]]
                municipality_code <- selections[[2]]

                measure = input$measure
                if (is.null(measure) || measure == ""){
                        measure = 'Arrivi'
                }else{
                        measure = input$measure
                }
                print(paste("measure inside proveniences: ", measure))
                proveniences <- get_global_proveniences(aggregate_movements, province_abbreviation, municipality_code, measure, current_year)
                if (change$language == "en"){
                        proveniences$provenienza <- translate_vector(proveniences$provenienza, change$language)
                }
                ### margins ##
                m <- list(
                        pad = 4
                )
                
                #d <- event_data("plotly_click", source = 'prov_pie' )
                d <- prov_pie$ev
                print(paste("d assigned: ", d))
                print("Click event")
                print(d)
                print(class(d))
                if (!is.null(d)){
                        print("point number")
                        print(d[["pointNumber"]])
                        
                }
                plot_title <- tr("distribuzione_provenienza", change$language)
                
                print("nation bar ev: ")
                print(paste("nation_bar_ev: ", nation_bar$ev))
                print("region bar ev: ")
                print(paste("region bar ev: ", region_bar$ev))
                #### Color selection ####
                colors <- c('rgb(255, 127, 14)', 'rgb(31, 119, 180)')
                if (is.null(d) && !is.null(nation_bar$ev)){
                   region_bar$ev <- NULL        
                   colors = c('rgb(255, 127, 14)', 'rgb(220, 220, 220)')
                   shinyjs::hide("prov_by_region", anim = T)
                }else if (is.null(d) && !is.null(region_bar$ev)){
                   nation_bar$ev <- NULL        
                   colors = c('rgb(220, 220, 220)', 'rgb(31, 119, 180)')
                   shinyjs::hide("prov_by_nation", anim = T)
                }else if ((!is.null(d) && d[["pointNumber"]] == 0) || ((!is.null(d) && d[["pointNumber"]] == 0) && (!is.null(region_bar$ev)))){ ##### Estero ####
                        js$resetProvByRegionClick()
                        # print("***********************************sono all'estero ****")
                        region_bar$ev <- NULL
                        colors = c('rgb(255, 127, 14)', 'rgb(220, 220, 220)')
                        shinyjs::hide("prov_by_region", anim = T)
                        shinyjs::show("prov_by_nation", anim = T)
                }else if ((!is.null(d) && d[["pointNumber"]] == 1) || ((!is.null(d) && d[["pointNumber"]] == 1) && (!is.null(nation_bar$ev)))){ #### Italia #####
                        js$resetProvByNationClick()
                        #js$resetProvByNationClick()
                        # print("***********************************sono in italia ****")
                        
                        nation_bar$ev <- NULL
                        colors = c('rgb(220, 220, 220)', 'rgb(31, 119, 180)')
                        shinyjs::hide("prov_by_nation", anim = T)
                        shinyjs::show("prov_by_region", anim = T)
                        
                }
                
                
                
                p <- plot_ly(proveniences, labels = ~provenienza, values = ~movimenti, type = 'pie', textinfo = 'percent', hoverinfo = 'text',
                             text = ~paste(provenienza, ":", movimenti), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE, source = 'prov_pie') %>%
                        layout(title = plot_title, showlegend = T) %>% config(displayModeBar = FALSE, collaborate = FALSE) %>%
                        highlight(
                                  persistent = TRUE)
        })
        
        output$prov_by_nation <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
                
                #selections <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)
                selections <- map$selected_params
                province_abbreviation <- selections[[1]]
                municipality_code <- selections[[2]]
                
                measure = input$measure
                if (is.null(measure) || measure == ""){
                  measure = 'Arrivi'
                }else{
                  measure = input$measure
                }
                print(paste("measure inside prov by nation: ", measure))
                ##################
                provenience_by_nation <- get_provenience_by_nation(aggregate_movements, province_abbreviation, municipality_code, measure, current_year)
                plot_title <- tr("distribuzione_per_stato", change$language)
                y_axis_title <- measure
                ### axis params ###
                f2 <- list(
                  family = "Old Standard TT, serif",
                  size = 14
                )
                a <- list(
                  title = "AXIS TITLE",
                  titlefont = f2,
                  showticklabels = TRUE,
                  tickangle = 45,
                  tickfont = f2,
                  exponentformat = "E"
                )
                m <- list(b=120)
                ####################
               if (change$language == "en"){
                 provenience_by_nation$nazione <- translate_vector(provenience_by_nation$nazione, change$language)
               }
                xform <- list(categoryorder = "array",
                              categoryarray = provenience_by_nation$nazione, title = "", tickfont = list(size = 9), tickangle = 35)
                
                nation_ev <- nation_bar$ev
                base_color = 'rgb(255, 127, 14)'
                background_color = 'rgba(204,204,204,1)'
                color_set = rep(base_color, nrow(provenience_by_nation))

                if (!is.null(nation_ev)){
######tmp#########################################                        
                     # shinyjs::hide("prov_by_region", anim = T, animType = "fade")
                     # if (!is.null(region_bar$ev)){
                     #   region_bar$ev <- NULL
                     # }
####################################################                        
                     nation_chosen <- nation_ev[["x"]]
                     #index <- as.numeric(nation_ev[["pointNumber"]]) + 1
                     color_set = rep(background_color, nrow(provenience_by_nation))
                     names(color_set) = provenience_by_nation$nazione
                     color_set[nation_chosen] = base_color
                }
                
                #onevent("dblclick", "provenience_by_nation", shinyjs::show("prov_by_region")) 
                
                # # print("double click ****")
                # print(nation_bar$dbev)
                # if (!is.null(nation_bar$dbev)){
                #   shinyjs::show("prov_by_region", anim = T, animType = "fade")
                # }

                
                #color1[1] = line_color
                
                ######## Shared data to highlight ####
                #prov_by_nation <- SharedData$new(provenience_by_nation, ~nazione)
                
                p <- plot_ly(data = provenience_by_nation, x = ~nazione, y = ~movimenti, type = 'bar', marker = list(color = color_set, line = list(color = 'rgb(255,140,0)', width = 1.5)), source = 'nation_bar') %>%
                        layout(title = plot_title, xaxis = xform, yaxis = list(title = y_axis_title, tickfont = list(size = 9)),
                                margin = m) %>% config(displaylogo = F, collaborate = F, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "toImage", "resetScale2d")) %>%
                        highlight(
                                  persistent = TRUE,
                                  opacityDim = getOption("opacityDim", 0.1)
                              
                        )
                
                               #yaxis = list(tickfont = list(size = 8)), xaxis = list(title = "Regione di provenienza", tickfont = list(size = 8)))
        })
        
        
        output$prov_by_region <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
                
                #selections <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)
                selections <- map$selected_params
                province_abbreviation <- selections[[1]]
                municipality_code <- selections[[2]]
                
                measure = input$measure
                if (is.null(measure) || measure == ""){
                  measure = 'Arrivi'
                }else{
                  measure = input$measure
                }
          
                print(paste("measure inside prov by region: ", measure))

                prov_by_region <- get_provenience_by_region(aggregate_movements, province_abbreviation, municipality_code, measure, current_year)
                
                if (change$language == "en"){
                  prov_by_region$regione <- translate_vector(prov_by_region$regione, change$language)
                }
                
                region_ev <- region_bar$ev
                ### bar style ####
                base_color = 'rgb(31, 119, 180)'
                background_color = 'rgba(204,204,204,1)'
                color_set = rep(base_color, nrow(prov_by_region))
                if (!is.null(region_ev)){
#################tmp#############                        
                  # shinyjs::hide("prov_by_nation", anim = T, animType = "fade")
                  # if (!is.null(nation_bar$ev)){
                  #   nation_bar$ev <- NULL
                  # }
####################################                        
                  region_chosen <- region_ev[["x"]]
                  #index <- as.numeric(region_ev[["pointNumber"]]) + 1
                  color_set = rep(background_color, nrow(prov_by_region))
                  names(color_set) <- prov_by_region$regione
                  color_set[region_chosen] = base_color
                }
                
                
                #onevent("dblclick", "provenience_by_region", shinyjs::show("prov_by_nation"))
                
                plot_title <- tr("distribuzione_per_regione", change$language)
                y_axix_title <- measure

                
                
                
                xform <- list(categoryorder = "array",
                              categoryarray = prov_by_region$regione, title = "", tickfont = list(size = 9), tickangle = 35)
                m <- list(b=120)
                ###cool color  marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)' 
                p <- plot_ly(data = prov_by_region, x = ~regione, y = ~movimenti, type = 'bar', marker = list(color = color_set, line = list(color = 'rgb(8,48,107)', width = 1.5)), source = 'region_bar') %>%
                        layout(title = plot_title, xaxis = xform, yaxis = list(title = y_axix_title, tickfont = list(size = 9)), margin = m) %>% config(displaylogo = F, collaborate = F, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "toImage", "resetScale2d"))
                
        })
        

        
        output$sex <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
                
                #selections <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)
                selections <- map$selected_params
                province_abbreviation <- selections[[1]]
                municipality_code <- selections[[2]]
                
                ev <- prov_pie$ev
                print(paste("sex event: ", ev[["pointNumber"]]))
                
                ### plotly events from nations and regions bar chart
                nation_ev <- nation_bar$ev
                region_ev <- region_bar$ev
                
                sex_distribution <- get_sex(aggregate_web_data, province_abbreviation, municipality_code, ev, nation_ev, region_ev, change$language)
                sex_distribution <- sex_distribution %>% mutate(sesso = ifelse(sesso == "M", tr("maschi", change$language), tr("femmine", change$language)))
                
                
                ### Color selection ####
                d <- sex_pie$ev
                print(paste("sex filter: ", d[["pointNumber"]]))
                
                
                colors <- c('rgb(255, 127, 14)', 'rgb(31, 119, 180)')
                if (!is.null(d) && d[["pointNumber"]] == 0){ ##### Female ####
                  colors = c('rgb(255, 127, 14)', 'rgb(220, 220, 220)')
                }else if(!is.null(d) && d[["pointNumber"]] == 1){ #### Male #####
                  colors = c('rgb(220, 220, 220)', 'rgb(31, 119, 180)')
                }
                
                
                if (nrow(sex_distribution) == 0 | all(is.na(sex_distribution$sesso))){
                        plot_title <- tr("diagramma_sesso_nd", change$language)
                        ax <- list(
                                title = "",
                                zeroline = FALSE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                showgrid = FALSE
                        )
                        p = plot_ly() %>% layout(title = plot_title, xaxis = ax, yaxis = ax)
                }else{
                        plot_title <- tr("distribuzione_per_sesso", change$language)
                        p <- plot_ly(sex_distribution, labels = ~sesso, values = ~movimenti, type = 'pie', textinfo = 'percent', hoverinfo = 'percent',
                                     text = ~paste(sesso, ":", movimenti), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE, source = 'sex_pie') %>%
                                layout(title = plot_title, showlegend = T) %>% config(displayModeBar = FALSE, collaborate = FALSE) 
                }

                
        })
        
        
        output$accomodated_type <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
                
                #selections <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)
                selections <- map$selected_params
                province_abbreviation <- selections[[1]]
                municipality_code <- selections[[2]]
          
                ev <- prov_pie$ev
                print(paste("accomodated type provenience event: ", ev[["pointNumber"]]))
                profile_ev <- sex_pie$ev
                print(paste("accomodated type profile event: ", profile_ev["pointNumber"]))
                ### plotly events from nations and regions bar chart
                nation_ev <- nation_bar$ev
                region_ev <- region_bar$ev
                
                accomodated_type <- get_accomodated_type(aggregate_web_data, province_abbreviation, municipality_code, ev, profile_ev, nation_ev, region_ev, change$language)
                if (change$language == "en"){
                        accomodated_type$tipo_alloggiato <- translate_vector(accomodated_type$tipo_alloggiato, change$language)
                }
                
                
                accomodated_ev <- accomodated_bar$ev
                base_color = 'rgb(158,202,225)'
                background_color = 'rgba(204,204,204,1)'
                color_set = rep(base_color, nrow(accomodated_type))
                if (!is.null(accomodated_ev)){
                        accomodated_type_chosen = accomodated_ev[["x"]]
                        color_set = rep(background_color, nrow(accomodated_type))
                        names(color_set) = accomodated_type$tipo_alloggiato
                        color_set[accomodated_type_chosen] = base_color
                }

                #c("Famigliare", "Capo Famiglia", "Ospite Singolo", "Membro Gruppo", "Capo Gruppo")
                #accomodated_type$tipo_alloggiato
                print(accomodated_type)
                if (nrow(accomodated_type) == 0 | all(is.na(accomodated_type$tipo_alloggiato))){
                        plot_title <- tr("diagramma_tipoalloggiato_nd", change$language)
                        ax <- list(
                                title = "",
                                zeroline = FALSE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                showgrid = FALSE
                        )
                        p = plot_ly() %>% layout(title = plot_title, xaxis = ax, yaxis = ax)  
                }else{
                        xform <- list(categoryorder = "array",
                                      categoryarray = c("Famigliare", "Capo Famiglia", "Ospite Singolo", "Membro Gruppo", "Capo Gruppo"), title = "")
                        plot_title <- tr("distribuzione_per_alloggiato", change$language)
                        p <- plot_ly(data = accomodated_type, x = ~tipo_alloggiato, y = ~arrivi, type = 'bar', text = ~paste(tipo_alloggiato, ", ", arrivi, "%", sep = ''),  hoverinfo = 'text', marker = list(color = color_set, line = list(color = 'rgb(8,48,107)', width = 1.5)), source = "accomodated_bar") %>%
                                layout(title = plot_title, xaxis = xform, bargap = 0.8, yaxis = list(title = "(%)")) %>% config(displaylogo = F, collaborate = F, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "toImage", "resetScale2d")) 
                }
        })
        
        
        output$age_range <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
                
                #selections <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)
                selections <- map$selected_params
                province_abbreviation <- selections[[1]]
                municipality_code <- selections[[2]]
          
          
                ev <- prov_pie$ev
                print(paste("age range: ", ev[["pointNumber"]]))
                profile_ev <- sex_pie$ev
                print(paste("age range profile event: ", ev[["pointNumber"]]))
                ### plotly events from nations and regions bar chart
                nation_ev <- nation_bar$ev
                region_ev <- region_bar$ev
                accomodated_ev <- accomodated_bar$ev
                print("Accomodated type event:")
                print(accomodated_ev)
                
                
                age_range <- get_age_range(aggregate_web_data, province_abbreviation, municipality_code, ev, profile_ev, nation_ev, region_ev, accomodated_ev, change$language)
                print(age_range)
                if (nrow(age_range) == 0 | all(is.na(age_range$eta))){
                        plot_title <- tr("diagramma_agerange_nd", change$language)
                        ax <- list(
                                title = "",
                                zeroline = FALSE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                showgrid = FALSE
                        )
                        p = plot_ly() %>% layout(title = plot_title, xaxis = ax, yaxis = ax)
                }else{
                        plot_title <- tr("distribuzione_per_eta", change$language)
                        p <- plot_ly(data = age_range, x = ~eta, y = ~arrivi, text = ~paste(eta, ", ", arrivi, "%", sep=''), hoverinfo = 'text', type = 'bar', marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                                layout(title = plot_title, bargap = 0.8, xaxis = list(title = tr("fascia_eta", change$language)), yaxis = list(title = "(%)")) %>% config(displaylogo = F, collaborate = F, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "toImage", "resetScale2d"))
                        
                }
                

        })
        
        
        output$trend_comparison <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
                month_range <- get_month_range(aggregate_movements, current_year)
                months <- month_range$mesestr
                print("months from month range")
                print(months)
                
                #selections <- get_map_selections(data$clickedProvince[["id"]], data$clickedMunicipality[["id"]], sardinian_provinces)
                selections <- map$selected_params
                province_abbreviation <- selections[[1]]
                municipality_code <- selections[[2]]
                
                measure = input$measure
                if (is.null(measure) || measure == ""){
                  measure = 'Arrivi'
                }else{
                  measure = input$measure
                }
                print("*** measure inside line comparison")
                print(measure)
                
                ### plotly event from provenience pie chart ####
                ev <- prov_pie$ev
                print(paste("Trend event: ", ev[["pointNumber"]]))
                
                ### plotly events from nations and regions bar chart
                nation_ev <- nation_bar$ev
                region_ev <- region_bar$ev
                
                trends <- get_last_three_years(aggregate_movements, province_abbreviation, municipality_code, measure, ev, nation_ev, region_ev, change$language)
                print("**** Trends")
                print(trends)
                #trend_2014 = filter(trends, anno == 2014) %>% select(., c("mese"))
                last_years <- tail(unique(trends$anno), n = 3)
                
                ### last three years 
                # trends1 <- trends[trends$anno == last_years[1], ]
                # mesi1 <- translate_vector(trends1$mese, change$language)
                # trends1$mese <- factor(x = mesi1, levels = mesi1)
                # 
                # trends2 <- trends[trends$anno == last_years[2], ]
                # mesi2 <- translate_vector(trends2$mese, change$language)
                # trends2$mese <- factor(x = mesi2, levels = mesi2)
                # 
                # trends3 <- trends[trends$anno == last_years[3], ]
                # mesi3 <- translate_vector(trends3$mese, change$language) 
                # trends3$mese <- factor(x = mesi3, levels = mesi3)
               ##############################################
                #change time interval
                
                #trends1 <- filter(trends, periodo == "anno1") ### last year
                trends1 <- filter(trends, anno == current_year) ### last year
                trends1 <- fill_months(trends1, months)
                mesi1 <- translate_vector(trends1$mese, change$language)
                m1 <- factor(mesi1, levels = mesi1)
                #intervallo1 <- trends1$intervallo[1]
                intervallo1 <- current_year
                #trends2 <- filter(trends, periodo == "anno2")
                trends2 <- filter(trends, anno == (current_year - 1))
                trends2 <- fill_months(trends2, months)
                mesi2 <- translate_vector(trends2$mese, change$language) #past years
                m2 <- factor(mesi2, levels = mesi2)
                #intervallo2 <- trends2$intervallo[1]
                intervallo2 <- current_year - 1
                #trends3 <- filter(trends, periodo == "anno3")
                trends3 <- filter(trends, anno == (current_year - 2))
                trends3 <- fill_months(trends3, months)
                mesi3 <- translate_vector(trends3$mese, change$language) ### past years
                m3 <- factor(mesi3, levels = mesi3)
                #intervallo3 <- trends3$intervallo[1]
                intervallo3 <- current_year - 2 
                
                month_list <- list(m1, m2, m3)
                reference_month_list <- get_longest_vector(month_list)
                mlist <- factor(month_range$mesestr, levels = month_range$mesestr)
                print("*** mlist ***")
                print(mlist)
                reference_month_list <- translate_vector(months, change$language)
                
                print(paste("Lunghezza mesi: ", length(reference_month_list)))
                #print(m)
                ### Layout params              
                y_axix_title <- measure
                xform <- list(categoryorder = "array", categoryarray = reference_month_list, title = "", showticklabels = TRUE)
                
                
                plot_title <- tr("andamento_triennio", change$language)
                p <- plot_ly(trends1, x = ~m1, y = ~movimenti, name = paste(y_axix_title, intervallo1), type = 'scatter', mode = 'lines+markers',  text = ~paste(tr(tolower(mese), change$language), anno, "<br>", measure, ":", movimenti ), hoverinfo = 'text') %>%
                        add_trace(data = trends2, x = ~m2, y = ~movimenti, name = paste(y_axix_title, intervallo2), mode = 'lines+markers') %>%
                        add_trace(data = trends3, x = ~m3, y = ~movimenti, name = paste(y_axix_title, intervallo3), mode = 'lines+markers') %>%                        
                        layout(title = plot_title,
                               xaxis = xform,
                               yaxis = list (title = y_axix_title)) %>% config(displaylogo = F, collaborate = F, modeBarButtonsToRemove = list("zoom2d", "zoomIn2d", "zoomOut2d", "toImage", "select2d", "lasso2d", "resetScale2d"))      
        })
        
        
        output$downloadProv <- downloadHandler(
          filename = tr("download_provinciali", change$language),
          content = function(file) {
            write.csv(download_prov, file, row.names = FALSE)
          }
        )
        
        output$downloadCom <- downloadHandler(
                filename = tr("download_comunali", change$language),
                content = function(file) {
                        write.csv(download_com, file, row.names = FALSE)
                }
        )
        
        
        #### Download file csv #### 
        
        # Reactive value for selected dataset ----
        # datasetInput <- reactive({
        #   switch(input$dataset,
        #          "Dati Provinciali" = dati_provinciali,
        #          "Dati Comunali" = dati_comunali)
        # })
        # 
        # output$table <- renderTable({
        #   if (input$dataset == "Dati Provinciali"){
        #     rock
        #   }else{
        #     cars
        #   }
        #   
        #   
        # })
        # 
        # 
        
        
        
        
        
        # observeEvent(input$province_map_shape_click, { # update the location selectInput on map clicks
        #         p <- input$province_map_shape_click[[1]]
        #         print(p)
        # 
        # })
        
        # points <- eventReactive(input$recalc, {
        #         cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
        # }, ignoreNULL = FALSE)
        # 
        # output$mymap <- renderLeaflet({
        #         leaflet() %>%
        #                 addProviderTiles(providers$Stamen.TonerLite,
        #                                  options = providerTileOptions(noWrap = TRUE)
        #                 ) %>%
        #                 addMarkers(data = points())
        })


  # output$distPlot <- renderPlot({
  # 
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
#  })


