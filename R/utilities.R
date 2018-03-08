### This function returns True if the municipality is included in province provided


get_map_selections <- function(province_map_click_id, municipalities_map_click_id, sardinian_provinces){
  print("*** map selections ***")
  province_abbreviation <- NULL
  municipality_code <- NULL
  
  if (!is.null(province_map_click_id)){
    province_code <- province_map_click_id
    province_abbreviation <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
    print(paste("Province abbreviation: ", province_abbreviation))
    
  }
  
  if(!(is.null(municipalities_map_click_id))){
    municipality_code = municipalities_map_click_id
    if (!sameProvince(municipality_code, province_code)){
      print(paste("Municipality code", municipality_code))
      municipality_code <- NULL
      
    }
    
  }
  
  # print(c(province_abbreviation, municipality_code))
  print("***** end map selections ****")
  # c(province_abbreviation, municipality_code)
  return(list(province_abbreviation, municipality_code))
  
  
}

sameProvince <- function(municipality_code, province_code){
  return(substr(municipality_code, 1, nchar(province_code)) == province_code)
  
}

filter_dataset <- function(dataset, province_abbreviation = NULL, municipality_code = NULL, prov_pie_event = NULL, profile_pie_event = NULL, nation_bar_ev = NULL, region_bar_ev = NULL, accomodated_bar_ev = NULL, lang_chosen = NULL){
  if(!is.null(province_abbreviation)){
    dataset <-  filter(dataset, provincia == province_abbreviation)
    if (!is.null(municipality_code)){
      if (substring(municipality_code, 1, 1) == "9"){
        municipality_code = paste("0", municipality_code, sep = '')
      }
      dataset <- dataset %>% filter(codicecomune == municipality_code)
    }
    
  }
  if(!is.null(prov_pie_event)){
    target_field <- ifelse("periodo" %in% names(dataset), "codicenazione", "codiceluogo")
    if (prov_pie_event[["pointNumber"]] == 0){
      dataset <- dataset %>% filter(!grepl("^9", dataset[[target_field]]))
      paste(paste("foreigners dataset", nrow(dataset)))
    }else if (prov_pie_event[["pointNumber"]] == 1){
      dataset <- dataset %>% filter(grepl("^9", dataset[[target_field]])) 
      dataset <- dataset %>% filter(!grepl("^9999", dataset[[target_field]])) 
    }
  }
  #browser()
if(!is.null(nation_bar_ev)){
        print("*** inside filter dataset ***")
        print(paste("lang chosen: ", lang_chosen))
        if(lang_chosen == "en"){
                target_field2 = ifelse("periodo" %in% names(dataset), "descrizione_eng", "descrizioneluogo_eng")
        }else{
                target_field2 = ifelse("periodo" %in% names(dataset), "descrizione", "descrizioneluogo")          
        }
        print(paste("target field 2:", target_field2))
        dataset <- dataset %>% filter(dataset[[target_field2]] == nation_bar_ev[["x"]])
        
}
if (!is.null(region_bar_ev)){
        if(lang_chosen == "en"){
                target_field3 = ifelse("periodo" %in% names(dataset), "descrizione_eng", "descrizioneluogo_eng")
        }else{
                target_field3 = ifelse("periodo" %in% names(dataset), "descrizione", "descrizioneluogo")
        }
        print(paste("target field 3:", target_field3))
        dataset <- dataset %>% filter(dataset[[target_field3]] == region_bar_ev[["x"]])    
}


  if("sesso" %in% names(dataset) && !is.null(profile_pie_event)){
          if (length(unique(dataset$sesso)) != 1){
                  if (profile_pie_event[["pointNumber"]] == 0){
                          #paste("filtering females")
                          print("filtering females")
                          dataset <- dataset %>% filter(sesso_str == "F")
                  }else if (profile_pie_event[["pointNumber"]] == 1){
                          print("filtering males")
                          dataset <- dataset %>% filter(sesso_str == "M")
                  }                  
          }else{
                  dataset
          }
  }
  
  if(!is.null(accomodated_bar_ev)){
          accomodated_type_chosen = accomodated_bar_ev[["x"]]
          print(paste("PRE accomodated type chosen: ", accomodated_type_chosen))
          if (lang_chosen == "en"){
            tipo_alloggiato = unique(dataset$tipoalloggiato_str)
            print("tipo alloggiato:")
            print(tipo_alloggiato)
            accomodated_type = translate_vector(tipo_alloggiato, lang_chosen)
            if (accomodated_type_chosen %in% accomodated_type){
                index = which(accomodated_type == accomodated_type_chosen)
                accomodated_type_chosen = tipo_alloggiato[index]
            }else{
                    accomodated_type_chosen = ""
            }


          }
          print(paste("POST accomodated_type_chosen: ", accomodated_type_chosen))
          if("sesso" %in% names(dataset)){
                  dataset <- dataset %>% filter(dataset[["tipoalloggiato_str"]] == accomodated_type_chosen)
          }
  }  
  
  return(dataset)
}


build_marker_popup <- function(denominazione, tipologia, classificazione, mesi_apertura, posti_letto, language){
    
    # tipologia = tr("tipologia", language)
    # classificazione = tr("classificazione", language)
    # mesi_apertura = tr("mesi_apertura", language)
    # posti_letto = tr("posti_letto", language)
    denominazione = paste(tr("denominazione", language), denominazione, "</br>")
    tipologia = paste(tr("tipologia", language), tipologia, "</br>")
    classificazione = paste(tr("classificazione", language), classificazione, "</br>")
    mesi_apertura = paste(tr("mesi_apertura", language), mesi_apertura, "</br>")
    posti_letto = paste(tr("posti_letto", language), posti_letto)
    
    pop_up = paste(denominazione, tipologia, classificazione, mesi_apertura, posti_letto)
    p = paste("<p>", pop_up, "</p>")
}


get_longest_vector <- function(l){
  ordered_list <- l[order(lengths(l), decreasing = T)]
  return(ordered_list[[1]])
}

get_language <- function(language = "it"){
        language
}

language = get_language()


get_month_range <- function(dataset = aggregate_movements){
  x <- filter(dataset, periodo == "anno1")
  mapping <- unique(cbind(x$mese, x$mesestr_ita, x$anno_rif))
  mapping <- data.frame(mapping)
  names(mapping) <- c("mese", "mesestr", "anno")
  mapping <- mapping[order(mapping$anno, mapping$mese, decreasing = F), ]
}