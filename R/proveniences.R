get_global_proveniences <- function(dataset, province_abbreviation, municipality_code, measure){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        # italians <- dataset %>% filter(grepl("^9", codicenazione)) %>% filter(!grepl("^999", codicenazione)) %>% filter(periodo == "anno1") 
        # foreigners <- dataset %>% filter(!grepl("^9", codicenazione)) %>% filter(periodo == "anno1")

        italians <- dataset %>% filter(grepl("^999$", codicenazione)) %>% filter(periodo == "anno1")
        foreigners <- dataset %>% filter(grepl("9999", codicenazione)) %>% filter(periodo == "anno1")
        if (nrow(italians) > 0){
          italians$descrizione = "Italia"
        }
        if (nrow(foreigners) > 0){
          foreigners$descrizione = "Estero"
        }
        
        d <- rbind(italians, foreigners)
        
        dependent_variable <- d$tot_arrivi
        measure = tolower(measure)
        if (!is.null(measure) || measure != "") {
                if ((measure == 'presenze') || (measure == "presences")){
                        dependent_variable <- d$tot_presenze
                }
        }

        proveniences <- tryCatch({
                aggregate(dependent_variable ~ d$descrizione, FUN = sum)},
                error = function(cond) {
                  message("get_global_proveniences function does not have rows to aggregate ")
                  data.frame(matrix(nrow = 1, ncol = 2))
                  })                
        # measure = tolower(measure)
        # if (!is.null(measure) || measure != "") {
        #         if ((measure == 'presenze') || (measure == "presences")){
        #                 proveniences <- aggregate(d$tot_presenze ~ d$descrizione, FUN = sum)
        #         }
        # }
        names(proveniences) = c("provenienza", "movimenti")
        print(proveniences)
        proveniences
}

get_current_coverage <- function(dataset, province_abbreviation = NULL, municipality_code = NULL){

   if (is.null(province_abbreviation)){
     dataset <- filter(dataset, provincia == '')
   }else if (!is.null(province_abbreviation)){
     if(is.null(municipality_code)){
       dataset <- filter(dataset, provincia == province_abbreviation & codicecomune == '')
     }else{
       if (substring(municipality_code, 1, 1) == "9"){
         municipality_code = paste("0", municipality_code, sep = '')
       }
       dataset <- filter(dataset, provincia == province_abbreviation & codicecomune == municipality_code)
     }
   }

  current_coverage <- filter(dataset, periodo == "anno1" & anno_rif == max(as.numeric(anno_rif))) %>% filter(mese == max(as.numeric(mese))) %>% select(c("mesestr_ita", "copertura"))
  # current_coverage = select(filter(dataset, periodo == 'anno1' & anno_rif == max(anno_rif) & mese == max(mese)), c("mesestr_ita", "copertura"))    

  names(current_coverage) <- c("mese", "copertura")
  if(nrow(current_coverage) == 0){
    current_month <- filter(dataset, periodo == "anno1" & anno_rif == max(as.numeric(anno_rif))) %>% filter(provincia == '' & mese == max(as.numeric(mese))) %>% select(c("mesestr_ita")) 
    current_coverage[1,1] = current_month
    current_coverage[1,2] = ""
  }
  current_coverage
  
}


get_coverage <- function(dataset, province_abbreviation = NULL, municipality_code = NULL){
  if (is.null(province_abbreviation)){
    dataset <- filter(dataset, periodo == "anno1" & provincia == '') %>% arrange(desc(as.numeric(anno_rif)), desc(as.numeric(mese)))
  }else{
    if (is.null(municipality_code)){
      dataset <- filter(dataset, periodo == "anno1" & provincia == province_abbreviation & codicecomune == '') %>% arrange(desc(as.numeric(anno_rif)), desc(as.numeric(mese)))
    }else{
      if (substring(municipality_code, 1, 1) == "9"){
        municipality_code = paste("0", municipality_code, sep = '')
      }
      dataset <- filter(dataset, periodo == "anno1" & provincia == province_abbreviation & codicecomune == municipality_code) %>% arrange(desc(as.numeric(anno_rif)), desc(as.numeric(mese)))
      
    }
  }
  coverage <- select(dataset, c("anno_rif", "mesestr_ita", "copertura"))
  if(nrow(coverage) != 0){
    coverage$copertura <- sapply(coverage$copertura, FUN = function(x) {
      if (!is.na(x) & x != ''){
        as.numeric(x)*100
      }else{
        "ND"}
      })
  }else{
    coverage[1,1] = ""
    coverage[1,2] = ""
    coverage[1,3] = ""
  }
  names(coverage) = c("anno", "mese", "copertura")
  coverage
}

get_provenience_by_nation <- function(dataset, province_abbreviation, municipality_code, measure){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        foreigners <- dataset %>% filter(!grepl("^9", codicenazione)) %>% filter(periodo == "anno1")
        dependent_variable <- foreigners$tot_arrivi
        measure = tolower(measure)
        if (!is.null(measure) || measure != "") {
                if ((measure == 'presenze') || (measure == "presences")){
                        dependent_variable <- foreigners$tot_presenze
                }
        }
        proveniences <- tryCatch({
                aggregate(dependent_variable ~ foreigners$descrizione, FUN = sum)},
                error = function(cond) {
                        message("get_provenience_by_nation function does not have rows to aggregate ")
                        data.frame(matrix(nrow = 1, ncol = 2))
                })                
        # measure = tolower(measure)
        # if (!is.null(measure) || measure != "") {
        #   if ((measure == 'presenze') || (measure == "presences")){
        #     proveniences <- aggregate(foreigners$tot_presenze ~ foreigners$descrizione, FUN = sum)
        #   }
        # }
        names(proveniences) = c("nazione", "movimenti")
        proveniences <- proveniences[order(proveniences$movimenti, decreasing = T), ]
        

}

get_provenience_by_region <- function(dataset, province_abbreviation, municipality_code, measure){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)  
        italians_all <- dataset %>% filter(grepl("^9", codicenazione)) %>% filter(!grepl("9999", codicenazione)) %>% filter(periodo == "anno1")
        italians <- italians_all %>% filter(!grepl("Italia", descrizione)) %>% filter(!grepl("Estero", descrizione))
        
        dependent_variable <- italians$tot_arrivi
        if (!is.null(measure) || measure != "") {
                if ((measure == 'presenze') || (measure == "presences")){
                        dependent_variable <- italians$tot_presenze
                }
        }
        proveniences <- tryCatch({
                aggregate(dependent_variable ~ italians$descrizione, FUN = sum)},
                error = function(cond) {
                        message("get_provenience_by_region function does not have rows to aggregate ")
                        data.frame(matrix(nrow = 1, ncol = 2))
                })                        
        # measure = tolower(measure)
        # if (!is.null(measure) || measure != "") {
        #   if ((measure == 'presenze') || (measure == "presences")){
        #     proveniences <- aggregate(italians$tot_presenze ~ italians$descrizione, FUN = sum)
        #   }
        # }
        names(proveniences) = c("regione", "movimenti")
        proveniences <- proveniences[order(proveniences$movimenti, decreasing = T), ]
        
}