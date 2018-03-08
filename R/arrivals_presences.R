get_arrivals <- function(dataset){
        ### Arrival number in the last 12 months
        data <- dataset[dataset$periodo == "anno1", ]
        arrivals <- tryCatch({
                aggregate(data$tot_arrivi ~ data$provincia, FUN = sum)},
                error = function(cond){
                        message("get_arrivals function does not have rows to aggregate ")
                        data.frame(matrix(nrow = 1, ncol = 5))        
                }) 
        names(arrivals) <- c("province", "tot_arrivals")
        res <- arrivals[order(arrivals$tot_arrivals, decreasing = T), ]
        res
        
}

get_arrivals_by_municipal_code <- function(dataset){
        #### Arrival number by municipal code in the last 12 months
        data <- dataset[dataset$periodo == "anno1", ]
        arrivals <- tryCatch({
                aggregate(data$tot_arrivi ~ data$codicecomune, FUN = sum)},
                error = function(cond){
                        message("get_arrivals_by_municipal_code function does not have rows to aggregate ")
                        data.frame(matrix(nrow = 1, ncol = 2))      
                }) 
        names(arrivals) <- c("municipal_code", "tot_arrivals")
        arrivals = arrivals[arrivals$municipal_code != "", ]
        arrivals$municipal_code <- arrivals$municipal_code %>% gsub("^0", "", .)
        res <- arrivals[order(arrivals$tot_arrivals, decreasing = T), ]
        res
        
}

get_presences <- function(dataset){
        ### Presences in  the last 12 months
        data <- dataset[dataset$periodo == "anno1",  ]
        presences <- tryCatch({
                aggregate(data$tot_presenze ~ data$provincia, FUN = sum)},
                error = function(cond){
                        message("get_presences function does not have rows to aggregate ")
                        data.frame(matrix(nrow = 1, ncol = 2))      
                })
        names(presences) <- c("province", "tot_presences")
        res <- presences[order(presences$tot_presences, decreasing = T), ]
        res
        
}


get_presences_by_municipal_code <- function(dataset){
        #### Presences by municipal code in the last 12 months
        data <- dataset[dataset$periodo == "anno1",  ]
        presences <- tryCatch({
                        aggregate(data$tot_presenze ~ data$codicecomune, FUN = sum)},
                        error = function(cond){
                                message("get_presences_by_municipal_code function does not have rows to aggregate ")
                                data.frame(matrix(nrow = 1, ncol = 2))   
                        }) 
        names(presences) <- c("municipal_code", "tot_presences")
        presences = presences[presences$municipal_code != "", ]
        presences$municipal_code <- presences$municipal_code %>% gsub("^0", "", .)
        res <- presences[order(presences$tot_presences, decreasing = T), ]
        res
        
        
}

#dataset, province_abbreviation = NULL, municipality_code = NULL, prov_pie_event = NULL, profile_pie_event = NULL, nation_bar_ev = NULL, region_bar_ev = NULL, accomodated_bar_ev = NULL, lang_chosen = NULL

get_last_three_years <- function(dataset, province_abbreviation, municipality_code, measure, prov_pie_event, nation_bar_ev, region_bar_ev, lang_chosen){
        dataset <- dataset %>% filter(!grepl("^999$", codicenazione)) %>% filter(!grepl("9999", codicenazione))
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code, prov_pie_event, NULL, nation_bar_ev, region_bar_ev, NULL, lang_chosen)
        mapping <- unique(cbind(dataset$mese, dataset$mesestr_ita))
        mapping_list <- mapping[,2]
        names(mapping_list) <- mapping[,1]
        measure = tolower(measure)
        # res <- aggregate(dataset$tot_arrivi ~ dataset$mese + dataset$anno_rif, FUN = sum)
        dependent_variable <- dataset$tot_arrivi
        measure = tolower(measure)
        if (!is.null(measure) || measure != "") {
                if ((measure == 'presenze') || (measure == "presences")){
                        dependent_variable <- dataset$tot_presenze
                }
        }
        
        res <- tryCatch({
                aggregate(dependent_variable ~ dataset$periodo + dataset$mese + dataset$anno_rif + dataset$periodo_str, FUN = sum)},
                error = function(cond){
                                message("get_provenience_by_nation function does not have rows to aggregate ")
                                data.frame(matrix(nrow = 1, ncol = 5))}) 
        # if (!is.null(measure) || measure != "") {
        #   if ((measure == 'presenze') || (measure == "presences")){
        #     res <- aggregate(dataset$tot_presenze ~ dataset$periodo + dataset$mese + dataset$anno_rif + dataset$periodo_str, FUN = sum)
        #     #res <- aggregate(dataset$tot_presenze ~ dataset$mese + dataset$anno_rif, FUN = sum)
        #   }
        # }
        names(res) <- c("periodo", "mese", "anno", "intervallo", "movimenti")
        out <- res %>% mutate(mese = mapping_list[mese])
 
        out
}