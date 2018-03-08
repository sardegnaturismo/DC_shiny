require(dplyr)
get_sex <- function(dataset, province_abbreviation, municipality_code, prov_pie_event, nation_bar_ev, region_ev, lang_chosen){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code, prov_pie_event, NULL, nation_bar_ev, region_ev, NULL, lang_chosen)
        sex <- tryCatch({
                aggregate(dataset$tot_arrivi ~ dataset$sesso_str, FUN = sum)},
                error = function(cond){
                        message("Aggregate function does not have row to aggregate")
                        sex = data.frame(matrix(nrow = 1, ncol = 2))
                })
        names(sex) <- c("sesso", "movimenti")
        sex <- sex %>% filter(grepl("[F|M]", sesso))
        print(sex)

}

get_accomodated_type <- function(dataset, province_abbreviation, municipality_code, prov_pie_event, profile_pie_event, nation_bar_ev, region_bar_ev, lang_chosen){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code, prov_pie_event, profile_pie_event, nation_bar_ev, region_bar_ev, NULL, lang_chosen)
        accomodated_type <- tryCatch({
                aggregate(dataset$tot_arrivi ~ dataset$tipoalloggiato_str, FUN = sum)},
                error = function(cond){
                        message("Aggregate function does not have row to aggregate")
                        accomodated_type = data.frame(matrix(nrow = 1, ncol = 2))
                        })
        names(accomodated_type) <- c("tipo_alloggiato", "arrivi")
        accomodated_type <- accomodated_type %>% filter(!(tipo_alloggiato == ''))
        accomodated_type = accomodated_type[order(accomodated_type$arrivi, decreasing = T), ]
        accomodated_type$arrivi = sapply(accomodated_type$arrivi, FUN = function(x){
          tot <- sum(accomodated_type$arrivi)        
          if (is.numeric(x) & (is.numeric(tot) & tot != 0)){
              round(x*100/tot)
          }else{
            0
          }
        })
        accomodated_type

}

get_age_range <- function(dataset, province_abbreviation, municipality_code, prov_pie_event, profile_pie_event, nation_bar_ev, region_bar_ev, accomodated_bar_ev, lang_chosen){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code, prov_pie_event, profile_pie_event, nation_bar_ev, region_bar_ev, accomodated_bar_ev, lang_chosen)        
        dd <- dataset  %>% 
                filter(!(fasciaeta == "ND"))
        age_range <- tryCatch({
                aggregate(dd$tot_arrivi ~ dd$fasciaeta, FUN = sum)},
                error = function(cond){
                        message("Aggregate function does not have row to aggregate")
                        age_range = data.frame(matrix(nrow = 1, ncol = 2))
                        })
        
        
        names(age_range) = c("fasciaeta", "arrivi")
        result <- age_range %>% mutate(fasciaeta = gsub("eta_", "", fasciaeta) %>% gsub("65\\+", ">65", .))
        result$fasciaeta = factor(x = result$fasciaeta, levels = result$fasciaeta)
        names(result) <- c("eta", "arrivi")
        result$arrivi = sapply(result$arrivi, FUN = function(x){
                tot <- sum(result$arrivi) 
                if (is.numeric(x) & (is.numeric(tot) & tot != 0)){
                   round(x*100/tot)
                }else{
                  0
                }
        })
        result
   
}