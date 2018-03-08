translate_vector <- function(data_vector, language){
        normalized_data <- tolower(as.character(data_vector)) %>% gsub(" ", "", .)
        translate <- sapply(normalized_data, function(x) tr(x, language))
}