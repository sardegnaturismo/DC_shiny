source("R/translator.R")
library(htmltools)
generateHeaderTitle <- function(language){
        
         x = paste(tr("header_title", language), "(", tr("fonte", language), "<a href='http://operatori.sardegnaturismo.it/it/sired-0'>SIRED</a>", ")") 

}

generateRadio <- function(language){
        print("Generate Radio:")
        print(tr("misura", language))
        radio_title = tr("misura", language)
        choice1 = tr("arrivi", language)
        choice2 = tr("presenze", language)
        radioButtons("measure", label = radio_title,
                     c(choice1, choice2), selected = choice1)
}

generateMapFilterButton <- function(language){
        button_label = tr("elimina_filtri_mappe", language)
        actionButton("stop_map_filters", label = button_label, class='stop_filter')
}

generateFilterButton <- function(language, id, label){
        button_label = tr(label, language)
        actionButton(id, label = button_label, class='stop_filter')
        
        
}