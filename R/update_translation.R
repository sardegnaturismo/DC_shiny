# update the processed translation file translation.bin
# run this every time dictionary.csv is updated 
# it reads the look-up table in dictionary.csv and turns it into a 2D list

library(plyr)
translationContent <- read.delim("internazionalization/it_en_dictionary2.csv", header = TRUE, sep = ";", as.is = TRUE, encoding = "UTF-8") 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))

save(translation, file = "internazionalization/translation.bin")


