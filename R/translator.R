


tr <- function(text, language){ # translates text into current language
        print(language)
        x = sapply(text,function(s) translation[[s]][[language]], USE.NAMES=FALSE)
        as.character(x)
}