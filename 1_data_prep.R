
#####
# preparation
#####


# packages
packages <- c("readxl", "dplyr", "tidyverse",
              "magrittr", "tidytext", "quanteda", "textcat")
lapply(packages, library, character.only = TRUE)


# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/Corona Survey (Alexander Langenkamp)/Topic model project/"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)

data <- read.csv("C:/Users/czymara.local/PowerFolders/Corona Survey (Alexander Langenkamp)/Backup 08.04/sdata_corona-survey_2020-04-08_10-58.csv",
                 header = TRUE, sep = "\t", encoding = "UTF-8")

head(data$OF01_01) # privat

head(data$OF02_01) # politik


### recode missings
missings <- c(".a", ".b", ".c", ".i")

data$DE06 <- sapply(data$DE06, function(x) replace(x,x %in% missings, NA) ) # haushaltstyp

# loneliness
lonvars <- c(paste("LO01_0", 1:9, sep=""), "LO01_10", "LO01_11")
data[lonvars] <- sapply(data[lonvars], function(x) replace(x,x %in% missings, NA) )
# remove(lonvars)

data$DE11 <- sapply(data$DE11, function(x) replace(x,x %in% missings, NA) ) # partei

# media cons
mediavars <- c(paste("ME02_0", 1:7, sep=""))
data[mediavars] <- sapply(data[mediavars], function(x) replace(x,x %in% missings, NA) )
remove(mediavars)


### generate variables

# living alone
data %<>%
  mutate(lialone = if_else(DE06==3,
                           "living alone",
                           "not living alone"))

table(data$lialone)

# loniless index
lonvars_neg <- c("LO01_01", "LO01_04", "LO01_06",
             "LO01_07", "LO01_08", "LO01_11")

data[lonvars] <- sapply(data[lonvars], function(x) as.numeric(x)*-1 + 6) # rescale

data$lonlindex <- rowSums(sapply(data[lonvars], as.numeric))

summary(data$lonlindex)


data$longquart <- cut(data$lonlindex, breaks=quantile(data$lonlindex,
                                                      probs=seq(0,1, by=0.25), na.rm=TRUE),
                      include.lowest = T)

table(data[data$lonlindex == 22, "longquart"]) # disjunkt


# check for non german answers
data$language <- textcat(data$OF01_01)
table(data$language)

table(data[data$language == "russian-iso8859_5", "OF01_01"])


save.image("analysis/in/data.RData")


