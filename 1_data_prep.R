
#####
# preparation
#####


# packages
packages <- c("readxl", "dplyr", "tidyverse",
              "magrittr", "tidytext", "quanteda")
lapply(packages, library, character.only = TRUE)


# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/lehrstuhlstuff/Corona Survey/"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)

data <- read.csv("Backup 08.04/sdata_corona-survey_2020-04-08_10-58.csv", header = TRUE, sep = "\t", encoding = "UTF-8")

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
data$lonlindex <- rowSums(sapply(data[lonvars], as.numeric))

summary(data$lonlindex)



save.image("cc/data.RData")
