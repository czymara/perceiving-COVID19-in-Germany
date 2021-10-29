

#####
# RELATIVE FREQUENCY ANALYSIS
#####


# packages
packages <- c("dplyr", "tidyverse",
              "magrittr", "tidytext", "quanteda")
lapply(packages, library, character.only = TRUE)


# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/going/CoronaTopicModels/"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)

SEED <- 1337

options(stringsAsFactors = FALSE)
library(ggplot2); theme_set(theme_bw() +
                              theme(axis.line = element_line(colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank(),
                                    axis.text = element_text(color="black"),
                                    axis.ticks = element_line(colour = "black")))


load("analysis/in/data.RData")


# prep
corpus_priv <- corpus(as.character(data$OF01_01),
                      docvars = data.frame(wohntyp = data$wohntyp))

toks_priv <- tokens(corpus_priv, remove_punct = T,
                    remove_numbers = T,
                    remove_symbols = T,
                    remove_separators = F,
                    split_hyphens = T,
                    remove_url = T,
                    include_docvars = T)

stopWords_de <- read.table("analysis/in/stopwords-de.txt", encoding = "UTF-8", colClasses=c("character"))

toks_priv <-  tokens_remove(toks_priv, c(stopWords_de$V1, stopwords("german")),
                            case_insensitive = TRUE, padding = FALSE)

toks_priv <- tokens_wordstem(toks_priv, language = "german")

DFM_priv <- dfm(toks_priv)

DFM_priv <- dfm_trim(DFM_priv, max_docfreq = 0.20,  min_docfreq = 0.001, docfreq_type = "prop")

rowsum_priv <- apply(DFM_priv , 1, sum) # identify text with no common terms
DFM_priv   <- DFM_priv[rowsum_priv > 0, ]  #remove all docs without these terms


DFM_group <- dfm_group(DFM_priv, groups = DFM_priv@docvars$wohntyp)

### ANALYSIS
DFM_priv@docvars$singleparentsVSrest <-
  dplyr::recode(DFM_priv@docvars$wohntyp,
                "single parent" = 1,
                .default = 0)

singleparentsVSrest <- textstat_keyness(DFM_priv,
                              target =  DFM_priv@docvars$singleparentsVSrest == 1)

textplot_keyness(singleparentsVSrest)

