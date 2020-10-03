
#####
# TOPIC MODELS (gender diff)
#####


# packages
packages <- c("readxl", "dplyr", "tidyverse", "xlsx",
              "magrittr", "imputeTS", "tidytext", "quanteda", "stm")
lapply(packages, library, character.only = TRUE)


# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/projects/CoronaTopicModels/"
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


###
# Preparation
###

data_edit <- data[ which(data$OF01_01 != ""), ] # priv
data_edit <- data[ which(data$OF02_01 != ""), ] # pol

priv <- as.data.frame(data_edit$OF01_01)
names(priv)[1] <- "comment"
priv$type <- "priv"

pol <- as.data.frame(data_edit$OF02_01)
names(pol)[1] <- "comment"
pol$type <- "pol"

PrivPolComb <- rbind(priv, pol)

corpus <- corpus(as.character(PrivPolComb$comment),
                      docvars = data.frame(type = PrivPolComb$type))

toks <- tokens(corpus, remove_punct = T,
                    remove_numbers = T,
                    remove_symbols = T,
                    remove_separators = F,
                    split_hyphens = T,
                    remove_url = T,
                    include_docvars = T)

stopWords_de <- read.table("analysis/in/stopwords-de.txt", encoding = "UTF-8", colClasses=c("character"))

toks <-  tokens_remove(toks, c(stopWords_de$V1, stopwords("german")),
                            case_insensitive = TRUE, padding = FALSE)

toks <- tokens_wordstem(toks, language = "german")

DFM_comb <- dfm(toks)

DFM_comb <- dfm_trim(DFM_comb, max_docfreq = 0.20,  min_docfreq = 0.001, docfreq_type = "prop")


rowsum <- apply(DFM_comb , 1, sum) # identify text with no common terms
DFM_comb   <- DFM_comb[rowsum > 0, ]  #remove all docs without these terms


###
# Analysis
###

Ntopic <- 8

set.seed(SEED)
PrivPol_topics <- stm(DFM_comb,
                     K = Ntopic,
                     seed = 1337,
                     verbose = F,
                     init.type = "Spectral",
                     prevalence = ~type,
                     data = DFM_comb@docvars,
                     # content = ~lialone
                     #, control = list(alpha = 1)
                     )

summary(PrivPol_topics)

corpusred <- corpus_subset(corpus_priv, id %in% DFM_priv@docvars$id)
examplecomments <- findThoughts(gender_topics, texts = corpusred,
                                n = 10, topics = c(1, 7, 6, 8))

# table
topic_prob_PrivPol <- summary(PrivPol_topics)
topic_prob_PrivPol <- t(topic_prob_PrivPol$prob)

# topic names
topicNames <- c("Economy", # 1
                "Everyday life", # 2
                "Family", # 3
                "Individual worries", # 4
                "Society", # 5
                "Social contacts", # 6
                "Paid work", # 7
                "Childcare") # 8

colnames(topic_prob_gender) <- topicNames

write.xlsx(topic_prob_gender,
           file = paste0("analysis/out/gender/topics.xlsx"))


# difference between domains
est <- estimateEffect(1:Ntopic ~ type, PrivPol_topics,
                      meta = DFM_comb@docvars, uncertainty = "Global")

# plot
plot(est, covariate = "type",
     model = PrivPol_topics, method = "difference",
     cov.value1 = "priv", cov.value2 = "pol"
     )

