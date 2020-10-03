
#####
# TOPIC MODELS (total)
#####


# packages
packages <- c("readxl", "dplyr", "tidyverse", "xlsx", "cowplot",
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
# TOPIC MODELS
###

wohntypenlvl <- levels(data$wohntyp)

## create separate corpora (for example comments later)
corpora <- lapply(wohntypenlvl, function(wohntypenx){
  data_priv <- data[ which(data$wohntyp == wohntypenx), ]

  data_priv <- data_priv[ which(data_priv$OF01_01 != ""), ]
  assign(paste0("corp_", wohntypenx),
         corpus(as.character(data_priv$OF01_01),
                                             docvars = data.frame(id = data_priv$CASE)))
})


### run topic models
topicmodels <- lapply(wohntypenlvl, function(wohntypenx){
  data_priv <- data[ which(data$wohntyp == wohntypenx), ]

  data_priv <- data_priv[ which(data_priv$OF01_01 != ""), ]
  corpus_priv <- corpus(as.character(data_priv$OF01_01),
                 docvars = data.frame(id = data_priv$CASE))

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

# DFM_priv <- DFM_priv[complete.cases(DFM_priv@docvars$lialone), ] # listwise deletion

Ntopic <- 8

set.seed(SEED)
topicmodels <-assign(paste0("topicmodel_", wohntypenx),
       stm(DFM_priv,
           K = Ntopic,
           seed = 1337,
           verbose = F,
           data = DFM_priv@docvars
                    ))

# topic_prob_privat <- summary(paste0("topicmodel_", wohntypenx))
# topic_prob_privat <- t(topic_prob_privat$prob)
# colnames(topic_prob_privat) <- sapply(1:8, function(x) paste0("Topic ", x))

# write.xlsx(topic_prob_privat,
#           file = paste0("analysis/out/private_topics_", wohntypenx, ".xlsx"))
})

# plot
plots <- lapply(1:4, function(x){
beta <- tidy(topicmodels[[x]])

# words per topic
gamma <- tidy(topicmodels[[x]], matrix = "gamma",
             # document_names = rownames(DFM_priv)
              )
})


# topic names

# couple with kids
plots[[1]]$topiclbl <- recode(as.character(plots[[1]]$topic),
                              "1" = "Health\nworries",
                              "2" = "Parenting",
                              "3" = "Home",
                              "4" = "Childcare",
                              "5" = "Family\nworries",
                              "6" = "Isolation",
                              "7" = "Everyday\nlife",
                              "8" = "Schooling"
                              )

plotCoupleKid <- ggplot(data = plots[[1]],
                        aes(y = reorder(topiclbl, gamma),
                            x=gamma
                        )) +
  stat_summary(fun="mean", geom="bar") +
  labs(title = "Couple with kids",
       y = NULL,
       x = "Topic probability")

# living alone
plots[[2]]$topiclbl <- recode(as.character(plots[[2]]$topic),
                              "1" = "Restrictions",
                              "2" = "Everyday\nworries",
                              "3" = "Negative\nconsequences",
                              "4" = "Family",
                              "5" = "Loneliness",
                              "6" = "Isolation",
                              "7" = "Social life",
                              "8" = "Activities"
                              )

plotLivingAlone <- ggplot(data = plots[[2]],
                          aes(y = reorder(topiclbl, gamma),
                              x=gamma
                          )) +
  stat_summary(fun="mean", geom="bar") +
  labs(title = "Living alone, no kids",
       y = NULL,
       x = "Topic probability")


# not alone, no kid
plots[[3]]$topiclbl <- recode(as.character(plots[[3]]$topic),
                              "1" = "Consequences",
                              "2" = "Family life",
                              "3" = "Social life",
                              "4" = "Everyday\nlife",
                              "5" = "Leisure",
                              "6" = "Behavior",
                              "7" = "Partner",
                              "8" = "Isolation"
                              )

plotNotAloneNoKid <- ggplot(data = plots[[3]],
                            aes(y = reorder(topiclbl, gamma),
                                x=gamma
                            )) +
  stat_summary(fun="mean", geom="bar") +
  labs(title = "Not alone, no kids",
       y = NULL,
       x = "Topic probability")

# distribution of topics
aggregate(plots[[3]]$gamma, list(plots[[3]]$topiclbl), mean)

# how often is topic most frequent one (rank-1)
domtopSingNoKid <- plots[[3]] %>%
  group_by(document) %>%
  slice(which.max(gamma))

table(domtopSingNoKid[4])

max(table(domtopSingNoKid[4]))/sum(table(domtopSingNoKid[4])) # how large the share with this topic is no 1


# single parents
plots[[4]]$topiclbl <- recode(as.character(plots[[4]]$topic),
       "1" = "Positive\neffects",
       "2" = "Care work",
       "3" = "Negative\neffects",
       "4" = "Financial\nworries",
       "5" = "Feelings",
       "6" = "Health",
       "7" = "Everyday\nlife",
       "8" = "Social\ncontacts"
       )

plotSingleParent <- ggplot(data = plots[[4]],
                           aes(y = reorder(topiclbl, gamma),
                               x=gamma
                           )) +
  stat_summary(fun="mean", geom="bar") +
  labs(title = "Single parent",
       y = NULL,
       x = "Topic probability")

# example comments
findThoughts(topicmodels[[4]], texts = corpora[[4]],
                                n = 10, topics = 4)


# distribution of topics
aggregate(plots[[4]]$gamma, list(plots[[4]]$topiclbl), mean)

# how often is topic most frequent one (rank-1)
domtopSingPar <- plots[[4]] %>%
  group_by(document) %>%
  slice(which.max(gamma))

table(domtopSingPar[4])

max(table(domtopSingPar[4]))/sum(table(domtopSingPar[4])) # how large the share with this topic is no 1



## combine all plots
plot_grid(plotSingleParent,
          plotCoupleKid,
          plotLivingAlone,
          plotNotAloneNoKid
          )

dev.copy(png,"analysis/out/private_topicmodels_wohntyp_en.png", height = 800)
dev.off()


