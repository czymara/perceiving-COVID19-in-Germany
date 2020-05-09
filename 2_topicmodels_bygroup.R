
#####
# TOPIC MODELS (total)
#####


# packages
packages <- c("readxl", "dplyr", "tidyverse", "xlsx",
              "magrittr", "imputeTS", "tidytext", "quanteda", "stm")
lapply(packages, library, character.only = TRUE)


# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/Corona Survey (Alexander Langenkamp)/Topic model project/"
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
                                    panel.background = element_blank()))

load("analysis/in/data.RData")

###
# TOPIC MODELS
###

wohntypen <- levels(data$wohntyp)

### privat item
graphics <- lapply(wohntypen, function(wohntypenx){
  data_priv <- data[ which(data$wohntyp == wohntypenx), ]

  data_priv <- data_priv[ which(data_priv$OF01_01 != ""), ]
  corpus_priv <- corpus(as.character(data_priv$OF01_01),
                # docvars = data.frame(socmeduse = data_priv$ME02_03,
                #                      lialone = data_priv$lialone,
                #                      id = data_priv$CASE)
                                      )

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
private_topics <- stm(DFM_priv,
                    K = Ntopic,
                    seed = 1337,
                    verbose = F,
                    init.type = "Spectral",
                   # prevalence = ~lialone,
                   # data = DFM_priv@docvars,
                   # content = ~lialone
                   #, control = list(alpha = 1)
                    )


#corpusred <- corpus_subset(corpus_priv, id %in% DFM_priv@docvars$id)
#examplecomments <- findThoughts(private_topics, texts = corpusred,
#            n = 2, topics = 5)

# plotQuote(examplecomments, width = 30, main = "Financial worries")


topic_prob_privat <- summary(private_topics)
topic_prob_privat <- t(topic_prob_privat$prob)
colnames(topic_prob_privat) <- sapply(1:8, function(x) paste0("Topic ", x))

write.xlsx(topic_prob_privat,
           file = paste0("analysis/out/private_topics_", wohntypenx, ".xlsx"))

# topics per document
beta <- tidy(private_topics)

# words per topic
gamma <- tidy(private_topics, matrix = "gamma",
              document_names = rownames(DFM_priv)
              )

# plot
topterms_pertopic <- beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(3, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = "\n ")) %>%
  unnest(cols = c(terms))


gamma_terms <- gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  #  arrange(desc(gamma)) %>%
  mutate(topic = as.matrix(topterms_pertopic[2]),
    topic = reorder(topic, gamma))

gamma_terms %>%
  ggplot(aes(x = reorder(topic, gamma), gamma)) +
  geom_col(show.legend = F) +
  coord_flip() +
  labs(title =wohntypenx,
       x = NULL,
       y = "topic salience") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
})

lapply(1:4, function(x){
plot(test[[x]])
dev.copy(png, paste0("analysis/out/private_topic_freq_", x, ".png"))
dev.off()
})

graphscomb_topics <- plot_grid(graphics[[1]],
                               graphics[[2]],
                               graphics[[3]],
                               graphics[[4]])

title_topics <- ggdraw() + draw_label("Topics", fontface='bold')

plot_grid(title_topics, graphscomb_topics,
          ncol=1, rel_heights=c(0.1, 1))

dev.copy(png,"analysis/out/private_topicmodels_wohntyp.png", height = 800)
dev.off()

