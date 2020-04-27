
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


load("analysis/in/data.RData")

#### TOPIC MODELS

## privat topics
data_priv <- data[ which(data$OF01_01 != ""), ]

corpus_priv <- corpus(as.character(data_priv$OF01_01),
                 docvars = data.frame(socmeduse = data_priv$ME02_03,
                                      lialone = data_priv$lialone
                                      ))

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

DFM_priv <- DFM_priv[complete.cases(DFM_priv@docvars$lialone), ] # listwise deletion


# und los
Ntopic <- 8

private_topics <- stm(DFM_priv,
                    K = Ntopic,
                    seed = 1337,
                    verbose = T,
                    init.type = "Spectral",
                    prevalence =~lialone,
                    data = DFM_priv@docvars
                    #, control = list(alpha = 1)
                    )

topic_prob_privat <- summary(private_topics)
topic_prob_privat <- topic_prob_privat$prob

write.xlsx(topic_prob_privat, file = "analysis/out/private_topics.xlsx")


plot(private_topics, type = "summary")
dev.copy(png,"analysis/out/private_topic_freq.png")
dev.off()



prep_priv <- estimateEffect(1:Ntopic ~ lialone, private_topics,
                       meta = DFM_priv@docvars, uncertainty = "Global")

plot(prep_priv, covariate = "lialone",
     model = private_topics, method = "difference",
     cov.value1 = "living alone", cov.value2 = "not living alone",
     xlab = "Not living alone ..... living alone",
     main = "Effect of living alone")
dev.copy(png,"analysis/out/private_effect_alone.png")
dev.off()


