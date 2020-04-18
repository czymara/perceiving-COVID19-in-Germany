
#####
# TOPIC MODELS (living alone, separate)
#####


# packages
packages <- c("readxl", "dplyr", "tidyverse", "xlsx",
              "magrittr", "imputeTS", "tidytext", "quanteda", "stm")
lapply(packages, library, character.only = TRUE)


# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/lehrstuhlstuff/Corona Survey/"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)


load("coRona2/in/data.RData")


Ntopic <- 8

data <- data[ which(data$OF01_01 != ""), ]


## dummy
datallivalodum <- data[ which(data$lialone == "living alone" | data$lialone == "not living alone") , ]

corpus_lialo <- corpus(as.character(datallivalodum$OF01_01),
                        docvars = data.frame(socmeduse = datallivalodum$ME02_03,
                                             lialone = datallivalodum$lialone))

toks_lialodum <- tokens(corpus_lialo, remove_punct = T,
                      remove_numbers = T,
                      remove_symbols = T,
                      remove_separators = F,
                      split_hyphens = T,
                      remove_url = T,
                      include_docvars = T)

stopWords_de <- read.table("coRona2/in/stopwords-de.txt", encoding = "UTF-8", colClasses=c("character"))

toks_lialodum <-  tokens_remove(toks_lialodum, c(stopWords_de$V1, stopwords("german")),
                              case_insensitive = TRUE, padding = FALSE)

toks_lialodum <- tokens_wordstem(toks_lialodum, language = "german")

DFM_lialo <- dfm(toks_lialodum)

DFM_lialo <- dfm_trim(DFM_lialo, max_docfreq = 0.20,  min_docfreq = 0.001, docfreq_type = "prop")


rowsum_lialodum <- apply(DFM_lialo , 1, sum) # identify text with no common terms
DFM_lialo   <- DFM_lialo[rowsum_lialodum > 0, ]  #remove all docs without these terms

DFM_lialo <- DFM_lialo[complete.cases(DFM_lialo@docvars$lialone), ] # listwise deletion


# model
lialodum_topics <- stm(DFM_lialo,
                        K = Ntopic,
                        seed = 1337,
                        verbose = T,
                        init.type = "Spectral",
                        prevalence = ~lialone,
                        content = ~lialone,
                        data = DFM_lialo@docvars
                        #, control = list(alpha = 1)
                       )

summary(lialodum_topics)

sapply(1:8, function(x){
  plot(lialodum_topics, type = "perspectives", topics = x)
  dev.copy(png,paste("coRona2/out/topiccomp/private_topic_lialo_", x, ".png", sep=""))
  dev.off()
  })


### separate analysis

## living alone
datalone <- data[ which(data$lialone == "living alone"), ]


corpus_lon <- corpus(as.character(datalon$OF01_01),
                     docvars = data.frame(socmeduse = datalon$ME02_03,
                                          lialone = datalon$lialone
                     ))

toks_lon <- tokens(corpus_lon, remove_punct = T,
                   remove_numbers = T,
                   remove_symbols = T,
                   remove_separators = F,
                   split_hyphens = T,
                   remove_url = T,
                   include_docvars = T)

toks_lon <-  tokens_remove(toks_lon, c(stopWords_de$V1, stopwords("german")),
                           case_insensitive = TRUE, padding = FALSE)

toks_lon <- tokens_wordstem(toks_lon, language = "german")

DFM_lon <- dfm(toks_lon)

DFM_lon <- dfm_trim(DFM_lon, max_docfreq = 0.20,  min_docfreq = 0.001, docfreq_type = "prop")


rowsum_lon <- apply(DFM_lon , 1, sum) # identify text with no common terms
DFM_lon   <- DFM_lon[rowsum_lon > 0, ]  #remove all docs without these terms

DFM_lon <- DFM_lon[complete.cases(DFM_lon@docvars$lialone), ] # listwise deletion


# model
lonely_topics <- stm(DFM_lon,
                     K = Ntopic,
                     seed = 1337,
                     verbose = T,
                     init.type = "Spectral",
                     data = DFM_lon@docvars
                     #, control = list(alpha = 1)
                     )

topic_lon_privat <- summary(lonely_topics)
topic_lon_privat <- topic_lon_privat$prob

write.xlsx(topic_lon_privat, file = "coRona2/out/private_topics_lonely.xlsx")


plot(lonely_topics, type = "summary")
dev.copy(png,"coRona2/out/private_topic_lonely_freq.png")
dev.off()


## not living alone
datanotlon <- data[ which(data$lialone == "not living alone"), ]


corpus_notlon <- corpus(as.character(datanotlon$OF01_01),
                        docvars = data.frame(socmeduse = datanotlon$ME02_03,
                                             lialone = datanotlon$lialone
                        ))

toks_notlon <- tokens(corpus_notlon, remove_punct = T,
                      remove_numbers = T,
                      remove_symbols = T,
                      remove_separators = F,
                      split_hyphens = T,
                      remove_url = T,
                      include_docvars = T)

toks_notlon <-  tokens_remove(toks_notlon, c(stopWords_de$V1, stopwords("german")),
                              case_insensitive = TRUE, padding = FALSE)

toks_notlon <- tokens_wordstem(toks_notlon, language = "german")

DFM_notlon <- dfm(toks_notlon)

DFM_notlon <- dfm_trim(DFM_notlon, max_docfreq = 0.20,  min_docfreq = 0.001, docfreq_type = "prop")


rowsum_notlon <- apply(DFM_notlon , 1, sum) # identify text with no common terms
DFM_notlon   <- DFM_notlon[rowsum_notlon > 0, ]  #remove all docs without these terms

DFM_notlon <- DFM_notlon[complete.cases(DFM_notlon@docvars$lialone), ] # listwise deletion


# model
notlonely_topics <- stm(DFM_notlon,
                        K = Ntopic,
                        seed = 1337,
                        verbose = T,
                        init.type = "Spectral",
                        # prevalence =~lialone,
                        data = DFM_notlon@docvars
                        #, control = list(alpha = 1)
)

topic_notlon_privat <- summary(notlonely_topics)
topic_notlon_privat <- topic_notlon_privat$prob

write.xlsx(topic_notlon_privat, file = "coRona2/out/private_topics_notlonely.xlsx")


plot(notlonely_topics, type = "summary")
dev.copy(png,"coRona2/out/private_topic_notlonely_freq.png")
dev.off()

