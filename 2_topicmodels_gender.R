
#####
# TOPIC MODELS (gender diff)
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

# gender variable
data %<>%
  mutate(gender = if_else(DE03==1,
                          "male",
                          if_else(DE03==2,
                                  "female",
                                  NULL)))

data_priv <- data[ which(data$OF01_01 != ""), ]
corpus_priv <- corpus(as.character(data_priv$OF01_01),
                      docvars = data.frame(gender = data_priv$gender,
                                           id = data_priv$CASE
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

DFM_priv <- DFM_priv[complete.cases(DFM_priv@docvars$gender), ] # listwise deletion

Ntopic <- 8

set.seed(SEED)
gender_topics <- stm(DFM_priv,
                      K = Ntopic,
                      seed = 1337,
                      verbose = F,
                      init.type = "Spectral",
                      prevalence = ~gender,
                      data = DFM_priv@docvars,
                      # content = ~lialone
                      #, control = list(alpha = 1)
                      )


corpusred <- corpus_subset(corpus_priv, id %in% DFM_priv@docvars$id)
examplecomments <- findThoughts(gender_topics, texts = corpusred,
            n = 2, topics = c(6, 8, 7, 1))

# 6 and 8 more likely for women:
examplecomments$docs$`Topic 6`[[1]]
examplecomments$docs$`Topic 6`[[2]]
docvars(examplecomments$docs$`Topic 6`)

examplecomments$docs$`Topic 8`[[1]]
examplecomments$docs$`Topic 8`[[2]]
docvars(examplecomments$docs$`Topic 8`)

# 7 and 1 more likely for men
examplecomments$docs$`Topic 7`[[1]]
examplecomments$docs$`Topic 7`[[2]]
docvars(examplecomments$docs$`Topic 7`)

examplecomments$docs$`Topic 1`[[1]]
examplecomments$docs$`Topic 1`[[2]]
docvars(examplecomments$docs$`Topic 1`)


# plotQuote(examplecomments, width = 30, main = "Financial worries")


topic_prob_gender <- summary(gender_topics)
topic_prob_gender <- t(topic_prob_gender$prob)
colnames(topic_prob_gender) <- sapply(1:8, function(x) paste0("Topic ", x))

write.xlsx(topic_prob_gender,
           file = paste0("analysis/out/gender/    topics.xlsx"))


# topics per document
beta <- tidy(gender_topics)

# words per topic
gamma <- tidy(gender_topics, matrix = "gamma",
              document_names = rownames(DFM_priv))


# version2
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
  labs(x = NULL,
       y = "topic salience") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

dev.copy(png,"analysis/out/gender/topic_freq.png")
dev.off()


# difference between genders
est <- estimateEffect(1:Ntopic ~ gender, gender_topics,
                      meta = DFM_priv@docvars, uncertainty = "Global")

# plot gender differences

# ref plot
plot(est, covariate = "gender",
     model = private_topics, method = "difference",
     cov.value1 = "male", cov.value2 = "female")

# better version
effects <- sapply(1:8, function(x) est$parameters[[x]][[x]]$est[2])
se <- sapply(1:8, function(x) sqrt(est$parameters[[x]][[x]]$vcov[2,2]))

effecttable <- as.data.frame(cbind(effects, se))

effecttable$CIupper <- effecttable$effects + 1.96*effecttable$se
effecttable$CIlower <- effecttable$effects - 1.96*effecttable$se
effecttable <- effecttable*-1
effecttable %<>%
  mutate(sig = if_else(CIupper<0 & CIlower<0 |
                         CIupper>0 & CIlower>0,
                       1, 0))
effecttable$labels <- with(gamma_terms, reorder(topic, gamma))[1:8]

ggplot(data = effecttable) +
  geom_point(aes(y = gamma_terms$topic[1:8],
                 x = effects,
                 colour = as.factor(sig))) +
  geom_vline(xintercept = 0, # linetype=4,
             colour="black") +
  geom_errorbarh(aes(xmin=CIlower, xmax=CIupper,
                     y = gamma_terms$topic[1:8],
                     alpha = 0.2, colour = "grey")) +
  xlab("Differences between women compared to men") + ylab("Topics") +
  theme(legend.position = "none")

dev.copy(png,"analysis/out/gender/gender_effect.png")
dev.off()



