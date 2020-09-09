
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
# TOPIC MODELS
###

# gender variable

data_priv <- data[ which(data$OF01_01 != ""), ]

corpus_priv <- corpus(as.character(data_priv$OF01_01),

                      docvars = data.frame(wohntyp = data_priv$wohntyp,
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

# listwise deletion
DFM_priv <- DFM_priv[complete.cases(DFM_priv@docvars$wohntyp), ]


Ntopic <- 15

set.seed(SEED)
pooledhh_topics <- stm(DFM_priv,
                     K = Ntopic,
                     seed = 1337,
                     verbose = F,
                     init.type = "Spectral",
                     prevalence = ~wohntyp,
                     data = DFM_priv@docvars,
                     #, control = list(alpha = 1)
)

summary(pooledhh_topics)


# table
topic_prob_hhpooled <- summary(pooledhh_topics)
topic_prob_hhpooled <- t(topic_prob_hhpooled$prob)

# topic names
topicNames <- c("...") # 8

colnames(topic_prob_hhpooled) <- topicNames

write.xlsx(topic_prob_hhpooled,
           file = paste0("analysis/out/topics_hhpooled.xlsx"))


# topics per document
beta <- tidy(gender_topics)

# words per topic
gamma <- tidy(gender_topics, matrix = "gamma",
              document_names = rownames(DFM_priv))


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
  mutate(topic = topicNames,
         topic = reorder(topic, gamma))

# win.metafile("analysis/out/gender/topic_freq.wmf")
pdf("analysis/out/gender/topic_freq.pdf")
gamma_terms %>%
  ggplot(aes(x = reorder(topic, gamma), gamma)) +
  geom_col(show.legend = F) +
  coord_flip() +
  labs(x = NULL,
       y = "Topic probability") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
dev.off()


# difference between genders
est <- estimateEffect(1:Ntopic ~ wohntyp, pooledhh_topics,
                      meta = DFM_priv@docvars, uncertainty = "Global")

# plot gender differences

# ref plot
dev.copy(png, "analysis/out/private_topic_freq_effectpooled.png", height = 1500)
plot(est, covariate = "wohntyp",
     model = topic_prob_hhpooled, method = "pointestimate", # marginal topic proportion for each level
     cov.value1 = "male", cov.value2 = "female"
     )
dev.off()


# win.metafile("analysis/out/gender/gender_effect.wmf")
pdf("analysis/out/gender/gender_effect.pdf")
ggplot(data = effecttable) +
  geom_vline(xintercept = 0, # linetype=4,
             colour="black") +
  geom_errorbarh(aes(xmin=CIlower, xmax=CIupper,
                     y = labels,
                     colour = "grey")) +
  geom_point(aes(y = labels,
                 x = effects,
                 colour = as.factor(sig))) +
  xlab("Men                                                                Women") + ylab("") +
  theme(legend.position = "none")
dev.off()

