
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

# export
write.table(as.character(examplecomments$docs),
           # file = "analysis/out/gender/examplecomments.txt",
            sep = "\t",
            row.names = FALSE)


# table
topic_prob_gender <- summary(gender_topics)
topic_prob_gender <- t(topic_prob_gender$prob)

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


# difference between domains
est <- estimateEffect(1:Ntopic ~ type, PrivPol_topics,
                      meta = DFM_comb@docvars, uncertainty = "Global")

# ref plot
plot(est, covariate = "type",
     model = PrivPol_topics, method = "difference",
     cov.value1 = "priv", cov.value2 = "pol"
     )

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

