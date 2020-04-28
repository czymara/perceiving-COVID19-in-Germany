
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

### privat item
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


### und los
Ntopic <- 8

set.seed(SEED)
private_topics <- stm(DFM_priv,
                    K = Ntopic,
                    seed = 1337,
                    verbose = F,
                    init.type = "Spectral",
                    prevalence = ~lialone,
                    data = DFM_priv@docvars,
                    # content = ~lialone
                    #, control = list(alpha = 1)
                    )

topic_prob_privat <- summary(private_topics)
topic_prob_privat <- t(topic_prob_privat$prob)
colnames(topic_prob_privat) <- sapply(1:8, function(x) paste0("Topic ", x))

write.xlsx(topic_prob_privat, file = "analysis/out/private_topics.xlsx")

# topics per document
beta <- tidy(private_topics)

# words per topic
gamma <- tidy(private_topics, matrix = "gamma",
              document_names = rownames(DFM_priv)
              )

# plot
plot(private_topics, type = "summary")
dev.copy(png,"analysis/out/private_topic_freq.png")
dev.off()

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

dev.copy(png,"analysis/out/private_topic_freq_v2.png")
dev.off()



# difference for those living alone
est <- estimateEffect(1:Ntopic ~ lialone, private_topics,
                       meta = DFM_priv@docvars, uncertainty = "Global")

# plot
plot(est, covariate = "lialone",
     model = private_topics, method = "difference",
     cov.value1 = "living alone", cov.value2 = "not living alone",
     xlab = "Not living alone ..... living alone",
     main = "Effect of living alone")

dev.copy(png,"analysis/out/private_effect_alone.png")
dev.off()

# version 2
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
  xlab("Effect of living alone") + ylab("Topics") +
  theme(legend.position = "none")

dev.copy(png,"analysis/out/private_effect_alone_v2.png")
dev.off()

