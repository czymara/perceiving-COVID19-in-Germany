
#####
# CO-OCCURANCE ANALYSIS
#####


# packages
packages <- c("tidyverse", "magrittr", "rtweet",
              "tidytext", "widyr", "quanteda",
              "influential", "ggraph", "cowplot", "SnowballC", "xlsx")
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



# load data
load("analysis/in/data.RData")

# convert DFM to tidytext
# privat_lonely_tidy <- tidy(DFM_priv) # common terms are already removed here


### create tidy text
data_priv <- data[ which(data$OF01_01 != ""), ]

answers_priv <- tibble(id = data_priv$CASE,
                       alonedummy = data_priv$lialone,
                       wohntyp = data_priv$wohntyp,
                       text = as.character(data_priv$OF01_01))

privat_lonely_tidy <- answers_priv %>% unnest_tokens(word, text)

# stop words
stopWords_de <- read.table("analysis/in/stopwords-de.txt", encoding = "UTF-8", colClasses=c('character'))

privat_lonely_tidy_reduc <- privat_lonely_tidy %>% filter(!word %in% c(stopWords_de$V1))

# remove numbers
privat_lonely_tidy_reduc <- privat_lonely_tidy_reduc[-grep("\\b\\d+\\b", privat_lonely_tidy_reduc$word),]

# stemming
privat_lonely_tidy_reduc$wordstem <- wordStem(privat_lonely_tidy_reduc$word, language = "de")

# listwise deletion
privat_lonely_tidy <- privat_lonely_tidy_reduc[!is.na(privat_lonely_tidy_reduc$wordstem),]

# count words co-occuring within users (id is user ID, I think)
word_pairs_private<- privat_lonely_tidy %>%
  pairwise_count(wordstem, id, sort = T)

word_pairs_private

# occuring with kind
word_pairs_private %>%
  filter(str_detect(item1, "^kind*"))


# privat_lonely_tidy$wordstem <- gsub("^soli.*", "soli", privat_lonely_tidy$wordstem)
privat_lonely_tidy$wordstem <- gsub("^famil.*", "famil", privat_lonely_tidy$wordstem)



###
# nach haushaltstyp
###

privat_lonely_tidy_alone <- privat_lonely_tidy[ which(privat_lonely_tidy$wohntyp == "living alone"), ]
privat_lonely_tidy_singlekids <- privat_lonely_tidy[ which(privat_lonely_tidy$wohntyp == "single parent"), ]
privat_lonely_tidy_couplekids <- privat_lonely_tidy[ which(privat_lonely_tidy$wohntyp == "couple with kids"), ]
privat_lonely_tidy_nokids <- privat_lonely_tidy[ which(privat_lonely_tidy$wohntyp == "not alone, no kids"), ]

priv_word_correlations_alone <- privat_lonely_tidy_alone %>%
  group_by(wordstem) %>%
  filter(n() >= 20) %>% # filter relatively common words
  pairwise_cor(wordstem, id, sort = T)

priv_word_correlations_singlekids <- privat_lonely_tidy_singlekids %>%
  group_by(wordstem) %>%
  filter(n() >= 5) %>% # less common terms included cause of lower N
  pairwise_cor(wordstem, id, sort = T)

# words correlating with kind among single parents
priv_word_correlations_singlekids %>%
  filter(str_detect(item1, "^kind*"))


priv_word_correlations_couplekids <- privat_lonely_tidy_couplekids %>%
  group_by(wordstem) %>%
  filter(n() >= 20) %>%
  pairwise_cor(wordstem, id, sort = T)

# words correlating with kind among couples with children
priv_word_correlations_couplekids %>%
  filter(str_detect(item1, "^kind*"))


priv_word_correlations_nokids <- privat_lonely_tidy_nokids %>%
  group_by(wordstem) %>%
  filter(n() >= 20) %>%
  pairwise_cor(wordstem, id, sort = T)


# export for translation
write.xlsx(priv_word_correlations_alone,
           file = "analysis/out/zonstiges/correlations_alone.xlsx")
write.xlsx(priv_word_correlations_singlekids,
           file = "analysis/out/zonstiges/correlations_singlekids.xlsx")
write.xlsx(priv_word_correlations_couplekids,
           file = "analysis/out/zonstiges/correlations_couplekids.xlsx")
write.xlsx(priv_word_correlations_nokids,
           file = "analysis/out/zonstiges/correlations_nokids.xlsx")

# import translated ones
priv_word_correlations_alone_EN <-
  read.xlsx("analysis/in/transl/Kopie von correlations_alone_englisch.xlsx", 1)
priv_word_correlations_alone_EN <- priv_word_correlations_alone_EN[2:4]

priv_word_correlations_singlekids_EN <-
  read.xlsx("analysis/in/transl/Kopie von correlations_singlekids_englisch_v2.xlsx", 1)
priv_word_correlations_singlekids_EN <- priv_word_correlations_singlekids_EN[2:4]

priv_word_correlations_couplekids_EN <-
  read.xlsx("analysis/in/transl/Kopie von correlations_couplekids_englisch.xlsx", 1)
priv_word_correlations_couplekids_EN <- priv_word_correlations_couplekids_EN[2:4]

priv_word_correlations_nokids_EN <-
  read.xlsx("analysis/in/transl/Kopie von correlations_nokids_englisch_v2.xlsx", 1)
priv_word_correlations_nokids_EN <- priv_word_correlations_nokids_EN[2:4]


## plot word correlations
# alone
corralone <- priv_word_correlations_alone %>%
  filter(item1 %in% c("angst", "famil")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  xlab("") +
  ylab("") +
  labs(title = "Living alone") +
  coord_flip()

# single prent kids
corrsinglekids <- priv_word_correlations_singlekids %>%
  filter(item1 %in% c("angst", "famil")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  xlab("") +
  ylab("") +
  labs(title = "Single parent") +
  coord_flip()

# couple with kids
corrcouplekids <- priv_word_correlations_couplekids %>%
  filter(item1 %in% c("angst", "famil")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  xlab("") +
  ylab("") +
  labs(title = "Couple with kids") +
  coord_flip()

# without kids
corrnokids <- priv_word_correlations_nokids %>%
  filter(item1 %in% c("angst", "famil")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  xlab("") +
  ylab("") +
  labs(title = "Living together without kids") +
  coord_flip()


graphscomb_corr <- plot_grid(corralone, corrsinglekids,
                             corrcouplekids, corrnokids)

title_corr <- ggdraw() + draw_label("Correlations of selected terms", fontface='bold') # make title

plot_grid(title_corr, graphscomb_corr, ncol=1, rel_heights=c(0.1, 1)) # add title

dev.copy(png,"analysis/out/private_word_correlations_wohntyp.png", width=800)
dev.off()


# plot network
#pdf("analysis/out/private_word_networks_wohntyp_en.pdf")

networkalone <- priv_word_correlations_alone_EN %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Living alone, no kids") +
  panel_border()

networksinglekids <- priv_word_correlations_singlekids_EN %>%
  filter(correlation > .5) %>% # higher correlations
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Single parent") +
  panel_border()

networkcouplekids <- priv_word_correlations_couplekids_EN %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Couple with kids") +
  panel_border()

networknoalonenokkids <- priv_word_correlations_nokids_EN %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Not alone, no kids") +
  panel_border()

graphscomb_network <- plot_grid(networksinglekids,
                                networkcouplekids,
                                networkalone,
                                networknoalonenokkids)

# title_network <- ggdraw() + draw_label("Word correlations", fontface='bold') # make title

# plot_grid(title_network, graphscomb_network, ncol=1, rel_heights=c(0.1, 1)) # add title

dev.copy(png,"analysis/out/private_word_networks_wohntyp_en.png", width=800)
dev.off()


