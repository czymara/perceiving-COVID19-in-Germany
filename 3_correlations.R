
#####
# CO-OCCURANCE ANALYSIS
#####


# packages
packages <- c("tidyverse", "magrittr", "rtweet",
              "tidytext", "widyr", "quanteda",
              "influential", "ggraph", "cowplot", "SnowballC")
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


# load data
load("analysis/in/data.RData")

# convert DFM to tidytext
# privat_lonely_tidy <- tidy(DFM_priv) # common terms are already removed here


### create tidy text
data_priv <- data[ which(data$OF01_01 != ""), ]

answers_priv <- tibble(id = data_priv$CASE,
                       private = data_priv$lialone,
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

# occuring with solidaritaet
word_pairs_private$item1 <- gsub("^soli.*", "soli", word_pairs_private$item1) # stem all solidarity related terms to "soli"

word_pairs_private %>%
  filter(str_detect(item1, "^soli*"))


## phi coefficient: how much more likely is that either word X and Y appear, or neither do, than that one appears without the other
privat_lonely_tidy$wordstem <- gsub("^soli.*", "soli", privat_lonely_tidy$wordstem)

privat_lonely_tidy$wordstem <- gsub("^famil.*", "famil", privat_lonely_tidy$wordstem)


priv_word_correlations <- privat_lonely_tidy %>%
  group_by(wordstem) %>%
  filter(n() >= 20) %>% # filter relatively common words
  pairwise_cor(wordstem, id, sort = T)

priv_word_correlations

# correlating with solidaritaet
priv_word_correlations %>%
  filter(str_detect(item1, "soli*")) # not mentioned enough


# plot
priv_word_correlations %>%
  filter(item1 %in% c("angst", "soli", "famil")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

dev.copy(png,"analysis/out/private_word_correlations.png")
dev.off()


# plot as network
set.seed(SEED)
priv_word_correlations %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Word correlations > 0.25")

dev.copy(png,"analysis/out/private_word_networks.png")
dev.off()


###
# living alone vs. not
###

privat_lonely_tidy_alone <- privat_lonely_tidy[ which(privat_lonely_tidy$private == "living alone"), ]
privat_lonely_tidy_notalone <- privat_lonely_tidy[ which(privat_lonely_tidy$private == "not living alone"), ]

priv_word_correlations_alone <- privat_lonely_tidy_alone %>%
  group_by(wordstem) %>%
  filter(n() >= 20) %>% # filter relatively common words
  pairwise_cor(wordstem, id, sort = T)

priv_word_correlations_notalone <- privat_lonely_tidy_notalone %>%
  group_by(wordstem) %>%
  filter(n() >= 20) %>% # filter relatively common words
  pairwise_cor(wordstem, id, sort = T)


## plot word correlations
# alone
set.seed(SEED)
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
  labs(title = "Living alone") +
  coord_flip()

# not alone
set.seed(SEED)
corrnotalone <- priv_word_correlations_notalone %>%
  filter(item1 %in% c("angst", "famil")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  xlab("") +
  labs(title = "Not living alone") +
  coord_flip()

graphscomb_corr <- plot_grid(corralone, corrnotalone)

title_corr <- ggdraw() + draw_label("Correlations of selected terms", fontface='bold') # make title

plot_grid(title_corr, graphscomb_corr, ncol=1, rel_heights=c(0.1, 1)) # add title

dev.copy(png,"analysis/out/private_word_correlations_livingaloneornot.png", width=800)
dev.off()


# plot network
set.seed(SEED)
networkalone <- priv_word_correlations_alone %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Living alone") +
  panel_border()

set.seed(SEED)
networknotalone <- priv_word_correlations_notalone %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Not living alone") +
  panel_border()


graphscomb_network <- plot_grid(networkalone, networknotalone)

title_network <- ggdraw() + draw_label("Word correlations (>0.25)", fontface='bold') # make title

plot_grid(title_network, graphscomb_network, ncol=1, rel_heights=c(0.1, 1)) # add title

dev.copy(png,"analysis/out/private_word_networks_livingaloneornot.png")
dev.off()


