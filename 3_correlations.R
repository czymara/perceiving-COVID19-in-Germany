
#####
# CO-OCCURANCE ANALYSIS
#####


# packages
packages <- c("tidyverse", "magrittr", "rtweet", "tidytext", "widyr", "quanteda", "influential", "ggraph", "cowplot", "SnowballC")
lapply(packages, library, character.only = TRUE)

# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/lehrstuhlstuff/Corona Survey/"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)

# load data
load("coRona2/in/data.RData")

# convert DFM to tidytext
privat_lonely_tidy <- tidy(DFM_priv) # common terms are already removed here


### create tidy text
data_priv <- data[ which(data$OF01_01 != ""), ]

answers_priv <- tibble(id = data_priv$CASE,
                       private = data_priv$lialone,
                       text = as.character(data_priv$OF01_01))

privat_lonely_tidy <- answers_priv %>% unnest_tokens(word, text)

# stop words
stopWords_de <- read.table("coRona2/in/stopwords-de.txt", encoding = "UTF-8", colClasses=c('character'))

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
  filter(str_detect(item1, "famil*")) # not mentioned enough


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

dev.copy(png,"coRona2/out/private_word_correlations.png")
dev.off()


# plot as network
set.seed(1337)
priv_word_correlations %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "Word correlations > 0.25")

dev.copy(png,"coRona2/out/private_word_networks.png")
dev.off()


## living alone vs. not
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

# plot
set.seed(1337)
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

set.seed(1337)
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


corrgraphscomb <- plot_grid(networkalone, networknotalone)

title <- ggdraw() + draw_label("Word correlations > 0.25", fontface='bold') # make title

plot_grid(title, corrgraphscomb, ncol=1, rel_heights=c(0.1, 1)) # add title

dev.copy(png,"coRona2/out/private_word_networks_livingaloneornot.png")
dev.off()


