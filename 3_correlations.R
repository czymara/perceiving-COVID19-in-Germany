
#####
# CO-OCCURANCE ANALYSIS
#####


# packages
packages <- c("tidyverse", "magrittr", "rtweet", "tidytext", "widyr", "quanteda", "influential", "ggraph", "cowplot")
lapply(packages, library, character.only = TRUE)

# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/lehrstuhlstuff/Corona Survey/"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)

# load data
load("data.RData")


# count words co-occuring within users (id is user ID, I think)
word_pairs_de <- DE_tweets_prep %>%
  pairwise_count(word, user_id, sort = TRUE)

word_pairs_de

word_pairs_en <- EN_tweets_prep %>%
  pairwise_count(word, user_id, sort = TRUE)

word_pairs_en



# which words occurs most often with china?
word_pairs_de %>%
  filter(str_detect(item1, "china"))

word_pairs_en %>%
  filter(str_detect(item1, "china"))

## phi coefficient: how much more likely is that either both word X and Y appear, or neither do, than that one appears without the other
# equivalent to Pearson correlation when applied to binary data
word_cors_de <- DE_tweets_prep %>%
  group_by(word) %>%
  filter(n() >= 20) %>% # filter relatively common words
  pairwise_cor(word, user_id, sort = TRUE)

word_cors_de

word_cors_en <- EN_tweets_prep %>%
  group_by(word) %>%
  filter(n() >= 20) %>% # filter relatively common words
  pairwise_cor(word, user_id, sort = TRUE)

word_cors_en


# which words correlates most often with antisemitisch
word_cors_de %>%
  filter(str_detect(item1, "china"))

word_cors_en %>%
  filter(str_detect(item1, "china"))


# plot
word_cors %>%
  filter(item1 %in% c("china", "afd", "rassismus") |
           str_detect(item1, "^anti[-]{0,1}semi") |
           str_detect(item1, "jude") |
           str_detect(item1, "muslim") |
           str_detect(item1, "nazi")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# plot as network
set.seed(2016)
network_de <- word_cors_de %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# + labs(title = "German tweets")

set.seed(2016)
network_en <- word_cors_en %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# +  labs(title = "English tweets")

corrgraphscomb <- plot_grid(network_de, network_en,
               labels=c("German tweets", "English tweets")
               )

title <- ggdraw() + draw_label("Word correlations > 0.6 on COVIT-19 tweets", fontface='bold') # make title

plot_grid(title, corrgraphscomb, ncol=1, rel_heights=c(0.1, 1)) # add title


