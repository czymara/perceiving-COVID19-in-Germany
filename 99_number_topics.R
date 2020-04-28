
packages <- c("ldatuning")
lapply(packages, library, character.only = TRUE)

# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/Corona Survey (Alexander Langenkamp)/Topic model project/"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)

SEED <- 1337

load("analysis/in/data.RData")


topicnrs <- FindTopicsNumber(
  DFM_priv,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(iter=500, seed = SEED, verbose=25),
  mc.cores = 2L,
  verbose = F
  )

# plot
topicnrplot <- FindTopicsNumber_plot(topicnrs)
topicnrplot

dev.copy(png,"analysis/out/ntopics_20.png")
dev.off()

