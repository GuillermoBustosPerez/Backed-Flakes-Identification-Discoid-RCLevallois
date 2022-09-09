test <- Morpho::read.csv.folder(
  folder = "Data/GM csvs",
  x = 1:508,
  y = 1:3,
  pattern = ".csv",
  dec = ",",
  header = TRUE)

filenames <- list.files(path = "Data/GM csvs", pattern = ".csv") 
filenames <- gsub('.{4}$', '', filenames)

#### Set array with names and ####
Flakes_LM <- test$arr
dimnames(Flakes_LM)[[3]] <- filenames

#### Check landmarks #####
Morpho::checkLM(
  Flakes_LM, 
  path = "Meshes/", 
  Rdata = FALSE,
  render = "s",
  suffix = ".ply")