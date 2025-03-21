# download the VRC01 CATNAP data from GitHub

# for trajectory traversing
library("rprojroot") 
this_path <- normalizePath(".", mustWork = FALSE)
proj_root <- rprojroot::find_root_file(criterion = ".projectile", path = this_path)

# required package for downloading the data
library("RCurl")

# read in the data from Magaret et al. (2019)
dataset <- read.csv(text = getURL("https://raw.githubusercontent.com/benkeser/vrc01/master/data/fulldata.csv"), header = TRUE)

# create new outcomes
dataset$ic50.censored <- as.numeric(dataset$ic50.censored)
dataset$sens50 <- as.numeric(dataset$ic50.geometric.mean.imputed < 1)
dataset$sens80 <- as.numeric(dataset$ic80.geometric.mean.imputed < 1)

saveRDS(dataset, file = paste0(proj_root, "/data/vrc01_data.rds"))
