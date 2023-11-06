#load packages
library("tidyverse")
library("here")
library("folderfun")

####write tibble into a csv file####
write_data <- function(folderName){
  data_tib<-read_data(folderName)
  #I changed the filename to avoid overwriting my sim_data.csv.
  write_csv(x= data_tib, ffOut("sim_out.csv"))
  #outputPath<-concat(ffOut,"/sim_output/sim_data.csv")
  outputPath <- ffOut("sim_data.csv")
  return(outputPath)
}

####construct a tibble of all sim data####
read_data <- function(folderName){
  pathList <- get_sims(folderName)
  buildtib <- NULL
  pathlength<-1:length(pathList)
  for(i in pathlength){
    print(paste0(i,"  ",ffIn(pathList[i])))
    sim_tib_data <- read_one(ffIn(pathList[i]))
    buildtib <- bind_rows(buildtib, sim_tib_data)
  }
  return(buildtib)
}


####load in sim data from a file, called by read_data####
#@pathName accepts the file path as a parameter and loads in the file.
read_one <- function(pathname){
  weak <- grepl("weak", pathname)
  sim_file <- tryCatch(readRDS(pathname), error = function(e){
    tibble::tibble(mc_id = NA, n = NA, p = NA, fam = NA, link = NA, x_dist = NA,
                   corr = NA, linear = NA, weak = NA, est_type = NA, truth = NA, point_est = NA, 
                   se = NA, cil = NA, ciu = NA)
  })
  # sim_data <- tibble::as_tibble(sim_file) %>% mutate(weak = weak, .before = "est_type")
  sim_data <- tibble::as_tibble(sim_file)
  return(sim_data)
}

####crawl directory for many simulation output files, called by read_data####
#goes through the working directory and returns file path names
get_sims <- function(folderName){
  pathList <- list.files(path=folderName, recursive=TRUE, 
                include.dirs = TRUE, pattern="rds")
  return(pathList)
}