#load packages
library("here")
library("folderfun")

#Set up file handling variables and methods
here::i_am("README.md")

#set input/output paths for different use cases
# inputPath="/fh/scratch/delete90/huang_y/bwillia2/sl_screening/"
# outputPath="/home/aking2/sl_screening/sim_output"
inputPath <- here::here("sim_output", "compiled_results")
outputPath <- here::here("sim_output")

#File paths for local testing
#inputPath="C:/Users/andre/github/brians_repository/sl_screening/sim_output"
#outputPath="C:/Users/andre/github/brians_repository/sl_screening/sim_output"
setff("In", inputPath)
setff("Out", outputPath)
source(file=here::here("sim_code/read_data.R"))

#Put simulation data together and read it in
path_to_csv <- write_data(inputPath)
