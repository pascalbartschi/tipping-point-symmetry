## script to run all experiments and set up directoy structure

# create dirs
dirs = c("data", 
         "data/asymmetric-sim/", 
         "data/bush-sim/", 
         "data/symmetric-sim/", 
         "data/asymmetric-sim/stressor-asym/", 
         "data/asymmetric-sim/trait-asym/")

for (dir in dirs){
  if (!dir.exists(dir)){
    dir.create(dir)
  }
}

# run all:
file_list <- list.files(path = "experiments/", pattern = "*run_*")
file_list <- file_list[-which(file_list == "run-all.R")] # pop this script itself

for (file in file_list){
  source(file)
}