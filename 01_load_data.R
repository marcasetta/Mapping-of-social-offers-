library(dplyr)

if (!file.exists("data/")) {
  dir.create("data2/")
}
if(!file.exists("data/Data_for_Map.csv") | !file.exists("data/Data_for_list.csv") | !file.exists("data/Data_for_algo.csv")){
  source("00_data_preparation.R")
}

data_original = read.csv("data/Social Counselling in Munich - main table.csv")
data_map = read.csv2("data/Data_for_Map.csv", check.names=FALSE)
data_list = read.csv2("data/Data_for_list.csv", check.names=FALSE)
data_wide = read.csv2("data/Data_for_algo.csv", check.names=FALSE)


