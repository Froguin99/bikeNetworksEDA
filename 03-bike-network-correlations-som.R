# this script aims to implment the data aquired in scripts 00 and 01
# into self-organising maps in order to classify cities into groups

# install package
#install.packages('kohonen')
#install.packages('tidyverse')
#install.packages('ggcorrplot')

# libary imports
library(tidyverse)
library(kohonen)
library(ggcorrplot)

# get working directory and read in file
wd <- getwd()
file <- paste(wd,"Data/Cleaned_Data/combined_places.csv", sep ='/')
comb_streets <- read.csv(file)

file <- paste(wd,"Data/Cleaned_Data/bike_places.csv", sep ='/')
bike_streets <- read.csv(file)

file <- paste(wd,"Data/Cleaned_Data/road_places.csv", sep ='/')
road_streets <- read.csv(file)


### find correlations ###


correlation_func <- function(dataframe){
  # set a seed for repoducablity
  set.seed(7)
  
  # set place names as row names
  rownames(dataframe) <- dataframe$place
  dataframe$place <- NULL # remove orignal data frame
  
  #set as a matrix and rescale
  dataframe = as.matrix(dataframe)
  dataframe.sc <- scale(dataframe)
  
  # set NANs to 0s
  dataframe[is.na(dataframe)] <- 0
  dataframe.sc[is.na(dataframe.sc)] <- 0
  
  # create correlation plot
  ggcorrplot(cor(dataframe), 
             method = c("square"), 
             tl.cex = 5, # text size
             tl.srt = 60, # text rotation
             title = "Metric correlation plot")
  
  # create correlation plot
  ggcorrplot(cor(dataframe.sc), 
             method = c("square"), 
             tl.cex = 5, # text size
             tl.srt = 60, # text rotation
             title = "Rescaled Metric correlation plot")
}


# call function
correlation_func(comb_streets)

correlation_func(bike_streets)

correlation_func(road_streets)





### create and train SOM ###


#?som to get help guide

som_function <- function(dataframe){
  # set a seed for repoducablity
  set.seed(7)
  
  # set place names as row names
  rownames(dataframe) <- dataframe$place
  dataframe$place <- NULL # remove orignal data frame
  
  #set as a matrix and rescale
  dataframe = as.matrix(dataframe)
  dataframe.sc <- scale(dataframe)
  
  # set NANs to 0s
  dataframe[is.na(dataframe)] <- 0
  dataframe.sc[is.na(dataframe.sc)] <- 0
  
  # define a grid for the SOM and train
  dataframe.som <- som(dataframe.sc, grid = somgrid(6,4, topo = "hexagonal"))
  plot(dataframe.som, type = 'codes', codeRendering = "segments",  bgcol = "transparent", border = TRUE)
  
  # plot training progress
  plot(dataframe.som, type = "changes")
  
}

som_function(comb_streets)

som_function(road_streets)

som_function(bike_streets)

## define a grid for the SOM and train

# codeRendering = line, star etc

