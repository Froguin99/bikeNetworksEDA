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
streets <- read.csv(file)

# set place names as row names
rownames(streets) <- streets$place
streets$place <- NULL # remove original data frame

# set as matrix
streets = as.matrix(streets)


# rescale?
streets.sc <- scale(streets) 

# change NANs to 0
streets[is.na(streets)] <- 0
streets.sc[is.na(streets.sc)] <- 0

set.seed(7) # for repoducablity


### create correlation plot ###

ggcorrplot(cor(streets), 
           method = c("square"), 
           tl.cex = 5, # text size
           tl.srt = 60, # text rotation
           title = "Combined street network metric correlation plot") 





#?som to get help guide

#### train the SOM ####

## define a grid for the SOM and train

streets.som <- som(streets.sc, grid = somgrid(6, 4, topo =  "hexagonal"))
plot(streets.som, main = "Streets data", type = "counts")
plot(streets.som, main = "Streets data", type = "property", property= getCodes(streets.som)[, 6])

# plot all
plot(streets.som, type = 'codes', codeRendering="segments", bgcol = "transparent", border = TRUE) 

# codeRendering = line, star etc

