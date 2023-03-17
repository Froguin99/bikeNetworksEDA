# this script aims to implment the data aquired in scripts 00 and 01
# into self-organising maps in order to classify cities into groups
# it also highlights correlations between cities cycleablity and its street network indicators

# install package
#install.packages('kohonen')
#install.packages('tidyverse')
#install.packages('ggcorrplot')
install.packages('caret')
#install.packages('factoextra')

### Running the file ### ------------------------------------------------------------------------------

install.packages("caret")
library(caret)

# libary imports
#library(dplyr)
library(ggplot2)
library(tidyverse)
library(kohonen)
library(ggcorrplot)
library(caret)
library(factoextra)







### READING IN FILES ### -----------------------------------------------------------------------------
# get working directory and read in file
wd <- getwd()
file <- paste(wd,"Data/Cleaned_Data/euro_ranking.csv", sep ='/')
streets <- read.csv(file)



### CLEANING ### -------------------------------------------------------------------------------------
# set place as the row name
rownames(streets) <- streets$place

# remove spare column
streets$place <- NULL

# calculate proprtions for disconnected bike components
streets$disconnected_components_bike <- streets$disconnected_components_bike/streets$street_length_total_bike


### PEARSON CORRELATION COEFFICIENT CREATOR ### ------------------------------------------------------

# Create an empty dataframe to store the correlation coefficients with the correct number of rows
cor_df <- data.frame(matrix(ncol = ncol(streets), nrow = 1))

# Add the column names to the correlation dataframe
colnames(cor_df) <- colnames(streets)

# Loop through each column in the streets dataframe
for (col in colnames(streets)) {
  # Calculate the correlation coefficient between the active_mobility_ranking column and the current column
  correlation_coefficient <- cor(streets$active_mobility_ranking, streets[[col]])
  
  # Add the correlation coefficient to the cor_df dataframe
  cor_df[1, col] <- correlation_coefficient
}
# drop any columns with NA
cor_df <- cor_df[, colSums(is.na(cor_df)) != nrow(cor_df)]

# remove ranking column
cor_df <- subset(cor_df, select = -c(active_mobility_ranking))

# Filter out any columns from cor_df with a smaller absolute value than 0.5
filtered_df <- subset(cor_df, select = -c(which(colSums(abs(cor_df) < 0.4) == nrow(cor_df))))




# create a bar plot of correlations
barplot(as.matrix(cor_df),
        las=2, cex.names=.5,
        main = 'Pearson Correlation Coefficient of Indicators')

barplot(as.matrix(filtered_df),
        las=2, cex.names=.5,
        main = 'Pearson Correlation Coefficient of Indicators')




### MISC PLOTTING ### -------------------------------------------------------------------------------


# Create an index column and remove
streets <- streets %>% rownames_to_column(var = "index_column")
streets <- streets %>% mutate(place_names = index_column)
streets <- streets %>% select(-index_column)

ggplot(streets, aes(x = active_mobility_ranking, y = disconnected_components_bike)) + 
  geom_point(aes(colour = place_names), size = 4)






















### SOMs ### ------------------------------------------------------------------------------------------




# set cleaning to true or false (true recommended)
cleaning <- TRUE

# libary imports
library(tidyverse)
library(kohonen)
library(ggcorrplot)
library(dplyr)


# Generate behaviours clusters based on two methos:
# Self-organising maps and hierarchical clustering.



### READING IN FILES ### ------------------------------------------------------------------------------
# get working directory and read in file
wd <- getwd()
file <- paste(wd,"Data/Cleaned_Data/combined_places.csv", sep ='/')
comb_streets <- read.csv(file)

file <- paste(wd,"Data/Cleaned_Data/bike_places.csv", sep ='/')
bike_streets <- read.csv(file)

file <- paste(wd,"Data/Cleaned_Data/road_places.csv", sep ='/')
road_streets <- read.csv(file)


### Removing surplus columns ### ----------------------------------------------------------------------

## clean dataframes ##

# set place as the row name
rownames(comb_streets) <- comb_streets$place
rownames(bike_streets) <- bike_streets$place
rownames(road_streets) <- road_streets$place

# remove spare column
comb_streets$place <- NULL
bike_streets$place <- NULL
road_streets$place <- NULL

# calculate proprtions for disconnected bike components
comb_streets$disconnected_components_bike <- comb_streets$disconnected_components_bike/comb_streets$street_length_total_bike
bike_streets$disconnected_components_bike <- bike_streets$disconnected_components_bike/bike_streets$street_length_total_bike
road_streets$disconnected_components_bike <- road_streets$disconnected_components_bike/road_streets$street_length_total_bike



if (cleaning == TRUE){
  # remove count columns
  comb_streets <- comb_streets %>% select(-ends_with("_count_bike"))
  comb_streets <- comb_streets %>% select(-ends_with("_count_road"))
  
  bike_streets <- bike_streets %>% select(-ends_with("_count_bike"))
  bike_streets <- bike_streets %>% select(-ends_with("_count_road"))
  
  road_streets <- road_streets %>% select(-ends_with("_count_bike"))
  road_streets <- road_streets %>% select(-ends_with("_count_road"))
  
  # remove other optional columns
  
  comb_streets <- comb_streets %>% select(-ends_with("components_road"))
  bike_streets <- bike_streets %>% select(-ends_with("components_road"))
  road_streets <- road_streets %>% select(-ends_with("components_road"))
  
  comb_streets <- comb_streets %>% select(-contains("edge_length_total"))
  bike_streets <- bike_streets %>% select(-contains("edge_length_total"))
  road_streets <- road_streets %>% select(-contains("edge_length_total"))

}

## prepare for future analysis ##

# set as a matrix and rescale
comb_streets = as.matrix(comb_streets)
comb_streets.sc <- scale(comb_streets)

bike_streets = as.matrix(bike_streets)
bike_streets.sc <- scale(bike_streets)

road_streets = as.matrix(road_streets)
road_streets.sc <- scale(road_streets)

# set NANs to 0s
comb_streets[is.na(comb_streets)] <- 0
comb_streets.sc[is.na(comb_streets.sc)] <- 0

bike_streets[is.na(bike_streets)] <- 0
bike_streets.sc[is.na(bike_streets.sc)] <- 0

road_streets[is.na(road_streets)] <- 0
road_streets.sc[is.na(road_streets.sc)] <- 0


### find correlations ### ------------------------------------------------------------------------


correlation_func <- function(dataframe, dataframe.sc){
  # set a seed for repoducablity
  set.seed(7)

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
correlation_func(comb_streets, comb_streets.sc)

correlation_func(bike_streets, bike_streets.sc)

correlation_func(road_streets, road_streets.sc)





### create and train SOM ### -----------------------------------------------------------------------------


# Hierarchical clustering -------------------------------------------------



#?som to get help guide

som_function <- function(dataframe, dataframe.sc){
  # set a seed for repoducablity
  set.seed(7)
  
  process <- preProcess(dataframe, method=c('range'))
  norm_scale <- predict(process, dataframe)
  norm_scale$id_pd <- behaviours$id_pd
  norm_scale <- norm_scale %>% dplyr::select(id_pd, everything())
  
  # Generate matrix
  m <- as.matrix(norm_scale[-1])
  
  # Distance matrix
  distance <- get_dist(m, method='manhattan')
  
  # Algorithm
  hc <- hclust(distance, method='ward.D')
  
  # Dendogram
  plot(hc, labels = FALSE)
  rect.hclust(hc,k=8,border = 2:5)
  
  # Clusters
  clusters <- cutree(hc,8)
  norm_scale$hc_cluster <- clusters
  
  # Results
  results_hc <- norm_scale[-1] %>% group_by(hc_cluster) %>% 
    dplyr::summarize_all(list(mean = mean)) %>% 
    as.data.frame() %>% 
    mutate(hc_cluster = as.factor(hc_cluster))
  
  # define a grid for the SOM and train
  dataframe.som <- som(dataframe.sc, grid = somgrid(9,6, topo = "hexagonal"), rlen = 20000 )
  plot(dataframe.som, type = 'codes', codeRendering = "segments",  bgcol = "transparent", border = TRUE)
 
  
  # plot training progress
  plot(dataframe.som, type = "changes")
  
}

som_function(comb_streets, comb_streets.sc)

som_function(road_streets, road_streets.sc)

som_function(bike_streets, bike_streets.sc)

## define a grid for the SOM and train

# codeRendering = line, star etc

### testing ###

## creating cluster groups

#bike_streets.som <- som(bike_streets.sc, grid = somgrid(9,6, topo = 'hexagonal'))
#plot(bike_streets.som, type = 'codes', codeRendering = "segments",  bgcol = "transparent", border = TRUE)
#bike_streets.som.hc <- cutree(hclust(dist(bike_streets.som$codes)) ,h = 5)
#dataframe.som.hc <- cutree(hclust(dist(dataframe.som)))
#add.cluster.boundaries(dataframe.som, dataframe.som.hc, lwd = 5)

