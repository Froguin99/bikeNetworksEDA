# set cleaning to true or false (true recommended)
cleaning <- TRUE

# libary imports
library(tidyverse)
library(kohonen)
library(ggcorrplot)
library(dplyr)
library(factoextra)
library(ggplot2)
library(plotly)
library(ggdendro)

# Generate behaviours clusters based on two methos:
# Self-organising maps and hierarchical clustering.



### READING IN FILES ### ------------------------------------------------------------------------------
# get working directory and read in file
wd <- getwd()
file <- paste(wd,"Data/Cleaned_Data/combined_places.csv", sep ='/')
comb_streets <- read.csv(file)


### Removing surplus columns ### ----------------------------------------------------------------------

## clean dataframes ##


if (cleaning == TRUE){
  # remove count columns
  comb_streets <- comb_streets %>% select(-ends_with("_count_bike"))
  comb_streets <- comb_streets %>% select(-ends_with("_count_road"))
  
  # remove other optional columns
  comb_streets <- comb_streets %>% select(-ends_with("components_road"))
  comb_streets <- comb_streets %>% select(-contains("edge_length_total"))

  
}

# set place as the row name
#rownames(comb_streets) <- comb_streets$place


# remove spare column
#comb_streets$place <- NULL



# calculate proprtions for disconnected bike components
comb_streets$disconnected_components_bike <- comb_streets$disconnected_components_bike/comb_streets$street_length_total_bike



if (cleaning == TRUE){
  # remove count columns
  comb_streets <- comb_streets %>% select(-ends_with("_count_bike"))
  comb_streets <- comb_streets %>% select(-ends_with("_count_road"))

  
  # remove other optional columns
  
  comb_streets <- comb_streets %>% select(-ends_with("components_road"))
  comb_streets <- comb_streets %>% select(-contains("edge_length_total"))
  comb_streets <- comb_streets %>% select(-contains("way_int"))

  
}

# remove places column
temp_comb_streets <- comb_streets[, -1]

## prepare for future analysis ##

# set as a matrix and rescale
temp_comb_streets = as.matrix(temp_comb_streets)
comb_streets.sc <- scale(temp_comb_streets)


# set NANs to 0s
comb_streets[is.na(comb_streets)] <- 0
comb_streets.sc[is.na(comb_streets.sc)] <- 0

# ensure streets are in a vector
comb_streets.sc <- unlist(comb_streets.sc)


#-----------------------------------------------------------------------------------------------------------------------


# normalise scale
norm_scale <- as.data.frame(comb_streets.sc)

# Generate matrix
m <- as.matrix(norm_scale[-1])


# set row names to place names
comb_streets.sc <- cbind(comb_streets.sc, comb_streets$place)

# Convert the matrix to a dataframe
comb_streets.sc_df <- as.data.frame(comb_streets.sc)

# Get the values of the last column
last_col <- comb_streets.sc[, ncol(comb_streets.sc)]

# Set the row names to the values of the last column
row.names(comb_streets.sc_df) <- last_col

# Remove the last column
comb_streets.sc_df <- comb_streets.sc_df[, -ncol(comb_streets.sc_df)]

# Convert the dataframe back to a matrix
comb_streets.sc <- as.matrix(comb_streets.sc_df)



# Distance matrix
distance <- get_dist(comb_streets.sc, method='manhattan')

# Algorithm
hc <- hclust(distance, method='ward.D')

# Dendogram
plot(hc)
####
p <- ggdendrogram(hc, rotate = FALSE, size = 2)
ggplotly(p)



##
# using dendrogram objects
hcd = as.dendrogram(hc)

# vector of colors
labelColors = c("#FF5416", "#036564", "#EB6841", "#008000", "#DB41D7", "#BD9316", "#13ADB3")

# cut dendrogram in 8 clusters
clusMember = cutree(hc, 8)

# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

# using dendrapply
clusDendro = dendrapply(hcd, colLab)

# make plot
par(mar=c(17, 5, 5, 5), bg= "white")
plot(clusDendro, main = "City Road and Cycle Network Structure Cluster Dendrogram")



##

####
rect.hclust(hc,k=8, border = 2:3)

# Clusters
clusters <- cutree(hc,7)
norm_scale$hc_cluster <- clusters

# Results
results_hc <- norm_scale[-1] %>% group_by(hc_cluster) %>% 
  dplyr::summarize_all(list(mean = mean)) %>% 
  as.data.frame() %>% 
  mutate(hc_cluster = as.factor(hc_cluster))


#-------------------------------------------------------------------------------------------------------------------------


# Self-Organising maps ----------------------------------------------------
n = sqrt(5*sqrt(nrow(norm_scale))) # Grid dimensions
grid <- somgrid(xdim = n, 
                ydim = n, 
                topo = 'hexagonal',
                neighbourhood.fct = 'gaussian',
                toroidal = T)
# Estimate model
set.seed(7)
som_model <- som(X = m,
                 grid = grid,
                 dist.fcts = 'manhattan',
                 rlen = 20000)



# -----------------------------------------------

# join clusters to original dataframe
comb_streets <- cbind(comb_streets, clusters)

# plot clusters


ggplot(comb_streets, aes(x = place, y = clusters)) +
  geom_col()

place_cluster <- comb_streets[, c("place", "clusters")]


# plot 
# make plot
par(mar=c(5, 5, 5, 5), bg= "white")
plot(som_model, type = 'codes',
     codeRendering = "lines", 
     bgcol = "transparent", 
     border = TRUE,
     main = "Metric Signatures SOM")

# to identify, use:
# identify(som_model)

# type = '' can be counts, codes, changes, mapping, quality

# ----------------------------------------------
# Results
som_model_cluster <- cutree(hclust(dist(getCodes(som_model))), 8)

# Add node
norm_scale$node_activity <- som_model$unit.classif

# Join cluster to node
x <- as.data.frame(som_model_cluster) %>%
  rownames_to_column(.,'node_activity') %>%
  mutate(node_activity = gsub('V','',node_activity))
x$node_activity <- as.integer(x$node_activity)
norm_scale <- left_join(norm_scale, x, by = 'node_activity')

# Cluster column as factor
norm_scale$som_model_cluster <- as.factor(norm_scale$som_model_cluster)

results_som <- norm_scale[-1] %>% group_by(som_model_cluster) %>% 
  dplyr::summarize_all(list(mean = mean)) %>% 
  as.data.frame() %>% 
  mutate(som_model_cluster = as.factor(som_model_cluster))




# Name clusters
results_hc <- results_hc %>%
  mutate(cluster_name = case_when(hc_cluster == 1 ~ 'Students',
                                  hc_cluster == 2 ~ 'Working leisurers',
                                  hc_cluster == 3 ~ 'Working carers',
                                  hc_cluster == 4 ~ 'Housekeepers',
                                  hc_cluster == 5 ~ 'Workaholics',
                                  hc_cluster == 6 ~ 'Leisurers',
                                  hc_cluster == 7 ~ 'Active life',
                                  hc_cluster == 8 ~ 'Culture fans'))
results_som <- results_som %>%
  mutate(cluster_name = case_when(som_model_cluster == 1 ~ 'Students',
                                  som_model_cluster == 2 ~ 'Active life',
                                  som_model_cluster == 3 ~ 'Workaholics',
                                  som_model_cluster == 4 ~ 'Working leisurers',
                                  som_model_cluster == 5 ~ 'Culture fans',
                                  som_model_cluster == 6 ~ 'Working housekeepers',
                                  som_model_cluster == 7 ~ 'Leisurers',
                                  som_model_cluster == 8 ~ 'Housekeepers'))


# Translate results into weights ------------------------------------------
weights <- results_som[,c(2:8)]
#weights[]<- lapply(weights,rank,ties.method="min")
weights$cluster_name <- results_som$cluster_name

# Rows to columns ---------------------------------------------------------
weights <- as.data.frame(t(weights))
colnames(weights) <- weights[8,]
weights <- weights[-8,]
weights <- weights[,order(colnames(weights))]


