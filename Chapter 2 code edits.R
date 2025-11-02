#This is an edited version of the Chapter 2 main code
#I have taken out a lot of the commented sections from that code for ease of reading and editing


#Read in all necessary libraries
library(sf)
library(readr)

#Read in migratory bird and human adjancncy matrices
bird_adj = as.matrix(read.csv("county_adjacency_matrix_blue_winged_teal_county.csv")[,-1])
human_adj = as.matrix(read.csv("county_adjacency_matrix_mourning_dove_county.csv")[,-1])


#CSV files for each state is being created
#Need to work on combining all CSV files in to one data frame
poultry <- read.csv("~/Desktop/Census Data/Poultry State CSV/Alabama.csv")

#All location association should be done while combining poultry county data


####Preparing for disease simulation####

#these two sections need to be reevaluated
# Disease states: 0 = susceptible, 1 = infected
bird_state <- rep(0, 10)
poultry_state <- rep(0, 10)
human_state <- rep(0, 10)

# Introduce disease in migratory birds
bird_state[sample(1:length(bird_adj), 2)] <- 1

# Parameters
beta_bird_poultry <- 0.3   # transmission from bird to poultry
beta_poultry_human <- 0.2  # transmission from poultry to humans
beta_bird_bird <- 0.1      # within-bird spread
beta_human_human <- 0.05   # within-human spread
beta_poultry_poulty <- 0.3  #within-poultry spread


#Transmission model 
# Initialize edge lists for tracking transmission
bird_trans_edges <- list()
human_trans_edges <- list()
cross_trans_edges <- list()
