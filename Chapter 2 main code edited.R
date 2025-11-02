#This is one of the main R scripts I have been working with 
#Read in all necessary libraries
library(data.table)
library(sf)
library(ggplot2)
library(igraph)

#Read in migratory bird and human adjancency matrices
bird_adj = as.matrix(read.csv("adj2/county_adjacency_matrix_blue_winged_teal_county_y.csv")[,-1])
human_adj = as.matrix(read.csv("adj2/county_adjacency_matrix_mourning_dove_county_n.csv")[,-1])

#Read in poultry
poultry = read.csv("NABBP_2023_grp_06.csv")[,-1]

#Read in the spatial layer
counties = read_sf(dsn = "gadm36_levels_shp", layer = "gadm36_2")
counties = subset(counties,is.element(NAME_0,c("Canada","United States","Mexico")))


#Associate poultry data with locations
#coords from banding data, change to location in poultry data
poultry = subset(poultry, !is.na(LON_DD) & !is.na(LAT_DD))
poultry_sf = st_as_sf(poultry, coords = c("LON_DD", "LAT_DD"))
st_crs(poultry_sf) = st_crs(counties)

#Pair poultry data with spatial units
require(data.table)
poultry_sf$ID = 1:nrow(poultry_sf)
counties_visitied = st_join(poultry_sf, counties, join = st_intersects)
counties_visitied_sub = st_drop_geometry(counties_visitied[,c("ID", "GID_2")])

setDT(counties_visitied);setDT(poultry_sf)


#merge county IDs with original dataset 
poultry_with_counties = counties_visitied[poultry_sf, mult = "first", on = "BAND", nomatch = 0L]

####Preparing for disease simulation#####

# Disease states: 0 = susceptible, 1 = infected
bird_state <- rep(0,nrow(counties))
poultry_state <- rep(0, nrow(counties))
human_state <- rep(0, nrow(counties))

# Introduce disease in migratory birds
bird_state[sample(1:length(bird_state),  3)] <- 1
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

# spillover_probability <- function(interaction_count) {
#   1 - exp(-0.3 * interaction_count)
# }

# Initialize vector to store initial infection location for each bird and poultry
#Not accurate for current code
bird_origin <- rep(NA, nrow(counties))
poultry_origin <-rep(NA, nrow(counties))

#Currently does not take in to account poultry to poultry transmission
for (t in 1:timesteps) {
  # Spread among birds
  bird_infections <- which(bird_state == 0 & (bird_adj %*% (bird_state == 1)) > 0)
  for (i in bird_infections) {
    if (rbinom(1, 1, beta_bird_bird)) {
      bird_state[i] <- 1
      # Record which infected neighbor caused it
      infectors <- which(bird_adj[i, ] == 1 & bird_state == 1)
      if (length(infectors) > 0) {
        bird_trans_edges[[length(bird_trans_edges) + 1]] <- c(sample(infectors, 1), i)
      }
    }
  }
  
  # Bird → Poultry
  for (i in 1:10) {
    if (bird_state[i] == 1 && poultry_state[i] == 0) {
      if (rbinom(1, 1, beta_bird_poultry)) {
        poultry_state[i] <- 1
        cross_trans_edges[[length(cross_trans_edges) + 1]] <- c(paste0("Bird", i), paste0("Poultry", i))
      }
    }
  }
  
  # #poultry to poultry
  # poultry_infections <- which(poultry_state == 0 & (poultry_adj %*% (poultry_state == 1)) > 0)
  # for (i in poultry_infections) {
  #   if (rbinom(1, 1, beta_poultry_poultry)) {
  #     poultry_state[i] <- 1
  #     # Record which infected neighbor caused it
  #     infectors <- which(poultry_adj[i, ] == 1 & poultry_state == 1)
  #     if (length(infectors) > 0) {
  #       poultry_trans_edges[[length(poultry_trans_edges) + 1]] <- c(sample(infectors, 1), i)
  #     }
  #   }
  # }
  
  # Poultry → Human
  for (i in 1:10) {
    if (poultry_state[i] == 1 && human_state[i] == 0) {
      if (rbinom(1, 1, beta_poultry_human)) {
        human_state[i] <- 1
        cross_trans_edges[[length(cross_trans_edges) + 1]] <- c(paste0("Poultry", i), paste0("Human", i))
      }
    }
  }
  
  # During transmission, record the origin
  for (i in 1:10) {
    if (bird_state[i] && poultry_state[i] == 1 && human_state[i] == 0 && rbinom(1, 1, beta_poultry_human)) {
      human_state[i] <- 1
      # Find the source poultry's origin
      source_origin <- bird_origin[i]
      # Store or record the transmission along with source origin
      cross_trans_edges[[length(cross_trans_edges) + 1]] <- list(
        from = paste0("Poultry", i),
        to = paste0("Human", i),
        source_origin = source_origin
      )
    }
  }
  
  # Human → Human
  # human_infections <- which(human_state == 0 & (human_adj %*% (human_state == 1)) > 0)
  # for (i in human_infections) {
  #   if (rbinom(1, 1, beta_human_human)) {
  #     human_state[i] <- 1
  #     infectors <- which(human_adj[i, ] == 1 & human_state == 1)
  #     if (length(infectors) > 0) {
  #       human_trans_edges[[length(human_trans_edges) + 1]] <- c(sample(infectors, 1), i)
  #     }
  #   }
  # }
}






####Creating community structure and network anaylsis####
###Make sure all data file names are updated##

# Combine all transmission edges
all_edges <- do.call(rbind, c(bird_trans_edges, human_trans_edges, cross_trans_edges))
g <- graph_from_edgelist(as.matrix(all_edges), directed = TRUE)

# Plot the graph
plot(g, vertex.label.cex=0.8, edge.arrow.size=0.4, layout=layout_with_fr)


#detect communities
community_graph = walktrap.community(g)
#add community membership to shapefile
members = community_graph$membership
members[is.element(members,which(table(community_graph$membership)<10))] = NA
counties$membership = members
counties_communities_only = subset(counties,!is.element(membership,communities_remove))
#plot shapefile
commplot = ggplot()   + 
  geom_sf(colour="NA",data=counties,size=.5,fill="light grey")+
  geom_sf(colour="NA",data=counties_communities_only,size=.5, mapping = aes(fill = as.factor(membership))) +
  ggtitle(paste0("Commplot; total connections: ", sum(county_adjacency_matrix), " Species: ","Transmission trial"))+
  xlim(-170,-63) + ylim(22,72)

ggsave(commplot, filename=paste0("commplot","Transmission trial",".png"))

#eigenvector centrality 
counties$evec = eigen_centrality(graph)$vector

evec_plot = ggplot()   +
  geom_sf(colour="NA",data=counties,size=.5, mapping = aes(fill = evec)) +
  scale_fill_distiller(palette="YlOrRd",trans="log10")+
  ggtitle(paste0("Eigenvector centrality; total connections: ", sum(county_adjacency_matrix), " Species: ","Transmission trial"))+
  xlim(-170,-63) + ylim(22,72)
ggsave(evec_plot, filename=paste0("evecplot",'Transmission trial',".png"))

#betweenness centrality 
counties$betweenness = betweenness(graph)

betweenness_plot = ggplot()   +
  geom_sf(colour="NA",data=counties,size=.5, mapping = aes(fill = betweenness)) +
  scale_fill_distiller(palette="Spectral",trans="log2")+
  ggtitle(paste0("Betweenness centrality; Total connections: ", sum(county_adjacency_matrix), " Species: ","Transmission trial"))+
  xlim(-170,-63) + ylim(22,72)

ggsave(betweenness_plot, filename=paste0("betweennessplot","Transmission trial",".png"))

