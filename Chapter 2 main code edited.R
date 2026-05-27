#This is one of the main R scripts I have been working with 
#Read in all necessary libraries
library(data.table)
library(sf)
library(ggplot2)
library(igraph)
library(raster)
library(exactextractr)

#Read in migratory bird and human adjancency matrices
bird_adj = as.matrix(read.csv("adj2/county_adjacency_matrix_blue_winged_teal_county_y.csv")[,-1])
human_adj = read.csv("~/Downloads/weekly_county2county_2019_01_28.csv")


human_points = unique(human_adj[c("geoid_o", "lng_o", "lat_o")])



human_pop = raster("humanpop/NA_PopulationDensity_2020.tif")

#Get wgs4 projection
wgs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Assign wgs84 to raster with lambert projection
human_pop=projectRaster(human_pop, crs=wgs)
#Read in the spatial layer
counties = read_sf(dsn = "gadm36_levels_shp", layer = "gadm36_2")
counties = subset(counties,is.element(NAME_0,c("Canada","United States","Mexico")))
counties$human_pop <- exact_extract(human_pop, counties, fun = "sum")
counties$newID = 1:nrow(counties)

human_points_sf <- st_as_sf(human_points, coords = c("lng_o", "lat_o"), crs = 4326, remove = FALSE)
good_points <- st_join(human_points_sf,counties,join=st_within)
new_ids_usa = data.frame(good_points$geoid_o, good_points$newID)
names(new_ids_usa) = c("geoid","newID")
counties_usa = merge(counties, new_ids_usa)

library(data.table)
library(Matrix)

## --- Helpers ---
normalize_geoid <- function(x) {
  # Safe normalization in case one table has 01001 and the other has 1001, etc.
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  
  # If purely numeric (or numeric-as-character), pad to 5 digits (county FIPS style)
  ok <- !is.na(x) & grepl("^[0-9]+$", x)
  if (any(ok)) x[ok] <- sprintf("%05d", as.integer(x[ok]))
  x
}

make_adj_sparse <- function(dt_pairs, ids = NULL, weight_col = "visitor_flows") {
  stopifnot(all(c("newID_o", "newID_d", weight_col) %in% names(dt_pairs)))
  
  if (is.null(ids)) {
    ids <- sort(unique(c(dt_pairs$newID_o, dt_pairs$newID_d)))
  } else {
    ids <- sort(unique(ids))
  }
  
  idx <- setNames(seq_along(ids), as.character(ids))
  i <- idx[as.character(dt_pairs$newID_o)]
  j <- idx[as.character(dt_pairs$newID_d)]
  x <- dt_pairs[[weight_col]]
  
  sparseMatrix(
    i = i, j = j, x = x,
    dims = c(length(ids), length(ids)),
    dimnames = list(as.character(ids), as.character(ids))
  )
}

## --- Convert to data.table + normalize join keys ---
setDT(human_adj)
setDT(new_ids_usa)

human_adj[, `:=`(
  geoid_o = normalize_geoid(geoid_o),
  geoid_d = normalize_geoid(geoid_d)
)]
new_ids_usa[, geoid := normalize_geoid(geoid)]

## --- Attach newIDs to origin and destination ---
map_o <- new_ids_usa[, .(geoid, newID_o = newID)]
map_d <- new_ids_usa[, .(geoid, newID_d = newID)]

human_mapped <- merge(human_adj, map_o, by.x = "geoid_o", by.y = "geoid", all.x = TRUE)
human_mapped <- merge(human_mapped, map_d, by.x = "geoid_d", by.y = "geoid", all.x = TRUE)


# Keep only fully mapped OD pairs
human_mapped <- human_mapped[!is.na(newID_o) & !is.na(newID_d)]

## --- Aggregate flows to newID x newID (this is where the “many geoids → one newID” gets summed) ---
flows_newID_week <- human_mapped[, .(
  visitor_flows = sum(visitor_flows, na.rm = TRUE),
  pop_flows     = sum(pop_flows,     na.rm = TRUE)
), by = .(date_range, newID_o, newID_d)]

## ===== OPTION A: One adjacency matrix aggregated across all weeks =====
flows_newID_total <- flows_newID_week[, .(
  visitor_flows = sum(visitor_flows, na.rm = TRUE),
  pop_flows     = sum(pop_flows,     na.rm = TRUE)
), by = .(newID_o, newID_d)]

# Use the full set of newIDs from your shapefile mapping as the matrix universe:
all_newIDs <- sort(unique(new_ids_usa$newID))

A_visitors <- make_adj_sparse(flows_newID_total, ids = all_newIDs, weight_col = "visitor_flows")
A_pop      <- make_adj_sparse(flows_newID_total, ids = all_newIDs, weight_col = "pop_flows")


#randomly assign farm numbers to counties
counties$farm_number = sample(0:10, nrow(counties), replace = T)
#Associate poultry data with locations

# 
# #coords from banding data, change to location in poultry data
# poultry = subset(poultry, !is.na(LON_DD) & !is.na(LAT_DD))
# poultry_sf = st_as_sf(poultry, coords = c("LON_DD", "LAT_DD"))
# st_crs(poultry_sf) = st_crs(counties)
# 
# #Pair poultry data with spatial units
# require(data.table)
# poultry_sf$ID = 1:nrow(poultry_sf)
# counties_visitied = st_join(poultry_sf, counties, join = st_intersects)
# counties_visitied_sub = st_drop_geometry(counties_visitied[,c("ID", "GID_2")])
# 
# setDT(counties_visitied);setDT(poultry_sf)
# 
# 
# #merge county IDs with original dataset 
# poultry_with_counties = counties_visitied[poultry_sf, mult = "first", on = "BAND", nomatch = 0L]
# ####Preparing for disease simulation#####

# Disease states: 0 = susceptible, 1 = infected
bird_state <- rep(0,nrow(counties))
poultry_state <- rep(0, nrow(counties))
human_state <- rep(0, nrow(counties))

# Introduce disease in migratory birds
bird_state[sample(1:length(bird_state),  3)] <- 1
# Parameters
beta_bird_poultry <- 3e-7  # transmission from bird to poultry
beta_bird_poultry_max <- 9e-5  #within-poultry spread -- per farm per day (Llanos-Soto et al 2025)

beta_poultry_human <- 0.02  # transmission from poultry to humans
beta_poultry_human_max <- 0.5  # transmission from poultry to humans
beta_bird_bird <- 0.04      # within-bird spread
beta_bird_bird_max <-.08 #Henaux et al 2010
beta_human_human <- 0.005   # within-human spread
beta_human_human_max <- 0.1   # within-human spread
beta_poultry_poulty <- 0.05  #within-poultry spread # https://experts.umn.edu/en/publications/estimating-the-between-farm-transmission-rates-for-highly-pathoge/
beta_poultry_poulty_max <- 0.15  #within-poultry spread

#Transmission model 
# Initialize edge lists for tracking transmission


bird_bird_trans = matrix(0, dim(bird_adj)[1], dim(bird_adj)[2])
bird_poultry_trans = matrix(0, dim(bird_adj)[1], dim(bird_adj)[2])
poultry_human_trans = matrix(0, dim(bird_adj)[1], dim(bird_adj)[2])
human_human_trans = matrix(0, dim(bird_adj)[1], dim(bird_adj)[2])


# spillover_probability <- function(interaction_count) {
#   1 - exp(-0.3 * interaction_count)
# }

timesteps = 50
#Currently does not take in to account poultry to poultry transmission
for (t in 1:timesteps) {
  print(t)
  # Spread among birds
  #bird_infections is possibly infected 
  bird_infectors = which(bird_state > 0)
  infection_weights = colSums(bird_adj[bird_infectors,] )
    
    possible_bird_infected <- which(infection_weights > 0)
    possible_bird_infected_weight <-infection_weights[possible_bird_infected]
    bird_infection_probability = beta_bird_bird + (beta_bird_bird_max - beta_bird_bird) * possible_bird_infected_weight / (possible_bird_infected_weight + 100)
      infection_outcome = rbinom(n = length(bird_infection_probability), 1, bird_infection_probability)
      new_infections = possible_bird_infected[infection_outcome == 1]
        bird_state[new_infections] <- 1
        if (length(new_infections) > 0){
        for (i in 1:length(new_infections)){
        # Record which infected neighbor caused it
          infector_weights = bird_adj[bird_infectors,new_infections[i]] / sum(bird_adj[bird_infectors,new_infections[i]])
          infector_select = sample(bird_infectors,size = 1, prob = infector_weights)
          bird_bird_trans[infector_select,new_infections[i]] = bird_bird_trans[infector_select,new_infections[i]] +1
        }
      }
      
    
  
      
  
  
  # Bird → Poultry
  for (i in 1:length(bird_state)) {
    

    if (bird_state[i] == 1 && poultry_state[i] == 0) {
      bird_infection_probability = min( beta_bird_poultry *counties$farm_number[i] , beta_bird_poultry_max)
      if (length(bird_infection_probability)>0){
      if (rbinom(1, 1, bird_infection_probability)) {
        poultry_state[i] <- 1
        bird_poultry_trans[i,i] =  bird_poultry_trans[i,i]+1
      }
    }}
  }
  
  
  # Poultry → Human
  for (i in 1:length(bird_state)) {
    if (poultry_state[i] == 1 && human_state[i] == 0) {
      human_infection_probability = beta_poultry_human + (beta_poultry_human_max - beta_poultry_human) * counties$human_pop[i] / (counties$human_pop[i] + mean(counties$human_pop))
      if (rbinom(1, 1, human_infection_probability)) {
        human_state[i] <- 1
        poultry_human_trans[i,i] = poultry_human_trans[i,i]+1
      }
    }
  }
  
  
human_adj = as.matrix(A_pop)
human_infectors = which(human_state > 0)
human_infectors_mobility=as.numeric(colnames(human_adj)[is.element(colnames(human_adj), human_infectors)])

human_infectors_colsrows=which(is.element(colnames(human_adj), human_infectors_mobility))

if (length(human_infectors_colsrows) > 0){
  if (length(human_infectors_colsrows)== 1){
   human_infection_weights = as.numeric(human_adj[human_infectors_colsrows,] ) 
  }
  
  if (length(human_infectors_colsrows) > 1){
    human_infection_weights = colSums(human_adj[human_infectors_colsrows,] )
  }

possible_human_infected <- which(human_infection_weights > 0)
possible_human_infected_weight <-human_infection_weights[possible_human_infected]
human_infection_probability = beta_human_human + (beta_human_human_max - beta_human_human) * possible_human_infected_weight / (possible_human_infected_weight + 100)
human_infection_outcome = rbinom(n = length(human_infection_probability), 1, human_infection_probability)
human_new_infections = possible_human_infected[human_infection_outcome == 1]

human_new_infections_id = as.numeric(colnames(human_adj)[human_new_infections])
human_state[human_new_infections_id] <- 1
if (length(human_new_infections_id) > 0){
  if (length(human_infectors_mobility) > 1){
  for (i in 1:length(human_new_infections_id)){
    # Record which infected neighbor caused it
    
    human_infector_weights = human_adj[human_infectors_colsrows,human_new_infections[i]] / sum(human_adj[human_infectors_colsrows,human_new_infections[i]])
    human_infector_select = as.numeric(sample(human_infectors_mobility,size = 1, prob = human_infector_weights))
    human_human_trans[human_infector_select, human_new_infections_id[i]] = human_human_trans[human_infector_select, human_new_infections_id[i]]+1
  }
  }
    if (length(human_infectors_mobility) == 1){
      for (i in 1:length(human_new_infections_id)){
        human_human_trans[human_infectors_mobility, human_new_infections_id[i]] = 
          human_human_trans[human_infectors_mobility, human_new_infections_id[i]]+1
    }
  }
}
}


}




sum_edges = bird_bird_trans + bird_poultry_trans + poultry_human_trans + human_human_trans
g <- graph_from_adjacency_matrix(sum_edges)


#detect communities
community_graph = walktrap.community(g)
#add community membership to shapefile
members = community_graph$membership
members[is.element(members,which(table(community_graph$membership)<10))] = NA
counties$membership = members
counties_communities_only = subset(counties,!is.na(membership))
#plot shapefile
commplot = ggplot()   + 
  geom_sf(colour="NA",data=counties,size=.5,fill="light grey")+
  geom_sf(colour="NA",data=counties_communities_only,size=.5, mapping = aes(fill = as.factor(membership))) +
  xlim(-170,-63) + ylim(22,72)


#eigenvector centrality 
counties$evec = eigen_centrality(g)$vector

evec_plot = ggplot()   +
  geom_sf(colour="NA",data=counties,size=.5, mapping = aes(fill = evec)) +
  scale_fill_distiller(palette="YlOrRd",trans="log10")+
  xlim(-170,-63) + ylim(22,72)
ggsave(evec_plot, filename=paste0("evecplot",'Transmission trial',".png"))

#betweenness centrality 
counties$betweenness = betweenness(graph)

betweenness_plot = ggplot()   +
  geom_sf(colour="NA",data=counties,size=.5, mapping = aes(fill = betweenness)) +
  scale_fill_distiller(palette="Spectral",trans="log2")+
  xlim(-170,-63) + ylim(22,72)

ggsave(betweenness_plot, filename=paste0("betweennessplot","Transmission trial",".png"))

