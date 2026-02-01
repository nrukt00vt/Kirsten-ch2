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
ggplot() + geom_sf(counties_usa, mapping = aes(), colour="red", fill = "green")


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

## --- Sanity checks on mapping ---
# Each geoid should map to exactly one newID
dup_geoid <- new_ids_usa[, .N, by = geoid][N > 1]
if (nrow(dup_geoid) > 0) {
  stop("Found geoids mapping to multiple newIDs. Resolve before aggregating.")
}

## --- Attach newIDs to origin and destination ---
map_o <- new_ids_usa[, .(geoid, newID_o = newID)]
map_d <- new_ids_usa[, .(geoid, newID_d = newID)]

human_mapped <- merge(human_adj, map_o, by.x = "geoid_o", by.y = "geoid", all.x = TRUE)
human_mapped <- merge(human_mapped, map_d, by.x = "geoid_d", by.y = "geoid", all.x = TRUE)

# Optionally inspect what didn't map
unmapped <- human_mapped[is.na(newID_o) | is.na(newID_d),
                         .N, by = .(missing_o = is.na(newID_o), missing_d = is.na(newID_d))]
print(unmapped)

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


## ===== OPTION B: A list of adjacency matrices, one per date_range =====
flows_split <- split(flows_newID_week, by = "date_range", keep.by = FALSE)

A_by_week_visitors <- lapply(flows_split, function(dd) {
  make_adj_sparse(dd[, .(newID_o, newID_d, visitor_flows)], ids = all_newIDs, weight_col = "visitor_flows")
})

A_by_week_pop <- lapply(flows_split, function(dd) {
  make_adj_sparse(dd[, .(newID_o, newID_d, pop_flows)], ids = all_newIDs, weight_col = "pop_flows")
})

## Example:
# names(A_by_week_visitors)               # date_range strings
# A_by_week_visitors[[1]]["2148","2149"]  # flow that week

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
beta_bird_poultry <- 0.1   # transmission from bird to poultry
beta_bird_poultry_max <- 0.5  #within-poultry spread

beta_poultry_human <- 0.02  # transmission from poultry to humans
beta_poultry_human_max <- 0.5  # transmission from poultry to humans
beta_bird_bird <- 0.01      # within-bird spread
beta_bird_bird_max <-.5
beta_human_human <- 0.005   # within-human spread
beta_human_human_max <- 0.1   # within-human spread
beta_poultry_poulty <- 0.03  #within-poultry spread
beta_poultry_poulty_max <- 0.5  #within-poultry spread

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
          bird_trans_edges[[length(bird_trans_edges) + 1]] <- c(infector_select, new_infections[i])
        }
      }
      
    
  
      
  
  
  # Bird → Poultry
  for (i in 1:length(bird_state)) {
    

    if (bird_state[i] == 1 && poultry_state[i] == 0) {
      bird_infection_probability = beta_bird_poultry + (beta_bird_poultry_max - beta_bird_poultry) * counties$farm_number[i] / (counties$farm_number[i] + 100)
      if (length(bird_infection_probability)>0){
      if (rbinom(1, 1, bird_infection_probability)) {
        poultry_state[i] <- 1
        print(i)
        cross_trans_edges[[length(cross_trans_edges) + 1]] <- c(paste0("Bird", i), paste0("Poultry", i))
      }
    }}
  }
  
  # #poultry to poultry
 #  poultry_infections <- which(poultry_state == 0 & (poultry_adj %*% (poultry_state == 1)) > 0)
  # for (i in poultry_infections) {
  #   if (rbinom(1, 1, beta_poultry_poultry)) {
  #     poultry_state[i] <- 1
  #     # Record 3which infected neighbor caused it
  #     infectors <- which(poultry_adj[i, ] == 1 & poultry_state == 1)
  #     if (length(infectors) > 0) {
  #       poultry_trans_edges[[length(poultry_trans_edges) + 1]] <- c(sample(infectors, 1), i)
  #     }
  #   }
  # }
  
  # Poultry → Human
  for (i in 1:length(bird_state)) {
    if (poultry_state[i] == 1 && human_state[i] == 0) {
      human_infection_probability = beta_human_human + (beta_human_human_max - beta_human_human) * counties$human_pop[i] / (counties$human_pop[i] + mean(counties$human_pop))
      print(human_infection_probability)
      if (rbinom(1, 1, human_infection_probability)) {
        print(paste(i, "infected"))
        human_state[i] <- 1
        cross_trans_edges[[length(cross_trans_edges) + 1]] <- c(paste0("Poultry", i), paste0("Human", i))
      }
    }
  }
  
  
human_adj = as.matrix(A_pop)
human_infectors = which(human_state > 0)
human_infectors_mobility=colnames(human_adj)[is.element(colnames(human_adj), human_infectors)]

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

human_new_infections_id = colnames(human_adj)[human_new_infections]
human_state[human_new_infections_id] <- 1
if (length(human_new_infections_id) > 0){
  if (length(human_infectors_mobility) > 1){
  for (i in 1:length(human_new_infections_id)){
    # Record which infected neighbor caused it
    
    human_infector_weights = human_adj[human_infectors_colsrows,human_new_infections[i]] / sum(human_adj[human_infectors_colsrows,human_new_infections[i]])
    human_infector_select = sample(human_infectors_mobility,size = 1, prob = human_infector_weights)
    human_trans_edges[[length(human_trans_edges) + 1]] <- c(human_infector_select, human_new_infections_id[i])
  }
  }
    if (length(human_infectors_mobility) == 1){
      for (i in 1:length(human_new_infections_id)){
        human_trans_edges[[length(human_trans_edges) + 1]] <- c(human_infectors_mobility, human_new_infections_id[i])
    }
  }
}
}



  
  # Human → Human
human_infections <- which(human_state == 0 & (human_adj %*% (human_state == 1)) > 0)
human_infections_weight <-as.numeric(human_adj %*% (human_state == 1))[human_infections]
for (i in 1:length(human_infections)) {
  human_infection_probability = beta_human_human + (beta_human_human_max - beta_human_human) * human_infections_weight[i] / (human_infections_weight[i] + 100)
  #print(human_infection_probability)
  if (rbinom(1, 1, human_infection_probability)) {
    human_state[human_infections[i]] <- 1
    # Record which infected neighbor caused it
    human_infectors <- which(human_adj[human_infections[i], ] == 1 & human_state == 1)
    if (length(human_infectors) > 0) {
      human_trans_edges[[length(human_trans_edges) + 1]] <- c(sample(infectors, 1), human_infections[i])
    }
  }
}
>>>>>>> d72e410a34a9fb3f8eee5bbc84ba19b0ef4d6311

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

