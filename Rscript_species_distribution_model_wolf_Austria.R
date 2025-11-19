# *************************************************************
# Prioritizing areas for wolf management in Austria by modelling habitat potential and depredation risk
# *************************************************************
# version: 18.11.25
# r code authored by Florian Kunz, Fabian Knufinke, Mathias Amon, Luca Fuchs
# script corresponds to publication: XXX [will be added as soon as accepted]

# How to cite: [will be added as soon as accepted]


# *************************************************************
# Chapter 1: prep presence data for SDM
# *************************************************************
# script reads raw occurrence data and generates input data for maxent runs (occurrence and biasfiles)

{library("dplyr")
  library("readxl")
  library("terra")
  library("gdistance")
  library("tidysdm")
  library("sf")
  library("kuenm")}

source("/Rfunction_extract.by.mask.R")# sourcing function, see https://github.com/FloKu/R_functions

path <- "C:/Users/" #set your path

database <- read_excel("/2024_12_10_Datenbank_Wolfsnachweise_OE.xlsx", 
                       sheet = "database", col_types = c("text", "numeric", "numeric", "numeric", 
                                                         "text", "numeric", "numeric", "numeric", 
                                                         "text", "text", "text", "text", 
                                                         "numeric", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text", "text"))

# check for errors/NA in Genauigkeit
length(which(is.na(as.numeric(as.character(database$Genauigkeit)))))
which(is.na(as.numeric(as.character(database$Genauigkeit))))

# check for errors/NA in SCALP
length(which(is.na(as.numeric(as.character(database$SCALP)))))
which(is.na(as.numeric(as.character(database$SCALP))))

# 0) basic descriptives
# **************************
database %>% 
  filter(SCALP<=1) %>% 
  mutate(Riss_Praesenz=as.factor(Riss_Praesenz)) %>% 
  count(Riss_Praesenz)

database %>% 
  filter(SCALP<=1) %>% 
  mutate(Datenquelle=as.factor(Datenquelle)) %>% 
  count(Datenquelle)

database %>% 
  filter(SCALP<=1) %>% 
  mutate(Hinweisart_clean=as.factor(Hinweisart_clean)) %>% 
  count(Hinweisart_clean)

# 1) filter presence data
# **************************
scalp <- 1
genauigkeit <- 100

presence <- database %>% 
  dplyr::select("x", "y", "Genauigkeit", "SCALP", "Riss_Praesenz", "DNA_Ergebnis", "Hinweisart_clean") %>%
  filter(Genauigkeit<=genauigkeit) %>% 
  filter(SCALP==scalp) %>% 
  mutate(DNA_Ergebnis=as.factor(DNA_Ergebnis))

str(presence)
plot(presence[,c(1,2)])

# check data for mistakes in coordinates (mistakes should be corrected in the csv file)
head(presence[order(presence$x, decreasing=T),])
head(presence[order(presence$y, decreasing=T),])

# check data
dplyr::count(presence, Riss_Praesenz)
dplyr::count(presence, Hinweisart_clean)

# remove datapoints outside the study area 

# Warning: Sample at 477456.757048824, 520453.017590053 in occ_joint.csv is missing some environmental data (e.g. distanz_zu_wald)
# arning: Sample at 412918.210533135, 295862.828871286 in occ_joint.csv is missing some environmental data (e.g. distanz_zu_wald)

# -> the points were removed manually 

# 2) disperser analysis
# **************************
# Aim: find individuals that were recorded several times with at least 100km or more between sighings -> these are classified as dispersers and subsequently removed
# This code is used on the database, to see which individuals might classify as disperser, then such dispersers are manually excluded from the data which was already filtered before

database$DNA_Ergebnis <- as.factor(database$DNA_Ergebnis)
str(database)

# create results data.frame
results <- data.frame(matrix(vector(), length(levels(database$DNA_Ergebnis)), 2, dimnames=list(c(), c("Individuum", "max_distance"))))
results$Individuum <- levels(database$DNA_Ergebnis)
head(results)

# extract coordinates as SpatVector and use distance to compute distances between all points
coords <- vect(database, geom=c("x", "y"), crs="EPSG:3416")

for (level in levels(coords$DNA_Ergebnis)) {
  subset <- terra::subset(coords, coords$DNA_Ergebnis==level)
  if (length(subset)!=1) {
    dist <- terra::distance(subset)
    results$max_distance[results$Individuum==level] <- max(dist)/1000
  } else {
    results$max_distance[results$Individuum==level] <- 0
  }
}

# check results
head(results[order(results$max_distance, decreasing=T),])

results_filtered <- results[results$max_distance>100,]
results_filtered[order(results_filtered$max_distance, decreasing=T),]

# checks
print(paste0("Number of potential dispersers: ", nrow(results_filtered)))
print(paste0("Number of Indivuduals in the presence data: ", length(as.factor(levels(presence$DNA_Ergebnis)))))

# create vector with individuals (potential dispersers) that need to be checked
disperser <- c()

for (i in 1:nrow(results_filtered)) {
  if (results_filtered$Individuum[i] %in% levels(presence$DNA_Ergebnis)) {disperser <- append(disperser, results_filtered$Individuum[i], after =1)}
}

# The following Individuals need to be checked manually
disperser

{# 059MATK: Salzburg ist kein Disperser, Tirol, STMK und Kärntnen
  presence <- presence[-(which(presence$DNA_Ergebnis=="059MATK")),]
  # 243MATK: In NÖ territorial, in Stmk EInzelnachweis (laut ÖZ disp.)
  presence <- presence[-(which(presence$DNA_Ergebnis=="243MATK")),]
  # 235MATK: kein Disperser
  # 218MATK: Disperser, weil es Punkte über 100km Entfernung in einem Zeitraum unter 6 M. gibt; Linie der Wanderung über mehrere Bundesländer erkennbar
  presence <- presence[-(which(presence$DNA_Ergebnis=="218MATK")),]
  # 210MATK: Disperser, (auch laut ÖZ Bericht) weil über 100km zwischen den letzten Nachweisen; alle 3 Punkte (aus 2023) in größerer Distanz 
  presence <- presence[-(which(presence$DNA_Ergebnis=="210MATK")),]
  # 181MATK: abgesehen von Voralberg , alles disp. (laut ÖZ Bericht Disp.)
  nrow(presence[which(presence$DNA_Ergebnis=="181MATK"),])
  presence %>%
    mutate(row_index = row_number()) %>% 
    filter(DNA_Ergebnis == "181MATK")
  plot(presence[presence$DNA_Ergebnis == "181MATK", c(1, 2)], pch = 19)
  presence <- presence[-c(108, 186, 187),]
  # 150MATK: Disperser weil über 100km auseinader
  presence <- presence[-(which(presence$DNA_Ergebnis=="150MATK")),]
  # 147MATK: Territorium eindeutig in NÖ und STMK Gebiet
  presence <- presence[-(which(presence$DNA_Ergebnis=="147MATK")),]
  # 134MATK: Disperser, weil 1 Nachweis aus 2021 aus Tirol und der andere Nachweis aus NÖ im Jahr 2022
  presence <- presence[-(which(presence$DNA_Ergebnis=="134MATK")),]
  # 112MATK: Disperser, weil über 100km zwischen den letzten Nachweisen; alle Punkte aus 2021
  presence <- presence[-(which(presence$DNA_Ergebnis=="112MATK")),]
  # 105MATK: kein Disperser, weil über 6M. in einem Gebiet (unter 100km)
  # 092MATK: nur drei Punkte, alle disperser
  presence <- presence[-(which(presence$DNA_Ergebnis=="092MATK")),]}

print(paste0("Points left after removing dispersers: ", nrow(presence)))

# check data
dplyr::count(presence, Riss_Praesenz)

write.csv(presence, paste0(path, "/cleaned_presence.csv"))

# 3) Filter: accounting for sampling bias 
# **************************
# as this involves random subsampling, a loop was used to generate 10 different data sets of presence points

# read in data
presence_for_loop <- read.csv(paste0(path, "/cleaned_presence.csv"))
waldmaske <- rast("C/forest_3416.tif")

presence_for_loop <- presence_for_loop[,2:8]
nrow(presence_for_loop)
head(presence_for_loop)

for (i in 1:10) {
  # 3.1) Reduce the number of non-forest points, random fraction
  # **************************
  seed <- 10+i^2
  set.seed(seed)
  print(seed)
  
  print(paste0("Iteration: ", i, ", Filter 3.1: subsample from non-forest points"))
  
  # create SpatVector
  presence_v <- vect(st_as_sf(presence_for_loop, coords=c("x","y"), crs=3416))
  
  # Extract raster values
  raster_values <- terra::extract(waldmaske, presence_v) # [, 2] to get the values column
  
  # Add the extracted values as a new column to your sf object
  presence_v$IstWald <- raster_values[,2]
  
  # redo into dataframe
  presence <- as.data.frame(presence_v, geom = "XY")
  presence <- presence[,c(7,8,1,2,3,4,5,6)]
  
  print(paste0("Records befor filtering: ", nrow(presence)))
  
  # filter a random fraction of presence points outside of forest
  presence_noriss <- presence[which(presence$Riss_Praesenz!="Riss"),]
  presence_riss <- presence[which(presence$Riss_Praesenz=="Riss"),]
  
  presence_forest <- presence_noriss[which(presence_noriss$IstWald==1),]
  presence_noforest <- presence_noriss[is.na(presence_noriss$IstWald==TRUE),]
  
  # filter noriss data
  presence_noforest_sample <- sample_frac(presence_noforest, 0.25)
  
  presence_noriss <- rbind(presence_forest, presence_noforest_sample)
  presence <- rbind(presence_noriss, presence_riss)
  
  print(paste0("Number of points after non-forested points were subsampled: ", nrow(presence)))
  
  # 3.2) Reduce the number of Riss points, first spatial filtering, then random fraction
  # **************************
  print(paste0("Iteration: ", i, ", Filter 3.2: subsample from Riss points"))
  
  print(paste0("Records befor filtering: ", nrow(presence)))
  
  # first spatial filtering with homerange, then random selection of fraction
  presence_noriss <- presence[which(presence$Riss_Praesenz!="Riss"),]
  presence_riss <- presence[which(presence$Riss_Praesenz=="Riss"),]
  
  # spatial filtering  
  presence_riss_sf <- st_as_sf(presence_riss, coords = c("x", "y"), crs = 3416)
  presence_riss_thinned <- tidysdm::thin_by_dist(presence_riss_sf, dist_min=km2m(1.2))
  coords <- as.data.frame(st_coordinates(presence_riss_thinned))
  colnames(coords) <- c("x", "y")
  presence_riss_df <- cbind(st_drop_geometry(presence_riss_thinned), coords)
  
  # random selection of fraction
  presence_riss_sample <- sample_frac(presence_riss_df, 0.25)
  
  # combine with original
  presence_1 <- rbind(presence_noriss, presence_riss_sample)
  nrow(presence_1)
  
  print(paste0("Number of points after filtering Riss: ", nrow(presence_1)))
  
  rm(presence_noriss, presence_riss)
  
  # 3.3) spatial filtering to reduce autocorrelation
  # **************************
  print(paste0("Iteration: ", i, ", Filter 3.3: Spatial subsampling"))
  
  presence_sf <- st_as_sf(presence_1, coords = c("x", "y"), crs = 3416)
  
  presence_thinned <- tidysdm::thin_by_dist(presence_sf, dist_min=km2m(0.5))
  
  coords <- as.data.frame(st_coordinates(presence_thinned))
  colnames(coords) <- c("X", "Y")
  presence_2 <- cbind(st_drop_geometry(presence_thinned), coords)
  
  print(paste0("Final number of points after spatial subsampling: ", nrow(presence_2)))
  
  # write csv file
  maxent_data <- presence_2 %>% 
    mutate(Species="Canis_lupus") %>% 
    rename("Longitude" = "X") %>% 
    rename("Latitude" = "Y") %>% 
    dplyr::select("Species", "Longitude", "Latitude")
  maxent_data_df <- as.data.frame(maxent_data)
  
  write.csv(maxent_data_df, file = paste0(path, "/occ_joint_", i, ".csv"), row.names = F)
  
  # write shape file
  maxent_csv <- read.csv(paste0(path, "/occ_joint_", i, ".csv"), header = TRUE)
  maxent_shp <- vect(maxent_csv, geom = c("Longitude", "Latitude"), crs = "EPSG:3416")
  writeVector(maxent_shp, filename=paste0(path, "/occ_joint_", i, ".shp"), overwrite=TRUE)
}


# 4) Biasfiles
# **************************
# Biasfiles were created in SDMtoolbox, as the toolbox implemented a much faster algorithm
# Biasfiles then needed to be rescaled and reprojected

path <- ""
ref <- rast(paste0(path, "run_040225_final.tif"))

for (i in 2:10) {
  print(i)
  
  # read biasfiles
  bias <- rast(paste0(path, "presence_and_bias/bias_", i, ".asc"))
  old_range <- range(values(bias), na.rm=TRUE) 
  new_range <- c(1,1000) 
  
  # rescaling
  bias_rescaled <- ((bias - old_range[1]) / (old_range[2] - old_range[1])) * (new_range[2] - new_range[1]) + new_range[1]
  bias_rescaled <- fun.extract.by.mask(bias_rescaled, ref, F)
  
  writeRaster(bias_rescaled, paste0(path,"presence_and_bias/bias_", i, "_rescaled.asc"), NAflag=-9999, overwrite=TRUE)
}


# *************************************************************
# Chapter 2: prep presence data for DSM
# *************************************************************
library(terra)
library(readxl)
library(dplyr)
library(writexl)

# 1.1) Prefiltering of depredation data
# *********************************************
database <- read_excel("2024_12_10_Datenbank_Wolfsnachweise_OE.xlsx",
                       sheet="database", col_types = c("text", "numeric", "numeric", "numeric", 
                                                       "text", "numeric", "numeric", "numeric", 
                                                       "text", "text", "text", "text", 
                                                       "numeric", "text", "text", "text", 
                                                       "text", "text", "text", "text", "text", "text"))
scalp <- 1
genauigkeit <- 100
riss <- "Riss"
old_2014 <- 400060.207543004 # x coordinate of point from 2014
old_2015 <- 357231.594790051 # x coordinate of point from 2015
winter_months <- c(1, 2, 3, 4, 11, 12)
winter_months_2 <- c(390320.798525524, 362915.481552234, 405974.29539108, 356083.633462869,
                     397457.149920761, 385075.872575514, 385200.444136982, 425290.985932141,
                     425342.72621183, 425339.577600299, 396069.128453418, 393905.716798215,
                     410329.232582196, 353641.030669642, 353664.068111961, 384821.696244189,
                     458735.82573869, 455129.946072385, 146722.462374877, 121126.388595564) # x coordinates of points from winter months where column "Monat" was empty

riss <- database %>% 
  filter(Genauigkeit<=genauigkeit) %>% 
  filter(SCALP==scalp) %>% 
  filter(Riss_biaspoints==riss) %>% 
  filter(x != old_2014 & x != old_2015) %>% 
  filter(!Monat %in% winter_months) %>% 
  filter(!x %in% winter_months_2) %>% 
  mutate(DNA_Ergebnis=as.factor(DNA_Ergebnis)) # now has to be spatially filtered in a final step

write_xlsx(riss, "riss_projected.xlsx")
riss_vect <- vect(riss, geom=c("x", "y"), crs="EPSG:3416")
writeVector(riss_vect, "riss_projected.gpkg")


# 1.2) Spatial thinning using spThin
# ******************************************
library(spThin)

riss_vect_4326 <- project(riss_vect, "EPSG:4326")
riss_vect_4326_df <- as.data.frame(riss_vect_4326, geom="WKT")
riss_vect_4326_coords <- geom(riss_vect_4326)[, c("x", "y")]
riss_vect_4326_df_total <- cbind(riss_vect_4326_df, riss_vect_4326_coords)
riss_vect_4326_df_total$Quelle <- riss_vect_4326_df_total$Datenquelle
names(riss_vect_4326_df_total)[names(riss_vect_4326_df_total) == 'Datenquelle'] <- 'species'
riss_vect_4326_df_total$species <- "RISS"

thin(riss_vect_4326_df_total, lat.col="y", long.col="x", spec.col="species",
     thin.par=3, reps=1000, locs.thinned.list.return=TRUE,
     write.files=TRUE, max.files=1, out.dir="/Presence",
     out.base="Thin", write.log.file=TRUE, log.file = "thin_log.txt", verbose=TRUE)

riss_4326_thinned_csv <- read.csv("Thin_thin1.csv", sep=",", dec=".")
riss_4326_thinned <- vect(riss_4326_thinned_csv, geom=c("x", "y"), crs="EPSG:4326")
riss_thinned_projected <- project(riss_4326_thinned, "EPSG:3416")
writeVector(riss_thinned_projected, "riss_thinned_projected.gpkg") # manual comparison to "riss_projected.gpkg" in QGIS
# -> 5 points have been excluded despite certainly stemming from a different individual than all remaining close points.. these need to be re-introduced

riss_wronglyexcluded_above1000m <- riss[c(25, 36, 113), ] # fürs Biasfile wird auch noch gethinned mit 1000m, daher wäre es sinnlos die ganz nahe beieinander liegenden reinzugeben, nur damit sie dann wieder rausgeschmissen werden
riss_wronglyexcluded_above1000m_vect <- vect(riss_wronglyexcluded_above1000m, geom=c("x", "y"), crs="EPSG:3416")
riss_thinned_projected_forbiasfile <- rbind(riss_thinned_projected, riss_wronglyexcluded_above1000m_vect)
writeVector(riss_thinned_projected_forbiasfile, "riss_thinned_projected_forbiasfile.gpkg")

riss_wronglyexcluded <- riss[c(25, 36, 66, 113, 116), ]
riss_wronglyexcluded_vect <- vect(riss_wronglyexcluded, geom=c("x", "y"), crs="EPSG:3416")

riss_thinned_projected_final <- rbind(riss_thinned_projected, riss_wronglyexcluded_vect)
writeVector(riss_thinned_projected_final, "riss_thinned_projected_final.gpkg") # these are all input datapoints for the model (109 points!)

riss_thinned_projected_final_csv <- as.data.frame(geom(riss_thinned_projected_final))
riss_thinned_projected_final_csv$Species <- "RISS"
riss_thinned_projected_final_csv$Longitude <- riss_thinned_projected_final_csv$x
riss_thinned_projected_final_csv$Latitude <- riss_thinned_projected_final_csv$y
riss_thinned_projected_final_csv$geom <- NULL
riss_thinned_projected_final_csv$part <- NULL
riss_thinned_projected_final_csv$x <- NULL
riss_thinned_projected_final_csv$y <- NULL
riss_thinned_projected_final_csv$hole <- NULL
write_xlsx(riss_thinned_projected_final_csv, "riss_thinned_projected_final.csv")
# these are all input datapoints for the model!

# 2.) Biasfile
# *****************************

# 2.1. prepping of presence data
# **********************************
# we have to include other wolf presences in the biasfile to tackle the occurence bias
# (and we also have to thin the data before creating the biasfile 

database <- read_excel("2024_12_10_Datenbank_Wolfsnachweise_OE.xlsx",
                       sheet="database", col_types = c("text", "numeric", "numeric", "numeric", 
                                                       "text", "numeric", "numeric", "numeric", 
                                                       "text", "text", "text", "text", 
                                                       "numeric", "text", "text", "text", 
                                                       "text", "text", "text", "text", "text", "text"))
scalp <- 1
genauigkeit <- 100
biaspoints <- c("biaspoints", "Wildtierriss")
gatterwild <- "Wildtierriss (Gatter)" 
ausland <- c("CZ", "IT")
old_2017 <- c(645407.74689793, 643766.534326266) 
winter_months <- c(1, 2, 3, 4, 11, 12)
winter_months_2 <- c(564072.000994645, 564072.000994645, 580018.049171706,
                     519054.585572484, 404563.461907577, 415764.885942689,
                     506896.29587102, 565305.431382824, 556252.277086584,
                     557432.093828825, 445300.388340525, 385109.48957392,
                     367821.561643383, 389646.565508783, 400329.958629984,
                     408808.317761732, 355000.431929488, 596741.884781307,
                     548895.407645902, 525903.699611608, 514144.913727448,
                     501012.088497242, 424812.352673976, 450744.958674504,
                     117659.332030502, 117233.893800504, 135142.623221989,
                     135118.329361436, 141914.514993627, 547072.815667615,
                     554047.722824135, 552204.943808852, 543014.320219101) # x coordinates of points from winter months where column "Monat" was empty

biaspoints <- database %>% 
  filter(Genauigkeit<=genauigkeit) %>% 
  filter(SCALP==scalp) %>%
  filter(Riss_biaspoints %in% biaspoints) %>%
  filter(Hinweisart_clean != gatterwild) %>%
  filter(!Land %in% ausland) %>%
  filter(!x %in% old_2017) %>%
  filter(!Monat %in% winter_months) %>%
  filter(!x %in% winter_months_2) %>% 
  mutate(DNA_Ergebnis=as.factor(DNA_Ergebnis)) # now has to be spatially filtered

write_xlsx(biaspoints, "biaspoints_projected.xlsx")
biaspoints_vect <- vect(biaspoints, geom=c("x", "y"), crs="EPSG:3416")
writeVector(biaspoints_vect, "biaspoints_projected.gpkg")


# 2.2. Thinning of presence data
# **********************************
library(spThin)

biaspoints_vect <- rbind(biaspoints_vect, riss_thinned_projected_forbiasfile)
biaspoints_vect_4326 <- project(biaspoints_gesamt_vect, "EPSG:4326")
biaspoints_vect_4326_df <- as.data.frame(biaspoints_vect_4326, geom="WKT")
biaspoints_vect_4326_coords <- geom(biaspoints_vect_4326)[, c("x", "y")]
biaspoints_vect_4326_df_total <- cbind(biaspoints_vect_4326_df, biaspoints_vect_4326_coords)
biaspoints_vect_4326_df_total$Quelle <- biaspoints_vect_4326_df_total$Datenquelle
names(biaspoints_vect_4326_df_total)[names(biaspoints_vect_4326_df_total) == 'Datenquelle'] <- 'species'
biaspoints_vect_4326_df_total$species <- "praesenz"

thin(biaspoints_vect_4326_df_total, lat.col="y", long.col="x", spec.col="species",
     thin.par=1, reps=1000, locs.thinned.list.return=TRUE,
     write.files=TRUE, max.files=1, out.dir="C:/Users/fuchs/Desktop/R_Projekte/rissanfälligkeit_neu/Presence",
     out.base="Thin2", write.log.file=TRUE, log.file = "thin_log2.txt", verbose=TRUE)

biaspoints_4326_thinned_csv <- read.csv("Thin2_thin1.csv", sep=",", dec=".")
biaspoints_4326_thinned <- vect(biaspoints_4326_thinned_csv, geom=c("x", "y"), crs="EPSG:4326")
biaspoints_thinned_projected <- project(biaspoints_4326_thinned, "EPSG:3416")
writeVector(biaspoints_thinned_projected, "biaspoints_thinned_projected.gpkg") 

# checking if all depredation datapoints are included in the points to be used for the biasfile

missing_points <- erase(riss_thinned_projected_forbiasfile, biaspoints_thinned_projected)
print(missing_points)
writeVector(missing_points, "missing_points.gpkg") # 4 are missing (not 6, 2 must be a mistake) due to being overruled by regular presence datapoints

# excluding these regular presence datapoints

biaspoints_thinned_projected_dezimiert <- biaspoints_thinned_projected[-c(22, 27, 46, 52), ]

# adding these plus the 2 excluded in the creation of "riss_thinned_projected_forbiasfile.gpkg"
biasfile_wronglyexcluded <- riss[c(66, 113, 114, 116, 142, 167), ]
biasfile_wronglyexcluded_vect <- vect(biasfile_wronglyexcluded, geom=c("x", "y"), crs="EPSG:3416")
biaspunkte_thinned_projected_final <- rbind(biaspoints_thinned_projected_dezimiert, biasfile_wronglyexcluded_vect)
writeVector(biaspunkte_thinned_projected_final, "biaspoints_thinned_projected_final.gpkg") 
# these are all input datapoints for the biasfile! (177 points!)

# 2.3) Biasfiles
# **************************
# Biasfiles were created in SDMtoolbox, as the toolbox implemented a much faster algorithm
# Biasfiles then needed to be rescaled and reprojected

path <- ""
ref <- rast(paste0(path, "run_040225_final.tif"))

# read biasfiles
bias <- rast(paste0(path, "/bias.asc_"))
old_range <- range(values(bias), na.rm=TRUE) 
new_range <- c(1,1000) 
  
# rescaling
bias_rescaled <- ((bias - old_range[1]) / (old_range[2] - old_range[1])) * (new_range[2] - new_range[1]) + new_range[1]
bias_rescaled <- fun.extract.by.mask(bias_rescaled, ref, F)
  
writeRaster(bias_rescaled, paste0(path,"/bias.asc"), NAflag=-9999, overwrite=TRUE)


# *************************************************************
# Chapter 3: kuenm  for SDM
# *************************************************************

# 1) splitting presence points 
# **************************************
library(dplyr)
library(kuenm)

occ_wolf <- read.csv(paste0(path, "/occ_wolf_maxent.csv"), header=TRUE)
split <- kuenm_occsplit(occ_wolf, train.proportion = 0.75, method = "random", save = TRUE, name = "occ")

# Access elements of the split object
train <- split$train
test <- split$test
joint <- split$joint

# Convert to data frames 
train_df <- as.data.frame(train)
test_df <- as.data.frame(test)
joint_df <- as.data.frame(joint)

# Save each data frame as a CSV file
write.csv(train_df, file = "/occ_train.csv", row.names = FALSE)
write.csv(test_df, file = "/occ_test.csv", row.names = FALSE)
write.csv(joint_df, file = "/occ_joint.csv", row.names = FALSE)

# 2) kuenm 
# **************************************
# create sets of variables
help("kuenm_varcomb")
vs <- kuenm_varcomb(var.dir = "Moving_window", out.dir = "grids", min.number = 5, in.format = "ascii", out.format = "ascii")

# create candidate models
help("kuenm_cal")

oj <- "presence_points/occ_joint.csv"
otr <- "presence_points/occ_train.csv"
mvars <- "grids"
bcal <- "batch_cal_291124"
candir <- "Candidate_Models_291124"
regm <- c(2,3)
fclas <- c("lqh","lq","lh")
maxpath <- "C:/Users/floriankunz/Desktop/maxent"
args = "biasfile=E:/floriankunz/LEKO_Habiatmodell/maxent_input_data/biasfile_10km.asc biastype=3"
max.memory <- 20000

kuenm_cal(occ.joint = oj, occ.tra = otr, M.var.dir = mvars, batch = bcal,
          out.dir = candir, reg.mult = regm, f.clas = fclas, 
          maxent.path = maxpath, wait = FALSE, run = TRUE)

# evaluation of candidate models
help("kuenm_ceval")

ote <- "presence_points/occ_test.csv"
cresdir <- "Calibration_results_291124"

cal_eval <- kuenm_ceval(path = candir, occ.joint = oj, occ.tra = otr, occ.test = ote, batch = bcal,
                        out.eval = cresdir, threshold = 6, rand.percent = 50, iterations = 5000,
                        kept = TRUE, selection = "OR_AICc", parallel.proc = FALSE)

#final models 
help("kuenm_mod")

bfmod <- "batch_model_291124"
moddir <- "Final_Models_fin_291124"


kuenm_mod(occ.joint = oj, M.var.dir = mvars, out.eval = cresdir, maxent.path = maxpath, 
          out.dir = moddir, batch = bfmod, rep.n = 10, rep.type = "Bootstrap", 
          jackknife = TRUE, out.format = "cloglog", project = FALSE,
          write.mess = FALSE, write.clamp = FALSE, args = args, wait = FALSE, run = TRUE)


# *************************************************************
# Chapter 4: kuenm  for DSM
# *************************************************************
library(dplyr)
library(kuenm)

# 1) splitting presence points 
# **************************************
# creating independent dataset for model evaluation
occ_Riss <- read.csv("riss_thinned_projected_final.csv", header=TRUE)
split <- kuenm_occsplit(occ_Riss, train.proportion = 0.75, method = "random", save = TRUE, name = "occ")

# Access elements of the split object
train <- split$train
test <- split$test
joint <- split$joint

# Convert to data frames if necessary
train_df <- as.data.frame(train)
test_df <- as.data.frame(test)
joint_df <- as.data.frame(joint)

# Save each data frame as a CSV file
write.csv(train_df, file = "occ_train.csv", row.names = FALSE)
write.csv(test_df, file = "occ_test.csv", row.names = FALSE)
write.csv(joint_df, file = "occ_joint.csv", row.names = FALSE)

# 1) kuenm run
# **************************************
setwd("") 

help("kuenm_varcomb")

kuenm_varcomb(var.dir="Final_variables", out.dir="M_variables_Final",
              min.number=3, in.format="ascii", out.format="ascii")

help("kuenm_cal")

oj <- "occ_joint.csv"
otr <- "occ_train.csv"
mvars <- "M_variables_Final"
candir <- "Candidate_Models_Final_2"
bcal <- "Candidate_Models_Final_2"
regm <- c(3)
maxpath <- "C:/Users/fuchs/Desktop/maxent/maxent"
max.memory <- 1300

fclas <- c("lqh", "lq", "lh", "qh")

kuenm_cal(occ.joint = oj, occ.tra = otr, M.var.dir = mvars, batch = bcal,
          out.dir = candir, reg.mult = regm, f.clas = fclas, 
          args = "biasfile=D:/Depredation/bias.asc biastype=3 maximumiterations=5000 togglelayertype=cat_",
          max.memory = max.memory, maxent.path = maxpath, wait = TRUE, run = TRUE)

help(kuenm_ceval)

ote <- "occ_test.csv"
path <- "Candidate_Models_Final_2"
cresdir <- "Calibration_results_Final_2"

Final_evaluation <- kuenm_ceval(path=path, occ.joint=oj, occ.tra=otr, occ.test=ote,
                                batch=bcal, out.eval=cresdir, threshold=5, rand.percent=50, iterations=5000,
                                kept=TRUE, selection="OR_AICc")

help(kuenm_mod)

bfmod <- "Final_Models_2"
moddir <- "Final_Models_2"

kuenm_mod(occ.joint = oj, M.var.dir = mvars, out.eval = cresdir, maxent.path = maxpath, 
          out.dir = moddir, batch = bfmod, rep.n = 20, rep.type = "Bootstrap", 
          jackknife = TRUE, max.memory = 1300, out.format = "cloglog", project = FALSE,
          write.mess = FALSE, write.clamp = FALSE, wait = TRUE, run = TRUE,
          args = "biasfile=D:/Depredation/bias.asc biastype=3 maximumiterations=5000 togglelayertype=cat_")

# 2) Run where livestock data is density instead of cat
# **************************************
setwd("")

help("kuenm_varcomb")

kuenm_varcomb(var.dir="data_with_AMA", out.dir="M_variables",
              min.number=5, in.format="ascii", out.format="ascii")

help("kuenm_cal")

oj <- "occ_joint.csv"
otr <- "occ_train.csv"
mvars <- "M_variables"
candir <- "Candidate_Models"
bcal <- "Candidate_Models"
regm <- c(3)
maxpath <- "C:/Users/floriankunz/Desktop/Software/maxent/maxent"
max.memory <- 1300
fclas <- c("qh")

kuenm_cal(occ.joint = oj, occ.tra = otr, M.var.dir = mvars, batch = bcal,
          out.dir = candir, reg.mult = regm, f.clas = fclas, 
          args = "biasfile=C:/Users/floriankunz/2025_projects/2025_MS_LEKO/DRM/Maxent_Input_data/Biasfile/bias.asc biastype=3 maximumiterations=5000",
          max.memory = max.memory, maxent.path = maxpath, wait = TRUE, run = TRUE)

help(kuenm_ceval)

ote <- "occ_test.csv"
path <- "Candidate_Models"
cresdir <- "Calibration_results"

Final_evaluation <- kuenm_ceval(path=path, occ.joint=oj, occ.tra=otr, occ.test=ote,
                                batch=bcal, out.eval=cresdir, threshold=5, rand.percent=50, iterations=5000,
                                kept=TRUE, selection="OR_AICc")

help(kuenm_mod)

bfmod <- "Final_Models"
moddir <- "Final_Models"

kuenm_mod(occ.joint = oj, M.var.dir = mvars, out.eval = cresdir, maxent.path = maxpath, 
          out.dir = moddir, batch = bfmod, rep.n = 20, rep.type = "Bootstrap", 
          jackknife = TRUE, max.memory = 1300, out.format = "cloglog", project = FALSE,
          write.mess = FALSE, write.clamp = FALSE, wait = TRUE, run = TRUE,
          args = "biasfile=C:/Users/floriankunz/2025_projects/2025_MS_LEKO/DRM/Maxent_Input_data/Biasfile/bias.asc biastype=3 maximumiterations=5000")

# 3) Run with both livestock variables to check which models are better if kuenm can choose freely
# **************************************
setwd("")

help("kuenm_varcomb")

kuenm_varcomb(var.dir="data_with_AMA", out.dir="M_variables",
              min.number=5, in.format="ascii", out.format="ascii")

help("kuenm_cal")

oj <- "occ_joint.csv"
otr <- "occ_train.csv"
mvars <- "M_variables"
candir <- "Candidate_Models"
bcal <- "Candidate_Models"
regm <- c(3)
maxpath <- "C:/Users/floriankunz/Desktop/Software/maxent/maxent"
max.memory <- 1300
fclas <- c("qh")

kuenm_cal(occ.joint = oj, occ.tra = otr, M.var.dir = mvars, batch = bcal,
          out.dir = candir, reg.mult = regm, f.clas = fclas, 
          args = "biasfile=C:/Users/floriankunz/2025_projects/2025_MS_LEKO/DRM/Maxent_Input_data/Biasfile/bias.asc biastype=3 maximumiterations=5000",
          max.memory = max.memory, maxent.path = maxpath, wait = TRUE, run = TRUE)

help(kuenm_ceval)

ote <- "occ_test.csv"
path <- "Candidate_Models"
cresdir <- "Calibration_results"

Final_evaluation <- kuenm_ceval(path=path, occ.joint=oj, occ.tra=otr, occ.test=ote,
                                batch=bcal, out.eval=cresdir, threshold=5, rand.percent=50, iterations=5000,
                                kept=TRUE, selection="OR_AICc")

help(kuenm_mod)

bfmod <- "Final_Models"
moddir <- "Final_Models"

kuenm_mod(occ.joint = oj, M.var.dir = mvars, out.eval = cresdir, maxent.path = maxpath, 
          out.dir = moddir, batch = bfmod, rep.n = 20, rep.type = "Bootstrap", 
          jackknife = TRUE, max.memory = 1300, out.format = "cloglog", project = FALSE,
          write.mess = FALSE, write.clamp = FALSE, wait = TRUE, run = TRUE,
          args = "biasfile=C:/Users/floriankunz/2025_projects/2025_MS_LEKO/DRM/Maxent_Input_data/Biasfile/bias.asc biastype=3 maximumiterations=5000")


# *************************************************************
# Chapter 5: post-run analyses
# *************************************************************
library("terra")
library("dplyr")

path <- ""

# 1.1) Summarize all 10 HSI repeats (using different input data) into results
# **************************
#----
# create vectors to store the repeats of SDM
{models <- list()
  stdevs <- list()
  auc <- c()
  alt.PI <- c()
  alt.PC <- c()
  for.PI <- c()
  for.PC <- c()
  d2s.PI <- c()
  d2s.PC <- c()
  d2f.PI <- c()
  d2f.PC <- c()
  vrm.PI <- c()
  vrm.PC <- c()
  bin <- c()}

for (i in 1:10) {
  model <- rast(paste0(path, "HSI/re-run_", i, "/Canis_lupus_avg.asc"))
  sd <- rast(paste0(path, "HSI/re-run_", i, "/Canis_lupus_stddev.asc"))
  stats <- read.csv(paste0(path, "HSI/re-run_", i, "/maxentResults.csv"))
  
  # append model and sd to vector
  models[[i]] <- model
  stdevs[[i]] <- sd
  
    # append stats to vector
  auc <- c(auc, stats$Test.AUC[21])
  alt.PI <- c(alt.PI, stats$seehöhe.permutation.importance[21])
  alt.PC <- c(alt.PC, stats$seehöhe.contribution[21])
  for.PI <- c(for.PI, stats$wald_im_umkreis_1200m.permutation.importance[21])
  for.PC <- c(for.PC, stats$wald_im_umkreis_1200m.contribution[21])
  d2s.PI <- c(d2s.PI, stats$distanz_zu_menschlichen_siedlungen.permutation.importance[21])
  d2s.PC <- c(d2s.PC, stats$distanz_zu_menschlichen_siedlungen.contribution[21])
  d2f.PI <- c(d2f.PI, stats$distanz_zu_wald.permutation.importance[21])
  d2f.PC <- c(d2f.PC, stats$distanz_zu_wald.contribution[21])
  vrm.PI <- c(vrm.PI, stats$vrm_im_umkreis_1200m.permutation.importance[21])
  vrm.PC <- c(vrm.PC, stats$vrm_im_umkreis_1200m.contribution[21])
  bin <- c(bin, stats$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[21])
}

# calculate average HSI model ad stdev
models_stack <-sds(models)
stdevs_stack <-sds(stdevs)

hsi_avg <- terra::app(models_stack, mean)
hsi_sd <- terra::app(stdevs_stack, sd)

writeRaster(hsi_avg, paste0(path, "HSI/final_repeats/final_HSI_avg.tif"))
writeRaster(hsi_sd, paste0(path, "HSI/final_repeats/final_HSI_sd.tif"))

# calculate final PI and PC for all repeats
summary <- data.frame(matrix(nrow=10, ncol=12))
colnames(summary) <- c("AUC", "alt.PI", "alt.PC", "for.PI", "for.PC", "d2s.PI", "d2s.PC", "d2f.PI", "d2f.PC", "vrm.PI", "vrm.PC", "binary")

{summary$AUC <- auc
  summary$alt.PI <- alt.PI
  summary$alt.PC <- alt.PC
  summary$for.PI <- for.PI
  summary$for.PC <- for.PC
  summary$d2s.PI <- d2s.PI
  summary$d2s.PC <- d2s.PC
  summary$d2f.PI <- d2f.PI
  summary$d2f.PC <- d2f.PC
  summary$vrm.PI <- vrm.PI
  summary$vrm.PC <- vrm.PC
  summary$binary <- bin}

average <- summary %>% 
  summarise(across(where(is.numeric), mean))

summary <- rbind(summary, average)
write.csv2(summary, paste0(path, "HSI/final_repeats/summary.csv"))
#----

# 1.2) Summarize DRM results
# **************************
#----
drm_sum <- read.csv(paste0(path, "DRM/re-run_1/maxentResults.csv"))
colnames(drm_sum)

# drm average Test AUC
drm_sum$Test.AUC[21]

# drm distance forest
drm_sum$ATfinal_Distanz_zu_Wald.permutation.importance[21]
drm_sum$ATfinal_Distanz_zu_Wald.contribution[21]

# drm livestock
drm_sum$cat_ATfinal_Schafe_Rinder.permutation.importance[21]
drm_sum$cat_ATfinal_Schafe_Rinder.contribution[21]

# drm red deer
drm_sum$ATfinal_HSIRotwild_in_1200m_Umkreis.permutation.importance[21]
drm_sum$ATfinal_HSIRotwild_in_1200m_Umkreis.contribution[21]

# distance open area
drm_sum$ATfinal_Distanz_zu_Offenland.permutation.importance[21]
drm_sum$ATfinal_Distanz_zu_Offenland.contribution[21]

# drm slope
drm_sum$ATfinal_durchschnittliche_Steigung_in_1200m_Umkreis.permutation.importance[21]
drm_sum$ATfinal_durchschnittliche_Steigung_in_1200m_Umkreis.contribution[21]

# drm binary conversion
drm_sum$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[21]
#----


# 2) TSS and kappa of models
# **************************
# function by https://github.com/KarlssonCatharina/MaxEnt_TSS_calculations/blob/master/MaxEnt_TSS_calculations.R
TSS_calculations <- function (sample_clog, prediction_clog, n, th) {
  
  xx <- sum(sample_clog > th)
  yy <- sum(prediction_clog > th)
  xxx <- sum(sample_clog < th)
  yyy <- sum(prediction_clog < th)
  
  ncount <- sum(xx,yy,xxx,yyy)
  
  overallaccuracy <- (xx + yyy)/ncount 
  sensitivity <- xx / (xx + xxx)
  specificity <- yyy / (yy + yyy)
  tss <- sensitivity + specificity - 1
  
  #kappa calculations
  a <- xx + xxx
  b <- xx + yy
  c <- yy + yyy
  d <- xxx + yyy
  e <- a * b
  f <- c * d
  g <- e + f
  h <- g / (ncount * ncount)
  hup <- overallaccuracy - h
  hdown <- 1 - h
  
  kappa <- hup/hdown
  Po <- (xx + yyy) / ncount
  Pe <- ((b/ncount) * (a/ncount)) + ((d/ncount) * (c/ncount))
  Px1 <- Po - Pe
  Px2 <- 1 - Pe
  Px3 <- Px1/Px2
  
  tx1 <- xx + yyy
  tx2 <- 2 * a * c
  tx3 <- a - c
  tx4 <- xx - yyy
  tx5 <- ncount * (( 2 * a ) - tx4)
  tx6 <- ncount * tx1
  
  kappamax <- (tx6 - tx2 - (tx3 * tx4)) / ((tx5 - tx3) - (tx3 * tx4))
  
  #cat(" Maxent results for model with\n",a,"test sample predictions\n",c ,"background predicitons\n\n TSS value:        ", tss,"\n Overall accuracy: ",overallaccuracy,"\n Sensitivity:      ",sensitivity,"\n Specificity:      ",specificity,"\n Kappa:            ",kappa,"\n Kappa max:        ",kappamax)
  result <- c(tss, overallaccuracy, sensitivity, specificity, kappa, kappamax)
  return(result)
}

# analysis for HSI
#----
tss_means <- c()

# loop over all 10 repeats
for (i in 1:10) {
  
  print(paste0("Re-run: ", i))
  sum <- read.csv(paste0(path, "HSI/re-run_", i, "/maxentResults.csv"))
  
  tss <- c()
  
  # run loop over all 20 iterations per repeat
  for (j in 1:20) {
    print(paste0("Iteration: ", j))
    bp <- sprintf("/Canis_lupus_%d_backgroundPredictions.csv", j-1)
    sa <- sprintf("/Canis_lupus_%d_samplePredictions.csv", j-1)
    n <- sum$X.Background.points[j]
    th <- sum$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[j]
    
    #read in the files
    backgroundpredictions <- read.csv(paste0(path,"HSI/re-run_",i, bp))
    samplepredictions <- read.csv(paste0(path,"HSI/re-run_",i, sa))
    
    #we need the last column so will set the number as x
    x <- length(backgroundpredictions)
    
    #extract the cloglog/logistic results
    backgroundclog <- backgroundpredictions[,x]
    
    #now read in the sample predictions for testing
    # we need the last column again of logistic or cloglog predictions so set a second x
    x2 <- length(samplepredictions)
    
    #extract the cloglog/logistic results for sample
    sampleclog <- samplepredictions[,x2]
    tss_iteration <- TSS_calculations(sampleclog,backgroundclog,n,th)[1]
    tss <- c(tss, tss_iteration)
  }
  mean <- mean(tss)
  tss_means <- c(tss_means, mean)
}

# full mean TSS for HSI
tss_means <- c(tss_means, mean(tss_means))

# add to summary
summary <- summary %>% 
  mutate(TSS = tss_means)
write.csv2(summary, paste0(path, "HSI/final_repeats/summary.csv"))
#----

# analysis for DRM
#----
# read for thresholds
sum <- read.csv(paste0(path, "DRM/re-run_1/maxentResults.csv"))

tss <- c()

# run loop over all iterations
for (i in 1:20) {
  
  print(sum$Species[i])
  
  bp <- sprintf("/RISS_%d_backgroundPredictions.csv", i-1)
  sa <- sprintf("/RISS_%d_samplePredictions.csv", i-1)
  n <- sum$X.Background.points[i]
  th <- sum$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[i]
  
  #read in the files
  backgroundpredictions <- read.csv(paste0(path,"DRM/re-run_1", bp))
  samplepredictions <- read.csv(paste0(path,"DRM/re-run_1", sa))
  
  #we need the last column so will set the number as x
  x <- length(backgroundpredictions)
  
  #extract the cloglog/logistic results
  backgroundclog <- backgroundpredictions[,x]
  
  #now read in the sample predictions for testing
  # we need the last column again of logistic or cloglog predictions so set a second x
  x2 <- length(samplepredictions)
  
  #extract the cloglog/logistic results for sample
  sampleclog <- samplepredictions[,x2]
  tss_iteration <- TSS_calculations(sampleclog,backgroundclog,n,th)[1]
  tss <- c(tss, tss_iteration)
}

mean(tss)
#----


# 3) binary maps
# **************************
# for HSI, loop over average models
#----
area <- c()

for (i in 1:10) {
  
  print(i)
  
  # read in data
  hsi <- rast(paste0(path, "HSI/re-run_", i, "/Canis_lupus_avg.asc"))
  sum <- read.csv(paste0(path, "HSI/re-run_", i, "/maxentResults.csv"))
  th <- sum$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[21]
  
  # calculate area in km2
  binary <- hsi
  binary[binary < th] <- NA
  binary[binary >= th] <- 1
  size <- terra::global(binary, fun="notNA")/100
  
  area <- c(area, size$notNA)
}

mean(area)
sd(area)
# On average, 39779.18 km2 are suitable habitat

100/(terra::global(hsi, fun="notNA")/100)*mean(area)
# translating to xx percent of the country

# Area of Austria in km2, a check
terra::global(hsi, fun="notNA")/100
#----

# for HSI, the average model
#----
hsi <- rast(paste0(path, "HSI/final_repeats/final_HSI_avg.tif"))
treshold_hsi <- summary$binary[11]

hsi_binary <- hsi
hsi_binary[hsi_binary < treshold_hsi] <- 0
hsi_binary[hsi_binary >= treshold_hsi] <- 1
writeRaster(hsi_binary, paste0(path, "HSI/final_repeats/hsi_binary.tif"), overwrite=T)

hsi_binary[hsi_binary == 0] <- NA
terra::global(hsi_binary, fun="notNA")/100
# Austria has 37256 km2 of wolf habitat
#----

# compare binary HSI average model to forest 
#----
hsi_bin <- rast(paste0(path, "HSI/final_repeats/hsi_binary.tif"))
wald <- rast(paste0("C:/Users/floriankunz/seadrive_root/Florian_1/Für mich freigegeben/LEKO WOLF AT/DATEN/final_data/Wald/Wald_nominal.tif"))

wald <- resample(wald, hsi_bin)
crs(hsi_bin) <- crs(wald)

overlay <- hsi_bin+wald

overlay_2 <- overlay
overlay_2[overlay_2 < 2] <- NA
terra::global(overlay_2, fun="notNA")/100
# 31657 km2 are similar between HSI and forest 

overlay_1 <- overlay
overlay_1[overlay_1 == 0] <- NA
overlay_1[overlay_1 == 2] <- NA
terra::global(overlay_1, fun="notNA")/100
# 14763 km2 exist that aere either HSI or wald

overlay <- hsi_bin*2-wald

overlay_3 <- overlay
overlay_3[overlay_3 < 2] <- NA
terra::global(overlay_3, fun="notNA")/100
# 7902 km2 of suitable habitat are situated outside forests
#----

# for HSI, best run based on AUC
#----
hsi1 <- rast(paste0(path, "HSI/re-run_1/Canis_lupus_avg.asc"))
hsi_sum1 <- read.csv(paste0(path, "HSI/re-run_1/maxentResults.csv"))
treshold_hsi1 <- hsi_sum1$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[21]

hsi_binary <- hsi1
hsi_binary[hsi_binary < treshold_hsi1] <- 0
hsi_binary[hsi_binary >= treshold_hsi1] <- 1
writeRaster(hsi_binary, paste0(path, "HSI/final_040225/hsi_binary.tif"), overwrite=T)

hsi_binary[hsi_binary == 0] <- NA
terra::global(hsi_binary, fun="notNA")/100
# Austria has 37256 km2 of wolf habitat
#----

# read DRM
#----
drm_sum <- read.csv(paste0(path, "DRM/re-run_1/maxentResults.csv"))
treshold_drm <- drm_sum$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[21]
drm <- rast(paste0(path, "DRM/re-run_1/RISS_avg.asc"))

# Area of depredation risk in km2
drm_binary <- drm
drm_binary[drm_binary < treshold_drm] <- 0
drm_binary[drm_binary >= treshold_drm] <- 1
writeRaster(drm_binary, paste0(path, "DRM/final/drm_binary.tif"), overwrite=T)

drm_binary[drm_binary == 0] <- NA
terra::global(drm_binary, fun="notNA")/100
# Austria has 16340 km2 of alpine pasture for risk
#----


# 4) Number of territories
# **************************
# 4.1) based on the 10x10km,based on the approach by Planillo et al. 2024
#----
hsi_bin <- rast(paste0(path, "HSI/final_repeats/HSI_binary.tif"))

# aggregate binary map to 10x10km
hsi_10km <- terra::aggregate(hsi_bin, fact = 100, fun = sum, na.rm = TRUE)
plot(hsi_10km)

# remove all cells with less than 50 % habitat coverage
hsi_10km[hsi_10km < 5000] <- NA

# remove all cells that stand alone
clusters <- patches(hsi_10km, directions = 8, zeroAsNA=TRUE)  # 8 directions for connectivity (queen's case)
plot(clusters)

cluster_sizes <- freq(clusters)  # Frequency table of cluster IDs and sizes

size_threshold <- 1
small_clusters <- cluster_sizes[cluster_sizes$count <= size_threshold, 2]

hsi_10km_clean <- hsi_10km
hsi_10km_clean[clusters %in% small_clusters] <- NA

plot(hsi_10km_clean)

terra::global(hsi_10km_clean, fun="notNA")*100
# Austria has 38900 km2 of wolf habitat

(terra::global(hsi_10km_clean, fun="notNA")*100)/200
# which translates to 194.5 territories
#----

# 4.2) Number of territories within 100x100
# function to calculate number of territories
n_territories <- function (model, th, terr_size) { # terr_size in km2
  
  # create binary map per iteration
  binary <- model
  binary[binary < th] <- 0
  binary[binary >= th] <- 1

  # check if patches occur
  clusters <- patches(binary, directions = 8, zeroAsNA=TRUE)  # 8 directions for connectivity (queen's case)
  cluster_sizes <- freq(clusters)  # Frequency table of cluster IDs and sizes

  # remove all patches that dont meet the criteria of a minimum habitat of [terr_size] km2
  if (nrow(cluster_sizes) != 1) {
    small_clusters <- cluster_sizes[cluster_sizes$count <= (terr_size)*100, 2] # apply territory size in correct unit
    binary[clusters %in% small_clusters] <- NA
  }
  
  # calculate number of territories
  binary[binary == 0] <- NA
  n <- (terra::global(binary, fun="notNA")/100)/terr_size
  return(n)
}
#----

# 4.2.1) analysis for the average run
#----
model <- rast(paste0(path, "/HSI/final_repeats/HSI_binary.tif"))
hsi_sum <- read.csv2(paste0(path, "/HSI/final_repeats/summary.csv"))
th <- hsi_sum$binary[11]
terr_size <- 200

n_territories(model, th, terr_size)
# Austria has 39559 km2 of wolf habitat
# which translates to 169.59 wolf territories
#----

# 4.2.2) analysis for each repeat, to get a range
#----
territories <- c()

for (i in 1:10) {
  
  print(i)
  
  # read in data
  hsi <- rast(paste0(path, "HSI/re-run_", i, "/Canis_lupus_avg.asc"))
  sum <- read.csv(paste0(path, "HSI/re-run_", i, "/maxentResults.csv"))
  binary <- sum$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[21]
  
  # calculate territories
  n <- n_territories(hsi, binary, 200)
  print(n)
  
  territories <- c(territories, n$notNA)
}

# results
mean(territories)
sd(territories)
# mean: 168.37 sd: 41.64
#----

# 4.2.3) analysis for best run based on AUC
#----
sum <- read.csv(paste0(path, "/HSI/re-run_1/maxentResults.csv"))
terr_size <- 200

territories <- data.frame(matrix(nrow=20, ncol=2))
colnames(territories) <- c("iteration", "count")

# run loop over all iterations
for (i in 1:20) {
  print(sum$Species[i])
  
  # read model and treshold
  model <- rast(paste0(path, "/HSI/re-run_1/", sprintf("Canis_lupus_%d.asc", i-1)))
  th <- sum$Maximum.test.sensitivity.plus.specificity.Cloglog.threshold[i]
  
  # calculate number of territories 
  n <- n_territories(model, th, terr_size)
  territories[i,1] <- sum$Species[i]
  territories[i,2] <- n
}

territories
mean(territories)
sd(territories)
#----


# 5) Prioritization
# **************************
# read in models
hsi <- rast(paste0(path, "/HSI/final_repeats/final_HSI_avg.tif"))
drm <- rast(paste0(path, "/DRM/final/final_DRM.tif"))

# read in binary
hsi_bin <- rast(paste0(path, "/HSI/final_repeats/hsi_binary.tif"))
drm_bin <- rast(paste0(path, "/DRM/final/drm_binary.tif"))

# 5.1) Overlay binary 
combined <- hsi_bin+drm_bin

combined_2 <- combined
combined_2[combined_2 < 2] <- NA
terra::global(combined_2, fun="notNA")/100
# 8368.72 km2 are shared in both SDM and DRM

combined_1 <- combined
combined_1[combined_1 == 2] <- NA
combined_1[combined_1 == 0] <- NA
terra::global(combined_1, fun="notNA")/100
# 39161.29 km2 are exclusive to one of the models

# 5.2) correlation
stack <- c(hsi, drm)
layerCor(stack, fun="pearson")
# cor coef 0.0979

# 5.3) SARI (suitability-adjusted risk index)
# create mask
mask <- hsi_bin + drm_bin
mask[mask == 1] <- 0

# SARI
sari <- (drm*hsi)/(hsi+(1-drm))
writeRaster(sari, paste0(path, "COM/SARI.tif"))

# cut to mask and reclassify into priority area
sari_bin <- sari
sari_bin[mask == 0] <- NA
writeRaster(sari_bin, paste0(path, "COM/SARI_bin.tif"))

terra::global(sari_bin, fun="notNA")/100
# In Austria, 8368 km2 need to be prioritized

# categorization based on percentiles
values <- values(sari, na.rm = TRUE)  # Extract raster values, excluding NA
p90 <- quantile(values, 0.9)  # 25th percentile
p70 <- quantile(values, 0.7)  # 75th percentile

reclass_matrix <- matrix(c(
  -Inf, p70, 1,  # Bottom -> Category 3
  p70, p90, 2,   # Top 30% -> Category 2
  p90, Inf, 3    # Top 10% -> Category 1
), ncol = 3, byrow = TRUE)

# Reclassify the raster
sari_cat <- classify(sari, reclass_matrix)
sari_bin_cat <- classify(sari_bin, reclass_matrix)
  
# Save the reclassified raster
writeRaster(sari_cat, "COM/SARI_cat.tif", overwrite = TRUE)
writeRaster(sari_bin_cat, "COM/SARI_bin_cat.tif", overwrite = TRUE)

sari_bin_cat[sari_bin_cat ==2] <- NA
terra::global(sari_bin_cat, fun="notNA")/100
# 4875 km2 of area are covered by sari_bin in category 1


# *************************************************************
# Chapter 4: visualization
# *************************************************************
# version: 18.11.2025
# author: Fabian Knufinke

require("terra")
require("tidyverse")
require("ape")
require("BAMMtools")
require("classInt")
require("ggplot2")
require("ggspatial")
require("cowplot")

# 0) Preface
# **************************
#path Fabians Laptop
path <- "C:/Users/"

# Creating new versions of the maps for the publications for the habitat and drm
le_wolf <- terra::rast(paste0(path, "final_HSI_avg.tif") )
drm <- terra::rast(paste0(path, "final_DRM.tif") )
SARI <- terra::rast(paste0(path, "SARI.tif") )
SARI_bin <- terra::rast(paste0(path, "SARI_bin.tif") )

crs(le_wolf) <- "epsg:3416"
crs(drm) <- "epsg:3416"
crs(SARI) <- "epsg:3416"
crs(SARI_bin) <- "epsg:3416"

breaks_le <- classIntervals(values(le_wolf), n=5, style="jenks")$brks
breaks_riss <- classIntervals(values(drm), n=5, style="jenks")$brks

le_wolf_class <- classify(le_wolf, breaks_le, include.lowest=TRUE) 
riss_mod_class <- classify(drm, breaks_riss, include.lowest=TRUE) 

colors_le <- c("#f7f7f7", "#d9f2d9", "#b2e6b2", "#8cda8c", "#66ce66")
colors_SARI <- c("#f7f7f7",'#fecc5c','#fd8d3c','#f03b20','#bd0026')
colors_DRM <- c("#ECFEFF","#A2F4FD","#42D3F2","#2C92B8","#015F78")
colors_SARI_BIN <- c('#fecc5c','#bd0026')

legend_labels_eng <- c("very low", "low", "medium", "high", "very high")

# Plot the maps in those colours
le_wolf_class_plot <- plot(le_wolf_class, col= c("#f7f7f7", "#d9f2d9", "#b2e6b2", "#8cda8c", "#66ce66"))
riss_mod_class_class_plot <- plot(riss_mod_class, c("#ECFEFF","#A2F4FD","#42D3F2","#2C92B8","#015F78"))

le_wo_all <- plot(le_wolf_class, col= c("#f7f7f7", "#d9f2d9", "#b2e6b2", "#8cda8c", "#66ce66"), axes=FALSE, legend=FALSE)
riss_wo_all <-plot(riss_mod_class, col= c("#ECFEFF","#A2F4FD","#42D3F2","#2C92B8","#015F78"), axes=FALSE, legend=FALSE)

library(patchwork)

# Convert raster to data frame for ggplot
le_df <- as.data.frame(le_wolf_class, xy=TRUE)
colnames(le_df)[3] <- "value"

le_plot_wo_title <- ggplot(data = le_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(value))) + 
  scale_fill_manual(values = colors_le, labels = legend_labels_eng, name = "SDM") + 
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.125,0.8),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  coord_sf(crs= "epsg:3416") 
plot(le_plot_wo_title)

# Convert raster to data frame for ggplot
breaks_drm <- classIntervals(values(drm), n=5, style="jenks")$brks
drm_class <- classify(drm, breaks_drm, include.lowest=TRUE)

drm_df <- as.data.frame(drm_class, xy=TRUE)
colnames(drm_df)[3] <- "value"

drm_plot_wo_title <- ggplot(data = drm_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(value))) + 
  scale_fill_manual(values = colors_DRM, labels = legend_labels_eng, name = "DRM") + 
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.125,0.8),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  coord_sf(crs= "epsg:3416") 
plot(drm_plot_wo_title)


# Convert raster to data frame for ggplot
breaks_SARI <- classIntervals(values(SARI), n=5, style="jenks")$brks
SARI_class <- classify(SARI, breaks_SARI, include.lowest=TRUE)

SARI_df <- as.data.frame(SARI_class, xy=TRUE)
colnames(SARI_df)[3] <- "value"

# now produce the polygon of the inlet which is zoomed in for the next plot
library(sf)

polygon_coords <- matrix(c(
  290990.4, 367978.7,
  326039.4, 367978.7,
  326039.4, 389354.5,
  290990.4, 389354.5,
  290990.4, 367978.7  
), ncol = 2, byrow = TRUE)

# Create a terra SpatVector polygon
polygon_sf <- st_as_sf(vect(polygon_coords, type = "polygons", crs = "epsg:3416"))

SARI_plot_wo_title <- ggplot(data = SARI_df) + 
  geom_raster(aes(x = x, y = y, fill = factor(value))) + 
  scale_fill_manual(values = colors_SARI, labels = legend_labels_eng, name = "SARI") + 
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.125,0.8),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  coord_sf(crs= "epsg:3416") +
  geom_sf(data = polygon_sf, fill = "transparent", color = "black", size = 1) # add the polygon to the map

plot(SARI_plot_wo_title)

#  Include orthofoto in the map
# we need to load leaflet, fit the boundaries according to the area we want to investigate and then
# plot the SARI above of the map

library(leaflet)

# Define breaks for 0%-90% and 90%-100% of the values
# for that we are first extracting the breaks and then using them as fixed breaks because this is not working directly with unequally distributed
# breaks in the package

quantile(SARI_df$value, seq(0.9, 1, 0.1))

# create the breaks of quantiles with 10, to create 10% pieces of the data. then use the orginal SARI with the breaks extracted from there to create a 
# 90 and a 10% class
classIntervals(values(SARI_bin), n = 10, style = "quantile")$brks # pick the second highest as the break (90%)
breaks_SARI_bin <- classIntervals(values(SARI_bin), n = 2,
                                  fixedBreaks = c(0, 0.8163857, 1),
                                  style = "fixed")$brks


SARI_bin_class <- classify(SARI_bin, breaks_SARI_bin, include.lowest=TRUE) # this reclassifies not by a defined class label, but by the class borders/breaks

# be aware! leaflet has theoretically a project part in the addRasterImage function, but this does NOT work correct, therefore project it manually before
r_leaflet <- projectRasterForLeaflet(SARI_bin_class, method = "bilinear")  # be aware, that we use Sari Bin here!

# Define color palette and use it as a binary colour scheme with bins
# this is already defined before but we need it here for the legend
colors_SARI_BIN <- colorBin(palette = colors_SARI_BIN, domain = values(r_leaflet), bins=c(0, 0.8163857, 1), na.color = "transparent")

# Create leaflet map
sari_orthofoto_leaflet <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("BasemapAT.orthofoto") %>% 
  fitBounds(11.88880, 47.20280, 12.35675, 47.40002) %>% 
  addRasterImage(r_leaflet, colors = colors_SARI_BIN, opacity = 0.5, project = FALSE) %>%
  addScaleBar(position="bottomright", options = scaleBarOptions(
    maxWidth = 200,
    metric = TRUE,
    imperial = FALSE,
    updateWhenIdle = TRUE)) %>% 
  addLegend("topleft", colors = c('#fecc5c','#bd0026'),# use the colours here hard coded because pal and labels does not work together
            labels = c("Bottom 90%", "Top 10%"),
            title = "masked SARI",
            opacity = 1)%>%
  htmlwidgets::onRender("
    function(el, x) {
      var css = `
        .leaflet .legend {
          line-height: 35px;
          font-size: 35px;
        }
        .leaflet .legend i {
          width: 35px;
          height: 35px;
        }
        
      `;
      var style = document.createElement('style');
      style.type = 'text/css';
      style.appendChild(document.createTextNode(css));
      document.head.appendChild(style);
    }
  ")

sari_orthofoto_leaflet

# save the SARI map manually from the plotting window as jpeg

# load in the orhofoto and then change the size so that it better fits into the setup of maps => scale to 90%
sari_orthofoto <- ggdraw() +
  draw_image("SARI_Orthophoto.jpeg",
             scale = 0.9) 

# Create a blank plot to add white space
blank_space <- ggplot() + theme_void()

# Combine the blank space with the actual plots
plotrow <- plot_grid(blank_space, 
                     le_plot_wo_title, 
                     drm_plot_wo_title, 
                     blank_space,
                     SARI_plot_wo_title, 
                     sari_orthofoto, 
                     ncol = 3,
                     nrow = 2,
                     labels = c("", "A", "B", "", "C", "D"), 
                     rel_widths = c(0.1, 1, 1, 0.1, 1, 1)
) 

# Adjust the width of the blank space
# include the blank labels in there to include the empty plots in there and therefore also the relative widths
# have to be included there two times
plotrow

# Add labels with adjusted positions
final_plot <- ggdraw(plotrow) +
  draw_label("Longitude", x = 0.51, y = 0.03, size = 14) +
  draw_label("Latitude", x = 0.03, y = 0.51, angle = 90, size = 14)
final_plot
# we have to adjust the labels again to compensate for the blank space grid, that's we we use the x and y differences , normally 0.5, but adjusting for the 0,1


ggplot2::ggsave(paste0(path, "Maps.jpeg"), 
                plot = final_plot, 
                device = "jpeg",
                dpi = "retina") 


# Bar plot

# Data was extracted from: https://baer-wolf-luchs.at/monitoring/risszahlen
Jahre <- 2009:2024
Nutztierverluste_Wolf <- c(83, 117, 15, 45, 23, 28, 158, 41,22,154,186,330,849,1780,1128,726)
Anzahl_Wölfe <- c(7,7,1,3,5,8,8,15,19,43,50,43,65,80,104,102)
Wolfsdaten <- data.frame(Jahre, Nutztierverluste_Wolf, Anzahl_Wölfe)

# I need a df with the year in one row and the type of incident + number in the other, therefore:
Wolfsdaten_long <- pivot_longer(Wolfsdaten, cols = starts_with(c("Nutztierverluste_Wolf", "Anzahl_Wölfe")), 
                                names_to = "Nachweis", values_to = "Count")

Wolf_BarPlot <- ggplot(data = Wolfsdaten, aes(x = Jahre)) +
  geom_bar(aes(y = Anzahl_Wölfe), stat = "identity",  fill = "#21918c") + 
  geom_line(aes(y = Nutztierverluste_Wolf/10, group = 1), size = 2, colour = "#440154") +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Number of wolves (bars)",
    breaks = c(0, 50, 100, 150),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(
      trans = ~ .*10, 
      name = "Number of livestock losses (line)",
      
    )
  ) +
  theme_minimal() +
  labs(x ="Years") +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.title.y.right = element_text(size = 18),
    axis.text.x = element_text(size = 15),  
    axis.text.y = element_text(size = 15),  
    axis.text.y.right = element_text(size = 18)  
  )

ggplot2::ggsave(paste0(path, "BarPlot.jpeg"), 
                plot = Wolf_BarPlot, 
                device = "jpeg",
                dpi = "retina") 

## END ##