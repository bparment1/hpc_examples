####################################    Fire Alaska Analyses   #######################################
############################  Analyse using time series and fire locations  #######################################
#This script performs analyzes for the Exercise 2 of the workshop using NDVI data derived from MODIS.
#The overall goal is to explore the impact of fire on surface state variables observed from Remote Sensing.
#Additional data is provided with location of three fire polygons containing pixels affected by fire. 
#
#AUTHORS: Benoit Parmentier                                             
#DATE CREATED: 03/15/2017 
#DATE MODIFIED: 06/08/2017
#Version: 1
#PROJECT: AAG 2017
#TO DO:
#
#COMMIT: modifying script to run on SESYNC HPC for testing and learning purpose
#
#Links to investigate: 
#https://stackoverflow.com/questions/29784829/r-raster-package-split-image-into-multiples
#

#################################################################################################

###Loading R library and packages                                                      

library(sp) # spatial/geographfic objects and functions
library(rgdal) #GDAL/OGR binding for R with functionalities
library(spdep) #spatial analyses operations, functions etc.
library(gtools) # contains mixsort and other useful functions
library(maptools) # tools to manipulate spatial data
library(parallel) # parallel computation, part of base package no
library(rasterVis) # raster visualization operations
library(raster) # raster functionalities
library(forecast) #ARIMA forecasting
library(xts) #extension for time series object and analyses
library(zoo) # time series object and analysis
library(lubridate) # dates functionality
library(colorRamps) #contains matlab.like color palette
library(rgeos) #contains topological operations
library(sphet) #contains spreg, spatial regression modeling
library(BMS) #contains hex2bin and bin2hex, Bayesian methods
library(bitops) # function for bitwise operations
library(foreign) # import datasets from SAS, spss, stata and other sources
library(gdata) #read xls, dbf etc., not recently updated but useful
library(classInt) #methods to generate class limits
library(plyr) #data wrangling: various operations for splitting, combining data
library(gstat) #spatial interpolation and kriging methods
library(readxl) #functionalities to read in excel type data
library(psych) #pca/eigenvector decomposition functionalities

###### Functions used in this script

function_preprocessing_and_analyses <- "fire_alaska_analyses_preprocessing_functions_03102017.R" #PARAM 1
function_analyses <- "exercise2_fire_alaska_analyses_functions_03232017.R" #PARAM 1
function_change_analysis <- "exercise2_change_analyses_functions_04082017b.R"
script_path <- "/nfs/bparmentier-data/Data/slurm_test/scripts"
source(file.path(script_path,function_preprocessing_and_analyses)) #source all functions used in this script 1.
source(file.path(script_path,function_analyses)) #source all functions used in this script 1.
source(file.path(script_path,function_change_analysis))
       
#####  Parameters and argument set up ###########

#ARGS 1
in_dir_NDVI <- "/research-home/bparmentier/Data/slurm_test/Exercise_2/data/NDVI_alaska_2005"
#ARGS 2
in_dir_var <- "/research-home/bparmentier/Data/slurm_test/Exercise_2/data"
#ARGS 3
out_dir <- "/research-home/bparmentier/Data/slurm_test/Exercise_2/outputs"

#ARGS 4
infile_ecoreg <- "wwf_terr_ecos_Alaska_ECOREGIONS_ECOSYS_ALB83.shp" #WWF ecoregions 2001 for Alaska
#ARGS 5
fire_poly_shp_fname <- "OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_reclassed.shp"

#ARGS 6
NA_value <- -9999 #PARAM6
#ARGS 7
out_suffix <-"slurm_test_alaska_06082017" #output suffix for the files and ouptu folder #PARAM 8
#ARGS 8
create_out_dir_param=TRUE #PARAM9
#ARGS 9
date_range <- c("2005.01.01","2005.12.31") #NDVI Alaska, year 2005 (this is a 16 days product)

#ARGS 10:

#ARGS 11: TILE INDEX this is from the array

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid <- Sys.getenv('SLURM_ARRAYID')
# coerce the value to an integer
tile_index <- as.numeric(slurm_arrayid)
#tile_index <- 1  for testing

###

## not set in the inputs args
#region coordinate reference system
CRS_reg <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
file_format <- ".tif" #PARAM5
NA_flag_val <- NA_value #PARAM7

################# START SCRIPT ###############################

### PART I: READ AND PREPARE DATA FOR ANALYSES #######

#get tile ID

out_suffix <- paste("tile_",tile_index,out_suffix)
## First create an output directory

if(is.null(out_dir)){
  out_dir <- dirname(in_dir) #output will be created in the input dir
}

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix_s)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}


##### PART I: DISPLAY AND EXPLORE DATA ##############

## Second list.files and create raster images stack

lf_NDVI <- list.files(path=in_dir_NDVI, pattern="*.tif",full.names=T)
r_NDVI_ts <- stack(lf_NDVI)

plot(r_NDVI_ts)
#date_range <- c("2005.01.01","2005.12.31") #NDVI Alaska, year 2005 (this is a 16 days product)
  
#generate dates for 16 days product
dates_val <- generate_dates_by_step(date_range[1],date_range[2],16)$dates #NDVI Katrina

file.info(lf_NDVI[1])$size/(1024*1024) # this is in bytes, convert to mb
dataType(r_NDVI_ts) #Examine the data type used in the storing of data, this is float 32 signed: FLT4S
inMemory(r_NDVI_ts) #Is the data in memory? Raster package does not load in memory automatically.
dim(r_NDVI_ts) #dimension of the raster object: rows, cols, layers/bands

lf_var <- list.files(path=in_dir_var,pattern="*.tif$",full.names=T)
r_var <- stack(lf_var) # create a raster stack, this is not directly stored in memory
dim(r_var) #dimension of the stack with 
plot(r_var)

##### PART II: CROP TO PROCESSING AREA ##############

### Crop using the new tile ####
crop_file_name <- paste("ndvi_","tile_",tile_index,".tif",sep="")
r_NDVI_ts <- crop(r_NDVI_ts,tile_spdf,crop_file_name,overwrite=T)

crop_r_var_file_name <- paste("r_var_","tile_",tile_index,".tif")
r_var <- crop(r_var,tile_spdf,crop_r_var_file_name,overwrite=T,sep="")

######### PART III: time series analyses #################

# Perform PCA


# Using LST time series perform similar analysis
#r_NDVI_mean <- stackApply(r_NDVI_ts, indices=rep(1,23), fun=mean,na.rm=T) # works too but slower
r_NDVI_mean <- mean(r_NDVI_ts, na.rm=TRUE) # mean by pixel
projection(r_NDVI_ts) <- CRS_reg

mean_fire_NDVI_ts_tb <- zonal(stack(r_NDVI_ts),r_fire_poly,fun="mean") #mean square error
mean_fire_NDVI_ts_df <- as.data.frame(t(mean_fire_NDVI_ts_tb[-1,-1]))
names(mean_fire_NDVI_ts_df) <- c("fire_pol1","fire_pol2","fire_pol3")

## Make a time series object

###############################################
######## Let's carry out a PCA in T-mode #######

#Correlate long term mean to PC!
cor_mat_layerstats <- layerStats(r_NDVI_ts, 'pearson', na.rm=T)
cor_matrix <- cor_mat_layerstats$`pearson correlation coefficient`
class(cor_matrix)
dim(cor_matrix)
View(cor_matrix)
image(cor_matrix)

pca_mod <-principal(cor_matrix,nfactors=3,rotate="none")
class(pca_mod$loadings)
str(pca_mod$loadings)
plot(pca_mod$loadings[,1],type="b",
     xlab="time steps",
     ylab="PC loadings",
     ylim=c(-1,1),
     col="blue")
lines(pca_mod$loadings[,2],type="b",col="red")
lines(pca_mod$loadings[,3],type="b",col="black")
title("Loadings for the first three components using T-mode")

##Make this a time series
loadings_df <- as.data.frame(pca_mod$loadings[,1:3])
pca_loadings_dz <- zoo(loadings_df,dates_val) #create zoo object from data.frame and date sequence object
#?plot.zoo to find out about zoo time series plotting of indexes
plot(pca_loadings_dz,
     type="b",
     plot.type="single",
     col=c("blue","red","black"),
     xlab="time steps",
     ylab="PC loadings",
     ylim=c(-1,1))
title("Loadings for the first three components using T-mode")
names_vals <- c("pc1","pc2","pc3")
legend("topright",legend=names_vals,
       pt.cex=0.8,cex=1.1,col=c("blue","red","black"),
       lty=c(1,1), # set legend symbol as lines
       pch=1, #add circle symbol to line
       lwd=c(1,1),bty="n")

## Add scree plot
plot(pca_mod$values,main="Scree plot: Variance explained",type="b")

### Generate scores from eigenvectors
## Do it two different ways:

#By converting data and working with matrix:
df_NDVI_ts <- as(r_NDVI_ts,"SpatialPointsDataFrame")
df_NDVI_ts <- as.data.frame(df_NDVI_ts) #convert to data.frame because predict works on data.frame
names(df_NDVI_ts) <- c(paste0("pc_",seq(1,23,1)),"x","y")
names(df_NDVI_ts)

pca_all <- as.data.frame(predict(pca_mod,df_NDVI_ts[,1:23])) ## Apply model object on the data.frame
#pca_all <-predict(pca_mod,df_NDVI_ts) ## Apply model object on the data.frame

coordinates(pca_all) <- df_NDVI_ts[,c("x","y")] #Assign coordinates
class(pca_all) #Check type of class

raster_name <- paste0("pc1_NDVI_2005_",out_suffix,".tif") #output raster name for component 1
r_pc1<-rasterize(pca_all,r_NDVI_ts,"PC1",fun=min,overwrite=TRUE,
                  filename=raster_name)
raster_name <- paste0("pc2_NDVI_2005.tif") #output raster name for component 2
r_pc2<-rasterize(pca_all,r_NDVI_ts,"PC2",fun=min,overwrite=TRUE,
                 filename=raster_name)

### Using predict function: this is recommended for raster imagery!!
# note the use of the 'index' argument
r_pca <- predict(r_NDVI_ts, pca_mod, index=1:3,filename="pc_scores.tif",overwrite=T) # fast
plot(r_pca)

plot(stack(r_pc1,r_pc2))
#layerStats(r_pc1,r_NDVI_mean )
cor_pc <- layerStats(stack(r_pc1,r_NDVI_mean),'pearson', na.rm=T)
cor_pc #PC1 correspond to the average mean by pixel as expected.
plot(r_pc2)

################### End of Script #########################
