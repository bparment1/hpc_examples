####################################    Using rslurm example   #######################################
############################  Analyse using time series and fire locations  #######################################
#This script experiment with rslurm package and explores its various options
#AUTHORS: Benoit Parmentier                                             
#DATE CREATED: 01/22/2017 
#DATE MODIFIED: 01/22/2017
#Version: 1
#PROJECT: PEATBOGS 
#TO DO:
#
#COMMIT: running rslurm with example data
#
#################################################################################################

###Loading R library and packages                                                      

library(parallel)

##### Functions used in this script 

create_dir_fun <- function(out_dir,out_suffix){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    out_dir <- file.path(out_dir,out_name)
  }
  #create if does not exists
  if(!file.exists(out_dir)){
    dir.create(out_dir)
  }
  return(out_dir)
}

load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#####  Parameters and argument set up ###########

#ARGS 1
in_dir <- "/nfs/bparmentier-data/Data/SLURM_test/rslurm_test/data"
#ARGS 2
out_dir <- "/nfs/bparmentier-data/Data/SLURM_test/rslurm_test//outputs"
#ARGS 2
out_suffix <-"rslurm_test_01222018" #output suffix for the files and ouptu folder #PARAM 8
#ARGS 3
create_out_dir_param=TRUE #PARAM9
#ARGS 4
num_cores <- 2 # set to two for now

################# START SCRIPT ###############################

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

infile_name_example <- "surveys.csv"
download.file('https://github.com/datacarpentry/datacarpentry/raw/master/data/biology/surveys.csv',
              file.path(out_dir,infile_name_example),'wget')


### Select the relevant tile to process
surveys <- read.table(file.path(out_dir,infile_name_example),
                        header=T,
                        fill=TRUE,
                        stringsAsFactors = F,
                        sep=",")

View(surveys)

#surveys <- read.csv('surveys.csv')
surveys_complete <- surveys[complete.cases(surveys), ]

surveys_complete$species <- factor(surveys_complete$species)
species_mean <- tapply(surveys_complete$wgt, surveys_complete$species, mean)
species_max <- tapply(surveys_complete$wgt, surveys_complete$species, max)
species_min <- tapply(surveys_complete$wgt, surveys_complete$species, min)
species_sd <- tapply(surveys_complete$wgt, surveys_complete$species, sd)
nlevels(surveys_complete$species) # or length(species_mean)
surveys_summary <- data.frame(species=levels(surveys_complete$species),
                              mean_wgt=species_mean,
                              sd_wgt=species_sd,
                              min_wgt=species_min,
                              max_wgt=species_max)
pdf("mean_per_species.pdf")
barplot(surveys_summary$mean_wgt)
dev.off()

################### END OF SCRIPT ##################