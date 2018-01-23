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
library(rslurm)

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


#### READ OR CREATE DATA INPUT:

# Create a data frame of mean/sd values for normal distributions
pars <- data.frame(par_m = seq(-10, 10, length.out = 1000),
                   par_sd = seq(0.1, 10, length.out = 1000))


#### SOURCE OR CREATE FUNCTION TO RUN:
# Create a function to parallelize
ftest <- function(par_m, par_sd) {
  samp <- rnorm(10^7, par_m, par_sd)
  c(s_m = mean(samp), s_sd = sd(samp))
}


#### TEST 1: SLURM RUN WIHTOUT OPTIONS

sjob1 <- slurm_apply(ftest, pars)
print_job_status(sjob1)

sjob1 <- slurm_apply(ftest, pars)
print_job_status(sjob1)

## Access output
res <- get_slurm_out(sjob1, "table") #this return the outputs recombined as data.frame
all.equal(pars, res) # Confirm correct output
cleanup_files(sjob1) #remove everything

#### TEST 2: SLURM RUN WITH OPTIONS AND USING ARRAY JOB

## send your to multiple nodes:
slurm_apply(f=ftest, 
            params=pars, 
            jobname = "parallel_test", 
            nodes = 3, 
            cpus_per_node = 8,
            add_objects = NULL, 
            pkgs = rev(.packages()), 
            libPaths = NULL,
            slurm_options = list(), 
            submit = TRUE)

#### More options for SLURM
#SBATCH --job-name=bp_workshop_ex2_array_test    # Job name
#SBATCH --mail-type=ALL                          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=bparmentier@sesync.org       # Where to send mail
#SBATCH --time=00:50:00                          # Time limit hrs:min:sec
#SBATCH --output=bp_workshop_ex2_array_test_%A_%a.out   # Standard output from console
#SBATCH --error=bp_workshop_ex2_array_test_%A_%a.err   # Error log 
#SBATCH --partition=sesynctest        # queue name, this is for debugging and testing

### Need to create a list with slurm options:
options_list = list("sesyncshared",             # partition (equivalent to queue) to submit job
                    "ALL",                      # mail-type: Mail events (NONE, BEGIN, END, FAIL, ALL)
                    "bparmentier@sesync.org",   # mail-user: where to send email
                    "00:25:00",                 # Time limit hrs:min:sec
                    "bp_workshop_ex2_array_test_%A_%a.out",
                    "bp_workshop_ex2_array_test_%A_%a.err")
                    
                    ##Now assign option name for slurm job
names(options_list) <- c("partition",
                         "mails-type",
                         "mail-user",
                         "time",
                         "output",
                         "error")

### Now submit job with full information
sjobs2 <- slurm_apply(f=ftest, 
                      params=pars, 
                      jobname = "parallel_test2", 
                      nodes = 3, 
                      cpus_per_node = 8,
                      add_objects = NULL, 
                      pkgs = rev(.packages()), 
                      libPaths = NULL,
                      slurm_options = options_list, 
                      submit = TRUE)

#$bparmentier@sshgw02:~$ squeue
#JOBID PARTITION     NAME     USER ST       TIME  NODES NODELIST(REASON)
#293916_0 sesyncsha parallel bparment  R       0:05      1 pn25
#293916_1 sesyncsha parallel bparment  R       0:05      1 pn27
#293916_2 sesyncsha parallel bparment  R       0:05      1 pn28
#292138 sesyncsha canopy_o jzambran  R 4-01:14:35      1 pn26
#292081 sesyncsha run_seed phmarcha  R 19-10:19:45      1 pn30

################### END OF SCRIPT ##################