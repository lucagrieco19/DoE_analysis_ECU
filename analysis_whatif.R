### The following code reproduces the "What-if" analysis described in the paper
### "Using design of experiments with discrete event simulation in operational
### research: a review" by Gjerloev et al.


### Initialisation ###############################################################


# Uploading required R libraries
library(xlsx)
library(rsm)
library(lhs)

# Loading R functions implementing the DES model and I/O processing
source("R_functions/readInput.R")
source("R_functions/buildSimObjects.R")
source("R_functions/runSim.R")
source("R_functions/printOutput.R")

# Set of IDs of analyses that we want to run - these are the names of the
# corresponding folders in the folder "data"
analysisID <- c("analysis_whatif")

# Number of envisaged simulation runs
n_runs <- 100

# Time limit for simulation (e.g. it will stop at day 100)
until <- 100

# Time range from which results are computed (earlier than that it is warm-up period)
from <- 10
to <- 40

# Read default model parameters from files
pars <- readInput(analysisID)

# Variable keeping track of the total number of beds as in the analysis we keep this fixed
tot_beds <- pars$capacity_info["ECU","beds"] + pars$capacity_info["ICU","beds"]

# Multiplier used below to show results in hours (whereas input is in days)
time_transformation <- 24


### Setting what-if analysis parameters ##############################################
alpha_values <- seq(0,0.4,0.05)
ecu_beds_values <- pars$capacity_info["ECU","beds"] + seq(-4,4,1)
basic_design <- data.frame(
  alpha = rep(alpha_values,each=length(ecu_beds_values)),
  ecu_beds = rep(ecu_beds_values,length(alpha_values))
)
rownames(basic_design) <- paste("scen",basic_design$alpha,basic_design$ecu_beds,sep='_')



### Running simulations for What-if analysis ##########################################

simulation_results <- lapply(rownames(basic_design),function(s){
  
  # Adapting model parameters to the current scenario
  mod_pars <- pars
  
  mod_pars$id <- s
  mod_pars$alpha <- basic_design[s,"alpha"]
  mod_pars$ecu_beds <- basic_design[s,"ecu_beds"]
  
  mod_pars$patient_info["groupA","traj1"] <- mod_pars$patient_info["groupA","traj1"] + mod_pars$alpha / 4
  mod_pars$patient_info["groupA","traj2"] <- mod_pars$patient_info["groupA","traj2"] - mod_pars$alpha
  mod_pars$patient_info["groupA","traj3"] <- mod_pars$patient_info["groupA","traj3"] + mod_pars$alpha / 4
  mod_pars$patient_info["groupA","traj4"] <- mod_pars$patient_info["groupA","traj4"] + mod_pars$alpha / 4
  mod_pars$patient_info["groupA","traj5"] <- mod_pars$patient_info["groupA","traj5"] + mod_pars$alpha / 4
  
  mod_pars$capacity_info["ECU","beds"] <- mod_pars$ecu_beds
  mod_pars$capacity_info["ICU","beds"] <- tot_beds - mod_pars$ecu_beds
  
  # Seting up simulation environment
  sim_obj <- buildSimObjects(mod_pars)
  
  # Running simulation
  sim_res <- runSim(sim_obj,mod_pars,n_runs,until)
  
  # Creating output data
  printOutput(analysisID,sim_res,from,to,mod_pars,save_summary=FALSE)
  
})
names(simulation_results) <- rownames(basic_design)

# Saving raw simulation results
saveRDS(simulation_results,file=paste("data/",analysisID,"/output/simulation_results_whatif.rds",sep=''))

# Uploading raw simulation results - if needed, to avoid recomputing them
#simulation_results <- readRDS(paste("data/",analysisID,"/output/simulation_results_whatif.rds",sep=''))



### Extracting What-if analysis summaries ######################################

# Building result tables containing summaries from simulation runs

summ <- vector('list',length=0)

for(o in c("bed_utilisation_by_area","waiting_time_by_area","total_bed_costs")){

  for(a in c("ICU","ECU")){
    
    tmp <- matrix(0,length(alpha_values),length(ecu_beds_values),dimnames=list(alpha_values,ecu_beds_values))
    
    for(av in rownames(tmp)){
      for(bv in colnames(tmp)){
        scenID <- paste("scen",av,bv,sep='_')
        tmp[av,bv] <- round(mean(simulation_results[[scenID]][[o]][[a]]),2)
      }
    }

    summ[[paste(o,a,sep='_')]] <- tmp
    
  }
  
}

tmp <- matrix(0,length(alpha_values),length(ecu_beds_values),dimnames=list(alpha_values,ecu_beds_values))
for(av in rownames(tmp)){
  for(bv in colnames(tmp)){
    scenID <- paste("scen",av,bv,sep='_')
    tmp[av,bv] <- round(mean(simulation_results[[scenID]][["overall_waiting_time"]]),2)
  }
}
summ[["overall_waiting_time"]] <- tmp

tmp <- matrix(0,length(alpha_values),length(ecu_beds_values),dimnames=list(alpha_values,ecu_beds_values))
for(av in rownames(tmp)){
  for(bv in colnames(tmp)){
    scenID <- paste("scen",av,bv,sep='_')
    tmp[av,bv] <- round(mean(simulation_results[[scenID]][["overall_bed_utilisation"]]),2)
  }
}
summ[["overall_bed_utilisation"]] <- tmp

summ[["total_bed_costs"]] <- summ$total_bed_costs_ICU + summ$total_bed_costs_ECU

summ[["overall_waiting_time"]] <- summ[["overall_waiting_time"]] * time_transformation
summ[["waiting_time_by_area_ECU"]] <- summ[["waiting_time_by_area_ECU"]] * time_transformation
summ[["waiting_time_by_area_ICU"]] <- summ[["waiting_time_by_area_ICU"]] * time_transformation



# Creating a spreadsheet containing output summaries from simulations

write.xlsx(summ[[1]],file=paste("data/",analysisID,"/output/summ_whatif.xlsx",sep=''),sheetName=names(summ)[1],append=FALSE)
for(i in 2:length(summ)){
  write.xlsx(summ[[i]],file=paste("data/",analysisID,"/output/summ_whatif.xlsx",sep=''),sheetName=names(summ)[i],append=TRUE)
}



# Plotting heatmaps for the output metrics defined above

lev_wait <- c(0,0.5,1,1.5,2,2.5,3,6,12,24,36,48,60,72,1000)
col_wait <- c(colorRampPalette(c("#32CD32", "#A3A3A3"))(length(lev_wait)))
for(i in grep("waiting_time",names(summ))){
  png(paste("data/",analysisID,"/output/",names(summ)[i],".png",sep=''),height=1000,width=1000,pointsize=36)
  filled.contour(alpha_values,ecu_beds_values,summ[[i]], levels=lev_wait,col=col_wait, plot.axes = {
    axis(1);
    axis(2);
    contour(alpha_values,ecu_beds_values,summ[[i]],levels=lev_wait, add = TRUE, lwd = 1)
  }, color.palette = col_wait, xlab="alpha", ylab="ECU beds", main=names(summ)[i])
  dev.off()
}

lev_util <- seq(0,1,0.05)
col_util <- c(colorRampPalette(c("#32CD32", "#A3A3A3"))(length(lev_util)))
for(i in grep("bed_utilisation",names(summ))){
  png(paste("data/",analysisID,"/output/",names(summ)[i],".png",sep=''),height=1000,width=1000,pointsize=36)
  filled.contour(alpha_values,ecu_beds_values,summ[[i]], levels=lev_util,col=col_util, plot.axes = {
    axis(1);
    axis(2);
    contour(alpha_values,ecu_beds_values,summ[[i]],levels=lev_util, add = TRUE, lwd = 1)
  }, color.palette = col_util, xlab="alpha", ylab="ECU beds", main=names(summ)[i])
  dev.off()
}

lev_cost <- seq(0,60,2)
col_cost <- c(colorRampPalette(c("#32CD32", "#A3A3A3"))(length(lev_cost)))
for(i in grep("bed_costs",names(summ))){
  png(paste("data/",analysisID,"/output/",names(summ)[i],".png",sep=''),height=1000,width=1000,pointsize=36)
  filled.contour(alpha_values,ecu_beds_values,summ[[i]], levels=lev_cost,col=col_cost, plot.axes = {
    axis(1);
    axis(2);
    contour(alpha_values,ecu_beds_values,summ[[i]],levels=lev_cost, add = TRUE, lwd = 1)
  }, color.palette = col_cost, xlab="alpha", ylab="ECU beds", main=names(summ)[i])
  dev.off()
}

