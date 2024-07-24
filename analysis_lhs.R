### The following code reproduces the "Latin Hypercube Sampling" analysis with
### second-order meta-models described in the paper "Using design of experiments
### with discrete event simulation in operational research: a review" by Gjerloev et al.


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
analysisID <- c("analysis_lhs")

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


### Setting LHS analysis parameters ######################################################

min_alpha <- 0
max_alpha <- 0.4
centre_alpha <- ( max_alpha + min_alpha ) / 2
halfwd_alpha <- ( max_alpha - min_alpha ) / 2

min_beta <- 0.1
max_beta <- 0.4
centre_beta <- ( max_beta + min_beta ) / 2
halfwd_beta <- ( max_beta - min_beta ) / 2

min_ecu_beds <- pars$capacity_info["ECU","beds"] - 4
max_ecu_beds <- pars$capacity_info["ECU","beds"] + 4
centre_ecu_beds <- ( max_ecu_beds + min_ecu_beds ) / 2
halfwd_ecu_beds <- ( max_ecu_beds - min_ecu_beds ) / 2

min_arr_rate <- 2.4
max_arr_rate <- 8.4
centre_arr_rate <- ( max_arr_rate + min_arr_rate ) / 2
halfwd_arr_rate <- ( max_arr_rate - min_arr_rate ) / 2


# Generating LHS with numbers from uniform distribution
lhs <- randomLHS(81,4)

basic_design <- data.frame(
  alpha = qunif(lhs[,1],min_alpha,max_alpha),
  ecu_beds = round(qunif(lhs[,2],min_ecu_beds,max_ecu_beds),0),
  beta = qunif(lhs[,3],min_beta,max_beta),
  arr_rate = qunif(lhs[,4],min_arr_rate,max_arr_rate)
)
rownames(basic_design) <- paste("comb",1:nrow(basic_design),sep='_')



### Running simulations for LHS analysis ############################################

simulation_results <- lapply(rownames(basic_design),function(s){
  
  # Adapting model parameters to the current scenario
  mod_pars <- pars
  
  mod_pars$id <- s
  mod_pars$alpha <- basic_design[s,"alpha"]
  mod_pars$beta <- basic_design[s,"beta"]
  mod_pars$ecu_beds <- basic_design[s,"ecu_beds"]
  mod_pars$arr_rate <- basic_design[s,"arr_rate"]
  
  mod_pars$patient_info["groupA","traj1"] <- mod_pars$patient_info["groupA","traj1"] + mod_pars$alpha / 4
  mod_pars$patient_info["groupA","traj2"] <- mod_pars$patient_info["groupA","traj2"] - mod_pars$alpha
  mod_pars$patient_info["groupA","traj3"] <- mod_pars$patient_info["groupA","traj3"] + mod_pars$alpha / 4
  mod_pars$patient_info["groupA","traj4"] <- mod_pars$patient_info["groupA","traj4"] + mod_pars$alpha / 4
  mod_pars$patient_info["groupA","traj5"] <- mod_pars$patient_info["groupA","traj5"] + mod_pars$alpha / 4
  
  mod_pars$capacity_info["ECU","beds"] <- mod_pars$ecu_beds
  mod_pars$capacity_info["ICU","beds"] <- tot_beds - mod_pars$ecu_beds
  
  distr <- strsplit(pars$patient_info$daily_arr_rate,split=',')[[1]][1]
  mod_pars$patient_info["groupA","daily_arr_rate"] <- paste(distr,mod_pars$arr_rate,sep=',')
  prop <- mod_pars$beta
  icu_stay <- as.numeric(strsplit(mod_pars$trajectory_info$traj2$groupA[mod_pars$trajectory_info$traj2$Area=="ICU"],split=',')[[1]][2])
  for(t in c("traj3","traj4","traj5")){
    mod_pars$trajectory_info[[t]]$groupA[pars$trajectory_info[[t]]$Area=="ECU"] <- paste("lognorm",icu_stay*prop,sep=',')
    mod_pars$trajectory_info[[t]]$groupA[pars$trajectory_info[[t]]$Area=="ICU"] <- paste("lognorm",icu_stay*(1-prop*sum(pars$trajectory_info[[t]]$Area=="ECU")),sep=',')
  }
  
  # Setting up simulation environment
  sim_obj <- buildSimObjects(mod_pars)
  
  # Running simulation
  sim_res <- runSim(sim_obj,mod_pars,n_runs,until)
  
  # Creating output data
  printOutput(analysisID,sim_res,from,to,mod_pars,save_summary=FALSE)
  
})
names(simulation_results) <- rownames(basic_design)

# Saving raw simulation results
saveRDS(simulation_results,file=paste("data/",analysisID,"/output/simulation_results_lhs.rds",sep=''))

# Uploading raw simulation results - if needed, to avoid recomputing them
#simulation_results <- readRDS(paste("data/",analysisID,"/output/simulation_results_lhs.rds",sep=''))


### Fitting the simulation data with meta-models #################################

# Organising simulation results into a design table used for model fitting

full_design<-Reduce("rbind.data.frame",lapply(simulation_results,function(r){
  tmp <- na.omit(data.frame(
    r$pars$alpha,
    r$pars$ecu_beds,
    r$pars$beta,
    r$pars$arr_rate,
    Reduce("cbind",list(
      r$waiting_time_by_area$ECU * time_transformation, 
      r$waiting_time_by_area$ICU * time_transformation,
      r$overall_waiting_time * time_transformation,
      r$bed_utilisation_by_area$ECU,
      r$bed_utilisation_by_area$ICU,
      r$overall_bed_utilisation,
      r$total_bed_costs$ECU,
      r$total_bed_costs$ICU,
      r$total_bed_costs$ECU + r$total_bed_costs$ICU
    ))
  ))
  colnames(tmp) <- c(colnames(basic_design),c("wait_ECU","wait_ICU","overall_wait","util_ECU","util_ICU","overall_util","costs_ECU","costs_ICU","overall_costs"))
  rownames(tmp) <- paste(r$pars$id,1:nrow(tmp),sep='_')
  tmp
}))
coded_design <- coded.data(
  full_design,
  x1~( alpha - centre_alpha ) / halfwd_alpha,
  x2~( ecu_beds - centre_ecu_beds ) / halfwd_ecu_beds,
  x3~( beta - centre_beta ) / halfwd_beta,
  x4~( arr_rate - centre_arr_rate ) / halfwd_arr_rate
)



## Fitting simulation data with second-order meta-models #####################

wait_ECU <- rsm(wait_ECU ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/wait_ECU_SO_summary.txt",sep=''))
print(summary(wait_ECU))
sink()
png(paste("data/",analysisID,"/output/wait_ECU_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(wait_ECU, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Waiting ECU")
dev.off()

wait_ICU <- rsm(wait_ICU ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/wait_ICU_SO_summary.txt",sep=''))
print(summary(wait_ICU))
sink()
png(paste("data/",analysisID,"/output/wait_ICU_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(wait_ICU, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Waiting ITU")
dev.off()

overall_wait <- rsm(overall_wait ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/overall_wait_SO_summary.txt",sep=''))
print(summary(overall_wait))
sink()
png(paste("data/",analysisID,"/output/overall_wait_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(overall_wait, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Waiting overall")
dev.off()



util_ECU <- rsm(util_ECU ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/util_ECU_SO_summary.txt",sep=''))
print(summary(util_ECU))
sink()
png(paste("data/",analysisID,"/output/util_ECU_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(util_ECU, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Utilisation ECU")
dev.off()

util_ICU <- rsm(util_ICU ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/util_ICU_SO_summary.txt",sep=''))
print(summary(util_ICU))
sink()
png(paste("data/",analysisID,"/output/util_ICU_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(util_ICU, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Utilisation ITU")
dev.off()

overall_util <- rsm(overall_util ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/overall_util_SO_summary.txt",sep=''))
print(summary(overall_util))
sink()
png(paste("data/",analysisID,"/output/overall_util_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(overall_util, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Utilisation overall")
dev.off()



costs_ECU <- rsm(costs_ECU ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/costs_ECU_SO_summary.txt",sep=''))
print(summary(costs_ECU))
sink()
png(paste("data/",analysisID,"/output/costs_ECU_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(costs_ECU, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Costs ECU")
dev.off()

costs_ICU <- rsm(costs_ICU ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/costs_ICU_SO_summary.txt",sep=''))
print(summary(costs_ICU))
sink()
png(paste("data/",analysisID,"/output/costs_ICU_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(costs_ICU, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Costs ITU")
dev.off()

overall_costs <- rsm(overall_costs ~ SO(x1,x2,x3,x4), data = coded_design)
sink(paste("data/",analysisID,"/output/overall_costs_SO_summary.txt",sep=''))
print(summary(overall_costs))
sink()
png(paste("data/",analysisID,"/output/overall_costs_SO.png",sep=''),height=800,width=1000)
par(mfrow = c(2,3), cex=1.5)
contour(overall_costs, ~ x1 + x2 + x3 + x4, image = TRUE, labcex=1.5, main = "Costs overall")
dev.off()



# Applying fitted second-order meta-models to arbitrarily chosen scenarios

alpha_test <- c(0,0.15,0.3)
ecu_beds_test <- pars$capacity_info["ECU","beds"] + seq(-4,4,1)
arr_rate_test <- seq(2.4,8.4,0.1)

wait_model <- lapply(alpha_test,function(a){
  tmp <- matrix(0,length(ecu_beds_test),length(arr_rate_test),dimnames=list(ecu_beds_test,arr_rate_test))
  for(bed_v in ecu_beds_test){
    for(arr_v in arr_rate_test){
      tmp[as.character(bed_v),as.character(arr_v)] <- max(0,predict.lm(overall_wait,data.frame(
        x1 = (a-centre_alpha)/halfwd_alpha,
        x2 = (bed_v-centre_ecu_beds)/halfwd_ecu_beds,
        x3 = 0,
        x4 = (arr_v-centre_arr_rate)/halfwd_arr_rate
      )))
    }
  }
  tmp
})
names(wait_model) <- alpha_test



# Plotting heatmaps of the above model predictions

lev_wait <- c(0,0.5,1,1.5,2,2.5,3,6,12,24,36,48,60,72,1000)
col_wait <- c(colorRampPalette(c("#32CD32", "#A3A3A3"))(length(lev_wait)))
for(i in names(wait_model)){
  png(paste("data/",analysisID,"/output/pred_wait_alpha_",i,".png",sep=''),height=1000,width=1000,pointsize=36)
  filled.contour(ecu_beds_test,arr_rate_test,wait_model[[i]], levels=lev_wait,col=col_wait, plot.axes = {
    axis(1);
    axis(2);
    contour(ecu_beds_test,arr_rate_test,wait_model[[i]],levels=lev_wait, add = TRUE, lwd = 1)
  }, color.palette = col_wait, xlab="ECU beds", ylab="arr_rate", main=paste("Wait - alpha = ",i,sep=''))
  dev.off()
}


