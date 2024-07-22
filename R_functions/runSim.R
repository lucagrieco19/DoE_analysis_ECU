
runSim <- function(sim_obj,pars,n_runs,until){
  
  require(simmer)
  
  nodes <- sim_obj$nodes
  resources <- sim_obj$res
  capacities <- sim_obj$cap
  trajectories <- sim_obj$traj
  
  res <- lapply(1:n_runs, function(i) {
    
    env <- simmer("ECU_model")
    
    for(n in nodes){
      env <- add_resource(env,name=resources[n],capacity=capacities[n])
    }
    
    gr <- sapply(names(trajectories),function(t){strsplit(trajectories[[t]]$name,split='_')[[1]][2]})
    path <- sapply(names(trajectories),function(t){strsplit(trajectories[[t]]$name,split='_')[[1]][1]})
    distr <- sapply(names(trajectories),function(t){strsplit(pars$patient_info[gr[t],"daily_arr_rate"],split=',')[[1]][1]})
    rate <- sapply(names(trajectories),function(t){as.numeric(strsplit(pars$patient_info[gr[t],"daily_arr_rate"],split=',')[[1]][2]) * pars$patient_info[gr[t],path[t]]})
    func <- lapply(names(trajectories),function(t){function(){
      d <- distr[t]
      r <- rate[t]
      if(d=="poiss"){
        rexp(1,r)
      }else if(d=="det"){
        1/r
      }else{
        stop("Error in the arrival input - wrong distribution name used")
      }
    }})
    names(func) <- names(trajectories)
    
    for(t in names(trajectories)){
      if(rate[t]>0){
        env <- add_generator(env,trajectories[[t]]$name,trajectories[[t]],func[[t]],priority=pars$patient_info[gr[t],"priority"])
      }
    }
    
    run(env,until=until)
    
  })
  
  return(res)
  
}



