
buildSimObjects <- function(pars){
  
  require(simmer)
  
  nodes <- rownames(pars$capacity_info)
  resources <- paste(nodes,colnames(pars$capacity_info)[1],sep='_')
  names(resources) <- nodes
  capacities <- pars$capacity_info$beds
  names(capacities) <- nodes
  
  trajectories <- lapply(names(pars$trajectory_info),function(z){
    tmp1 <- lapply(pars$groupID,function(t){
      traj <- trajectory(paste(z,t,'',sep='_'))
      steps <- pars$trajectory_info[[z]][,1]
      distrib <- sapply(pars$trajectory_info[[z]][[t]],function(x){strsplit(x,split=',')[[1]][1]})
      param1 <- as.numeric(sapply(pars$trajectory_info[[z]][[t]],function(x){strsplit(x,split=',')[[1]][2]}))
      param2 <- as.numeric(sapply(pars$trajectory_info[[z]][[t]],function(x){strsplit(x,split=',')[[1]][3]}))
      f <- lapply(1:length(steps),function(i){function(d,p1,p2){
        d <- distrib[i]
        p1 <- param1[i]
        p2 <- param2[i]
        if(d=="exp"){
          rexp(1,1/p1)
        }else if(d=="det"){
          p1
        }else if(d=="unif"){
          runif(1,p1,p2)
        }else if(d=="lognorm"){
          if(p1>0 & p2>0){
            rlnorm(1,log(p1/sqrt(1+p2^2/p1^2)),sqrt(log(1+p2^2/p1^2)))
          }else{
            0
          }
        }else{
          stop("Error in the length-of-stay input - wrong distribution name used")
        }
      }})
    
      for(i in 1:length(steps)){
        traj <- seize(traj,resources[steps[i]],1)
        traj <- timeout(traj,f[[i]])
        traj <- release(traj,resources[steps[i]],1)
      }
      traj
    })
    tmp1
  })
  trajectories <- Reduce("c",trajectories)
  names(trajectories) <- sapply(trajectories,function(x){x$name})
  
  return(list(nodes=nodes,res=resources,cap=capacities,traj=trajectories))
  
}



