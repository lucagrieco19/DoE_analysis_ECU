
printOutput <- function(analysisID,sim_res,from,to,pars,save_summary){
  
  require(simmer.plot)
  
  arr <- get_mon_arrivals(sim_res,per_resource=TRUE)
  
  summ <- get_mon_resources(sim_res)
  
  
#  set.seed(1)
  
#  items <- unique(sapply(summ$resource,function(r){strsplit(r,split='_')[[1]][1]}))
#  items <- items[order(items,decreasing=TRUE)]
  
#  col <- sample(colors(),size=length(unique(summ$resource)))
#  names(col) <- items
  
  wait <- round(arr$end_time - arr$start_time - arr$activity,2)
  arr$wait <- wait
  
  
  raw_output <- list(
    analysisID = analysisID,
    monitored_arrivals = arr,
    monitored_resources=summ,
    pars = pars
  )
  
  
  arr <- arr[round(arr$start_time,0)>=from & round(arr$start_time,0)<=to,]
  summ <- summ[round(summ$time,0)>=from & round(summ$time,0)<=to,]
  
  
#### Quality of service ###################################
  
  overall_qual <- sapply(1:max(arr$replication),function(r){
    sum(arr$wait[arr$replication==r])
  })
  
  n_patients <- sapply(1:max(arr$replication),function(r){
    length(unique(arr$name[arr$replication==r]))
  })
  
  overall_wait <- overall_qual / n_patients
  
  qual_by_area <- lapply(unique(summ$resource),function(n){
    sapply(1:max(arr$replication),function(r){
      sum(arr$wait[arr$resource==n & arr$replication==r])
    })
  })
  names(qual_by_area) <- sapply(unique(summ$resource),function(n){strsplit(n,split='_')[[1]][1]})
  
  n_pat_by_area <- lapply(unique(summ$resource),function(n){
    sapply(1:max(arr$replication),function(r){
      length(arr$name[arr$resource==n & arr$replication==r])
    })
  })
  names(n_pat_by_area) <- sapply(unique(summ$resource),function(n){strsplit(n,split='_')[[1]][1]})
  
  wait_by_area <- lapply(names(qual_by_area),function(a){
    qual_by_area[[a]] / n_pat_by_area[[a]]
  })
  names(wait_by_area) <- names(qual_by_area)
  
  
  
###########################################################
  
  
#  groups <- unique(sapply(arr$name,function(s){strsplit(s,split='_')[[1]][2]}))
#  groups <- c(groups[order(groups)],"overall")
  
#  d <- Reduce('rbind.data.frame',lapply(groups,function(g){
    
#    s <- g
#    if(g=="overall") s <- '_'
    
#    na.omit(data.frame(group=g,wait=sapply(1:n_runs,function(r){
#      ind <- intersect(which(arr$replication==r),grep(s,arr$name))
#      mean(wait[ind]) * 24
#    })))
    
#  }))

#  p <- ggplot(d, aes(x=group, y=wait)) +
#    geom_boxplot() +
#    ylab("Waiting time (hours)") +
#    xlab("Patient group") +
#    ggtitle("Time a patient spends waiting") +
#    theme(axis.text=element_text(size=14), axis.title=element_text(size=16), title=element_text(size=14)) +
#    #ylim(c(0,max(d$wait)))
#    ylim(c(0,50))
#  ggsave(filename=paste("data/",analysisID,"/output/AvgWait.png",sep=''), plot=p)
  

  
#### Resource utilisation ################################################  

  util <- lapply(unique(summ$resource),function(n){
    
    time_in_use <- sapply(1:max(summ$replication),function(r){
      
      tmp <- summ[summ$resource==n & summ$replication==r,]
      tmp <- tmp[order(tmp$time),]
      
      if(nrow(tmp)>1){
      
        t_start <- tmp$time[1:(nrow(tmp)-1)]
        t_end <- tmp$time[2:nrow(tmp)]
        occ <- tmp$server[1:(nrow(tmp)-1)]
      
        res <- sum((t_end - t_start) * occ)
        
      }else if(nrow(tmp)==1){
        
        res <- (max(0,to - tmp$time[1])) * tmp$server[1]
        
      }else{
        
        res <- 0
        
      }
      
      res
      
    })
    
    sim_time <- sapply(1:max(summ$replication),function(r){
      
      tmp <- summ[summ$resource==n & summ$replication==r,]
      tmp <- tmp[order(tmp$time),]
      
      if(nrow(tmp)>1){
        
        t_start <- tmp$time[1:(nrow(tmp)-1)]
        t_end <- tmp$time[2:nrow(tmp)]
        cap <- tmp$capacity[1:(nrow(tmp)-1)]
      
        res <- sum((t_end - t_start) * cap)
        
      }else if(nrow(tmp)==1){
        
        res <- (to - from) * tmp$capacity[1]
        
      }else{
        
        res <- 0
        
      }
      
      res
      
    })
    
    list(time_in_use=time_in_use,sim_time=sim_time)
    
  })
  names(util) <- sapply(unique(summ$resource),function(n){strsplit(n,split='_')[[1]][1]})
  
  util_by_area <- lapply(names(util),function(a){
    util[[a]]$time_in_use / util[[a]]$sim_time
  })
  names(util_by_area) <- names(util)
  
  overall_util <- sapply(1:max(summ$replication),function(r){
    sum(sapply(names(util),function(a){
      util[[a]]$time_in_use[r]
    })) / sum(sapply(names(util),function(a){
      util[[a]]$sim_time[r]
    }))
  })

  
  
  
  
##############################################################
  
  
#### Staff needs #############################################

  total_staff_days <- lapply(unique(summ$resource),function(n){
    
    sapply(1:max(summ$replication),function(r){
      
      tmp <- summ[summ$resource==n & summ$replication==r,]
      tmp <- tmp[order(tmp$time),]
      
      area <- strsplit(n,split='_')[[1]][1]
      staff_per_patient <- 1 / pars$staff_info[area,"patients_per_staff"]
      
      if(nrow(tmp)>1){
      
        t_start <- tmp$time[1:(nrow(tmp)-1)]
        t_end <- tmp$time[2:nrow(tmp)]
      
        n_patients <- tmp$server[1:(nrow(tmp)-1)]
      
        res <- sum((t_end - t_start) * n_patients * staff_per_patient)
        
      }else if(nrow(tmp)==1){
        
        res <- sum(max(0,to - tmp$time[1]) * tmp$server[1] * staff_per_patient)
        
      }else{
        
        res <- 0
        
      }
      
      res
      
    })
    
  })
  names(total_staff_days) <- sapply(unique(summ$resource),function(n){strsplit(n,split='_')[[1]][1]})
  
  
  
##############################################################
  
  
#### Monetary costs per day ##################################
  
  staff_costs <- lapply(names(total_staff_days),function(a){
    total_staff_days[[a]] * pars$staff_info[a,"staff_daily_salary"] / (to - from + 1)
  })
  names(staff_costs) <- names(total_staff_days)
  
  bed_costs <- lapply(names(util),function(a){
    util[[a]]$time_in_use * pars$capacity_info[a,"bed_day_cost"] / (to - from + 1)
  })
  names(bed_costs) <- names(util)
  
  
##############################################################
  

  
  
  
  
  
  
#  p <- ggplot(util, aes(x=area, y=utilisation, fill=area)) +
#    geom_boxplot(show.legend=FALSE) +
#    ylab("Ratio Time in use to Simulation time") +
#    xlab("Hospital area") +
#    ggtitle("Average bed utilisation") +
#    theme(axis.text=element_text(size=14), axis.title=element_text(size=16), title=element_text(size=14)) +
#    scale_fill_manual(values=col) +
#    ylim(c(0,1))
#  ggsave(filename=paste("data/",analysisID,"/output/AvgUtilisation.png",sep=''), plot=p)
  
  
#  summ2 <- aggregate(summ$server,by=list(summ$resource,round(summ$time,0)),FUN=mean)
#  summ2 <- cbind(summ2,aggregate(summ$server,by=list(summ$resource,round(summ$time,0)),FUN=sd)[,3]/sqrt(length(unique(summ$replication))))
#  summ2 <- cbind(summ2,aggregate(summ$capacity,by=list(summ$resource,round(summ$time,0)),FUN=mean)[,3])
#  colnames(summ2) <- c("resource","day","avg_occ","err_occ","capacity")
#  summ2$resource <- sapply(summ2$resource,function(r){strsplit(r,split='_')[[1]][1]})
  
#  for(r in names(col)){
#    data <- summ2[summ2$resource==r,]
#    data$day <- data$day - from
#    p <- ggplot(data, aes(x = as.factor(day), y = avg_occ, fill = resource)) + 
#      geom_bar(position=position_dodge(), stat="identity", width=0.8, show.legend = FALSE) +
#      geom_errorbar(aes(ymin=sapply(avg_occ-err_occ,function(s){max(0,s)}), ymax=avg_occ+err_occ), width=0.2, position=position_dodge(0.8), show.legend = FALSE) +
#      geom_line(aes(y=capacity,group=1)) +
#      xlab("Day") +
#      ylab("") +
#      ggtitle(paste(r," beds occupied daily",sep='')) +
#      theme(axis.text.x=element_text(size=6), axis.text.y=element_text(size=10), axis.title=element_text(size=16), title=element_text(size=14)) +
#      scale_fill_manual(values=col[r]) +
#      #ylim(c(0,max(summ2$capacity)))
#      ylim(c(0,50))
#    ggsave(filename=paste("data/",analysisID,"/output/",r,"_OccupancyOverTime.png",sep=''), plot=p)
#  }
  
#  summ <- cbind(summ,staff=summ$server * sapply(1:nrow(summ),function(i){
#    a <- strsplit(summ$resource[i],split='_')[[1]][1]
#    1 / pars$staff_info[a,"patients_per_staff"]
#  }))
#  summ3 <- aggregate(summ$staff,by=list(summ$resource,round(summ$time,0)),FUN=mean)
#  summ3 <- cbind(summ3,aggregate(summ$staff,by=list(summ$resource,round(summ$time,0)),FUN=sd)[,3]/sqrt(length(unique(summ$replication))))
#  colnames(summ3) <- c("resource","day","avg_staff","err_staff")
#  summ3$resource <- sapply(summ3$resource,function(r){strsplit(r,split='_')[[1]][1]})
#  
#  data <- summ3
#  data$day <- data$day - from
#  p <- ggplot(data, aes(x = as.factor(day), y = avg_staff, fill = resource)) + 
#    geom_bar(position="stack", stat="identity", width=0.8, show.legend = FALSE) +
#    #geom_errorbar(aes(ymin=sapply(avg_staff-err_staff,function(s){max(0,s)}), ymax=avg_staff+err_staff), width=0.2, position=position_dodge(0.8), show.legend = FALSE) +
#    xlab("Day") +
#    ylab("") +
#    labs(fill="Hospital area") +
#    ggtitle("Daily staff need (FTE)") +
#    theme(axis.text.x=element_text(size=5), axis.text.y=element_text(size=10), axis.title=element_text(size=16), title=element_text(size=14), legend.title=element_text(size=6), legend.text=element_text(size=6)) +
#    scale_fill_manual(values=col) +
#    #ylim(c(0,sum(pars$capacity_info/pars$staff_info$patients_per_staff)))
#    ylim(c(0,50))
#  ggsave(filename=paste("data/",analysisID,"/output/StaffNeedOverTime.png",sep=''), plot=p)
 
  res <- list(
    overall_quality_of_service = overall_qual,
    quality_of_service_by_area = qual_by_area,
    n_patients = n_patients,
    overall_waiting_time = overall_wait,
    waiting_time_by_area = wait_by_area,
    overall_bed_utilisation = overall_util,
    bed_utilisation_by_area = util_by_area,
    total_staff_days_needed = total_staff_days,
    total_staff_costs = staff_costs,
    total_bed_costs = bed_costs,
    raw_output=raw_output,
    pars = pars
  )
  
  if(save_summary){
    saveRDS(res,paste("data/",analysisID,"/output/",analysisID,"_results_summary.rds",sep=''))
    
    png(filename=paste("data/",analysisID,"/output/BedUtilisation.png",sep=''),width=3.25,height=3.25,units="in",res=300)
    boxplot(overall_util,main="Overall bed utilisation",xlab="All areas",col="deepskyblue",ylim=c(0,1))
    dev.off()
    
    png(filename=paste("data/",analysisID,"/output/WaitingTime.png",sep=''),width=3.25,height=3.25,units="in",res=300)
    boxplot(overall_qual/n_patients,main="Overall waiting time",xlab="All areas",col="deepskyblue",ylim=c(0,max(overall_qual/n_patients)))
    dev.off()
    
  }
  
  return(res)
   
}


