
readInput <- function(analysisID){
  
  require(readxl)

  capacity_info <- as.data.frame(read_excel(paste("data/",analysisID,"/input/hospital_capacity.xlsx",sep=''),sheet="capacities"))
  rownames(capacity_info) <- capacity_info[,1]
  capacity_info <- capacity_info[,-1,drop=FALSE]

  staff_info <- as.data.frame(read_excel(paste("data/",analysisID,"/input/hospital_capacity.xlsx",sep=''),sheet="monitored_resources"))
  rownames(staff_info) <- staff_info[,1]
  staff_info <- staff_info[,-1,drop=FALSE]
  
  patient_info <- as.data.frame(read_excel(paste("data/",analysisID,"/input/patient_groups.xlsx",sep=''),sheet="groups"))
  rownames(patient_info) <- patient_info$groupID
  patient_info <- patient_info[,-1,drop=FALSE]
  
  trajID <- colnames(patient_info[-c(1,2)])
  groupID <- rownames(patient_info)
  
  trajectory_info <- lapply(trajID,function(t){
    as.data.frame(read_excel(paste("data/",analysisID,"/input/patient_trajectories.xlsx",sep=''),sheet=t))
  })
  names(trajectory_info) <- trajID
  
  return(list(
    analysisID=analysisID,
    trajID=trajID,
    groupID=groupID,
    patient_info=patient_info,
    trajectory_info=trajectory_info,
    capacity_info=capacity_info,
    staff_info=staff_info
  ))
  
}








