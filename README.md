## Introduction

The code contained in this repository reproduces the results described in the paper "Using design of experiments with discrete event simulation in operational research: a review" by Gjerloev et al.


## Instructions

To run the analyses, download the repository to your local computer, preserving the folder structure.

The data folder contains one folder for each analysis. The folder name is the analysis ID used in the R code.

Each analysis folder contains an empty folder named "output" as well as a folder named "input" containing three MS Excel files: "hospital_capacity.xlsx", "patient_trajectories.xlsx", "patient_groups.xlsx".

### hospital_capacity.xlsx
This file contains capacity data.
In the "capacities" tab, for each area (node in the queueing network), the first column contains the name of the area, the second column contains the capacity (number of beds) and the third column contains the cost of the resource (bed) per time unit.
The "monitored_resource" tab is not used in the current version of the code.

### patient_trajectories.xlsx
Each tab is named after a trajectory ID. A trajectory is a sequence of areas visited by the patient. Areas can be repeated in a trajectory.
For each step in the trajectory (second column), a service time distribution needs to be specified in the format DISTRIBUTION,PARAMETERS - they need to be separated by a comma, without spaces
Supported values for the DISTRIBUTION = {exp,lognorm,unif,det}, respectively for Exponential, Lognormal, Uniform and Deterministic.
The DISTRIBUTION tag must be followed by a comma (no spaces) and then comma-separated (no spaces) distribution parameters, one for Exponential and Deterministic, two for Lognormal and Uniform. For instance: "lognorm,6,0.8" means that the service times follow a Lognormal distribution with mean and standard deviation in the unlogged scale of 6 and 0.8, respectively; "exp,5" means that the service times forllow an Exponential distribution with mean 5.
Several groups of patients can be defined using several columns and patient group IDs in case patients have the same trajectories but different service times. Alternatively, these could be defined as separate trajectories using different tabs of the spreadsheet.

### patient_groups.xlsx
For each patient group defined above, this file stores their arrival rate into the system (first node of their trajectory) as well as the split of patients of that group into different trajectories.
Similarly to service times, the arrival rate is specified in the format DISTRIBUTION,PARAMETERS
Supported values for the DISTRIBUTION = {poiss,det}, respectively for Poisson and Deterministic.
The DISTRIBUTION tag must be followed by a comma (no spaces) and then the mean arrival rate. For instance, "poiss,9.6" means that arrivals follow a Poisson process with rate 9.6.

The output folder will be automatically populated with simulation data and summary plots.

