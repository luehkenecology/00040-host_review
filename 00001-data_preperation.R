
# set working directory
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)

all <- read.csv("host.scientific_all.csv", sep = ";", 
                header = T, row.names = 1)

A <- data.frame()
B <- data.frame()

library(myTAI)

for(i in 1:length(unique(all$species))){
  
  for(k in 1:10){
    
    try(B <- taxonomy( organism = as.character(unique(all$species)[i]),
                       db       = "ncbi",
                       output   = "classification" ))
    
    if(!is.na(B)){break}
    
    print(k)
    
  }
  
  if(length(B) == 1){
    A <- rbind(A, c(as.character(unique(all$species)[i]), c(NA,NA,NA)))
  } else {
    A <- rbind(A, cbind(taxon = unique(all$species)[i], B ))
  }
  
  
  saveRDS(A, file = "output/tax_host_sci1.RData")
  
  print(i)
}