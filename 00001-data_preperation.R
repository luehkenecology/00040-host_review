#################################
# set working directory
#################################
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)
getwd()

#################################
# libraries
#################################
library(tidyverse)
library(readxl)

# function
recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

#################################
# read all data
#################################

ff <- function(x) {
  #x <- "data/bfa_additional_2.xlsx"
  n_sheets <- excel_sheets(x)
  list.usa <- list()
  
  for(i in 1:(length(n_sheets))){
    list.usa[[i]] <- read_excel(x, sheet = i)
    list.usa[[i]]$sheet <- i
    list.usa[[i]]$year <- as.character(list.usa[[i]]$year)
    list.usa[[i]]$specimen_feeding_on_this_host <- as.character(list.usa[[i]]$specimen_feeding_on_this_host)
    list.usa[[i]]$host_scientific <- as.character(list.usa[[i]]$host_scientific)
    list.usa[[i]]$paper <- as.character(list.usa[[i]]$paper)
    list.usa[[i]]$no_of_diff_host_species <- as.character(list.usa[[i]]$no_of_diff_host_species)
    
    if(sum(str_detect(colnames(list.usa[[i]]), "forage_ratio"))){
      list.usa[[i]]$forage_ratio <- as.character(list.usa[[i]]$forage_ratio)
    } else {
      list.usa[[i]]$insecticide <- as.character(list.usa[[i]]$insecticide)
    }
  }
  
  #do.call(rbind, list.usa)
  bind_rows(list.usa)
}

library(stringr)
library(dplyr)
bfa_USA1 <- ff("data/bfa_USA_A-E.xlsx")
bfa_USA2 <- ff("data/bfa_USA_F-M.xlsx")
bfa_USA3 <- ff("data/bfa_USA_N-Z.xlsx")
bfa_Europe <- ff("data/bfa_Europe.xlsx")
bfa_other1 <- ff("data/bfa_other_A-H.xlsx") # sheet "0" geloescht
bfa_other2 <- ff("data/bfa_other_I-K.xlsx") # sheet "0" geloescht
bfa_other3 <- ff("data/bfa_other_L-Z.xlsx") # sheet "0" geloescht

bfa_USA1$host_point_counts <- as.character(bfa_USA1$host_point_counts)

all <- bind_rows(bfa_USA1,
                 bfa_USA2,
                 bfa_USA3,
                 bfa_Europe,
                 bfa_other1,
                 bfa_other2,
                 bfa_other2)

ups <- read_excel("speciess.xlsx")

A <- data.frame(oldvalue = ups$oldvalue)

asti <- data.frame(old = A, 
                   new = paste(ups$genus_manual, ups$species_manual,sep = " "))
asti <- na.omit(asti)
all$mosquito_species_new[!is.na(all$mosquito_species)] <- recoderFunc(all$mosquito_species[!is.na(all$mosquito_species)], asti$old, 
                                                                      asti$new)

host_group <- read_excel("host-group.xlsx")
host_group2 <- data.frame(old = host_group$old, 
                          new = host_group$new)
host_group3 <- na.omit(host_group2)
all$host_group_new <- recoderFunc(all$host_group, host_group3$old, 
                                  host_group3$new)

rename_host_scientific <- read_excel("data/Copy of hosts_unify.xlsx")

rename_common <- data.frame(old = rename_host_scientific$old_common_name, 
                            new = rename_host_scientific$final)
rename_common2 <- na.omit(rename_common)
all$host[!is.na(all$host)] <- recoderFunc(all$host[!is.na(all$host)], rename_common2$old, 
                                          rename_common2$new)
table(is.na(all$host))

rename_scientific <- data.frame(old = rename_host_scientific$old_scientific_name, 
                                new = rename_host_scientific$final)
rename_scientific2 <- na.omit(rename_scientific)
all$host_scientific[!is.na(all$host_scientific)] <- recoderFunc(all$host_scientific[!is.na(all$host_scientific)], rename_scientific2$old, 
                                                                rename_scientific2$new)

table(is.na(all$host_scientific) & is.na(all$host))
all$host[is.na(all$host_scientific)]

all$host_scientific_new <- ifelse(is.na(all$host_scientific) == T, all$host, all$host_scientific)

table(is.na(all$host_scientific_new))

write.table(all, "output/merged_dataset.csv", sep = ";", col.names = NA)


# problems
write.table(data.frame(all[is.na(all$host_scientific_new),]) , "e.csv", sep = ";", col.names = NA)


spec_uni <- sort(unique(all$host_scientific_new))

all_names <- unique(unlist(strsplit(spec_uni, c(" / ", "/ "))))

unique(all_names)

###c
#################################
library(myTAI)

#manual <- c("Reptilia", "Fish")
#subgenus or subgenus --> genus


OST <- lapply(1:length(all_names), function(x) taxonomy( organism =
                                                           as.character(all_names[x]),
                                                         db       = "ncbi",
                                                         output   = "classification" ))
OST_save <- OST

# values for NAs
test <- OST[[3]][1,] 
test[1:15,1:3] <- NA
test$value <- all_names[is.na(unlist(lapply(OST5, `[[`, 1)))]

OST2 <- lapply(OST, function(x) cbind(x, value = x[nrow(x),1]))
OST3 <- do.call(rbind, OST2[unlist(lapply(OST2, function(x) ncol(x))) == 4])
OST_all <- rbind(test, OST3)

#write.table(OST_all, "output/taxa_info.csv", sep =  ";")

OST_all_2 <- subset(OST_all, rank %in% c("class", "family","genus","species","subspecies"))

library(tidyr)
OST_all_2$name
resphape(OST_all_2, key = name, value = value)
df3 <- data.frame(id = 1:4, age = c(40,50,60,50), dose1 = c(1,2,1,2),
                  dose2 = c(2,1,2,1), dose4 = c(3,3,3,3))


df3 <- data.frame(school = rep(1:3, each = 4), class = rep(9:10, 6),
                  time = rep(c(1,1,2,2), 3), score = rnorm(12))
wide <- reshape(OST_all_2, timevar = c("rank"),idvar = c("value"), direction = "wide")



# merge with nice data



for(i in 1:length(spec_uni) ){
  
  for(k in 1:10){
    
    try(B <- taxonomy( organism =
                         as.character(spec_uni[i]),
                       db       = "ncbi",
                       output   = "classification" ))
    
    #if(!is.na(B)){break}
    
    #print(k)
    
  }
  
  if(length(B) == 1){
    A <- rbind(A, c(as.character(unique(all$host_scientific_new)[i]),
                    c(NA,NA,NA)))
  } else {
    A <- rbind(A, cbind(taxon = unique(all$host_scientific_new)[i], B ))
  }
  
  
  saveRDS(A, file = "output/tax_host_sci1.RData")
  
  print(i)
}


OO <- all %>%
  filter(mosquito_species %in% c("Culex quinquefasciatus","Culex nigripalpus","Culex erraticus", "Culex coronator")) %>%
  filter(host_group_new %in% c("amphibian","avian","human", "mammalian", "non-human mammalian", "reptilian")) %>%
  group_by(mosquito_species, host_group_new) %>%
  summarize(sum = sum((as.numeric(as.vector(specimen_feeding_on_this_host))), na.rm = T)) %>%
  group_by(mosquito_species) %>%
  mutate(sum_species = sum(sum))%>%
  group_by(mosquito_species, host_group_new) %>%
  mutate(perc = sum/sum_species*100)

data.frame(OO)

#write.table(data.frame(OO), "sub_David.csv", sep = ";", col.names = NA)

# host names
A <- data.frame(unique(all[c("host_scientific","host")]))
dimnames(A)[[2]] <- c("oldvalue", "normal") 

host_scientific_rename <- read.csv("data/host.scientific_all2.csv", 
                                   header = T, row.names = 1, sep = ";")

A2 <- merge(A, host_scientific_rename,  by = "oldvalue", all.x = T)

dimnames(A2)[[2]] <- c("old_scientific", "oldvalue","X",    "X.1",      "new_scientific") 

host_normal_rename <- read.csv("data/host_all2.csv", 
                               header = T, row.names = 1, sep = ";")

A3 <- merge(A2, host_normal_rename,  by = "oldvalue", all.x = T)

#write.table(A3, "hosts_unify.csv", sep = ";", col.names = NA)


EE <- unique(all$mosquito_species)
unlist(lapply(str_split(EE, " "), `[[`, 1))

unlist(lapply(str_split(EE, " ", n=2)[unlist(lapply(str_split(EE, " ", n=2), length)) > 1], `[[`, 2))


e <- str_split(unique(all$mosquito_species), " ")

#write.csv(unique(all$mosquito_species),"asss.csv")

unique(all[c("host", "host_scientific")])


ee <- all %>%
  group_by(mosquito_species, host_group) %>%
  summarise(sum = length(host_group)) %>%
  arrange(desc(sum))

head(data.frame(ee))

sum(as.numeric(all$specimen_feeding_on_this_host), na.rm = T)

table((all$country))

all$host_group

A <- data.frame(oldvalue = unique(all$mosquito_species))

# moquito rename 
scientific_mosquito_rename <- read.csv("mosq.species_all2.csv", 
                                       header = T, row.names = 1, sep = ";")

asdf <- merge(A, scientific_mosquito_rename,  by = "oldvalue", all.x = T)


asdf$X1 <- unlist(lapply(str_split(asdf$oldvalue, " "), `[[`, 1))

asdf$X2[unlist(lapply(str_split(asdf$oldvalue, " ", n=2), length)) > 1] <- unlist(lapply(str_split(asdf$oldvalue, " ", n=2)[unlist(lapply(str_split(asdf$oldvalue, " ", n=2), length)) > 1], `[[`, 2))

lapply(asdf$X2, length)

#write.table(asdf, "Ast.csv", sep = ";", col.names = NA)

ups <- read_excel("speciess.xlsx")

asti <- cbind(old = A, 
              new = paste(ups$genus_manual, ups$species_manual,sep = " "))
asti <- na.omit(asti)
all$mosquito_species_new <- recoderFunc(all$mosquito_species, asti$old, 
                                        asti$new)


host_group <- read_excel("host-group.xlsx")
host_group2 <- data.frame(old = host_group$old, 
                          new = host_group$new)
host_group3 <- na.omit(host_group2)
all$host_group_new <- recoderFunc(all$host_group, host_group3$old, 
                                  host_group3$new)


unique(all$mosquito_species_new)[str_detect(unique(all$mosquito_species_new), c("quinquefasciatus"))]

table(all$host_group_new)
colnames(all)
unique(all$host_group_new)
OO <- all %>%
  filter(mosquito_species %in% c("Culex quinquefasciatus","Culex nigripalpus","Culex erraticus", "Culex coronator")) %>%
  # filter(host_group_new %in% c("amphibian","avian","human", "mammalian", "non-human mammalian", "reptilian")) %>%
  group_by(mosquito_species, host_group) %>%
  summarize(sum = sum((as.numeric(as.vector(specimen_feeding_on_this_host))), na.rm = T)) 

data.frame(OO)  
sum(as.numeric(as.vector(all$specimen_feeding_on_this_host)), na.rm = T)

%>%
  group_by(paper, mosquito_species) %>%
  filter(sum==max(sum))


library(pdftools)
PDF <- pdf_text("Valid Species List_95.pdf")
uu <- lapply(PDF, function(x) unlist(str_split(x, "\r\n")))
spec_ep <- word(unlist(uu), 1)


g <- unlist(unlist(uu))

uu2 <- lapply(1:3649, function(x) str_split(g[x], ":"))

uu3 <- lapply(1:3649, function(x) unlist(uu2[[x]])[2])

uu4 <- lapply(uu3, function(x) trimws(x))
gen_ep <- word(unlist(uu4), 1)

ALO <- cbind(gen_ep, spec_ep)

lapply(1:length(spec_ep), function (x) table(str_detect(asdf$oldvalue, spec_ep[x])))

spec_ep[which(str_detect(all$mosquito_species[1000], spec_ep))]
spec_ep[2830:2840]
?str_detect

lapply(str_split(unlist(uu), ":"), `[[`, 2)



unlist(uu3)

oo <- lapply(uu, function(x) str_split(x, ": "))
oo2 <- lapply(oo, function(x) lapply(x, `[[`, 2))

lapply(uu[[1]], `[[`, 1)

unlist(specs1[3:4])
specs1 <- lapply(unlist(str_split(PDF, "\r\n")), function(x) str_split(x, " "))
unlist(lapply(specs1, `[[`, 1))

species_epis <- unlist(lapply(str_split(PDF[[1]], "\r\n"), `[[`, 10))


#ly the species with new value
scientific_mosquito_rename_sub <- scientific_mosquito_rename[!scientific_mosquito_rename$newvalue == "",]


all$mosquito_species <- recoderFunc(all$mosquito_species, 
                                    scientific_mosquito_rename_sub$oldvalue,
                                    scientific_mosquito_rename_sub$newvalue)

unique(all$mosquito_species)
colnames(all)
library(dplyr)
as.numeric(as.vector(all$specimen_feeding_on_this_host))

all[all$mosquito_species == "Culex quinquefasciatus",]$paper
all[all$mosquito_species == "Culex quinquefasciatus",]

all$unqiueID1 <- paste(all$mosquito_species, all$paper, all$sheet, sep = "_")
all$unqiueID2 <- paste(all$paper, all$sheet, sep = "_")


# per species
OO <- all %>%
  filter(host_group %in% c("amphibian","avian","human", "mammalian", "non-h.mammalian", "reptilian")) %>%
  group_by(mosquito_species, paper, sheet) %>%
  summarize(sum = sum(round(as.numeric(as.vector(specimen_feeding_on_this_host))), na.rm = T)) %>%
  group_by(paper, mosquito_species) %>%
  filter(sum==max(sum))

OO_select <- paste(OO$mosquito_species, OO$paper, OO$sheet, sep = "_")

no_specimens_per_species <- all %>%
  filter(unqiueID1 %in% OO_select) %>%
  group_by(mosquito_species) %>%
  summarize(sum = sum(round(as.numeric(as.vector(specimen_feeding_on_this_host))), na.rm = T)) %>%
  arrange(desc(sum))


AA <- all %>%
  filter(mosquito_species %in% no_specimens_per_species$mosquito_species[1:11]) %>%
  filter(host_group %in% c("amphibian","avian","human", "mammalian", "non-h.mammalian", "reptilian")) %>%
  filter(unqiueID1 %in% OO_select) %>%
  group_by(mosquito_species, host_group) %>%
  summarize(sum = sum(round(as.numeric(as.vector(specimen_feeding_on_this_host))), na.rm = T)) %>%
  group_by(mosquito_species) %>%
  mutate(label = sum(sum, na.rm = T))

library(plyr)
AA$host_group <- revalue(AA$host_group, c("non-h.mammalian" = "non-human mammalian"))
detach("package:plyr", unload=TRUE)

# plot 1
png(file = "species.png",width = 6, height=4.2 , units = 'in', res = 500)
ggplot(data=AA, aes(x=reorder(mosquito_species, -label), y=sum, fill = host_group)) +
  geom_bar(position="fill", stat="identity") +
  xlab("Mosquito taxa") +
  ylab("Percentage") +
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, face = "italic"))+
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0))+
  guides(fill=guide_legend(title="Host group"))+
  geom_text(data=AA,aes(x=mosquito_species,y=0.5,label=label, angle=90,hjust=1,vjust=0.5))
dev.off()

# plot 2
# per country
no_species_country <- all %>%
  group_by(mosquito_species) %>%
  summarize(sum = length(unique(country))) %>%
  arrange(desc(sum))


OO <- all %>%
  filter(mosquito_species %in% c("Culex quinquefasciatus")) %>%
  filter(host_group %in% c("amphibian","avian","human", "mammalian", "non-h.mammalian", "reptilian")) %>%
  group_by(paper, sheet) %>%
  summarize(sum = sum(round(as.numeric(as.vector(specimen_feeding_on_this_host))), na.rm = T)) %>%
  group_by(paper) %>%
  filter(sum==max(sum))

OO_select <- paste(OO$paper, OO$sheet, sep = "_")

AA1 <- all %>%
  filter(mosquito_species %in% c("Culex quinquefasciatus")) %>%
  filter(host_group %in% c("amphibian","avian","human", "mammalian", "non-h.mammalian", "reptilian")) %>%
  filter(unqiueID2 %in% OO_select) %>%
  group_by(country, host_group) %>%
  summarize(sum = sum(round(as.numeric(as.vector(specimen_feeding_on_this_host))), na.rm = T)) %>%
  group_by(country) %>%
  mutate(label = sum(sum, na.rm = T))

library(plyr)
AA1$host_group <- revalue(AA1$host_group, c("non-h.mammalian" = "non-human mammalian"))
detach("package:plyr", unload=TRUE)

# plot 2
png(file = "culex_pipiens_sl_country.png",width = 4.5, height=4.2 , units = 'in', res = 500)
ggplot(data=AA1, aes(x=reorder(country, -label), y=sum, fill = host_group)) +
  geom_bar(position="fill", stat="identity") +
  xlab("Country") +
  ylab("Percentage") +
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0))+
  guides(fill=guide_legend(title="Host group"))+
  geom_text(data=AA1,aes(x=country,y=0.5,label=label, angle=90,hjust=1,vjust=0.5))
dev.off()


#################################
# standardize scientific names
#################################
scientific_rename <- read.csv("data/host.scientific_all2.csv", 
                              header = T, row.names = 1, sep = ";")

# only the species with new value
scientific_rename_sub <- scientific_rename[!scientific_rename$newvalue == "",]

all$host_scientific_new <- recoderFunc(all$host_scientific, 
                                       scientific_rename_sub$oldvalue,
                                       scientific_rename_sub$newvalue)

#################################
# standardize common names
#################################
rename_host <- read.csv("data/host_all2.csv", sep = ";", header = T)

# renaming host
# selection of entries not "" for newvalue
rename_host_sub <- rename_host[rename_host$newvalue != "",]

# deleting last row with NA
rename_host_sub <- rename_host_sub[!is.na(rename_host_sub$newvalue),]


# renaming for all
all$host_new <- recoderFunc(all$host, rename_host_sub$oldvalue,
                            rename_host_sub$newvalue)


#################################
# double hosts
#################################
all$id <- rownames(all)

A1 <- which(str_detect(all$host_new, " / ") & (!is.na(all$host_new)))

A2 <- which(str_detect(all$host_scientific_new, " / ") & (!is.na(all$host_scientific_new)))

A3_common <- A1[!(A1 %in% A2)]
A3_scientific <- A2[!(A2 %in% A1)]
A3_both <- A1[(A1 %in% A2)]

B1 <- lapply(strsplit(all$host_new[all$id %in% A3_common], " / "), function(x) length(x))
C1 <- strsplit(all$host_new[c(all$id %in% A3_common)], " / ")

B2 <- lapply(strsplit(all$host_scientific_new[all$id %in% A3_scientific], " / "), function(x) length(x))
C2 <- strsplit(all$host_scientific_new[c(all$id %in% A3_scientific)], " / ")

repets <- rep(1,nrow(all))
repets[all$id %in% A3_common] <- unlist(B1)
idx <- rep(1:nrow(all), repets)
dupdf <- all[idx,]
dupdf$host_new[dupdf$id %in% A3_common] <- unlist(C1)

repets <- rep(1,nrow(dupdf))
repets[dupdf$id %in% A3_scientific] <- unlist(B2)
idx <- rep(1:nrow(dupdf), repets)
dupdf2 <- dupdf[idx,]
dupdf2$host_scientific_new[dupdf2$id %in% A3_scientific] <- unlist(C2)

repets <- rep(1,nrow(dupdf2))
repets[dupdf2$id %in% A3_both] <- 2
idx <- rep(1:nrow(dupdf2), repets)
dupdf3 <- dupdf2[idx,]
dupdf3$host_new[dupdf3$id %in% A3_both] <- unlist(strsplit(dupdf$host_new[dupdf$id %in% A3_both], " / "))
dupdf3$host_scientific_new[dupdf3$id %in% A3_both] <- unlist(strsplit(dupdf$host_scientific_new[dupdf$id %in% A3_both], " / "))

# unique scientific and common names
unique_para <- unique(dupdf3[c("host","host_new", "host_scientific","host_scientific_new")])

#write.table(unique_para, "dupdf3.csv", sep = ";", col.names = NA)

speciesss <- read_excel("hosts_unify.xlsx")

library("taxize")
#oel <- lapply(1:nrow(speciesss), 
#       function(x) comm2sci(as.character(speciesss[x,2]), db = "ncbi"))
#oel2 <- oel

oel3 <- unlist(lapply(oel,function(x) if(identical(x,character(0))) ' ' else as.character(x)))
#write.table(unlist(lapply(oel3, `[[`, 1)), "oel.csv", sep = ";")


gib <- which(is.na(unique_para$host))
host_necessary2 <- unique_para %>% filter(is.na(host))
com_name <- lapply(1:nrow(host_necessary2), function(x) sci2comm(host_necessary2$host_scientific_new[x], db = 'ncbi')[[1]][1])
unique_para$host_new[c(gib)] <- unlist(com_name)

#write.table(unique_para, "dupdf3.csv", sep = ";", col.names = NA)


gib2 <- which(is.na(unique_para$host_scientific))
host_necessary3 <- unique_para %>% filter(is.na(host_scientific))
sci_name <- lapply(1:nrow(host_necessary3), function(x) comm2sci(host_necessary3$host_new[x], db = 'ncbi')[[1]][1])
unique_para$host_scientific_new[c(gib2)] <- unlist(sci_name)

gg<- unique_para[
  order(unique_para[,3], unique_para[,4],unique_para[,1], unique_para[,2] ),
]

sci2comm("Sciuridae", db = "ncbi")
comm2sci("Aves", db = "ncbi")

gg2 <- unique(gg[c("host","host_new", "host_scientific","host_scientific_new")])
gg2[5,1] == gg2[6,1]
#write.table(gg, "unique_para.csv", sep = ";", col.names = NA)




#################################
# set working directory
#################################
library(myTAI)

for(i in 2:length(unique(all$host_scientific_new))){
  
  for(k in 1:10){
    
    try(B <- taxonomy( organism =
                         as.character(unique(all$host_scientific_new)[i]),
                       db       = "ncbi",
                       output   = "classification" ))
    
    if(!is.na(B)){break}
    
    print(k)
    
  }
  
  if(length(B) == 1){
    A <- rbind(A, c(as.character(unique(all$host_scientific_new)[i]),
                    c(NA,NA,NA)))
  } else {
    A <- rbind(A, cbind(taxon = unique(all$host_scientific_new)[i], B ))
  }
  
  
  saveRDS(A, file = "output/tax_host_sci1.RData")
  
  print(i)
}
















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