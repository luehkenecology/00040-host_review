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