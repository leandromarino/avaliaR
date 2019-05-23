read.blm.grou <-function(file, group.names = F){
  blm <- readLines(con = file)
  check_grou <- substr(blm, 1, 5) == ">GROU"
  check_estr <- substr(blm, 1, 1) == "("
  inicio <- which(check_grou == TRUE)
  fim <- c(which(check_grou == TRUE)[-1],which(check_estr == TRUE)[1])-1
  group <- list()
  for(i in 1:length(inicio)){
    aux <- blm[inicio[i]:fim[i]]
    group[[i]] <- paste0(aux,collapse = '')
  }
  group <- unlist(group)
  group <- lapply(strsplit(group,'[(]|[)]'), '[', i = 2)
  group <- unlist(group)
  group <- strsplit(group,',')
  for(i in 1:length(group)){
    mode(group[[i]]) <- 'integer'
  }
  
  if(group.names){
    group_names <- strsplit(blm[check_grou],',')
    group_names <- unlist(lapply(group_names,'[',i=1))
    group_names <- strsplit(group_names,'=')
    group_names <- unlist(lapply(group_names,'[',i=2))
    group_names <- gsub(pattern = "'", replacement = '', x = group_names)
    names(group) <- group_names
    rm(group_names)
  }
  
  
  group
}

