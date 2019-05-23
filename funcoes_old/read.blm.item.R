
read.blm.item <- function(file){
  blm <- readLines(con = file)
  check_item <- substr(blm, 1, 5) == ">ITEM"
  check_test <- substr(blm, 1, 5) == ">TEST"
  inicio <- which(check_item == TRUE)
  fim <- c(which(check_item == TRUE)[-1],which(check_test == TRUE)[1])-1
  
  nomeblg <- list()
  for(i in 1:length(inicio)){
    aux <- blm[inicio[i]:fim[i]]
    nomeblg[[i]] <- paste0(aux,collapse = '')
  }
  
  nomeblg <- unlist(nomeblg)
  nomeblg <- lapply(strsplit(nomeblg,'INAMES[=][(]|[)];'), '[', i = 2)
  nomeblg <- gsub("\n",'',nomeblg)
  nomeblg <- strsplit(nomeblg,',')
  nomeblg <- as.list(trim(unlist(nomeblg)))
  
  indices_fortran <- grep(pattern = '.*[(].*',nomeblg) #Elementos do vetor
  
  # nomes que foram feitos com fortran
  
  for( i in indices_fortran){
    aux <- nomeblg[[i]]
    aux <- lapply(strsplit(aux,'[(]|[)]'), '[', i = c(1,3))
    aux <- as.list(unlist(aux))
    aux[[1]] <- stringr::str_match(string = aux[[1]],pattern = '(.*?)(\\d+)(?<=$)')
    aux[[2]] <- stringr::str_match(string = aux[[2]],pattern = '(.*?)(\\d+)(?<=$)')
    aux <- data.frame(do.call(rbind, aux), stringsAsFactors=FALSE)
    num_char <- max(nchar(aux$X3))
    aux$X4 <- as.integer(aux$X3)
    nomeblg[[i]] <- paste0(aux$X2[1],sprintf(paste0("%0",num_char,'d'), aux$X4[1]:aux$X4[2]))
  }
  
  nomeblg <- unlist(nomeblg)
  nomeblg
}
