
AlteraNivel <- function(EmpPerc, itens, niveis){
  nivel <- EmpPerc[[1]]$level
  nivel[itens] <- as.character(niveis)
  for(i in 1:length(EmpPerc)){
    EmpPerc[[i]]$level <- nivel
  }
  EmpPerc
}
