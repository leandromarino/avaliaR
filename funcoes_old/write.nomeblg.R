

write.nomeblg <- function(nomeblg, ncol = 8)
{
  aux <- length(nomeblg)
  nlin <- floor(aux/ncol) + 1 
  nadicional <- ncol - aux%%ncol
  nomeblg = sprintf("%9s",c(paste0(nomeblg,','),rep("",nadicional)))
  write.table(matrix(nomeblg, ncol = ncol, nrow = nlin, byrow = TRUE), 
              sep=' ', quote = FALSE, col.names = FALSE, row.names = FALSE)
}

