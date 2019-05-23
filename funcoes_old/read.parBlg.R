
read.parBlg <- function(file)
{
  dados <-  read.fwf(file,
                     widths=c(8,-4,-24,10,10,10,10,-20,10,10),
                     skip=4,
                     col.names=c("nomeblg","aorig","epa","borig","epb","c","epc"),
                     colClasses=c("character"),
                     header = FALSE, flush = TRUE)
  for(i in 2:7) dados[,i] <- as.numeric(dados[,i])
  dados
}

