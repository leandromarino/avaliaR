
read.scoBlg <- function(file, length_id)
{
  read.fwf(file, widths=list(c(3,-2,length_id),c(6,-9,5,5,10,12,12)),
           skip=2,
           col.names=c("grupo","id","peso","ntried","nacer","pacer","proficor","dpor"),
           colClasses = c('integer','character','numeric','integer','integer','numeric','numeric','numeric'))
}

