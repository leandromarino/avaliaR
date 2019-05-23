
read.rjBlg <- function(file)
{
  rj <- read.fwf(file,  skip=2,
                 widths=list( c(-81),c(-81),c(-81),c(-81),c( 13,2,-12,10,rep(11,4)),c(-81),c(-81),c(-82),
                              c(-81),c(-81),c(-81),c(-81),c(-27,10,rep(11,4)),c(-81),c(-81),c(-82),
                              c(-81),c(-81),c(-81),c(-81),c(-27,10,rep(11,4)),c(-81),c(-81),c(-82),
                              c(-81),c(-81),c(-81),c(-81),c(-27,10,rep(11,4)),c(-81),c(-81),c(-82),
                              c(-81),c(-81),c(-81),c(-81),c(-27,10,rep(11,4)),c(-81),c(-81),c(-82),
                              c(-81),c(-81),c(-81),c(-81),c(-27,10,rep(11,4)),c(-81),c(-81),c(-82),
                              c(-81),c(-81),c(-81),c(-81),c(-27,10,rep(11,4)),c(-81),c(-81),c(-82),
                              c(-81),c(-81),c(-81),c(-81),c(-27,10,rep(11,4)),c(-81),c(-81),c(-82)),
                 colClasses=c('character','integer',rep("numeric",5*8)),header=F,flush=T)
  names(rj) <- c('nomeblg','grupo',paste0("V",1:40))
  rj$nomeblg <- avaliaR::removeBrancos(rj$nomeblg)
  rj
}
