#' Exibe Paleta de cores
#' 
#' @usage MostraPaleta(cores)
#' 
#' @param cores Vetor com as cores que compoem a paleta de cores escolhida
#' 
#' @examples
#' 
#' ## 
#'  MostraPaleta(c('blue','cyan','red','yellow'))
#'  


MostraPaleta <- function(cores){
  par(mar= rep(0.5,4))
  plot(0,0,type='n',ylab='',xlab='',axes=F,ylim=c(0,1),xlim=c(0,length(cores)))
  for(i in 1:length(cores)) rect(i-1,0,i,1,col=cores[i],border=NA)

  cores_rgb <- col2rgb(cores)
  cores_rgb['red'  ,] <- cores_rgb['red'  ,]*0.299
  cores_rgb['green',] <- cores_rgb['green',]*0.587
  cores_rgb['blue' ,] <- cores_rgb['blue' ,]*0.114
  cores_rgb <- rbind(cores_rgb, colSums(cores_rgb))
  rownames(cores_rgb)[4] <- 'soma_cor'
  
  cor_texto <- ifelse(cores_rgb['soma_cor',] > 170, '#000000', '#FFFFFF')
  
  text(x=seq(0.5,length(cores)-0.5,1),y=0.15,labels = paste('cores ', 1:length(cores),sep=''), font = 2, col = cor_texto)
  text(x=seq(0.5,length(cores)-0.5,1),y=0.10,labels = cores, font = 2, col = cor_texto)
  text(x=seq(0.5,length(cores)-0.5,1),y=0.05,labels = apply(col2rgb(cores),2,paste0,collapse=','),font = 2 , col = cor_texto)
  box()
}
