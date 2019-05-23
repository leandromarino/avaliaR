
EmpPerc_Peso <- function(items, responses,vecform,profi,resp_possible,nitems,nitemform,nforms,min_break,max_break,by_break,escala0100=F,peso = NULL)
{
  
  if(is.null(peso)) peso <- rep(1,length(responses))
  
  (cut_break <- c(-Inf,seq((min_break+by_break/2),(max_break-by_break/2),by_break),Inf))
  (if(escala0100==T) cut_break <- c(-Inf,0.1,cut_break[-c(1:2,length(cut_break)-1,length(cut_break))],99.9,Inf))
  (exib_break <- seq((min_break) , (max_break), by_break))
  (if(escala0100==T) exib_break <- c(0,exib_break[-c(1,length(exib_break))],100))
  
  it_level <- (cut(profi,cut_break,labels = exib_break,include.lowest=T,right=F))
  
  #as.matrix(table(vecform,cut(profi,cut_break,labels = exib_break,include.lowest = TRUE, right = FALSE)))
  
  ### criando matriz de respostas
  resp_matrix <- data.frame(do.call(rbind,strsplit(responses,NULL)),stringsAsFactors=F)
  for(i in 1:nitemform){
    resp_matrix[,i] <- factor(resp_matrix[,i],levels=resp_possible)
  }
  
  
  #n?mero total de respondentes
  nalu <- length(responses)
  
  #n?mero de alternativas (incluindo branco/inv?lido e n?o-apresentado)
  nalt <- length(resp_possible)
  
  
  itempos <- ItemPos(items,nitems,tipo='character')
  
  EmpPerc <- list()
  EmpPerc[[1]] <- list()
  EmpPerc[[2]] <- list()
  writeLines("\n Calculando os percentuais emp?ricos...")
  for(i in 1:nitems){
    setTxtProgressBar(txtProgressBar(min=1,max=nitems,style=3,width=70,initial=1),i)
    (df_empperc <- data.frame(matrix(NA,ncol=3,nrow=nalu)))
    (df_empperc[,1] <- factor(df_empperc[,1],levels=exib_break))
    (df_empperc[,2] <- factor(df_empperc[,2],levels=resp_possible))
    (df_empperc[,3] <- peso)
    (nlin <- nrow(itempos[[i]]))
    for(j in 1:nlin){
      (form = itempos[[i]][j,'form'])
      (pos  = as.numeric(itempos[[i]][j,'pos']))
      (aux <- vecform==form)
      (df_empperc[aux,1] <- it_level[aux])
      (df_empperc[aux,2] <- resp_matrix[aux,pos])
    }
    #(df_empperc <- df_empperc[df_empperc[,1]!='0',])
    tempaux <- tapply(df_empperc[,3], list(df_empperc[,2],df_empperc[,1]), sum, na.rm = TRUE)
    tempaux[is.na(tempaux)] <- 0
    (EmpPerc[[1]][[i]] <- tempaux)
    EmpPerc[[2]][[i]] <- EmpPerc[[1]][[i]]
    for(k in 1: nrow(EmpPerc[[1]][[i]])){
      EmpPerc[[2]][[i]][k,] <- EmpPerc[[1]][[i]][k,]/colSums(EmpPerc[[1]][[i]])
      EmpPerc[[2]][[i]][,colSums(EmpPerc[[1]][[i]])==0] <- 0
    } 
    
  }
  EmpPerc
}
