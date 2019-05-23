busca_itens_comuns <- function(file, compara_grupo){
  # file = paste0(dirBLG_CH_EM, 'Encc18_CH_EM_V1.BLM')
  # compara_grupo = 8
  # rm(file, blm_group, grp, group_ref, compara_grupo, i, x, itens_comuns, grupo_de_comparacao)
  
  blm_group <- read.blm.grou(file, group.names = T)
  grupo_de_comparacao <- blm_group[[compara_grupo]]
  
  group_ref <- c(1:length(blm_group))[-compara_grupo]
  
  grp=1
  
  itens_comuns <- list()
  i=1
  for(grp in group_ref){
    x <- intersect(x = blm_group[[grp]], y = grupo_de_comparacao)
    if(length(x) > 0){
      itens_comuns[[i]] <- x
      names(itens_comuns)[i] <- paste(names(blm_group)[grp], 'x', names(blm_group)[compara_grupo])
      i = i+1
    }
  }
  itens_comuns
}


