# file = paste0(dirBLG_CH_EM, 'Encc18_CH_EM_V1.PH1')
# compara_grupo = 8
# gabpar = rbind(gabparEncc18CH_EM, gabparEncc18CH_EML[gabparEncc18CH_EM$itemblg > max(gabparEncc18CH_EM$itemblg),])
# 

dif_ph1 <- function(file, compara_grupo, gabpar){
  
  ph1 <- read.ph1_stats(file)
  
  grupo_de_comparacao <- ph1[[compara_grupo]]
  
  grupo_de_comparacao <- merge(grupo_de_comparacao, gabpar[, c('nomeblg', 'codbni')], by = 'nomeblg', all.x = T)
  
  nome_grupo_compara <- names(ph1)[compara_grupo]
  
  colnames(grupo_de_comparacao)[3:8] <- paste0(colnames(grupo_de_comparacao)[3:8],"_",nome_grupo_compara)
  
  group_ref <- c(1:length(ph1))[-c(compara_grupo,length(ph1))]
  
  grp=1
  
  itens_comuns <- list()
  i=1
  for(grp in group_ref){
    x <- merge(x = ph1[[grp]], y = grupo_de_comparacao, by = c('itemblg', 'nomeblg'))
    if(nrow(x) > 0){
      cols <- c('itemblg', 'nomeblg', 'codbni', 'pct', paste0('pct_',nome_grupo_compara), 'bise', paste0('bise_',nome_grupo_compara))
      x <- x[,cols]
      x$tipo_dif <- paste(names(ph1)[grp], 'x', names(ph1)[compara_grupo])
      x$pct_dif <- x$pct - x[,paste0('pct_',nome_grupo_compara)]
      cols <- c('itemblg', 'nomeblg', 'codbni', 'tipo_dif', 'bise', paste0('bise_',nome_grupo_compara),
                'pct', paste0('pct_',nome_grupo_compara), 'pct_dif')
      x <- x[,cols]
      colnames(x) <- c('itemblg','nomeblg','coditem','tipo_dif','bise_G1','bise_G2','pct_G1','pct_G2', 'dif_G1_x_G2')
      itens_comuns[[i]] <- x
      names(itens_comuns)[i] <- paste(names(ph1)[grp], 'x', names(ph1)[compara_grupo])
      i = i+1
    }
  }
  itens_comuns <- do.call(rbind, itens_comuns)
  itens_comuns <- itens_comuns[order(itens_comuns$itemblg),]
  itens_comuns
  
}

# rm(file, ph1, grp, x, itens_comuns, compara_grupo, group_ref, i, nome_grupo_compara, grupo_de_comparacao, gabpar)

#Store(dif_ph1, lib = funcoes, lib.loc = dirEncceja18)
