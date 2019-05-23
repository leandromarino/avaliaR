# dados       = ema1603cad
# vars_agrega = c('ano','regiao','uf','codpol','codmun','codesc','codtur','codalu')
# prefix_qtd = 'qtd'
# nome_qtd = c('ano','reg','est','pol','mun','esc','tur','alu')
# 

GeraBaseTurmas <- function(dados, vars_agrega, prefix_qtd, nome_qtd){
  (nome_qtd = paste0(prefix_qtd,nome_qtd))
  (aux_vars = length(vars_agrega))
  (aux_nome = length(nome_qtd))
  
  (vars_agrega1 <- vars_agrega[-aux_vars])
  
  (tabela <- data.frame(matrix(NA,ncol = length(c(vars_agrega[-aux_vars],nome_qtd))))[-1,])
  (colnames(tabela) <- c(vars_agrega[-aux_vars],nome_qtd))
  
  rev_nome_qtd <- rev(nome_qtd)
  
  #primeironivel
  (tabela0 <- aggregate(list(qtdalu = dados[,vars_agrega[aux_vars]]), as.list(dados[,vars_agrega[-aux_vars]]), length))
  (tabela0 <- cbind(tabela0,
                    tabela[1:nrow(tabela0),!is.element(colnames(tabela),colnames(tabela0))]))
  (tabela0[, substr(colnames(tabela0),1,3) == prefix_qtd & colnames(tabela0) != 'qtdalu'] <- 1)
  (rownames(tabela0) <- 1:nrow(tabela0))
  
  result <- list()
  result[[1]] <- tabela0
  rm(tabela0)
  
  for(vari in 2:(aux_vars - 1)){
    (auxqtd = rev_nome_qtd[1:vari])
    (auxvar = rev(rev(vars_agrega)[-c(1:vari)]))
    
    (colu_qtd_tabela <- nome_qtd[!is.element(nome_qtd,auxqtd)])
    (colu_var_tabela <- vars_agrega1[!is.element(vars_agrega1,auxvar)])
    
    
    tabela0 <- aggregate(result[[vari-1]][, auxqtd],
                         result[[vari-1]][, auxvar, drop = FALSE],
                         sum)
    
    tabela0 <- cbind(tabela0,
                     tabela[1:nrow(tabela0),c(colu_qtd_tabela,colu_var_tabela)])
    tabela0[,colu_qtd_tabela] <- 1
    tabela0[,colu_var_tabela] <- '*Total'
    
    result[[vari]] <- tabela0
    
  }
  
  result <- do.call(rbind,result)
  result <- result[do.call(order,result[,vars_agrega[-aux_vars]]),]
  rownames(result) <- 1:nrow(result)
  
  result
}


# Exemplo de Uso
# x <- GeraBaseTurmas(dados       = ema1603cad,
#                 vars_agrega = c('ano','regiao','uf','codpol','codmun','codesc','codtur','codalu'),
#                 prefix_qtd = 'qtd',
#                 nome_qtd = c('ano','reg','est','pol','mun','esc','tur','alu'))
