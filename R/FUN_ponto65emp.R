ponto65emp <- function(nivelitem, breaks, ndec = 0) {
  
  # nivelitem objeto que sai da funcao item level
  # breaks intervalo considerado para o calculo dos niveis usualmente 25(saeb), 25(enem), 10 (encceja)
  nivelitem <- nivelitem[[3]]
  
  level <- ifelse(substr(nivelitem$level, 1, 1) == "<" | nivelitem$level == "N.A.", NA, nivelitem$level)
  
  matrix_colunas <- cbind(it = 1:nrow(nivelitem), niv_ant = as.numeric(level) - 10, niv = as.numeric(level))
  
  rownames(matrix_colunas) <- paste0('Item', sprintf("%02d", 1:nrow(matrix_colunas)))
  
  calc_ponto65 <- function(x){
    lin <- x[1]
    niv_ant <- as.character(x[2])
    niv <- as.character(x[3])
    
    num <- 0.65 - nivelitem[lin, niv_ant]
    den <- nivelitem[lin, niv] - nivelitem[lin, niv_ant]
    ponto65emp <- as.integer(niv) + breaks * num / den
    ponto65emp
  }
  
  
  ponto65emp <- round2(x = apply(matrix_colunas[!is.na(matrix_colunas[,2]), ], 1, calc_ponto65), n = ndec)
  
  ponto65emp[rownames(matrix_colunas)]
  
  ponto65emp
}


#Store(ponto65emp, lib = funcoes, lib.loc = dirEncceja18)
