# exp_gp1 = expEnccPT12MT_EMv3
# gabpar_gp1 = gabparEnccPT12MT_EM
# nome_gp1 <- 'PT12'
# 
# exp_gp2 = expEncc18MT_EMv3
# gabpar_gp2 = gabparEncc18MT_EMLv3
# nome_gp2 = 'EN18'

item_comum <- function(exp_gp1, gabpar_gp1, nome_gp1, exp_gp2, gabpar_gp2, nome_gp2){
  itcom <- data.frame(nomeblg = intersect(rownames(exp_gp1), rownames(exp_gp2)),stringsAsFactors = FALSE)
  itcom <- itcom[itcom$nomeblg != 0, , drop = FALSE]
  cols <- c('codbni','nomeblg','itemblg','it')
  itcom <- merge(itcom, gabpar_gp1[,cols], by = 'nomeblg', all.x = TRUE)[,cols]
  colnames(itcom)[4] <- nome_gp1
  itcom[,nome_gp2] <- gabpar_gp2[as.character(itcom$codbni),'it']
  itcom$gab        <- gabpar_gp2[as.character(itcom$codbni),'gab']
  itcom <- itcom[order(itcom[,nome_gp2]),]
  itcom  
}


# itcom_EnccPT12_Encc18MT_EMv3 <- item_comum(exp_gp1 = expEnccPT12MT_EMv3, gabpar_gp1 = gabparEnccPT12MT_EM , nome_gp1 = 'PT12',
#                                            exp_gp2 = expEncc18MT_EMv3,   gabpar_gp2 = gabparEncc18MT_EMLv3, nome_gp2 = 'EN18')

