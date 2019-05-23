
write.ItemLevel <- function(itemlevel,caminho){
  xlsx::write.xlsx(x = get(itemlevel)[[3]], file = paste0(caminho,itemlevel,'.xlsx'), col.names = TRUE, row.names = FALSE, sheetName = 'NivelItem',showNA = FALSE)
  xlsx::write.xlsx(x = get(itemlevel)[[1]], file = paste0(caminho,itemlevel,'.xlsx'), col.names = TRUE, row.names = FALSE, sheetName = 'NumAluTot',showNA = FALSE, append = TRUE)
  xlsx::write.xlsx(x = get(itemlevel)[[2]], file = paste0(caminho,itemlevel,'.xlsx'), col.names = TRUE, row.names = FALSE, sheetName = 'NumAluGab',showNA = FALSE, append = TRUE)
}

