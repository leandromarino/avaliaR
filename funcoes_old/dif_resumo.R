# temp <- function(...) {
#   input_list <- as.list(substitute(list(...)))[-1L]
#   input_list
# }
# input_list <- s

dif_resumo <- function(...) {
  input_list <- as.list(substitute(list(...)))[-1L]

  output <- list()
  for(i in 1:length(input_list)){
    x <- get(as.character(input_list[[i]]))$InfoItem
    aux <- colnames(x)[8]
    x$tipodif_G1_x_G2 <- gsub(pattern = "_", replacement = " ", x = aux)
    colnames(x)[1] <- 'codbni'
    colnames(x) <- gsub(pattern = unlist(strsplit(aux,"_"))[1], replacement = "G1", colnames(x))
    colnames(x) <- gsub(pattern = unlist(strsplit(aux,"_"))[3], replacement = "G2", colnames(x))
    colnames(x)[9:14] <- gsub(pattern = 'G1', replacement = "G1_", colnames(x)[9:14])
    colnames(x)[9:14] <- gsub(pattern = 'G2', replacement = "G2_", colnames(x)[9:14])
    output[[i]] <- x
  }
  output <- do.call(rbind, output)
  output <- output[order(output$codbni), ]
  output
}

# Store(dif_resumo, lib = funcoes, lib.loc = dirEncceja18)

#  x <- dif_resumo(difEnccPT12_Encc18CH_EMv1, difEnccPT14_Encc18CH_EMv1, difEnccPT16_Encc18CH_EMv1, difEncc14_Encc18CH_EMv1)
#  x
