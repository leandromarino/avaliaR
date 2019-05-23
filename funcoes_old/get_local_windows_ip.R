get_local_windows_ip <- function(){
  x <- system("ipconfig", intern=TRUE)
  x <- removeBrancos(unlist(strsplit(x[grep("IPv4", x)], ":"))[2])
  x
}
