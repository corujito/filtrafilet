datainfo <- function(mydata){
  list(
    #all = as.list(colnames(mydata)),
    all = as.list(c("Publication.Year", "Authors", "Source.Title", "V5")),
    strings = as.list(names(which(unlist(lapply(mydata, is.character))))),
    other = as.list(names(which(unlist(lapply(mydata, function(x){!is.character(x)})))))
  )
}
