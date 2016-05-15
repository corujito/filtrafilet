column_name_options <<- c("Publication.Year", "Authors", "Source.Title", "V5")
datainfo <- function(mydata){
  list(
    #all = as.list(colnames(mydata)),
    all = as.list(column_name_options),
    strings = as.list(names(which(unlist(lapply(mydata, is.character))))),
    other = as.list(names(which(unlist(lapply(mydata, function(x){!is.character(x)})))))
  )
}
