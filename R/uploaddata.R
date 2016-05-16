uploaddata <- function(csvfile, ...){
  #init
  #column_name_options <<- c("Publication.Year", "Authors", "Source.Title", "V5")
  #init end

  if(substring(tolower(csvfile), nchar(csvfile)-3) != ".csv"){
    stop('Uploaded data needs to be .csv file. When using MS Excel, try "Save As" and select type "CSV (comma delimited)"');
  }
  
  #read csv data
  c <- read.csv(csvfile, row.names = NULL, stringsAsFactors=FALSE, ...);
  #c <- read.csv(csvfile, sep=",", stringsAsFactors=FALSE)

  c$Source.Title <-toupper(c$Source.Title) #Deixando o título dos Journals todo com letra maiuscula, pois será utilizado como chave
    
  ## Base com JCR ##
  #jcr2 <- read.csv('data/jcr2.csv', header = F, sep = ",")
  #jcr2 <- jcr2[-1,]
  jcr2[,2] <- toupper(jcr2[,2]) #Deixando o título dos Journals todo com letra maiuscula, pois será utilizado como chave

  mydata <- merge(x=c, y=jcr2, by.x = "Source.Title", by.y = "Full.Journal.Title", all.x = TRUE)
  
  #convert columns with 7 or less levels to factors
  for(i in seq_along(mydata)){
    if(length(unique(mydata[[i]])) < 8){
      mydata[[i]] <- as.factor(mydata[[i]]);
    }
  }
  
  #return dataset
  return(mydata)  
}
