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
  jcr <- jcr2[!duplicated(jcr2$Full.Journal.Title), ] #retirando duplicidade do arquivo jcr
  
  mydata <- merge(x=c, y=jcr, by.x = "Source.Title", by.y = "Full.Journal.Title", all.x = TRUE)
  mydata <- mydata[!duplicated(mydata$Title), ]
  
  #convert columns with 7 or less levels to factors
  for(i in seq_along(mydata)){
    if(length(unique(mydata[[i]])) < 8){
      mydata[[i]] <- as.factor(mydata[[i]]);
    }
  }
  
  list(
    message = paste(nrow(jcr), nrow(jcr2), nrow(mydata), nrow(c))
  )
  
  ## Tratamento de algumas variáveis da base ##
  
  ## Transformando campo jcr em numerico ##
  mydata$Journal.Impact.Factor <- as.numeric(as.character(mydata$Journal.Impact.Factor))
  
  ## Transformando campo total.citations em numerico ##
  mydata$Total.Citations <- as.numeric(as.character(mydata$Total.Citations))
  
  ## Transformando campo publication.year em numerico ##
  mydata$Publication.Year <- as.numeric(as.character(mydata$Publication.Year))
  
  ## Substituindo NA de jcr e total de citacoes por 0 ##
  for (i in which(is.na(mydata$Journal.Impact.Factor)==T)){mydata$Journal.Impact.Factor[i] = -1}  #melhorar codigo sem utilizar for
  for (i in which(is.na(mydata$Total.Citations)==T)){mydata$Total.Citations[i] = 0} #melhorar codigo sem utilizar for
  
  #return dataset
  return(mydata)  
}
