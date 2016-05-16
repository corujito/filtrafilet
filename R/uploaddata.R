uploaddata <- function(csvfile, ...){
  #init
  #column_name_options <<- c("Publication.Year", "Authors", "Source.Title", "V5")
  #init end

  if(substring(tolower(csvfile), nchar(csvfile)-3) != ".csv"){
    stop('Uploaded data needs to be .csv file. When using MS Excel, try "Save As" and select type "CSV (comma delimited)"');
  }
  
  #read csv data
  c <- read.csv(csvfile, row.names = NULL, stringsAsFactors=FALSE, ...);
  # eliminando duplicidade por título do artigo #
  dados <- c[!duplicated(c$Title), ]
  
  #c <- read.csv(csvfile, sep=",", stringsAsFactors=FALSE)

  dados$Source.Title <-toupper(dados$Source.Title) #Deixando o título dos Journals todo com letra maiuscula, pois será utilizado como chave
    
  ## Tirando duplicidade do arquivo de JCR ##
  jcr <- jcr[!duplicated(jcr$V2), ]
  
  #Deixando o título dos Journals todo com letra maiuscula, pois será utilizado como chave
  jcr[,2] <- toupper(jcr[,2]) 

  mydata <- merge(x=dados, y=jcr, by.x = "Source.Title", by.y = "Full.Journal.Title", all.x = TRUE)
  mydata <- mydata[!duplicated(mydata$Title), ]
  
  #convert columns with 7 or less levels to factors
  for(i in seq_along(mydata)){
    if(length(unique(mydata[[i]])) < 8){
      mydata[[i]] <- as.factor(mydata[[i]]);
    }
  }
  
  ######################
  # tratamento da base #
  ######################
  
  #deixando JCR como numérico 
  mydata$V5 <- as.numeric(as.character(mydata$V5))
  mydata$Total.Citations <- as.numeric(as.character(mydata$Total.Citations))
  
  # Atribuindo -1 para periódico sem JCR
  mydata[is.na(mydata$V5), 97] <- -1 #utilizar nome da coluna no lugar de posicao???????
  
  # Total citacoes está NA para quando não tem citações. Trocando NA por zero.
  mydata[is.na(mydata$Total.Citations), 20] <- 0 #utilizar nome da coluna no lugar de posicao???????
  
  list(
    message = paste(nrow(dados), nrow(jcr), nrow(mydata))
  )
  
  #return dataset
  return(mydata)  
}
