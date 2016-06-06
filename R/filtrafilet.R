filtrafilet <- function (dados, comjcr, jcrmin, anomin, citano, porcpareto) {
  dados_original_row <- nrow(dados) #dados é a saida o upload
  dados <- dados[!duplicated(dados$Title), ]
  jcrmin  <- as.numeric(jcrmin)
  anomin  <- as.numeric(anomin)
  citano  <- as.numeric(citano)
  porcpareto  <- as.numeric(porcpareto)
  comjcr <- as.numeric(comjcr)
  
  if (comjcr == 0) {
    nome.coluna.jcr <- paste("Artigos sem JCR")
  } else if (comjcr == 1) {
    nome.coluna.jcr <- paste("Artigos com JCR >= ", jcrmin)
  } else if (comjcr == 2) {
    nome.coluna.jcr <- paste("Artigos sem JCR e artigos com JCR >= ", jcrmin)
  }
  
  filtro.jcr = filtro.jcr(dados, comjcr, jcrmin)
  
  filtro.artigos.recentes = filtro.recentes(filtro.jcr, anomin)
  
  artigos.recentes.citacao = filtro.recentes.citacao(filtro.artigos.recentes, citano)
  
  filtro.artigos.antigos = filtro.antigos(filtro.jcr, anomin)
  
  artigos.antigos = filtro.antigos.pareto(filtro.artigos.antigos, porcpareto)
 
  ## Artigos finais ##
  
  artigos.finais <<- rbind(artigos.recentes.citacao,artigos.antigos)
  quantidade.filtros <- cbind(
    c(dados_original_row,
      nrow(dados),
      nrow(filtro.jcr),
      nrow(filtro.artigos.recentes),
      nrow(artigos.recentes.citacao),
      nrow(filtro.artigos.antigos),
      nrow(artigos.antigos), 
      nrow(artigos.finais)),
    c("Total de documentos",
      "Documentos sem duplicação",
      nome.coluna.jcr,
      paste("Artigos recentes: publicados a partir do ano ", as.numeric(format(Sys.Date(), "%Y"))-anomin),
      paste("Artigos recentes com média anual de citações >= ", citano),
      paste("Artigos antigos: publicados antes do ano ", as.numeric(format(Sys.Date(), "%Y"))-anomin),
      paste("Artigos antigos que juntos representam", porcpareto,"%", "da quantidade total de citações (regra de Pareto)"),
      "Quantidade de artigos selecionados"))
  
  ## Criando arquivo de bibliografia ##
  bibliografia <- 0
  for (i in 1:nrow(artigos.finais)) {
    bibliografia[i] <- paste(
      artigos.finais$Authors[i], ". ",
      artigos.finais$Title[i], ". ",
      artigos.finais$Source.Title[i], ", vol.",
      artigos.finais$Volume[i], ", p. ",
      artigos.finais$Beginning.Page[i], "-",
      artigos.finais$Ending.Page[i], ", ",
      artigos.finais$Publication.Year[i], ".", sep=""
      )
  }
  
  write.table(bibliografia, "bibliografia.txt", sep = " ")
  
  ## Craindo Arquivo com nome dos artigos e numero de citações ##
  nome.artigos <- cbind(artigos.finais$Title, artigos.finais$Total.Citations, artigos.finais$V5)
  nome.artigos <- nome.artigos[order(as.numeric(nome.artigos[,2]), decreasing=TRUE),]
  colnames(nome.artigos) <- c("Título", "Número de citações")
  
  write.table(nome.artigos, "nome_artigos_citacoes.txt", sep="\t")
  
  #list(message = paste(quantidade.filtros[1], quantidade.filtros[2]))
  plot(1, type="n", axes=F, xlab="", ylab="")
  gridExtra::grid.table(quantidade.filtros, cols = c("Quantidade","Filtros"))
}

### PRIMEIRO CRITÉRIO DE INCLUSÃO: JCR MIN ###
filtro.jcr <- function (dados, comjcr, jcrmin) {
  if (comjcr == 0) {
    filtro.jcr <- subset(dados, as.numeric(as.character(dados$Journal.Impact.Factor)) < 0)
  } else if (comjcr == 1) {
    filtro.jcr <- subset(dados, as.numeric(as.character(dados$Journal.Impact.Factor)) >= jcrmin)
  } else if (comjcr == 2) {
    filtro.jcr <- subset(dados, as.numeric(as.character(dados$Journal.Impact.Factor)) >= jcrmin | as.numeric(as.character(dados$Journal.Impact.Factor))<0)
  }
  if (nrow(filtro.jcr) == 0) {
    stop('Não foi aplicado filtro de JCR - todos os journals com valor de JCR inferior ao selecionado');
  }
  filtro.jcr <- filtro.jcr[!duplicated(filtro.jcr$Title), ]
  return(filtro.jcr) 
}

### SEGUNDO CRITÉRIO DE INCLUSÃO: APENAS ARTIGOS RECENTES ###
filtro.recentes <- function(filtro.jcr, anomin){
  filtro.artigos.recentes <- subset(filtro.jcr, as.numeric(filtro.jcr$Publication.Year) >= as.numeric(format(Sys.Date(), "%Y"))-anomin)
  return(filtro.artigos.recentes)
}
 
filtro.recentes.citacao <- function(filtro.artigos.recentes, citano){
  # Filtro por número de citacoes anuais dos artigos recentes #
  if (nrow(filtro.artigos.recentes) > 0) { # se houver artigos recentes, então aplica o filtro de citacao
    idade.artigo <- 0
    for (i in 1:nrow(filtro.artigos.recentes)) {
      if (filtro.artigos.recentes$Publication.Year[i] >= as.numeric(format(Sys.Date(), "%Y"))) {
        idade.artigo[i] <- 1
      } else {
        idade.artigo[i] <- ((as.numeric(format(Sys.Date(), "%Y"))) - as.numeric(filtro.artigos.recentes$Publication.Year[i]))
      }
    }
    artigos.recentes.citacao <- filtro.artigos.recentes[which(as.numeric(filtro.artigos.recentes$Total.Citations)/idade.artigo >= citano), ]
  } else {
    artigos.recentes.citacao <- filtro.artigos.recentes
  }
  return(artigos.recentes.citacao)
}

### TERCEIRO CRITÉRIO DE INCLUSÃO: PARETO POR NRO DE CITACOES (85%) DOS ARTIGOS ANTIOS (ANTES DOS ÚLTIMOS anomin ANOS) ###
filtro.antigos <- function(filtro.jcr, anomin){
  filtro.artigos.antigos <- subset(filtro.jcr, as.numeric(filtro.jcr$Publication.Year) < as.numeric(format(Sys.Date(), "%Y"))-anomin)
}

filtro.antigos.pareto <- function(filtro.artigos.antigos, porcpareto){
  # Filtro pareto dos artigos antigos #
  if (nrow(filtro.artigos.antigos) > 0) { # se houver artigos antigos, então aplica o filtro de citacao
    soma.citacoes <- 0
    # Obtendo o total de citações #
    for (i in 1:nrow(filtro.artigos.antigos)) {
      if (is.na(filtro.artigos.antigos$Total.Citations[i] ==T)) { filtro.artigos.antigos$Total.Citations[i]=0 }
      aux <- filtro.artigos.antigos$Total.Citations[i]
      soma.citacoes = soma.citacoes + aux
    }
    
    # Cálculo da % de citações de cada artigo em relação ao total de citações #
    filtro.artigos.antigos["Porcentagem.Citacao"] <- 0 #criando uma nova coluna chamada Porcentagem.Citacao
    filtro.artigos.antigos$Porcentagem.Citacao <- 100*filtro.artigos.antigos$Total.Citations/ soma.citacoes
    artigos.antigos <- filtro.artigos.antigos[order(-filtro.artigos.antigos$Porcentagem.Citacao),] #ordenando por nro de citação
    
    # Cálculo da % acumulada de citações #
    artigos.antigos["porcentagem.acumulada"] <- 0 #criando uma nova coluna chamada porcentagem.acumulada
    if (nrow(filtro.artigos.antigos) < 2) {
      artigos.antigos$porcentagem.acumulada <- 100
    } else {
      for (i in 2:nrow(filtro.artigos.antigos)) {
        artigos.antigos$porcentagem.acumulada[1] = artigos.antigos$Porcentagem.Citacao[1]
        aux <- artigos.antigos$Porcentagem.Citacao[i]
        artigos.antigos$porcentagem.acumulada[i] = artigos.antigos$porcentagem.acumulada[i-1] + aux
      }
    }
    
    artigos.antigos.pareto <- subset(artigos.antigos, artigos.antigos$porcentagem.acumulada <= porcpareto)
    
    artigos.antigos <- subset(artigos.antigos.pareto, select = -c(ncol(artigos.antigos.pareto)-1, ncol(artigos.antigos.pareto)))
  } else {
    artigos.antigos <- filtro.artigos.antigos
  }
}

geragrafico <- function (mydata, nameColumnToPlot, ...) {
  mydata <- mydata[!duplicated(mydata$Title), ]
  analise.autor <- strsplit(mydata$Authors, ";")
  ## Primeiro autor ##
  prim.autor <- NULL
  library(stringr)
  for (i in 1:nrow(mydata)) {
    aux <- str_trim(analise.autor[[i]][1])
    prim.autor <- c(aux, prim.autor)
  }
  tabela.prim.autor <- as.data.frame(table(prim.autor))
  tabela.prim.autor <- tabela.prim.autor[order(as.numeric(tabela.prim.autor[,2]), decreasing=TRUE),]
    
  ## Autor ##
  autor <- NULL
  library(stringr)
  for (i in 1:length(analise.autor)) {
    for(j in 1:length(analise.autor[[i]])) {
      aux <- str_trim(analise.autor[[i]][1])
      autor <- c(aux, autor) 
    }
  }
  tabela.autor <- as.data.frame(table(autor))
  tabela.autor <- tabela.autor[order(as.numeric(tabela.autor[,2]), decreasing=TRUE),]
    
  ## Journals ##
  tabela.revistas <- as.data.frame(table(mydata$Source.Title)) # contagem de artigos por revista
  tabela.revistas <- tabela.revistas[order(as.numeric(tabela.revistas[,2]), decreasing=TRUE),] # colocando a contagem em ordem decrescente (revistas com mais artigos)

  if (nameColumnToPlot == "Publication.Year") {
      par(las=1)
      par(mar=c(10, 4.1, 4.1, 2.1)) 
      min <- min(as.numeric(names(table(mydata[[nameColumnToPlot]]))))
      max <- max(as.numeric(names(table(mydata[[nameColumnToPlot]]))))
      quantidade.ano <- table(factor(mydata[[nameColumnToPlot]], levels=min:max))
      plot(quantidade.ano, type="l", col="blue", xlab = "Ano de publicação", ylab = "Quantidade de artigos", main = "Quantidade de artigos publicados por ano", yaxs = "i")
  } else if (nameColumnToPlot == "Author") {
      if (nrow(tabela.autor) < 20){aux <- nrow(tabela.autor)} else {aux <- 20}
      par(las=3) # para deixar o nome dos autores na vertical
      par(mar=c(10, 4.1, 4.1, 2.1)) # aumentando espaço do gráfico para caber nome dos autores
      barplot(tabela.autor[1:aux,2], names.arg = tabela.autor[1:aux,1], ylab ="Quantidade de artigos", col=rainbow(aux), main=paste("Quantidade de artigos dos ", aux, " autores com maior quantidade de publicação"))
  } else if (nameColumnToPlot == "First Author") {
    if (nrow(tabela.prim.autor) < 20){aux <- nrow(tabela.prim.autor)} else {aux <- 20}
    par(las=3) # para deixar o nome dos autores na vertical
    par(mar=c(10, 4.1, 4.1, 2.1)) # aumentando espaço do gráfico para caber nome dos autores
    barplot(tabela.prim.autor[1:aux,2], names.arg = tabela.prim.autor[1:aux,1], ylab ="Quantidade de artigos", col=rainbow(aux), main=paste("Quantidade de artigos dos ", aux, " primeiros autores com maior quantidade de publicação"))
  } else if (nameColumnToPlot == "Source.Title") {
    if (nrow(tabela.revistas) < 10){aux <- nrow(tabela.revistas)} else {aux <- 10}
    par(las=2)
    par(mar=c(25, 4.1, 4.1, 2.1)) 
    barplot(tabela.revistas[1:aux,2], names.arg = tabela.revistas[1:aux,1], ylab ="Quantidade de artigos", col=rainbow(aux), main=paste("Quantidade de artigos das ", aux, " revistas com maior quantidade de publicação"))
  } else if (nameColumnToPlot == "JCR") {
    if (max(mydata$V5) - min(mydata$V5[(which(mydata$V5 > 0))]) < 30){aux <- 6} else {aux <- 15}
    hist(mydata$Journal.Impact.Factor[(which(mydata$Journal.Impact.Factor > 0))], nclass = aux, xlab = "JCR", ylab = "Quantidade de artigos", main = "Distribuição de artigos por JCR - apenas periódicos com JCR", col="blue")
  }
}