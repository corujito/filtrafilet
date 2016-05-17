filtrafilet <- function(dados, jcrmin, anomin, citano, porcpareto) {
    jcrmin  <- as.numeric(jcrmin)
    anomin  <- as.numeric(anomin)
    citano  <- as.numeric(citano)
    porcpareto  <- as.numeric(porcpareto)
	
    length(which(dados$Journal.Impact.Factor == -1)) 
    # ## PRIMEIRO CRITÉRIO DE INCLUSÃO: JCR > 2##
    filtro.jcr <- subset(dados, as.numeric(as.character(dados$Journal.Impact.Factor))>=jcrmin)
   
    # ## SEGUNDO CRITÉRIO DE INCLUSÃO: APENAS ARTIGOS RECENTES (ÚLTIMOS 2 ANOS) ##
    filtro.artigos.recentes <- subset(filtro.jcr, as.numeric(filtro.jcr$Publication.Year) >= as.numeric(format(Sys.Date(), "%Y"))-anomin)
    #filtro.jcr$Average.per.Year >= citano
    
    # Filtro por número de citacoes anuais dos artigos recentes #
    if (nrow(filtro.artigos.recentes) > 0){ # se houver artigos recentes, então aplica o filtro de citacao
      idade.artigo <- 0
      for (i in 1:nrow(filtro.artigos.recentes)){
        if (filtro.artigos.recentes$Publication.Year[i] >= as.numeric(format(Sys.Date(), "%Y"))){
          idade.artigo[i] <- 1
        } else {idade.artigo[i] <- ((as.numeric(format(Sys.Date(), "%Y"))) - as.numeric(filtro.artigos.recentes$Publication.Year[i]))}
      }
      artigos.recentes.citacao <- filtro.artigos.recentes[which(as.numeric(filtro.artigos.recentes$Total.Citations)/idade.artigo >= citano), ]
    } else {artigos.recentes.citacao <- filtro.artigos.recentes}
    
    # ## TERCEIRO CRITÉRIO DE INCLUSÃO: PARETO POR NRO DE CITACOES (85%) DOS ARTIGOS ANTIOS (ANTES DOS ÚLTIMOS anomin ANOS) ##
    filtro.artigos.antigos <- subset(filtro.jcr, as.numeric(filtro.jcr$Publication.Year) < as.numeric(format(Sys.Date(), "%Y"))-anomin)
    
    # Filtro pareto dos artigos antigos #
    if (nrow(filtro.artigos.antigos) > 0){ # se houver artigos antigos, então aplica o filtro de citacao
      soma.citacoes <- 0
      # Obtendo o total de citações #
      for (i in 1:nrow(filtro.artigos.antigos)) {
        if (is.na(filtro.artigos.antigos$Total.Citations[i] ==T)){filtro.artigos.antigos$Total.Citations[i]=0}
        aux <- filtro.artigos.antigos$Total.Citations[i]
        soma.citacoes = soma.citacoes + aux
      }
      
      # Cálculo da % de citações de cada artigo em relação ao total de citações #
      filtro.artigos.antigos["Porcentagem.Citacao"] <- 0 #criando uma nova coluna chamada Porcentagem.Citacao
      filtro.artigos.antigos$Porcentagem.Citacao <- 100*filtro.artigos.antigos$Total.Citations/ soma.citacoes
      artigos.antigos <- filtro.artigos.antigos[order(-filtro.artigos.antigos$Porcentagem.Citacao),] #ordenando por nro de citação
      
      # Cálculo da % acumulada de citações #
      artigos.antigos["porcentagem.acumulada"] <- 0 #criando uma nova coluna chamada porcentagem.acumulada
      if (nrow(filtro.artigos.antigos) < 2){artigos.antigos$porcentagem.acumulada <- 100} else{
        for (i in 2:nrow(filtro.artigos.antigos)) {
          artigos.antigos$porcentagem.acumulada[1] = artigos.antigos$Porcentagem.Citacao[1]
          aux <- artigos.antigos$Porcentagem.Citacao[i]
          artigos.antigos$porcentagem.acumulada[i] = artigos.antigos$porcentagem.acumulada[i-1] + aux
        }
      }
      
      artigos.antigos.pareto <- subset(artigos.antigos, artigos.antigos$porcentagem.acumulada < porcpareto)
      
      artigos.antigos <- subset(artigos.antigos.pareto, select = -c(ncol(artigos.antigos.pareto)-1, ncol(artigos.antigos.pareto)))
    } else {artigos.antigos <- filtro.artigos.antigos}
    
    ## Artigos finais ##
    
    artigos.finais <<- rbind(artigos.recentes.citacao,artigos.antigos)
    quantidade.filtros <- cbind(c(nrow(dados), nrow(filtro.jcr), nrow(filtro.artigos.recentes), nrow(artigos.recentes.citacao), nrow(filtro.artigos.antigos), nrow(artigos.antigos), 
                                  nrow(artigos.finais)),c("Quantidade de documentos sem duplicação", "Filtro JCR", "Artigos recentes", "Recentes - Filtro por número de citações por ano",
                                                          "Artigos antigos",  "Antigos - Filtro pela regra de Pareto" , "Quantidade de artigos selecionados"))
  
    library(gridExtra)
    grid.table(quantidade.filtros, cols = c("Quantidade","Filtros"))
    
    list(message = paste(nrow(dados), length(which(dados$Journal.Impact.Factor == -1)), nrow(filtro.jcr), nrow(filtro.artigos.recentes), nrow(filtro.artigos.antigos), nrow(artigos.recentes.citacao),nrow(artigos.antigos)))
}

geragrafico <- function(mydata, nameColumnToPlot, ...){
      
      analise.autor <- strsplit(mydata$Authors, ";")
      ## Primeiro autor ##
        prim.autor <- NULL
        library(stringr)
        for (i in 1:nrow(mydata))
        {
          aux <- str_trim(analise.autor[[i]][1])
          prim.autor <- c(aux, prim.autor)
        }
      ## Autor ##
        autor <- NULL
        library(stringr)
        for (i in 1:length(analise.autor))
        {
          for(j in 1:length(analise.autor[[i]]))
          {
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
        plot(table(mydata[[nameColumnToPlot]]), type="l", col="blue", xlab = "Ano de publicação", ylab = "Quantidade de artigos", main = "Quantidade de artigos publicados por ano")
    } else if (nameColumnToPlot == "Authors") {
        if (nrow(tabela.autor) < 20){aux <- nrow(tabela.autor)} else {aux <- 20}
        par(las=3) # para deixar o nome dos autores na vertical
        par(mar=c(10, 4.1, 4.1, 2.1)) # aumentando espaço do gráfico para caber nome dos autores
        barplot(tabela.autor[1:aux,2], names.arg = tabela.autor[1:aux,1], ylab ="Quantidade de artigos", col=rainbow(aux), main=paste("Quantidade de artigos dos ", aux, " autores com maior quantidade de publicação"))
    } else if (nameColumnToPlot == "Source.Title") {
      if (nrow(tabela.revistas) < 10){aux <- nrow(tabela.revistas)} else {aux <- 10}
      par(las=2)
      par(mar=c(25, 4.1, 4.1, 2.1)) 
      barplot(tabela.revistas[1:aux,2], names.arg = tabela.revistas[1:aux,1], ylab ="Quantidade de artigos", col=rainbow(aux), main=paste("Quantidade de artigos das ", aux, " revistas com maior quantidade de publicação"))
    } else if (nameColumnToPlot == "V5") {
      if (max(mydata$V5) - min(mydata$V5[(which(mydata$V5 > 0))]) < 30){aux <- 6} else {aux <- 15}
      hist(mydata$Journal.Impact.Factor[(which(mydata$Journal.Impact.Factor > 0))], nclass = aux, xlab = "JCR", ylab = "Quantidade de artigos", main = "Distribuição de artigos por JCR - apenas periódicos com JCR", col="blue")
    }
}