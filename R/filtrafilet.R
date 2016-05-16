filtrafilet <- function(dados, jcrmin, anomin, citano, porcpareto) {
    jcrmin  <- as.numeric(jcrmin)
    anomin  <- as.numeric(anomin)
    citano  <- as.numeric(citano)
    porcpareto  <- as.numeric(porcpareto)
	
    # ## PRIMEIRO CRITÉRIO DE INCLUSÃO: JCR > 2##
    filtro.jcr <- subset(dados, as.numeric(dados$Journal.Impact.Factor)>=jcrmin)
   
    # ## SEGUNDO CRITÉRIO DE INCLUSÃO: APENAS ARTIGOS RECENTES (ÚLTIMOS 2 ANOS) ##
    filtro.artigos.recentes <- subset(filtro.jcr, filtro.jcr$Publication.Year >= as.numeric(format(Sys.Date(), "%Y"))-anomin)# & filtro.jcr$Average.per.Year >= citano)
    
    # ## TERCEIRO CRITÉRIO DE INCLUSÃO: PARETO POR NRO DE CITACOES (85%) DOS ARTIGOS ANTIOS (ANTES DOS ÚLTIMOS anomin ANOS) ##
    filtro.artigos.antigos <- subset(filtro.jcr, filtro.jcr$Publication.Year < as.numeric(format(Sys.Date(), "%Y"))-anomin)
    
    list(
    message = paste(nrow(filtro.jcr), nrow(filtro.artigos.recentes), nrow(filtro.artigos.antigos))
    )

    # soma.citacoes <- 0
    # # filtro.artigos.antigos[150,20] <- 0
    # # Obtendo o total de citações #
    # for (i in 1:nrow(filtro.artigos.antigos)) {
    #   aux <- filtro.artigos.antigos[i,20] # melhorar!! Total.Citations
    #   soma.citacoes = soma.citacoes + aux
    # }
    
    # # Cálculo da % de citações de cada artigo em relação ao total de citações #
    # filtro.artigos.antigos["Porcentagem.Citacao"] <- 0 #criando uma nova coluna chamada Porcentagem.Citacao
    # filtro.artigos.antigos$Porcentagem.Citacao <- 100*filtro.artigos.antigos$Total.Citations/ soma.citacoes
    # artigos.antigos <- filtro.artigos.antigos[order(-filtro.artigos.antigos$Porcentagem.Citacao),] #ordenando por nro de citação
    
    # # Cálculo da % acumulada de citações #
    # artigos.antigos["porcentagem.acumulada"] <- 0 #criando uma nova coluna chamada porcentagem.acumulada
    # for (i in 2:nrow(filtro.artigos.antigos)) {
    #   artigos.antigos$porcentagem.acumulada[1] = artigos.antigos$Porcentagem.Citacao[1]
    #   aux <- artigos.antigos$Porcentagem.Citacao[i]
    #   artigos.antigos$porcentagem.acumulada[i] = artigos.antigos$porcentagem.acumulada[i-1] + aux
    # }
    
    # filtro.pareto <- subset(artigos.antigos, artigos.antigos$porcentagem.acumulada < porcpareto)
    
    # # filtro.pareto <- subset(filtro.pareto, select = -c(100,101))
    
    # ## Artigos finais ##
    # #print(ncol(filtro.artigos.recentes))
    # #print(ncol(filtro.pareto))

    # artigos.finais <- rbind(filtro.artigos.recentes[,1:nrow(filtro.artigos.recentes)],filtro.pareto[,1:nrow(filtro.artigos.recentes)])

    # ## SAÍDA 1 ##
    # quantidade.filtros <- cbind(c(nrow(dados), nrow(filtro.jcr), nrow(filtro.artigos.recentes), nrow(filtro.artigos.antigos), nrow(filtro.pareto), 
    #                                 nrow(artigos.finais)),c("Quantidade de documentos inciais", "Filtro JCR", "Artigos recentes", 
    #                                 "Artigos antigos", "Filtro Pareto", "Quantidade de artigos selecionados"))
   
    # library(gridExtra)

    # grid.table(quantidade.filtros, cols = c("Quantidade","Filtros")) # TABELA ESTILO GRÁFICO - PARA APARECER NA TELA

    # # ## SAÍDA 2 ##

    # ## Criando arquivo de bibliografi ##
    # citacao <- 0
    # for (i in 1:nrow(artigos.finais)){
    #       citacao[i] <- paste(artigos.finais$Authors[i],". ", artigos.finais$Title[i],". ", artigos.finais$Source.Title[i],", vol.", artigos.finais$Volume[i], 
    #                  ", p. ", artigos.finais$Beginning.Page[i], "-", artigos.finais$Ending.Page[i], ", ",  artigos.finais$Publication.Year[i], ".", sep="")
    #      }
    
    # write.table(citacao, "/Users/renatapelissari/Downloads/artigos_bibliografia2", sep = " ") # ARQUIVO PARA BIBLIOGRAFIA
    
    # ## SAÍDA 3 ##
    # ## Arquivo com nome dos artigos e numero de citações##
    # nome.artigos<- cbind(artigos.finais[,2], artigos.finais$Total.Citations)
    # nome.artigos <- nome.artigos[order(as.numeric(nome.artigos[,2]), decreasing=TRUE),]

    # write.table(nome.artigos, "/Users/renatapelissari/Downloads/nome_artigos_citacoes", sep="\t")
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
        plot(table(mydata[[nameColumnToPlot]]), type="l", col="blue", xlab = "Ano de publicação", ylab = "Quantidade de artigos", main = "Quantidade de artigos publicados por ano")
    } else if (nameColumnToPlot == "Authors") {
      par(las=3) # para deixar o nome dos autores na vertical  
      barplot(tabela.autor[1:20,2], names.arg = tabela.autor[1:20,1], ylab ="Quantidade de artigos", col=rainbow(20), main="Quantidade de artigos dos 20 autores com maior quantidade de publicação")
    } else if (nameColumnToPlot == "Source.Title") {
        plot(table(mydata[[nameColumnToPlot]]), type="l", col="blue", xlab = "Ano de publicação", ylab = "Quantidade de artigos", main = "Quantidade de artigos por revista")
    } else if (nameColumnToPlot == "V5") {
        plot(table(mydata[[nameColumnToPlot]]), type="l", col="blue", xlab = "Ano de publicação", ylab = "Quantidade de artigos", main = "JCR")
    }
}