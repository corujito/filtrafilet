filtrafilet <- function(dados, jcrmin, anomin, citano, porcpareto) {
    jcrmin  <- as.numeric(jcrmin)
    anomin  <- as.numeric(anomin)
    citano  <- as.numeric(citano)
    porcpareto  <- as.numeric(porcpareto)
	## Base com todos os artigos selecionados ##
    #dados <- rbind(dados1, dados2)
    dados[,6] <-toupper(dados$Source.Title) #Deixando o título dos Journals todo com letra maiuscula, pois será utilizado como chave
    
    ## Base com JCR ##
    #jcr2 <- read.csv('data/jcr2.csv', header = F, sep = ",")
    #jcr2 <- jcr2[-1,]
    jcr2[,2] <- toupper(jcr2[,2]) #Deixando o título dos Journals todo com letra maiuscula, pois será utilizado como chave

    dados.jcr <- merge(x=dados, y=jcr2, by.x = "Source.Title", by.y = "Full.Journal.Title", all.x = TRUE)
    
    # ## PRIMEIRO CRITÉRIO DE INCLUSÃO: JCR > 2##
    filtro.jcr <- subset(dados.jcr, as.numeric(dados.jcr$Journal.Impact.Factor)>=jcrmin)
   
    # ## SEGUNDO CRITÉRIO DE INCLUSÃO: APENAS ARTIGOS RECENTES (ÚLTIMOS 2 ANOS) ##
    filtro.artigos.recentes <- subset(filtro.jcr, filtro.jcr$Publication.Year >= as.numeric(format(Sys.Date(), "%Y"))-anomin & filtro.jcr$Average.per.Year >= citano)
    
    # ## TERCEIRO CRITÉRIO DE INCLUSÃO: PARETO POR NRO DE CITACOES (85%) DOS ARTIGOS ANTIOS (ANTES DOS ÚLTIMOS anomin ANOS) ##
    filtro.artigos.antigos <- subset(filtro.jcr, filtro.jcr$Publication.Year < as.numeric(format(Sys.Date(), "%Y"))-anomin)
    
    soma.citacoes <- 0
    # filtro.artigos.antigos[150,20] <- 0
    # Obtendo o total de citações #
    for (i in 1:nrow(filtro.artigos.antigos)) {
      aux <- filtro.artigos.antigos[i,20] # melhorar!! Total.Citations
      soma.citacoes = soma.citacoes + aux
    }
    
    # Cálculo da % de citações de cada artigo em relação ao total de citações #
    filtro.artigos.antigos["Porcentagem.Citacao"] <- 0 #criando uma nova coluna chamada Porcentagem.Citacao
    filtro.artigos.antigos$Porcentagem.Citacao <- 100*filtro.artigos.antigos$Total.Citations/ soma.citacoes
    artigos.antigos <- filtro.artigos.antigos[order(-filtro.artigos.antigos$Porcentagem.Citacao),] #ordenando por nro de citação
    
    # Cálculo da % acumulada de citações #
    artigos.antigos["porcentagem.acumulada"] <- 0 #criando uma nova coluna chamada porcentagem.acumulada
    for (i in 2:nrow(filtro.artigos.antigos)) {
      artigos.antigos$porcentagem.acumulada[1] = artigos.antigos$Porcentagem.Citacao[1]
      aux <- artigos.antigos$Porcentagem.Citacao[i]
      artigos.antigos$porcentagem.acumulada[i] = artigos.antigos$porcentagem.acumulada[i-1] + aux
    }
    
    filtro.pareto <- subset(artigos.antigos, artigos.antigos$porcentagem.acumulada < porcpareto)

    list(
    	message = paste(ncol(filtro.pareto), nrow(filtro.pareto), ncol(filtro.artigos.recentes), colnames(filtro.pareto), colnames(filtro.artigos.recentes))
  	)
    
    # filtro.pareto <- subset(filtro.pareto, select = -c(101,102))
    
    # ## Artigos finais ##
    # #print(ncol(filtro.artigos.recentes))
    # #print(ncol(filtro.pareto))
    
    # artigos.finais <- rbind(filtro.artigos.recentes[,1:100],filtro.pareto[,1:100])

    # ## SAÍDA 1 ##
    # quantidade.filtros <- cbind(c(nrow(dados), nrow(filtro.jcr), nrow(filtro.artigos.recentes), nrow(filtro.artigos.antigos), nrow(filtro.pareto), 
    #                                 nrow(artigos.finais)),c("Quantidade de documentos inciais", "Filtro JCR", "Artigos recentes", 
    #                                 "Artigos antigos", "Filtro Pareto", "Quantidade de artigos selecionados"))
   
    # library(gridExtra)

    # # ## SAÍDA 2 ##
    # grid.table(quantidade.filtros, cols = c("Quantidade","Filtros")) # TABELA ESTILO GRÁFICO - PARA APARECER NA TELA

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
  plot(table(mydata$Publication.Year), type="l", col="blue", xlab = "Ano de publicação", 
          ylab = "Quantidade de artigos", main = "Quantidade de artigos publicados por ano")
}