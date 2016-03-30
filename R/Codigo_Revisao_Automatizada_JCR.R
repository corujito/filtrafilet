## ENTRADAS DA FUNÇÃO ##
    # arquivo - endereço e nome do arquivo de entrada
    # end.salvar - endereço onde serão salvos arquivos de saída
    # jcr.min - valor mínimo de jcr aceito para selecionar documentos (valor numérico positivo. Ex: 1.5)
    # ano.min - até quantos anos atrás um artigo será considerado recentes (valor inteiro numérico. Ex: 3)
    # cit.ano - valor mínimo de citações por ano para seleção de artigos recentes (valor numério positivo)
    # por.pareto - porcentagemde a ser utilizada na regra de pareto para seleção de artigos antigos (valor entre 0 e 100)

## SAÍDAS DA FUNÇÃO ##
    # Número de artigos selecionados após a aplicação de cada um dos filtros
    # Arquivo txt com o nome dos artigos selecionados e a quantidade de citações de cada um
    # Arquivo txt com as informações dos documentos selecionados em formato de bibliografia conforme ABNT

selecaoDocumentos <- function(arquivo, jcr.min, ano.min, cit.ano, porc.pareto) {

            ## Leitura das bases selecionados no web of science ##
            dados <- read.csv("/Users/renatapelissari/Downloads/savedrecs_2.csv", sep = ",", stringsAsFactors = FALSE)
            
            ## Base com todos os artigos selecionados ##
            #dados <- rbind(dados1, dados2)
            dados[,6] <-toupper(dados$Source.Title) #Deixando o título dos Journals todo com letra maiuscula, pois será utilizado como chave
            
            ## Base com JCR ##
            jcr <- read.csv('/Users/renatapelissari/Downloads/JournalHomeGrid.csv', header = F, sep = ",")
            jcr <- jcr[-1,]
            jcr[,2] <- toupper(jcr[,2]) #Deixando o título dos Journals todo com letra maiuscula, pois será utilizado como chave
            
            dados.jcr <- merge(x=dados, y=jcr, by.x = "Source.Title", by.y = "V2", all.x = TRUE)
            
            ## PRIMEIRO CRITÉRIO DE INCLUSÃO: JCR > 2##
            
            filtro.jcr <- subset(dados.jcr, as.numeric(dados.jcr[,97])>=jcr.min)
           
            ## SEGUNDO CRITÉRIO DE INCLUSÃO: APENAS ARTIGOS RECENTES (ÚLTIMOS 2 ANOS) ##
            
            filtro.artigos.recentes <- subset(filtro.jcr, filtro.jcr$Publication.Year >= as.numeric(format(Sys.Date(), "%Y"))-ano.min & filtro.jcr$Average.per.Year >= cit.ano)
            
            ## TERCEIRO CRITÉRIO DE INCLUSÃO: PARETO POR NRO DE CITACOES (85%) DOS ARTIGOS ANTIOS (ANTES DOS ÚLTIMOS ano.min ANOS) ##
            
            filtro.artigos.antigos <- subset(filtro.jcr, filtro.jcr$Publication.Year < as.numeric(format(Sys.Date(), "%Y"))-ano.min)
            
            soma.citacoes <- 0
            #filtro.artigos.antigos[150,20] <- 0
            # Obtendo o total de citações #
            for (i in 1:nrow(filtro.artigos.antigos)) {
              aux <- filtro.artigos.antigos[i,20]
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
            
            filtro.pareto <- subset(artigos.antigos, artigos.antigos$porcentagem.acumulada < porc.pareto)
            
            filtro.pareto <- subset(filtro.pareto, select = -c(101,102))
            
            ## Artigos finais ##
            #print(ncol(filtro.artigos.recentes))
            #print(ncol(filtro.pareto))
            
            artigos.finais <- rbind(filtro.artigos.recentes[,1:100],filtro.pareto[,1:100])
            quantidade.filtros <- cbind(c(nrow(dados), nrow(filtro.jcr), nrow(filtro.artigos.recentes), nrow(filtro.artigos.antigos), nrow(filtro.pareto), 
                                            nrow(artigos.finais)),c("Quantidade de documentos inciais", "Filtro JCR", "Artigos recentes", 
                                            "Artigos antigos", "Filtro Pareto", "Quantidade de artigos selecionados"))
           
            library(gridExtra)
            grid.table(quantidade.filtros, cols = c("Quantidade","Filtros"))
       
            ## Criando arquivo de bibliografi ##
            citacao <- 0
            for (i in 1:nrow(artigos.finais)){
                  citacao[i] <- paste(artigos.finais$Authors[i],". ", artigos.finais$Title[i],". ", artigos.finais$Source.Title[i],", vol.", artigos.finais$Volume[i], 
                             ", p. ", artigos.finais$Beginning.Page[i], "-", artigos.finais$Ending.Page[i], ", ",  artigos.finais$Publication.Year[i], ".", sep="")
                 }
            
            write.table(citacao, "/Users/renatapelissari/Downloads/artigos_bibliografia2", sep = " ")
            
            ## Arquivo com nome dos artigos e numero de citações##
            nome.artigos<- cbind(artigos.finais[,2], artigos.finais$Total.Citations)
            nome.artigos <- nome.artigos[order(as.numeric(nome.artigos[,2]), decreasing=TRUE),]

            write.table(nome.artigos, "/Users/renatapelissari/Downloads/nome_artigos_citacoes", sep="\t")
}           

selecaoDocumentos("/Users/renatapelissari/Downloads/savedrecs_2.csv", 2.0, 2, 1, 80) 

