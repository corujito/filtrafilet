filtrafilet <- function(mydata){

}

geragrafico <- function(mydata, nameColumnToPlot, ...){
  # barplot(table(mydata$nameColumnToPlot)[1:10,], type="l", col="blue", xlab = "Ano de publicação", 
  #         ylab = "Quantidade de artigos", main = "Quantidade de artigos publicados por ano")
	hist(as.numeric(mydata$nameColumnToPlot), nclass = 4, xlab = "JCR", ylab = "Quantidade de artigos", 
          main = "Distribuição de artigos por JCR", col="blue")
}