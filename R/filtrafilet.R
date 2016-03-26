filtrafilet <- function(mydata){

}

geragrafico <- function(mydata, nameColumnToPlot, ...){
  plot(table(mydata$Publication.Year), type="l", col="blue", xlab = "Ano de publicação", 
          ylab = "Quantidade de artigos", main = "Quantidade de artigos publicados por ano")
}