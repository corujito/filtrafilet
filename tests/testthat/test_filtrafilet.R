context("Filtra Filet")

setup <- function() {
  dados <<- uploaddata("../../scopus_2.csv", NULL)
  dadossemdupli <- dados[!duplicated(dados$Title), ]
  filtrojcr <<- filtro.jcr(dados, 1, 1)
  filtrorecentes <<- filtro.recentes(filtrojcr, 2)
  filtroantigos <<- filtro.antigos(filtrojcr, 2)
}

test_that("retirando duplicidade", {
  setup()
  dadossemdupli <- dados[!duplicated(dados$Title), ]
  expect_equal(nrow(dados), 315)
  expect_equal(nrow(dadossemdupli), 313)
  
})

test_that("filtro.jcr", {
  setup()
  comjcr = 1
  jcrmin = 1
  filtro.jcr = filtro.jcr(dados, comjcr, jcrmin)
  expect_equal(nrow(filtro.jcr), 151)
})

test_that("filtro.recentes", {
  setup()
  anomin = 2
  filtro.recentes = filtro.recentes(filtrojcr, anomin)
  expect_equal(nrow(filtro.recentes), 28)
})

test_that("filtro.recentes.citacao", {
  setup()
  citano = 1
  filtro.recentes.citacao = filtro.recentes.citacao(filtrorecentes, citano)
  expect_equal(nrow(filtro.recentes.citacao), 17)
})

test_that("filtro.antigos", {
  setup()
  anomin = 2
  filtro.antigos = filtro.antigos(filtrojcr, anomin)
  expect_equal(nrow(filtro.antigos), 123)
})

test_that("filtro.antigos.pareto", {
  setup()
  porcpareto = 80
  filtro.antigos.pareto = filtro.antigos.pareto(filtroantigos, porcpareto)
  expect_equal(nrow(filtro.antigos.pareto), 53)
})








