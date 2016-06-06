context("Filtra Filet")

setup <- function() {
  dados <<- uploaddata("../../webscience.txt", NULL)
  filtrojcr <<- filtro.jcr(dados, 1, 1)
  filtrorecentes <<- filtro.recentes(filtrojcr, 2)
  filtroantigos <<- filtro.antigos(filtrojcr, 2)
}

test_that("filtro.jcr", {
  setup()
  comjcr = 1
  jcrmin = 1
  filtro.jcr = filtro.jcr(dados, comjcr, jcrmin)
  expect_equal(nrow(filtro.jcr), 8)
})

test_that("filtro.recentes", {
  setup()
  anomin = 2
  filtro.recentes = filtro.recentes(filtrojcr, anomin)
  expect_equal(nrow(filtro.recentes), 0)
})

test_that("filtro.recentes.citacao", {
  setup()
  citano = 0
  filtro.recentes.citacao = filtro.recentes.citacao(filtrorecentes, citano)
  expect_equal(nrow(filtro.recentes.citacao), 0)
})

test_that("filtro.antigos", {
  setup()
  anomin = 2
  filtro.antigos = filtro.antigos(filtrojcr, anomin)
  expect_equal(nrow(filtro.antigos), 8)
})

test_that("filtro.antigos.pareto", {
  setup()
  porcpareto = 80
  filtro.antigos.pareto = filtro.antigos.pareto(filtroantigos, porcpareto)
  expect_equal(nrow(filtro.antigos.pareto), 5)
})








