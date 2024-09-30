library(quarto)
library(tidyverse)
library(glue)
library(purrr)


if(getwd() != "D:/OneDrive/R/hums/01_consultas") setwd("01_consultas")


codigos <- ALL_consultas %>% distinct(codigo) %>% pull(codigo) %>% as.character()


informes <- tibble(
  input = "02_informe.qmd",
  output_format = "html",
  output_file = str_glue("{codigos}.html"),
  execute = TRUE,
  execute_params = map(codigos, ~list(codigo = .))
)

pwalk(informes, quarto_render)
