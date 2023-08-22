#' @title Análisis de variable nominal
#' @importFrom epiDisplay tab1
#'

variable_nominal_analisis <- function(variable, frecuencia = TRUE, grafico = TRUE) {
  if (frecuencia) {
    epiDisplay::tab1(variable, sort.group = "decreasing", graph = grafico,
                     main = "Distribución de la variable.")
  }

  if (frecuencia != TRUE && grafico == TRUE) {
    frecuen <- epiDisplay::tab1(variable, sort.group = "decreasing", graph = grafico,
                                main = "Distribución de la variable.")

  }
}
