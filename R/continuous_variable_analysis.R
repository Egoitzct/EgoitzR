#' @title Continuous Variable Analysis
#' @author Egoitz Carral
#' @param var1 La variable a analizar (numérica y continua)
#' @param media Valor booleano, calcular o no la media
#' @param mediana Valor booleano, calcular o no la mediana
#' @param moda Valor booleano, calcular o no la moda
#' @param dt Valor booleano, calcular o no la desviación típica
#' @param varianza Valor booleano, calcular o no la varianza
#' @param rango Valor booleano, calcular o no el rango
#' @param RIC Valor booleano, calcular o no el rangointercuartil
#' @param grafico Valor booleano, dibujar o no un histograma
#' @param simetria Valor booleano, calcular o no la simetria
#' @param curtosis Valor booleano, calcular o no la curtosis
#'
#' @export

continuous_variable_analysis <- function(var1, media = TRUE, mediana = FALSE,
                                         moda = FALSE, dt = TRUE, varianza = TRUE,
                                         rango = FALSE, RIC = TRUE, grafico = TRUE,
                                         simetria = TRUE, curtosis = TRUE, max_min = FALSE, na.rm = FALSE, ...){
  if (media)
    media <- mean(var1, na.rm = na.rm)

  if (mediana)
    mediana <- median(var1, na.rm = na.rm)

  if (moda) {
    x <- unique(var1)
    moda <- x[which.max(tabulate(match(var1, x)))]
  }

  if (dt)
    dt <- sd(var1, na.rm = na.rm)

  if (varianza)
    varianza <- dt ^2

  if (rango)
    rango <- max(var1) - min(var1)

  if (RIC)
    RIC <- IQR(var1, na.rm = na.rm)

  if (simetria) {
    simetria <- moments::skewness(var1, na.rm = na.rm)
  }

  if (curtosis) {
    curtosis <- moments::kurtosis(var1, na.rm = na.rm)
  }

  if (max_min) {
    maximo <- max(var1, na.rm = na.rm)
    minimo <- min(var1, na.rm = na.rm)
  } else {
    maximo <- FALSE
    minimo <- FALSE
  }

  if (grafico) {
    hist(var1, prob = TRUE, col = "white",
         border = "black")
    lines(density(var1, na.rm = na.rm), col = egoitz_cols("madder"), lwd = 2)
  }

  resultados <- data.frame(stringsAsFactors = FALSE,
                           Variable = c("Variable 1"),
                           Media = c(media),
                           Mediana = c(mediana),
                           Moda = c(moda),
                           Desv_Tip = c(dt),
                           Varianza = c(varianza),
                           Max = c(maximo),
                           Min = c(minimo),
                           Rango = c(rango),
                           RIC = c(RIC),
                           Simetría = c(simetria),
                           Curtosis = c(curtosis))

  resultados

}
