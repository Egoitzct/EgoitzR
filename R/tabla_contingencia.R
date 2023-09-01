#' @title Tabla de Contingencia
#' @param var1 Variable que se quiera introducir para formar una tabla de contingencia
#' @param var2 Variable que se quiera introducir para formar una tabla de contingencia
#' @export

tabla_contingencia <- function(var1, var2) {
  tabla_base <- table(var1, var2)
  tabla_frecuencia <- prop.table(tabla_base)
  tabla_base_2 <- addmargins(tabla_frecuencia * 100)

  tabla_base_2
}

#' @param tabla Tabla usada para calcular los coeficientes Chi-Cuadrado, Phi, V de Cramer y el coeficiente de contingencia

phi_coef <- function(tabla) {
  unname(sqrt(chisq.test(tabla)$statistic / sum(tabla)))
}

cramer_v <- function(tabla) {
  unname(sqrt(chisq.test(tabla)$statistic / (sum(tabla) * (min(dim(tabla)) - 1))))
}

coef_conting <- function(tabla) {
  unname(sqrt(chisq.test(tabla)$statistic / (chisq.test(tabla)$statistic + sum(tabla))))
}

chi_cuadrado <- function(tabla, phi_coef = FALSE, coef_conting = TRUE,
                         cramer_v = TRUE) {
  suppressWarnings({

  chi <- chisq.test(tabla)

  if (phi_coef) {
    phi <- phi_coef(tabla)
  } else {
    phi <- "FALSE"
  }

  if (cramer_v) {
    cramer <- cramer_v(tabla)
  } else {
    cramer <- "FALSE"
  }

  if (coef_conting) {
    contingencia <- coef_conting(tabla)
  } else {
    contingencia <- "FALSE"
  }

  resultados <- data.frame(stringsAsFactors = FALSE,
                           Chi_Cuadrado = c(chi[["statistic"]][["X-squared"]]),
                           p_value = c(chi$p.value),
                           df = c(chi[["parameter"]][["df"]]),
                           Phi = c(phi),
                           V_Cramer = c(cramer),
                           Coef_Contingencia = c(contingencia))

  resultados

  })
}

