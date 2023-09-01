#' @title Tabla de Contingencia
#'
#' @export

tabla_contingencia <- function(var1, var2) {
  tabla_base <- table(var1, var2)
  tabla_frecuencia <- prop.table(tabla_base)
  tabla_base_2 <- addmargins(tabla_frecuencia * 100)

  tabla_base_2
}

phi_coef <- function(tabla) {
  unname(sqrt(chisq.test(tabla)$statistic / sum(tabla)))
}

cramer_v <- function(tabla) {
  unname(sqrt(chisq.test(tabla)$statistic / (sum(tabla) * (min(dim(tabla)) - 1))))
}

coef_conting <- function(tabla) {
  unname(sqrt(chisq.test(tabla)$statistic / (chisq.test(tabla)$statistic + sum(tabla))))
}

chi_cuadrado <- function(var1, var2, phi_coef = FALSE, coef_conting = TRUE,
                         cramer_v = TRUE) {
  suppressWarnings({

  chi <- chisq.test(table(var1, var2))

  if (phi_coef) {
    phi <- phi_coef(table(var1, var2))
  } else {
    phi <- "FALSE"
  }

  if (cramer_v) {
    cramer <- cramer_v(table(var1, var2))
  } else {
    cramer <- "FALSE"
  }

  if (coef_conting) {
    contingencia <- coef_conting(table(var1, var2))
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

