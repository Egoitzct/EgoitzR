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

#' @noRd
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
                         cramer_v = TRUE, likelihood_ratio = TRUE) {
  suppressWarnings({

  chi <- chisq.test(tabla)

  chi_statistic <- chi[["statistic"]][["X-squared"]]
  chi_statistic <- as.numeric(format(round(chi_statistic, 3), nsmall = 3))

  if (phi_coef) {
    phi <- phi_coef(tabla)
    phi <- as.numeric(format(round(phi, 3), nsmall = 3))
  } else {
    phi <- "FALSE"
  }

  if (cramer_v) {
    cramer <- cramer_v(tabla)
    cramer <- as.numeric(format(round(cramer, 3), nsmall = 3))
  } else {
    cramer <- "FALSE"
  }

  if (coef_conting) {
    contingencia <- coef_conting(tabla)
    contingencia <- as.numeric(format(round(contingencia, 3), nsmall = 3))
  } else {
    contingencia <- "FALSE"
  }

  if (likelihood_ratio) {
    observed <- as.numeric(as.vector(chi[["observed"]]))
    expected <- as.numeric(as.vector(chi[["expected"]]))
    num_observations <- length(observed)

    while (num_observations != 0) {
      if (observed[num_observations] == 0){
        observed <- observed[-num_observations]
        expected <- expected[-num_observations]
        num_observations <- num_observations - 1
      } else {
        num_observations <- num_observations - 1
      }
    }

    likelihood_ratio <- 2 * (sum(observed * (log(observed / expected))))
    likelihood_ratio <- as.numeric(format(round(likelihood_ratio, 3), nsmall = 3))
  } else {
    likelihood_ratio <- "FALSE"
  }

  resultados <- data.frame(stringsAsFactors = FALSE,
                           Chi_Cuadrado = c(chi_statistic),
                           Likelihood_Ratio = c(likelihood_ratio),
                           P = c(chi$p.value),
                           GL = c(chi[["parameter"]][["df"]]),
                           Phi = c(phi),
                           V_Cramer = c(cramer),
                           Coef_Contingencia = c(contingencia))

  resultados

  })
}

mosaico_tabla_contingencia <- function(tabla, title = " ", egoitz_color = "gray_1") {

  layout(matrix(c(1), ncol = 1, nrow = 1))
  tabla_2 <- prop.table(tabla, margin = 2)

  mosaicplot(tabla, cex = 1.1, color = egoitz_cols(egoitz_color),
             main = title)
}

barras_tabla_contingencia <- function(tabla, title = " ", egoitz_palette = "gray") {

  color_num <- length(unique(rownames(tabla)))

  colors <- egoitz_color_pal(egoitz_palette)(color_num)

  tabla_2 <- prop.table(tabla, margin = 2)

  layout(matrix(c(1, 3, 2, 3), nrow = 2, ncol = 2),
         heights = c(2, 1),
         widths = c(2, 2))

  par(mar = c(0, 4, 3, 1))
  barplot(tabla, col = colors, beside = TRUE, main = title)

  par(mar = c(0, 2, 3, 2))
  barplot(tabla_2, col = colors, beside = TRUE,
          main = paste(title, "(Proporción sobre 100)", sep = "\n"))

  plot.new()
  legend("center", rownames(tabla),fill = colors, border = "black")
}
