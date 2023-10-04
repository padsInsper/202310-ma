plot(cars)

arrow::write_parquet(cars, "cars.parquet")

library(tidyverse)

#' ajusta o modelo de regressão linear
#'
#' @param dados dados para ajustar o modelo
#'
#' @return coeficientes do modelo
ajusta_modelo <- function(dados) {
  modelo <- lm(dist ~ speed, data = dados)
  p <- ggplot(dados, aes(speed, dist)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Distancia vs Velocidade",
      subtitle = "Modelo de regressão linear",
      x = "Velocidade",
      y = "Distância"
    ) +
    theme_minimal()
  #print(p)
  coef(modelo)
}

dados <- cars |>
  dplyr::slice_sample(prop = 1, replace = TRUE)

res <- ajusta_modelo(dados)

resultados <- purrr::map(1:1000, \(x) {
  dados <- cars |>
    dplyr::slice_sample(prop = 1, replace = TRUE)
  ajusta_modelo(dados)
}, .progress = TRUE)

res_tabela <- purrr::map_dfr(resultados, \(x) {
  tibble::enframe(x)
}, .id = "id")

# comentário
res_tabela |>
  tidyr::pivot_wider() |>
  janitor::clean_names() |>
  ggplot(aes(x = speed)) +
  geom_histogram()
