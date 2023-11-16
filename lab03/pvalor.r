# valor p em função de N em um teste chi-quadrado

set.seed(5)
N <- 1e6
dados <- tibble::tibble(
  a = sample(letters[1:4], N, replace = TRUE)
) |>
  dplyr::mutate(b = dplyr::case_when(
    a == "a" ~ sample(LETTERS[1:4], N, TRUE, c(.253, .249, .249, .249)),
    a == "b" ~ sample(LETTERS[1:4], N, TRUE, c(.249, .253, .249, .249)),
    a == "c" ~ sample(LETTERS[1:4], N, TRUE, c(.249, .249, .253, .249)),
    a == "d" ~ sample(LETTERS[1:4], N, TRUE, c(.249, .249, .249, .253))
  ))

table(dados)

tamanhos <- c(1000, 10000, 100000, 1000000)

tamanhos |>
  purrr::set_names(format(tamanhos, scientific = FALSE)) |>
  purrr::map_dfr(~{
    da <- head(dados, .x)
    tabela <- table(da)
    teste <- chisq.test(tabela)
    broom::tidy(teste)
  }, .id = "tamanho")

# quando não tem efeito, o valor-p tem distribuição uniforme!

N <- 1000
valores_p <- purrr::rerun(N, {
  x <- rnorm(N)
  y <- 1 + rnorm(N, sd = .0001)
  broom::tidy(lm(y~x))$p.value[2]
}) |> purrr::flatten_dbl()

hist(valores_p)


