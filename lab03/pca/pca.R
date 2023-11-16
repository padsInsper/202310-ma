# PCA (mapa perceptual) ---------------------------------------------------

# referencias
# http://factominer.free.fr/course/MOOC.html
# https://www.gastonsanchez.com
# https://pca4ds.github.io/
# https://rpkgs.datanovia.com/factoextra/index.html
# https://bookdown.org/content/1340/pca_office.html#data-2

library(tidyverse)
library(FactoMineR)
library(factoextra)

# dados -------------------------------------------------------------------

# fonte?
dados <- readxl::read_excel("dados/toothpaste.xlsx")

dplyr::glimpse(dados)

# funcoes do factoextra ---------------------------------------------------

res_pca <- dados |>
  select(-consumer) |>
  mutate(age = cut(age, c(20, 40, 54))) |>
  FactoMineR::PCA(
    ncp = Inf, graph = FALSE,
    quali.sup = c(7, 8)
  )


get_eig(res_pca)

fviz_screeplot(res_pca, addlabels = TRUE)

fviz_contrib(res_pca, choice = "var", axes = 1)
fviz_contrib(res_pca, choice = "var", axes = 2)

fviz_pca_var(res_pca, col.var = "black")

fviz_pca_var(res_pca, col.var = "red", repel = TRUE)
fviz_pca_var(
  res_pca, col.var = "contrib", repel = TRUE
)


fviz_pca_ind(
  res_pca, col.ind = "contrib", repel = TRUE
)

fviz_pca_biplot(res_pca, repel = TRUE, habillage = "gender") +
  scale_color_viridis_d(begin = .2, end = .8)

fviz_pca_biplot(res_pca, repel = TRUE, habillage = "age") +
  scale_color_viridis_d(begin = .2, end = .8)


# Visualize
# Use habillage to specify groups for coloring

fviz_pca_ind(
  res_pca,
  label = "none",
  habillage = "age",
  palette = viridis::viridis(4, 1, .2, .8),
  addEllipses = TRUE
)

set.seed(1)
cluster <- kmeans(scale(dplyr::select(dados, -consumer, -gender)), 3)

fviz_cluster(
  cluster,
  dados |>
    dplyr::select(-consumer, -gender)
) +
  theme_minimal()

