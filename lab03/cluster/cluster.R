# http://www.sthda.com/english/wiki/practical-guide-to-cluster-analysis-in-r-book
# https://bookdown.org/content/1340/cluster.html#data-4
# https://radiant-rstats.github.io/docs/

# cluster -----------------------------------------------------------------

library(tidyverse)
library(factoextra)

dados <- readxl::read_excel("dados/segmentation_office.xlsx", "SegmentationData")

set.seed(1)
nomes <- randomNames::randomNames(
  40, which.names = "first",
  sample.with.replacement = FALSE
)

dados_cluster <- dados |>
  select(variety_of_choice:return_policy) |>
  mutate(nome = nomes) |>
  column_to_rownames("nome")


distancias <- dist(dados_cluster)

# ward --------------------------------------------------------------------

h_clust <- hclust(distancias, method = "ward.D2")

plot(h_clust, k = 4) # 🤢

factoextra::fviz_dend(h_clust, k = 3) # 🤩


# separando clusters
h_clust2 <- hcut(distancias, k = 4)

fviz_dend(h_clust2, rect = TRUE, cex = .8)

fviz_silhouette(h_clust2, label = TRUE, print.summary = TRUE) +
  scale_fill_viridis_d(begin = .2, end = .8) +
  scale_colour_viridis_d(begin = .2, end = .8) +
  theme_minimal(14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

