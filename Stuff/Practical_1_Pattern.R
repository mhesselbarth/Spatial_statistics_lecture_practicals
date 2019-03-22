library(tidyverse)
library(helpeR)

douglas_fir <- readr::read_delim(file = "Solutions/Data/DouglasFir_Adult_vs_Seedling_OGN.txt",
                                 delim = ';')

douglas_fir <- dplyr::filter(douglas_fir, mark == 1)

plot_pattern <- ggplot2::ggplot(data = douglas_fir) + 
  geom_point(ggplot2::aes(x = x, y = y), shape = 1) + 
  coord_cartesian() + 
  labs(x="x [m]", y="y [m]", title="Western Hemlock - Adult trees in old-growth south") + 
  theme_bw()

helpeR::save_ggplot(plot = plot_pattern, 
                    filename = "Practical_1_Pattern.pdf", path = "Stuff",
                    width = 29.7, height = 21.0, units = "cm")

