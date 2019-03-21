library(tidyverse)

setwd('/Users/Maximilian/ownCloud/03_Lehre/Spatial_Statistics/Data/R')

douglas_fir <- read_delim(file = 'DouglasFir_Adult_vs_Seedling_OG-N.txt', delim = ';')

douglas_fir <- filter(douglas_fir, mark == 1)


plot_pattern <- ggplot(data=douglas_fir) + 
  geom_point(aes(x=x, y=y), shape=1) + 
  coord_cartesian() + 
  labs(x="x [m]", y="y [m]", title="Western Hemlock - Adult trees in old-growth south") + 
  theme_bw()

ggsave(filename = "Practical_1_Pattern.pdf", plot = plot_pattern, 
       path = '/Users/Maximilian/ownCloud/03_Lehre/Spatial_Statistics/Practicals')
