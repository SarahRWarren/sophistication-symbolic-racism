library(tidyverse)
library(readr)

ir <- read_csv("data/ir.csv", col_types = cols(Month = col_character()), 
                   skip = 2)
ir2 <- ir[80:193,]

ggplot(ir2, aes(x = Month, y = `Institutional racism: (United States)`)) +
  geom_point() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Month-Year",
       y = "Search Frequency")
ggsave("fig/google-trends.png", width = 12, height = 4)