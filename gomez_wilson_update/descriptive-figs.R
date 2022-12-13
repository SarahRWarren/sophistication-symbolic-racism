library(tidyverse)
library(haven)

df1 <-read_dta("data/NJ county data for JOP.dta")
df2 <-read_dta("data/seasides votes for JOP.dta")

county <- df1$county
beach <- df1$beach
machine <- df1$machine
dem_votes <- df1$DEM_1908
eight <- as.data.frame(cbind(county, beach, machine, dem_votes)) %>%
  mutate(year = 1908)

dem_votes <- df1$DEM_1912
twelve <- as.data.frame(cbind(county, beach, machine, dem_votes)) %>%
  mutate(year = 1912)

dem_votes <- df1$DEM_1916
sixteen <- as.data.frame(cbind(county, beach, machine, dem_votes)) %>%
  mutate(year = 1916)

dem_votes <- df1$DEM_1920
twenty <- as.data.frame(cbind(county, beach, machine, dem_votes)) %>%
  mutate(year = 1920)

dem_votes <- df1$DEM_1924
twentyfour <- as.data.frame(cbind(county, beach, machine, dem_votes)) %>%
  mutate(year = 1924)

df <- rbind(eight, twelve, sixteen, twenty, twentyfour)
df$dem_votes <- as.numeric(df$dem_votes)

df_beach <- df %>% subset(beach == 1)
df_else <- df %>% subset(beach == 0)

ggplot(df, aes(x=year, y=dem_votes, color=beach)) + 
  geom_point(alpha = 0.5) + geom_jitter() +
  theme_bw() +
  geom_abline(intercept = intercept, slope = slope, 
              linetype="dashed", size=1) +
  labs(title = "Dem Pres. Vote Share Over Time",
       x = "",
       y = "Number of Votes")

ggplot(df_beach, aes(x=year, y=dem_votes)) + 
  geom_point() + geom_line() +
  geom_vline(xintercept = 1916, color = "grey", linetype="dashed") +
  theme_bw()  +
  facet_wrap(vars(county)) +
  labs(title = "Dem Pres. Vote Share Over Time in Beach Counties",
       x = "",
       y = "Number of Votes")
ggsave("figs/beach-county-vote-over-time.png", height=4, width = 6)

ggplot(df_else, aes(x=year, y=dem_votes)) + 
  geom_point() + geom_line() +
  geom_vline(xintercept = 1916, color = "grey", linetype="dashed") +
  theme_bw() + facet_wrap(vars(county)) +
  labs(title = "Dem Pres. Vote Share Over Time in Inland Counties",
       x = "",
       y = "Number of Votes")
ggsave("figs/inland-county-vote-over-time.png",
       height = 6, width = 8)

ggplot(df, aes(x=year, y=dem_votes, color=beach)) + 
  geom_point() + geom_line() +
  geom_vline(xintercept = 1916, color = "grey", linetype="dashed") +
  theme_bw() + facet_wrap(vars(county)) +
  labs(title = "Dem Pres. Vote Share Over Time",
       x = "",
       y = "Number of Votes")
ggsave("figs/county-vote-over-time-by-beach.png",
       height = 6, width = 8)