library(tidyverse)

df <- read_csv("data/anes_combined.csv")

df$resent_deserve_f <- factor(df$resent_deserve_f, levels = c("Agree Strongly", 
                                                           "Agree", 
                                                           "Neither",
                                         "Disagree", "Disagree Strongly"))
df$resent_try_f <- factor(df$resent_try_f, levels = c("Agree Strongly", 
                                                              "Agree", 
                                                              "Neither",
                                                              "Disagree", "Disagree Strongly"))
df$resent_workway_f <- factor(df$resent_workway_f, levels = c("Agree Strongly", 
                                                          "Agree", 
                                                          "Neither",
                                                          "Disagree", "Disagree Strongly"))
df$resent_slavery_f <- factor(df$resent_slavery_f, levels = c("Agree Strongly", 
                                                          "Agree", 
                                                          "Neither",
                                                          "Disagree", "Disagree Strongly"))

df <- df %>%
  group_by(resent_slavery_f) %>%
  mutate(av_soph_slav = mean(soph))

df <- df %>%
  group_by(resent_try_f) %>%
  mutate(av_soph_try = mean(soph))

df <- df %>%
  group_by(resent_workway_f) %>%
  mutate(av_soph_work = mean(soph))

df <- df %>%
  group_by(resent_deserve_f) %>%
  mutate(av_soph_des = mean(soph))

ggplot(df, aes(x=resent_deserve_f, y=av_soph_des)) + geom_boxplot() +
  theme_minimal()

ggplot(df, aes(x=resent_slavery_f, y=av_soph_slav)) + geom_boxplot() +
  theme_minimal()

ggplot(df, aes(x=resent_try_f, y=av_soph_try)) + geom_boxplot() +
  theme_minimal()

ggplot(df, aes(x=resent_workway_f, y=av_soph_work)) + geom_boxplot() +
  theme_minimal()


##boxplots
ggplot(df, aes(x=resent_deserve_f, y=soph)) + geom_boxplot() +
  theme_minimal()

ggplot(df, aes(x=resent_slavery_f, y=soph)) + geom_boxplot() +
  theme_minimal()

ggplot(df, aes(x=resent_try_f, y=soph)) + geom_boxplot() +
  theme_minimal()

ggplot(df, aes(x=resent_workway_f, y=soph)) + geom_boxplot() +
  theme_minimal()

##low soph (<.5)
low_soph <- df%>%
  subset(soph <= 0.5)

low_soph <- low_soph %>%
  group_by(resent_slavery_f) %>%
  mutate(av_soph_slav = mean(soph))

low_soph <- low_soph %>%
  group_by(resent_try_f) %>%
  mutate(av_soph_try = mean(soph))

low_soph <- low_soph %>%
  group_by(resent_workway_f) %>%
  mutate(av_soph_work = mean(soph))

low_soph <- low_soph %>%
  group_by(resent_deserve_f) %>%
  mutate(av_soph_des = mean(soph))

ggplot(low_soph, aes(x=resent_workway_f, y=..density..)) + geom_density() +
  theme_minimal()

ggplot(low_soph, aes(x=resent_try_f, y=..density..)) + geom_density() +
  theme_minimal()

ggplot(low_soph, aes(x=resent_slavery_f, y=..density..)) + geom_density() +
  theme_minimal()

ggplot(low_soph, aes(x=resent_deserve_f, y=..density..)) + geom_density() +
  theme_minimal()