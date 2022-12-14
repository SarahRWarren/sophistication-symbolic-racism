#library(haven)
library(tidyverse)
library(labelled)
library(MASS)
library(stargazer)
library(lsmeans)

df1 <- read_rds("data/df_2012.Rds") %>%
  mutate(year = 2012)

summary(df1$dem_edugroup_x)
df2 <- read_rds("data/df2_2016.Rds") %>%
  mutate(year = 2016) %>%
  mutate(dem_edugroup_x = as.numeric(dem_edugroup_x),
         dem_edugroup_x = recode(dem_edugroup_x,
                                 "1" = 1,
                                 "2" = 1,
                                 "3" = 1,
                                 "4" = 1,
                       "5" = 1,
                       "6" = 1,
                       "7" = 1,
                       "8" = 1,
                       "9" = 2,
                       "10" = 3,
                       "11" = 3,
                       "12" = 3,
                       "13" = 4,
                       "14" = 5,
                       "15" = 5,
                       "16" = 5,
                       "90" = 6,
                       "95" = 6,
                       "-9" = 6)) %>%
  subset(dem_edugroup_x > 0) %>%
  subset(dem_edugroup_x < 6)

#df3 <- read_rds("data/df3_2020.Rds") %>%
#  mutate(year=2020)
library(plyr)

df<-rbind.fill(df1, df2)

#make racism items factors
df$resent_deserve_m <- factor(df$resent_deserve, levels = c("1", 
                                                            "2", 
                                                            "3",
                                                            "4", "5"))
df$resent_try_m <- factor(df$resent_try, levels = c("1", 
                                                    "2", 
                                                    "3",
                                                    "4", "5"))
df$resent_workway_m <- factor(df$resent_workway, levels = c("1", 
                                                            "2", 
                                                            "3",
                                                            "4", "5"))
df$resent_slavery_m <- factor(df$resent_slavery, levels = c("1", 
                                                            "2", 
                                                            "3",
                                                            "4", "5"))
write_csv(df, "data/df_combined.csv")
write_rds(df, "data/df_combined.Rds")

##estimate ordered probit, group by year
##I predict that sophistication should predict resent_slavery and resent_deserve,
##but not predict resent_workway and resent_try

###sophistication category
df$soph_cat <- cut(df$soph,
                   breaks=c("0", "0.25", "0.5", "0.75", "1"),
                   labels=c('Low (0-0.25)', 'Med-Low (0.25-0.5)', 
                            'Med-High (0.5-0.75)', "High (0.75-1)")) %>%
  glimpse()

#####pretty pictures
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

df$symb_racism <- (df$resent_deserve + df$resent_slavery +
                     df$resent_try + df$resent_workway)/20
summary(df$symb_racism)

for_fig <- df %>%
  dplyr::select(soph, soph_cat, symb_racism, resent_deserve, 
         resent_deserve_m,
         resent_deserve_f, resent_slavery, resent_slavery_m, resent_slavery_f,
         resent_try, resent_try_f, resent_try_m, 
         resent_workway, resent_workway_m, resent_workway_f,
         year, dem_edugroup_x) %>%
  drop_na() 

for_fig$education_f <- factor(for_fig$dem_edugroup_x, levels = c(1,
                                                                 2,
                                                                 3,
                                                                 4,
                                                                 5),
                              labels =              c("Less than HS",
                                                    "High School",
                                                    "Some Post-HS",
                                                    "Bachelor's",
                                                    "Graduate"))


ggplot(for_fig, aes(x=soph, y=symb_racism)) + geom_point() + geom_jitter() +
  theme_bw() + facet_wrap(vars(year)) +
  geom_smooth(method='lm',formula=y~x) +
  labs(x = "Sophistication",
       y = "Symbolic Racism")
ggsave("fig/symbolic-racism-desc.png")

ggplot(for_fig, aes(x=soph)) + geom_bar() +
  facet_grid(vars(year)) + theme_bw() +
  labs(x = "Sophistication",
       y = "Count")
ggsave("fig/sophistication-desc.png")

a <- ggplot(for_fig, aes(x=resent_slavery_f)) + geom_bar() +
  facet_wrap(vars(soph_cat)) + theme_bw() +
  labs(x = "Past Slavery Harder for Blacks",
       y = "Count")
#ggsave("fig/slav-soph.png", width = 9, height = 4)

b <- ggplot(for_fig, aes(x=resent_deserve_f)) + geom_bar() +
  facet_wrap(vars(soph_cat)) + theme_bw() +
  labs(x = "Blacks Get Less than Deserve",
       y = "Count")
#ggsave("fig/deserve-soph.png", width = 9, height = 4)

c <- ggplot(for_fig, aes(x=resent_try_f)) + geom_bar() +
  facet_wrap(vars(soph_cat)) + theme_bw() +
  labs(x = "Blacks Should Try Harder",
       y = "Count")
#ggsave("fig/try-soph.png", width = 9, height = 4)

d <- ggplot(for_fig, aes(x=resent_workway_f)) + geom_bar() +
  facet_wrap(vars(soph_cat)) + theme_bw() +
  labs(x = "Blacks Should Work Harder",
       y = "Count")

e <- gridExtra::grid.arrange(a,b,c,d, ncol = 2)
ggsave("fig/response-by-soph.png", plot = e, width = 18, height = 6)

ggplot(for_fig, aes(x=education_f)) + geom_bar() +
  facet_wrap(vars(soph_cat)) + theme_bw() +
  labs(x = "",
       y = "Count")
ggsave("fig/soph-education.png", width = 10, height = 5)
##create dfs for each year
df_2012 <- df %>%
  subset(year==2012)

df_2016 <- df %>%
  subset(year==2016)

#df_2020 <- df %>%
#  subset(year==2020)

###analysis, grouped
m1_12 <- lm(symb_racism ~ soph + female + dem_edugroup_x +
           econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
           paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
         data = df_2012)

m1_16 <- lm(symb_racism ~ soph + female + dem_edugroup_x +
              econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
              paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
            data = df_2016)

##2012 models
m2_12 <- polr(resent_deserve_m ~ soph + female + dem_edugroup_x +
           econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
           paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
         data = df_2012, method = c("probit"))
m3_12 <- polr(resent_slavery_m ~ soph + female + dem_edugroup_x +
           econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
           paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
         data = df_2012,  method = c("probit"))
m4_12 <- polr(resent_try_m ~ soph + female + dem_edugroup_x +
           econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
           paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
         data = df_2012,  method = c("probit"))
m5_12 <- polr(resent_workway_m ~ soph + female + dem_edugroup_x +
           econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
           paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
         data = df_2012,  method = c("probit"))

##2016 analysis
m2_16 <- polr(resent_deserve_m ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2016,  method = c("probit"))
m3_16 <- polr(resent_slavery_m ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2016,  method = c("probit"))
m4_16 <- polr(resent_try_m ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2016,  method = c("probit"))
m5_16 <- polr(resent_workway_m ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2016,  method = c("probit"))


####OLS versions for robustness
##2012 models
m2_12_ols <- lm(resent_deserve ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2012)
m3_12_ols <- lm(resent_slavery ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2012)
m4_12_ols <- lm(resent_try ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2012)
m5_12_ols <- lm(resent_workway ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2012)

##2016 analysis
m2_16_ols <- lm(resent_deserve ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2016)
m3_16_ols <- lm(resent_slavery ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2016)
m4_16_ols <- lm(resent_try ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2016)
m5_16_ols <- lm(resent_workway ~ soph + female + dem_edugroup_x +
                econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
              data = df_2016)

stargazer(m2_12_ols, m3_12_ols, m2_16_ols, m3_16_ols, #m2_20, m3_20,
          title="Structural Attributions 2012-2016 (OLS)", 
          type="latex", style = "apsr",
          align=TRUE, out="tables/probits_structural_ols.tex",
          covariate.labels = c("Political Sophistication",
                               "Female", "Education",
                               "Economic Individualism",
                               "Egalitarianism", "South", "Democrat",
                               "Church Attendance", "Ideology",
                               "Black Feeling Therm.", "Age",
                               "Income"),
          dep.var.labels = c("Question 1 (2012)", 
                             "Question 2 (2012)",
                             "Question 1 (2016)",
                             "Question 2 (2016)"))

stargazer(m4_12_ols, m5_12_ols, m4_16_ols, m5_16_ols, #m4_20, m5_20,
          title="Individual Attributions 2012-2016 (OLS)", 
          type="latex", style = "apsr",
          align=TRUE, out="tables/probits_individual_OLS.tex",
          covariate.labels = c("Political Sophistication",
                               "Female", "Education",
                               "Economic Individualism",
                               "Egalitarianism", "South", "Democrat",
                               "Church Attendance", "Ideology",
                               "Black Feeling Therm.", "Age",
                               "Income"),
          dep.var.labels = c("Question 1 (2012)", 
                             "Question 2 (2012)",
                             "Question 1 (2016)",
                             "Question 2 (2016)"))


##2020 analysis - NO SOCIAL MEDIA
#m2_20 <- polr(resent_deserve_m ~ soph + female + dem_edugroup_x +
#                egal_scale + south + pid_dum_dem + relig_church +
#                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
#              data = df_2020, Hess=TRUE)
#m3_20 <- polr(resent_slavery_m ~ soph + female + dem_edugroup_x +
#                egal_scale + south + pid_dum_dem + relig_church +
#                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
#              data = df_2020, Hess=TRUE)
#m4_20 <- polr(resent_try_m ~ soph + female + dem_edugroup_x +
#                egal_scale + south + pid_dum_dem + relig_church +
#                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
#              data = df_2020, Hess=TRUE)
#m5_20 <- polr(resent_workway_m ~ soph + female + dem_edugroup_x +
#                egal_scale + south + pid_dum_dem + relig_church +
#                paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
#              data = df_2020, Hess=TRUE)


##2020 analysis - SOCIAL MEDIA
#m2_20_sm <- polr(resent_deserve_m ~ soph + female + dem_edugroup_x +
#                egal_scale + south + pid_dum_dem + relig_church +
#                paprofile_libcon_self + ftcasi_black + dem_age_r_x + 
#                  inc_incgroup_pre +
#                  facebook + twitter + instagram + tiktok + reddit +
#                  snapchat + youtube + other,
#              data = df_2020, Hess=TRUE)
#m3_20_sm <- polr(resent_slavery_m ~ soph + female + dem_edugroup_x +
#                egal_scale + south + pid_dum_dem + relig_church +
#                paprofile_libcon_self + ftcasi_black + dem_age_r_x + 
#                  inc_incgroup_pre +
#                  facebook + twitter + instagram + tiktok + reddit +
#                 snapchat + youtube + other,
#              data = df_2020, Hess=TRUE)
#m4_20_sm <- polr(resent_try_m ~ soph + female + dem_edugroup_x +
#                egal_scale + south + pid_dum_dem + relig_church +
#                paprofile_libcon_self + ftcasi_black + dem_age_r_x + 
#                  inc_incgroup_pre +
#                  facebook + twitter + instagram + tiktok + reddit +
#                  snapchat + youtube + other,
#              data = df_2020, Hess=TRUE)
#m5_20_sm <- polr(resent_workway_m ~ soph + female + dem_edugroup_x +
#                egal_scale + south + pid_dum_dem + relig_church +
#                paprofile_libcon_self + ftcasi_black + dem_age_r_x + 
#                  inc_incgroup_pre +
#                  facebook + twitter + instagram + tiktok + reddit +
#                  snapchat + youtube + other,
#              data = df_2020, Hess=TRUE)


stargazer(m1_12, m1_16,
          title="Model 1 (OLS)", type="latex", style = "apsr",
          align=TRUE, out="tables/ols.tex",
          dep.var.labels = c("Symbolic Racism"),
          covariate.labels = c("Political Sophistication",
                               "Female", "Education",
                               "Economic Individualism",
                               "Egalitarianism", "South", "Democrat",
                               "Church Attendance", "Ideology",
                               "Black Feeling Therm.", "Age",
                               "Income"))

summary(df_2012$symb_racism)
summary(df_2016$symb_racism)

library(sjPlot)

plot(m1_12)
plot(m1_16)

res_12 <- m1_12$residuals
fit_12 <- m1_12$fitted.values

plotdf_12 <- as.data.frame(cbind(res_12, fit_12))

ggplot(plotdf_12, aes(x=fit_12, y=res_12)) + 
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values",
       y= "Residuals")
ggsave("fig/resid-fitted-12.png")

res_16 <- m1_16$residuals
fit_16 <- m1_16$fitted.values

plotdf_16 <- as.data.frame(cbind(res_16, fit_16))

ggplot(plotdf_16, aes(x=fit_16, y=res_16)) + 
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values",
       y= "Residuals")
ggsave("fig/resid-fitted-16.png")



library(lmtest)
bptest(m1_12)
bptest(m1_16)

#install.packages("robustbase")
library(robustbase)
lmrobfit_12 <- lmrob(symb_racism ~ soph + female + dem_edugroup_x +
                    econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                    paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
                  data = df_2012)

lmrobfit_16 <- lmrob(symb_racism ~ soph + female + dem_edugroup_x +
                       econ_indiv + egal_scale + south + pid_dum_dem + relig_church +
                       paprofile_libcon_self + ftcasi_black + dem_age_r_x + inc_incgroup_pre,
                     data = df_2016)

stargazer(m1_12, lmrobfit_12, m1_16, lmrobfit_16,
          title="Model 1 (OLS)", type="latex", style = "apsr",
          align=TRUE, out="tables/robust-ols.tex",
          dep.var.labels = c("Symbolic Racism"),
          covariate.labels = c("Political Sophistication",
                               "Female", "Education",
                               "Economic Individualism",
                               "Egalitarianism", "South", "Democrat",
                               "Church Attendance", "Ideology",
                               "Black Feeling Therm.", "Age",
                               "Income"))






stargazer(m2_12, m3_12, m2_16, m3_16, #m2_20, m3_20,
          title="Structural Attributions 2012-2016", 
          type="latex", style = "apsr",
          align=TRUE, out="tables/probits_structural.tex",
          covariate.labels = c("Political Sophistication",
                               "Female", "Education",
                               "Economic Individualism",
                               "Egalitarianism", "South", "Democrat",
                               "Church Attendance", "Ideology",
                               "Black Feeling Therm.", "Age",
                               "Income"),
          dep.var.labels = c("Question 1 (2012)", 
                             "Question 2 (2012)",
                             "Question 1 (2016)",
                             "Question 2 (2016)"))

stargazer(m4_12, m5_12, m4_16, m5_16, #m4_20, m5_20,
          title="Individual Attributions 2012-2016", 
          type="latex", style = "apsr",
          align=TRUE, out="tables/probits_individual.tex",
          covariate.labels = c("Political Sophistication",
                               "Female", "Education",
                               "Economic Individualism",
                               "Egalitarianism", "South", "Democrat",
                               "Church Attendance", "Ideology",
                               "Black Feeling Therm.", "Age",
                               "Income"),
          dep.var.labels = c("Question 1 (2012)", 
                             "Question 2 (2012)",
                             "Question 1 (2016)",
                             "Question 2 (2016)"))


#stargazer(m2_20_sm, m3_20_sm, m4_20_sm, m5_20_sm,
#          title="2020 With Social Media", 
#          type="latex", style = "apsr",
#          align=TRUE, out="tables/probits_social_media.tex")


###diagnostics on ordered probit
#install.packages("sure")
library(sure)
library(gridExtra)

a_12 <- autoplot.polr(m2_12, what = "fitted")
#  ggsave("fig/m2_12_resid_fittted.png")
b_12<-autoplot.polr(m3_12, what = "fitted")
#  ggsave("fig/m3_12_resid_fittted.png")
c_12<-autoplot.polr(m4_12, what = "fitted")
#  ggsave("fig/m4_12_resid_fittted.png")
d_12<-autoplot.polr(m5_12, what = "fitted")
#  ggsave("fig/m5_12_resid_fittted.png")

fr_12 <- grid.arrange(a_12,b_12,c_12,d_12)
ggsave("fig/fitted_residuals_12.png", plot=fr_12,
       height = 8, width = 8)

  
a_16<-autoplot.polr(m2_16, what = "fitted")
#  ggsave("fig/m2_16_resid_fittted.png")
b_16<-autoplot.polr(m3_16, what = "fitted")
#  ggsave("fig/m3_16_resid_fittted.png")
c_16<-autoplot.polr(m4_16, what = "fitted")
#  ggsave("fig/m4_16_resid_fittted.png")
d_16<-autoplot.polr(m5_16, what = "fitted")
#  ggsave("fig/m5_16_resid_fittted.png")

  
fr_16 <- grid.arrange(a_16,b_16,c_16,d_16)
ggsave("fig/fitted_residuals_16.png", plot=fr_16,
         height = 8, width = 8)
  
e_12<-autoplot.polr(m2_12, what = "qq")
#  ggsave("fig/m2_12_qq.png")
f_12 <-  autoplot.polr(m3_12, what = "qq")
#  ggsave("fig/m3_12_qq.png")
g_12 <-  autoplot.polr(m4_12, what = "qq")
#  ggsave("fig/m4_12_qq.png")
h_12 <-  autoplot.polr(m5_12, what = "qq")
#ggsave("fig/m5_12_qq.png")
qq_12 <- grid.arrange(e_12, f_12, g_12, h_12)
ggsave("fig/qq_12.png", plot=qq_12,
       height = 8, width = 8)


e_16<-  autoplot.polr(m2_16, what = "qq")
#  ggsave("fig/m2_16_qq.png")
f_16<-  autoplot.polr(m3_16, what = "qq")
 # ggsave("fig/m3_16_qq.png")
g_16<-  autoplot.polr(m4_16, what = "qq")
  #ggsave("fig/m4_16_qq.png")
h_16<-  autoplot.polr(m5_16, what = "qq")
  #ggsave("fig/m5_16_qq.png")

qq_16 <- grid.arrange(e_16, f_16, g_16, h_16)
  ggsave("fig/qq_16.png", plot=qq_16,
         height = 8, width = 8)
  
