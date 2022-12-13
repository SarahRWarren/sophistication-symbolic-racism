library(haven)
library(tidyverse)
library(labelled)
library(MASS)
library(stargazer)
library(lsmeans)

#load 2016 data, pull relevant variables
anes_2016 <- read_dta("data/anes_timeseries_2016.dta") %>%
  dplyr::select(V162213, V162212, V162214, V162211, V162072, V162073b, V162076b,
         V161516, V161515, V161342, V161267, V163003, V161244,
         V161270, V161155, V161126, V162312, V162243, V162244, V162245, V162246,
         V162183, V162184,V161244, V161310x, V161361x) %>%
  dplyr::rename(dem_age_r_x = V161267)

anes_2016 <- anes_2016 %>%
  dplyr::rename(dem_raceeth_x = V161310x,
              resent_deserve = V162213,
              inc_incgroup_pre = V161361x,
              resent_slavery = V162212,
              resent_try = V162214,
              resent_workway = V162211,
              ofcrec_vp_correct = V162072,
              ofcrec_speaker_correct = V162073b,
              ofcrec_cj_correct = V162076b,
              knowl_senmaj = V161516, 
              knowl_housemaj = V161515,
              egal_equal = V162243,
              egal_worryless = V162244,
              egal_bigprob = V162245,
              egal_fewerprobs = V162246,
              govrole_big = V162183,
              govrole_market = V162184,
              relig_church = V161244,
              gender_respondent_x = V161342,
              sample_region = V163003,
              pid_self = V161155,
              paprofile_libcon_self = V161126,
              dem_edugroup_x = V161270,
              ftcasi_black = V162312)

##drop NA values
anes_2016 <- anes_2016 %>%
  subset(dem_age_r_x >= 0) %>%
  subset(inc_incgroup_pre >= 0) %>%
  subset(ofcrec_vp_correct >= 0) %>%
  subset(ofcrec_cj_correct >= 0) %>%
  subset(ofcrec_speaker_correct >= 0) %>%
  subset(knowl_housemaj > 0) %>%
  subset(knowl_senmaj > 0) %>%
  subset(resent_deserve > 0) %>%
  subset(resent_slavery > 0) %>%
  subset(resent_try > 0) %>%
  subset(resent_workway > 0) %>%
  subset(relig_church > 0) %>%
  subset(pid_self > 0 & pid_self < 5) %>%
  subset(paprofile_libcon_self > 0) %>%
  subset(dem_edugroup_x > 0) %>%
  subset(ftcasi_black > 0) %>%
  subset(egal_equal > 0) %>%
  subset(egal_bigprob > 0) %>%
  subset(egal_worryless > 0) %>%
  subset(egal_fewerprobs > 0) %>%
  subset(govrole_big > 0) %>%
  subset(govrole_market > 0) %>%
  subset(gender_respondent_x < 3)

anes_2016 <- anes_2016 %>%
  mutate(knowl_senmaj = as.numeric(knowl_senmaj),
         knowl_senmaj = recode(knowl_senmaj,
                               "1" = 0,
                               '2' = 1)) %>%
  mutate(knowl_senmaj_f = as.character(knowl_senmaj),
         knowl_senmaj_f = recode(knowl_senmaj,
                                 "1" = "Correct",
                                 "0" = "Incorrect")) %>%
  mutate(knowl_housemaj = as.numeric(knowl_housemaj),
         knowl_housemaj = recode(knowl_housemaj,
                                 "1" = 0,
                                 "2" = 1)) %>%
  mutate(knowl_housemaj_f = as.character(knowl_housemaj),
         knowl_housemaj_f = recode(knowl_housemaj,
                                   "0" = "Incorrect",
                                   "1" = "Correct")) %>%
  mutate(govrole_big = as.numeric(govrole_big),
         govrole_big = recode(govrole_big,
                              "1" = 1,
                              "2" = 0)) %>%
  mutate(govrole_market = as.numeric(govrole_market),
         govrole_market = recode(govrole_market,
                                 "1" = 0,
                                 "2" = 1)) %>%
  mutate(pid_dum_dem = as.numeric(pid_self),
         pid_dum_dem = recode(pid_self,
                              "1" = 1,
                              "2" = 0,
                              "3" = 0)) %>%
  mutate(female = as.numeric(gender_respondent_x),
         female = recode(gender_respondent_x,
                         "1" = 0,
                         "2" = 1)) %>%
  mutate(south = as.numeric(sample_region),
         south = recode(sample_region,
                        "1" = 0,
                        "2" = 0,
                        "3" = 1,
                        "4" = 0)) 

summary(anes_2016$govrole_big)
summary(anes_2016$knowl_housemaj)
summary(anes_2016$knowl_senmaj)

df2 <- remove_labels(anes_2016)
df2$knowl_housemaj <- as.numeric(df2$knowl_housemaj)
class(df2$knowl_housemaj)
df2$knowl_senmaj <- as.numeric(df2$knowl_senmaj)
class(df2$knowl_senmaj)

#count partially correct answers as correct + create factor version of resentment vars
df2 <- df2 %>%
  mutate(ofcrec_cj_correct= as.numeric(ofcrec_cj_correct),
         ofcrec_cj_correct = recode(ofcrec_cj_correct,
                                    "0" = 0,
                                    "0.5" = 1,
                                    "1" = 1,
                                    "2" = 1)) %>%
    mutate(ofcrec_vp_correct= as.numeric(ofcrec_vp_correct),
         ofcrec_vp_correct = recode(ofcrec_vp_correct,
                                    "0" = 0,
                                    "0.5" = 1,
                                    "1" = 1,
                                    "2" = 1)) %>%
  mutate(ofcrec_speaker_correct= as.numeric(ofcrec_speaker_correct),
         ofcrec_speaker_correct = recode(ofcrec_speaker_correct,
                                         "0" = 0,
                                         "0.5" = 1,
                                         "1" = 1,
                                         "2" = 1)) 

#create additive 0-1 sophistication scale
#each right answer is worth 1pt
df2$soph <- (df2$ofcrec_vp_correct + df2$ofcrec_cj_correct + 
              df2$ofcrec_speaker_correct +
              df2$knowl_housemaj + df2$knowl_senmaj)/5
summary(df2$soph)

#recode egalitarianism so highest response is most egalitarian
df2 <- df2 %>%
  mutate(egal_equal = as.numeric(egal_equal),
         egal_equal = recode(egal_equal,
                             "1" = 5,
                             "2" = 4,
                             "3" = 3,
                             "4" = 2,
                             "5" = 1)) %>%
  mutate(egal_fewerprobs = as.numeric(egal_fewerprobs),
         egal_fewerprobs = recode(egal_fewerprobs,
                                  "1" = 5,
                                  "2" = 4,
                                  "3" = 3,
                                  "4" = 2,
                                  "5" = 1))

#make 4-pt egal scale
df2$egal_scale <- (df2$egal_equal + df2$egal_bigprob + df2$egal_fewerprobs +
                    df2$egal_worryless)/20
summary(df2$egal_scale)

#make econ-individualism scale
df2$econ_indiv <- (df2$govrole_big + df2$govrole_market)/2
summary(df2$econ_indiv)

#recode symbolic racism items so highest = most racist
df2 <- df2 %>%
  mutate(resent_try = as.numeric(resent_try),
         resent_try = recode(resent_try,
                             "1" = 5,
                             "2" = 4,
                             "3" = 3,
                             "4" = 2,
                             "5" = 1)) %>%
  mutate(resent_workway = as.numeric(resent_workway),
         resent_workway = recode(resent_workway,
                                 "1" = 5,
                                 "2" = 4,
                                 "3" = 3,
                                 "4" = 2,
                                 "5" = 1)) %>%
  mutate(resent_deserve_f = as.character(resent_deserve),
         resent_deserve_f = recode(resent_deserve,
                                   "1" = "Agree Strongly",
                                   "2" = "Agree",
                                   "3" = "Neither",
                                   "4" = "Disagree",
                                   "5" = "Disagree Strongly")) %>%
  mutate(resent_slavery_f = as.character(resent_slavery),
         resent_slavery_f = recode(resent_slavery,
                                   "1" = "Agree Strongly",
                                   "2" = "Agree",
                                   "3" = "Neither",
                                   "4" = "Disagree",
                                   "5" = "Disagree Strongly")) %>%
  mutate(resent_try_f = as.character(resent_try),
         resent_try_f = recode(resent_try,
                               "5" = "Agree Strongly",
                               "4" = "Agree",
                               "3" = "Neither",
                               "2" = "Disagree",
                               "1" = "Disagree Strongly")) %>%
  mutate(resent_workway_f = as.character(resent_workway),
         resent_workway_f = recode(resent_workway,
                                   "5" = "Agree Strongly",
                                   "4" = "Agree",
                                   "3" = "Neither",
                                   "2" = "Disagree",
                                   "1" = "Disagree Strongly")) %>%
  subset(dem_raceeth_x == 1)
write_rds(df2, "data/df2_2016.Rds")