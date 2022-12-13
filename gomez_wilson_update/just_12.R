library(haven)
library(tidyverse)
library(labelled)
library(MASS)
library(stargazer)
library(lsmeans)

#load 2012 data, pull relevant variables
anes_2012 <- read_dta("data/anes_timeseries_2012.dta") %>% 
  dplyr::select(resent_deserve, resent_slavery, resent_try, resent_workway, 
         ofcrec_vp_correct, ofcrec_speaker_correct, ofcrec_cj_correct,
         knowl_senmaj, knowl_housemaj, gender_respondent_x, sample_region,
         relig_church, pid_self, paprofile_libcon_self,
         dem_edugroup_x, dem_raceeth_x, dem_age_r_x, inc_incgroup_pre,
         ftcasi_black, egal_equal, egal_toofar, egal_bigprob, egal_worryless, 
         egal_notbigprob, egal_fewerprobs, govrole_big, govrole_market) %>%
  subset(ofcrec_vp_correct >= 0) %>%
  subset(inc_incgroup_pre >= 0) %>%
  subset(dem_age_r_x >= 0) %>%
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
  subset(egal_toofar > 0) %>%
  subset(egal_bigprob > 0) %>%
  subset(egal_worryless > 0) %>%
  subset(egal_notbigprob > 0) %>%
  subset(egal_fewerprobs > 0) %>%
  subset(govrole_big > 0) %>%
  subset(govrole_market > 0) %>%
    mutate(knowl_senmaj = as.numeric(knowl_senmaj),
         knowl_senmaj = recode(knowl_senmaj,
                               "1" = "1",
                               "2" = "0")) %>%
  mutate(knowl_senmaj_f = as.character(knowl_senmaj),
         knowl_senmaj_f = recode(knowl_senmaj,
                                 "1" = "Correct",
                                 "0" = "Incorrect")) %>%
  mutate(knowl_housemaj = as.numeric(knowl_housemaj),
         knowl_housemaj = recode(knowl_housemaj,
                                 "1" = "0",
                                 "2" = "1")) %>%
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
  

df <- remove_labels(anes_2012)
df$knowl_housemaj <- as.numeric(df$knowl_housemaj)
class(df$knowl_housemaj)
df$knowl_senmaj <- as.numeric(df$knowl_senmaj)
class(df$knowl_senmaj)

#count partially correct answers as correct + create factor version of resentment vars
df <- df %>%
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
df$soph <- (df$ofcrec_vp_correct + df$ofcrec_cj_correct + 
              df$ofcrec_speaker_correct +
              df$knowl_housemaj + df$knowl_senmaj)/5
summary(df$soph)

#recode egalitarianism so highest response is most egalitarian
df <- df %>%
  mutate(egal_equal = as.numeric(egal_equal),
         egal_equal = recode(egal_equal,
                                   "1" = 5,
                                   "2" = 4,
                                   "3" = 3,
                                   "4" = 2,
                                   "5" = 1)) %>%
  mutate(egal_bigprob = as.numeric(egal_bigprob),
         egal_bigprob = recode(egal_bigprob,
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

#make 6-pt egal scale
df$egal_scale <- (df$egal_equal + df$egal_bigprob + df$egal_fewerprobs +
                    df$egal_notbigprob + df$egal_toofar + 
                    df$egal_worryless)/30
summary(df$egal_scale)
#make econ-individualism scale
df$econ_indiv <- (df$govrole_big + df$govrole_market)/2
summary(df$econ_indiv)
#recode symbolic racism items so highest = most racist
df <- df %>%
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

write_rds(df, "data/df_2012.Rds")