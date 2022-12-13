library(haven)
library(tidyverse)
library(labelled)
library(MASS)
library(stargazer)
library(lsmeans)

#load 2020 data, pull relevant variables
anes_2020 <- read_dta("data/anes_timeseries_2020.dta") %>%
  dplyr::select(V202302, V202301, V202303, V202300, V202138y, V202139y2, V202142y2,
             V201647, V201646, V202260, V202261, V202262, V202263, V201452,
             V201600, V201228, V201200, V201510, V202480, V203003,
             V201549x, V201507x, V202468x, V202541a, V202541b, V202541c,
             V202541d, V202541e, V202541f, V202541g, V202541h, V202541i) %>%
  dplyr::rename(dem_raceeth_x = V201549x,
                inc_incgroup_pre = V202468x,
                dem_age_r_x = V201507x,
                resent_deserve = V202302,
                resent_slavery = V202301,
                resent_try = V202303,
                resent_workway = V202300,
                ofcrec_vp_correct = V202138y,
                ofcrec_speaker_correct = V202139y2,
                ofcrec_cj_correct = V202142y2,
                knowl_senmaj = V201647,
                knowl_housemaj = V201646,
                egal_equal = V202260,
                egal_worryless = V202261,
                egal_bigprob = V202262,
                egal_fewerprobs = V202263,
                relig_church = V201452,
                gender_respondent_x = V201600,
                sample_region = V203003,
                pid_self = V201228,
                paprofile_libcon_self = V201200,
                dem_edugroup_x = V201510,
                ftcasi_black = V202480,
                facebook = V202541a,
                twitter = V202541b,
                instagram = V202541c,
                reddit = V202541d,
                youtube = V202541e,
                snapchat = V202541f,
                tiktok = V202541g,
                other = V202541h,
                none = V202541i)

anes_2020 <- anes_2020 %>%
      subset(ofcrec_vp_correct >= 0) %>%
      subset(dem_age_r_x >= 0) %>%
  subset(inc_incgroup_pre >= 0) %>%
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
  subset(gender_respondent_x > 0) %>%
  subset(facebook >= 0) %>%
  subset(twitter >= 0) %>%
  subset(instagram >= 0) %>%
  subset(reddit >= 0) %>%
  subset(youtube >= 0) %>%
  subset(snapchat >= 0) %>%
  subset(tiktok >= 0) %>%
  subset(other >= 0) %>%
  subset(none >= 0) %>%
  mutate(knowl_senmaj = as.numeric(knowl_senmaj),
             knowl_senmaj = recode(knowl_senmaj,
                                   "1" = "0",
                                   "2" = "1")) %>%
      mutate(knowl_senmaj_f = as.character(knowl_senmaj),
             knowl_senmaj_f = recode(knowl_senmaj,
                                     "1" = "Correct",
                                     "0" = "Incorrect")) %>%
      mutate(knowl_housemaj = as.numeric(knowl_housemaj),
             knowl_housemaj = recode(knowl_housemaj,
                                     "1" = "1",
                                     "2" = "0")) %>%
      mutate(knowl_housemaj_f = as.character(knowl_housemaj),
             knowl_housemaj_f = recode(knowl_housemaj,
                                       "0" = "Incorrect",
                                       "1" = "Correct")) %>%
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

df3 <- remove_labels(anes_2020)
df3$knowl_housemaj <- as.numeric(df3$knowl_housemaj)
class(df3$knowl_housemaj)
df3$knowl_senmaj <- as.numeric(df3$knowl_senmaj)
class(df3$knowl_senmaj)

#count partially correct answers as correct
df3 <- df3 %>%
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
df3$soph <- (df3$ofcrec_vp_correct + df3$ofcrec_cj_correct + 
               df3$ofcrec_speaker_correct +
               df3$knowl_housemaj + df3$knowl_senmaj)/5
summary(df3$soph)

#recode egalitarianism so highest response is most egalitarian
df3 <- df3 %>%
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

#egalitarian scale
df3$egal_scale <- (df3$egal_equal + df3$egal_bigprob + df3$egal_fewerprobs +
                     df3$egal_worryless)/20
summary(df3$egal_scale)

#there is no individualism scale :(

#recode symbolic racism items so highest = most racist
df3 <- df3 %>%
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
write_rds(df3, "data/df3_2020.Rds")
