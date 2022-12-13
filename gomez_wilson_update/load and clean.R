library(haven)
library(tidyverse)
library(labelled)

#pull in: gender, age, region (South), 
#income, church attendance, education, Partisanship, ideology,
#anti-black affect (inverse black FT), egalitarianism

#load 2012 data, pull relevant variables
anes_2012 <- read_dta("data/anes_timeseries_2012.dta") %>%
  select(resent_deserve, resent_slavery, resent_try, resent_workway, 
         ofcrec_vp_correct, ofcrec_speaker_correct, ofcrec_cj_correct,
         knowl_senmaj, knowl_housemaj, gender_respondent_x, 
         dem_agegrp_iwdate_x, sample_region, inc_totmiss40, 
         relig_church, dem_edu, pid_self, libcpre_self, 
         ftcasi_black, egal_equal, egal_toofar, 
         egal_bigprob, egal_worryless, 
         egal_notbigprob, egal_fewerprobs)%>%
  subset(ofcrec_vp_correct >= 0) %>%
  subset(ofcrec_cj_correct >= 0) %>%
  subset(ofcrec_speaker_correct >= 0) %>%
  subset(knowl_housemaj > 0) %>%
  subset(knowl_senmaj > 0) %>%
  subset(resent_deserve > 0) %>%
  subset(resent_slavery > 0) %>%
  subset(resent_try > 0) %>%
  subset(resent_workway > 0) %>%
  subset(dem_agegrp_iwdate_x > 0) %>%
  subset(inc_totmiss40 > 0) %>%
  subset(relig_church > 0) %>%
  subset(dem_edu > 0) %>%
  subset(pid_self > 0 & pid_self < 5) %>%
  subset(libcpre_self > 0) %>%
  subset(ftcasi_black > 0) %>%
  subset(egal_equal > 0) %>%
  subset(egal_toofar > 0) %>%
  subset(egal_bigprob > 0) %>%
  subset(egal_worryless > 0) %>%
  subset(egal_notbigprob > 0) %>%
  subset(egal_fewerprobs > 0) %>%
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
                                 "1" = "Correct"))


#load 2016 data, pull relevant variables
anes_2016 <- read_dta("data/anes_timeseries_2016.dta") %>%
  select(V162213, V162212, V162214, V162211, V162072, V162073b, V162076b,
         V161516, V161515, V161342, V161267, V163003, V161352, V161244,
         V161270, V161155, V161126, V162312, V162243) %>%
  rename(resent_deserve = V162213) %>%
  rename(resent_slavery = V162212) %>%
  rename(resent_try = V162214) %>%
  rename(resent_workway = V162211) %>%
  rename(ofcrec_vp_correct = V162072) %>%
  rename(ofcrec_speaker_correct = V162073b) %>%
  rename(ofcrec_cj_correct = V162076b) %>%
  rename(knowl_senmaj = V161516) %>%
  rename(knowl_housemaj = V161515) %>%
  subset(ofcrec_vp_correct >= 0) %>%
  subset(ofcrec_cj_correct >= 0) %>%
  subset(ofcrec_speaker_correct >= 0) %>%
  subset(knowl_housemaj > 0) %>%
  subset(knowl_senmaj > 0) %>%
  subset(resent_deserve > 0) %>%
  subset(resent_slavery > 0) %>%
  subset(resent_try > 0) %>%
  subset(resent_workway > 0) %>%
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
                                 "1" = "0",
                                 "2" = "1")) %>%
  mutate(knowl_housemaj_f = as.character(knowl_housemaj),
         knowl_housemaj_f = recode(knowl_housemaj,
                                   "0" = "Incorrect",
                                   "1" = "Correct"))

#load 2020 data, pull relevant variables
anes_2020 <- read_dta("data/anes_timeseries_2020.dta") %>%
  select(V202302, V202301, V202303, V202300, V202138y, V202139y2, V202142y2,
         V201647, V201646) %>%
  rename(resent_deserve = V202302) %>%
  rename(resent_slavery = V202301) %>%
  rename(resent_try = V202303) %>%
  rename(resent_workway = V202300) %>%
  rename(ofcrec_vp_correct = V202138y) %>%
  rename(ofcrec_speaker_correct = V202139y2) %>%
  rename(ofcrec_cj_correct = V202142y2) %>%
  rename(knowl_senmaj = V201647) %>%
  rename(knowl_housemaj = V201646) %>%
  subset(ofcrec_vp_correct >= 0) %>%
  subset(ofcrec_cj_correct >= 0) %>%
  subset(ofcrec_speaker_correct >= 0) %>%
  subset(knowl_housemaj > 0) %>%
  subset(knowl_senmaj > 0) %>%
  subset(resent_deserve > 0) %>%
  subset(resent_slavery > 0) %>%
  subset(resent_try > 0) %>%
  subset(resent_workway > 0) %>%
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
                                   "1" = "Correct"))
#remove labels because dta
anes_2012 <- remove_labels(anes_2012)
anes_2016 <- remove_labels(anes_2016)
anes_2020 <- remove_labels(anes_2020)

df <- rbind(anes_2012, anes_2016, anes_2020)
#15124 obs

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
                                    "2" = 1)) %>%
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
                                   "1" = "Agree Strongly",
                                   "2" = "Agree",
                                   "3" = "Neither",
                                   "4" = "Disagree",
                                   "5" = "Disagree Strongly")) %>%
  mutate(resent_workway_f = as.character(resent_workway),
         resent_workway_f = recode(resent_workway,
                                   "1" = "Agree Strongly",
                                   "2" = "Agree",
                                   "3" = "Neither",
                                   "4" = "Disagree",
                                   "5" = "Disagree Strongly"))
  
  
#create additive 0-1 sophistication scale
#each right answer is worth 1pt
df$soph <- (df$ofcrec_vp_correct + df$ofcrec_cj_correct + 
              df$ofcrec_speaker_correct +
              df$knowl_housemaj + df$knowl_senmaj)/5
summary(df$soph)

write_csv(df, "data/anes_combined.csv")