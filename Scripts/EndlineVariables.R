# Egypt Analysis Replication File: EndlineVariables
# This Script codes all the endline variables used in our Analysis.
# 06/06/2021
library(stargazer)
library(xlsx)
library(plyr) # revalue function
library(stringr)
library(dplyr)
library(tidyr) # replace_na function

## ----  Read the merged data with coded baseline responses ---- 
merged <- read.csv('Datasets/Baseline/merged_coded_baseline_variables.csv', fileEncoding = "latin1")

# filter to only those who responded to the endline survey
merged <- filter(merged, responded == 1)

## ---- Attitudes ---- 
# E41 - Husb Final Say (husb_final_say_end)
# E43 - Husb Income (husb_inc_end)
# E44 - Yelling Just (husb_just_yell_end)
# E46 - Future Equal Say (future_equal_say_end)
# E47 - Future Equal Rights (future_equal_rights_end)
# E48 - Working Outside Household Independence (woman_work_outside_end)
# E49 - Male colleague comments on look (colleague_comment_looks_end)
# E50 - Intervene work harassment(intervene_work_harass_end)
# E51 - Clothing justifies harassing (women_clothes_harass_end)
# E52 - Intervene hitting on the street (intervene_hitting_street)
# E53 - Intervene sexually harassing street(you_intervene_harass_end)
# E54 - Avoid authorities for shame (parents_assault_auth_end)
# E55 - Relative harass of child seriousness (child_relative_assault_end)
# E56 - Female circumcision is important for marriage (female_circ_marriage_end)

# True or False questions
# E57 - Verbal harassment has legal consequences (verbal_harassment_legal_end)
# E58 - Khul: women can divorce husb withouth a reason (khul_divorce_tf_end)
# E59 - Marriage permitted for under age 18 with familiar consent (marriage_age_tf_end)
# E60 - Female circumcision health benefits (circ_tf_end)

map_agree <- c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly agree" = 5)

# E41
merged$husb_final_say_end_num <- as.numeric(revalue(merged$husb_final_say_end, map_agree))

# E43
merged$husb_inc_end_num <- as.numeric(revalue(merged$husb_inc_end, map_agree))

# E44 
merged$husb_just_yell_end_num <- as.numeric(revalue(merged$husb_just_yell_end, map_agree))

# E46
merged$future_equal_say_end_num <- as.numeric(revalue(merged$future_equal_say_end, map_agree))

# E47
merged$future_equal_rights_end_num <- as.numeric(revalue(merged$future_equal_rights_end, map_agree))

# E48
merged$woman_work_outside_end_num <- as.numeric(revalue(merged$woman_work_outside_end, map_agree))

# E49
merged$colleague_comment_looks_end_num <- as.numeric(revalue(merged$colleague_comment_looks_end, map_agree))

# E50
merged$intervene_work_harass_end_num <- as.numeric(revalue(merged$intervene_work_harass_end, map_agree))

# E51
merged$women_clothes_harass_end_num <- as.numeric(revalue(merged$women_clothes_harass_end, map_agree))

# E52
merged$intervene_hitting_street_num <- as.numeric(revalue(merged$intervene_hitting_street, map_agree))

# E53
merged$you_intervene_harass_end_num <- as.numeric(revalue(merged$you_intervene_harass_end, map_agree))

# E54
merged$parents_assault_auth_end_num <- as.numeric(revalue(merged$parents_assault_auth_end, map_agree))

# E55
merged$child_relative_assault_end_num <- as.numeric(revalue(merged$child_relative_assault_end, map_agree))

# E56 
merged$female_circ_marriage_end_num <- as.numeric(revalue(merged$female_circ_marriage_end, map_agree))

# TRUE or FALSE questions
# We coded and indicator for each question according to whether they answered correctly 
# omitted answers "NA" are considered incorrect responses

# E57: right answer = true
merged <- mutate(merged, verbal_harassment_legal_end_num = ifelse(merged$verbal_harassment_legal_end == "True",1,0))
merged$verbal_harassment_legal_end_num <- replace_na(merged$verbal_harassment_legal_end_num, 0)

# E58: right answer = true
merged <- mutate(merged, khul_divorce_tf_end_num = ifelse(merged$khul_divorce_tf_end == "True",1,0))
merged$khul_divorce_tf_end_num <- replace_na(merged$khul_divorce_tf_end_num, 0)

# E59: right answer = false
merged <- mutate(merged, marriage_age_tf_end_num = ifelse(merged$marriage_age_tf_end == "False",1,0))
merged$marriage_age_tf_end_num <- replace_na(merged$marriage_age_tf_end_num, 0)

# E60: right answer = false
merged <- mutate(merged, circ_tf_end_num = ifelse(merged$circ_tf_end == "False",1,0))
merged$circ_tf_end_num <- replace_na(merged$circ_tf_end_num, 0)

## ---- Behavior and Reporting ---- 

# E61 - bcovid_yelled_end
# E62 - dcovid_yelled_end
# E63 - bcovid_hit_end
# E64 - dcovid_hit_end
# E65 - bcovid_assault_end
# E66 - dcovid_assault_end

# Given the low frequency of "I refuse to answer" responses, these were coded as 0
map_time <- c("I refuse to answer" = 0, "Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Very often" = 5)

#E61
length(which(merged$bcovid_yelled_end == "I refuse to answer")) # = 36
merged$bcovid_yelled_end_num <- as.numeric(revalue(merged$bcovid_yelled_end, map_time))

#62
length(which(merged$dcovid_yelled_end == "I refuse to answer")) # = 39
merged$dcovid_yelled_end_num <- as.numeric(revalue(merged$dcovid_yelled_end, map_time))

#63
length(which(merged$bcovid_hit_end == "I refuse to answer")) # = 48
merged$bcovid_hit_end_num <- as.numeric(revalue(merged$bcovid_hit_end, map_time))

#64
length(which(merged$dcovid_hit_end == "I refuse to answer")) # = 50
merged$dcovid_hit_end_num <- as.numeric(revalue(merged$dcovid_hit_end, map_time))

#65
length(which(merged$bcovid_assault_end == "I refuse to answer")) # = 85
merged$bcovid_assault_end_num <- as.numeric(revalue(merged$bcovid_assault_end, map_time))

#66
length(which(merged$dcovid_assault_end == "I refuse to answer")) # = 81
merged$dcovid_assault_end_num <- as.numeric(revalue(merged$dcovid_assault_end, map_time))

## ---- Hypothetical Behavior ---- 
# E67 - dv_talkhusb_end
# E68 - dv_talktfam_end
# E69 - dv_report_end
# E70 - dv_onlineres_end
# E71 - dv_contactorg_end
# E72 - sa_talkfam_end
# E73 - sa_report_end
# E74 - sa_onlineres_end
# E75 - sa_contactorg_end

like <- c("Very unlikely" = 1, "Unlikely" = 2 , "Neither likely nor unlikely" = 3, "Likely" = 4 ,"Very likely" = 5)

# E67 
merged$dv_talkhusb_end_num <- as.numeric(revalue(merged$dv_talkhusb_end, like))

# E68
merged$dv_talktfam_end_num <- as.numeric(revalue(merged$dv_talktfam_end, like))


# E69
merged$dv_report_end_num <- as.numeric(revalue(merged$dv_report_end, like))

# E70
merged$dv_onlineres_end_num <- as.numeric(revalue(merged$dv_onlineres_end, like))

# E71 
merged$dv_contactorg_end_num <- as.numeric(revalue(merged$dv_contactorg_end, like))

# E72
merged$sa_talkfam_end_num <- as.numeric(revalue(merged$sa_talkfam_end, like))

# E73
merged$sa_report_end_num <- as.numeric(revalue(merged$sa_report_end, like))

# E74
merged$sa_onlineres_end_num <- as.numeric(revalue(merged$sa_onlineres_end, like))

# E75
merged$sa_contactorg_end_num <- as.numeric(revalue(merged$sa_contactorg_end, like))

## ---- Knowledge ---- 
# E76 - options_dv_end (complete)
# E77 - options_sexassault_end (Complete)
# E78 - online_dvsa_end (Waiting Translation)
# E79 - bcvovid_accessonline_end
# E80 - dcovid_accessonline_end
# E81 - org_dvsa_end (INCOMPLETE)
# E82 - bcovid_contactorg_end
# E83 - dcovid_contactorg_end

# E76: Options available to a woman affected by domestic violence
# Create a txt file to translate with the Arabic responses: "Other"
options_dv <- cbind(grep('Other', merged$options_dv_end), merged$options_dv_text_end[grep('Other', merged$options_dv_end)])

colnames(options_dv) <- c("index", "answer")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(options_dv, file = 'Datasets/Treatment for other endline/options_dv.txt', fileEncoding = "UTF-8")

# Read the translated document
options_dv_end <- read.xlsx("Datasets/Treatment for other endline/options_dv_text_end_translated.xlsx", 
                            "options_dv", colIndex = c(1,3))

# change the Other values 
merged$options_dv_end <- replace(merged$options_dv_end , list = options_dv_end$index, 
                                 options_dv_end$Choice.Answer)

merged$options_dv_end_org_num <- 0
merged[grep("organization", merged$options_dv_end, value = F), "options_dv_end_org_num"] <- 1

merged$options_dv_end_res_num <- 0
merged[grep("online resources", merged$options_dv_end, value = F), "options_dv_end_res_num"] <- 1

# E77: Options Sexual Assault
# Create a txt file to translate with the Arabic responses: "Other"
options_sa<- cbind(grep('Other', merged$options_sexassault_end), 
                   merged$options_sexassault_text_end[grep('Other', merged$options_sexassault_end)])

colnames(options_sa) <- c("index", "text response")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(options_sa, file = 'Datasets/Treatment for other endline/options_sa.txt', fileEncoding = "UTF-8")

# Read the translated document
options_sexassault_end <- read.xlsx("Datasets/Treatment for other endline/options_sexassault_text_end_translated.xlsx", 
                                    "options_sa", colIndex = c(1,3))

# change the Other values 
merged$options_sexassault_end <- replace(merged$options_sexassault_end , list = options_sexassault_end$index, 
                                         options_sexassault_end$Choice.Answer)

merged$options_sexassault_end_org_num <- 0
merged[grep("org", merged$options_sexassault_end, value = F), "options_sexassault_end_org_num"] <- 1


merged$options_sexassault_end_res_num <- 0
merged[grep("online", merged$options_sexassault_end, value = F), "options_sexassault_end_res_num"] <- 1

# E78: 
length(which(merged$online_dvsa_end == "I do not know any.")) # 2559

# Create a txt file to translate with the Arabic responses: "I do not know any."
index<- which(merged$online_dvsa_end != "I do not know any.")
online_dvsa_1_5_end <-cbind(as.integer(index), merged$online_dvsa_end[index],
                            merged$online_dvsa_1_end[index], merged$online_dvsa_2_end[index], merged$online_dvsa_3_end[index], 
                            merged$online_dvsa_4_end[index], merged$online_dvsa_5_end[index])

colnames(online_dvsa_1_5_end) <- c("index", "online_dvsa_end", "online_dvsa_1_end", "online_dvsa_2_end", "online_dvsa_3_end", "online_dvsa_4_end", "online_dvsa_5_end")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(online_dvsa_1_5_end, file = 'Datasets/Treatment for other endline/online_dvsa_1_5_end.txt', fileEncoding = "UTF-8")

options_sa <- cbind(grep('Other', merged$options_sexassault_end), 
                    merged$options_sexassault_text_end[grep('Other', merged$options_sexassault_end)])

# Read the translated document
online_dvsa_translated <- read.xlsx("Datasets/Treatment for other endline/online_dvsa_1_5_end_translated_0419.xlsx", 
                                    "online_dvsa_1_5_end", colIndex = c(1,3,4,5,6,7,8))

online_dvsa_translated$pasted_answer <- mapply(paste, sep = ",", online_dvsa_translated$online_dvsa_1_end, online_dvsa_translated$online_dvsa_2_end, 
                                               online_dvsa_translated$online_dvsa_3_end.1, online_dvsa_translated$online_dvsa_4_end,
                                               online_dvsa_translated$online_dvsa_5_end)

merged$aux <- 0
merged$aux <- replace(merged$aux , list = online_dvsa_translated$index, 
                      online_dvsa_translated$pasted_answer)

# Create a variable for valid responses excluding authorities and ECWR
merged$online_dvsa_noaut_end_num <- 0
merged[grep("organization", merged$aux, value = F), "online_dvsa_noaut_end_num"] <- 1
merged[grep("social_media", merged$aux, value = F), "online_dvsa_noaut_end_num"] <- 1
merged[grep("media", merged$aux, value = F), "online_dvsa_noaut_end_num"] <- 1
merged[grep("internet", merged$aux, value = F), "online_dvsa_noaut_end_num"] <- 1

# Only ECWR as valid response
merged$online_dvsa_nehad_end_num <- 0
merged[grep("ecwr_nehad", merged$aux, value = F), "online_dvsa_nehad_end_num"] <- 1

# For the pertinent questions ahead, 
# we coded "I don't know" responses to the middle value; and refuse responses as non-responses = 0.
map_tiem_2 <- c("I refuse to answer" = 0,  "I don't know"  = 3, "Never" = 1, "Rarely" = 2, 
                "Sometimes" = 3, "Often" = 4, "Very often" = 5)

# E79: access online for sexual harassment or assault
merged$bcvovid_accessonline_end_num <- as.numeric(revalue(merged$bcvovid_accessonline_end, map_tiem_2))
merged$bcvovid_accessonline_end_num <- replace_na(merged$bcvovid_accessonline_end_num, 1)

# E80
merged$dcovid_accessonline_end_num <- as.numeric(revalue(merged$dcovid_accessonline_end, map_tiem_2))
merged$dcovid_accessonline_end_num <- replace_na(merged$dcovid_accessonline_end_num, 1)

# E81
# Create a txt file to translate with the Arabic responses: "I do not know any."
index<- which(merged$org_dvsa_end != "I do not know any.")
org_dvsa_1_5_end <-cbind(as.integer(index), merged$org_dvsa_end[index],
                         merged$org_dvsa_1_end[index], merged$org_dvsa_2_end[index], merged$org_dvsa_3_end[index], 
                         merged$org_dvsa_4_end[index], merged$org_dvsa_5_end[index])

colnames(org_dvsa_1_5_end) <- c("index", "org_dvsa_end", "org_dvsa_1_end", "org_dvsa_2_end", "org_dvsa_3_end", "org_dvsa_4_end", "org_dvsa_5_end")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(org_dvsa_1_5_end, file = 'Datasets/Treatment for other endline/org_dvsa_1_5_end.txt', fileEncoding = "UTF-8")

# Read translated document
org_dvsa_translated <- read.xlsx("Datasets/Treatment for other endline/org_dvsa_1_5_end_translated_0419.xlsx", 
                                 "online_dvsa_1_5_end", colIndex = c(1,3,4,5,6,7))

org_dvsa_translated$pasted_answer <- mapply(paste, sep = ",", org_dvsa_translated$org_dvsa_1_end, org_dvsa_translated$org_dvsa_2_end, 
                                            org_dvsa_translated$org_dvsa_3_end, org_dvsa_translated$org_dvsa_4_end,
                                            org_dvsa_translated$org_dvsa_5_end)

merged$aux <- 0
merged$aux <- replace(merged$aux , list = org_dvsa_translated$index, 
                      org_dvsa_translated$pasted_answer)

# Create a variable for valid responses excluding authorities and ECWR
merged$org_dvsa_noaut_end_num <- 0
merged[grep("organization", merged$aux, value = F), "org_dvsa_noaut_end_num"] <- 1

# Only ECWR
merged$org_dvsa_nehad_end_num <- 0
merged[grep("ecwr_nehad", merged$aux, value = F), "org_dvsa_nehad_end_num"] <- 1


# E82
merged$bcovid_contactorg_end_num <- as.numeric(revalue(merged$bcovid_contactorg_end, map_tiem_2))
merged$bcovid_contactorg_end_num <- replace_na(merged$bcovid_contactorg_end_num, 1)

# E83
merged$dcovid_contactorg_end_num <- as.numeric(revalue(merged$dcovid_contactorg_end, map_tiem_2))
merged$dcovid_contactorg_end_num <- replace_na(merged$dcovid_contactorg_end_num, 1)

## ---- Revealed Preference ---- 
# Continuous variable
map_donation <- c("No, I would not like to donate any money, and thus prefer to receive 25 EGP mobile credit for participating in this survey." 
                  = 0,
                  "Yes, I would like to donate 10 EGP, and thus receive only 15 EGP mobile credit for participating in this survey." = 10, 
                  "Yes, I would like to donate 15 EGP, and thus receive only 10 EGP mobile credit for participating in this survey." = 15,
                  "Yes, I would like to donate 25 EGP and thus receive no mobile credit for participating in this survey." = 25)

merged$donation__end_cont <- as.numeric(revalue(merged$donation_end, map_donation))

# Dummy to whether they donated any amount
merged <- mutate(merged, donation_end_num = ifelse(
  merged$donation_end == "No, I would not like to donate any money, and thus prefer to receive 25 EGP mobile credit for participating in this survey.",0,1))

## ---- First stage for TV show ---- 
# E17 - Watches TV morning/afternoon/evening
# E19 - TV Channels of TV show
# E20 - TV Show
# E21 - Mentioned watched TV show Saturday evening
# E22 - 
# E23 - 
# E24 - 
# E25 - 
# E26 - 
# E27 - 
# E28 - 

# E17 - Watches TV morning/afternoon/evening
merged <- mutate(merged, tv_morning_num_end = ifelse(merged$tv_morn_end == "Yes",1,0))
merged <- mutate(merged, tv_afternoon_num_end = ifelse(merged$tv_aft_end == "Yes",1,0))
merged <- mutate(merged, tv_evening_num_end = ifelse(merged$tv_eve_end == "Yes",1,0))

# E19: TV Channels
# Create a txt file to translate with the Arabic responses: "Other, please specify:"
merged$three_viewed_channel_end_num <- 0

# if "Al-Kahera Wal Nas" is one of the top 3 shows watched, then tv_top3_chan_num = 1
merged[grep("Al-Kahera Wal Nas", merged$three_viewed_channel_end, value = F), "three_viewed_channel_end_num"] <- 1

channel_end <- cbind(as.integer(which(merged$three_viewed_channel_end == "Other, please specify:")),
                     merged$three_viewed_channel_text_end[which(merged$three_viewed_channel_end == "Other, please specify:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(channel_end, file = 'Datasets/Treatment for other endline/channel_end.txt', fileEncoding = "UTF-8")

# Read translated document
channel_end_translated <- read.xlsx("Datasets/Treatment for other endline/channel_end_translated.xlsx", 
                                    "channel_end", colIndex = c(1,4))

# change the Other values 
merged$three_viewed_channel_end_num <- replace(merged$three_viewed_channel_end_num , list = channel_end_translated$index, 
                                               channel_end_translated$Choice.Answer)

merged$three_viewed_channel_end_num <- replace_na(merged$three_viewed_channel_end_num, 0)

# E20: TV Shows
# Create a txt file to translate with the Arabic responses: "Other, please specify:"
three_viewed_shows <- cbind(as.integer(which(merged$three_viewed_shows_end == "Other")),
                            merged$three_viewed_shows_text_end[which(merged$three_viewed_shows_end == "Other")])

# Output the text answers
Sys.setlocale("LC_CTYPE", "arabic")
write.csv(three_viewed_shows, file = 'Datasets/Treatment for other endline/three_viewed_shows.txt', fileEncoding = "UTF-8")


# Read translated document
three_viewed_shows_Translated <- read.xlsx("Datasets/Treatment for other endline/three_viewed_shows_translated.xlsx", 
                                           "three_viewed_shows", colIndex = c(1,4))

# change the Other values 
merged$three_viewed_shows_end <- replace(merged$three_viewed_shows_end , list = three_viewed_shows_Translated$index, 
                                         three_viewed_shows_Translated$Choice.Answer )

# initialize new dummy column as 0's
merged$three_viewed_shows_end_num <- 0

# If the type "Legal awareness" or "Family shows" is found then, tv_top3_shows_num = 1
merged[grep('Legal awareness|Family shows', merged$three_viewed_shows_end, value = F), 
       "three_viewed_shows_end_num"] <- 1


# E21: TV Sat Show 
# Create a txt file to translate with the Arabic responses: "Show:"
sat_2mos_end <- cbind(as.integer(which(merged$tv_sat_2mos_end == "Show:")),
                      merged$tv_sat_2mos_text_end[which(merged$tv_sat_2mos_end == "Show:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(sat_2mos_end, file = 'Datasets/Treatment for other endline/sat_2mos_end.txt', fileEncoding = "UTF-8")

# Read translated document
sat_2mos_end_translated <- read.xlsx("Datasets/Treatment for other endline/sat_2mos_end_translated.xlsx", 
                                     "sat_2mos_end", colIndex = c(1,3))

merged$tv_sat_2mos_end_num <- 0
merged$tv_sat_2mos_end_num <- replace(merged$tv_sat_2mos_end_num , list = sat_2mos_end_translated$index, 
                                      sat_2mos_end_translated$nehads_show)

# Cross Tab for sat shows 
xtabs(~ merged$tv_sat_2mos_end + tv_sat_2mos_end_num, data = merged, addNA =TRUE) # only 183 answers = 1

# E22: Watch Hekayat 
merged <- mutate(merged, watch_hekayat_end_num = ifelse(merged$watch_hekayat_end == "Yes",1,0))

merged$watch_hekayat_end_num <- replace_na(merged$watch_hekayat_end_num, 0)

# cross tabulation
mytable <- xtabs(~watch_hekayat_end+watch_hekayat_end_num, data=merged, addNA = TRUE)
ftable(mytable)

# E23: Heard of Hekayat
unique(merged$heard_hekayat_end)

merged <- mutate(merged, heard_hekayat_end_num = ifelse(merged$heard_hekayat_end == "Yes",1,0))
merged$heard_hekayat_end_num[which(merged$watch_hekayat_end == "Yes")] <- 1
merged$heard_hekayat_end_num <- replace_na(merged$heard_hekayat_end_num, 0)

# cross tabulation
mytable <- xtabs(~heard_hekayat_end+heard_hekayat_end_num, data=merged, addNA = TRUE)
ftable(mytable)

# E24: How Heard of Hekayat 

# heard of it via WhatsApp
merged$how_heard_hekayat_num <- 0
merged[grep('I have heard of it via WhatsApp', merged$how_heard_hekayat, value = F), 
       "how_heard_hekayat_num"] <- 1

# heard of it via social media (Facebook)
merged$how_heard_hekayat_sm_num <- 0
merged[grep('Facebook', merged$how_heard_hekayat, value = F), 
       "how_heard_hekayat_sm_num"] <- 1

# E25: Reminder over WhatsApp to watch Hekayat Nehad 
merged <- mutate(merged, whatsapp_remind_2mos_end_num = ifelse(merged$whatsapp_remind_2mos_end == "Yes",1 ,0))

# We coded NA as 0, since they did not receive a remainder
merged$whatsapp_remind_2mos_end_num <- replace_na(merged$whatsapp_remind_2mos_end_num, 0)

# E26: How many episodes did you watch?
map_ep <- c("Between 1 and 3" = 1, "Between 4 and 6" = 2, "Between 7 and 9" = 3, 
            "I don't know" = 2, "I refuse to answer" = 0)

# NA = 0 since people did not watch the TV show
merged$episodes_hekayat_end_num <- as.numeric(revalue(merged$episodes_hekayat_end, map_ep))
merged$episodes_hekayat_end_num <- replace_na(merged$episodes_hekayat_end_num, 0)

# New variable: Whether Watched Episode
merged$wheter_episodes_hekayat_end_num <- ifelse(merged$episodes_hekayat_end_num > 0, 1, 0)

# E27: Describe the content of episodes watched
# Create a txt file to translate with the Arabic responses: "Description:"
episodes_content_end <- cbind(as.integer(which(merged$episode_content_end == "Description:")),
                              merged$episode_content_text_end[which(merged$episode_content_end == "Description:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(episodes_content_end, file = 'Datasets/Treatment for other endline/episodes_content_end.txt', fileEncoding = "UTF-8")

# Read the translated document
episodes_content_end_translated <- read.xlsx("Datasets/Treatment for other endline/episodes_content_end_translated.xlsx", 
                                             "episodes_content_end", colIndex = c(1,4,5))

# We don't consider vague responses
merged$episodes_content_end_num_vague <- 0
merged$episodes_content_end_num_vague <- replace(merged$episodes_content_end_num_vague, list = episodes_content_end_translated$index,
                                                 episodes_content_end_translated$correct_answer - episodes_content_end_translated$correct_answer_vague)

# E28: Describe most liked topics 
# Create a txt file to translate with the Arabic responses: "Topic:"
episodes_topics_end <- cbind(as.integer(which(merged$episode_content_like_end == "Topic:")),
                             merged$episode_content_like_text_end[which(merged$episode_content_like_end == "Topic:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(episodes_topics_end, file = 'Datasets/Treatment for other endline/episodes_topics_end.txt', fileEncoding = "UTF-8")

# Read the translated document
episodes_topics_end_translated <- read.xlsx("Datasets/Treatment for other endline/episodes_topics_end_translated.xlsx", 
                                            "episodes_topics_end", colIndex = c(1,3,4))

# No vague responses
merged$episode_content_like_end_num_vague <- 0
merged$episode_content_like_end_num_vague <- replace(merged$episode_content_like_end_num_vague, list = episodes_topics_end_translated$index,
                                                     episodes_topics_end_translated$Choice.Answer - episodes_topics_end_translated$episodes_topics_end_vague)

## ---- First stage for Facebook and  WhatsApp Treatment ---- 
# Variables names 
# E33 - Videos of Empowerment and Support socmed
# E34 - Videos of Empowerment and Support WhatsApp
# E35 - Received Videos WA or FB
# E36 - Watched videos WA or FB
# E37 - Number of videos watched
# E38 - Content Watched
# E39 - Most Liked Topics

mapping_time <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Very often" = 5)

# E33: Videos of Empowerment and Support socmed 
# We considered NA responses as 1 since is equivalent to never seeing a video
merged$videos_socmed_2mos_end_num <- as.numeric(revalue(merged$videos_socmed_2mos_end, mapping_time))
merged$videos_socmed_2mos_end_num <- replace_na(merged$videos_socmed_2mos_end_num, 1)


# E34: Videos of Empowerment and Support WhatsApp 
merged$videos_whatsapp_2mos_end_num <- as.numeric(revalue(merged$videos_whatsapp_2mos_end, mapping_time))
merged$videos_whatsapp_2mos_end_num <- replace_na(merged$videos_whatsapp_2mos_end_num, 1)


# E35: Received Videos WA or FB 
merged$videos_whatsapp_receive_end_num <- ifelse(merged$videos_whatsapp_receive_end == "No" | 
                                                   merged$videos_whatsapp_receive_end == "I refuse to answer" |
                                                   merged$videos_whatsapp_receive_end == "I don't know",0,1)

# E36: Watched videos WA or FB 
# We considered NA responses as 0 since either the respondent did not receive the videos or did not watch them
merged$videos_watched_end_num <- ifelse(merged$videos_watched_end =="Yes", 1,0)
merged$videos_watched_end_num <- replace_na(merged$videos_watched_end_num, 0)

# E37: Number of videos watched 
# We consideed NA responses as 0, since If the respondents did not answer this questions is because they did not received the videos

number_map <- c("I refuse to answer" = 0, "Between 1 and 3" = 1, "Between 4 and 6" = 2, 
                "Between 7 and 9" = 3, "Between 10 and 13" = 4,
                "I don't know" = 2)

merged$videos_watched_no_end_num <- as.numeric(revalue(merged$videos_watched_no_end, number_map))
merged$videos_watched_no_end_num <- replace_na(merged$videos_watched_no_end_num, 0)

# E38: Content Watched
# Create a txt file to translate with the Arabic responses: "Description:"
length(which(merged$videos_watched_content_end == "Description")) # = 1260

videos_watched_content <- cbind(as.integer(which(merged$videos_watched_content_end == "Description")), 
                                merged$videos_watched_content_text_end[which(merged$videos_watched_content_end == "Description")])

colnames(videos_watched_content) <- c("index", "videos_watched_content")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(videos_watched_content, file = 'Datasets/Treatment for other endline/videos_watched_content.txt', fileEncoding = "UTF-8")

# Read the translated document
videos_watched_content_translated <- read.xlsx("Datasets/Treatment for other endline/videos_watched_content_translated.xlsx", 
                                               "videos_watched_content", colIndex = c(1,3,4))

# No vague responses
merged$videos_watched_content_end_num_vague <- 0

merged$videos_watched_content_end_num <- replace(merged$videos_watched_content_end_num, list = videos_watched_content_translated$index,
                                                 videos_watched_content_translated$choice_answer_vid_content)

merged$videos_watched_content_end_num_vague <- replace(merged$videos_watched_content_end_num_vague, list = videos_watched_content_translated$index, videos_watched_content_translated$choice_answer_vid_content - videos_watched_content_translated$vague_answer_vid_content)

# E39: Most Liked Topics 
# Ix Topics accurate
# Create a txt file to translate with the Arabic responses: "Description:"

length(which(merged$video_content_like_end == "Description")) # = 1497


videos_liked_topics <- cbind(as.integer(which(merged$video_content_like_end == "Description")), 
                             merged$video_content_like_end_text[which(merged$video_content_like_end == "Description")])

colnames(videos_liked_topics) <- c("index", "videos_liked_topics")

# Export bcovid_Activity as txt file with code UTF-8. need to import in excel as UTF-8 to read Arabic 
Sys.setlocale("LC_CTYPE", "arabic")
write.csv(videos_liked_topics, file = 'Datasets/Treatment for other endline/videos_liked_topics.txt', fileEncoding = "UTF-8")

# Read the translated document
videos_liked_topics_translated <- read.xlsx("Datasets/Treatment for other endline/videos_liked_topics_translated.xlsx", 
                                            "videos_liked_topics", colIndex = c(1,3,4))

# No vague responses
merged$video_content_like_end_num_vague <- 0
merged$video_content_like_end_num_vague <- replace(merged$video_content_like_end_num_vague, list = videos_liked_topics_translated$index,
                                                   videos_liked_topics_translated$videos_liked_topics_01 - 
                                                     videos_liked_topics_translated$videos_liked_topics_vague)

## ---- new folder ---- 
dir.create(file.path("Datasets", "Finaldata"), showWarnings = FALSE)

## ---- final data ---- 
# Save the new data with baseline + endline variables coded as our final dataset
write.csv(merged, file = 'Datasets/FinalData/Finaldata.csv', row.names = F)
