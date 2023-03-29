# Egypt Analysis Replication File
# Handling duplicated responses - extra analysis
# 28/01/2023

## ----  Read the dups data with baseline and endline responses
original <- read_excel("Datasets/InitialData/merged_baseline_endline_030921.xlsx")

# Filter only to those who responded the endline
original <- filter(original, responded == 1)

# Original duplicated data ids (# we dropped 227? - This is wrong!)
dups_ids <- read.csv("Datasets/InitialData/replied_endline_baselineIDs_dupes11012020_nopii.csv")
length(unique(dups_ids$responseidx)) # 217 respondents that filled out the survey at least twice
sum(table(dups_ids$responseidx) == 2) # 199 responded exactly twice
sum(table(dups_ids$responseidx) == 3) # 17 responded exactly twice
sum(table(dups_ids$responseidx) == 5) # 1 responded exactly twice

# Duplicated only data - New dataset
dups <- read_excel("Datasets/InitialData/merged_baseline_endline_dupesonly_open_01182023.xlsx")
length(unique(dups$ResponseId)) # 210 respondents that filled out the survey at least twice
sum(table(dups$ResponseId) == 2) # 194 responded exactly twice
sum(table(dups$ResponseId) == 3) # 16 responded three times

# Select first response from each respondent
dups <- dups %>% group_by(ResponseId) %>% arrange(ResponseId, date) # Arrange within respondent 
dups <- dups[!duplicated(dups$ResponseId), ] # select first answer for each respondent
dups <- dups %>% ungroup()

# ------------------------------------------------------------------------------  
# Create a pooled variable for Facebook and WhatsApp Individual
dups$pooledF_WI <- 0
dups$pooledF_WI[which(dups$facebook_10018 == 1)] <- 1
dups$pooledF_WI[which(dups$whatsapp_10018 == 1)] <- 1

# Create a pooled variable for Facebook, WhatsApp Individual, and WhatsApp Group
dups$pooledF_WIG <- 0
dups$pooledF_WIG[which(dups$facebook_10018 == 1)] <- 1
dups$pooledF_WIG[which(dups$whatsapp_10018 == 1)] <- 1
dups$pooledF_WIG[which(dups$in_group_10018 == 1)] <- 1

# Create a pooled variable for all treatmetns indicators
dups$pooledT <- 0
dups$pooledT[which(dups$control_10018 != 1)] <- 1

## ---- Demographic variables ----
# Variable Names
#B1 - Gender
#B2 - Age
#B3 - Status 
#B4 - Husb age
#B5 - length marriage
#B6 - husb lives loc
#B7 - male children
#B8 - female children
#B9 - family members
#B10 - education
#B11 - husb education
#B12 - bC main act
#B13 - bc husb main act
#B14 - bc main act
#B15 - bc husb main act
#B16 - inc decline c19

# B7-8: We replace NA values of Male/female children with 0 
dups$male_children[dups$male_children == "More than 5"] <- 5
dups$female_children[dups$female_children == "More than 5"] <- 5
dups$male_children <- as.numeric(dups$male_children)
dups$female_children <- as.numeric(dups$female_children)

dups$male_children <- replace_na(dups$male_children, 0)
dups$female_children <- replace_na(dups$female_children, 0)

# B9: Adding up family members other than yourself, children, and husb
dups$sum_family_mem <- rowSums(sapply(dups[, c(28:39)],
                                        function(x) as.numeric(as.character(x))), na.rm = TRUE)

ind1 <- which(dups$married != 1 )

# B11: Husband education
dups <- mutate(dups, education_husb_num =  
                   ifelse(education_husb == "BA" | education_husb == "MA and above", 1, 0),
                 education_husb_num = replace_na(education_husb_num, 0))

dups$education_husb_num[ind1] <- NA

# B12: Before COVID-19 main act 
# Create a txt file to translate with the Arabic responses: "Other"
bcovid_activity <- cbind(as.integer(which(is.na(dups$bcovid_act_mar))), 
                         dups$bcovid_act_mar_text[which(is.na(dups$bcovid_act_mar))],
                         dups$bcovid_mainact_unm_text[which(is.na(dups$bcovid_act_mar))]
)
dups$bcovid_activity <- dups$bcovid_act_mar

colnames(bcovid_activity) <- c("index", "answer_mar", "answer_unm")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(bcovid_activity, file = 'Datasets/Treatment for other baseline/Dups/bcovid_activity.txt', fileEncoding = "UTF-8")

# Read the translated document
bcovid_activity_trans <- read.xlsx("Datasets/Treatment for other baseline/Dups/bcovid_activity_translated.xlsx", "bcovid_activity", colIndex = c(1,5))

dups$bcovid_activity <- replace(dups$bcovid_activity, list = bcovid_activity_trans$index, 
                                  bcovid_activity_trans$Choice.Answer)
dups$bcovid_activity <- coalesce(dups$bcovid_activity, dups$bcovid_mainact_unm) # merge married and unmarried in a single

# Variable for full time at home
dups <- mutate(dups, bcovid_activity_num =  
                   ifelse(bcovid_activity == "Work from inside of the household (not including housework or taking care of family)" | 
                            bcovid_activity == "Work inside the house doing housework or taking care of family" |
                            bcovid_activity =="Not working at home", 1, 0),
                 bcovid_activity_num = replace_na(bcovid_activity_num, 0))

# Partially at home variable
dups <- mutate(dups, bcovid_activity_ph_num =  
                   ifelse(bcovid_activity == "Study" | 
                            bcovid_activity == "Work partly outside the household and partly inside the house", 1, 0),
                 bcovid_activity_ph_num = replace_na(bcovid_activity_ph_num, 0))

# B13: Before COVID husband main activity
# Instead of creating file since there only 4 answers to translate, we translate them here 
which(!is.na(dups$bcovid_act_husbmar_text)) # Is only for married, thus fewer
dups$bcovid_act_husbmar[29] <- "Work outside of the household" # dups$bcovid_act_husbmar_text[29] = "Traveling outside Egypt for work"
dups$bcovid_act_husbmar[146] <- "Not working at home" # dups$bcovid_act_husbmar_text[146] = "Not working on retirement"
dups$bcovid_act_husbmar[169] <- "Work outside of the household" # dups$bcovid_act_husbmar_text[169] = "Sleep, then eat, then go to his friends"
dups$bcovid_act_husbmar[190] <- "Not working at home" # dups$bcovid_act_husbmar_text[190] = "does not work"

dups <- mutate(dups, bcovid_act_husbmar_num =  
                   ifelse(bcovid_act_husbmar == "Work from inside of the household (not including housework or taking care of family)" | 
                            bcovid_act_husbmar == "Work inside the house doing housework or taking care of family" |
                            bcovid_act_husbmar =="Not working at home", 1, 0),
                 bcovid_act_husbmar_num = replace_na(bcovid_act_husbmar_num, 0))

# Partially at home variable
dups <- mutate(dups, bcovid_activity_husbmar_ph_num =  
                   ifelse(bcovid_act_husbmar == "Study" | 
                            bcovid_act_husbmar == "Work partly outside the household and partly inside the house", 1, 0),
                 bcovid_activity_husbmar_ph_num = replace_na(bcovid_activity_husbmar_ph_num, 0))

# B14: dc main act 
# Create a txt file to translate with the Arabic responses: "Other"
which(!is.na(dups$dcovid_mainact_mar_text))
which(!is.na(dups$dcovid_mainact_unm_text))

dups$dcovid_mainact_mar[121] = "Not working at home" # dups$dcovid_mainact_mar_text[121] = The workplace has been closed and waxed, and there is no way for Maabasha
dups$dcovid_mainact_mar[182] = "Work from inside of the household (not including housework or taking care of family)" # dups$dcovid_mainact_mar_text [182] = I work as a school
dups$dcovid_mainact_mar[16] = "Study" # dups$dcovid_mainact_unm_text[16] = Home work and study
dups$dcovid_mainact_mar[23] = "Study" # dups$dcovid_mainact_unm_text[23] = Studying, doing some fun online and spending time with family
dups$dcovid_mainact_mar[41] = "Work inside the house doing housework or taking care of family" # dups$dcovid_mainact_unm_text[41] = Sitting at home helping my family and studying online
dups$dcovid_mainact_mar[100] = "Not working at home" # dups$dcovid_mainact_unm_text[100] = Online exercises
dups$dcovid_mainact_mar[183] = "Not working at home" # dups$dcovid_mainact_unm_text[183] = I do not work

dups$dcovid_activity <- coalesce(dups$dcovid_mainact_mar, dups$dcovid_mainact_unm)

dups <- mutate(dups, dcovid_activity_num =  
                   ifelse(dcovid_activity == "Work from inside of the household (not including housework or taking care of family)" | 
                            dcovid_activity == "Work inside the house doing housework or taking care of family" |
                            dcovid_activity =="Not working at home", 1, 0),
                 dcovid_activity_num = replace_na(dcovid_activity_num, 0))

# Partially at home variable
dups <- mutate(dups, dcovid_activity_ph_num =  
                   ifelse(dcovid_activity == "Study" | 
                            dcovid_activity == "Work partly outside the household and partly inside the house", 1, 0),
                 dcovid_activity_ph_num = replace_na(dcovid_activity_ph_num, 0))

# B15 - dc husb main act 
dcovid_activity_husb <- cbind(as.integer(which(dups$dcovid_mainact_husbmar == "Other")),
                              dups$dcovid_mainact_husbmar_text[which(dups$dcovid_mainact_husbmar == "Other")])

colnames(dcovid_activity_husb) <- c("index", "answer")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(dcovid_activity_husb, file = 'Datasets/Treatment for other baseline/Dups/dcovid_activity_husb.txt', fileEncoding = "UTF-8")

# read the translated document
dcovid_activity_husb_trans <- read.xlsx("Datasets/Treatment for other baseline/Dups/dcovid_activity_husb_translated.xlsx", "dcovid_activity_husb", colIndex = c(1,4))

# Create a new variable for main activity of the husband
dups$dcovid_activity_husb <- replace(dups$dcovid_mainact_husbmar, list = dcovid_activity_husb_trans$index, 
                                       dcovid_activity_husb_trans$Choice.Answer)

dups <- mutate(dups, dcovid_activity_husb_num =  
                   ifelse(dcovid_activity_husb == "Work from inside of the household (not including housework or taking care of family)" | 
                            dcovid_activity_husb == "Work inside the house doing housework or taking care of family" |
                            dcovid_activity_husb =="Not working at home", 1, 0),
                 dcovid_activity_husb_num = replace_na(dcovid_activity_husb_num, 0))

# Partially at home variable
dups <- mutate(dups, dcovid_activity_husbmar_ph_num =  
                   ifelse(dcovid_activity_husb == "Study" | 
                            dcovid_activity_husb == "Work partly outside the household and partly inside the house", 1, 0),
                 dcovid_activity_husbmar_ph_num = replace_na(dcovid_activity_husbmar_ph_num, 0))

## ---- First stage for TV show ----
# Variable Names
# B17 - TV Time
# B18 - TV Option
# B19 - TV Channels
# B20 - TV Shows
# B21 - TV Sat Show 

# B17: TV Time 
# Indicator an indicator for each: Watching TV morning, afternoon, and evening
dups <- mutate(dups, tv_morning_num = ifelse(dups$tv_morning == "Yes",1,0))
dups <- mutate(dups, tv_afternoon_num = ifelse(dups$tv_afternoon == "Yes",1,0))
dups <- mutate(dups, tv_evening_num = ifelse(dups$tv_evening == "Yes",1,0))

# B18 - TV Option 
dups <- mutate(dups, tv_sattelite_num = ifelse(dups$tv_sattelite == "Yes",1,0))

# B19: TV Channels 
# Initialize all values of the numeric variable in 0
dups$tv_top3_chan_num <- 0

# if "Al-Kahera Wal Nas" is one of the top 3 shows watched, then tv_top3_chan_num = 1
dups[grep("Al-Kahera Wal Nas", dups$tv_top3_chan, value = F), "tv_top3_chan_num"] <- 1

# B20: TV Shows 
# Initialize all values of the numeric variable in 0
dups$tv_top3_shows_num <- 0

# If the type "Legal awareness" or "Family shows" is found then, tv_top3_shows_num = 1
dups[grep('Legal awareness|Family shows', dups$tv_top3_shows, value = F), 
       "tv_top3_shows_num"] <- 1

# B21: TV Sat Show  
# Create a txt file to translate with the Arabic responses: "Show:"
sat_show <- cbind(as.integer(which(dups$sat_evening == "Show:")),
                  dups$sat_evening_text[which(dups$sat_evening == "Show:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(sat_show, file = 'Datasets/Treatment for other baseline/Dups/sat_show.txt', fileEncoding = "UTF-8")

# Read the translated document
sat_show_Translated <- read.xlsx("Datasets/Treatment for other baseline/Dups/sat_show_translated.xlsx", "sat_show", colIndex = c(1,4))

dups$sat_show_num <- 0
dups$sat_show_num <- replace(dups$sat_show_num , list = sat_show_Translated$index, 
                               sat_show_Translated$hekayat_nehad)

## Social Media Habits
# B30 - Hours spent on social media
hours_map <- c("None" = 0, "Up to 2 hours" = 1, "Up to 5 hours" = 2, "Up to 10 hours" = 3,  "10 hours or more" = 4)

dups$hours_soc_med_num <- as.numeric(revalue(dups$hours_soc_med, hours_map))

# Indicator if WhatsApp is mentioned
dups$socmed_whatsapp <- 0
dups[grep('WhatsApp', dups$soc_med_use, value = F), 
       "socmed_whatsapp"] <- 1

# Indicator if Facebook is mentioned
dups$socmed_Facebook <- 0
dups[grep('Facebook', dups$soc_med_use, value = F), 
       "socmed_Facebook"] <- 1

# Indicator if Instagram is mentioned
dups$socmed_Instagram <- 0
dups[grep('Instagram', dups$soc_med_use, value = F), 
       "socmed_Instagram"] <- 1

# Indicator if YouTube is mentioned
dups$socmed_YouTube <- 0
dups[grep('YouTube', dups$soc_med_use, value = F), 
       "socmed_YouTube"] <- 1

# Indicator if Twitter is mentioned
dups$socmed_Twitter <- 0
dups[grep('Twitter', dups$soc_med_use, value = F), 
       "socmed_Twitter"] <- 1

# Indicator if Snapchat is mentioned
dups$socmed_Snapchat <- 0
dups[grep('Snapchat', dups$soc_med_use, value = F), 
       "socmed_Snapchat"] <- 1

# Indicator if Telegram is mentioned
dups$socmed_Telegram <- 0
dups[grep('Telegram', dups$soc_med_use, value = F), 
       "socmed_Telegram"] <- 1

## ---- First stage for Facebook and WhatsApp treatment ----
# Variable names
# B33 - violence socmed 
# B34 - violence whatsApp

# B33: violence socmed 
mapping_time1 <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Seen frequently" = 5)
dups$'X2mos_socmed_dv_num'<- as.numeric(revalue(dups$'2mos_socmed_dv', mapping_time1))

# B34: violence whatsApp 
mapping_time2 <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3,
                   "Often" = 4, "Received many" = 5)

dups$'X2mos_whatsapp_dv_num' <- as.numeric(revalue(dups$'2mos_whatsapp_dv', mapping_time2))

## Knowledge of and Experience of Accessing Resources
# B-78: Know Online
length(which(dups$know_online.x == "I do not know any.")) # = 151

dups$know_online_1_5_coded <- mapply(paste, sep = ",", dups$know_online_text1_coded, dups$know_online_text2_coded,
                                              dups$know_online_text3_coded, dups$know_online_text4_coded, dups$know_online_text5_coded)

# Create a variable for valid responses excluding authorities
dups$know_online_valid_noaut_num <- 0

dups[grep("2", dups$know_online_1_5_coded, value = F), "know_online_valid_noaut_num"] <- 1
dups[grep("3", dups$know_online_1_5_coded, value = F), "know_online_valid_noaut_num"] <- 1
dups[grep("4", dups$know_online_1_5_coded, value = F), "know_online_valid_noaut_num"] <- 1
dups[grep("5", dups$know_online_1_5_coded, value = F), "know_online_valid_noaut_num"] <- 1

# Only ECWR as valid response
dups$know_online_nehad_num <- 0
dups[grep("1", dups$know_online_1_5_coded, value = F), "know_online_nehad_num"] <- 1
dups[grep("6", dups$know_online_1_5_coded, value = F), "know_online_nehad_num"] <- 1
dups[grep("7", dups$know_online_1_5_coded, value = F), "know_online_nehad_num"] <- 1

# B81  
# B-81: Know Organization
length(which(dups$know_org.x == "I do not know any.")) # = 153

dups$know_org_1_5_coded <- mapply(paste, sep = ",", dups$know_org_text1_coded, dups$know_org_text2_coded,
                                     dups$know_org_text3_coded, dups$know_org_text4_coded, dups$know_org_text5_coded)

# Create a variable for valid responses excluding authorities
dups$know_org_noaut_valid_num <- 0
dups[grep("2", dups$know_org_1_5_coded, value = F), "know_org_noaut_valid_num"] <- 1
dups[grep("3", dups$know_org_1_5_coded, value = F), "know_org_noaut_valid_num"] <- 1
dups[grep("4", dups$know_org_1_5_coded, value = F), "know_org_noaut_valid_num"] <- 1
dups[grep("5", dups$know_org_1_5_coded, value = F), "know_org_noaut_valid_num"] <- 1

# Only ECWR as valid response
dups$know_org_nehad_num <- 0
dups[grep("1", dups$know_org_1_5_coded, value = F), "know_org_nehad_num"] <- 1
dups[grep("6", dups$know_org_1_5_coded, value = F), "know_org_nehad_num"] <- 1
dups[grep("7", dups$know_org_1_5_coded, value = F), "know_org_nehad_num"] <- 1

# Need to code future equal say and equal rights 
map_agree <- c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly agree" = 5)
dups$future_equal_say_num <- as.numeric(revalue(dups$future_equal_say, map_agree))
dups$future_equal_rights_num <- as.numeric(revalue(dups$future_equal_rights, map_agree))

# Need to code look online or contact an organization
like <- c("Very unlikely" = 1, "Unlikely" = 2 , "Neither likely nor unlikely" = 3, "Likely" = 4 ,"Very likely" = 5)
dups$look_online_num <- as.numeric(revalue(dups$look_online, like))
dups$contact_org_num <- as.numeric(revalue(dups$contact_org, like))

# Before and during COVID-19 look online or look for an organization
map_time <- c("Refuse" = 0, "Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Very often" = 5)
dups$dc19_look_online_num <- as.numeric(revalue(dups$dc19_look_online, map_time))
dups$dc19_look_org_num <- as.numeric(revalue(dups$dc19_look_org, map_time))
dups$bc19_look_online_num <- as.numeric(revalue(dups$bc19_look_online, map_time))
dups$bc19_look_org_num <- as.numeric(revalue(dups$bc19_look_org, map_time))

# Other variables
dups$bc19_yell_num <- as.numeric(revalue(dups$bc19_yell, map_time))
dups$bc19_hit_num <- as.numeric(revalue(dups$bc19_hit, map_time))
dups$dc19_yell_num <- as.numeric(revalue(dups$dc19_yell, map_time))
dups$dc19_hit_num <- as.numeric(revalue(dups$dc19_hit, map_time))
dups$talk_husband_num <- as.numeric(revalue(dups$talk_husband, like))
dups$talk_family_num <- as.numeric(revalue(dups$talk_family, like))
dups$husb_final_say_num <- as.numeric(revalue(dups$husb_final_say, map_agree))
dups$husb_provide_inc_num <- as.numeric(revalue(dups$husb_provide_inc, map_agree))
dups$husb_justified_yell_num <- as.numeric(revalue(dups$husb_justified_yell, map_agree))
dups$report_authorities_num <- as.numeric(revalue(dups$report_authorities, like))

# ------------------------------------------------------------------------------
# Endline Data manipulation ----------------------------------------------------
# ------------------------------------------------------------------------------
map_agree <- c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly agree" = 5)

# E41
dups$husb_final_say_end_num <- as.numeric(revalue(dups$husb_final_say_end, map_agree))

# E43
dups$husb_inc_end_num <- as.numeric(revalue(dups$husb_inc_end, map_agree))

# E44 
dups$husb_just_yell_end_num <- as.numeric(revalue(dups$husb_just_yell_end, map_agree))

# E46
dups$future_equal_say_end_num <- as.numeric(revalue(dups$future_equal_say_end, map_agree))

# E47
dups$future_equal_rights_end_num <- as.numeric(revalue(dups$future_equal_rights_end, map_agree))

# E48
dups$woman_work_outside_end_num <- as.numeric(revalue(dups$woman_work_outside_end, map_agree))

# E49
dups$colleague_comment_looks_end_num <- as.numeric(revalue(dups$colleague_comment_looks_end, map_agree))

# E50
dups$intervene_work_harass_end_num <- as.numeric(revalue(dups$intervene_work_harass_end, map_agree))

# E51
dups$women_clothes_harass_end_num <- as.numeric(revalue(dups$women_clothes_harass_end, map_agree))

# E52
dups$intervene_hitting_street_num <- as.numeric(revalue(dups$intervene_hitting_street, map_agree))

# E53
dups$you_intervene_harass_end_num <- as.numeric(revalue(dups$you_intervene_harass_end, map_agree))

# E54
dups$parents_assault_auth_end_num <- as.numeric(revalue(dups$parents_assault_auth_end, map_agree))

# E55
dups$child_relative_assault_end_num <- as.numeric(revalue(dups$child_relative_assault_end, map_agree))

# E56 
dups$female_circ_marriage_end_num <- as.numeric(revalue(dups$female_circ_marriage_end, map_agree))

# TRUE or FALSE questions
# We coded and indicator for each question according to whether they answered correctly 
# omitted answers "NA" are considered incorrect responses

# E57: right answer = true
dups <- mutate(dups, verbal_harassment_legal_end_num = ifelse(dups$verbal_harassment_legal_end == "True",1,0))
dups$verbal_harassment_legal_end_num <- replace_na(dups$verbal_harassment_legal_end_num, 0)

# E58: right answer = true
dups <- mutate(dups, khul_divorce_tf_end_num = ifelse(dups$khul_divorce_tf_end == "True",1,0))
dups$khul_divorce_tf_end_num <- replace_na(dups$khul_divorce_tf_end_num, 0)

# E59: right answer = false
dups <- mutate(dups, marriage_age_tf_end_num = ifelse(dups$marriage_age_tf_end == "False",1,0))
dups$marriage_age_tf_end_num <- replace_na(dups$marriage_age_tf_end_num, 0)

# E60: right answer = false
dups <- mutate(dups, circ_tf_end_num = ifelse(dups$circ_tf_end == "False",1,0))
dups$circ_tf_end_num <- replace_na(dups$circ_tf_end_num, 0)

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
length(which(dups$bcovid_yelled_end == "I refuse to answer")) # = 4
dups$bcovid_yelled_end_num <- as.numeric(revalue(dups$bcovid_yelled_end, map_time))

#62
length(which(dups$dcovid_yelled_end == "I refuse to answer")) # = 3
dups$dcovid_yelled_end_num <- as.numeric(revalue(dups$dcovid_yelled_end, map_time))

#63
length(which(dups$bcovid_hit_end == "I refuse to answer")) # = 3
dups$bcovid_hit_end_num <- as.numeric(revalue(dups$bcovid_hit_end, map_time))

#64
length(which(dups$dcovid_hit_end == "I refuse to answer")) # = 4
dups$dcovid_hit_end_num <- as.numeric(revalue(dups$dcovid_hit_end, map_time))

#65
length(which(dups$bcovid_assault_end == "I refuse to answer")) # = 6
dups$bcovid_assault_end_num <- as.numeric(revalue(dups$bcovid_assault_end, map_time))

#66
length(which(dups$dcovid_assault_end == "I refuse to answer")) # = 4
dups$dcovid_assault_end_num <- as.numeric(revalue(dups$dcovid_assault_end, map_time))

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
dups$dv_talkhusb_end_num <- as.numeric(revalue(dups$dv_talkhusb_end, like))

# E68
dups$dv_talktfam_end_num <- as.numeric(revalue(dups$dv_talktfam_end, like))

# E69
dups$dv_report_end_num <- as.numeric(revalue(dups$dv_report_end, like))

# E70
dups$dv_onlineres_end_num <- as.numeric(revalue(dups$dv_onlineres_end, like))

# E71 
dups$dv_contactorg_end_num <- as.numeric(revalue(dups$dv_contactorg_end, like))

# E72
dups$sa_talkfam_end_num <- as.numeric(revalue(dups$sa_talkfam_end, like))

# E73
dups$sa_report_end_num <- as.numeric(revalue(dups$sa_report_end, like))

# E74
dups$sa_onlineres_end_num <- as.numeric(revalue(dups$sa_onlineres_end, like))

# E75
dups$sa_contactorg_end_num <- as.numeric(revalue(dups$sa_contactorg_end, like))

# 73 - 83
dups$online_dvsa_1_5_end <- mapply(paste, sep = ",", dups$online_dvsa_1_end_coded, dups$online_dvsa_2_end_coded,
                                  dups$online_dvsa_3_end_coded, dups$online_dvsa_4_end_coded, dups$online_dvsa_5_end_coded)

# Create a variable for valid responses excluding authorities
dups$online_dvsa_noaut_end_num <- 0
dups[grep("2", dups$online_dvsa_1_5_end, value = F), "online_dvsa_noaut_end_num"] <- 1
dups[grep("3", dups$online_dvsa_1_5_end, value = F), "online_dvsa_noaut_end_num"] <- 1
dups[grep("4", dups$online_dvsa_1_5_end, value = F), "online_dvsa_noaut_end_num"] <- 1
dups[grep("5", dups$online_dvsa_1_5_end, value = F), "online_dvsa_noaut_end_num"] <- 1

# Only ECWR as valid response
dups$online_dvsa_nehad_end_num <- 0
dups[grep("1", dups$online_dvsa_1_5_end, value = F), "online_dvsa_nehad_end_num"] <- 1
dups[grep("6", dups$online_dvsa_1_5_end, value = F), "online_dvsa_nehad_end_num"] <- 1
dups[grep("7", dups$online_dvsa_1_5_end, value = F), "online_dvsa_nehad_end_num"] <- 1

# Know organization DVSA
dups$org_dvsa_1_5_end <- mapply(paste, sep = ",", dups$org_dvsa_1_end_coded, dups$org_dvsa_2_end_coded,
                                   dups$org_dvsa_3_end_coded, dups$org_dvsa_4_end_coded, dups$org_dvsa_5_end_coded)

# Create a variable for valid responses excluding authorities
dups$org_dvsa_noaut_end_num <- 0
dups[grep("2", dups$org_dvsa_1_5_end, value = F), "org_dvsa_noaut_end_num"] <- 1
dups[grep("3", dups$org_dvsa_1_5_end, value = F), "org_dvsa_noaut_end_num"] <- 1
dups[grep("4", dups$org_dvsa_1_5_end, value = F), "org_dvsa_noaut_end_num"] <- 1
dups[grep("5", dups$org_dvsa_1_5_end, value = F), "org_dvsa_noaut_end_num"] <- 1

# Only ECWR as valid response
dups$org_dvsa_nehad_end_num <- 0
dups[grep("1", dups$org_dvsa_1_5_end, value = F), "org_dvsa_nehad_end_num"] <- 1
dups[grep("6", dups$org_dvsa_1_5_end, value = F), "org_dvsa_nehad_end_num"] <- 1
dups[grep("7", dups$org_dvsa_1_5_end, value = F), "org_dvsa_nehad_end_num"] <- 1

## ---- Revealed Preference ---- 
# Continuous variable
map_donation <- c("No, I would not like to donate any money, and thus prefer to receive 25 EGP mobile credit for participating in this survey." 
                  = 0,
                  "Yes, I would like to donate 10 EGP, and thus receive only 15 EGP mobile credit for participating in this survey." = 10, 
                  "Yes, I would like to donate 15 EGP, and thus receive only 10 EGP mobile credit for participating in this survey." = 15,
                  "Yes, I would like to donate 25 EGP and thus receive no mobile credit for participating in this survey." = 25)

dups$donation__end_cont <- as.numeric(revalue(dups$donation_end, map_donation))

# Dummy to whether they donated any amount
dups <- mutate(dups, donation_end_num = ifelse(
  dups$donation_end == "No, I would not like to donate any money, and thus prefer to receive 25 EGP mobile credit for participating in this survey.",0,1))

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
dups <- mutate(dups, tv_morning_num_end = ifelse(dups$tv_morn_end == "Yes",1,0))
dups <- mutate(dups, tv_afternoon_num_end = ifelse(dups$tv_aft_end == "Yes",1,0))
dups <- mutate(dups, tv_evening_num_end = ifelse(dups$tv_eve_end == "Yes",1,0))

# E19: TV Channels
# Create a txt file to translate with the Arabic responses: "Other, please specify:"
dups$three_viewed_channel_end_num <- 0

# if "Al-Kahera Wal Nas" is one of the top 3 shows watched, then tv_top3_chan_num = 1
dups[grep("Al-Kahera Wal Nas", dups$three_viewed_channel_end, value = F), "three_viewed_channel_end_num"] <- 1

# Code other responses 
channel_end <- cbind(as.integer(which(dups$three_viewed_channel_end == "Other, please specify:")),
                     dups$three_viewed_channel_text_end[which(dups$three_viewed_channel_end == "Other, please specify:")])

# None of the other responses corresponds to Al-Kahera Wal Nas (the 9 obs are already 0)

# Initialize new dummy column as 0's
dups$three_viewed_shows_end_num <- 0

# If the type "Legal awareness" or "Family shows" is found then, tv_top3_shows_num = 1
dups[grep('Legal awareness|Family shows', dups$three_viewed_shows_end, value = F), 
       "three_viewed_shows_end_num"] <- 1

# E21: TV Sat Show 
# Create a txt file to translate with the Arabic responses: "Show:"
sat_2mos_end <- cbind(as.integer(which(dups$tv_sat_2mos_end == "Show:")),
                      dups$tv_sat_2mos_text_end[which(dups$tv_sat_2mos_end == "Show:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(sat_2mos_end, file = 'Datasets/Treatment for other endline/Dups/sat_2mos_end.txt', fileEncoding = "UTF-8")

# Read translated document
sat_2mos_end_translated <- read.xlsx("Datasets/Treatment for other endline/Dups/sat_2mos_end_translated.xlsx", 
                                     "sat_2mos_end", colIndex = c(1,3))

dups$tv_sat_2mos_end_num <- 0
dups$tv_sat_2mos_end_num <- replace(dups$tv_sat_2mos_end_num , list = sat_2mos_end_translated$index, 
                                      sat_2mos_end_translated$nehads_show)
# Cross Tab for sat shows 
xtabs(~ dups$tv_sat_2mos_end + tv_sat_2mos_end_num, data = dups, addNA =TRUE) # only 12 answers = 1

# E22: Watch Hekayat 
dups <- mutate(dups, watch_hekayat_end_num = ifelse(dups$watch_hekayat_end == "Yes",1,0))

dups$watch_hekayat_end_num <- replace_na(dups$watch_hekayat_end_num, 0)

# cross tabulation
mytable <- xtabs(~watch_hekayat_end+watch_hekayat_end_num, data=dups, addNA = TRUE)
ftable(mytable)

# E23: Heard of Hekayat
unique(dups$heard_hekayat_end)

dups <- mutate(dups, heard_hekayat_end_num = ifelse(dups$heard_hekayat_end == "Yes",1,0))
dups$heard_hekayat_end_num[which(dups$watch_hekayat_end == "Yes")] <- 1
dups$heard_hekayat_end_num <- replace_na(dups$heard_hekayat_end_num, 0)

# cross tabulation
mytable <- xtabs(~heard_hekayat_end+heard_hekayat_end_num, data=dups, addNA = TRUE)
ftable(mytable)

# E24: How Heard of Hekayat 

# heard of it via WhatsApp
dups$how_heard_hekayat_num <- 0
dups[grep('I have heard of it via WhatsApp', dups$how_heard_hekayat, value = F), 
       "how_heard_hekayat_num"] <- 1

# heard of it via social media (Facebook)
dups$how_heard_hekayat_sm_num <- 0
dups[grep('Facebook', dups$how_heard_hekayat, value = F), 
       "how_heard_hekayat_sm_num"] <- 1

# E25: Reminder over WhatsApp to watch Hekayat Nehad 
dups <- mutate(dups, whatsapp_remind_2mos_end_num = ifelse(dups$whatsapp_remind_2mos_end == "Yes",1 ,0))

# We coded NA as 0, since they did not receive a remainder
dups$whatsapp_remind_2mos_end_num <- replace_na(dups$whatsapp_remind_2mos_end_num, 0)

# E26: How many episodes did you watch?
map_ep <- c("Between 1 and 3" = 1, "Between 4 and 6" = 2, "Between 7 and 9" = 3, 
            "I don't know" = 2, "I refuse to answer" = 0)

# NA = 0 since people did not watch the TV show
dups$episodes_hekayat_end_num <- as.numeric(revalue(dups$episodes_hekayat_end, map_ep))
dups$episodes_hekayat_end_num <- replace_na(dups$episodes_hekayat_end_num, 0)

# New variable: Whether Watched Episode
dups$wheter_episodes_hekayat_end_num <- ifelse(dups$episodes_hekayat_end_num > 0, 1, 0)

# E27: Describe the content of episodes watched
# Create a txt file to translate with the Arabic responses: "Description:"
episodes_content_end <- cbind(as.integer(which(dups$episode_content_end == "Description:")),
                              dups$episode_content_text_end[which(dups$episode_content_end == "Description:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(episodes_content_end, file = 'Datasets/Treatment for other endline/Dups/episodes_content_end.txt', fileEncoding = "UTF-8")

# Read the translated document
episodes_content_end_translated <- read.xlsx("Datasets/Treatment for other endline/Dups/episodes_content_end_translated.xlsx", 
                                             "episodes_content_end", colIndex = c(1,4,5))

# We don't consider vague responses
dups$episodes_content_end_num_vague <- 0
dups$episodes_content_end_num_vague <- replace(dups$episodes_content_end_num_vague, list = episodes_content_end_translated$index,
                                                 episodes_content_end_translated$correct_answer - episodes_content_end_translated$correct_answer_vague)

# E28: Describe most liked topics 
# Create a txt file to translate with the Arabic responses: "Topic:"
episodes_topics_end <- cbind(as.integer(which(dups$episode_content_like_end == "Topic:")),
                             dups$episode_content_like_text_end[which(dups$episode_content_like_end == "Topic:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(episodes_topics_end, file = 'Datasets/Treatment for other endline/Dups/episodes_topics_end.txt', fileEncoding = "UTF-8")

# Read the translated document
episodes_topics_end_translated <- read.xlsx("Datasets/Treatment for other endline/Dups/episodes_topics_end_translated.xlsx", 
                                            "episodes_topics_end", colIndex = c(1,3,4))

# No vague responses
dups$episode_content_like_end_num_vague <- 0
dups$episode_content_like_end_num_vague <- replace(dups$episode_content_like_end_num_vague, list = episodes_topics_end_translated$index,
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
dups$videos_socmed_2mos_end_num <- as.numeric(revalue(dups$videos_socmed_2mos_end, mapping_time))
dups$videos_socmed_2mos_end_num <- replace_na(dups$videos_socmed_2mos_end_num, 1)

# E34: Videos of Empowerment and Support WhatsApp 
dups$videos_whatsapp_2mos_end_num <- as.numeric(revalue(dups$videos_whatsapp_2mos_end, mapping_time))
dups$videos_whatsapp_2mos_end_num <- replace_na(dups$videos_whatsapp_2mos_end_num, 1)

# E35: Received Videos WA or FB 
dups$videos_whatsapp_receive_end_num <- ifelse(dups$videos_whatsapp_receive_end == "No" | 
                                                 dups$videos_whatsapp_receive_end == "I refuse to answer" |
                                                 dups$videos_whatsapp_receive_end == "I don't know",0,1)

# E36: Watched videos WA or FB 
# We considered NA responses as 0 since either the respondent did not receive the videos or did not watch them
dups$videos_watched_end_num <- ifelse(dups$videos_watched_end =="Yes", 1,0)
dups$videos_watched_end_num <- replace_na(dups$videos_watched_end_num, 0)

# E37: Number of videos watched 
# We consideed NA responses as 0, since If the respondents did not answer this questions is because they did not received the videos
number_map <- c("I refuse to answer" = 0, "Between 1 and 3" = 1, "Between 4 and 6" = 2, 
                "Between 7 and 9" = 3, "Between 10 and 13" = 4,
                "I don't know" = 2)

dups$videos_watched_no_end_num <- as.numeric(revalue(dups$videos_watched_no_end, number_map))
dups$videos_watched_no_end_num <- replace_na(dups$videos_watched_no_end_num, 0)

# E38: Content Watched
# Create a txt file to translate with the Arabic responses: "Description:"
length(which(dups$videos_watched_content_end == "Description")) # = 1260

videos_watched_content <- cbind(as.integer(which(dups$videos_watched_content_end == "Description")), 
                                dups$videos_watched_content_text_end[which(dups$videos_watched_content_end == "Description")])

colnames(videos_watched_content) <- c("index", "videos_watched_content")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(videos_watched_content, file = 'Datasets/Treatment for other endline/Dups/videos_watched_content.txt', fileEncoding = "UTF-8")

# Read the translated document
videos_watched_content_translated <- read.xlsx("Datasets/Treatment for other endline/Dups/videos_watched_content_translated.xlsx", 
                                               "videos_watched_content", colIndex = c(1,3,4))

# No vague responses
dups$videos_watched_content_end_num <- 0
dups$videos_watched_content_end_num_vague <- 0

dups$videos_watched_content_end_num <- replace(dups$videos_watched_content_end_num, list = videos_watched_content_translated$index,
                                                 videos_watched_content_translated$choice_answer_vid_content)

dups$videos_watched_content_end_num_vague <- replace(dups$videos_watched_content_end_num_vague, list = videos_watched_content_translated$index, 
                                                     videos_watched_content_translated$choice_answer_vid_content - videos_watched_content_translated$vague_answer_vid_content)

# E39: Most Liked Topics 
# Ix Topics accurate
# Create a txt file to translate with the Arabic responses: "Description:"
length(which(dups$video_content_like_end == "Description")) # = 66

videos_liked_topics <- cbind(as.integer(which(dups$video_content_like_end == "Description")), 
                             dups$video_content_like_end_text[which(dups$video_content_like_end == "Description")])

colnames(videos_liked_topics) <- c("index", "videos_liked_topics")

# Export bcovid_Activity as txt file with code UTF-8. need to import in excel as UTF-8 to read Arabic 
Sys.setlocale("LC_CTYPE", "arabic")
write.csv(videos_liked_topics, file = 'Datasets/Treatment for other endline/Dups/videos_liked_topics.txt', fileEncoding = "UTF-8")

# Read the translated document
videos_liked_topics_translated <- read.xlsx("Datasets/Treatment for other endline/Dups/videos_liked_topics_translated.xlsx", 
                                            "videos_liked_topics", colIndex = c(1,3,4))

# No vague responses
dups$video_content_like_end_num_vague <- 0
dups$video_content_like_end_num_vague <- replace(dups$video_content_like_end_num_vague, list = videos_liked_topics_translated$index,
                                                   videos_liked_topics_translated$choice_answer_vid_content - 
                                                     videos_liked_topics_translated$vague_answer_vid_content)

# Need to code future equal say and equal rights 
map_agree <- c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly agree" = 5)
dups$future_equal_say_end_num <- as.numeric(revalue(dups$future_equal_say_end, map_agree))
dups$future_equal_rights_end_num <- as.numeric(revalue(dups$future_equal_rights_end, map_agree))


# Before and during COVID-19 look online or look for an organization
map_time <- c("Refuse" = 0, "Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Very often" = 5)
dups$dcovid_accessonline_end_num <- as.numeric(revalue(dups$dcovid_accessonline_end, map_time))
dups$dcovid_contactorg_end_num <- as.numeric(revalue(dups$dcovid_contactorg_end, map_time))
dups$bcvovid_accessonline_end_num <- as.numeric(revalue(dups$bcvovid_accessonline_end, map_time))
dups$bcovid_contactorg_end_num <- as.numeric(revalue(dups$bcovid_contactorg_end, map_time))

dups$dcovid_accessonline_end_num[is.na(dups$dcovid_accessonline_end_num)] <- 1
dups$dcovid_contactorg_end_num[is.na(dups$dcovid_contactorg_end_num)] <- 1
dups$bcvovid_accessonline_end_num[is.na(dups$bcvovid_accessonline_end_num)] <- 1
dups$bcovid_contactorg_end_num[is.na(dups$bcovid_contactorg_end_num)] <- 1


c("dcovid_accessonline_end_num","dcovid_contactorg_end_num")
# Write data with only dups
write.csv(dups, file = 'Datasets/FinalData/Finaldata_Dups_Only.csv', row.names = F)


# ------------------------------------------------------------------------------
# Create Z-scores 
# ------------------------------------------------------------------------------
rm(list=setdiff(ls(), c(lsf.str(), "dups")))

# Read treated original dataset
original <- read.csv(file = 'Datasets/FinalData/Finaldata.csv', fileEncoding = "latin1")

# Change class of some variables for matching
cols.num <- c("Progress.y","Durationinseconds.y", "age_end")
dups[cols.num] <- sapply(dups[cols.num],as.numeric)

# Concatenate in a single final with duplicated osbervations
final_wDups <- bind_rows(original, dups)

# First stage for TV show 
FS_TV_data <- final_wDups[c("tv_evening_num_end", "three_viewed_channel_end_num",
                       "three_viewed_shows_end_num", "tv_sat_2mos_end_num", "watch_hekayat_end_num",
                       "heard_hekayat_end_num", "how_heard_hekayat_num",
                       "whatsapp_remind_2mos_end_num", "wheter_episodes_hekayat_end_num" , "episodes_hekayat_end_num",
                       "episodes_content_end_num_vague", "episode_content_like_end_num_vague")]
# Calculate the z-score
final_wDups$FS_TV_zscore <- scale(rowMeans(scale(FS_TV_data) %*% diag(c(1,1,1,1,1,1,1,1,1,1,1,1))))

# Baseline z-score
controls <- c("tv_evening_num", "tv_sattelite_num", "tv_top3_chan_num", "tv_top3_shows_num", "sat_show_num")
final_wDups$FS_TV_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1,1,1,1)))

#  First stage for Social Media ------------------------------------------------
FS_fw_data <- final_wDups[c("videos_socmed_2mos_end_num", "videos_whatsapp_2mos_end_num",
                       "videos_whatsapp_receive_end_num", "videos_watched_end_num",
                       "videos_watched_no_end_num", "videos_watched_content_end_num_vague", 
                       "video_content_like_end_num_vague")]

final_wDups$FS_FW_zscore <- scale(rowMeans(scale(FS_fw_data) %*% diag(c(1,1,1,1,1,1,1))))

# Baseline z-score
controls <- c("X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num")
final_wDups$FS_FW_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1)))

# Knowledge of online resources and organizations  -----------------------------
knowledge2 <- final_wDups[c("online_dvsa_noaut_end_num","online_dvsa_nehad_end_num", "org_dvsa_noaut_end_num", "org_dvsa_nehad_end_num")]
final_wDups$RF_knowledge2_zscore <- scale(rowMeans(scale(knowledge2) %*% diag(c(1, 1, 1, 1))))

# Baseline z-score
controls <- c("know_online_valid_noaut_num", "know_org_noaut_valid_num", "know_online_nehad_num", "know_org_nehad_num")
final_wDups$RF_knowledge2_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1,1,1)))

# Attitudes towards female empowerment, husband role, and women role in the workplace (domestic related) -----
husb_work_att <- final_wDups[c("husb_final_say_end_num","husb_inc_end_num", "husb_just_yell_end_num",
                          "woman_work_outside_end_num", "female_circ_marriage_end_num", "circ_tf_end_num",
                          "marriage_age_tf_end_num", "khul_divorce_tf_end_num")]

final_wDups$RF_att1_zscore <- scale(rowMeans(scale(husb_work_att) %*% diag(c(-1,-1,-1,1,-1,-1,-1,1))))

# Baseline z-score
controls <- c("husb_final_say_num", "husb_provide_inc_num", "husb_justified_yell_num")
final_wDups$RF_att1_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(-1,-1,-1)))

# Attitudes towards violence outside the household -----------------------------
att2 <- final_wDups[c("colleague_comment_looks_end_num", "verbal_harassment_legal_end_num", "intervene_work_harass_end_num", 
                 "women_clothes_harass_end_num","intervene_hitting_street_num", "you_intervene_harass_end_num", 
                 "parents_assault_auth_end_num", "child_relative_assault_end_num")]

final_wDups$RF_att2_zscore <- scale(rowMeans(scale(att2) %*% diag(c(1,1,1,-1,1,1,-1,1))))

# Baseline z-score (No lagged dvs)

# Revealed Preference ----------------------------------------------------------
donation <- final_wDups[c("donation__end_cont", "donation_end_num")]
final_wDups$RF_preference_zscore <- scale(rowMeans(scale(donation) %*% diag(c(1, 1))))

# No lagged DVs

# 4 - 5: DV look online for resources or org -----------------------------------
hyp_be <- final_wDups[c("dv_onlineres_end_num", "dv_contactorg_end_num")]
final_wDups$RF_hypDM2_zscore <- scale(rowMeans(scale(hyp_be) %*% diag(c(1, 1))))

# Baseline z-score
controls <- c("look_online_num", "contact_org_num")
final_wDups$RF_hypDM2_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1)))

# Variables contact or look organization/online resources ----------------------
hyp_be_2 <- final_wDups[c("sa_onlineres_end_num", "sa_contactorg_end_num")]
final_wDups$RF_hypSA2_zscore <- scale(rowMeans(scale(hyp_be_2) %*% diag(c(1, 1))))

# No lagged dvs

# During COVID-19 --------------------------------------------------------------
know_during <- final_wDups[c("dcovid_accessonline_end_num","dcovid_contactorg_end_num")]
final_wDups$RF_dcovid2_zscore <- scale(rowMeans(scale(know_during)%*% diag(c(1, 1))))

# Baseline z-score
controls <- c("dc19_look_online_num", "dc19_look_org_num")
final_wDups$RF_dcovid2_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1)))

final_wDups$dcovid_accessonline_end_num_refuse <- ifelse(final_wDups$dcovid_accessonline_end == "I refuse to answer", 1, 0)
final_wDups$dcovid_accessonline_end_num_refuse <- replace_na(final_wDups$dcovid_accessonline_end_num_refuse, 1)

final_wDups$dcovid_contactorg_end_num_refuse <- ifelse(final_wDups$dcovid_contactorg_end == "I refuse to answer", 1, 0)
final_wDups$dcovid_contactorg_end_num_refuse <- replace_na(final_wDups$dcovid_contactorg_end_num_refuse, 1)

# Future Outlook ---------------------------------------------------------------
fut_out <- final_wDups[c("future_equal_say_end_num","future_equal_rights_end_num")]
final_wDups$RF_fo_zscore <- scale(rowMeans(scale(fut_out) %*% diag(c(1,1))))

# Baseline z-score
controls <- c("future_equal_say_num", "future_equal_rights_num")
final_wDups$RF_fo_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1)))

# Reduced form for behavior and reporting during COVID-19 ----------------------
rep_during_data <- final_wDups[c("dcovid_yelled_end_num","dcovid_hit_end_num","dcovid_assault_end_num")]
final_wDups$RF_dcovid_zscore <- scale(rowMeans(scale(rep_during_data) %*% diag(c(1, 1, 1))))

# Baseline z-score
controls <- c("dc19_yell_num", "dc19_hit_num")
final_wDups$RF_dcovid_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1)))

# Behavior and Reporting (part of Observed Behavior) ---------------------------
# BEFORE COVID -----------------------------------------------------------------
rep_before_data <- final_wDups[c("bcovid_yelled_end_num", "bcovid_hit_end_num", "bcovid_assault_end_num")]
final_wDups$RT_bcovid_zscore <- scale(rowMeans(scale(rep_before_data) %*% diag(c(1, 1, 1))))

# Baseline z-score
controls <- c("bc19_yell_num", "bc19_hit_num")
final_wDups$RT_bcovid_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1)))

final_wDups$bcovid_yelled_end_num_refuse <- ifelse(final_wDups$bcovid_yelled_end == "I refuse to answer", 1, 0)
final_wDups$bcovid_hit_end_num_refuse <- ifelse(final_wDups$bcovid_hit_end == "I refuse to answer", 1, 0)
final_wDups$bcovid_assault_end_num_refuse <- ifelse(final_wDups$bcovid_assault_end == "I refuse to answer", 1, 0)

# Hypothetical Behavior -----------------------------------------------------------
# Domestic Violence, Sexual Assault, or Harassment
# Reduced form for hypothetical behavior around domestic violence (DV) variables,part 2
hyp_be <- final_wDups[c("dv_talkhusb_end_num", "dv_talktfam_end_num", "dv_report_end_num")]
final_wDups$RF_hypDM1_zscore <- scale(rowMeans(scale(hyp_be) %*% diag(c(1, 1, 1))))

# Baseline z-score
controls <- c("talk_husband_num", "talk_family_num", "report_authorities_num")
final_wDups$RF_hypDM1_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1,1)))

# Part 2: Sexual Assault or Harassment -----------------------------------------
hyp_be_2 <- final_wDups[c("sa_talkfam_end_num", "sa_report_end_num")]
final_wDups$RF_hypSA1_zscore <- scale(rowMeans(scale(hyp_be_2) %*% diag(c(1, 1))))

# Knowledge, Behavior, and Reporting -------------------------------------------
# Before COVID-19
know_before <- final_wDups[c("bcvovid_accessonline_end_num", "bcovid_contactorg_end_num")]
final_wDups$RT_bcovidAccess_zscore <- scale(rowMeans(scale(know_before) %*% diag(c(1, 1))))

# Baseline z-score
controls <- c("bc19_look_online_num", "bc19_look_org_num")
final_wDups$RT_bcovidAccess_zscore_base <- scale(rowMeans(scale(final_wDups[controls]) %*% c(1,1)))

# Refuse covariates

# Yell - hit - assault
final_wDups$dcovid_yelled_end_num_refuse <- ifelse(final_wDups$dcovid_yelled_end == "I refuse to answer", 1, 0)
final_wDups$dcovid_hit_end_num_refuse <- ifelse(final_wDups$dcovid_hit_end == "I refuse to answer", 1, 0)
final_wDups$dcovid_assault_end_num_refuse <- ifelse(final_wDups$dcovid_assault_end == "I refuse to answer", 1, 0)

final_wDups$dcovid_yelled_end_num_refuse <- replace_na(final_wDups$dcovid_yelled_end_num_refuse, 1)
final_wDups$dcovid_hit_end_num_refuse <- replace_na(final_wDups$dcovid_hit_end_num_refuse, 1)
final_wDups$dcovid_assault_end_num_refuse <- replace_na(final_wDups$dcovid_assault_end_num_refuse, 1)


# During COVID-19
final_wDups$dcovid_accessonline_end_num_refuse <- ifelse(final_wDups$dcovid_accessonline_end == "I refuse to answer", 1, 0)
final_wDups$dcovid_accessonline_end_num_refuse <- replace_na(final_wDups$dcovid_accessonline_end_num_refuse, 1)

final_wDups$dcovid_contactorg_end_num_refuse <- ifelse(final_wDups$dcovid_contactorg_end == "I refuse to answer", 1, 0)
final_wDups$dcovid_contactorg_end_num_refuse <- replace_na(final_wDups$dcovid_contactorg_end_num_refuse, 1)

# Before COVID-19
final_wDups$bcvovid_accessonline_end_num_refuse <- ifelse(final_wDups$bcvovid_accessonline_end == "I refuse to answer", 1, 0)
final_wDups$bcvovid_accessonline_end_num_refuse <- replace_na(final_wDups$bcvovid_accessonline_end_num_refuse, 1)

final_wDups$bcovid_contactorg_end_num_refuse <- ifelse(final_wDups$bcovid_contactorg_end == "I refuse to answer", 1, 0)
final_wDups$bcovid_contactorg_end_num_refuse <- replace_na(final_wDups$bcovid_contactorg_end_num_refuse, 1)

#-------------------------------------------------------------------------------
# Save dataset for Analysis
# ------------------------------------------------------------------------------
# Save the new data with baseline, endline, and z-scores variables coded as our principal dataset
write.csv(final_wDups, file = 'Datasets/FinalData/Finaldata_wfinal_wDups.csv', row.names = F)
