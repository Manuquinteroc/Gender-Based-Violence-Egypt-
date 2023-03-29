# Egypt Analysis Replication File: BaselineVariables
# This Script codes the remaining baseline variables used in our Analysis.
# 06/06/2021

## ---- set working directory ---- 

## ----  Read the merged data with baseline and endline responses
merged <- read_excel("Datasets/InitialData/merged_baseline_endline_030921.xlsx")

# Create a pooled variable for Facebook and WhatsApp Individual
merged$pooledF_WI <- 0
merged$pooledF_WI[which(merged$facebook_10018 == 1)] <- 1
merged$pooledF_WI[which(merged$whatsapp_10018 == 1)] <- 1

# Create a pooled variable for Facebook, WhatsApp Individual, and WhatsApp Group
merged$pooledF_WIG <- 0
merged$pooledF_WIG[which(merged$facebook_10018 == 1)] <- 1
merged$pooledF_WIG[which(merged$whatsapp_10018 == 1)] <- 1
merged$pooledF_WIG[which(merged$in_group_10018 == 1)] <- 1

# Create a pooled variable for all treatmetns indicators
merged$pooledT <- 0
merged$pooledT[which(merged$control_10018 != 1)] <- 1

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
merged$male_children <- replace_na(merged$male_children, 0)
merged$female_children <- replace_na(merged$female_children, 0)

# B9: Adding up family members other than yourself, children, and husb
merged$sum_family_mem <- rowSums(sapply(merged[, c(28:39)],
                                        function(x) as.numeric(as.character(x))), na.rm = TRUE)

ind1 <- which(merged$married != 1 )

# B11: Husband education
merged <- mutate(merged, education_husb_num =  
                   ifelse(education_husb == "BA" | education_husb == "MA and above", 1, 0),
                 education_husb_num = replace_na(education_husb_num, 0))

merged$education_husb_num[ind1] <- NA

# B12: Before COVID-19 main act 
# Create a txt file to translate with the Arabic responses: "Other"
bcovid_activity <- cbind(as.integer(which(merged$bcovid_activity == "Other")), 
                         merged$bcovid_act_mar_text[which(merged$bcovid_activity == "Other")],
                         merged$bcovid_mainact_unm_text[which(merged$bcovid_activity == "Other")]
)

colnames(bcovid_activity) <- c("index", "answer_mar", "answer_unm")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(bcovid_activity, file = 'Datasets/Treatment for other baseline/bcovid_activity.txt', fileEncoding = "UTF-8")

# Read the translated document
bcovid_activity_trans <- read.xlsx("Datasets/Treatment for other baseline/bcovid_activity_translated.xlsx", "bcovid_activity", colIndex = c(1,5))

merged$bcovid_activity <- replace(merged$bcovid_activity, list = bcovid_activity_trans$index, 
                                  bcovid_activity_trans$Choice.Answer)

ind <- bcovid_activity_trans$index[is.na(bcovid_activity_trans$Choice.Answer)]

# Variable for full time at home
merged <- mutate(merged, bcovid_activity_num =  
                   ifelse(bcovid_activity == "Work from inside of the household (not including housework or taking care of family)" | 
                            bcovid_activity == "Work inside the house doing housework or taking care of family" |
                            bcovid_activity =="Not working at home", 1, 0),
                 bcovid_activity_num = replace_na(bcovid_activity_num, 0))

# Number of missing values
merged$bcovid_activity_num[ind] <- NA
length(which(is.na(merged$bcovid_activity_num))) # = 4 

# Partially at home variable
merged <- mutate(merged, bcovid_activity_ph_num =  
                   ifelse(bcovid_activity == "Study" | 
                            bcovid_activity == "Work partly outside the household and partly inside the house", 1, 0),
                 bcovid_activity_ph_num = replace_na(bcovid_activity_ph_num, 0))

# Number of missing values
merged$bcovid_activity_ph_num[ind] <- NA
length(which(is.na(merged$bcovid_activity_ph_num))) # = 4 

# B13: Before COVID husband main activity
# Create a txt file to translate with the Arabic responses: "Other"
bcovid_activity_husb <- cbind(as.integer(which(merged$bcovid_act_husbmar == "Other")),
                              merged$bcovid_act_husbmar_text[which(merged$bcovid_act_husbmar == "Other")])

colnames(bcovid_activity_husb) <- c("index", "answer")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(bcovid_activity_husb, file = 'Datasets/Treatment for other baseline/bcovid_activity_husb.txt', fileEncoding = "UTF-8")

bcovid_activity_husb_trans <- read.xlsx("Datasets/Treatment for other baseline/bcovid_activity_husb_translated.xlsx", "bcovid_activity_husb", colIndex = c(1,4))

merged$bcovid_act_husbmar <- replace(merged$bcovid_act_husbmar, list = bcovid_activity_husb_trans$index, 
                                     bcovid_activity_husb_trans$Choice.Answer)

ind <- bcovid_activity_husb_trans$index[is.na(bcovid_activity_husb_trans$Choice.Answer)]

# Double check we deleted all 'Other' answers 
ifelse("Other" %in% unique(merged$bcovid_act_husbmar), TRUE, FALSE)

merged <- mutate(merged, bcovid_act_husbmar_num =  
                   ifelse(bcovid_act_husbmar == "Work from inside of the household (not including housework or taking care of family)" | 
                            bcovid_act_husbmar == "Work inside the house doing housework or taking care of family" |
                            bcovid_act_husbmar =="Not working at home", 1, 0),
                 bcovid_act_husbmar_num = replace_na(bcovid_act_husbmar_num, 0))

# Number of missing values
merged$bcovid_act_husbmar_num[ind1] <- NA
merged$bcovid_act_husbmar_num[ind] <- NA

length(which(!is.na(merged$bcovid_act_husbmar_num))) # = 3039 out of 3042 married women

# Partially at home variable
merged <- mutate(merged, bcovid_activity_husbmar_ph_num =  
                   ifelse(bcovid_act_husbmar == "Study" | 
                            bcovid_act_husbmar == "Work partly outside the household and partly inside the house", 1, 0),
                 bcovid_activity_husbmar_ph_num = replace_na(bcovid_activity_husbmar_ph_num, 0))

# Number of missing values
merged$bcovid_activity_husbmar_ph_num[ind1] <- NA
merged$bcovid_activity_husbmar_ph_num[ind] <- NA

length(which(!is.na(merged$bcovid_activity_husbmar_ph_num))) # = 3039 out of 3042 married women

# B14: dc main act 
# Create a txt file to translate with the Arabic responses: "Other"
dcovid_activity <- cbind(as.integer(which(merged$dcovid_activity == "Other")), 
                         merged$dcovid_mainact_mar_text[which(merged$dcovid_activity == "Other")],
                         merged$dcovid_mainact_unm_text[which(merged$dcovid_activity == "Other")]
)
colnames(dcovid_activity) <- c("index", "answer_mar", "answer_unm")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(dcovid_activity, file = 'Datasets/Treatment for other baseline/dcovid_activity.txt', fileEncoding = "UTF-8")

# Read the translated document
dcovid_activity_trans <- read.xlsx("Datasets/Treatment for other baseline/dcovid_activity_translated.xlsx", "dcovid_activity", colIndex = c(1,5))

merged$dcovid_activity <- replace(merged$dcovid_activity, list = dcovid_activity_trans$index, 
                                  dcovid_activity_trans$Choice.Answer)

ind <- dcovid_activity_trans$index[is.na(dcovid_activity_trans$Choice.Answer)]

merged <- mutate(merged, dcovid_activity_num =  
                   ifelse(dcovid_activity == "Work from inside of the household (not including housework or taking care of family)" | 
                            dcovid_activity == "Work inside the house doing housework or taking care of family" |
                            dcovid_activity =="Not working at home", 1, 0),
                 dcovid_activity_num = replace_na(dcovid_activity_num, 0))

merged$dcovid_activity_num <- replace_na(merged$dcovid_activity_num, 0)

# Number of missing values
merged$dcovid_activity_num[ind] <- NA

length(which(is.na(merged$dcovid_activity_num))) # = 10
merged$dcovid_activity_num <- replace_na(merged$dcovid_activity_num, 0)

# Partially at home variable
merged <- mutate(merged, dcovid_activity_ph_num =  
                   ifelse(dcovid_activity == "Study" | 
                            dcovid_activity == "Work partly outside the household and partly inside the house", 1, 0),
                 dcovid_activity_ph_num = replace_na(dcovid_activity_ph_num, 0))

merged$dcovid_activity_ph_num[ind] <- NA
length(which(is.na(merged$dcovid_activity_ph_num))) # = 10

# B15 - dc husb main act 
# Create a txt file to translate with the Arabic responses: "Other"
dcovid_activity_husb <- cbind(as.integer(which(merged$dcovid_mainact_husbmar == "Other")),
                              merged$dcovid_mainact_husbmar_text[which(merged$dcovid_mainact_husbmar == "Other")])

colnames(dcovid_activity_husb) <- c("index", "answer")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(dcovid_activity_husb, file = 'Datasets/Treatment for other baseline/dcovid_activity_husb.txt', fileEncoding = "UTF-8")

# read the translated document
dcovid_activity_husb_trans <- read.xlsx("Datasets/Treatment for other baseline/dcovid_activity_husb_translated.xlsx", "dcovid_activity_husb", colIndex = c(1,4))

ind <- dcovid_activity_husb_trans$index[is.na(dcovid_activity_husb_trans$Choice.Answer)]

# Create a new variable for main activity of the husband
merged$dcovid_activity_husb <- replace(merged$dcovid_mainact_husbmar, list = dcovid_activity_husb_trans$index, 
                                       dcovid_activity_husb_trans$Choice.Answer)

merged <- mutate(merged, dcovid_activity_husb_num =  
                   ifelse(dcovid_activity_husb == "Work from inside of the household (not including housework or taking care of family)" | 
                            dcovid_activity_husb == "Work inside the house doing housework or taking care of family" |
                            dcovid_activity_husb =="Not working at home", 1, 0),
                 dcovid_activity_husb_num = replace_na(dcovid_activity_husb_num, 0))

merged$dcovid_activity_husb_num[ind] <- NA
merged$dcovid_activity_husb_num[ind1] <- NA
length(which(!is.na(merged$dcovid_activity_husb_num))) # = 3039 out of 3042 married women

# Partially at home variable
merged <- mutate(merged, dcovid_activity_husbmar_ph_num =  
                   ifelse(dcovid_activity_husb == "Study" | 
                            dcovid_activity_husb == "Work partly outside the household and partly inside the house", 1, 0),
                 dcovid_activity_husbmar_ph_num = replace_na(dcovid_activity_husbmar_ph_num, 0))

merged$dcovid_activity_husbmar_ph_num[ind] <- NA
merged$dcovid_activity_husbmar_ph_num[ind1] <- NA
length(which(!is.na(merged$dcovid_activity_husbmar_ph_num))) # = 3039 out of 3042 married women

## ---- First stage for TV show ----
# Variable Names
# B17 - TV Time
# B18 - TV Option
# B19 - TV Channels
# B20 - TV Shows
# B21 - TV Sat Show 

# B17: TV Time 
# Indicator an indicator for each: Watching TV morning, afternoon, and evening
merged <- mutate(merged, tv_morning_num = ifelse(merged$tv_morning == "Yes",1,0))
merged <- mutate(merged, tv_afternoon_num = ifelse(merged$tv_afternoon == "Yes",1,0))
merged <- mutate(merged, tv_evening_num = ifelse(merged$tv_evening == "Yes",1,0))

# B18 - TV Option 
merged <- mutate(merged, tv_sattelite_num = ifelse(merged$tv_sattelite == "Yes",1,0))

# B19: TV Channels 
# Initialize all values of the numeric variable in 0
merged$tv_top3_chan_num <- 0

# if "Al-Kahera Wal Nas" is one of the top 3 shows watched, then tv_top3_chan_num = 1
merged[grep("Al-Kahera Wal Nas", merged$tv_top3_chan, value = F), "tv_top3_chan_num"] <- 1

# B20: TV Shows 
# Initialize all values of the numeric variable in 0
merged$tv_top3_shows_num <- 0

# If the type "Legal awareness" or "Family shows" is found then, tv_top3_shows_num = 1
merged[grep('Legal awareness|Family shows', merged$tv_top3_shows, value = F), 
       "tv_top3_shows_num"] <- 1

# B21: TV Sat Show  
# Create a txt file to translate with the Arabic responses: "Show:"
sat_show <- cbind(as.integer(which(merged$sat_evening == "Show:")),
                  merged$sat_evening_text[which(merged$sat_evening == "Show:")])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(sat_show, file = 'Datasets/Treatment for other baseline/sat_show.txt', fileEncoding = "UTF-8")

# Read the translated document
sat_show_Translated <- read.xlsx("Datasets/Treatment for other baseline/sat_show_translated.xlsx", "sat_show", colIndex = c(1,3))

merged$sat_show_num <- 0
merged$sat_show_num <- replace(merged$sat_show_num , list = sat_show_Translated$index, 
                               sat_show_Translated$hekayat_nehad)

## Social Media Habits
# B30 - Hours spent on social media
hours_map <- c("None" = 0, "Up to 2 hours" = 1, "Up to 5 hours" = 2, "Up to 10 hours" = 3,  "10 hours or more" = 4)

merged$hours_soc_med_num <- as.numeric(revalue(merged$hours_soc_med, hours_map))

# Create a txt file to translate with the Arabic responses: "Other"
soc_med_use <- cbind(grep('Other', merged$soc_med_use), 
                     merged$soc_med_use_text[grep('Other', merged$soc_med_use)])

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(soc_med_use, file = 'Datasets/Treatment for other baseline/soc_med_use.txt', fileEncoding = "UTF-8")

# read the translated document
sat_show_Translated <- read.xlsx("Datasets/Treatment for other baseline/soc_med_use_translated.xlsx", "soc_med_use", colIndex = c(1,4))

aux <- 1
for (i in sat_show_Translated$index) {
  merged$soc_med_use[i] <- paste(merged$soc_med_use[i] , sat_show_Translated$Choice.Answer[aux], sep=",")
  aux <- aux + 1 
}

# Indicator if WhatsApp is mentioned
merged$socmed_whatsapp <- 0
merged[grep('WhatsApp', merged$soc_med_use, value = F), 
       "socmed_whatsapp"] <- 1

# Indicator if Facebook is mentioned
merged$socmed_Facebook <- 0
merged[grep('Facebook', merged$soc_med_use, value = F), 
       "socmed_Facebook"] <- 1

# Indicator if Instagram is mentioned
merged$socmed_Instagram <- 0
merged[grep('Instagram', merged$soc_med_use, value = F), 
       "socmed_Instagram"] <- 1

# Indicator if YouTube is mentioned
merged$socmed_YouTube <- 0
merged[grep('YouTube', merged$soc_med_use, value = F), 
       "socmed_YouTube"] <- 1

# Indicator if Twitter is mentioned
merged$socmed_Twitter <- 0
merged[grep('Twitter', merged$soc_med_use, value = F), 
       "socmed_Twitter"] <- 1

# Indicator if Snapchat is mentioned
merged$socmed_Snapchat <- 0
merged[grep('Snapchat', merged$soc_med_use, value = F), 
       "socmed_Snapchat"] <- 1

# Indicator if Telegram is mentioned
merged$socmed_Telegram <- 0
merged[grep('Telegram', merged$soc_med_use, value = F), 
       "socmed_Telegram"] <- 1

## ---- First stage for Facebook and WhatsApp treatment ----
# Variable names
# B33 - violence socmed 
# B34 - violence whatsApp

# B33: violence socmed 
mapping_time1 <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Seen frequently" = 5)
merged$'X2mos_socmed_dv_num'<- as.numeric(revalue(merged$'2mos_socmed_dv', mapping_time1))

# B34: violence whatsApp 
mapping_time2 <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3,
                   "Often" = 4, "Received many" = 5)

merged$'X2mos_whatsapp_dv_num' <- as.numeric(revalue(merged$'2mos_whatsapp_dv', mapping_time2))

# Filter to only that responded the endline, just for simplicity. We don't use the other responses.
merged <- filter(merged, responded == 1)

## Knowledge of and Experience of Accessing Resources
# B-78: Know Online
length(which(merged$know_online == "I do not know any.")) # = 2917

# Create a txt file to translate with the Arabic responses
index <- which(merged$know_online != "I do not know any.")
know_online_1_5 <-cbind(as.integer(index), merged$know_online[index],
                        merged$know_online_text1[index], merged$know_online_text2[index], merged$know_online_text3[index], 
                        merged$know_online_text4[index], merged$know_online_text5[index])

colnames(know_online_1_5) <- c("index", "know_online", "know_online1", "know_online2", "know_online3", "know_online4", "know_online5")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(know_online_1_5, file = 'Datasets/Treatment for other baseline/know_online_text1_5.txt', fileEncoding = "UTF-8")

# Read translated document
know_online_1_5_translated <- read.xlsx("Datasets/Treatment for other baseline/know_online1_5_translated_0419.xlsx", 
                                        "know_online_text1_5", colIndex = c(1,3,4,5,6,7))


know_online_1_5_translated$pasted_answer <- mapply(paste, sep = ",", know_online_1_5_translated$know_online1, know_online_1_5_translated$know_online2, 
                                                   know_online_1_5_translated$know_online3, know_online_1_5_translated$know_online4,
                                                   know_online_1_5_translated$know_online5)

merged$aux <- 0
merged$aux <- replace(merged$aux , list = know_online_1_5_translated$index, 
                      know_online_1_5_translated$pasted_answer)

# Create a variable for valid responses excluding authorities
merged$know_online_valid_noaut_num <- 0
merged[grep("organization", merged$aux, value = F), "know_online_valid_noaut_num"] <- 1
merged[grep("social_media", merged$aux, value = F), "know_online_valid_noaut_num"] <- 1
merged[grep("media", merged$aux, value = F), "know_online_valid_noaut_num"] <- 1
merged[grep("internet", merged$aux, value = F), "know_online_valid_noaut_num"] <- 1

# Only ECWR as valid response
merged$know_online_nehad_num <- 0
merged[grep("ecwr_nehad", merged$aux, value = F), "know_online_nehad_num"] <- 1

# B81  
# B-81: Know Organization
length(which(merged$know_org == "I do not know any.")) # = 3038

# Create a txt file to translate with the Arabic responses
index <- which(merged$know_org != "I do not know any.")
know_org_1_5 <-cbind(as.integer(index), merged$know_org[index],
                     merged$know_org_text1[index], merged$know_org_text2[index], merged$know_org_text3[index], 
                     merged$know_org_text4[index], merged$know_org_text5[index])

colnames(know_org_1_5) <- c("index", "know_org", "know_org1", "know_org2", "know_org3", "know_org4", "know_org5")

Sys.setlocale("LC_CTYPE", "arabic")
write.csv(know_org_1_5, file = 'Datasets/Treatment for other baseline/know_org_1_5.txt', fileEncoding = "UTF-8")

# Read translated document
know_org_1_5_translated <- read.xlsx("Datasets/Treatment for other baseline/know_org1_5_translation_0419.xlsx", 
                                     "know_org_1_5", colIndex = c(1,3,4,5,6,7))

know_org_1_5_translated$pasted_answer <- mapply(paste, sep = ",", know_org_1_5_translated$know_org1, know_org_1_5_translated$know_org2, 
                                                know_org_1_5_translated$know_org3, know_org_1_5_translated$know_org4,
                                                know_org_1_5_translated$know_org5)

merged$aux <- 0
merged$aux <- replace(merged$aux , list = know_org_1_5_translated$index, 
                      know_org_1_5_translated$pasted_answer)

# Create a variable for valid responses excluding authorities and ECWR
merged$know_org_noaut_valid_num <- 0
merged[grep("organization", merged$aux, value = F), "know_org_noaut_valid_num"] <- 1

# Only ECWR
merged$know_org_nehad_num <- 0
merged[grep("ecwr_nehad", merged$aux, value = F), "know_org_nehad_num"] <- 1


## ---- new folder ----
dir.create(file.path("Datasets", "Baseline"), showWarnings = FALSE)

## ---- baseline data ----
# Save the new data with baseline variables coded
write.csv(merged, file = 'Datasets/Baseline/merged_coded_baseline_variables.csv', row.names = F)

