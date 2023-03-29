# First Stage Analysis

# ------------------------------------------------------------------------------
# Auxiliary function to calculate the pvalues from one sided tests
oneSidedTest <- function(lm, negative, twosided){ # if negative = FALSE (For H1: beta > 0) if negative = TRUE (For H1: beta < 0)
  
  if(twosided == T) {
    p <- coef(summary(lm))[, 4]
    
  } else {
    p <- pt(coef(summary(lm))[, 3], lm$df, lower = negative) 
    
  }

  return(p)
}

ci.level = 0.9

# ------------------------------------------------------------------------------
# Read final dataset
merged <- read.csv('Datasets/Finaldata and Zscores/Finaldata and Zscores.csv', fileEncoding="latin1")

# Set the common covariates labels for all tables
covariates <- c("SM Individual", "SM Group", "TV")
treatment <- c("pooledF_WI", "in_group_10018", "reminder_10018")

# Aux for linear hypothesis tests 
test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

# Auxiliary boolean to include or exclude the non-response indicators
refuse <- NULL

# FS TV Show -------------------------------------------------------------------
dep_vars_names <- c("FS_TV_zscore","tv_evening_num_end", "three_viewed_channel_end_num",
                    "three_viewed_shows_end_num", "tv_sat_2mos_end_num", "watch_hekayat_end_num",
                    "heard_hekayat_end_num", "how_heard_hekayat_num",
                    "whatsapp_remind_2mos_end_num", "wheter_episodes_hekayat_end_num" , "episodes_hekayat_end_num",
                    "episodes_content_end_num_vague", "episode_content_like_end_num_vague")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

# Labels for DV
dep_var <- c("\\shortstack{Index of \\\\ (1,1,1,1,1,1,\\\\1,1,1,1,1,1)}",
              "\\shortstack{Watched TV \\\\ evening}",
              "\\shortstack{Watched \\\\channels of \\\\TV show}",
              "\\shortstack{Watched \\\\TV show \\\\type}",
              "\\shortstack{Mentioned \\\\watched TV \\\\show Saturday \\\\evening}",
              "\\shortstack{Watched \\\\TV show}",
              "\\shortstack{Heard of \\\\TV show}",
              "\\shortstack{Heard of \\\\TV show via \\\\WhatsApp}",
              "\\shortstack{Received \\\\TV show \\\\WhatsApp \\\\reminder}",
              "\\shortstack{Whether \\\\watched \\\\TV show \\\\episodes}", 
              "\\shortstack{Number of \\\\TV show \\\\episodes \\\\watched}",
              "\\shortstack{Accurate \\\\content of \\\\the TV show}",
              "\\shortstack{Accurate \\\\TV show topic \\\\liked}")

controls <- c("FS_TV_zscore_base", "tv_evening_num", "tv_sattelite_num", "tv_top3_chan_num", 
              "tv_top3_shows_num", "sat_show_num",
              "age", "educ_aboveBA", "married")

lagged <- c("FS_TV_zscore_base", "tv_evening_num", "tv_top3_chan_num", "tv_top3_shows_num", "sat_show_num",
            NA, NA, NA, NA, NA, NA, NA, NA)

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect on TV show consumption"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 20.5 # length in cm of the note for the table
latex_font = "tiny"
twosided = FALSE # if = FALSE: Perform one-sided tests (one-sided test only when hypothethisez and different than 0)
negative = FALSE # Auxiliary boolean to construct the pvalues from one-sided tests
label = "fs_tv"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size, 2.5), file = 'Tables/LASSO/S15.tex')


# First stage for Facebook and  WhatsApp Treatment -----------------------------
dep_vars_names <- c("FS_FW_zscore", "videos_socmed_2mos_end_num", "videos_whatsapp_2mos_end_num",
                    "videos_whatsapp_receive_end_num", "videos_watched_end_num",
                    "videos_watched_no_end_num", "videos_watched_content_end_num_vague", 
                    "video_content_like_end_num_vague")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1,1,1,1,1,1)}",
             "\\shortstack{Watched \\\\videos on \\\\social media}",
             "\\shortstack{Watched \\\\videos on\\\\ WhatsApp}",
             "\\shortstack{Received \\\\videos on \\\\WhatsApp or \\\\Facebook}",
             "\\shortstack{Watched \\\\videos on \\\\WhatsApp or \\\\Facebook}",
             "\\shortstack{Number of \\\\videos watched}",
             "\\shortstack{Accurate \\\\content of \\\\the videos}",
             "\\shortstack{Accurate \\\\video topic \\\\liked}")

controls <- c("FS_FW_zscore_base", "X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num",
              "age", "educ_aboveBA", "married")

lagged <- c("FS_FW_zscore_base", "X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num",
            NA, NA, NA, NA, NA)

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect on videos of women's empowerment and support consumption"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 19 # length in cm of the note for the table
latex_font = "tiny"
twosided = FALSE
negative = FALSE 
label = "fs_whats"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size, 1.5), file = 'Tables/LASSO/S16.tex')


# Reduced Form -----------------------------------------------------------------
# Knowledge of online resources and organizations ------------------------------
dep_vars_names <- c("RF_knowledge2_zscore", "online_dvsa_noaut_end_num","online_dvsa_nehad_end_num", 
                    "org_dvsa_noaut_end_num", "org_dvsa_nehad_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1,1,1)}", 
             "\\shortstack{Know online:\\\\ other than ECWR}",
             "\\shortstack{Know online:\\\\ ECWR}",
             "\\shortstack{Know organization:\\\\ other than ECWR}",
             "\\shortstack{Know organization:\\\\ ECWR}")

controls <- c("RF_knowledge2_zscore_base", "know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num",
              "bc19_look_org_num","dc19_look_org_num",
              "know_org_noaut_valid_num", "know_online_nehad_num", 
              "know_org_nehad_num","age", "educ_aboveBA", "married")

lagged <- c("RF_knowledge2_zscore_base", "know_online_valid_noaut_num", "know_online_nehad_num",
            "know_org_noaut_valid_num", "know_org_nehad_num")

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect on knowledge about treatment information"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 15 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = FALSE
negative = FALSE 
label = "rf_1"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S17.tex')


# Attitudes towards female empowerment, husband role, and women role in the workplace (domestic related) -----
dep_vars_names <- c("RF_att1_zscore", "husb_final_say_end_num","husb_inc_end_num", "husb_just_yell_end_num",
                     "woman_work_outside_end_num", "female_circ_marriage_end_num", "circ_tf_end_num",
                    "marriage_age_tf_end_num", "khul_divorce_tf_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (-1,-1,-1,1, \\\\ -1,-1,-1,1)}", 
             "Husband final say", 
             "Husband earn income", 
             "Yelling justified", 
             "\\shortstack{Gain  \\\\ independence \\\\ by working\\\\ outside the\\\\  household}",
             "\\shortstack{Circumcision \\\\ important \\\\ for women \\\\ marriage}", 
             "\\shortstack{Female \\\\circumcision \\\\ health \\\\benefits}",
             "\\shortstack{Marriage\\\\ permitted \\\\ under age 18 with \\\\family consent}",
             "\\shortstack{Khul: \\\\ Women can \\\\divorce husband \\\\withouth \\\\a reason}")

controls <- c("RF_att1_zscore_base", "husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num", 
                "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num", "age", "educ_aboveBA", "married")

lagged <- c("RF_att1_zscore_base", "husb_final_say_num", "husb_provide_inc_num",
            "husb_justified_yell_num", NA, NA, NA, NA, NA)

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effects on attitudes towards gender and marital equality"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 18.5 # length in cm of the note for the table
latex_font = "tiny"
twosided = FALSE
negative = FALSE 
label = "rf_2"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size, 1), file = 'Tables/LASSO/S18.tex')

# Attitudes towards violence outside the household ----------------------------------------------------
dep_vars_names <- c("RF_att2_zscore", "colleague_comment_looks_end_num", "verbal_harassment_legal_end_num", 
                    "intervene_work_harass_end_num", 
                    "women_clothes_harass_end_num","intervene_hitting_street_num", "you_intervene_harass_end_num", 
                    "parents_assault_auth_end_num", "child_relative_assault_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1,-1,1,\\\\1,-1,1,-1)}", 
             "\\shortstack{Colleague \\\\ comments \\\\ on female \\\\ look \\\\ sexual \\\\ harassment}", 
             "\\shortstack{Verbal \\\\ harassment\\\\ legal \\\\consequences}",
             "\\shortstack{Interfere to \\\\ support \\\\a woman \\\\ sexually \\\\harassed \\\\ at workplace}", 
             "\\shortstack{Inappropriate\\\\ clothing \\\\ or lack of Hijab \\\\ justifies \\\\ harassment}",
             "\\shortstack{Interfere \\\\ if a man \\\\ hits a woman \\\\ on the street}",
             "\\shortstack{Interfere if a \\\\man sexually \\\\ harasses  on\\\\ the street}",
             "\\shortstack{Avoid \\\\ the authorities \\\\ if your daughter \\\\ sexually \\\\ assaulted}",
             "\\shortstack{Seriousness \\\\ of a child \\\\ telling \\\\ that was \\\\ sexually \\\\harassed\\\\ by a relative}")

controls <- c("husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num",
                "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num", "age", "educ_aboveBA", "married")

lagged <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect on attitudes on sexual violence"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 17 # length in cm of the note for the table
latex_font = "tiny"
twosided = FALSE
negative = FALSE 
label = "rf_3"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S19.tex')

# Revealed Preference ----------------------------------------------------------
dep_vars_names <- c("RF_preference_zscore", "donation__end_cont", "donation_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\(1,1)}", 
             "Donation in EGP", 
             "\\shortstack{Donating more \\\\ than 0 EGP}")

controls <- c("age", "educ_aboveBA", "married")

lagged <- c(NA, NA, NA)

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect on donation to organizations supporting women"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 10.5 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = TRUE # Although there the direction is hypothesized, there is a null effect across all T's
negative = FALSE # Irrelevant once twosided = TRUE
label = "rf_4"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S20.tex')


# 4 - 5: DV look online for resources or org -----------------------------------
dep_vars_names <- c("RF_hypDM2_zscore", "dv_onlineres_end_num", "dv_contactorg_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1)}",
             "\\shortstack{Would use \\\\ online resources}",
             "\\shortstack{Would contact \\\\ organization}")

controls <- c("RF_hypDM2_zscore_base", "talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
                "age", "educ_aboveBA", "married")

lagged <- c("RF_hypDM2_zscore_base", "look_online_num", "contact_org_num")

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect on hypothetical use of online resources and contact with an organization when responding to domestic violence"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 10 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = FALSE
negative = FALSE 
label = "rf_5"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S21.tex')


# Variables contact or look organization/online resources ----------------------
dep_vars_names <- c("RF_hypSA2_zscore", "sa_onlineres_end_num", "sa_contactorg_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1)}", 
             "\\shortstack{Would use \\\\ online resources}",
             "\\shortstack{Would contact \\\\ organization}")


controls <- c("talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

lagged <- c(NA, NA, NA)

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect on  hypothetical use of online resources and contact with an organization when responding to sexual violence"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 10.5 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = FALSE
negative = FALSE 
label = "rf_6"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S22.tex')

# During COVID-19 --------------------------------------------------------------
dep_vars_names <- c("RF_dcovid2_zscore", "dcovid_accessonline_end_num","dcovid_contactorg_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1) }", 
             "\\shortstack{Used online\\\\ resources}", 
             "\\shortstack{Contacted \\\\ organization}")

controls <- c("RF_dcovid2_zscore_base", "know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num", "bc19_look_org_num", "dc19_look_org_num",
              "know_org_noaut_valid_num", "know_online_nehad_num", 
              "know_org_nehad_num", "age", "educ_aboveBA", "married")

lagged <- c("RF_dcovid2_zscore_base", "dc19_look_online_num", "dc19_look_org_num")

refuse <- list(c("dcovid_accessonline_end_num_refuse", "dcovid_contactorg_end_num_refuse"), 
               "dcovid_accessonline_end_num_refuse", "dcovid_contactorg_end_num_refuse")

omit_var <- c("Constant", "block_ids", controls, unlist(refuse))

title <- "Treatment effect on recent use of online resources and contact with an organization during COVID-19"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 10.5 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = FALSE
negative = FALSE 
label = "rf_7"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S23.tex')

refuse <- NULL

# -----------------------------------------------------------------------------------------------
# Future Outlook
dep_vars_names <- c("RF_fo_zscore", "future_equal_say_end_num","future_equal_rights_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1) }", 
             "\\shortstack{Used online\\\\ resources}", 
             "\\shortstack{Contacted \\\\ organization}")

controls <- c("RF_fo_zscore_base", "husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num", 
              "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num",
              "age", "educ_aboveBA", "married")

lagged <- c("RF_fo_zscore_base", "future_equal_say_num", "future_equal_rights_num")

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect on views on women's future outlook toward gender and marital equality"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 11 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = FALSE
negative = FALSE 
label = "rf_8"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S24.tex')


# Reduced form for behavior and reporting during COVID-19 ----------------------
dep_vars_names <- c("RF_dcovid_zscore", "dcovid_yelled_end_num","dcovid_hit_end_num","dcovid_assault_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1,1)}", 
             "\\shortstack{Heard of or \\\\ experienced yelling}", 
             "\\shortstack{Heard of or \\\\ experienced hitting}", 
             "\\shortstack{Heard of or \\\\ experienced sexual \\\\ abuse}")

controls <- c("RF_dcovid_zscore_base", "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num" , 
              "age", "educ_aboveBA", "married")

lagged <- c("RF_dcovid_zscore_base", "dc19_yell_num", "dc19_hit_num")

refuse <- list(c("dcovid_yelled_end_num_refuse", "dcovid_hit_end_num_refuse", "dcovid_assault_end_num_refuse"), 
               "dcovid_yelled_end_num_refuse", "dcovid_hit_end_num_refuse", "dcovid_assault_end_num_refuse")

omit_var <- c("Constant", "block_ids", controls, unlist(refuse))

title <- "Treatment effect on domestic and sexual violence experienced during COVID-19"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 13.5 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = TRUE
label = "rf_9"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S25.tex')

refuse <- NULL

# Behavior and Reporting (part of Observed Behavior) ---------------------------
# BEFORE COVID -----------------------------------------------------------------
dep_vars_names <- c("RT_bcovid_zscore", "bcovid_yelled_end_num", "bcovid_hit_end_num", "bcovid_assault_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1,1)}",
             "\\shortstack{Heard of or\\\\experienced yelling}", 
             "\\shortstack{Heard of or\\\\experienced hitting}", 
             "\\shortstack{Heard of or\\\\experienced sexual \\\\ abuse}")

controls <- c("RT_bcovid_zscore_base", "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num" , 
              "age", "educ_aboveBA", "married")

lagged <- c("RT_bcovid_zscore_base", "bc19_yell_num", "bc19_hit_num")

refuse <- list(c("bcovid_yelled_end_num_refuse", "bcovid_hit_end_num_refuse", "bcovid_assault_end_num_refuse"), 
               "bcovid_yelled_end_num_refuse", "bcovid_hit_end_num_refuse", "bcovid_assault_end_num_refuse")

omit_var <- c("Constant", "block_ids", controls, unlist(refuse))

title <- "Treatment effects on domestic and sexual violence experienced before COVID-19"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 13.5 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = TRUE
label = "rf_10"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S26.tex')

refuse <- NULL

# Hypothetical Behavior -----------------------------------------------------------
# Domestic Violence, Sexual Assault, or Harassment
# Reduced form for hypothetical behavior around domestic violence (DV) variables,part 2
dep_vars_names <- c("RF_hypDM1_zscore", "dv_talkhusb_end_num", "dv_talktfam_end_num", "dv_report_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1,1)}", 
             "\\shortstack{Would \\\\ talk husband}", 
             "\\shortstack{Would \\\\ talk family}", 
             "\\shortstack{Would \\\\ report \\\\ authorities}")

controls <- c("RF_hypDM1_zscore_base", "talk_husband_num", "talk_family_num", 
              "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

lagged <- c("RF_hypDM1_zscore_base", "talk_husband_num", "talk_family_num", "report_authorities_num")

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect of hypothetical talking to husband and family members, or reporting to authorities when responding to domestic violence"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 11 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = TRUE
label = "rf_11"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S27.tex')


# Part 2: Sexual Assault or Harassment -----------------------------------------
dep_vars_names <- c("RF_hypSA1_zscore", "sa_talkfam_end_num", "sa_report_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1)}", 
             "\\shortstack{Would  \\\\ talk family}", 
             "\\shortstack{Would \\\\  report  \\\\ authorities}")

controls <- c("talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
             "age", "educ_aboveBA", "married")

lagged <- c(NA, NA, NA)

omit_var <- c("Constant", "block_ids", controls)

title <- "Treatment effect of hypothetical talking to husband and family members, or reporting to authorities when responding to sexual violence"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 10.5 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = TRUE
label = "rf_12"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S28.tex')

# Knowledge, Behavior, and Reporting -------------------------------------------
# Before COVID-19
dep_vars_names <- c("RT_bcovidAccess_zscore", "bcvovid_accessonline_end_num",
                    "bcovid_contactorg_end_num")

dep_vars <- merged[, dep_vars_names]

control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

dep_var <- c("\\shortstack{Index of \\\\ (1,1) }", 
             "\\shortstack{Used online \\\\ resources}", 
             "\\shortstack{Contacted \\\\ organization}")

controls <- c("RT_bcovidAccess_zscore_base",
              "know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num", 
              "bc19_look_org_num", "dc19_look_org_num", "know_org_noaut_valid_num",
              "know_online_nehad_num", "know_org_nehad_num", 
              "age", "educ_aboveBA", "married")

lagged <- c("RT_bcovidAccess_zscore_base", "bc19_look_online_num", "bc19_look_org_num")

refuse <- list(c("bcvovid_accessonline_end_num_refuse", "bcovid_contactorg_end_num_refuse"), 
               "bcvovid_accessonline_end_num_refuse", "bcovid_contactorg_end_num_refuse")

omit_var <- c("Constant", "block_ids", controls, unlist(refuse))

title <- "Treatment effects on recent use of online resources and contact with an organization when responding to domestic and sexual violence before COVID-19"

# Create tables and save results
size = length(dep_vars_names) + 1
cm = 10.5 # length in cm of the note for the table
latex_font = "scriptsize"
twosided = TRUE
label = "rf_13"

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithm.R')

# Construct table
source('Scripts/General Scripts/TreatmentEffectJointAlgorithm.R')

# Save Table
cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size), file = 'Tables/LASSO/S29.tex')

refuse <- NULL