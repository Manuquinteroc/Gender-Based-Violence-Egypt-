# Run Analysis on main indexes: For sample with dups

# Auxiliary function to create one-sided p-values
oneSidedTest <- function(lm, negative, twosided){ # if negative = FALSE (For H1: beta > 0) if negative = TRUE (For H1: beta < 0)
  
  if(twosided == T) {
    p <- coef(summary(lm))[, 4]
    
  } else {
    p <- pt(coef(summary(lm))[, 3], lm$df, lower = negative) 
    
  }
  
  return(p)
}

ci.level = 0.95


# Aux for linear hypothesis tests 
test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

# Read postratification and survey datasets
merged <- read.csv('Datasets/FinalData/Finaldata_wfinal_wDups.csv', fileEncoding = "latin1")

covariates <- c("SM Individual", "SM Group", "TV")
treatment <- c("pooledF_WI", "in_group_10018", "reminder_10018")

# Run Analysis -----------------------------------------------------------------

# Stargazer aux
dep_var <- c("\\shortstack{Index of \\\\ TV show \\\\ consumption}", 
             "\\shortstack{Index of \\\\ videos of  \\\\women's \\\\empowerment \\\\and support \\\\consumption}", 
             "\\shortstack{Index of \\\\ knowledge  \\\\ about \\\\ treatment \\\\information}", 
             "\\shortstack{Index of \\\\ attitudes  \\\\  toward \\\\ gender and \\\\ marital \\\\equality}", 
             "\\shortstack{Index of \\\\ attitudes on  \\\\ sexual  \\\\ violence}", 
             "\\shortstack{Index of \\\\ donation to  \\\\ organizations  \\\\ supporting  \\\\ women}", 
             "\\shortstack{Index of \\\\ domestic and  \\\\ sexual violence  \\\\ experienced  \\\\ during  \\\\ COVID-19}", 
             "\\shortstack{Index of \\\\ hypothetical use  \\\\ of online  \\\\ resources  \\\\ and contact with  \\\\ an organization  \\\\ when responding  \\\\ to domestic  \\\\ violence}", 
             "\\shortstack{Index of \\\\ hypothetical use  \\\\ of online  \\\\ resources  \\\\ and contact with  \\\\ an organization  \\\\ when responding  \\\\ to sexual  \\\\ violence}", 
             "\\shortstack{Index of \\\\ recent use  \\\\ of online  \\\\ resources and  \\\\ contact with  \\\\ an organization  \\\\ during  \\\\ COVID-19}", 
             "\\shortstack{Index of \\\\ views on  \\\\ women's  \\\\ future outlook  \\\\ toward gender  \\\\ and marital  \\\\ equality}")

dep_vars_names <- c("FS_TV_zscore",
                    "FS_FW_zscore",
                    "RF_knowledge2_zscore",
                    "RF_att1_zscore",
                    "RF_att2_zscore",
                    "RF_preference_zscore",
                    "RF_dcovid_zscore",
                    "RF_hypDM2_zscore",
                    "RF_hypSA2_zscore",
                    "RF_dcovid2_zscore",
                    "RF_fo_zscore")

dep_vars <- merged[, dep_vars_names]
control_means <- round(colMeans(dep_vars[which(merged$control_10018 == 1),]), digits = 3)

control1 <- c("FS_TV_zscore_base", "tv_evening_num", "tv_sattelite_num", "tv_top3_chan_num", 
              "tv_top3_shows_num", "sat_show_num",
              "age", "educ_aboveBA", "married")

control2 <- c("FS_FW_zscore_base", "X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num",
              "age", "educ_aboveBA", "married")

control3 <- c("RF_knowledge2_zscore_base", "know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num",
              "bc19_look_org_num","dc19_look_org_num",
              "know_org_noaut_valid_num", "know_online_nehad_num", 
              "know_org_nehad_num","age", "educ_aboveBA", "married")

control4 <- c("RF_att1_zscore_base", "husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num", 
              "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num", "age", "educ_aboveBA", "married")

control5 <- c("husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num",
              "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num", "age", "educ_aboveBA", "married")

control6 <- c("age", "educ_aboveBA", "married")

control7 <- c("RF_dcovid_zscore_base", "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num" , 
              "age", "educ_aboveBA", "married")

control8 <- c("RF_hypDM2_zscore_base", "talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

control9 <- c("talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

control10 <- c("RF_dcovid2_zscore_base", "know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num", "bc19_look_org_num", "dc19_look_org_num",
               "know_org_noaut_valid_num", "know_online_nehad_num", 
               "know_org_nehad_num", "age", "educ_aboveBA", "married")

control11 <- c("RF_fo_zscore_base", "husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num", 
               "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num",
               "age", "educ_aboveBA", "married")

control_all <- list(control1, control2, control3, control4, control5, control6, control7, control8,
                    control9, control10, control11)

lagged <- c("FS_TV_zscore_base",
            "FS_FW_zscore_base",
            "RF_knowledge2_zscore_base",
            "RF_att1_zscore_base",
            NA,
            NA,
            "RF_dcovid_zscore_base",
            "RF_hypDM2_zscore_base",
            NA,
            "RF_dcovid2_zscore_base",
            "RF_fo_zscore_base")

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithmIndexes.R')

refuse7 <- c("dcovid_yelled_end_num_refuse", "dcovid_hit_end_num_refuse", "dcovid_assault_end_num_refuse")
refuse10 <- c("dcovid_accessonline_end_num_refuse", "dcovid_contactorg_end_num_refuse")

refuse <- list(NA,NA,NA,NA,NA,NA,refuse7,NA,NA,refuse10,NA)


# Create tables and save results
size = length(dep_vars_names) + 1
cm = 20.5 # length in cm of the note for the table
negative <- c(F,F,F,F,F,T,T,F,F,F,F) # six entrance is T because of null effects across all T's
twosided <- c(F,F,F,F,F,T,T,F,F,F,F)
label = "Main_dups"

latex_font = "tiny"
omit_var <- c("Constant", "block_ids",  unlist(control_all), refuse7, refuse10, lagged[!is.na(lagged)])

# Run and Save table for summary, weight1, and weight2
source('Scripts/General Scripts/TreatmentEffectJointAlgorithmIndexes_Dups.R')
