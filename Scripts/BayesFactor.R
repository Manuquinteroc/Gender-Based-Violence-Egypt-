# Compute Bayes Factor for null coefficients in main indexes 
library(BayesFactor)

# Auxiliary Functions ----------------------------------------------------------

clean_full <- function(x) {
  # Clean a string formula to change fixed effect labels for full model with T \neq 0.
  text <- paste(deparse(x), collapse = "")
  text <- str_replace_all(text, " ", "")
  str_replace(text, "factor\\(block_ids\\)", "block_ids") 
}

# Create null equations by removing the corresponding treatment to test
# These function take string formulas without spaces
remove_F_WI <- function(x){
  str_replace(x, "pooledF_WI\\+", "")
}

remove_WG <- function(x){
  str_replace(x, "in_group_10018\\+", "")
}

remove_TV <- function(x){
  str_replace(x, "reminder_10018\\+", "")
}

# Auxiliary function to add notes
add_notes <- function(xtable, note) {
  
  xtable <- gsub("\\\\end\\{table", "\\\\justify {\\\\footnotesize \\\\textit{Notes:} %} \\\\end\\{table", xtable)
  xtable <- gsub("%", note, xtable)  
  
  return(xtable) 
}

resizebox <- function(xtable) {
  
  xtable <- gsub("begin\\{tabular", "\\resizebox{\\\\textwidth}{!}{ \\\\begin{tabular", xtable)
  xtable <- gsub("end\\{tabular", "end\\{tabular}", xtable)
  
  return(xtable) 
}

# ------------------------------------------------------------------------------
merged <- read.csv('Datasets/Poststratification/Endline_weights.csv', fileEncoding = "latin1")

covariates <- c("SM Individual", "SM Group", "TV")
treatment <- c("pooledF_WI", "in_group_10018", "reminder_10018")

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
                    "RF_fo_zscore",
                    "RT_bcovid_zscore",
                    "RF_hypDM1_zscore",
                    "RF_hypSA1_zscore",
                    "RT_bcovidAccess_zscore")

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

control12 <- c("RT_bcovid_zscore_base", "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num" , 
              "age", "educ_aboveBA", "married")

control13 <- c("RF_hypDM1_zscore_base", "talk_husband_num", "talk_family_num", 
              "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

control14 <- c("talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

control15 <- c("RT_bcovidAccess_zscore_base",
              "know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num", 
              "bc19_look_org_num", "dc19_look_org_num", "know_org_noaut_valid_num",
              "know_online_nehad_num", "know_org_nehad_num", 
              "age", "educ_aboveBA", "married")

control_all <- list(control1, control2, control3, control4, control5, control6, control7, control8,
                    control9, control10, control11, control12, control13, control14, control15)

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
            "RF_fo_zscore_base",
            "RT_bcovid_zscore_base",
            "RF_hypDM1_zscore_base",
            NA,
            "RT_bcovidAccess_zscore_base")

# Run LASSO
source('Scripts/General Scripts/LASSOAlgorithmIndexes.R')

refuse7 <- c("dcovid_yelled_end_num_refuse", "dcovid_hit_end_num_refuse", "dcovid_assault_end_num_refuse")
refuse10 <- c("dcovid_accessonline_end_num_refuse", "dcovid_contactorg_end_num_refuse")
refuse12 <- c("bcovid_yelled_end_num_refuse", "bcovid_hit_end_num_refuse", "bcovid_assault_end_num_refuse")
refuse15 <- c("bcvovid_accessonline_end_num_refuse", "bcovid_contactorg_end_num_refuse")

refuse <- list(NA,NA,NA,NA,NA,NA,refuse7,NA,NA,refuse10,NA, refuse12, NA, NA, refuse15)

# Auxiliary lists to store lm objects for each panel
lm_A_main <- list()
lm_B_main <- list()
lm_C_main <- list()

# auxiliary formula list for Bayes Factor 
fmla1_list <- list()
fmla2_list <- list()
fmla3_list <- list()

count <- 1

# Run all regresions
for (x in dep_vars_names) {
  
  if (!any(is.na(refuse[[count]]))) { # If there are non-response indicators:
    
    # Panel A
    if(!any(is.na(control_list[[count]]))) {
      fmla1 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + ", paste(control_list[[count]], collapse = " + "),
                                 " + ", paste(refuse[[count]], collapse = " + "),
                                 "+ factor(block_ids)"))
    } else {
      fmla1 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "),
                                 " + ", paste(refuse[[count]], collapse = " + "),
                                 "+ factor(block_ids)"))
    }
    
    # Panel B 
    if(!is.na(lagged[count])) {
      fmla2 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + ", lagged[count],
                                 " + ", paste(refuse[[count]], collapse = " + "),
                                 "+ factor(block_ids)"))
    } else {
      fmla2 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + ", 
                                 paste(refuse[[count]], collapse = " + "),
                                 "+ factor(block_ids)"))
    }
    
    # Panel C
    fmla3 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + " ,
                               paste(refuse[[count]], collapse = " + "),"+ factor(block_ids)"))
    
    
  } else { # If there are non-response indicators:
    
    # Panel A
    if(!any(is.na(control_list[[count]]))) {
      fmla1 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + ", paste(control_list[[count]], collapse = " + "),
                                 "+ factor(block_ids)"))
    } else {
      fmla1 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "),
                                 "+ factor(block_ids)"))
    }
    
    # Panel B 
    if(!is.na(lagged[count])) {
      fmla2 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + ", lagged[count],
                                 "+ factor(block_ids)"))
    } else {
      fmla2 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), "+ factor(block_ids)"))
    }
    
    # Panel C
    fmla3 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), "+ factor(block_ids)"))
  }
  
  
  
  # Assign lm objects to different lists
  nam1 <- paste("lm_", count, "_A", sep = "")
  nam2 <- paste("lm_", count, "_B", sep = "")
  nam3 <- paste("lm_", count, "_C", sep = "")
  
  assign(nam1, lm(fmla1, weights = weight, data = merged))
  assign(nam2, lm(fmla2, weights = weight, data = merged))
  assign(nam3, lm(fmla3, weights = weight, data = merged))
  
  lm_A_main[[count]] <- get(nam1, envir = globalenv())
  lm_B_main[[count]] <- get(nam2, envir = globalenv())
  lm_C_main[[count]] <- get(nam3, envir = globalenv())
  
  # Auxiliary list of formulas 
  fmla1_list[[count]] <- fmla1
  fmla2_list[[count]] <- fmla2
  fmla3_list[[count]] <- fmla3
  
  count <- count + 1
  
}

# Needed for bayesFactor
merged$block_ids <- as.factor(merged$block_ids)

# Clean formulas for full equations
fmla1_list <- lapply(fmla1_list, clean_full)
fmla2_list <- lapply(fmla2_list, clean_full)
fmla3_list <- lapply(fmla3_list, clean_full)

# Remove indicators where needed
fmla1_null_F_WI <- lapply(fmla1_list, remove_F_WI)
fmla1_null_WG <- lapply(fmla1_list, remove_WG)
fmla1_null_TV <- lapply(fmla1_list, remove_TV)

# Select necessary formulas
# Null coefficients
# lm_A_main[[4]] -- F_WI, TV (10% - WG)
# lm_A_main[[5]] -- F_WI, WG (10% - TV)
# lm_A_main[[6]] -- F_WI, WG, TV 
# lm_A_main[[7]] -- F_WI, WG, TV
# lm_A_main[[9]] -- TV
# lm_A_main[[11]] -- WG
# lm_A_main[[12]] -- WG, TV
# lm_A_main[[13]] -- F_WI, WG, TV
# lm_A_main[[14]] -- F_WI, WG, TV
# lm_A_main[[15]] -- F_WI, WG, TV

F_WI_subset <- list(fmla1_list[[4]], fmla1_list[[5]], fmla1_list[[6]], fmla1_list[[7]], fmla1_list[[13]], fmla1_list[[14]], fmla1_list[[15]])
WG_subset <- list(fmla1_list[[4]], fmla1_list[[5]], fmla1_list[[6]], fmla1_list[[7]], fmla1_list[[11]], fmla1_list[[12]], fmla1_list[[13]], fmla1_list[[14]], fmla1_list[[15]])
TV_subset <- list(fmla1_list[[4]], fmla1_list[[5]], fmla1_list[[6]], fmla1_list[[7]], fmla1_list[[9]], fmla1_list[[12]], fmla1_list[[13]], fmla1_list[[14]], fmla1_list[[15]])

F_WI_subset_null <- list(fmla1_null_F_WI[[4]], fmla1_null_F_WI[[5]], fmla1_null_F_WI[[6]], fmla1_null_F_WI[[7]], fmla1_null_F_WI[[13]], fmla1_null_F_WI[[14]], fmla1_null_F_WI[[15]])
WG_subset_null <- list(fmla1_null_WG[[4]], fmla1_null_WG[[5]], fmla1_null_WG[[6]], fmla1_null_WG[[7]], fmla1_null_WG[[11]], fmla1_null_WG[[12]], fmla1_null_WG[[13]], fmla1_null_WG[[14]], fmla1_null_WG[[15]])
TV_subset_null <- list(fmla1_null_TV[[4]], fmla1_null_TV[[5]], fmla1_null_TV[[6]], fmla1_null_TV[[7]], fmla1_null_TV[[9]], fmla1_null_TV[[12]], fmla1_null_TV[[13]], fmla1_null_TV[[14]], fmla1_null_TV[[15]])

# Empty vectors to store Bayes Factors
BF_1 <- c()
BF_2 <- c()
BF_3 <- c()

# Compute quotient (BF) for all regressions comparing T = 0 and T \neq 0

# F_WI
set.seed(32)
count <- 1
for (count in 1:length(F_WI_subset)) { # calculates length(dep_vars_names) x 6
  nam1_BF <- paste("lm_", count, "_F_WI_BF", sep = "")
  assign(nam1_BF, lmBF(as.formula(F_WI_subset[[count]]), data = merged, whichRandom = NULL)) # iterations = 1000
  nam1_BF_null <- paste("lm_", count, "_F_WI_BF_null", sep = "")
  assign(nam1_BF_null, lmBF(as.formula(F_WI_subset_null[[count]]), data = merged, whichRandom = NULL))
  BF_1 = c(BF_1, get(paste("lm_", count, "_F_WI_BF", sep = "")) / get(paste("lm_", count, "_F_WI_BF_null", sep = "")))
  count <- count + 1
}

# WG 
set.seed(32)
count <- 1
for (count in 1:length(WG_subset)) { # calculates length(dep_vars_names) x 6
  nam2_BF <- paste("lm_", count, "_WG_BF", sep = "")
  assign(nam2_BF, lmBF(as.formula(WG_subset[[count]]), data = merged, whichRandom = NULL)) # whichRandom = "block_ids"
  nam2_BF_null <- paste("lm_", count, "_WG_BF_null", sep = "")
  assign(nam2_BF_null, lmBF(as.formula(WG_subset_null[[count]]), data = merged, whichRandom = NULL))
  BF_2 = c(BF_2, get(paste("lm_", count, "_WG_BF", sep = "")) / get(paste("lm_", count, "_WG_BF_null", sep = "")))
  count <- count + 1
}

# TV 
set.seed(32)
count <- 1
for (count in 1:length(TV_subset)) { # calculates length(dep_vars_names) x 6
  nam3_BF <- paste("lm_", count, "_TV_BF", sep = "")
  assign(nam3_BF, lmBF(as.formula(TV_subset[[count]]), data = merged, whichRandom = NULL))  
  nam3_BF_null <- paste("lm_", count, "_TV_BF_null", sep = "")
  assign(nam3_BF_null, lmBF(as.formula(TV_subset_null[[count]]), data = merged, whichRandom = NULL))  
  BF_3 = c(BF_3, get(paste("lm_", count, "_TV_BF", sep = "")) / get(paste("lm_", count, "_TV_BF_null", sep = "")))
  count <- count + 1
}

# Retrieve all BF values from the S4 objects returned from lmBF
BF_F_WI_values <- lapply(BF_1, function(x){
  as.data.frame(x)$bf
})

BF_WG_values <- lapply(BF_2, function(x){
  as.data.frame(x)$bf
})

BF_TV_values <- lapply(BF_3, function(x){
  as.data.frame(x)$bf
})

# From lists to vectors
BF_F_WI_values <- round(unlist(BF_F_WI_values), 3)
BF_WG_values <- round(unlist(BF_WG_values), 3)
BF_TV_values <- round(unlist(BF_TV_values), 3)

# Fill Bayes factor vectors with zeros for non-null coefficients
BF_F_WI <- c("-","-","-", BF_F_WI_values[1], BF_F_WI_values[2], BF_F_WI_values[3],BF_F_WI_values[4],"-","-","-","-","-", BF_F_WI_values[5], BF_F_WI_values[6], BF_F_WI_values[7])
BF_WG <- c("-","-","-", BF_WG_values[1], BF_WG_values[2], BF_WG_values[3], BF_WG_values[4],"-","-","-", BF_WG_values[5], BF_WG_values[6], BF_WG_values[7], BF_WG_values[8], BF_WG_values[9])
BF_TV <- c("-","-","-", BF_TV_values[1], BF_TV_values[2], BF_TV_values[3], BF_TV_values[4],"-", BF_TV_values[5],"-","-", BF_TV_values[6], BF_TV_values[7], BF_TV_values[8], BF_TV_values[9])


# Notice that when running the Bayes Factor to test if SM individual = 0, 1 BF is not calculated. For WhatsApp Group = 0 test, 3 BF are not calculated 
# And 2 BF for the TV coefficient that cannot be computed because of 
# minimization problems with the vmin in the optim(qs, Qg, gr = dQg, control = list(fnscale = -1), method = "BFGS") function. 
# We replace these 4 BF values with the analogous BF but using the models in Panel B (controlling by the lagged DV).

# Remove indicators where needed
fmla2_null_F_WI <- lapply(fmla2_list, remove_F_WI)
fmla2_null_WG <- lapply(fmla2_list, remove_WG)
fmla2_null_TV <- lapply(fmla2_list, remove_TV)

F_WI_subset2 <- list(fmla2_list[[7]])
WG_subset2 <- list(fmla2_list[[7]], fmla2_list[[11]], fmla2_list[[12]])
TV_subset2 <- list(fmla2_list[[7]], fmla2_list[[12]])

F_WI_subset_null2 <- list(fmla2_null_F_WI[[7]])
WG_subset_null2 <- list(fmla2_null_WG[[7]], fmla2_null_WG[[11]], fmla2_null_WG[[12]])
TV_subset_null2 <- list(fmla2_null_TV[[7]], fmla2_null_TV[[12]])

# F_WI
set.seed(32)
bf_f_wi = list()
bf_f_wi[1] <- lmBF(as.formula(F_WI_subset2[[1]]), data = merged, whichRandom = NULL) / lmBF(as.formula(F_WI_subset_null2[[1]]), data = merged, whichRandom = NULL)

bf_f_wi <- lapply(bf_f_wi, function(x){
  as.data.frame(x)$bf
})

bf_f_wi <- round(unlist(bf_f_wi), 3)

# WG
set.seed(32)
bf_wg = list()
bf_wg[1] <- lmBF(as.formula(WG_subset2[[1]]), data = merged, whichRandom = NULL) / lmBF(as.formula(WG_subset_null2[[1]]), data = merged, whichRandom = NULL)
bf_wg[2] <- lmBF(as.formula(WG_subset2[[2]]), data = merged, whichRandom = NULL) / lmBF(as.formula(WG_subset_null2[[2]]), data = merged, whichRandom = NULL)
bf_wg[3] <- lmBF(as.formula(WG_subset2[[3]]), data = merged, whichRandom = NULL) / lmBF(as.formula(WG_subset_null2[[3]]), data = merged, whichRandom = NULL)

bf_wg <- lapply(bf_wg, function(x){
  as.data.frame(x)$bf
})

bf_wg <- round(unlist(bf_wg), 3)

# TV
set.seed(32)
bf_tv = list()
bf_tv[1] <- lmBF(as.formula(TV_subset2[[1]]), data = merged, whichRandom = NULL) / lmBF(as.formula(TV_subset_null2[[1]]), data = merged, whichRandom = NULL)
bf_tv[2] <- lmBF(as.formula(TV_subset2[[2]]), data = merged, whichRandom = NULL) / lmBF(as.formula(TV_subset_null2[[2]]), data = merged, whichRandom = NULL)

bf_tv <- lapply(bf_tv, function(x){
  as.data.frame(x)$bf
})

bf_tv <- round(unlist(bf_tv), 3)

# Replace values
BF_F_WI[is.na(BF_F_WI)] <- bf_f_wi
BF_WG[is.na(BF_WG)] <- bf_wg
BF_TV[is.na(BF_TV)] <- bf_tv

# Create Xtable ----------------------------------------------------------------

# Rows
columns <- c("Index", "SM Individual", "SM Group", "TV")

Indexes <- c("Index on TV show consumption",
             "Index of video of women's empowerment and support consumption",
             "Index of knowledge about treatment information",
             "Index of attitudes toward gender and marital equality",
             "Index of attitudes on sexual violence",
             "Index of donation to organizations supporting women",
             "Index of domestic and sexual violence experienced during COVID-19",
             "Index of hypothetical use of online resources and contact with an organization when responding to domestic violence",
             "Index of hypothetical use of online resources and contact with an organization when responding to sexual violence",
             "Index of recent use of online resources and contact with an organization during COVID-19",
             "Index of views on women's future outlook toward gender and marital equality",
             "Index of domestic and sexual violence experienced before COVID-19",
             "Index of hypothetical talking to husband, family members, or reporting to authorities when responding to domestic violence",
             "Index of hypothetical talking to husband, family members, or reporting to authorities when responding to sexual violence",
             "Index of recent use of online resources and contact with an organization before COVID-19")

M <- as.matrix(cbind(rep(0,15), rep(0,15), rep(3, 15), rep(3, 15), rep(3, 15) ))

table1 <- cbind.data.frame(Indexes, BF_F_WI, BF_WG, BF_TV)
colnames(table1) <- columns

table1 <- xtable(table1, 
                 digits = M, 
                 label = "tab:BayesFactor",
                 align= c("c", "p{0.6\\textwidth}", "p{0.11\\textwidth}", "p{0.11\\textwidth}", "p{0.11\\textwidth}"),
                 caption = "Bayes Factor for statistically insignificant coefficients in our main indexes estimates")

# names(table1)[1]<-"\\multicolumn{1}{>{\\centering}p{10cm}}{Index}"
# names(table1)[2]<-"\\multicolumn{1}{>{\\centering}p{3cm}}{SM Individual}"
# names(table1)[3]<-"\\multicolumn{1}{>{\\centering}p{3cm}}{SM Group}"
# names(table1)[4]<-"\\multicolumn{1}{>{\\centering}p{3cm}}{TV}"

table1 <- print(table1, 
                table.placement = "H",
                size = "scriptsize",
                caption.placement = "top",
                include.rownames = FALSE,
                comment = FALSE, 
                sanitize.colnames.function=function(x){x},
                hline.after = c(-1,0,15))

note <- c("We compute the Bayes Factor for each non-statistically significant coefficient at the 95\\\\% 
          level in our main indexes when including all variables selected by the LASSO model. 
          The Bayes Factor compares under the null hypothesis the corresponding treatment indicator equal to $0$
          and under the alternative hypothesis distinct than $0$.")
table1 <- add_notes(table1, note)

# Save table
cat(table1,"\n", file = 'Tables/BayesFactor.tex')

