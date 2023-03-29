# Read final dataset
merged <- read.csv('Datasets/Finaldata/Finaldata.csv', fileEncoding = "latin1")

# Set the common covariates labels for all tables
covariates <- c("SM Individual", "SM Group", "TV")

# Single Balance table ---------------------------------------------------------
# Create a single variable for treatment
merged$treatment <- 0
merged$treatment[which(merged$pooledF_WI == 1)] <- 1
merged$treatment[which(merged$in_group_10018 == 1)] <- 2
merged$treatment[which(merged$reminder_10018 == 1)] <- 3

# Get frequencies per treatment art
n <- table(merged$treatment)

# Variables to test balance (60 vars)
summ_vars <- c(c("age", "ba_above", "male_children", "female_children", "sum_family_mem"),
               c("married", "husb_age", "length_marriage", "husb_live_loc_num", "education_husb_num"),
              c("bcovid_activity_num", "bcovid_activity_ph_num" , 
                "bcovid_act_husbmar_num","bcovid_activity_husbmar_ph_num",
                "dcovid_activity_num","dcovid_activity_ph_num",
                "dcovid_activity_husb_num", "dcovid_activity_husbmar_ph_num",
                "covid_inc_decline"),
              c("tv_morning_num", "tv_afternoon_num",  
                "tv_evening_num", "tv_sattelite_num",
                "tv_top3_chan_num",
                "tv_top3_shows_num", "sat_show_num"),
              c("hours_soc_med_num", "socmed_whatsapp", "socmed_Facebook", "socmed_Instagram",
                "socmed_YouTube", "socmed_Twitter", "socmed_Snapchat", "socmed_Telegram",
                "X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num"),
              c("husb_final_say_num", "husb_provide_inc_num",
                "husb_justified_yell_num", "husb_justified_beat_num", "prioritize_educ_num", 
                "future_equal_say_num", "future_equal_rights_num"),
              c("bc19_yell_num","bc19_hit_num",
                "dc19_yell_num", "dc19_hit_num"),
              c("talk_husband_num","talk_family_num","report_authorities_num",
                "look_online_num","contact_org_num"),
              c("know_online_valid_noaut_num", "know_online_nehad_num",
                "bc19_look_online_num", "dc19_look_online_num", 
                "know_org_noaut_valid_num", "know_org_nehad_num",
                "bc19_look_org_num", "dc19_look_org_num"))

# Get mean of each variable per treatment arm
mean_table <- merged %>% group_by(treatment) %>% 
  summarise(across(summ_vars, mean, na.rm = TRUE))
mean_table <- t(as.data.frame(round(mean_table, 2)))

# Get sd of each variable per treatment arm
sd_table <- merged %>% group_by(treatment) %>% 
  summarise(across(summ_vars, sd, na.rm = TRUE))
sd_table <- t(as.data.frame(round(sd_table, 2)))

# Paste Dataframes cell to cell
i <- 1
mean_sd_df <- as.data.frame(apply(mean_table, 2, function(x) {
  i<<-i+1
  paste0(x, " (", sd_table[,i-1],")")
  }))

# Delete first row
mean_sd_df <- mean_sd_df[-1,]

# Rename columns
names(mean_sd_df) <- c("Control", "SM Individual", "SM Group", "TV")

# Auxiliary vectors for eatch treatment arm: mean, sd, var, N to obtain t stat
mean_control <- mean_table[,1]
sd_control <- sd_table[,1]
var_control <- (sd_table[,1])^2
n_control <- rep(n[1], length(mean_control))

mean_ind <- mean_table[,2]
sd_ind <- sd_table[,2]
var_ind <- (sd_table[,2])^2
n_ind <- rep(n[2], length(mean_ind))

mean_g <- mean_table[,3]
sd_g <- sd_table[,3]
var_g <- (sd_table[,3])^2
n_g <- rep(n[3], length(mean_g))

mean_tv <- mean_table[,4]
sd_tv <- sd_table[,4]
var_tv <- (sd_table[,4])^2
n_tv <- rep(n[4], length(mean_tv))

# Obtain t-stat and Two-tailed test p-value
tstat <- function(m1,m2,v1,v2,n1,n2) {
  
  # Calculate t-value
  est <- m1 - m2
  se <- sqrt( (v1/n1) + (v2/n2) )
  t <- est/se
  
  # Two-tailed test p-value
  pval <- 2*pt(abs(t), min(n1, n2) - 1, lower.tail = F) 
  
  return(list(est, pval))
}

# Calculate t and p val 
aux_list_ind <- tstat(mean_control, mean_ind, var_control, var_ind, n_control, n_ind)
aux_list_g <- tstat(mean_control, mean_g, var_control, var_g, n_control, n_g)
aux_list_tv <- tstat(mean_control, mean_tv, var_control, var_tv, n_control, n_tv)

# Append mean difference and pvalues
est_df<- round(cbind.data.frame(aux_list_ind[[1]], aux_list_g[[1]], aux_list_tv[[1]]),2)
p_val <- round(cbind.data.frame(aux_list_ind[[2]], aux_list_g[[2]], aux_list_tv[[2]]), 3)

# Paste Dataframes cell to cell
i <- 1
df_means_df <- as.data.frame(apply(est_df, 2, function(x) {
  i<<-i+1
  paste0(x, " (", p_val[,i-1], ")")
}))

# Delete first row
df_means_df <- df_means_df[-1,]

# Rename columns
names(df_means_df) <- c("\\shortstack{Control $-$ \\\\ SM Individual}", "\\shortstack{Control $-$ \\\\SM Group}", "\\shortstack{Control $-$ \\\\TV}")

# Labels for each variable of interest
labels <- c("Age",
"Education (BA)",
"Number of male children",
"Number of female children",
"Other family members",
"Married" ,
"Husband's Age",
"Husband education (BA)",
"Marriage duration with current husband",
"Husband lives at home",
"Before COVID-19 Full time at home",
"Before COVID-19 Partially at home",
"Before COVID-19 Husband full time at home",
"Before COVID-19 Husband partially at home",
"During COVID-19 Full time at home",
"During COVID-19 Partially at home",
"During COVID-19 Husband full time at home",
"During COVID-19 Husband partially at home",
"COVID-19 income decline",
"Watches TV morning",
"Watches TV afternoon",
"Watches TV evening",
"Own TV satellite",
"Watches Channels of TV show",
"Watches TV show type",
"Mentioned watched TV show Saturday evening",
"Hours spent on social media",
"Uses WhatsApp",
"Uses Facebook",
"Uses Instagram",
"Uses Youtube",
"Uses Twitter",
"Uses Snapchat",
"Uses Telegram",
"Watched videos on social media",
"Watched videos on WhatsApp",
"Husband final say",
"Husband earn income",
"Yelling justified",
"Hitting justified",
"Male education priority",
"Future equal say",
"Future equal rights",
"Before COVID-19 heard of or experienced yelling",
"Before COVID-19 heard of or experienced hitting",
"During COVID-19 heard of or experienced yelling",
"During COVID-19 heard of or experienced hitting",
"Would talk husband",
"Would talk family",
"Would report authorities",
"Would use online resources",
"Would contact organization",
"Know online: other than ECWR",
"Know online: ECWR",
"Before COVID-19 used online resources",
"During COVID-19 used online resources",
"Know organization: other than ECWR",
"Know organization: ECWR",
"Before COVID-19 contacted organization",
"During COVID-19 contacted organization")

# Append dataframes means + mean differences
balance_table <- cbind.data.frame(mean_sd_df, df_means_df)

# Assign labels to rownames
rownames(balance_table) <- labels

# Create a table from the above dataframe
balance_table <- xtable(balance_table, 
                  label = "tab:single_balance", 
                  align = "lccccccc",
                  caption = "Balance tests")

# Auxiliary command to add horizontal lines and multicolumns
addtorow <- list(
  pos = list(-1,0, 60), 
  command = c( "\\hline \\multicolumn{1}{l}{} & \\multicolumn{4}{c}{\\shortstack{Treatment group: Mean (s.d.)}} & \\multicolumn{3}{c}{\\shortstack{Mean Differences\\\\ (p-value)}} \\\\ \\cmidrule(l{2pt}r{2pt}){2-5} \\cmidrule(l{2pt}r{2pt}){6-8} ",
               "\\hline ",
               "\\hline "))

# Produce table
balance_table <- print(balance_table, 
                       size="\\scriptsize",
                       table.placement = "H",
                       caption.placement = "top",
                       include.rownames = T,
                       comment = FALSE, 
                       add.to.row =  addtorow,
                       sanitize.colnames.function=function(x){x},
                       hline.after = NULL)

# Save table
cat(alignTable(balance_table, 2.2), file = 'Tables/single_balance.tex')

