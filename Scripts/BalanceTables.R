# Balance Tables 

# Read final dataset
merged <- read.csv('Datasets/Finaldata/Finaldata.csv', fileEncoding="latin1")

# Set the common covariates labels for all tables
covariates <- c("SM Individual", "SM Group", "TV")

ci.level = 0.95

# TableS4: ---------------------------------------------------------------------
demo_data <- merged[c("age", "ba_above", "male_children", "female_children", "sum_family_mem")]

demo_means <- round(colMeans(demo_data), digits = 3)

demo_control <- round(colMeans(demo_data[which(merged$control_10018 == 1),]), digits = 3)

age <- lm(age ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
          weights = weight, data = merged)

ba <- lm(ba_above ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
         weights = weight, data = merged)

male_c <- lm(male_children ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
             weights = weight, data = merged)

female_c <- lm(female_children ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
               weights = weight, data = merged)

sum_fam_mem <- lm(sum_family_mem ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                  weights = weight, data = merged)

dep_var_demo <- c("Age", 
                  "\\shortstack{Education \\\\ (BA)}", 
                  "\\shortstack{Number \\\\ of male \\\\ children}", 
                  "\\shortstack{Number \\\\ of female \\\\ children}", 
                  "\\shortstack{Other \\\\ family \\\\members}")

TableS4A <- stargazer(age, ba, male_c, female_c, sum_fam_mem,
                      header=FALSE,          
                      font.size="scriptsize",
                      label = "tab:B1",
                      #dep.var.caption = "",
                      ci=TRUE, 
                      ci.level= ci.level,
                      report = c("vc*sp"),
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=dep_var_demo,
                      covariate.labels= covariates,
                      omit = c("Constant","block_ids"), 
                      omit.stat=c("f", "ser","adj.rsq"), 
                      add.lines = list(c("Control Mean", demo_control)),
                      column.sep.width = "2pt",
                      title = "Balance on demographics variables",
                      type = "latex")

note.latex <- "\\multicolumn{6}{l} {\\parbox[t]{11cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS4A[grepl("Note", TableS4A)] <- note.latex

# Table S4 Panel B
husb_data <- merged[c("married", "husb_age", "length_marriage", "husb_live_loc_num", "education_husb_num")]

husb_means <- round(colMeans(husb_data, na.rm = TRUE), digits = 3)

husb_control <- round(colMeans(husb_data[which(merged$control_10018 == 1),], na.rm = TRUE), digits = 3)


married <- lm(married ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
              weights = weight, data = merged)

h_age <- lm(husb_age ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
            weights = weight, data = merged)

educ_husb <- lm(education_husb_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                weights = weight, data = merged)

length_marriage <- lm(length_marriage ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                      weights = weight, data = merged)

husb_liv <- lm(husb_live_loc_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
               weights = weight, data = merged)


husb_var <- c("Married", 
              "Age", 
              "\\shortstack{Education \\\\(BA)}",
              "\\shortstack{Marriage \\\\ duration}", 
              "\\shortstack{Husband \\\\ lives \\\\ at home}")


TableS4B <- stargazer(married, h_age, educ_husb, length_marriage, husb_liv, 
                      header=FALSE,          
                      font.size="scriptsize",
                      #dep.var.caption = "",
                      ci=TRUE, 
                      ci.level= ci.level,
                      report = c("vc*sp"),
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=husb_var,
                      covariate.labels= covariates,
                      omit = c("Constant","block_ids"), 
                      omit.stat=c("f", "ser","adj.rsq"), 
                      add.lines = list(c("Control Mean", husb_control)),
                      column.sep.width = "2pt",
                      title = "Balance on husband variables",
                      type = "latex")


note.latex <- "\\multicolumn{6}{l} {\\parbox[t]{12cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS4B[grepl("Note", TableS4B)] <- note.latex


# Table S5 ---------------------------------------------------------------------
covid_data <- merged[c("bcovid_activity_num", "bcovid_activity_ph_num" , 
                       "bcovid_act_husbmar_num","bcovid_activity_husbmar_ph_num",
                       "dcovid_activity_num","dcovid_activity_ph_num",
                       "dcovid_activity_husb_num", "dcovid_activity_husbmar_ph_num",
                       "covid_inc_decline")]

covid_means <- round(colMeans(covid_data, na.rm = TRUE), digits = 3)

covid_control <- round(colMeans(covid_data[which(merged$control_10018 == 1),], na.rm = TRUE), digits = 3)

bcovid_act <- lm(bcovid_activity_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                 weights = weight, data = merged)

bcovid_ph <- lm(bcovid_activity_ph_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                weights = weight, data = merged)

bcovid_act_husb <- lm(bcovid_act_husbmar_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                      weights = weight, data = merged)

bcovid_ph_husb <- lm(bcovid_activity_husbmar_ph_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                     weights = weight, data = merged)

dcovid_act <- lm(dcovid_activity_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                 weights = weight, data = merged)

dcovid_ph <- lm(dcovid_activity_ph_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                weights = weight, data = merged)

dcovid_act_husb <- lm(dcovid_activity_husb_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                      weights = weight, data = merged)

dcovid_ph_husb <- lm(dcovid_activity_husbmar_ph_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                     weights = weight, data = merged)

covid_inc <- lm(covid_inc_decline ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                weights = weight, data = merged)

covid_var <- c("\\shortstack{Full time\\\\ at home}", 
               "\\shortstack{Partially\\\\ at home}", 
               "\\shortstack{Husband \\\\full time \\\\at home}", 
               "\\shortstack{Husband \\\\ partially\\\\ at home}", 
               "\\shortstack{Full time \\\\at home}", 
               "\\shortstack{Partially\\\\ at Home}", 
               "\\shortstack{Husband \\\\full time \\\\at home}", 
               "\\shortstack{Husband \\\\partially\\\\ at home}",
               "\\shortstack{COVID-19 \\\\income \\\\decline}")

TableS5 <- stargazer(bcovid_act, bcovid_ph, bcovid_act_husb, bcovid_ph_husb, 
                     dcovid_act, dcovid_ph, dcovid_act_husb, dcovid_ph_husb, 
                     covid_inc,
                     header=FALSE,          
                     font.size="scriptsize",
                     dep.var.caption = "",
                     label = "tab:B2",
                     ci=TRUE, 
                     ci.level= ci.level,
                     report = c("vc*sp"),
                     dep.var.labels.include = FALSE,
                     table.placement = "H",
                     column.labels=covid_var,
                     covariate.labels= covariates,
                     omit = c("Constant","block_ids"), 
                     omit.stat=c("f", "ser","adj.rsq"), 
                     add.lines = list(c("Control Mean", covid_control)),
                     column.sep.width = "2pt",
                     title = "Balance on before and during COVID-19 home presence of respondent and husband, and whether household income declined with COVID-19",
                     type = "latex")


note.latex <- "\\multicolumn{10}{l} {\\parbox[t]{18cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS5[grepl("Note", TableS5)] <- note.latex

# Table S6 ---------------------------------------------------------------------
TV_data <- merged[c("tv_morning_num", "tv_afternoon_num",  
                    "tv_evening_num", "tv_sattelite_num",
                    "tv_top3_chan_num",
                    "tv_top3_shows_num", "sat_show_num")]

TV_means <- round(colMeans(TV_data), digits = 3)

TV_control <- round(colMeans(TV_data[which(merged$control_10018 == 1),]), digits = 3)

tv_morning <- lm(tv_morning_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                 weights = weight, data = merged)

tv_afternoon <- lm(tv_afternoon_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                   weights = weight, data = merged)

tv_evening <- lm(tv_evening_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                 weights = weight, data = merged)

tv_sattelite <- lm(tv_sattelite_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                   weights = weight, data = merged)

tv_top3_chan <- lm(tv_top3_chan_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                   weights = weight, data = merged)

tv_top3_shows <- lm(tv_top3_shows_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                    weights = weight, data = merged)

sat_show <- lm(sat_show_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
               weights = weight, data = merged)


dep_var_tv <- c("\\shortstack{Watches TV \\\\morning}", 
                "\\shortstack{Watches TV \\\\afternoon}", 
                "\\shortstack{Watches TV \\\\evening}", 
                "\\shortstack{Own TV \\\\satellite}",
                "\\shortstack{Watches Channels \\\\ of TV show}", 
                "\\shortstack{Watches TV\\\\show type}", 
                "\\shortstack{Mentioned \\\\watched TV \\\\show Saturday \\\\evening}")

TableS6 <- stargazer( tv_morning, tv_afternoon, tv_evening, tv_sattelite,
                      tv_top3_chan, tv_top3_shows, sat_show, 
                      header=FALSE,          
                      font.size="scriptsize",
                      dep.var.caption = "",
                      label = "tab:B3",
                      ci=TRUE, 
                      ci.level= ci.level,
                      report = c("vc*sp"),
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=dep_var_tv,
                      covariate.labels= covariates,
                      omit = c("Constant","block_ids"), 
                      omit.stat=c("f", "ser","adj.rsq"), 
                      add.lines = list(c("Control Mean",TV_control)),
                      column.sep.width = "2pt",
                      title = "Balance on TV show consumption variables",
                      type = "latex")


note.latex <- "\\multicolumn{8}{l} {\\parbox[t]{16cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS6[grepl("Note", TableS6)] <- note.latex


# Table S7 ---------------------------------------------------------------------
dep_var_Socmed <- c("\\shortstack{Hours spent \\\\ on social \\\\ media}", 
                    "\\shortstack{Uses \\\\ WhatsApp}", 
                    "\\shortstack{Uses \\\\ Facebook}", 
                    "\\shortstack{Uses \\\\ Instagram}", 
                    "\\shortstack{Uses \\\\ YouTube}", 
                    "\\shortstack{Uses \\\\ Twitter}", 
                    "\\shortstack{Uses \\\\ Snapchat}", 
                    "\\shortstack{Uses \\\\ Telegram}",
                    "\\shortstack{Watched \\\\videos on\\\\social media}", 
                    "\\shortstack{Watched \\\\ videos on \\\\ WhatsApp}")

dep_var_sm_data <- merged[c("hours_soc_med_num", "socmed_whatsapp", "socmed_Facebook", "socmed_Instagram",
                            "socmed_YouTube", "socmed_Twitter", "socmed_Snapchat", "socmed_Telegram",
                            "X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num")]


sm_means<- round(colMeans(dep_var_sm_data), digits = 3)
sm_control <- round(colMeans(dep_var_sm_data[which(merged$control_10018 == 1),]), digits = 3)


hours <- lm(hours_soc_med_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
            weights = weight, data = merged)

whats <- lm(socmed_whatsapp ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
            weights = weight, data = merged)

fb <- lm(socmed_Facebook ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
         weights = weight, data = merged)

instagram <- lm(socmed_Instagram ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                weights = weight, data = merged)

youtube <- lm(socmed_YouTube ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
              weights = weight, data = merged)

twitter <- lm(socmed_Twitter ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
              weights = weight, data = merged)

snapchat <- lm(socmed_Snapchat ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
               weights = weight, data = merged)

telegram <- lm(socmed_Telegram ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
               weights = weight, data = merged)

socmed_dv <- lm(merged$'X2mos_socmed_dv_num' ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                weights = weight, data = merged)

whatsapp_dv <- lm(merged$'X2mos_whatsapp_dv_num' ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                  weights = weight, data = merged)

TableS7 <- stargazer(hours, whats, fb, instagram, youtube, twitter, snapchat, telegram, socmed_dv, whatsapp_dv,
                     header=FALSE,          
                     font.size="scriptsize",
                     dep.var.caption = "",
                     dep.var.labels.include = FALSE,
                     label = "tab:B4",
                     ci=TRUE, 
                     ci.level= ci.level,
                     report = c("vc*sp"),
                     table.placement = "H",
                     column.labels=dep_var_Socmed,
                     covariate.labels= covariates,
                     omit = c("Constant","block_ids"), 
                     omit.stat=c("f", "ser","adj.rsq"), 
                     add.lines = list(c("Control Mean", sm_control)),
                     column.sep.width = "-2pt",
                     title = "Balance on social media habits and videos received variables",
                     type = "latex")

note.latex <- "\\multicolumn{11}{l} {\\parbox[t]{20cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. 
* denotes P$<$0.1, ** denotes P$<$0.05, and *** denotes P$<$0.01.}}  \\\\"
TableS7[grepl("Note", TableS7)] <- note.latex

# Table S8 ---------------------------------------------------------------------
dep_var_At <- c("\\shortstack{Husband \\\\final say}", 
                "\\shortstack{Husband \\\\earn income}", 
                "\\shortstack{Yelling \\\\justified}", 
                "\\shortstack{Hitting \\\\justified}", 
                "\\shortstack{Male education\\\\priority}", 
                "\\shortstack{Future \\\\equal say}", 
                "\\shortstack{Future \\\\equal rights}")

attitude_data <- merged[c("husb_final_say_num", "husb_provide_inc_num",
                          "husb_justified_yell_num", "husb_justified_beat_num", "prioritize_educ_num", 
                          "future_equal_say_num", "future_equal_rights_num")]

attitude_means <- round(colMeans(attitude_data), digits = 3)
at_control <- round(colMeans(attitude_data[which(merged$control_10018 == 1),]), digits = 3)


husb_final_say <- lm(husb_final_say_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                     weights = weight, data = merged)

husb_provide_inc <- lm(husb_provide_inc_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                       weights = weight, data = merged)

husb_justified_yell <- lm(husb_justified_yell_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                          weights = weight, data = merged)

husb_justified_beat<- lm(husb_justified_beat_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                         weights = weight, data = merged)

prioritize_educ <- lm(prioritize_educ_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                      weights = weight, data = merged)

future_equal_say <- lm(future_equal_say_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                       weights = weight, data = merged)

future_equal_rights <- lm(future_equal_rights_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                          weights = weight, data = merged)

TableS8 <- stargazer(husb_final_say, husb_provide_inc, husb_justified_yell, 
                     husb_justified_beat, prioritize_educ, future_equal_say, future_equal_rights,
                     header=FALSE,          
                     font.size="scriptsize",
                     dep.var.caption = "",
                     label = "tab:B5",
                     ci=TRUE, 
                     ci.level= ci.level,
                     report = c("vc*sp"),
                     dep.var.labels.include = FALSE,
                     table.placement = "H",
                     column.labels=dep_var_At,
                     covariate.labels= covariates,
                     omit = c("Constant","block_ids"), 
                     omit.stat=c("f", "ser","adj.rsq"), 
                     add.lines = list(c("Control Mean",at_control)),
                     column.sep.width = "2pt",
                     title = "Balance on attitudes toward gender and marital equality",
                     type = "latex")

note.latex <- "\\multicolumn{8}{l} {\\parbox[t]{18cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. * denotes P$<$0.1, ** denotes P$<$0.05, and *** denotes P$<$0.01.}} \\\\"
TableS8[grepl("Note", TableS8)] <- note.latex


# Table S9 ---------------------------------------------------------------------
rep_data <- merged[c("bc19_yell_num","bc19_hit_num",
                     "dc19_yell_num", "dc19_hit_num")]

rep_means <- round(colMeans(rep_data), digits = 3)

rep_control <- round(colMeans(rep_data[which(merged$control_10018 == 1),]), digits = 3)


bc_yell <- lm(bc19_yell_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
              weights = weight, data = merged)

bc_hit <- lm(bc19_hit_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
             weights = weight, data = merged)

dc_yell <- lm(dc19_yell_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
              weights = weight, data = merged)

dc_hit <- lm(dc19_hit_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
             weights = weight, data = merged)


dep_var_re <- c("\\shortstack{Heard of or \\\\ experienced yelling}", 
                "\\shortstack{Heard of or \\\\ experienced hitting}", 
                "\\shortstack{Heard of or \\\\ experienced yelling}", 
                "\\shortstack{Heard of or \\\\ experienced hitting}")

TableS9 <- stargazer(bc_yell, bc_hit, dc_yell, dc_hit,
                      header=FALSE,          
                      font.size="scriptsize",
                      dep.var.caption = "",
                     label = "tab:B6",
                     ci=TRUE, 
                     ci.level= ci.level,
                     report = c("vc*sp"),
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=dep_var_re,
                      covariate.labels= covariates,
                      omit = c("Constant","block_ids"), 
                      omit.stat=c("f", "ser","adj.rsq"), 
                      add.lines = list(c("Control Mean",rep_control)),
                      column.sep.width = "2pt",
                      title = "Balance on domestic violence experienced before and during COVID-19",
                      type = "latex")

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{15cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. * denotes P$<$0.1, ** denotes P$<$0.05, and *** denotes P$<$0.01.}} \\\\"
TableS9[grepl("Note", TableS9)] <- note.latex


# Table S10 ---------------------------------------------------------------------
scenarios_data <- merged[c("talk_husband_num","talk_family_num","report_authorities_num",
                           "look_online_num","contact_org_num")]

scenarios_means <- round(colMeans(scenarios_data), digits = 3)
sce_control <- round(colMeans(scenarios_data[which(merged$control_10018 == 1),]), digits = 3)


talk_husband <- lm(talk_husband_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                   weights = weight, data = merged)

talk_family <- lm(talk_family_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                  weights = weight, data = merged)

report_authorities <- lm(report_authorities_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                         weights = weight, data = merged)

look_online <- lm(look_online_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                  weights = weight, data = merged)

contact_org <- lm(contact_org_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                  weights = weight, data = merged)

dep_var_sc <- c("\\shortstack{Would talk \\ husband}", 
                "\\shortstack{Would Talk \\ family}", 
                "\\shortstack{Would report \\\\ authorities}", 
                "\\shortstack{Would use \\\\ online resources}", 
                "\\shortstack{Would contact \\\\ organization}")

TableS10 <- stargazer(talk_husband, talk_family, report_authorities, look_online, 
                      contact_org, 
                      header=FALSE,          
                      label = "tab:B7",
                      font.size="scriptsize",
                      dep.var.caption = "",
                      ci=TRUE, 
                      ci.level= ci.level,
                      report = c("vc*sp"),
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=dep_var_sc,
                      covariate.labels= covariates,
                      omit = c("Constant","block_ids"), 
                      omit.stat=c("f", "ser","adj.rsq"), 
                      add.lines = list(c("Control Mean", sce_control)),
                      column.sep.width = "2pt",
                      title = "Balance on hypothetical talking to husband and family members, reporting to authorities, use of online resources, 
                          and contact with an organization when responding to domestic violence",
                      type = "latex")

note.latex <- "\\multicolumn{6}{l} {\\parbox[t]{17cm}{ \\textit{Notes:}  
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS10[grepl("Note", TableS10)] <- note.latex


# Table S11 ---------------------------------------------------------------------
know_data <- merged[c( "know_online_valid_noaut_num", "know_online_nehad_num",
                       "bc19_look_online_num", "dc19_look_online_num", 
                       "know_org_noaut_valid_num", "know_org_nehad_num",
                       "bc19_look_org_num", "dc19_look_org_num")]

know_control <- round(colMeans(know_data[which(merged$control_10018 == 1),]), digits = 3)

known_online <- lm(know_online_valid_noaut_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                   weights = weight, data = merged)

known_online_nehad <- lm(know_online_nehad_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                         weights = weight, data = merged)

bc_look_online <- lm(bc19_look_online_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                     weights = weight, data = merged)

dc_look_online <- lm(dc19_look_online_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                     weights = weight, data = merged)

know_org <- lm(know_org_noaut_valid_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
               weights = weight, data = merged)

know_org_nehad <- lm(know_org_nehad_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                     weights = weight, data = merged)

bc_look_org<- lm(bc19_look_org_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                 weights = weight, data = merged)

dc_look_org <- lm(dc19_look_org_num ~ pooledF_WI + in_group_10018 +reminder_10018 + factor(block_ids),
                  weights = weight, data = merged)

dep_var_know <- c("\\shortstack{Know online:\\\\ other than \\\\ECWR}",
                  "\\shortstack{Know online: \\\\ECWR}",
                  "\\shortstack{Before \\\\COVID-19 \\\\used online \\\\resources}", 
                  "\\shortstack{During \\\\COVID-19 \\\\used online \\\\resources}", 
                  "\\shortstack{Know \\\\organization:\\\\other than \\\\ECWR}",
                  "\\shortstack{Know \\\\organization:\\\\ECWR}",
                  "\\shortstack{Before \\\\COVID-19 \\\\contacted \\\\organization}",
                  "\\shortstack{During \\\\COVID-19 \\\\contacted \\\\organization}")

TableS11 <- stargazer(known_online, known_online_nehad,bc_look_online, dc_look_online, 
                      know_org, know_org_nehad, bc_look_org, dc_look_org,
                      header=FALSE,        
                      font.size="scriptsize",
                      dep.var.caption = "",
                      label = "tab:B8",
                      ci=TRUE, 
                      ci.level= ci.level,
                      report = c("vc*sp"),
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=dep_var_know,
                      covariate.labels= covariates,
                      omit = c("Constant","block_ids"), 
                      omit.stat=c("f", "ser","adj.rsq"), 
                      add.lines = list(c("Control Mean", know_control)),
                      column.sep.width = "0pt",
                      title = "Balance on knowledge and experience of accessing resources for women",
                      type = "latex")

note.latex <- "\\multicolumn{9}{l} {\\parbox[t]{20cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment assignment, 
including randomization block fixed effects. 95\\% confidence intervals are in parenthesis (due to two-sided t-tests). 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS11[grepl("Note", TableS11)] <- note.latex


# Functions for headers
headerTableS5 <- function(table) {
  
  table <- paste(table, collapse = "")
  
  aux1 <- sub("\\]  &.*", 
              "] &\\\\multicolumn\\{4\\}\\{c\\}\\{Before COVID-19\\} & \\\\multicolumn\\{4\\}\\{c\\}\\{During COVID-19\\} \\\\\\\\ \\\\cmidrule(lr)\\{2-5\\} \\\\cmidrule(lr)\\{6-9\\}", table)
  
  aux2 <- sub(".*? & \\\\shortstack\\{", "& \\\\shortstack\\{", table )
  
  final <- paste(aux1, aux2, collapse = "")
  
  return(final)
}

headerTableS9 <- function(table) {
  
  table <- paste(table, collapse = "")
  
  aux1 <- sub("\\]  &.*", 
              "] &\\\\multicolumn\\{2\\}\\{c\\}\\{Before COVID-19\\} & \\\\multicolumn\\{2\\}\\{c\\}\\{During COVID-19\\} \\\\\\\\ \\\\cmidrule(lr)\\{2-3\\} \\\\cmidrule(lr)\\{4-5\\}", table)
  
  aux2 <- sub(".*? & \\\\shortstack\\{", "& \\\\shortstack\\{", table )
  
  
  final <- paste(aux1, aux2, collapse = "")
  
  return(final)
}


# Save tables
cat(Panel_Balance(TableS4A,TableS4B, 6),"\n", file = 'Tables/CI_pvals/S4.tex')
cat(alignTable(headerTableS5(TableS5), 2.25), "\n", file = 'Tables/CI_pvals/S5.tex')
cat(alignTable(TableS6, 1), "\n", file = 'Tables/CI_pvals/S6.tex')
cat(alignTable(TableS7, 2.6), "\n", file = 'Tables/CI_pvals/S7.tex')
cat(alignTable(TableS8, 1),"\n", file = 'Tables/CI_pvals/S8.tex')
cat(headerTableS9(TableS9),"\n", file = 'Tables/CI_pvals/S9.tex')
cat(TableS10,"\n", file = 'Tables/CI_pvals/S10.tex')
cat(alignTable(TableS11, 2), "\n", file = 'Tables/CI_pvals/S11.tex')

