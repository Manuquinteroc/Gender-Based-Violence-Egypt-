# First Stage Analysis

# Read final dataset
merged <- read.csv('Datasets/Finaldata/Finaldata.csv')

# Set the common covariates labels for all tables
covariates <- c("SM Individual", "SM Group", "TV")

# First stage for TV show 
FS_TV_data <- merged[c("tv_evening_num_end", "three_viewed_channel_end_num",
                       "three_viewed_shows_end_num", "tv_sat_2mos_end_num", "watch_hekayat_end_num",
                       "heard_hekayat_end_num", "how_heard_hekayat_num",
                       "whatsapp_remind_2mos_end_num", "wheter_episodes_hekayat_end_num" , "episodes_hekayat_end_num",
                       "episodes_content_end_num_vague", "episode_content_like_end_num_vague")]

TV_control_mean <- round(colMeans(FS_TV_data[which(merged$control_10018 == 1),]), digits = 3)

# Calculate the z-score
zscore <- scale(rowMeans(scale(FS_TV_data) %*% diag(c(1,1,1,1,1,1,1,1,1,1,1,1))))

# Save Zscore for Heterogeneous Effects
merged$FS_TV_zscore <- zscore

# Baseline z-score
controls <- c("tv_evening_num", "tv_sattelite_num", "tv_top3_chan_num", "tv_top3_shows_num", "sat_show_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1,1,1,1)))
merged$FS_TV_zscore_base <- zscore_base

# Missing 3: saturday and the last two
TV_dep_var <- c("\\shortstack{Index of \\\\ (1,1,1,1,1,1,\\\\1,1,1,1,1,1)}",
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


# Panel A: All family controls and demographics
# Missing 3: saturday and the last two
tv_0_2 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_1_2 <-lm(tv_evening_num_end ~ pooledF_WI + in_group_10018  + reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_2_2 <-lm(three_viewed_channel_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_3_2 <-lm(three_viewed_shows_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_4_2 <-lm(tv_sat_2mos_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_5_2 <-lm(watch_hekayat_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_6_2 <-lm(heard_hekayat_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_7_2 <-lm(how_heard_hekayat_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_8_2 <-lm(whatsapp_remind_2mos_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_9_2 <-lm(wheter_episodes_hekayat_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
              tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_10_2 <-lm(episodes_hekayat_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
               tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
               age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight,
             data = merged)

tv_11_2 <-lm(episodes_content_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
               tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
               age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight,
             data = merged)

tv_12_2 <-lm(episode_content_like_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
               tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
               age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight,
             data = merged)


model_names = c("tv_0_2","tv_1_2", "tv_2_2", "tv_3_2", "tv_4_2", "tv_5_2", 
                "tv_6_2", "tv_7_2", "tv_8_2", "tv_9_2", "tv_10_2", "tv_11_2", "tv_12_2")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("Constant","block_ids", "tv_top3_chan_num", "tv_evening_num", "tv_top3_shows_num",
              "tv_sattelite_num", "sat_show_num", "age", "educ_aboveBA", "married",
              "FS_TV_zscore_base")

TableS15A <- stargazer(tv_0_2, tv_1_2, tv_2_2, tv_3_2, tv_4_2, tv_5_2,
                       tv_6_2, tv_7_2, tv_8_2, tv_9_2, tv_10_2, tv_11_2, tv_12_2,
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,
                       font.size="scriptsize",
                       dep.var.labels.include = FALSE,
                       table.placement = "t",
                       column.labels=TV_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var,
                       omit.stat=c("f", "ser","adj.rsq"),
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on TV show consumption",
                       type = "latex")


# Panel B: Lagged DV
tv_0_2 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + FS_TV_zscore_base + 
              factor(block_ids),
            weights = weight,
            data = merged)

tv_1_2 <-lm(tv_evening_num_end ~ pooledF_WI + in_group_10018  + reminder_10018 +
              tv_evening_num +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_2_2 <-lm(three_viewed_channel_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
              tv_top3_chan_num +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_3_2 <-lm(three_viewed_shows_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              tv_top3_shows_num +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_4_2 <-lm(tv_sat_2mos_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              sat_show_num + 
              factor(block_ids),
            weights = weight,
            data = merged)

tv_5_2 <-lm(watch_hekayat_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_6_2 <-lm(heard_hekayat_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_7_2 <-lm(how_heard_hekayat_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_8_2 <-lm(whatsapp_remind_2mos_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_9_2 <-lm(wheter_episodes_hekayat_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_10_2 <-lm(episodes_hekayat_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
               factor(block_ids),
             weights = weight,
             data = merged)

tv_11_2 <-lm(episodes_content_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
               factor(block_ids),
             weights = weight,
             data = merged)

tv_12_2 <-lm(episode_content_like_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
               factor(block_ids),
             weights = weight,
             data = merged)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}


TableS15B <- stargazer(tv_0_2, tv_1_2, tv_2_2, tv_3_2, tv_4_2, tv_5_2,
                       tv_6_2, tv_7_2, tv_8_2, tv_9_2, tv_10_2, tv_11_2, tv_12_2,
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,
                       font.size="scriptsize",
                       dep.var.labels.include = FALSE,
                       table.placement = "t",
                       column.labels=TV_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var,
                       omit.stat=c("f", "ser","adj.rsq", "n"),
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on TV show consumption",
                       type = "latex")



# Panel C: No controls
tv_0_2 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_1_2 <-lm(tv_evening_num_end ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_2_2 <-lm(three_viewed_channel_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_3_2 <-lm(three_viewed_shows_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_4_2 <-lm(tv_sat_2mos_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_5_2 <-lm(watch_hekayat_end_num ~ pooledF_WI + in_group_10018+ reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_6_2 <-lm(heard_hekayat_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_7_2 <-lm(how_heard_hekayat_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_8_2 <-lm(whatsapp_remind_2mos_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_9_2 <-lm(wheter_episodes_hekayat_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

tv_10_2 <-lm(episodes_hekayat_end_num ~ pooledF_WI + in_group_10018 + reminder_10018 +
               factor(block_ids),
             weights = weight,
             data = merged)

tv_11_2 <-lm(episodes_content_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
               factor(block_ids),
             weights = weight,
             data = merged)

tv_12_2 <-lm(episode_content_like_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
               factor(block_ids),
             weights = weight,
             data = merged)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}


TableS15C <- stargazer(tv_0_2, tv_1_2, tv_2_2, tv_3_2, tv_4_2, tv_5_2,
                       tv_6_2, tv_7_2, tv_8_2, tv_9_2, tv_10_2, tv_11_2, tv_12_2,
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,
                       font.size="scriptsize",
                       dep.var.labels.include = FALSE,
                       table.placement = "t",
                       column.labels=TV_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var,
                       omit.stat=c("f", "ser","adj.rsq"),
                       add.lines = list(c("Control Mean", 0, TV_control_mean),
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on TV show consumption",
                       type = "latex")

note.latex <- "\\multicolumn{14}{l} {\\parbox[t]{26cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic controls and all baseline covariates in the outcome family: 
Watches TV evening, Owns TV satellite, Watches Channels of TV show, 
Watches TV show type, and Mentioned watched TV show Saturday evening. 
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS15C[grepl("Note", TableS15C)] <- note.latex

cat(Panel_3(TableS15A, TableS15B, TableS15C, "\\\\shortstack\\{Index", 14, 2.5), file = 'Tables/S15.tex')

#  First stage for Facebook and  WhatsApp Treatment ----------------------------
FS_fw_data <- merged[c("videos_socmed_2mos_end_num", "videos_whatsapp_2mos_end_num",
                       "videos_whatsapp_receive_end_num", "videos_watched_end_num",
                       "videos_watched_no_end_num", "videos_watched_content_end_num_vague", 
                       "video_content_like_end_num_vague")]

FS_fw_control_mean <- round(colMeans(FS_fw_data[which(merged$control_10018 == 1),]), digits = 3)

# Calculate the z-score
zscore <- scale(rowMeans(scale(FS_fw_data) %*% diag(c(1,1,1,1,1,1,1))))
merged$FS_FW_zscore <- zscore

# Baseline z-score
controls <- c("X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1)))
merged$FS_FW_zscore_base <- zscore_base

dep_var <- c("\\shortstack{Index of \\\\ (1,1,1,1,1,1,1)}",
             "\\shortstack{Watched \\\\videos on \\\\social media}",
             "\\shortstack{Watched \\\\videos on\\\\ WhatsApp}",
             "\\shortstack{Received \\\\videos on \\\\WhatsApp or \\\\Facebook}",
             "\\shortstack{Watched \\\\videos on \\\\WhatsApp or \\\\Facebook}",
             "\\shortstack{Number of \\\\videos watched}",
             "\\shortstack{Accurate \\\\content of \\\\the videos}",
             "\\shortstack{Accurate \\\\video topic \\\\liked}")

# Panel A: All family controls
lm_0_2 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + age + educ_aboveBA + married +factor(block_ids),
            weights = weight,
            data = merged)

lm_1_2 <-lm(videos_socmed_2mos_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + age + educ_aboveBA + married +factor(block_ids),
            weights = weight,
            data = merged)

lm_2_2 <-lm(videos_whatsapp_2mos_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + age + educ_aboveBA + married +factor(block_ids),
            weights = weight,
            data = merged)

lm_3_2 <-lm(videos_whatsapp_receive_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + age + educ_aboveBA + married +factor(block_ids),
            weights = weight,
            data = merged)

lm_4_2 <-lm(videos_watched_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + age + educ_aboveBA + married +factor(block_ids),
            weights = weight,
            data = merged)

lm_5_2 <-lm(videos_watched_no_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + age + educ_aboveBA + married +factor(block_ids),
            weights = weight,
            data = merged)

lm_6_2 <-lm(videos_watched_content_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + age + educ_aboveBA + married +factor(block_ids),
            weights = weight,
            data = merged)

lm_7_2 <-lm(video_content_like_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + age + educ_aboveBA + married +factor(block_ids),
            weights = weight,
            data = merged)

model_names = c("lm_0_2","lm_1_2", "lm_2_2", "lm_3_2", "lm_4_2", "lm_5_2", "lm_6_2", "lm_7_2")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "X2mos_socmed_dv_num" ,"X2mos_whatsapp_dv_num",
              "FS_FW_zscore_base")

TableS16A <- stargazer(lm_0_2, lm_1_2, lm_2_2, lm_3_2, lm_4_2, lm_5_2, lm_6_2, lm_7_2,
                       header=FALSE,
                       font.size="scriptsize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var,
                       covariate.labels= covariates,
                       omit = omit_var,
                       omit.stat=c("f", "ser","adj.rsq"),
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "2pt",
                       title = "Treatment effect on videos of women's empowerment and support consumption",
                       type = "latex")

# Panel B: Lagged DV
lm_0_2 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + FS_FW_zscore_base + 
              factor(block_ids),
            weights = weight,
            data = merged)

lm_1_2 <-lm(videos_socmed_2mos_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_socmed_dv_num +  factor(block_ids),
            weights = weight,
            data = merged)

lm_2_2 <-lm(videos_whatsapp_2mos_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              X2mos_whatsapp_dv_num + factor(block_ids),
            weights = weight,
            data = merged)

lm_3_2 <-lm(videos_whatsapp_receive_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_4_2 <-lm(videos_watched_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_5_2 <-lm(videos_watched_no_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_6_2 <-lm(videos_watched_content_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_7_2 <-lm(video_content_like_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS16B <- stargazer(lm_0_2, lm_1_2, lm_2_2, lm_3_2, lm_4_2, lm_5_2, lm_6_2, lm_7_2,
                       header=FALSE,
                       font.size="scriptsize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var,
                       covariate.labels= covariates,
                       omit = omit_var,
                       omit.stat=c( "n", "f", "ser","adj.rsq"),
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "2pt",
                       title = "Treatment effect on videos of women's empowerment and support consumption",
                       type = "latex")

# Panel C
lm_0_2 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_1_2 <-lm(videos_socmed_2mos_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_2_2 <-lm(videos_whatsapp_2mos_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
            weights = weight,
            data = merged)

lm_3_2 <-lm(videos_whatsapp_receive_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_4_2 <-lm(videos_watched_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_5_2 <-lm(videos_watched_no_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_6_2 <-lm(videos_watched_content_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

lm_7_2 <-lm(video_content_like_end_num_vague ~ pooledF_WI + in_group_10018  + reminder_10018 +
              factor(block_ids),
            weights = weight,
            data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS16C <- stargazer(lm_0_2, lm_1_2, lm_2_2, lm_3_2, lm_4_2, lm_5_2, lm_6_2, lm_7_2,
                       header=FALSE,
                       font.size="scriptsize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var,
                       covariate.labels= covariates,
                       omit = omit_var,
                       omit.stat=c("f", "ser","adj.rsq"),
                       add.lines = list(c("Control Mean", 0, FS_fw_control_mean), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "2pt",
                       title = "Treatment effect on videos of women's empowerment and support consumption",
                       type = "latex")

note.latex <- "\\multicolumn{9}{l} {\\parbox[t]{19.5cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
RRegressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Social media videos received and WhatsApp videos received.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS16C[grepl("Note", TableS16C)] <- note.latex

cat(Panel_3(TableS16A, TableS16B, TableS16C, "\\\\shortstack\\{Index", 9, 1.75), file = 'Tables/S16.tex')


# Reduced Form -----------------------------------------------------------------
# Knowledge of online resources and organizations  --------------------------------------------------------------
knowledge2 <- merged[c("online_dvsa_noaut_end_num","online_dvsa_nehad_end_num", "org_dvsa_noaut_end_num", "org_dvsa_nehad_end_num")]
know2_mean <- round(colMeans(knowledge2[which(merged$control_10018 == 1),]), digits = 3)
zscore <- scale(rowMeans(scale(knowledge2) %*% diag(c(1, 1, 1, 1))))
merged$RF_knowledge2_zscore <- zscore

# Baseline z-score
controls <- c("know_online_valid_noaut_num", "know_org_noaut_valid_num", "know_online_nehad_num", "know_org_nehad_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1,1,1)))
merged$RF_knowledge2_zscore_base <- zscore_base

know_during_var <- c("\\shortstack{Index of \\\\ (1,1,1,1)}", 
                     "\\shortstack{Know online:\\\\ other than ECWR}",
                     "\\shortstack{Know online:\\\\ ECWR}",
                     "\\shortstack{Know organization:\\\\ other than ECWR}",
                     "\\shortstack{Know organization:\\\\ ECWR}")

# All covariates
know_1 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
              know_org_noaut_valid_num + know_online_nehad_num + know_org_nehad_num +age + educ_aboveBA + married +
              factor(block_ids), 
            weights = weight, 
            data = merged)

know_2 <-lm(online_dvsa_noaut_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
              know_org_noaut_valid_num + know_online_nehad_num + know_org_nehad_num + age + educ_aboveBA + married +
              factor(block_ids), 
            weights = weight, 
            data = merged)

know_3 <-lm(online_dvsa_nehad_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
              know_org_noaut_valid_num +  know_online_nehad_num + know_org_nehad_num + age + educ_aboveBA + married +
              factor(block_ids), 
            weights = weight, 
            data = merged)

know_4 <-lm(org_dvsa_noaut_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
              know_org_noaut_valid_num +  know_online_nehad_num + know_org_nehad_num + age + educ_aboveBA + married +
              factor(block_ids), 
            weights = weight, 
            data = merged)

know_5 <-lm(org_dvsa_nehad_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
              know_org_noaut_valid_num +  know_online_nehad_num + know_org_nehad_num + age + educ_aboveBA + married +
              factor(block_ids), 
            weights = weight, 
            data = merged)

model_names = c("know_1", "know_2", "know_3", "know_4", "know_5")
list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "know_online_valid_num", "dc19_look_online_num", "bc19_look_online_num", "bc19_look_org_num",
              "dc19_look_org_num", "dcovid_accessonline_end_num_refuse", "dcovid_contactorg_end_num_refuse", "know_org_valid_num",
              "know_online_valid_noaut_num", "know_org_noaut_valid_num", "know_online_nehad_num", "know_org_nehad_num",
              "RF_knowledge2_zscore_base")

TableS17A <- stargazer(know_1, know_2, know_3, know_4, know_5,
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_during_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on knowledge about treatment information",
                       type = "latex")


# Lagged DV
know_1 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + RF_knowledge2_zscore_base +
              factor(block_ids), 
            weights = weight, 
            data = merged)


know_2 <-lm(online_dvsa_noaut_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_online_valid_noaut_num +
              factor(block_ids), 
            weights = weight, 
            data = merged)


know_3 <-lm(online_dvsa_nehad_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_online_nehad_num +
              factor(block_ids), 
            weights = weight, 
            data = merged)

know_4 <-lm(org_dvsa_noaut_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_org_noaut_valid_num +
              factor(block_ids), 
            weights = weight, 
            data = merged)


know_5 <-lm(org_dvsa_nehad_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_org_nehad_num +
              factor(block_ids), 
            weights = weight, 
            data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS17B <- stargazer(know_1, know_2, know_3, know_4, know_5,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_during_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("Control Mean", 0, know2_mean),
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on knowledge about treatment information",
                       type = "latex")

# Panel C: No controls
know_1 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              factor(block_ids), 
            weights = weight, 
            data = merged)


know_2 <-lm(online_dvsa_noaut_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              factor(block_ids), 
            weights = weight, 
            data = merged)


know_3 <-lm(online_dvsa_nehad_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              factor(block_ids), 
            weights = weight, 
            data = merged)

know_4 <-lm(org_dvsa_noaut_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              factor(block_ids), 
            weights = weight, 
            data = merged)


know_5 <-lm(org_dvsa_nehad_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              factor(block_ids), 
            weights = weight, 
            data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS17C <- stargazer(know_1, know_2, know_3, know_4, know_5,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_during_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, know2_mean),
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on knowledge about treatment information",
                       type = "latex")

note.latex <- "\\multicolumn{6}{l} {\\parbox[t]{18cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Know online: other than ECWR, Know online: ECWR, Before COVID-19 used online resources, 
During COVID-19 used online resources, Know organization: other than ECWR, Know organization: ECWR
Before COVID-19 contacted organization, and During COVID-19 contacted organization.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS17C[grepl("Note", TableS17C)] <- note.latex

cat(Panel_3(TableS17A, TableS17B, TableS17C, "\\\\shortstack\\{Index", 6, 1, 1.5), file = 'Tables/S17.tex')

# Attitudes towards female empowerment, husband role, and women role in the workplace (domestic related) -----
husb_work_att <- merged[c("husb_final_say_end_num","husb_inc_end_num", "husb_just_yell_end_num",
                          "woman_work_outside_end_num", "female_circ_marriage_end_num", "circ_tf_end_num",
                          "marriage_age_tf_end_num", "khul_divorce_tf_end_num")]

husb_work_att_mean <- round(colMeans(husb_work_att[which(merged$control_10018 == 1),]), digits = 3)

# Calculate the z-score
zscore <- scale(rowMeans(scale(husb_work_att) %*% diag(c(-1,-1,-1,1,-1,-1,-1,1))))
merged$RF_att1_zscore <- zscore

# Baseline z-score
controls <- c("husb_final_say_num", "husb_provide_inc_num", "husb_justified_yell_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(-1,-1,-1)))
merged$RF_att1_zscore_base <- zscore_base

att1_dep_var <- c("\\shortstack{Index of \\\\ (-1,-1,-1,1, \\\\ -1,-1,-1,1)}", 
                  "Husband final say", 
                  "Husband earn income", 
                  "Yelling justified", 
                  "\\shortstack{Gain  \\\\ independence \\\\ by working\\\\ outside the\\\\  household}",
                  "\\shortstack{Circumcision \\\\ important \\\\ for women \\\\ marriage}", 
                  "\\shortstack{Female \\\\circumcision \\\\ health \\\\benefits}",
                  "\\shortstack{Marriage\\\\ permitted \\\\ under age 18 with \\\\family consent}",
                  "\\shortstack{Khul: \\\\ Women can \\\\divorce husband \\\\withouth \\\\a reason}")


# All covariates 
domestic_z <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids),
                 weights = weight, data = merged)


domestic_1 <- lm(husb_final_say_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids),
                 weights = weight, data = merged)


domestic_2 <- lm(husb_inc_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids),
                 weights = weight, data = merged)


domestic_3 <- lm(husb_just_yell_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_4 <- lm(woman_work_outside_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_5 <- lm(female_circ_marriage_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

domestic_6 <- lm(circ_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_7 <- lm(marriage_age_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids),
                 weights = weight,  data = merged)

domestic_8 <- lm(khul_divorce_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
                   husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
                   factor(block_ids), 
                 weights = weight,  data = merged)

model_names = c("domestic_z", "domestic_1", "domestic_2", "domestic_3", "domestic_4", "domestic_5",
                "domestic_6", "domestic_7", "domestic_8")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "husb_final_say_num", "husb_provide_inc_num", 
              "husb_justified_yell_num", "prioritize_educ_num", "future_equal_say_num",
              "future_equal_rights_num", "husb_justified_beat_num", "RF_att1_zscore_base")


TableS18A <- stargazer(domestic_z, domestic_1, domestic_2, domestic_3, domestic_4,
                       domestic_5, domestic_6, domestic_7, domestic_8,
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,
                       font.size="scriptsize",
                       dep.var.labels.include = FALSE,
                       table.placement = "t",
                       column.labels=att1_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n","f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "2pt",
                       title = "Treatment effects on attitudes towards gender and marital equality",
                       type = "latex")

# Lagged DV
domestic_z <- lm(zscore ~ pooledF_WI + in_group_10018 + reminder_10018 + RF_att1_zscore_base +
                   factor(block_ids),
                 weights = weight, data = merged)


domestic_1 <- lm(husb_final_say_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_final_say_num +
                   factor(block_ids),
                 weights = weight, data = merged)


domestic_2 <- lm(husb_inc_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_provide_inc_num + 
                   factor(block_ids),
                 weights = weight, data = merged)


domestic_3 <- lm(husb_just_yell_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   husb_justified_yell_num + 
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_4 <- lm(woman_work_outside_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_5 <- lm(female_circ_marriage_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

domestic_6 <- lm(circ_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

domestic_7 <- lm(marriage_age_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

domestic_8 <- lm(khul_divorce_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}


TableS18B <- stargazer(domestic_z, domestic_1, domestic_2, domestic_3, domestic_4,
                       domestic_5, domestic_6, domestic_7, domestic_8,
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,
                       font.size="scriptsize",       
                       dep.var.labels.include = FALSE,
                       table.placement = "t",
                       column.labels=att1_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "2pt",
                       title = "Treatment effects on attitudes towards gender and marital equality",
                       type = "latex")

# Panel C
domestic_z <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_1 <- lm(husb_final_say_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_2 <- lm(husb_inc_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_3 <- lm(husb_just_yell_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_4 <- lm(woman_work_outside_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, data = merged)

domestic_5 <- lm(female_circ_marriage_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

domestic_6 <- lm(circ_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

domestic_7 <- lm(marriage_age_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

domestic_8 <- lm(khul_divorce_tf_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                   factor(block_ids),
                 weights = weight, na.action = na.omit, data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS18C <- stargazer(domestic_z, domestic_1, domestic_2, domestic_3, domestic_4,
                       domestic_5, domestic_6, domestic_7, domestic_8,
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,
                       font.size="scriptsize",       
                       dep.var.labels.include = FALSE,
                       table.placement = "t",
                       column.labels=att1_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, husb_work_att_mean), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "2pt",
                       title = "Treatment effects on attitudes towards gender and marital equality",
                       type = "latex")


note.latex <- "\\multicolumn{10}{l} {\\parbox[t]{25cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Husband final say, Husband earn income, Yelling justified, Hitting justified, Male education priority, 
Future equal say, and Future equal rights.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS18C[grepl("Note", TableS18C)] <- note.latex

cat(Panel_3(TableS18A, TableS18B, TableS18C, "\\\\shortstack\\{Index", 10, 1.5), file = 'Tables/S18.tex')

# Attitudes towards violence outside the household ----------------------------------------------------
att2_dep_var <- c("\\shortstack{Index of \\\\ (1,1,-1,1,\\\\1,-1,1,-1)}", 
                  "\\shortstack{Colleague \\\\ comments \\\\ on female \\\\ look \\\\ sexual \\\\ harassment}", 
                  "\\shortstack{Verbal \\\\ harassment\\\\ legal \\\\consequences}",
                  "\\shortstack{Interfere to \\\\ support \\\\a woman \\\\ sexually \\\\harassed \\\\ at workplace}", 
                  "\\shortstack{Inappropriate\\\\ clothing \\\\ or lack of Hijab \\\\ justifies \\\\ harassment}",
                  "\\shortstack{Interfere \\\\ if a man \\\\ hits a woman \\\\ on the street}",
                  "\\shortstack{Interfere if a \\\\man sexually \\\\ harasses  on\\\\ the street}",
                  "\\shortstack{Avoid \\\\ the authorities \\\\ if your daughter \\\\ sexually \\\\ assaulted}",
                  "\\shortstack{Seriousness \\\\ of a child \\\\ telling \\\\ that was \\\\ sexually \\\\harassed\\\\ by a relative}")


att2 <- merged[c("colleague_comment_looks_end_num", "verbal_harassment_legal_end_num", "intervene_work_harass_end_num", 
                 "women_clothes_harass_end_num","intervene_hitting_street_num", "you_intervene_harass_end_num", 
                 "parents_assault_auth_end_num", "child_relative_assault_end_num")]

att2_mean <- round(colMeans(att2[which(merged$control_10018 == 1),]), digits = 3)

# Calculate the z-score
zscore <- scale(rowMeans(scale(att2) %*% diag(c(1,1,1,-1,1,1,-1,1))))
merged$RF_att2_zscore <- zscore

# Baseline z-score (No lagged dvs)

# All covariates
att2_0 <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_1 <- lm(colleague_comment_looks_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_2 <- lm(verbal_harassment_legal_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight,  data = merged)

att2_3 <- lm(intervene_work_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_4 <- lm(women_clothes_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_5 <- lm(intervene_hitting_street_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_6 <- lm(you_intervene_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_7 <- lm(parents_assault_auth_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_8 <- lm(child_relative_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
               husb_justified_beat_num + future_equal_say_num + future_equal_rights_num + age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "husb_final_say_num", "prioritize_educ_num", 
              "husb_provide_inc_num", "husb_justified_yell_num", "husb_justified_beat_num",
              "future_equal_say_num", "future_equal_rights_num")

model_names = c("att2_0", "att2_1", "att2_2", "att2_3", "att2_4",
                "att2_5", "att2_6", "att2_7", "att2_8")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}


TableS19A <- stargazer(att2_0, att2_1, att2_2, att2_3, att2_4, att2_5, att2_6, att2_7, att2_8,
                       # align = TRUE,
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,
                       font.size="scriptsize",
                       dep.var.labels.include = FALSE,
                       table.placement = "t",
                       column.labels=att2_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on attitudes on sexual violence",
                       type = "latex")


# Lagged DV
att2_0 <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_1 <- lm(colleague_comment_looks_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_2 <- lm(verbal_harassment_legal_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, data = merged)

att2_3 <- lm(intervene_work_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_4 <- lm(women_clothes_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_5 <- lm(intervene_hitting_street_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_6 <- lm(you_intervene_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_7 <- lm(parents_assault_auth_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_8 <- lm(child_relative_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS19B <- stargazer(att2_0, att2_1, att2_2, att2_3, att2_4, att2_5, att2_6, att2_7, att2_8,
                       # align = TRUE,                           
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,          
                       font.size="scriptsize",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=att2_dep_var,
                       covariate.labels= covariates,
                       omit = c("Constant","block_ids"), 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on attitudes on sexual violence",
                       type = "latex")

# Panel C: No controls
att2_0 <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_1 <- lm(colleague_comment_looks_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_2 <- lm(verbal_harassment_legal_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, data = merged)

att2_3 <- lm(intervene_work_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_4 <- lm(women_clothes_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_5 <- lm(intervene_hitting_street_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_6 <- lm(you_intervene_harass_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_7 <- lm(parents_assault_auth_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

att2_8 <- lm(child_relative_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               factor(block_ids),
             weights = weight, na.action = na.omit, data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS19C <- stargazer(att2_0, att2_1, att2_2, att2_3, att2_4, att2_5, att2_6, att2_7, att2_8,
                       # align = TRUE,                           
                       float.env = "sidewaystable",
                       dep.var.caption = "",
                       header=FALSE,          
                       font.size="scriptsize",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=att2_dep_var,
                       covariate.labels= covariates,
                       omit = c("Constant","block_ids"), 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, att2_mean), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "0pt",
                       title = "Treatment effect on attitudes on sexual violence",
                       type = "latex")


note.latex <- "\\multicolumn{10}{l} {\\parbox[t]{22cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Husband final say, Husband earn income, Yelling justified, Hitting justified, Male education priority, 
Future equal say, and Future equal rights.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS19C[grepl("Note", TableS19C)] <- note.latex

cat(Panel_3(TableS19A, TableS19B, TableS19C, "\\\\shortstack\\{Index", 10), file = 'Tables/S19.tex')


# Revealed Preference ----------------------------------------------------------
donation <- merged[c("donation__end_cont", "donation_end_num")]

donation_mean <- round(colMeans(donation[which(merged$control_10018 == 1),]), digits = 3)
zscore <- scale(rowMeans(scale(donation) %*% diag(c(1, 1))))
merged$RF_preference_zscore <- zscore

# No lagged DVs

don_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

don_1 <-lm(donation__end_cont ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

don_2 <-lm(donation_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)


model_names = c("don_0", "don_1", "don_2")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

don_dep <- c("\\shortstack{Index of \\\\(1,1)}", 
             "Donation in EGP", 
             "\\shortstack{Donating more \\\\ than 0 EGP}")

omit_var <- c("age", "educ_aboveBA", "married", "Constant", "block_ids")

TableS20A <- stargazer(don_0, don_1, don_2, 
                      # align = TRUE,        
                      header=FALSE,          
                      font.size="footnotesize",
                      dep.var.caption = "",
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=don_dep,
                      covariate.labels= covariates,
                      omit = omit_var,
                      omit.stat=c("f", "ser","adj.rsq", "n"), 
                      add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                       c("SM Individual = TV (p-value)", pvals$X2), 
                                       c("SM Group= TV (p-value)", pvals$X3)),
                      column.sep.width = "20pt",
                      title = "Treatment effect on donation to organizations supporting women",
                      type = "latex")

# Panel B: Lagged DV 
don_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)


don_1 <-lm(donation__end_cont ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

don_2 <-lm(donation_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

model_names = c("don_0", "don_1", "don_2")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

don_dep <- c("Index of (1,1)", 
             "Donation in EGP", 
             "\\shortstack{Donating more \\\\ than 0 EGP}")

TableS20B <- stargazer(don_0, don_1, don_2, 
                      # align = TRUE,        
                      header=FALSE,          
                      font.size="footnotesize",
                      dep.var.caption = "",
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=don_dep,
                      covariate.labels= covariates,
                      omit = c("Constant","block_ids"), 
                      omit.stat=c("f", "ser","adj.rsq", "n"), 
                      add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                       c("SM Individual = TV (p-value)", pvals$X2), 
                                       c("SM Group= TV (p-value)", pvals$X3)),
                      column.sep.width = "20pt",
                      title = "Treatment effect on donation to organizations supporting women",
                      type = "latex")


# Panel C
don_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)


don_1 <-lm(donation__end_cont ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

don_2 <-lm(donation_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)


model_names = c("don_0", "don_1", "don_2")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

don_dep <- c("Index of (1,1)", 
             "Donation in EGP", 
             "\\shortstack{Donating more \\\\ than 0 EGP}")


TableS20C <- stargazer(don_0, don_1, don_2, 
                      # align = TRUE,        
                      header=FALSE,          
                      font.size="footnotesize",
                      dep.var.caption = "",
                      dep.var.labels.include = FALSE,
                      table.placement = "H",
                      column.labels=don_dep,
                      covariate.labels= covariates,
                      omit = c("Constant","block_ids"), 
                      omit.stat=c("f", "ser","adj.rsq"), 
                      add.lines = list(c("Control Mean", 0, donation_mean), 
                                       c("SM Individual = SM Group (p-value)", pvals$X1),
                                       c("SM Individual = TV (p-value)", pvals$X2), 
                                       c("SM Group= TV (p-value)", pvals$X3)),
                      column.sep.width = "20pt",
                      title = "Treatment effect on donation to organizations supporting women",
                      type = "latex")


note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{14cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS20C[grepl("Note", TableS20C)] <- note.latex

cat(Panel_3(TableS20A, TableS20B, TableS20C, "\\\\shortstack\\{Index", 4, NULL, 1.5), file = 'Tables/S20.tex')


# 4 - 5: DV look online for resources or org ---------------------------------------------------------------------------------
hyp_be <- merged[c("dv_onlineres_end_num", "dv_contactorg_end_num")]
hyp_mean_1 <- round(colMeans(hyp_be[which(merged$control_10018 == 1),]), digits = 3)
zscore <- scale(rowMeans(scale(hyp_be) %*% diag(c(1, 1))))
merged$RF_hypDM2_zscore <- zscore

# Baseline z-score
controls <- c("look_online_num", "contact_org_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1)))
merged$RF_hypDM2_zscore_base <- zscore_base


hyp_dep_1 <- c("\\shortstack{Index of \\\\ (1,1)}",
               "\\shortstack{Would use \\\\ online resources}",
               "\\shortstack{Would contact \\\\ organization}")

# All family covariates
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_1 <-lm(dv_onlineres_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_2 <-lm(dv_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

model_names = c("hyp_0", "hyp_1","hyp_2")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}
omit_var <- c("age", "educ_aboveBA", "married", "Constant", "block_ids", "talk_husband_num", "talk_family_num", 
             "report_authorities_num", "look_online_num", "contact_org_num", "RF_hypDM2_zscore_base")

#  need to add "Content Watched", "Most Liked Topics"
TableS21A <- stargazer(hyp_0, hyp_1, hyp_2,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_1,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect on hypothetical use of online resources and contact with an organization when responding to domestic violence",
                       type = "latex")

# Lagged DV
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + RF_hypDM2_zscore_base + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_1 <-lm(dv_onlineres_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             look_online_num +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_2 <-lm(dv_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             contact_org_num +
             factor(block_ids), 
           weights = weight, 
           data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS21B <- stargazer(hyp_0, hyp_1, hyp_2,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_1,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect on hypothetical use of online resources and contact with an organization when responding to domestic violence",
                       type = "latex")

# Panel C
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_1 <-lm(dv_onlineres_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_2 <-lm(dv_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS21C <- stargazer(hyp_0, hyp_1, hyp_2,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_1,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, hyp_mean_1),
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect on hypothetical use of online resources and contact with an organization when responding to domestic violence",
                       type = "latex")

note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{14cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Would talk husband, Would talk family, would report authorities, 
Would use online resources, and Would contact organization. 
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}}\\\\"
TableS21C[grepl("Note", TableS21C)] <- note.latex

cat(Panel_3(TableS21A, TableS21B, TableS21C, "\\\\shortstack\\{Index", 4, NULL, 1.5), file = 'Tables/S21.tex')

# Variables contact or look organization/online resources ----------------------
hyp_be_2 <- merged[c("sa_onlineres_end_num", "sa_contactorg_end_num")]
hyp_mean_2 <- round(colMeans(hyp_be_2[which(merged$control_10018 == 1),]), digits = 3)
zscore <- scale(rowMeans(scale(hyp_be_2) %*% diag(c(1, 1))))
merged$RF_hypSA2_zscore <- zscore

# No lagged dvs
hyp_dep_2 <- c("\\shortstack{Index of \\\\ (1,1)}", 
               "\\shortstack{Would use \\\\ online resources}",
               "\\shortstack{Would contact \\\\ organization}")

# All covariates
hyp_0 <- lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
              talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

hyp_1 <-lm(sa_onlineres_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_2 <-lm(sa_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids),
           weights = weight,
           data = merged)

model_names = c("hyp_0", "hyp_1","hyp_2")
list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant", "block_ids", "talk_husband_num", "talk_family_num", 
              "report_authorities_num", "look_online_num", "contact_org_num")

TableS22A <- stargazer(hyp_0, hyp_1, hyp_2,
                       #align = TRUE,
                       header=FALSE,
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_2,
                       covariate.labels= covariates,
                       omit = omit_var,
                       omit.stat=c("n", "f", "ser","adj.rsq"),
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "10pt",
                       title = "Treatment effect on  hypothetical use of online resources and contact with an organization when responding to sexual violence",
                       type = "latex")


# Lagged DV
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)


hyp_1 <-lm(sa_onlineres_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_2 <-lm(sa_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS22B <- stargazer(hyp_0, hyp_1, hyp_2,
                       #align = TRUE,
                       header=FALSE,
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_2,
                       covariate.labels= covariates,
                       omit = c("Constant", "block_ids"),
                       omit.stat=c("f", "ser","adj.rsq", "n"),
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "10pt",
                       title = "Treatment effect on  hypothetical use of online resources and contact with an organization when responding to sexual violence",
                       type = "latex")

# Panel C
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_1 <-lm(sa_onlineres_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_2 <-lm(sa_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS22C <- stargazer(hyp_0, hyp_1, hyp_2,
                       #align = TRUE,
                       header=FALSE,
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_2,
                       covariate.labels= covariates,
                       omit = c("Constant", "block_ids"),
                       omit.stat=c("f", "ser","adj.rsq"),
                       add.lines = list(c("Control Mean", 0, hyp_mean_2), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "10pt",
                       title = "Treatment effect on  hypothetical use of online resources and contact with an organization when responding to sexual violence",
                       type = "latex")


note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Would talk husband, Would talk family, would report authorities, 
Would use online resources, and Would contact organization. 
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}}  \\\\"
TableS22C[grepl("Note", TableS22C)] <- note.latex

cat(Panel_3(TableS22A, TableS22B, TableS22C, "\\\\shortstack\\{Index", 4, NULL, 1.5), file = 'Tables/S22.tex')


# During COVID-19 ------------------------------------------------------------
know_during <- merged[c("dcovid_accessonline_end_num","dcovid_contactorg_end_num")]
know_during_mean <- round(colMeans(know_during[which(merged$control_10018 == 1),]), digits = 3)
during_zscore <- scale(rowMeans(scale(know_during)%*% diag(c(1, 1))))
merged$RF_dcovid2_zscore <- during_zscore

# Baseline z-score
controls <- c("dc19_look_online_num", "dc19_look_org_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1)))
merged$RF_dcovid2_zscore_base <- zscore_base

merged$dcovid_accessonline_end_num_refuse <- ifelse(merged$dcovid_accessonline_end == "I refuse to answer", 1, 0)
merged$dcovid_accessonline_end_num_refuse <- replace_na(merged$dcovid_accessonline_end_num_refuse, 1)

merged$dcovid_contactorg_end_num_refuse <- ifelse(merged$dcovid_contactorg_end == "I refuse to answer", 1, 0)
merged$dcovid_contactorg_end_num_refuse <- replace_na(merged$dcovid_contactorg_end_num_refuse, 1)

know_during_var <- c("\\shortstack{Index of \\\\ (1,1) }", 
                     "\\shortstack{Used online\\\\ resources}", 
                     "\\shortstack{Contacted \\\\ organization}")

# All covariates
zs <-lm(during_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
          know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
          dcovid_accessonline_end_num_refuse + dcovid_contactorg_end_num_refuse + know_org_noaut_valid_num +know_online_nehad_num + 
          know_org_nehad_num + age + educ_aboveBA + married +
          factor(block_ids), 
        weights = weight, 
        data = merged)

dc_accesson <-lm(dcovid_accessonline_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                   know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
                   dcovid_accessonline_end_num_refuse + know_org_noaut_valid_num +know_online_nehad_num + know_org_nehad_num +
                   age + educ_aboveBA + married + factor(block_ids), 
                 weights = weight, 
                 data = merged)

dc_contactorg <-lm(dcovid_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                     know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
                     dcovid_contactorg_end_num_refuse + know_org_noaut_valid_num + know_online_nehad_num + know_org_nehad_num +
                     age + educ_aboveBA + married + factor(block_ids), 
                   weights = weight, 
                   data = merged)

model_names = c("zs", "dc_accesson","dc_contactorg")
list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "know_online_valid_noaut_num", "dc19_look_online_num", "bc19_look_online_num", "bc19_look_org_num",
              "dc19_look_org_num", "dcovid_accessonline_end_num_refuse", "dcovid_contactorg_end_num_refuse", "know_org_noaut_valid_num",
              "know_online_nehad_num", "know_org_nehad_num", "RF_dcovid2_zscore_base")

TableS23A <- stargazer(zs, dc_accesson, dc_contactorg,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_during_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "25pt",
                       title = "Treatment effect on recent use of online resources and contact with an organization during COVID-19",
                       type = "latex")

# Lagged DV
zs <-lm(during_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
          dcovid_accessonline_end_num_refuse + dcovid_contactorg_end_num_refuse + RF_dcovid2_zscore_base +
          factor(block_ids), 
        weights = weight, 
        data = merged)

dc_accesson <-lm(dcovid_accessonline_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                   dcovid_accessonline_end_num_refuse + dc19_look_online_num +
                   factor(block_ids), 
                 weights = weight, 
                 data = merged)

dc_contactorg <-lm(dcovid_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                     dcovid_contactorg_end_num_refuse + dc19_look_org_num +
                     factor(block_ids), 
                   weights = weight, 
                   data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS23B <- stargazer(zs, dc_accesson, dc_contactorg,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_during_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "25pt",
                       title = "Treatment effect on recent use of online resources and contact with an organization during COVID-19",
                       type = "latex")

# Panel C
zs <- lm(during_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
          dcovid_accessonline_end_num_refuse + dcovid_contactorg_end_num_refuse +
          factor(block_ids), 
        weights = weight, 
        data = merged)

dc_accesson <- lm(dcovid_accessonline_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                   dcovid_accessonline_end_num_refuse  +
                   factor(block_ids), 
                 weights = weight, 
                 data = merged)

dc_contactorg <- lm(dcovid_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                     dcovid_contactorg_end_num_refuse +
                     factor(block_ids), 
                   weights = weight, 
                   data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS23C <- stargazer(zs, dc_accesson, dc_contactorg,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_during_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, know_during_mean), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "25pt",
                       title = "Treatment effect on recent use of online resources and contact with an organization during COVID-19",
                       type = "latex")

note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{13.5cm}{ \\textit{Notes:}  
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Know online: other than ECWR, Know online: ECWR, Before COVID-19 used online resources, 
During COVID-19 used online resources, Know organization: other than ECWR, Know organization: ECWR
Before COVID-19 contacted organization, and During COVID-19 contacted organization.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS23C[grepl("Note", TableS23C)] <- note.latex

cat(Panel_3(TableS23A, TableS23B, TableS23C, "\\\\shortstack\\{Index", 4, NULL, 1.5), file = 'Tables/S23.tex')

# -----------------------------------------------------------------------------------------------
# Part 3
# Future Outlook
fut_out <- merged[c("future_equal_say_end_num","future_equal_rights_end_num")]
fut_out_mean <- round(colMeans(fut_out[which(merged$control_10018 == 1),]), digits = 3)
fo_dep_var <- c("\\shortstack{Index of \\\\(1,1)}", 
                "\\shortstack{Future\\\\ equal say}", 
                "\\shortstack{Future \\\\ equal rights}")

# Calculate the z-score
zscore <- scale(rowMeans(scale(fut_out) %*% diag(c(1,1))))
merged$RF_fo_zscore <- zscore

# Baseline z-score
controls <- c("future_equal_say_num", "future_equal_rights_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1)))
merged$RF_fo_zscore_base <- zscore_base

# All family variables covariates 
fo_0 <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
             husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
             age + educ_aboveBA + married +
             factor(block_ids),
           weights = weight, data = merged)

fo_1 <- lm(future_equal_say_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
             husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
             age + educ_aboveBA + married +
             factor(block_ids),
           weights = weight, data = merged)

fo_2 <- lm(future_equal_rights_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
             husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
             age + educ_aboveBA + married +
             factor(block_ids),
           weights = weight,  data = merged)

model_names = c("fo_0", "fo_1","fo_2")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num",
              "husb_justified_yell_num", "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num", "RF_fo_zscore_base")

TableS24A <- stargazer(fo_0, fo_1, fo_2,
                       #align = TRUE,
                       header=FALSE, 
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=fo_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect on views on women's future outlook toward gender and marital equality",
                       type = "latex")


# DV lagged covariates
fo_0 <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + RF_fo_zscore_base +
             factor(block_ids),
           weights = weight, data = merged)

fo_1 <- lm(future_equal_say_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             future_equal_say_num +
             factor(block_ids),
           weights = weight, data = merged)

fo_2 <- lm(future_equal_rights_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             future_equal_rights_num +
             factor(block_ids),
           weights = weight,  data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS24B <- stargazer(fo_0, fo_1, fo_2,
                       #align = TRUE,
                       header=FALSE, 
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=fo_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect on views on women's future outlook toward gender and marital equality",
                       type = "latex")


# Panel C
fo_0 <- lm(zscore ~ pooledF_WI + in_group_10018 + reminder_10018 + 
             factor(block_ids),
           weights = weight, data = merged)

fo_1 <- lm(future_equal_say_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             factor(block_ids),
           weights = weight, data = merged)

fo_2 <- lm(future_equal_rights_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             factor(block_ids),
           weights = weight,  data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS24C <- stargazer(fo_0, fo_1, fo_2,
                       #align = TRUE,
                       header=FALSE, 
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=fo_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, fut_out_mean), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect on views on women's future outlook toward gender and marital equality",
                       type = "latex")

note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Husband final say, Husband earn income, Yelling justified, Hitting justified, Male education priority, 
Future equal say, and Future equal rights.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}}  \\\\"
TableS24C[grepl("Note", TableS24C)] <- note.latex

cat(Panel_3(TableS24A, TableS24B, TableS24C, "\\\\shortstack\\{Index", 4, NULL, 1.5), file = 'Tables/S24.tex')


# Reduced form for behavior and reporting during COVID-19 ----------------------
rep_during_data <- merged[c("dcovid_yelled_end_num","dcovid_hit_end_num","dcovid_assault_end_num")]

rep_during_control <- round(colMeans(rep_during_data[which(merged$control_10018 == 1),]), digits = 3)

zscore <- scale(rowMeans(scale(rep_during_data) %*% diag(c(1, 1, 1))))

merged$RF_dcovid_zscore <- zscore

# Baseline z-score
controls <- c("dc19_yell_num", "dc19_hit_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1)))
merged$RF_dcovid_zscore_base <- zscore_base

dep_var_re <- c("\\shortstack{Index of \\\\ (1,1,1)}", 
                "\\shortstack{Heard of or \\\\ experienced yelling}", 
                "\\shortstack{Heard of or \\\\ experienced hitting}", 
                "\\shortstack{Heard of or \\\\ experienced sexual \\\\ abuse}")

merged$dcovid_yelled_end_num_refuse <- ifelse(merged$dcovid_yelled_end == "I refuse to answer", 1, 0)
merged$dcovid_hit_end_num_refuse <- ifelse(merged$dcovid_hit_end == "I refuse to answer", 1, 0)
merged$dcovid_assault_end_num_refuse <- ifelse(merged$dcovid_assault_end == "I refuse to answer", 1, 0)

# All covariates
d_zscore <-lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num  + 
                dcovid_yelled_end_num_refuse + dcovid_hit_end_num_refuse + dcovid_assault_end_num_refuse + 
                age + educ_aboveBA + married +
                factor(block_ids),
              weights = weight, data = merged)


dc_yell <- lm(dcovid_yelled_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num + dcovid_yelled_end_num_refuse +
                age + educ_aboveBA + married +
                factor(block_ids),
              weights = weight, data = merged)

dc_hit <- lm(dcovid_hit_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
               bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num + dcovid_hit_end_num_refuse +
               age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, data = merged)

dc_assa <- lm(dcovid_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num + dcovid_assault_end_num_refuse +
                age + educ_aboveBA + married +
                factor(block_ids),
              weights = weight, data = merged)


omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num",
              "dcovid_yelled_end_num_refuse", 
              "dcovid_hit_end_num_refuse", "dcovid_assault_end_num_refuse", "RF_dcovid_zscore_base")

model_names = c("d_zscore", "dc_yell", "dc_hit", "dc_assa")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS25A <- stargazer(d_zscore, dc_yell, dc_hit, dc_assa,
                       #align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var_re,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "5pt",
                       title = "Treatment effect on domestic and sexual violence experienced during COVID-19",
                       type = "latex")


# Lagged DP Variable
d_zscore <-lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + dcovid_yelled_end_num_refuse +
                dcovid_hit_end_num_refuse + dcovid_assault_end_num_refuse + RF_dcovid_zscore_base +
                factor(block_ids),
              weights = weight, data = merged)


dc_yell <- lm(dcovid_yelled_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                dc19_yell_num + dcovid_yelled_end_num_refuse +
                factor(block_ids),
              weights = weight, data = merged)

dc_hit <- lm(dcovid_hit_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
               dc19_hit_num + dcovid_hit_end_num_refuse +
               factor(block_ids),
             weights = weight, data = merged)

dc_assa <- lm(dcovid_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                dcovid_assault_end_num_refuse +
                factor(block_ids),
              weights = weight, data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS25B <- stargazer(d_zscore, dc_yell, dc_hit, dc_assa,
                       #align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var_re,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "5pt",
                       title = "Treatment effect on domestic and sexual violence experienced during COVID-19",
                       type = "latex")


# Panel C
d_zscore <-lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + dcovid_yelled_end_num_refuse +
                dcovid_hit_end_num_refuse + dcovid_assault_end_num_refuse + 
                factor(block_ids),
              weights = weight, data = merged)

dc_yell <- lm(dcovid_yelled_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                dcovid_yelled_end_num_refuse +
                factor(block_ids),
              weights = weight, data = merged)

dc_hit <- lm(dcovid_hit_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 +
               dcovid_hit_end_num_refuse +
               factor(block_ids),
             weights = weight, data = merged)

dc_assa <- lm(dcovid_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                dcovid_assault_end_num_refuse +
                factor(block_ids),
              weights = weight, data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS25C <- stargazer(d_zscore, dc_yell, dc_hit, dc_assa,
                       #align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var_re,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, rep_during_control), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "5pt",
                       title = "Treatment effect on domestic and sexual violence experienced during COVID-19",
                       type = "latex")

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{16.5cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Before COVID-19 heard of or experienced yelling, Before COVID-19 heard of or experienced hitting, 
During COVID-19 heard of or experienced yelling, and During COVID-19 heard of or experienced hitting
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}}\\\\"
TableS25C[grepl("Note", TableS25C)] <- note.latex

cat(Panel_3(TableS25A, TableS25B, TableS25C, "\\\\shortstack\\{Index", 5, NULL, 1.5), file = 'Tables/S25.tex')


# Behavior and Reporting (part of Observed Behavior) ---------------------------
# BEFORE COVID -----------------------------------------------------------------
rep_before_data <- merged[c("bcovid_yelled_end_num", "bcovid_hit_end_num", "bcovid_assault_end_num")]
rep_before_control <- round(colMeans(rep_before_data[which(merged$control_10018 == 1),]), digits = 3)
zscore <- scale(rowMeans(scale(rep_before_data) %*% diag(c(1, 1, 1))))
merged$RT_bcovid_zscore <- zscore

# Baseline z-score
controls <- c("bc19_yell_num", "bc19_hit_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1)))
merged$RT_bcovid_zscore_base <- zscore_base

merged$bcovid_yelled_end_num_refuse <- ifelse(merged$bcovid_yelled_end == "I refuse to answer", 1, 0)
merged$bcovid_hit_end_num_refuse <- ifelse(merged$bcovid_hit_end == "I refuse to answer", 1, 0)
merged$bcovid_assault_end_num_refuse <- ifelse(merged$bcovid_assault_end == "I refuse to answer", 1, 0)

dep_var_re <- c("\\shortstack{Index of \\\\ (1,1,1)}",
                "\\shortstack{Heard of or\\\\experienced yelling}", 
                "\\shortstack{Heard of or\\\\experienced hitting}", 
                "\\shortstack{Heard of or\\\\experienced sexual \\\\ abuse}")

# All covariates
b_zscore <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                 bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num  +
                 bcovid_yelled_end_num_refuse + bcovid_hit_end_num_refuse + bcovid_assault_end_num_refuse +
                 age + educ_aboveBA + married +
                 factor(block_ids),
               weights = weight, data = merged)

bc_yell <- lm(bcovid_yelled_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num + bcovid_yelled_end_num_refuse +
                age + educ_aboveBA + married +
                factor(block_ids),
              weights = weight, data = merged)

bc_hit <- lm(bcovid_hit_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num + bcovid_hit_end_num_refuse +
               age + educ_aboveBA + married +
               factor(block_ids),
             weights = weight, data = merged)
bc_assa <- lm(bcovid_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num + bcovid_assault_end_num_refuse +
                age + educ_aboveBA + married +
                factor(block_ids),
              weights = weight, data = merged)

model_names = c("b_zscore", "bc_yell", "bc_hit", "bc_assa")

list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", 
              "dc19_hit_num", "bcovid_yelled_end_num_refuse", "bcovid_assault_end_num_refuse",
              "dcovid_yelled_end_num_refuse", "bcovid_hit_end_num_refuse", "RT_bcovid_zscore_base")

TableS26A <- stargazer(b_zscore, bc_yell, bc_hit, bc_assa,
                       #align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var_re,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),                          
                       column.sep.width = "0pt",
                       title = "Treatment effects on domestic and sexual violence experienced before COVID-19",
                       type = "latex")

# Lagged DV
b_zscore <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + RT_bcovid_zscore_base +
                 bcovid_yelled_end_num_refuse +  bcovid_hit_end_num_refuse + bcovid_assault_end_num_refuse +
                 factor(block_ids),
               weights = weight, data = merged)

bc_yell <- lm(bcovid_yelled_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bc19_yell_num + bcovid_yelled_end_num_refuse +
                factor(block_ids),
              weights = weight, data = merged)

bc_hit <- lm(bcovid_hit_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               bc19_hit_num + bcovid_hit_end_num_refuse +
               factor(block_ids),
             weights = weight, data = merged)

bc_assa <- lm(bcovid_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bcovid_assault_end_num_refuse +
                factor(block_ids),
              weights = weight, data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}


TableS26B <- stargazer(b_zscore, bc_yell, bc_hit, bc_assa,
                       #align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var_re,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),                          
                       column.sep.width = "0pt",
                       title = "Treatment effects on domestic and sexual violence experienced before COVID-19",
                       type = "latex")

# Panel C
b_zscore <- lm(zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                 bcovid_yelled_end_num_refuse +  bcovid_hit_end_num_refuse + bcovid_assault_end_num_refuse +
                 factor(block_ids),
               weights = weight, data = merged)

bc_yell <- lm(bcovid_yelled_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bcovid_yelled_end_num_refuse +
                factor(block_ids),
              weights = weight, data = merged)

bc_hit <- lm(bcovid_hit_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
               bcovid_hit_end_num_refuse +
               factor(block_ids),
             weights = weight, data = merged)

bc_assa <- lm(bcovid_assault_end_num ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bcovid_assault_end_num_refuse +
                factor(block_ids),
              weights = weight, data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}


TableS26C <- stargazer(b_zscore, bc_yell, bc_hit, bc_assa,
                       #align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=dep_var_re,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, rep_before_control), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),                          
                       column.sep.width = "0pt",
                       title = "Treatment effects on domestic and sexual violence experienced before COVID-19",
                       type = "latex")

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{16cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Before COVID-19 heard of or experienced yelling, Before COVID-19 heard of or experienced hitting, 
During COVID-19 heard of or experienced yelling, and During COVID-19 heard of or experienced hitting
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}}\\\\"
TableS26C[grepl("Note", TableS26C)] <- note.latex

cat(Panel_3(TableS26A, TableS26B, TableS26C, "\\\\shortstack\\{Index", 5, NULL, 1.5), file = 'Tables/S26.tex')

# Hypothetical Behavior -----------------------------------------------------------
# Domestic Violence, Sexual Assault, or Harassment
# Reduced form for hypothetical behavior around domestic violence (DV) variables,part 2
hyp_be <- merged[c("dv_talkhusb_end_num", "dv_talktfam_end_num", "dv_report_end_num")]
hyp_mean_1 <- round(colMeans(hyp_be[which(merged$control_10018 == 1),]), digits = 3)
zscore <- scale(rowMeans(scale(hyp_be) %*% diag(c(1, 1, 1))))
merged$RF_hypDM1_zscore <- zscore

# Baseline z-score
controls <- c("talk_husband_num", "talk_family_num", "report_authorities_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1,1)))
merged$RF_hypDM1_zscore_base <- zscore_base

hyp_dep_1 <- c("\\shortstack{Index of \\\\ (1,1,1)}", 
               "\\shortstack{Would \\\\ talk husband}", 
               "\\shortstack{Would \\\\ talk family}", 
               "\\shortstack{Would \\\\ report \\\\ authorities}")

# All family covariates
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_1 <-lm(dv_talkhusb_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_2 <-lm(dv_talktfam_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_3 <-lm(dv_report_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids), 
           weights = weight, 
           data = merged)

model_names = c("hyp_0", "hyp_1", "hyp_2","hyp_3")
list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant", "block_ids", "talk_husband_num", "talk_family_num", 
             "report_authorities_num", "look_online_num", "contact_org_num", "RF_hypDM1_zscore_base")

#  need to add "Content Watched", "Most Liked Topics"
TableS27A <- stargazer(hyp_0, hyp_1, hyp_2, hyp_3,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_1,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect of hypothetical talking to husband and family members, or reporting to authorities when responding to domestic violence",
                       type = "latex")

# Only DV lagged control 
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + RF_hypDM1_zscore_base +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_1 <-lm(dv_talkhusb_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_2 <-lm(dv_talktfam_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_family_num +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_3 <-lm(dv_report_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             report_authorities_num +
             factor(block_ids), 
           weights = weight, 
           data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS27B <- stargazer(hyp_0, hyp_1, hyp_2, hyp_3,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_1,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect of hypothetical talking to husband and family members, or reporting to authorities when responding to domestic violence",
                       type = "latex")

# Panel C
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_1 <-lm(dv_talkhusb_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_2 <-lm(dv_talktfam_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_3 <-lm(dv_report_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             factor(block_ids), 
           weights = weight, 
           data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS27C <- stargazer(hyp_0, hyp_1, hyp_2, hyp_3,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_1,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, hyp_mean_1), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect of hypothetical talking to husband and family members, or reporting to authorities when responding to domestic violence",
                       type = "latex")

note.latex <- "\\multicolumn{5}{l} {\\parbox[t]{15.5cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Would talk husband, Would talk family, would report authorities, 
Would use online resources, and Would contact organization. 
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}}\\\\"
TableS27C[grepl("Note", TableS27C)] <- note.latex

cat(Panel_3(TableS27A, TableS27B, TableS27C, "\\\\shortstack\\{Index", 5, NULL, 1.5), file = 'Tables/S27.tex')

# Part 2: Sexual Assault or Harassment -----------------------------------------
hyp_be_2 <- merged[c("sa_talkfam_end_num", "sa_report_end_num")]
hyp_mean_2 <- round(colMeans(hyp_be_2[which(merged$control_10018 == 1),]), digits = 3)
zscore <- scale(rowMeans(scale(hyp_be_2) %*% diag(c(1, 1))))
merged$RF_hypSA1_zscore <- zscore

hyp_dep_2 <- c("\\shortstack{Index of \\\\ (1,1)}", 
               "\\shortstack{Would  \\\\ talk family}", 
               "\\shortstack{Would \\\\  report  \\\\ authorities}")

# All covariates
hyp_0 <- lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
              talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
              age + educ_aboveBA + married +
              factor(block_ids),
            weights = weight,
            data = merged)

hyp_1 <-lm(sa_talkfam_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_2 <-lm(sa_report_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             age + educ_aboveBA + married +
             factor(block_ids),
           weights = weight,
           data = merged)

model_names = c("hyp_0", "hyp_1", "hyp_2")
list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant", "block_ids", "talk_husband_num", "talk_family_num", 
              "report_authorities_num", "look_online_num", "contact_org_num")

TableS28A <- stargazer(hyp_0, hyp_1, hyp_2,
                       #align = TRUE,
                       header=FALSE,
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_2,
                       covariate.labels= covariates,
                       omit = omit_var,
                       omit.stat=c("n", "f", "ser","adj.rsq"),
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect of hypothetical talking to family members or reporting to authorities when responding to sexual violence",
                       type = "latex")

# Only DV lagged control 
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_1 <-lm(sa_talkfam_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_2 <-lm(sa_report_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS28B <- stargazer(hyp_0, hyp_1, hyp_2,
                       #align = TRUE,
                       header=FALSE,
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_2,
                       covariate.labels= covariates,
                       omit = c("Constant", "block_ids"),
                       omit.stat=c("f", "ser","adj.rsq", "n"),
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect of hypothetical talking to family members or reporting to authorities when responding to sexual violence",
                       type = "latex")

# Panel C
hyp_0 <-lm(zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_1 <-lm(sa_talkfam_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

hyp_2 <-lm(sa_report_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 +
             factor(block_ids),
           weights = weight,
           data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS28C <- stargazer(hyp_0, hyp_1, hyp_2,
                       #align = TRUE,
                       header=FALSE,
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=hyp_dep_2,
                       covariate.labels= covariates,
                       omit = c("Constant", "block_ids"),
                       omit.stat=c("f", "ser","adj.rsq"),
                       add.lines = list(c("Control Mean", 0, hyp_mean_2), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "20pt",
                       title = "Treatment effect of hypothetical talking to family members or reporting to authorities when responding to sexual violence",
                       type = "latex")

note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{13cm}{ \\textit{Notes:}  
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Would talk husband, Would talk family, would report authorities, 
Would use online resources, and Would contact organization. 
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}}  \\\\"
TableS28C[grepl("Note", TableS28C)] <- note.latex

cat(Panel_3(TableS28A, TableS28B, TableS28C, "\\\\shortstack\\{Index", 4, NULL, 1.5), file = 'Tables/S28.tex')

# Knowledge, Behavior, and Reporting -------------------------------------------
# Before COVID-19
know_before <- merged[c("bcvovid_accessonline_end_num",
                        "bcovid_contactorg_end_num")]
know_mean <- round(colMeans(know_before[which(merged$control_10018 == 1),]), digits = 3)

# calculating zscore
zscore_bef <- scale(rowMeans(scale(know_before) %*% diag(c(1, 1))))
merged$RT_bcovidAccess_zscore <- zscore_bef


# Baseline z-score
controls <- c("bc19_look_online_num", "bc19_look_org_num")
zscore_base <- scale(rowMeans(scale(merged[controls]) %*% c(1,1)))
merged$RT_bcovidAccess_zscore_base <- zscore_base

# refuse covariates
merged$bcvovid_accessonline_end_num_refuse <- ifelse(merged$bcvovid_accessonline_end == "I refuse to answer", 1, 0)
merged$bcvovid_accessonline_end_num_refuse <- replace_na(merged$bcvovid_accessonline_end_num_refuse, 1)

merged$bcovid_contactorg_end_num_refuse <- ifelse(merged$bcovid_contactorg_end == "I refuse to answer", 1, 0)
merged$bcovid_contactorg_end_num_refuse <- replace_na(merged$bcovid_contactorg_end_num_refuse, 1)

know_dep_var <- c("\\shortstack{Index of \\\\ (1,1) }", 
                  "\\shortstack{Used online \\\\ resources}", 
                  "\\shortstack{Contacted \\\\ organization}")

# All family covariates 
zs <-lm(zscore_bef ~ pooledF_WI + in_group_10018  + reminder_10018 + 
          know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
          bcvovid_accessonline_end_num_refuse + bcovid_contactorg_end_num_refuse +know_org_noaut_valid_num +know_online_nehad_num + 
          know_org_nehad_num + 
          age + educ_aboveBA + married +
          factor(block_ids), 
        weights = weight, 
        data = merged)

bc_accesson <-lm(bcvovid_accessonline_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                   know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
                   bcvovid_accessonline_end_num_refuse + know_org_noaut_valid_num +know_online_nehad_num + 
                   know_org_nehad_num + 
                   age + educ_aboveBA + married +
                   factor(block_ids), 
                 weights = weight, 
                 data = merged)

bc_contactorg <-lm(bcovid_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                     know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
                     bcovid_contactorg_end_num_refuse  + know_org_noaut_valid_num +know_online_nehad_num + 
                     know_org_nehad_num + 
                     age + educ_aboveBA + married +
                     factor(block_ids), 
                   weights = weight, 
                   data = merged)

model_names = c("zs", "bc_accesson", "bc_contactorg")
list_models = lapply(model_names, get)

test <- c("pooledF_WI = in_group_10018",
          "pooledF_WI = reminder_10018",
          "in_group_10018 = reminder_10018")

pvals <- data.frame(matrix(NA, nrow = length(model_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

omit_var <- c("age", "educ_aboveBA", "married", "Constant","block_ids", "know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num", 
              "bc19_look_org_num", "dc19_look_org_num", "bcvovid_accessonline_end_num_refuse", "know_org_noaut_valid_num", 
              "know_online_nehad_num","know_org_nehad_num",
              "dcovid_accessonline_end_num_refuse", "bcovid_contactorg_end_num_refuse", "dcovid_contactorg_end_num_refuse",
              "RT_bcovidAccess_zscore_base")

TableS29A <- stargazer(zs, bc_accesson, bc_contactorg,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("n", "f", "ser","adj.rsq"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "25pt",
                       
                       title = "Treatment effects on recent use of online resources and contact with an organization when responding to domestic and sexual violence before COVID-19",
                       type = "latex")

# Only DV lagged control 
zs <-lm(zscore_bef ~ pooledF_WI + in_group_10018  + reminder_10018 + RT_bcovidAccess_zscore_base +
          bcvovid_accessonline_end_num_refuse + bcovid_contactorg_end_num_refuse +
          factor(block_ids), 
        weights = weight, 
        data = merged)

bc_accesson <-lm(bcvovid_accessonline_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                   + bcvovid_accessonline_end_num_refuse + bc19_look_online_num +
                   factor(block_ids), 
                 weights = weight, 
                 data = merged)

bc_contactorg <-lm(bcovid_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                     bcovid_contactorg_end_num_refuse + bc19_look_org_num +
                     factor(block_ids), 
                   weights = weight, 
                   data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS29B <- stargazer(zs, bc_accesson, bc_contactorg,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq", "n"), 
                       add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "25pt",
                       title = "Treatment effects on recent use of online resources and contact with an organization when responding to domestic and sexual violence before COVID-19",
                       type = "latex")


# Panel C
zs <-lm(zscore_bef ~ pooledF_WI + in_group_10018  + reminder_10018 + 
          bcvovid_accessonline_end_num_refuse + bcovid_contactorg_end_num_refuse +
          factor(block_ids), 
        weights = weight, 
        data = merged)

bc_accesson <-lm(bcvovid_accessonline_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                   + bcvovid_accessonline_end_num_refuse  +
                   factor(block_ids), 
                 weights = weight, 
                 data = merged)

bc_contactorg <-lm(bcovid_contactorg_end_num ~ pooledF_WI + in_group_10018  + reminder_10018 + 
                     bcovid_contactorg_end_num_refuse  +
                     factor(block_ids), 
                   weights = weight, 
                   data = merged)

list_models = lapply(model_names, get)

for (i in 1:length(test)) {
  for (x in 1: length(model_names)) {
    paux <- round(linearHypothesis(list_models[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

TableS29C <- stargazer(zs, bc_accesson, bc_contactorg,
                       # align = TRUE,        
                       header=FALSE,          
                       font.size="footnotesize",
                       dep.var.caption = "",
                       dep.var.labels.include = FALSE,
                       table.placement = "H",
                       column.labels=know_dep_var,
                       covariate.labels= covariates,
                       omit = omit_var, 
                       omit.stat=c("f", "ser","adj.rsq"), 
                       add.lines = list(c("Control Mean", 0, know_mean), 
                                        c("SM Individual = SM Group (p-value)", pvals$X1),
                                        c("SM Individual = TV (p-value)", pvals$X2), 
                                        c("SM Group= TV (p-value)", pvals$X3)),
                       column.sep.width = "25pt",
                       title = "Treatment effects on recent use of online resources and contact with an organization when responding to domestic and sexual violence before COVID-19",
                       type = "latex")

note.latex <- "\\multicolumn{4}{l} {\\parbox[t]{13cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A include demographic covariates and all baseline covariates in the outcome family: 
Know online: other than ECWR, Know online: ECWR, Before COVID-19 used online resources, 
During COVID-19 used online resources, Know organization: other than ECWR, Know organization: ECWR
Before COVID-19 contacted organization, and During COVID-19 contacted organization.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
TableS29C[grepl("Note", TableS29C)] <- note.latex

cat(Panel_3(TableS29A, TableS29B, TableS29C, "\\\\shortstack\\{Index", 4, NULL, 1.5), file = 'Tables/S29.tex')

# Figures ----------------------------------------------------------------------
# Set up for ggplots
text.11 <- element_text(color = "black", size = 10)
final.text.11 <- element_text(color = "black", size = 11, hjust = 0.5, family="Arial")

th <-  theme(strip.text.x = final.text.11,
             strip.placement = "inside",
             strip.background = element_rect(colour = "black", fill = "white"),
             axis.text.x = element_blank(), 
             axis.text.y = text.11,
             axis.title.x=element_blank(),
             legend.text = final.text.11,
             legend.title = final.text.11,
             axis.text = final.text.11, 
             panel.border = element_rect(colour = "black", fill = NA),
             legend.position="bottom")

legend_guide <- guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

colors <- c("#000000", "#0072B2","#D55E00")

y_text <-c("Treatment effect")



# Figure 2
fs_tv <-lm(FS_TV_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
             tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
             factor(block_ids),
           weights = weight,
           data = merged)

fs_fbw <- lm(FS_FW_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
               X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + factor(block_ids),
             weights = weight,
             data = merged)

know_1 <-lm(RF_knowledge2_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
              know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
              know_org_noaut_valid_num + know_online_nehad_num + know_org_nehad_num +
              factor(block_ids), 
            weights = weight, 
            data = merged)

# Save lm regressions and retrieve coeffs and sd
tv <- summary(fs_tv)
fbw <- summary(fs_fbw)
know <- summary(know_1)

coef_tv <- tv$coefficients[2:4,1] 
interval_lm1 <- as.data.frame(confint(fs_tv, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_fbw <- fbw$coefficients[2:4,1] 
interval_lm2 <- as.data.frame(confint(fs_fbw, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_know <- know$coefficients[2:4,1] 
interval_lm3 <- as.data.frame(confint(know_1, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))


coef_tv <- c(coef_tv[[1]], coef_tv[[2]], coef_tv[[3]])
coef_fbw <- c(coef_fbw[[1]], coef_fbw[[2]], coef_fbw[[3]])
coef_know <- c(coef_know[[1]], coef_know[[2]], coef_know[[3]])


# Create an auxiliary data frame
c1 <- c(coef_tv, coef_fbw, coef_know)
c2 <- c(rep(c("SM Individual", "SM Group", "TV"),3))
c3 <- c(rep(c("Index of TV show consumption"),3), 
        rep(c("Index of videos of women's empowerment and support consumption"),3),
        rep(c("Index of knowledge about treatment information"),3))

data <- cbind.data.frame(as.numeric(c1), c2, c3, rbind.data.frame(interval_lm1, interval_lm2, interval_lm3))

colnames(data) <- c("coefs", "treatment", "variables", "left", "right")
data$variables <- factor(data$variable, levels = c("Index of TV show consumption", 
                                                   "Index of videos of women's empowerment and support consumption", 
                                                   "Index of knowledge about treatment information"))
positions <- c("SM Individual", "SM Group", "TV")
data$treatment <- factor(data$treatment, levels = positions)


Figure2 <- ggplot(data, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin= left, ymax= right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(25))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("SM Individual", "SM Group", "TV"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(Figure2, path = 'Figures', filename = "Figure2.pdf", device = cairo_pdf, 
                 width=7, height=5,dpi=300)


# Figure 3
att1 <- lm(RF_att1_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
             husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
             factor(block_ids),
           weights = weight, data = merged)


att2 <- lm(RF_att2_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
             husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
             factor(block_ids),
           weights = weight, na.action = na.omit, data = merged)


don <-lm(RF_preference_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
           factor(block_ids), 
         weights = weight, 
         data = merged)


# Save lm regressions and retrieve coeffs and sd
lm1 <- summary(att1)
lm2 <- summary(att2)
lm3 <- summary(don)

coef_lm1 <- lm1$coefficients[2:4,1] 
interval_lm1 <- as.data.frame(confint(att1, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm2 <- lm2$coefficients[2:4,1] 
interval_lm2 <- as.data.frame(confint(att2, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm3 <- lm3$coefficients[2:4,1] 
interval_lm3 <- as.data.frame(confint(don, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm1 <- c(coef_lm1[[1]], coef_lm1[[2]], coef_lm1[[3]])
coef_lm2 <- c(coef_lm2[[1]], coef_lm2[[2]], coef_lm2[[3]])
coef_lm3 <- c(coef_lm3[[1]], coef_lm3[[2]], coef_lm3[[3]])


# Create an auxiliary data frame
c1 <- c(coef_lm1, coef_lm2, coef_lm3)
c2 <- c(rep(c("SM Individual", "SM Group", "TV"),3))
c3 <- c( rep("Index of attitudes toward gender and marital equality", 3), 
         rep("Index of attitudes on sexual violence", 3), 
         rep("Index of donation to organizations supporting women", 3))
data2 <- cbind.data.frame(as.numeric(c1), c2, c3, rbind.data.frame(interval_lm1, interval_lm2, interval_lm3))

colnames(data2) <- c("coefs", "treatment", "variables", "left", "right")
data2$variables <- factor(data2$variable, levels = c("Index of attitudes toward gender and marital equality", 
                                                     "Index of attitudes on sexual violence", 
                                                     "Index of donation to organizations supporting women"))
positions <- c("SM Individual", "SM Group", "TV")
data2$treatment <- factor(data2$treatment, levels = positions)



Figure3 <- ggplot(data2, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax= right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(28))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("SM Individual", "SM Group", "TV"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(Figure3, path = 'Figures', filename = "Figure3.pdf", device = cairo_pdf, 
                 width=7, height=5,dpi=300)

# Figure 4 
dcovid1 <- lm(RF_dcovid_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num  + 
                dcovid_yelled_end_num_refuse + dcovid_hit_end_num_refuse + dcovid_assault_end_num_refuse + 
                factor(block_ids),
              weights = weight, data = merged)

hypdv <-lm(RF_hypDM2_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hypsa <- lm(RF_hypSA2_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
              talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
              factor(block_ids),
            weights = weight,
            data = merged)

dcovid2 <-lm(RF_dcovid2_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
               know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
               dcovid_accessonline_end_num_refuse + dcovid_contactorg_end_num_refuse + know_org_noaut_valid_num +know_online_nehad_num + 
               know_org_nehad_num +
               factor(block_ids), 
             weights = weight, 
             data = merged)


# Save lm regressions and retrieve coeffs and sd
lm1 <- summary(dcovid1)
lm2 <- summary(hypdv)
lm3 <- summary(hypsa)
lm4 <- summary(dcovid2)

coef_lm1 <- lm1$coefficients[2:4,1] 
interval_lm1 <- as.data.frame(confint(dcovid1, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm2 <- lm2$coefficients[2:4,1]
interval_lm2 <- as.data.frame(confint(hypdv, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm3 <- lm3$coefficients[2:4,1] 
interval_lm3 <- as.data.frame(confint(hypsa, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm4 <- lm4$coefficients[2:4,1] 
interval_lm4 <- as.data.frame(confint(dcovid2, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm1 <- c(coef_lm1[[1]], coef_lm1[[2]], coef_lm1[[3]])
coef_lm2 <- c(coef_lm2[[1]], coef_lm2[[2]], coef_lm2[[3]])
coef_lm3 <- c(coef_lm3[[1]], coef_lm3[[2]], coef_lm3[[3]])
coef_lm4 <- c(coef_lm4[[1]], coef_lm4[[2]], coef_lm4[[3]])

# Create an auxiliary data frame
c1 <- c(coef_lm1, coef_lm2, coef_lm3,coef_lm4)
c2 <- c(rep(c("SM Individual", "SM Group", "TV"),4))
c3 <- c( rep("Index of  domestic and sexual violence experienced during COVID-19", 3), 
         rep("Index of hypothetical use of online resources and contact with an organization when responding to domestic violence", 3), 
         rep("Index of hypothetical use of online resources and contact with an organization when responding to sexual violence", 3),
         rep("Index of recent use of online resources and contact with an organization during COVID-19", 3))

data3 <- cbind.data.frame(as.numeric(c1), c2, c3, rbind.data.frame(interval_lm1, interval_lm2, interval_lm3, interval_lm4))
colnames(data3) <- c("coefs", "treatment", "variables", "left", "right")
data3$variables <- factor(data3$variable, levels = c("Index of  domestic and sexual violence experienced during COVID-19", 
                                                     "Index of hypothetical use of online resources and contact with an organization when responding to domestic violence", 
                                                     "Index of hypothetical use of online resources and contact with an organization when responding to sexual violence",
                                                     "Index of recent use of online resources and contact with an organization during COVID-19"))
positions <- c("SM Individual", "SM Group", "TV")
data3$treatment <- factor(data3$treatment, levels = positions)

Figure4 <- ggplot(data3, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(21))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("SM Individual", "SM Group", "TV"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(Figure4, path = 'Figures', filename = "Figure4.pdf", device = cairo_pdf, 
                 width=7, height=5,dpi=300)

# Figure 5
fo <- lm(RF_fo_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
           husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
           husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
           factor(block_ids),
         weights = weight, data = merged)


# Save lm regressions and retrieve coeffs and sd
lm1 <- summary(fo)

coef_lm1 <- lm1$coefficients[2:4,1] 
interval_lm1 <- as.data.frame(confint(fo, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm1 <- c(coef_lm1[[1]], coef_lm1[[2]], coef_lm1[[3]])


# Create an auxiliary data frame
c1 <- c(coef_lm1)
c2 <- c(rep(c("SM Individual", "SM Group", "TV"),1))
c3 <- c( rep("Index of views on women's future outlook toward gender and marital equality", 3))
data4 <- cbind.data.frame(as.numeric(c1), c2, c3, rbind.data.frame(interval_lm1))

colnames(data4) <- c("coefs", "treatment", "variables", "left", "right")
data4$variables <- factor(data4$variable, levels = c("Index of views on women's future outlook toward gender and marital equality"))
positions <- c("SM Individual", "SM Group", "TV")
data4$treatment <- factor(data4$treatment, levels = positions)


Figure5 <- ggplot(data4, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(85))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("SM Individual", "SM Group", "TV"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(Figure5, path = 'Figures', filename = "Figure5.pdf", device = cairo_pdf, 
                 width=7, height=5,dpi=300)

# Figure S4
hyp_1 <-lm(RF_hypDM1_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
             talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
             factor(block_ids), 
           weights = weight, 
           data = merged)

hyp_2 <- lm(RF_hypSA1_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 +
              talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
              factor(block_ids),
            weights = weight,
            data = merged)


# Save lm regressions and retrieve coeffs and sd
lm1 <- summary(hyp_1)
lm2 <- summary(hyp_2)

coef_lm1 <- lm1$coefficients[2:4,1] 
interval_lm1 <- as.data.frame(confint(hyp_1, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm2 <- lm2$coefficients[2:4,1] 
interval_lm2 <- as.data.frame(confint(hyp_2, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm1 <- c(coef_lm1[[1]], coef_lm1[[2]], coef_lm1[[3]])
coef_lm2 <- c(coef_lm2[[1]], coef_lm2[[2]], coef_lm2[[3]])

# Create an auxiliary data frame
c1 <- c(coef_lm1, coef_lm2)
c2 <- c(rep(c("SM Individual", "SM Group", "TV"),2))
c3 <- c( rep("Index of hypothetical talking to husband, family members, or reporting to authorities when responding to domestic violence", 3), 
         rep("Index of hypothetical talking to husband and family members, or reporting to authorities when responding to sexual violence", 3))

data5 <- cbind.data.frame(as.numeric(c1), c2, c3, rbind.data.frame(interval_lm1, interval_lm2))

colnames(data5) <- c("coefs", "treatment", "variables", "left", "right")

data5$variables <- factor(data5$variable, levels = c("Index of hypothetical talking to husband, family members, or reporting to authorities when responding to domestic violence", 
                                                     "Index of hypothetical talking to husband and family members, or reporting to authorities when responding to sexual violence"))
positions <- c("SM Individual", "SM Group", "TV")
data5$treatment <- factor(data5$treatment, levels = positions)

FigureS4 <- ggplot(data5, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(40))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("SM Individual", "SM Group", "TV"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(FigureS4, path = 'Figures', filename = "FigureA1.pdf", device = cairo_pdf, 
                  width=7, height=5,dpi=300)


# Figure S5
bcovid1 <- lm(RT_bcovid_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
                bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num  +
                bcovid_yelled_end_num_refuse + bcovid_hit_end_num_refuse + bcovid_assault_end_num_refuse + 
                factor(block_ids),
              weights = weight, data = merged)


bcovid2 <-lm(RT_bcovidAccess_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
               know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
               bcvovid_accessonline_end_num_refuse + bcovid_contactorg_end_num_refuse +know_org_noaut_valid_num +know_online_nehad_num + 
               know_org_nehad_num + 
               factor(block_ids), 
             weights = weight, 
             data = merged)



# Save lm regressions and retrieve coeffs and sd
lm1 <- summary(bcovid1)
lm2 <- summary(bcovid2)

coef_lm1 <- lm1$coefficients[2:4,1] 
interval_lm1 <- as.data.frame(confint(bcovid1, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))

coef_lm2 <- lm2$coefficients[2:4,1] 
interval_lm2 <- as.data.frame(confint(bcovid2, c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))


coef_lm1 <- c(coef_lm1[[1]], coef_lm1[[2]], coef_lm1[[3]])
coef_lm2 <- c(coef_lm2[[1]], coef_lm2[[2]], coef_lm2[[3]])



# Create an auxiliary data frame
c1 <- c(coef_lm1, coef_lm2)
c2 <- c(rep(c("SM Individual", "SM Group", "TV"),2))
c3 <- c( rep("Index of domestic and sexual violence experienced before COVID-19", 3), 
         rep("Index of recent use of online resources and contact with an organization before COVID-19", 3))

data6 <- cbind.data.frame(as.numeric(c1), c2, c3, rbind.data.frame(interval_lm1, interval_lm2))

colnames(data6) <- c("coefs", "treatment", "variables", "left", "right")

data6$variables <- factor(data6$variable, levels = c("Index of domestic and sexual violence experienced before COVID-19", 
                                                     "Index of recent use of online resources and contact with an organization before COVID-19"))
positions <- c("SM Individual", "SM Group", "TV")
data6$treatment <- factor(data6$treatment, levels = positions)


FigureS5 <- ggplot(data6, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(43))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c("SM Individual", "SM Group", "TV"),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(FigureS5, path = 'Figures', filename = "FigureA2.pdf", device = cairo_pdf, 
                  width=7, height=5,dpi=300)

dir.create(file.path("Datasets", "Finaldata and Zscores"), showWarnings = FALSE)


## ---- final data ---- 
# Save the new data with baseline + endline variables coded as our final dataset
write.csv(merged, file = 'Datasets/Finaldata and Zscores/Finaldata and Zscores.csv', row.names = F)
