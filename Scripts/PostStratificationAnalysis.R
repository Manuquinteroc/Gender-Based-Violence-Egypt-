# Run Analysis on main indexes:
# Summary of main indexes with IPW and weights1 and weights 2 to mimic Facebook ads sample

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
merged <- read.csv('Datasets/Poststratification/Endline_weights.csv', fileEncoding = "latin1")

covariates <- c("SM Individual", "SM Group", "TV")
treatment <- c("pooledF_WI", "in_group_10018", "reminder_10018")

# Run Analysis with new weights ------------------------------------------------

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
negative <- c(F,F,F,F,F,T,T,F,F,F,F) # six entrance is TRUE because of null effects across all T's
twosided <- c(F,F,F,F,F,T,T,F,F,F,F)

latex_font = "tiny"
omit_var <- c("Constant", "block_ids",  unlist(control_all), refuse7, refuse10, lagged[!is.na(lagged)])

# Run and Save table for summary, weight1, and weight2
source('Scripts/General Scripts/TreatmentEffectJointAlgorithmIndexes.R')

# Main figures -----------------------------------------------------------------
# Set up for ggplots
text.11 <- element_text(color = "black", size = 5, family="Arial")
final.text.11 <- element_text(color = "black", size = 7, hjust = 0.5, family="Arial")

th <-  theme(strip.text.x = final.text.11,
             strip.placement = "inside",
             strip.background = element_rect(colour = "black", fill = "white"),
             axis.text.x = text.11, 
             axis.text.y = text.11,
             axis.title.y = final.text.11,
             axis.title.x=element_blank(),
             legend.text = final.text.11,
             legend.title = final.text.11,
             axis.text = final.text.11, 
             panel.border = element_rect(colour = "black", fill = NA),
             legend.position="bottom")

legend_guide <- guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

colors <- c("#000000", "#0072B2","#D55E00")

y_text <- c("Treatment effect")

# ------------------------------------------------------------------------------
# Figure 2
c1 <- c(lm_A_main[[1]]$coefficients[2:4], lm_A_main[[2]]$coefficients[2:4], lm_A_main[[3]]$coefficients[2:4])
c2 <- rbind.data.frame(confint(lm_A_main[[1]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90),
                       confint(lm_A_main[[2]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90),
                       confint(lm_A_main[[3]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90))
c3 <- c(rep(c("SM Individual", "SM Group", "TV"),3))
c4 <- c(rep(c("Index of TV show consumption"),3), 
        rep(c("Index of videos of women's empowerment and support consumption"),3),
        rep(c("Index of knowledge about treatment information"),3))

data <- cbind.data.frame(as.numeric(c1), c2, c3, c4)

colnames(data) <- c("coefs", "left", "right", "treatment", "variables")
data$variables <- factor(data$variable, levels = c("Index of TV show consumption", 
                                                   "Index of videos of women's empowerment and support consumption", 
                                                   "Index of knowledge about treatment information"))
positions <- c("SM Individual", "SM Group", "TV")
data$treatment <- factor(data$treatment, levels = positions)


Figure2 <- ggplot(data, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(50))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c(expression(atop("SM Individual         ", "(n = 1242)          ")), 
                                                     expression(atop("SM Group          ", "(n = 1382)          ")), 
                                                     expression(atop("TV          ", "(n = 702)          "))),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

# ggsave(Figure2, path = 'Figures', filename = "Figure2.pdf", device = cairo_pdf, 
#        width=7, height=5,dpi=300)
ggsave(Figure2, path = 'Figures', filename = "Figure2.pdf", device = cairo_pdf, 
       width = 180, height = 150, units = "mm", dpi=350)

# Figure 3 ---------------------------------------------------------------------
c1 <- c(lm_A_main[[4]]$coefficients[2:4], lm_A_main[[5]]$coefficients[2:4], lm_A_main[[6]]$coefficients[2:4])

# rename columns of different interval significance level
aux <- confint(lm_A_main[[6]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95)
colnames(aux) <- c("5 %", "95 %")

c2 <- rbind.data.frame(confint(lm_A_main[[4]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90),
                       confint(lm_A_main[[5]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90),
                       aux)

c3 <- c(rep(c("SM Individual", "SM Group", "TV"),3))
c4 <- c( rep("Index of attitudes toward gender and marital equality", 3), 
         rep("Index of attitudes on sexual violence", 3), 
         rep("Index of donation to organizations supporting women", 3))

data2 <- cbind.data.frame(as.numeric(c1), c2, c3, c4)

colnames(data2) <- c("coefs", "left", "right", "treatment", "variables")
data2$variables <- factor(data2$variable, levels = c("Index of attitudes toward gender and marital equality", 
                                                     "Index of attitudes on sexual violence", 
                                                     "Index of donation to organizations supporting women"))
positions <- c("SM Individual", "SM Group", "TV")
data2$treatment <- factor(data2$treatment, levels = positions)


Figure3 <- ggplot(data2, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(50))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c(expression(atop("SM Individual         ", "(n = 1242)          ")), 
                                                     expression(atop("SM Group          ", "(n = 1382)          ")), 
                                                     expression(atop("TV          ", "(n = 702)          "))),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(Figure3, path = 'Figures', filename = "Figure3.pdf", device = cairo_pdf, 
       width = 180, height = 150, units = "mm", dpi=350)

# Figure 4 ---------------------------------------------------------------------

# rename columns of different interval significance level
aux <- confint(lm_A_main[[7]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95)
colnames(aux) <- c("5 %", "95 %")

c1 <- c(lm_A_main[[7]]$coefficients[2:4], lm_A_main[[8]]$coefficients[2:4], lm_A_main[[9]]$coefficients[2:4], lm_A_main[[10]]$coefficients[2:4])
c2 <- rbind.data.frame(aux,
                       confint(lm_A_main[[8]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90),
                       confint(lm_A_main[[9]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90),
                       confint(lm_A_main[[10]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90))

c3 <- c(rep(c("SM Individual", "SM Group", "TV"), 4))
c4 <- c( rep("Index of  domestic and sexual violence experienced during COVID-19", 3), 
        rep("Index of hypothetical use of online resources and contact with an organization when responding to domestic violence", 3), 
        rep("Index of hypothetical use of online resources and contact with an organization when responding to sexual violence", 3),
        rep("Index of recent use of online resources and contact with an organization during COVID-19", 3))

data3 <- cbind.data.frame(as.numeric(c1), c2, c3, c4)

colnames(data3) <- c("coefs", "left", "right", "treatment", "variables")
data3$variables <- factor(data3$variable, levels = c("Index of  domestic and sexual violence experienced during COVID-19", 
                                                     "Index of hypothetical use of online resources and contact with an organization when responding to domestic violence", 
                                                     "Index of hypothetical use of online resources and contact with an organization when responding to sexual violence",
                                                     "Index of recent use of online resources and contact with an organization during COVID-19"))
positions <- c("SM Individual", "SM Group", "TV")
data3$treatment <- factor(data3$treatment, levels = positions)


Figure4 <- ggplot(data3, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(30))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c(expression(atop("SM Individual         ", "(n = 1242)          ")), 
                                                     expression(atop("SM Group          ", "(n = 1382)          ")), 
                                                     expression(atop("TV          ", "(n = 702)          "))),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(Figure4, path = 'Figures', filename = "Figure4.pdf", device = cairo_pdf, 
       width = 180, height = 150, units = "mm", dpi=350)


# Figure 5 ---------------------------------------------------------------------
c1 <- c(lm_A_main[[11]]$coefficients[2:4])
c2 <- rbind.data.frame(confint(lm_A_main[[11]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.90))
c3 <- c(rep(c("SM Individual", "SM Group", "TV"), 1))
c4 <- c( rep("Index of views on women's future outlook toward gender and marital equality", 3))

data4 <- cbind.data.frame(as.numeric(c1), c2, c3, c4)

colnames(data4) <- c("coefs", "left", "right", "treatment", "variables")
data4$variables <- factor(data4$variable, levels = c("Index of views on women's future outlook toward gender and marital equality"))
positions <- c("SM Individual", "SM Group", "TV")
data4$treatment <- factor(data4$treatment, levels = positions)


Figure5 <- ggplot(data4, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(80))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c(expression(atop("SM Individual         ", "(n = 1242)          ")), 
                                                     expression(atop("SM Group          ", "(n = 1382)          ")), 
                                                     expression(atop("TV          ", "(n = 702)          "))),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(Figure5, path = 'Figures', filename = "Figure5.pdf", device = cairo_pdf, 
       width = 180, height = 150, units = "mm", dpi=350)

# Appendix Figures -------------------------------------------------------------
dep_vars_names <- c("RF_hypDM1_zscore",
                    "RF_hypSA1_zscore",
                    "RT_bcovid_zscore",
                    "RT_bcovidAccess_zscore")

dep_vars <- merged[, dep_vars_names]

control1 <- c("RF_hypDM1_zscore_base", "talk_husband_num", "talk_family_num", 
              "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

control2 <- c("talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")


control3 <- c("RT_bcovid_zscore_base", "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num" , 
              "age", "educ_aboveBA", "married")

control4 <- c("RT_bcovidAccess_zscore_base",
              "know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num", 
              "bc19_look_org_num", "dc19_look_org_num", "know_org_noaut_valid_num",
              "know_online_nehad_num", "know_org_nehad_num", 
              "age", "educ_aboveBA", "married")

control_all <- list(control1, control2, control3, control4)

lagged <- c("RF_hypDM1_zscore_base",
            NA,
            "RT_bcovid_zscore_base",
            "RT_bcovidAccess_zscore_base")

source('Scripts/General Scripts/LASSOAlgorithmIndexes.R')

refuse3 <-c("bcovid_yelled_end_num_refuse", "bcovid_hit_end_num_refuse", "bcovid_assault_end_num_refuse")
refuse4 <- c("bcvovid_accessonline_end_num_refuse", "bcovid_contactorg_end_num_refuse")
refuse <- list(NA, NA, refuse3, refuse4)

# Create tables and save results
size = length(dep_vars_names) + 1
negative <- c(T,T,T,T)
twosided <- c(T,T,T,T)

# Auxiliary lists to store lm objects for each panel
lm_A <- list()
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
    
    print(fmla1)
    
  } else { # If there are no non-response indicators:
    
    # Panel A
    if(!any(is.na(control_list[[count]]))) {
      fmla1 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + ", paste(control_list[[count]], collapse = " + "),
                                 "+ factor(block_ids)"))
    } else {
      fmla1 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "),
                                 "+ factor(block_ids)"))
    }
    
  }
  

  # Assign lm objects to different lists
  nam1 <- paste("lm_", count, "_A", sep = "")

  assign(nam1, lm(fmla1, weights = weight, data = merged))

  lm_A[[count]] <- get(nam1, envir = globalenv())

  count <- count + 1
  
}


# Figure A1 --------------------------------------------------------------------
c1 <- c(lm_A[[1]]$coefficients[2:4], lm_A[[2]]$coefficients[2:4])
c2 <- rbind.data.frame(confint(lm_A[[1]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95),
                       confint(lm_A[[2]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))
c3 <- c(rep(c("SM Individual", "SM Group", "TV"), 2))
c4 <- c( rep("Index of hypothetical talking to husband, family members, or reporting to authorities when responding to domestic violence", 3), 
         rep("Index of hypothetical talking to husband and family members, or reporting to authorities when responding to sexual violence", 3))


dataA1 <- cbind.data.frame(as.numeric(c1), c2, c3, c4)

colnames(dataA1) <- c("coefs", "left", "right", "treatment", "variables")
dataA1$variables <- factor(dataA1$variable, levels = c("Index of hypothetical talking to husband, family members, or reporting to authorities when responding to domestic violence",
                                                       "Index of hypothetical talking to husband and family members, or reporting to authorities when responding to sexual violence"))
positions <- c("SM Individual", "SM Group", "TV")
dataA1$treatment <- factor(dataA1$treatment, levels = positions)


FigureA1 <- ggplot(dataA1, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(70))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c(expression(atop("SM Individual         ", "(n = 1242)          ")), 
                                                     expression(atop("SM Group          ", "(n = 1382)          ")), 
                                                     expression(atop("TV          ", "(n = 702)          "))),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(FigureA1, path = 'Figures', filename = "FigureA1.pdf", device = cairo_pdf, 
       width = 180, height = 150, units = "mm", dpi=350)


# Figure A2 --------------------------------------------------------------------
c1 <- c(lm_A[[3]]$coefficients[2:4], lm_A[[4]]$coefficients[2:4])
c2 <- rbind.data.frame(confint(lm_A[[3]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95),
                       confint(lm_A[[4]], c("pooledF_WI", "in_group_10018", "reminder_10018"), level = 0.95))
c3 <- c(rep(c("SM Individual", "SM Group", "TV"), 2))
c4 <- c( rep("Index of domestic and sexual violence experienced before COVID-19", 3), 
         rep("Index of recent use of online resources and contact with an organization before COVID-19", 3))


dataA2 <- cbind.data.frame(as.numeric(c1), c2, c3, c4)

colnames(dataA2) <- c("coefs", "left", "right", "treatment", "variables")
dataA2$variables <- factor(dataA2$variable, levels = c("Index of domestic and sexual violence experienced before COVID-19",
                                                       "Index of recent use of online resources and contact with an organization before COVID-19"))
positions <- c("SM Individual", "SM Group", "TV")
dataA2$treatment <- factor(dataA2$treatment, levels = positions)


FigureA2 <- ggplot(dataA2, aes(treatment, coefs), color = factor(treatment)) + 
  geom_point(aes(color = factor(treatment)), size = 5, fill = "#d3d3d3") + 
  geom_errorbar(aes(ymin = left, ymax = right), width = .5) +
  facet_grid(. ~ variables, switch="both", labeller = labeller(variables = label_wrap_gen(60))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th + 
  scale_color_manual(name = "Treatment",  labels = c(expression(atop("SM Individual         ", "(n = 1242)          ")), 
                                                     expression(atop("SM Group          ", "(n = 1382)          ")), 
                                                     expression(atop("TV          ", "(n = 702)          "))),
                     values= colors) +
  geom_hline(yintercept=0, size = 0.8) +
  ylab(y_text) +
  legend_guide

ggsave(FigureA2, path = 'Figures', filename = "FigureA2.pdf", device = cairo_pdf, 
       width = 180, height = 150, units = "mm", dpi=350)