# Analysis on difference in covariates at baseline between the baseline sample and 
# sample + attriters

# Read attrition data set at baseline 
baseline <- read.csv('Datasets/Baseline/cb_attrition.csv', fileEncoding = "latin1")
baseline$attrition_num <- ifelse(baseline$attrition == "attrition" ,0,1) # Numeric

# Revalue values
mapping_time <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Very often" = 5, "Refuse" = 3)
mapping_time2 <- c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Very often" = 5)
map_agree <- c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly agree" = 5)
like <- c("Very unlikely" = 1, "Unlikely" = 2 , "Neither likely nor unlikely" = 3, "Likely" = 4 ,"Very likely" = 5)

# ------------------------------------------------------------------------------
# Rename survey questions with variable names
# Demographics
  # Q2 = age
  # Q3 = Married
  # Q16 = Education 

baseline$age <- as.numeric(baseline$Q2)
baseline$married <- ifelse(baseline$Q3 == "Married",1,0)

baseline <- mutate(baseline, educaBA =  
                     ifelse(Q16 == "BA" | Q16 == "MA and above", 1, 0),
                   educaBA = replace_na(educaBA, 0))

# Attitudes --------------------------------------------------------------------
# Q87 = husb_final_say_num
# Q93 = prioritize_educ_num
# Q88 = husb_provide_inc_num
# Q89 = husb_justified_yell_num
# Q90 = husb_justified_beat_num

baseline$husb_final_say_num <- as.numeric(revalue(baseline$Q87, map_agree))
baseline$prioritize_educ_num <- as.numeric(revalue(baseline$Q93, map_agree))
baseline$husb_provide_inc_num <- as.numeric(revalue(baseline$Q88, map_agree))
baseline$husb_justified_yell_num <- as.numeric(revalue(baseline$Q89, map_agree))
baseline$husb_justified_beat_num <- as.numeric(revalue(baseline$Q90, map_agree))

attitudes <- baseline[c("husb_final_say_num","prioritize_educ_num","husb_provide_inc_num",
                      "husb_justified_yell_num","husb_justified_beat_num")]
baseline$zscore_at <- scale(rowMeans(scale(attitudes) %*% diag(c(-1,-1,-1,-1,-1))))

# Experienced violence ---------------------------------------------------------
# Q97 = dc19_yell_num
# Q99 = dc19_hit_num
baseline$dc19_yell_num <- as.numeric(revalue(baseline$Q97, mapping_time2))
baseline$dc19_hit_num <- as.numeric(revalue(baseline$Q99, mapping_time2))

violence <- baseline[c("dc19_yell_num", "dc19_hit_num")]
baseline$zscore_violence <- scale(rowMeans(scale(violence) %*% diag(c(1,1))))

# Resource knowledge  ----------------------------------------------------------
# Q47 = know_online_valid_noaut_num
# Q110 = know_org_noaut_valid_num
# Q = know_online_nehad_num (need to transalte and recode; no need for comparison)
# Q = know_org_nehad_num (need to transalte and recode; no need for comparison)
baseline$know_online_valid_noaut_num <- 0
baseline$know_online_valid_noaut_num[which(baseline$Q47 != "I do not know any.")] <- 1

baseline$know_org_noaut_valid_num <- 0
baseline$know_org_noaut_valid_num[which(baseline$Q110 != "I do not know any.")] <- 1

knowledge <- baseline[c("know_online_valid_noaut_num","know_org_noaut_valid_num")]
baseline$zscore_know <- scale(rowMeans(scale(knowledge)%*% diag(c(1,1))))

# Hypothetical use and contact  ------------------------------------------------
# Q105 = look_online_num
# Q107 = contact_org_num
baseline$look_online_num <- as.numeric(revalue(baseline$Q105, like))
baseline$contact_org_num <- as.numeric(revalue(baseline$Q107, like))

hypothetical <- baseline[c("look_online_num", "contact_org_num")]
baseline$zscore_rep1 <- scale(rowMeans(scale(hypothetical) %*% diag(c(1,1))))

# Recent use and contact -------------------------------------------------------
# Q50 = dc19_look_online_num
# Q53 = dc19_look_org_num
baseline$dc19_look_online_num <- as.numeric(revalue(baseline$Q50, mapping_time))
baseline$dc19_look_org_num <- as.numeric(revalue(baseline$Q53, mapping_time))

reporting2 <- baseline[c("dc19_look_online_num", "dc19_look_org_num")]
baseline$zscore_rep2 <- scale(rowMeans(scale(reporting2)%*% diag(c(1,1))))

# Run analysis -----------------------------------------------------------------
dep_vars_names <- c("age", "married", "educaBA", 
                    "zscore_at", "zscore_violence", "zscore_know", "zscore_rep1", "zscore_rep2")

dep_vars <- baseline[, dep_vars_names]

range <- c(paste0("[", min(baseline$age), ",", max(baseline$age), "]"),
           rep("{0,1}", 2),
           paste0("[", round(min(baseline$zscore_at),2), ",", round(max(baseline$zscore_at),2), "]"),
           paste0("[", round(min(baseline$zscore_violence),2), ",", round(max(baseline$zscore_violence),2), "]"),
           paste0("[", round(min(baseline$zscore_know),2), ",", round(max(baseline$zscore_know),2), "]"),
           paste0("[", round(min(baseline$zscore_rep1),2), ",", round(max(baseline$zscore_rep1),2), "]"),
           paste0("[", round(min(baseline$zscore_rep2),2), ",", round(max(baseline$zscore_rep2),2), "]"))

outcome_means <- round(colMeans(dep_vars), digits = 3)

list_ols <- list()
count <- 1

for (x in dep_vars_names) {
  fmla1 <- as.formula(paste0(x, "~ ", "+ attrition_num "))
  
  nam1 <- paste("lm_", count, sep = "")
  assign(nam1, lm(fmla1,  data = baseline))
  
  list_ols[[count]] <- get(nam1, envir = globalenv())
  
  count <- count + 1
}


dep_var <- c("Age", 
             "Married", 
             "Education",
             "Attitudes",
             "\\shortstack{Experienced \\\\violence}",
             "\\shortstack{Resource \\\\ knowledge}",
             "\\shortstack{Hypothetical use \\\\ and contact}",
             "\\shortstack{Recent use \\\\ and contact}")

covariates <- "In sample"

omit_var <- c("Constant")

tableC <- stargazer(list_ols,
                    header = FALSE,
                    font.size ="scriptsize",
                    label = "table:baseline_att",
                    dep.var.caption = "",
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq"),
                    column.sep.width = "0pt",
                    add.lines = list(c("Outcome Mean", outcome_means),
                                     c("Outcome Range", range)),
                    title = "Baseline covariates comparison between participants who provided valid responses and those who opted in to receive receive additional information and videos about women's issues in Egypt",
                    type = "latex")

note.latex <- "\\multicolumn{9}{l} {\\parbox[t]{16cm}{ \\textit{Notes:}
We report estimates from OLS regressions. 
Columns $1$ to $3$ are demographic variables. Column $4$ to $8$ are the main baseline indexes on 
attitudes towards gender and marital equality (Attitudes), 
domestic violence experienced during COVID-19 (Experienced violence), 
knowledge on treatment information (Resource knowledge), 
hypothetical use of online resources and contact with an organization when responding to domestic violence (Hypothetical use and contact), 
and recent use of online resources and contact with an organization variables (Recent use and contact).  
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
tableC[grepl("Note", tableC)] <- note.latex

cat(tableC)



# Figures ----------------------------------------------------------------------

basline_sample <- baseline %>% subset(attrition_num == 1)


# ggplot setup -----------------------------------------------------------------

# We define ggplot setup
legend.text.18 <- element_text(color = "black", size = 18, family="Arial")
final.text.14 <- element_text(color = "black", size = 14, hjust = 0.5, family="Arial")

# Define colors for all graphs
colors <- c("#000000", "#0072B2","#D55E00")
th_egypt <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.title.x=element_blank(), 
                  axis.title.y=element_blank(),
                  panel.border = element_blank())

# set the background color for all graphs
fill_all <- c("white")

# Age
g1A <- ggplot() +
  geom_density(aes(age, color = "AB"), size = 1, fill = fill_all, alpha = .4, data = basline_sample) +
  geom_density(aes(age, color = "ABSM"),  size = 1, fill = fill_all,  alpha = .4, data = baseline) +
  th_egypt +
  ylab("Density") + 
  scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.8,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset",  labels = c("Sample with 5618","Sample with 9431"),
                     values= colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  guides(colour = guide_legend(override.aes = list(size= 10,linetype = 1)))

ggsave(g1A, path = folder, filename = "Baseline Comparison/Baseline_A.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Married
map <- c("Married" = 3, "Separated" = 4, "Widowed" = 5, "Engaged"= 2, "Divorced" = 4, 
         "Single - living independently of family" = 1, 
         "Single - living with family" = 1)

basline_sample$socialstat <- as.numeric(revalue(basline_sample$Q3, map))
baseline$socialstat <- as.numeric(revalue(baseline$Q3, map))

x1 <- round(as.data.frame(fre(basline_sample$socialstat)[,3][1:5][[1]]),3)
x2 <- round(as.data.frame(fre(baseline$socialstat)[,3][1:5][[1]]),3)

data1 <- as.data.frame(cbind(x1,x2))
colnames(data1) <- c("sample", "initial")
rownames(data1) <- c("Single", "Engaged", "Married", "Separated", "Widowed")

positions <- c("Single", "Engaged", "Married", "Separated", "Widowed")

g1B <- ggplot(data1, aes(rownames(data1), sample)) +
  geom_col(aes(color = "AB"),  size = 1, fill = fill_all,
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = initial, color = "ABSM"),  size = 1, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  scale_x_discrete(limits = positions) + 
  th_egypt +
  scale_color_manual(name = "Dataset", labels = c("Sample with 5618","Sample with 9431"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1B, path = folder, filename = "Baseline Comparison/Baseline_B.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Education
map_educ <- c("No formal education" = 1, "Elementary" = 2, 
              "Preparatory/Basic" = 3, "Higher technical institutes"= 4, 
              "Vocational secondary" = 4, "General secondary" = 4, "BA" = 5, "MA and above" = 6)

basline_sample$socialstat <- as.numeric(revalue(basline_sample$Q16, map_educ))
baseline$socialstat <- as.numeric(revalue(baseline$Q16, map_educ))

basline_sample$socialstat[is.na(basline_sample$socialstat)] <- 7
baseline$socialstat[is.na(baseline$socialstat)] <- 7

x1 <- round(as.data.frame(fre(basline_sample$socialstat)[,3][1:6][[1]]),3)
x2 <- round(as.data.frame(fre(baseline$socialstat)[,3][1:6][[1]]),3)

data2 <- as.data.frame(cbind(x1,x2))
colnames(data2) <- c("sample", "initial")
rownames(data2) <- c("None", "Elementary", "Preparatory", "Secondary", "BA", "MA and above")

positions <- c("None", "Elementary", "Preparatory",
               "Secondary", "BA",
               "MA and above")

g1C <- ggplot(data2, aes(rownames(data2), sample)) +
  geom_col(aes(color = "AB"),  size = 1, fill = fill_all,
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = initial, color = "ABSM"),  size = 1, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  scale_x_discrete(limits = positions) + 
  th_egypt +
  scale_color_manual(name = "Dataset", labels = c("Sample with 5618","Sample with 9431"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1C, path = folder, filename = "Baseline Comparison/Baseline_C.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)


# Attitudes
g1D <- ggplot() +
  geom_density(aes(zscore_at, color = "AB"), size = 1, fill = fill_all, alpha = .4, data = basline_sample) +
  geom_density(aes(zscore_at, color = "ABSM"),  size = 1, fill = fill_all,  alpha = .4, data = baseline) +
  th_egypt +
  ylab("Density") + 
  #scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.8,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset",  labels = c("Sample with 5618","Sample with 9431"),
                     values= colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1D, path = folder, filename = "Baseline Comparison/Baseline_D.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Experienced violence
g1E <- ggplot() +
  geom_density(aes(zscore_violence, color = "AB"), size = 1, fill = fill_all, alpha = .4, data = basline_sample) +
  geom_density(aes(zscore_violence, color = "ABSM"),  size = 1, fill = fill_all,  alpha = .4, data = baseline) +
  th_egypt +
  ylab("Density") + 
  #scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.8,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset",  labels = c("Sample with 5618","Sample with 9431"),
                     values= colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1E, path = folder, filename = "Baseline Comparison/Baseline_E.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Resource knowledge
g1F <- ggplot() +
  geom_density(aes(zscore_know, color = "AB"), size = 1, fill = fill_all, alpha = .4, data = basline_sample) +
  geom_density(aes(zscore_know, color = "ABSM"),  size = 1, fill = fill_all,  alpha = .4, data = baseline) +
  th_egypt +
  ylab("Density") + 
  #scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.8,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset",  labels = c("Sample with 5618","Sample with 9431"),
                     values= colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1F, path = folder, filename = "Baseline Comparison/Baseline_F.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Hypothetical use and contact
g1G <- ggplot() +
  geom_density(aes(zscore_rep1, color = "AB"), size = 1, fill = fill_all, alpha = .4, data = basline_sample) +
  geom_density(aes(zscore_rep1, color = "ABSM"),  size = 1, fill = fill_all,  alpha = .4, data = baseline) +
  th_egypt +
  ylab("Density") + 
  #scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.8,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset",  labels = c("Sample with 5618","Sample with 9431"),
                     values= colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1G, path = folder, filename = "Baseline Comparison/Baseline_G.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Recent use and contact
g1H <- ggplot() +
  geom_density(aes(zscore_rep2, color = "AB"), size = 1, fill = fill_all, alpha = .4, data = basline_sample) +
  geom_density(aes(zscore_rep2, color = "ABSM"),  size = 1, fill = fill_all,  alpha = .4, data = baseline) +
  th_egypt +
  ylab("Density") + 
  #scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.8,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset",  labels = c("Sample with 5618","Sample with 9431"),
                     values= colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1H, path = folder, filename = "Baseline Comparison/Baseline_H.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

