# Run power test to verify our results are sufficiently powered. 
# library(lsr) # cohensD function

# Auxiliary Functions ----------------------------------------------------------
# Algorithm for each power test
power <- function(variable_name, n_treatment, ind_treatment, sided = "two.sided", alpha = 0.05, power_set = 0.8){
  attach(merged) # to use get(.) in our variable_name
  var <- get(variable_name) # Get variable vector
  detach(merged) # don't forget to detach data
  power <- pwr.t2n.test(n1 = n_control, n2 = n_treatment, alternative = sided, sig.level = alpha, power = power_set, d = NULL) # Compute min power
  
  return(power$d)
}

# Auxiliary function to add notes
add_notes <- function(xtable, note) {
  
  xtable <- gsub("\\\\end\\{table", "\\\\justify {\\\\footnotesize \\\\textit{Notes:} %} \\\\end\\{table", xtable)
  xtable <- gsub("%", note, xtable)  
  
  return(xtable) 
}

# ----------------------------------------------------------------------------
# Read final dataframe
merged <- read.csv('Datasets/Finaldata and Zscores/Finaldata and Zscores.csv', fileEncoding = "latin1")

# Sample sizes for each treatment group
n_control <- sum(merged$control_10018)
n_F_wI <- sum(merged$pooledF_WI)
n_WG <- sum(merged$in_group_10018)
n_TV <- sum(merged$reminder_10018)
n_tret <- n_F_wI + n_WG + n_TV

# Auxiliary indexes for each treatment group
ind_control <- which(merged$control_10018 == 1)
ind_F_WI <- which(merged$pooledF_WI == 1)
ind_WG <- which(merged$in_group_10018 == 1)
ind_TV <- which(merged$reminder_10018 == 1)
ind_treat <- which(merged$control_10018 == 0)

# Create vectors to store powers
power_F_WI <- c()
power_WG <- c()
power_TV <- c()

# sd(merged$RF_att1_zscore[ind_control])
# sd(merged$RF_att2_zscore[ind_control])
# sd(merged$RF_preference_zscore[ind_control])
# sd(merged$RF_dcovid_zscore[ind_control])
# sd(merged$RF_hypSA2_zscore[ind_control])
# sd(merged$RF_fo_zscore[ind_control])
# sd(merged$RT_bcovid_zscore[ind_control])
# sd(merged$RF_hypDM1_zscore[ind_control])
# sd(merged$RF_hypSA1_zscore[ind_control])
# sd(merged$RT_bcovidAccess_zscore[ind_control])


# Index of attitudes toward gender and marital equality (F_WI, TV)
power_F_WI <- c(power_F_WI, power("RF_att1_zscore", n_F_wI, ind_F_WI))
power_WG <- c(power_WG, power("RF_att1_zscore", n_WG, ind_WG))
power_TV <- c(power_TV, power("RF_att1_zscore", n_TV, ind_TV))

# Index of atittudes on sexual violence (F_WI, WG)
power_F_WI <- c(power_F_WI, power("RF_att2_zscore", n_F_wI, ind_F_WI))
power_WG <- c(power_WG, power("RF_att2_zscore", n_WG, ind_WG))
power_TV <- c(power_TV, power("RF_att2_zscore", n_TV, ind_TV))

# Index of donation to organizations supporting women (All)
power_F_WI <- c(power_F_WI, power("RF_preference_zscore", n_F_wI, ind_F_WI, sided = "two.sided"))
power_WG <- c(power_WG, power("RF_preference_zscore", n_WG, ind_WG, sided = "two.sided"))
power_TV <- c(power_TV, power("RF_preference_zscore", n_TV, ind_TV, sided = "two.sided"))

# Index of domestic and sexual violence experienced during COVID-19 (All)
power_F_WI <- c(power_F_WI, power("RF_dcovid_zscore", n_F_wI, ind_F_WI, sided = "two.sided"))
power_WG <- c(power_WG, power("RF_dcovid_zscore", n_WG, ind_WG, sided = "two.sided"))
power_TV <- c(power_TV, power("RF_dcovid_zscore", n_TV, ind_TV, sided = "two.sided"))

# Index of hypothetical use of online resources and contact with an organization when responding to sexual violence (TV)
power_TV <- c(power_TV, power("RF_hypSA2_zscore", n_TV, ind_TV))

# Index of views on women’s future outlook toward gender and marital equality (WG)
power_WG <- c(power_WG, power("RF_fo_zscore", n_WG, ind_WG))

# Index of domestic and sexual violence experienced before COVID-19 (WG, TV)
power_WG <- c(power_WG, power("RT_bcovid_zscore", n_WG, ind_WG, sided = "two.sided"))
power_TV <- c(power_TV, power("RT_bcovid_zscore", n_TV, ind_TV, sided = "two.sided"))

# Index of hypothetical talking to husband, family members, or reporting to authorities when responding to domestic violence (All)
power_F_WI <- c(power_F_WI, power("RF_hypDM1_zscore", n_F_wI, ind_F_WI, sided = "two.sided"))
power_WG <- c(power_WG, power("RF_hypDM1_zscore", n_WG, ind_WG, sided = "two.sided"))
power_TV <- c(power_TV, power("RF_hypDM1_zscore", n_TV, ind_TV, sided = "two.sided"))

# Index of hypothetical talking to husband, family members, or reporting to authorities when responding to sexual violence (All)
power_F_WI <- c(power_F_WI, power("RF_hypSA1_zscore", n_F_wI, ind_F_WI, sided = "two.sided"))
power_WG <- c(power_WG, power("RF_hypSA1_zscore", n_WG, ind_WG, sided = "two.sided"))
power_TV <- c(power_TV, power("RF_hypSA1_zscore", n_TV, ind_TV, sided = "two.sided"))

# Index of recent use of online resources and contact with an organization before COVID-19 (All)
power_F_WI <- c(power_F_WI, power("RT_bcovidAccess_zscore", n_F_wI, ind_F_WI, sided = "two.sided"))
power_WG <- c(power_WG, power("RT_bcovidAccess_zscore", n_WG, ind_WG, sided = "two.sided"))
power_TV <- c(power_TV, power("RT_bcovidAccess_zscore", n_TV, ind_TV, sided = "two.sided"))

# Round to 3 decimal places
power_F_WI <- round(power_F_WI, 3)
power_WG <- round(power_WG, 3)
power_TV <- round(power_TV, 3)

# Concatenate empty values
power_F_WI_final <- c("-","-","-", power_F_WI[1], power_F_WI[2], power_F_WI[3], power_F_WI[4],"-","-","-","-","-", power_F_WI[5], power_F_WI[6], power_F_WI[7])
power_WG_final <- c("-","-","-", power_WG[1], power_WG[2], power_WG[3], power_WG[4],"-","-","-", power_WG[5], power_WG[6], power_WG[7], power_WG[8], power_WG[9])
power_TV_final <- c("-","-","-", power_TV[1], power_TV[2], power_TV[3], power_TV[4],"-", power_TV[5],"-","-", power_TV[6], power_TV[7], power_TV[8], power_TV[9])

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

table1 <- cbind.data.frame(Indexes, power_F_WI_final, power_WG_final, power_TV_final)
colnames(table1) <- columns

table1 <- xtable(table1, 
                 digits = M, 
                 label = "tab:PowerTests",
                 align= c("c", "p{0.6\\textwidth}", "p{0.11\\textwidth}", "p{0.11\\textwidth}", "p{0.11\\textwidth}"),
                 caption = "Power tests for statistically insignificant coefficients in our main indexes estimates")

table1 <- print(table1, 
                table.placement = "H",
                size = "scriptsize",
                caption.placement = "top",
                include.rownames = FALSE,
                comment = FALSE, 
                sanitize.colnames.function=function(x){x},
                hline.after = c(-1,0,15))

note <- c("We present the minimum detectable effect given our sample size, a significance level of $0.05$, and power of $0.80$. We perform a two-sided test since we pre-specify a two-sided hypothesis for statistically insignificant effects.")
table1 <- add_notes(table1, note)

# Save table
cat(table1, file = "Tables/powerTest.tex")



