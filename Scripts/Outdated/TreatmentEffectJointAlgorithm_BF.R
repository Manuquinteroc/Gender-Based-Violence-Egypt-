# Run all regressions for the panel A,B,C
library(BayesFactor)
options("scipen"=100, "digits"= 5)

# Auxiliary lists to store lm objects for each panel
lm_A <- list()
lm_B <- list()
lm_C <- list()

# auxiliary formula list for Bayes Factor 
fmla1_list <- list()
fmla2_list <- list()
fmla3_list <- list()

count <- 1
# Run all regresions
for (x in dep_vars_names) {
  
  if (!is.null(refuse)) { # If there are non-response indicators:
    
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
  
  lm_A[[count]] <- get(nam1, envir = globalenv())
  lm_B[[count]] <- get(nam2, envir = globalenv())
  lm_C[[count]] <- get(nam3, envir = globalenv())
  
  # Auxiliary list of formulas 
  fmla1_list[[count]] <- fmla1
  fmla2_list[[count]] <- fmla2
  fmla3_list[[count]] <- fmla3
  
  count <- count + 1
  
}

# Apply Bayes factor
#  Clean formulas for full equations
fmla1_list <- lapply(fmla1_list, function(x){ 
  text <- paste(deparse(x), collapse = "")
  text <- str_replace_all(text, " ", "")
  str_replace(text, "factor\\(block_ids\\)", "block_ids") 
})

fmla2_list <- lapply(fmla2_list, function(x){ 
  text <- paste(deparse(x), collapse = "")
  text <- str_replace_all(text, " ", "")
  str_replace(text, "factor\\(block_ids\\)", "block_ids") 
})

fmla3_list <- lapply(fmla3_list, function(x){ 
  text <- paste(deparse(x), collapse = "")
  text <- str_replace_all(text, " ", "")
  str_replace(text, "factor\\(block_ids\\)", "block_ids") 
})

# Create null equations by removing treatments from all formulas
fmla1_list_null <- lapply(fmla1_list, function(x){
  str_replace(x, "pooledF_WI\\+in_group_10018\\+reminder_10018\\+", "")
})

fmla2_list_null <- lapply(fmla2_list, function(x){
  str_replace(x, "pooledF_WI\\+in_group_10018\\+reminder_10018\\+", "")
})

fmla3_list_null <- lapply(fmla3_list, function(x){
  str_replace(x, "pooledF_WI\\+in_group_10018\\+reminder_10018\\+", "")
})

# Needed for bayesFactor
merged$block_ids <- as.factor(merged$block_ids)

# Empty vectors to store Bayes Factors
BF_1 <- c()
BF_2 <- c()
BF_3 <- c()

# Compute quotient (BF) for all regressions comparing T = 0 and T \neq 0
set.seed(32)
count <- 1
for (count in 1:length(dep_vars_names)) { # calculates length(dep_vars_names) x 6
  nam1_BF <- paste("lm_", count, "_A_BF", sep = "")
  nam2_BF <- paste("lm_", count, "_B_BF", sep = "")
  nam3_BF <- paste("lm_", count, "_C_BF", sep = "")
  
  assign(nam1_BF, lmBF(as.formula(fmla1_list[[count]]), data = merged, whichRandom = NULL)) # iterations = 1000
  assign(nam2_BF, lmBF(as.formula(fmla2_list[[count]]), data = merged, whichRandom = NULL)) # whichRandom = "block_ids"
  assign(nam3_BF, lmBF(as.formula(fmla3_list[[count]]), data = merged, whichRandom = NULL))  
  
  nam1_BF_null <- paste("lm_", count, "_A_BF_null", sep = "")
  nam2_BF_null <- paste("lm_", count, "_B_BF_null", sep = "")
  nam3_BF_null <- paste("lm_", count, "_C_BF_null", sep = "")
  
  assign(nam1_BF_null, lmBF(as.formula(fmla1_list_null[[count]]), data = merged, whichRandom = NULL))
  assign(nam2_BF_null, lmBF(as.formula(fmla2_list_null[[count]]), data = merged, whichRandom = NULL))
  assign(nam3_BF_null, lmBF(as.formula(fmla3_list_null[[count]]), data = merged, whichRandom = NULL))  
  
  BF_1 = c(BF_1, get(paste("lm_", count, "_A_BF", sep = "")) / get(paste("lm_", count, "_A_BF_null", sep = "")))
  BF_2 = c(BF_2, get(paste("lm_", count, "_B_BF", sep = "")) / get(paste("lm_", count, "_B_BF_null", sep = "")))
  BF_3 = c(BF_3, get(paste("lm_", count, "_C_BF", sep = "")) / get(paste("lm_", count, "_C_BF_null", sep = "")))

  count <- count + 1
}


# Retrieve all BF values from the S4 objects returned from lmBF
BF_1_values <- lapply(BF_1, function(x){
  as.data.frame(x)$bf
})

BF_2_values <- lapply(BF_2, function(x){
  as.data.frame(x)$bf
})

BF_3_values <- lapply(BF_3, function(x){
  as.data.frame(x)$bf
})

# From lists to vectors
BF_1_values <- unlist(BF_1_values)
BF_2_values <- unlist(BF_2_values)
BF_3_values <- unlist(BF_3_values)

# If BF is greater than 100 change to ">100", otherwise leave actual value
BF_1_values <- ifelse(BF_1_values >= 100, "$>$ 100", round(BF_1_values, 3))
BF_2_values <- ifelse(BF_2_values >= 100, "$>$ 100", round(BF_2_values, 3))
BF_3_values <- ifelse(BF_3_values >= 100, "$>$ 100", round(BF_3_values, 3))

# Number of LASSO variables selected
lasso_number <- c(unlist(lapply(control_list, length))) - 1
lasso_number[lasso_number < 0] <- 0

# Run linear hypothesis tests
pvals <- data.frame(matrix(NA, nrow = length(dep_vars_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(dep_vars_names)) {
    paux <- round(linearHypothesis(lm_A[[x]], test[i], digits = 3, singular.ok = T)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

# Calculate one sided tests 
OneSided_pvals <- lapply(lm_A, oneSidedTest, negative, twosided)

tableA <- stargazer(lm_A,
                    header = FALSE,
                    font.size = latex_font,
                    dep.var.caption = "",
                    label = paste0("tab:", label),
                    p = OneSided_pvals,
                    ci = TRUE, 
                    ci.level= ci.level,
                    report = c("vc*sp"),
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq", "n"),
                    column.sep.width = "0pt",
                    add.lines = list(c("SM Individual = SM Group \\\\(p-value)", pvals$X1),
                                     c("SM Individual = TV \\\\(p-value)", pvals$X2), 
                                     c("SM Group= TV \\\\(p-value)", pvals$X3),
                                     c("Num. Lasso covariates", lasso_number),
                                     c("Bayes Factor", BF_1_values)),
                    title = title,
                    type = "latex")


# Panel B Table: 
pvals <- data.frame(matrix(NA, nrow = length(dep_vars_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(dep_vars_names)) {
    paux <- round(linearHypothesis(lm_B[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

# Calculate one sided tests 
OneSided_pvals <- lapply(lm_B, oneSidedTest, negative, twosided)

tableB <- stargazer(lm_B,
                    header = FALSE,
                    font.size ="tiny",
                    dep.var.caption = "",
                    p = OneSided_pvals,
                    ci = TRUE, 
                    ci.level= ci.level,
                    report = c("vc*sp"),
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq", "n"),
                    column.sep.width = "0pt",
                    add.lines = list(c("SM Individual = SM Group \\\\(p-value)", pvals$X1),
                                     c("SM Individual = TV \\\\(p-value)", pvals$X2), 
                                     c("SM Group= TV \\\\(p-value)", pvals$X3),
                                     c("Bayes Factor", BF_2_values)),
                    title = title,
                    type = "latex")

# Panel C Table: 
pvals <- data.frame(matrix(NA, nrow = length(dep_vars_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(dep_vars_names)) {
    paux <- round(linearHypothesis(lm_C[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

# Calculate one sided tests 
OneSided_pvals <- lapply(lm_C, oneSidedTest, negative, twosided)

tableC <- stargazer(lm_C,
                    header = FALSE,
                    font.size ="tiny",
                    dep.var.caption = "",
                    p = OneSided_pvals,
                    ci = TRUE, 
                    ci.level= ci.level,
                    report = c("vc*sp"),
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq"),
                    column.sep.width = "0pt",
                    add.lines = list(c("Control Mean", control_means),
                                     c("SM Individual = SM Group \\\\ (p-value)", pvals$X1),
                                     c("SM Individual = TV \\\\(p-value)", pvals$X2), 
                                     c("SM Group= TV \\\\(p-value)", pvals$X3),
                                     c("Bayes Factor", BF_3_values)),
                    title = title,
                    type = "latex")

note.latex <- paste0("\\multicolumn{", size, "}{l} {\\parbox[t]{", cm, "cm}{ \\textit{Notes:}
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 95\\% confidence intervals are in parenthesis. 
Regressions in Panel A use as controls the covariates selected by LASSO in which the treatment indicators,
lagged dependent variable, and fixed effects are forced into model and covariates are selected from the outcome family.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
tableC[grepl("Note", tableC)] <- note.latex

