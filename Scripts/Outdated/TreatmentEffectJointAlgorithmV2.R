# Run all regressions for the panel A,B,C

# Auxiliary lists to store lm objects for each panel
lm_A <- list()
lm_B <- list()
lm_C <- list()

count <- 1

# Run all regresions
for (x in dep_vars_names) {
  
  if (!is.null(refuse)) { # If there are non-response indicators:
    
    # Panel A
    if(!is.na(controls)) {
      fmla1 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + ", paste(controls, collapse = " + "),
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
    if(!is.na(controls)) {
      fmla1 <- as.formula(paste0(x, "~ ", paste(treatment, collapse = " + "), " + ", paste(controls, collapse = " + "),
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
  
  count <- count + 1
  
}

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
                    p = OneSided_pvals,
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq", "n"),
                    column.sep.width = "0pt",
                    add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                     c("SM Individual = TV (p-value)", pvals$X2), 
                                     c("SM Group= TV (p-value)", pvals$X3)),
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
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq", "n"),
                    column.sep.width = "0pt",
                    add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                     c("SM Individual = TV (p-value)", pvals$X2), 
                                     c("SM Group= TV (p-value)", pvals$X3)),
                    title = title,
                    type = "latex")

# Panel C Table: 
pvals <- data.frame(matrix(NA, nrow = length(dep_vars_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(dep_vars_names)) {
    paux <- round(linearHypothesis(lm_B[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
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
                    dep.var.labels.include = FALSE,
                    table.placement = "H",
                    column.labels = dep_var,
                    covariate.labels = covariates,
                    omit = omit_var,
                    omit.stat = c("f", "ser","adj.rsq"),
                    column.sep.width = "0pt",
                    add.lines = list(c("Control Mean", 0, control_means),
                                     c("SM Individual = SM Group (p-value)", pvals$X1),
                                     c("SM Individual = TV (p-value)", pvals$X2), 
                                     c("SM Group= TV (p-value)", pvals$X3)),
                    title = title,
                    type = "latex")

note.latex <- paste0("\\multicolumn{", size, "}{l} {\\parbox[t]{", cm, "cm}{ \\textit{Notes:}
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
Regressions in Panel A use as controls the covariates selected by LASSO in which the treatment indicators,
lagged dependent variable, and fixed effects are forced into model and covariates are selected from the outcome family.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
tableC[grepl("Note", tableC)] <- note.latex

