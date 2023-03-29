# ------------------------------------------------------------------------------
# Main indexes weight: Summary table for indexes -------------------------------

# Auxiliary lists to store lm objects for each panel
lm_A_main <- list()
lm_B_main <- list()
lm_C_main <- list()

count <- 1

# Run all regresions
for (x in dep_vars_names) {
  
  if (!any(is.na(refuse[[count]]))) { # If there are non-response indicators:
    
    # Panel A
    if (!any(is.na(control_list[[count]]))) {
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
  
  count <- count + 1
  
}

# Number of LASSO variables selected
lasso_number <- c(unlist(lapply(control_list, length))) - 1
lasso_number[lasso_number < 0] <- 0

# Run linear hypothesis tests
pvals <- data.frame(matrix(NA, nrow = length(dep_vars_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(dep_vars_names)) {
    paux <- round(linearHypothesis(lm_A_main[[x]], test[i], digits = 3, singular.ok = T)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

# Calculate one sided tests 
#OneSided_pvals <- lapply(lm_A, oneSidedTest, negative, twosided)
OneSided_pvals <- list()
for (i in 1:length(dep_vars_names)) {
  OneSided_pvals[[i]] <- oneSidedTest(lm_A_main[[i]], negative[i], twosided[i])
}

title <- "Treatment effect on main indexes including $210$ rrespondents who responded more than once to the endline"

tableA <- stargazer(lm_A_main,
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
                    add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                     c("SM Individual = TV (p-value)", pvals$X2), 
                                     c("SM Group= TV (p-value)", pvals$X3),
                                     c("Num. Lasso covariates", lasso_number)),
                    title = title,
                    type = "latex")


# Panel B Table: 
pvals <- data.frame(matrix(NA, nrow = length(dep_vars_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(dep_vars_names)) {
    paux <- round(linearHypothesis(lm_B_main[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

# Calculate one sided tests 
#OneSided_pvals <- lapply(lm_B, oneSidedTest, negative, twosided)
OneSided_pvals <- list()
for (i in 1:length(dep_vars_names)) {
  OneSided_pvals[[i]] <- oneSidedTest(lm_B_main[[i]], negative[i], twosided[i])
  
}

tableB <- stargazer(lm_B_main,
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
                    add.lines = list(c("SM Individual = SM Group (p-value)", pvals$X1),
                                     c("SM Individual = TV (p-value)", pvals$X2), 
                                     c("SM Group= TV (p-value)", pvals$X3)),
                    title = title,
                    type = "latex")

# Panel C Table: 
pvals <- data.frame(matrix(NA, nrow = length(dep_vars_names), ncol = length(test) ))

for (i in 1:length(test)) {
  for (x in 1: length(dep_vars_names)) {
    paux <- round(linearHypothesis(lm_C_main[[x]], test[i], digits = 3)$`Pr(>F)`[2], digits = 4)
    pvals[x,i] <- paux
  }
}

# Calculate one sided tests 
# OneSided_pvals <- lapply(lm_C, oneSidedTest, negative, twosided)

OneSided_pvals <- list()
for (i in 1:length(dep_vars_names)) {
  OneSided_pvals[[i]] <- oneSidedTest(lm_C_main[[i]], negative[i], twosided[i])
  
}

tableC <- stargazer(lm_C_main,
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
                                     c("SM Individual = SM Group (p-value)", pvals$X1),
                                     c("SM Individual = TV (p-value)", pvals$X2), 
                                     c("SM Group= TV (p-value)", pvals$X3)),
                    title = title,
                    type = "latex")

note.latex <- paste0("\\multicolumn{", size, "}{l} {\\parbox[t]{", cm, "cm}{ \\textit{Notes:}
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. or all specifications, we implemented positive one-sided tests of
statistical significance as pre-specified. 
Regressions in Panel A use as controls the covariates selected by LASSO in which the treatment indicators,
lagged dependent variable, and fixed effects are forced into model and covariates are selected from the outcome family.
Regressions in Panel B include the dependent variable at baseline (if available) as a control. 
Regressions in Panel C do not include any variable as a control. 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
tableC[grepl("Note", tableC)] <- note.latex

cat(Panel_3_LASSO(tableA, tableB, tableC, "\\\\shortstack\\{Index", size, 2.5), file = 'Tables/Summary_Dups.tex')
