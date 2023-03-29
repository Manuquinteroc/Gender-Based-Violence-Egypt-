# Auxiliary function for tables in balance and treatment effects

# Auxiliary function to align tables
alignTableV <- function(aligned, align = NULL) {
  
  aligned <- paste(aligned, collapse = "")
  
  aligned <- gsub("\\caption", "\\vspace*{-%cm} \\\\caption", aligned)
  aligned <- gsub("%", align, aligned)
  
  return(aligned)
}


alignTable <- function(aligned, align = NULL) {
  
  aligned <- paste(aligned, collapse = "")
  
  aligned <- gsub("begin\\{tabular", "hspace*{-%cm} \\\\begin\\{tabular", aligned)
  aligned <- gsub("%", align, aligned)
  
  return(aligned)
}

# Axuiliary function to create panel for Table5S
Panel_Balance <- function(panelA, panelB, x, align = NULL) {
  
  TableA <- paste(panelA, collapse = "")
  TableB <- paste(panelB, collapse = "")
  
  aux1 <- sub("\\\\\\[-1.8ex].*", "\\hline \\\\\\\\[-1.8ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel A: Respondent’s outcomes}} \\\\\\\\ \\\\hline \\\\\\\\[-1ex] ", TableA)
  
  # auxiliar text that uses the value of x
  text <- c(".*\\\\\\cline\\{2-%\\}")
  text <- gsub("%", x, text)
  
  aux2 <- sub(text, '', TableA)
  
  modified_tableA <- paste(aux1, aux2, collapse = "")
  
  modified_tableA <- sub("\\\\hline \\\\hline.*", " \\\\hline \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B: Whether married and husband’ outcomes}} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", modified_tableA)
  modified_tableA <- gsub("%", x, modified_tableA)
  
  modified_tableB <- sub(".*Married", " & Married", TableB)
  
  merged_table <- paste(modified_tableA, modified_tableB, collapse = "")
  
  if (!is.null(align)) {
    
    merged_table <- gsub("begin\\{tabular", "hspace*{-%cm} \\\\begin\\{tabular", merged_table)
    merged_table <- gsub("%", align, merged_table)
  }
  
  
  return(merged_table)
}


# Panel function for FS and RF
Panel_3 <- function(panelA, panelB, panelC, keyword, x, align = NULL, vs = NULL) {
  
  TableA <- paste(panelA, collapse = "")
  TableB <- paste(panelB, collapse = "")
  TableC <- paste(panelC, collapse = "")
  
  aux1 <- sub("\\\\\\[-1.8ex].*", "\\hline \\\\\\\\[-1.8ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel A: Controlling by all baseline covariates in the outcome family and demographics}} \\\\\\\\ \\\\hline \\\\\\\\[-1ex] ", TableA)
  
  aux2 <- sub(paste0(".*\\]  & ",  keyword), paste0("& ", keyword), TableA) 
  
  aux2 <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\textit.*", "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B: Controlling by the dependent variable at baseline (if available) }} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", aux2) 
  
  modified_tableA <- paste(aux1, aux2, collapse = "")

  # data for panel B 
  aux1_B <- sub(".*SM Individual &", "SM Individual &", TableB) 
  
  # Merge A and B
  merged_AB <- paste(modified_tableA, aux1_B, collapse = "")
  
  aux2_B <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\textit.*", "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel C: No covariates }} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", merged_AB) 
  
  # data for panel C 
  aux1_C <- sub(".*SM Individual &", "SM Individual &", TableC) 
  
  # Merge A, B, and C
  merged_ABC <- paste(aux2_B, aux1_C, collapse = "")
  merged_ABC <- gsub("%", x, merged_ABC)

  # Panel C
  # fix vertical height 
  if (!is.null(vs)) {
    merged_ABC <- gsub("\\centering", "centering \\\\vspace*{-%cm}", merged_ABC) 
    merged_ABC <- gsub("%", vs, merged_ABC)
    
  }
  
  # Fix horizontal direction
  if (!is.null(align)) {
    merged_ABC <- gsub("begin\\{tabular", "hspace*{-%cm} \\\\begin\\{tabular", merged_ABC)
    merged_ABC <- gsub("%", align, merged_ABC)
  }
  
  return(merged_ABC)
}


# Panel function for FS and RF
Panel_3_LASSO <- function(panelA, panelB, panelC, keyword, x, align = NULL, vs = NULL) {
  
  TableA <- paste(panelA, collapse = "")
  TableB <- paste(panelB, collapse = "")
  TableC <- paste(panelC, collapse = "")
  
  aux1 <- sub("\\\\\\[-1.8ex].*", "\\hline \\\\\\\\[-1.8ex] \\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel A: Controlling by the lagged dependent variable and covariates selected by LASSO}} \\\\\\\\ \\\\hline \\\\\\\\[-1ex] ", TableA)
  
  aux2 <- sub(paste0(".*\\]  & ",  keyword), paste0("& ", keyword), TableA) 
  
  aux2 <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\textit.*", "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel B: Controlling by the dependent variable at baseline (if available) }} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", aux2) 
  
  modified_tableA <- paste(aux1, aux2, collapse = "")
  
  # data for panel B 
  aux1_B <- sub(".*SM Individual &", "SM Individual &", TableB) 
  
  # Merge A and B
  merged_AB <- paste(modified_tableA, aux1_B, collapse = "")
  
  aux2_B <- sub("\\\\hline \\\\\\\\\\[-1.8ex] \\\\textit.*", "\\\\\\\\[-0.5ex] \\\\multicolumn{%}{l}{\\\\textbf{Panel C: No covariates }} \\\\\\\\ \\\\hline \\\\\\\\[-1ex]", merged_AB) 
  
  # data for panel C 
  aux1_C <- sub(".*SM Individual &", "SM Individual &", TableC) 
  
  # Merge A, B, and C
  merged_ABC <- paste(aux2_B, aux1_C, collapse = "")
  merged_ABC <- gsub("%", x, merged_ABC)
  
  # Panel C
  # fix vertical height 
  if (!is.null(vs)) {
    merged_ABC <- gsub("\\centering", "centering \\\\vspace*{-%cm}", merged_ABC) 
    merged_ABC <- gsub("%", vs, merged_ABC)
    
  }
  
  # Fix horizontal direction
  if (!is.null(align)) {
    merged_ABC <- gsub("begin\\{tabular", "hspace*{-%cm} \\\\begin\\{tabular", merged_ABC)
    merged_ABC <- gsub("%", align, merged_ABC)
  }
  
  return(merged_ABC)
}
