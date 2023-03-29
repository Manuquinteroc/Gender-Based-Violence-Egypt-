# This script produces all Figures and Tables of the paper
# Version: 13/03/2022

# Set working directory
rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install require packages
source('Scripts/RequiredPackages.R')

# Load Panel Functions
source('Scripts/PanelFunction.R')

# Create Baseline data set 
source('Scripts/BaselineVariables.R')

# Create Endline data set
source('Scripts/EndlineVariables.R')

# Arab Barometer Comparison
source('Scripts/ArabBarometerComparison.R')

# Map: Survey responses by Egyptian Governatore
source('Scripts/Map.R')

# Balance Tables
source('Scripts/BalanceTables.R')
source('Scripts/BalanceSingleTable.R')

# Outdated script to create Z-scores
# source('Scripts/Outdated/Zscores')

# First Stage and Reduced Form analysis with LASSO
source('Scripts/TreatmentEffectsLASSO.R')

# First Stage and Reduced Form analysis without LASSO
# source('Scripts/TreatmentEffects.R') # These tables are not reported in the papers but are left for robustness check

# Clicky Analysis
source('Scripts/ClickyAnalysis.R')

# Mobility Analysis 
source('Scripts/Mobility.R')

# HEs
source('Scripts/HEs.R')

# Post-stratification Analysis
source('Scripts/PostStratificationData.R')
source('Scripts/PostStratificationAnalysis.R')

# Baseline Attrition Analysis
source('Scripts/Baseline Attrition.R')

# Bayes Factor
source('Scripts/BayesFactor.R')

# Power tests
source('Scripts/PowerTests.R')

# Main indexes with duplicated respondents
source('Scripts/Dups data handling.R')
source('Scripts/MainResults_wDups.R')
