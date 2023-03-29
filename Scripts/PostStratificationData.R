# Post-stratification Analysis

# Facebook Ads dataset ---------------------------------------------------------
ads_data <- read.xlsx('Datasets/facebook_ad_data/All_Ads_Complete.xlsx', sheetIndex = 1) %>%
  dplyr::select(2,9:16,18) %>% dplyr::rename(notes = NA..1) 

# Convert to age groups to portions
ads_data[,4:9] <- ads_data[,4:9]/100

# Half proportions where men are registered
not_na <- !is.na(ads_data$notes)
ads_data[not_na,4:9] <- ads_data[not_na,4:9] / 2

# Multiple reach variable for each proportion
ads_data[, 4:9] <- ads_data[, 4:9]*ads_data$Reach

ads_data <- ads_data %>% dplyr::select(-c(1,2,10))

# Reshape dataset
ads_data <- melt(ads_data, id = c("Location")) 
ads_data$variable <- as.character(ads_data$variable)

# Recode Age stratums
ads_data$variable[which(ads_data$variable == "X18.24")] <- "18-24"
ads_data$variable[which(ads_data$variable == "X25.34")] <- "25-34"
ads_data$variable[which(ads_data$variable == "X35.44")] <- "35-44"
ads_data$variable[which(ads_data$variable == "X45.54")] <- "45-54"
ads_data$variable[which(ads_data$variable == "X55.64")] <- "55-64"
ads_data$variable[which(ads_data$variable == "X65.")] <- "65+"

ads_data <- ads_data %>% dplyr::rename(Age = variable)

# Combine intersected governorates, or subset governorates
ads_data$Location[which(ads_data$Location == "Qalyubia")] <- "Cairo, Giza, Qalyubia"
ads_data$Location[which(ads_data$Location == "Gharbia")] <- "Gharbia, kafr el-sheikh, dakahlia, beheira"
ads_data$Location[which(ads_data$Location == "Dakahlia")] <- "Gharbia, kafr el-sheikh, dakahlia, beheira"
ads_data$Location[which(ads_data$Location == "Beheira")] <- "Gharbia, kafr el-sheikh, dakahlia, beheira"
ads_data$Location[which(ads_data$Location == "Faiyum governorate")] <- "Faiyum"

ads_data$Location <- gsub(" governorate", "", ads_data$Location)

# Group by location and age group: frequencies within sample
ads_data <- ads_data %>% group_by(Age, Location) %>% summarise_all(sum) %>%
  dplyr::rename(reach = value) %>% mutate(freq1 = reach / sum(reach))

# Frequencies within location
postratification_ads <- ads_data %>% group_by(Location) %>% dplyr::mutate(freq2 = reach / sum(reach)) %>% ungroup()

# Survey sample ----------------------------------------------------------------
# Create analogous post-stratification table
gov <- read.csv('Datasets/Map/latlong_base_endrespondents_names.csv', fileEncoding = "latin1")
gov$governatore[which(gov$governatore == "0")] <- NA

# Read data set with governorates names
survey <- read.csv('Datasets/Finaldata and Zscores/Finaldata and Zscores.csv', fileEncoding = "latin1")

survey <- left_join(survey, gov[c("X", "governatore")], by = c("ResponseId" = "X")) %>%
  dplyr::rename(Location = governatore)

# 132 NA responses
sum(is.na(survey$Location))

# Stratify age
survey$Age <- cut(as.integer(survey$age), breaks = c(17, 24, 34, 44, 54, 64, 150),
                  labels = c("18-24","25-34","35-44","45-54","55-64","65+"),
                  ordered_result = TRUE)

# Stratify Location
unique(survey$Location)
unique(postratification_ads$Location)

survey <- survey %>% dplyr::mutate(Location = dplyr::recode(Location, 
                                                            Cairo = "Cairo, Giza, Qalyubia",     
                                                            Dakahlia = "Gharbia, kafr el-sheikh, dakahlia, beheira",
                                                            Alexandria = "Alexandria",
                                                            Suhag = "Sohag",       
                                                            Aswan = "Aswan",
                                                            Giza = "Cairo, Giza, Qalyubia",
                                                            Behera = "Gharbia, kafr el-sheikh, dakahlia, beheira",
                                                            Sharkia = "Al Sharqia",
                                                            Kalyoubia = "Cairo, Giza, Qalyubia",
                                                            Fayoum = "Faiyum",
                                                            Luxor = "Luxor",
                                                            Menoufia = "Monufia",
                                                            Assiut = "Asyut",
                                                            Ismailia = "Ismailia",  
                                                            Gharbia = "Gharbia, kafr el-sheikh, dakahlia, beheira",
                                                            Qena = "Qena",
                                                            `Red Sea` = "Red sea",
                                                            Menia = "Minya",
                                                            Suez = "Suez",
                                                            `Kafr El-Shikh` = "Gharbia, kafr el-sheikh, dakahlia, beheira",
                                                            `Port Said` = "Port Said",
                                                            `Beni Suef` = "Beni Suef",
                                                            `New Valley` = "New Valley",
                                                            Matrouh = "Matrouh",
                                                            .default = NA_character_))


# Create Post-stratification table
poststrat_df_survey <- survey  %>% group_by(Age, Location, .drop = FALSE) %>% dplyr:: summarise(reach_s = n())

poststrat_df_survey$freq1_s <- poststrat_df_survey$reach_s / sum(poststrat_df_survey$reach_s)

poststrat_df_survey <- poststrat_df_survey %>% group_by(Location) %>% dplyr::mutate(freq2_s = reach_s / sum(reach_s)) %>% ungroup()

# Merge both post-stratification datasets
unique(poststrat_df_survey$Location) %in% unique(postratification_ads$Location) # only NA governatore wo match

postrat_merged <- left_join(postratification_ads, poststrat_df_survey, by = c("Location", "Age"))


postrat_merged$weight1 <- postrat_merged$freq1 / postrat_merged$freq1_s
postrat_merged$weight2 <- postrat_merged$freq2 / postrat_merged$freq2_s

# Repalce Inf, NA, NaN with ceros
postrat_merged$weight1[which(!is.finite(postrat_merged$weight1))] <- 0
postrat_merged$weight2[which(!is.finite(postrat_merged$weight2))] <- 0

# Read postratification and survey datasets
postrat_merged <- read.csv('Datasets/Poststratification/Poststratification.csv', fileEncoding = "latin1")
merged2 <- read.csv('Datasets/Finaldata and Zscores/Finaldata and Zscores.csv', fileEncoding = "latin1")

# Append weights to survey dataset
aux <- c("Location", "Age", "weight1", "weight2")

merged <- left_join(survey, postrat_merged[aux], by = c("Location", "Age"))

# Construct weights as IPW * QuotientOfFrequencies
merged$weight1 <- merged$weight*merged$weight1
merged$weight2 <- merged$weight*merged$weight2


# Save Post-stratification tale and survey with weights
write.csv(postrat_merged, 'Datasets/Poststratification/Poststratification.csv') # post-stratification table
write.csv(merged, 'Datasets/Poststratification/Endline_weights.csv') # Endline survey with new weights

# ggplot setup -----------------------------------------------------------------
colors <- c("#0072B2","#D55E00")
# We define ggplot setup
legend.text.18 <- element_text(color = "black", size = 18, family="Arial")
final.text.14 <- element_text(color = "black", size = 14, hjust = 0.5, family="Arial")
final.text.12 <- element_text(color = "black", size = 12, hjust = 0.5, family="Arial")


th_egypt <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.title.x=element_blank(), 
                  axis.title.y=element_blank(),
                  panel.border = element_blank())

# set the background color for all graphs
fill_all <- c("white")
folder <- c('Figures')

# ------------------------------------------------------------------------------

# Comparison of Facebook Ads sample with surveys sample
postratification_ads1 <- postratification_ads %>% dplyr::select(Age, reach) %>% group_by(Age) %>% summarise_all(sum)
postratification_ads1$freq_Age <- (postratification_ads1$reach / sum(postratification_ads1$reach))*100
poststrat_df_survey1 <- poststrat_df_survey %>% dplyr::select(Age, reach_s) %>% group_by(Age) %>% summarise_all(sum)
poststrat_df_survey1$freq_Age <- (poststrat_df_survey1$reach_s / sum(poststrat_df_survey1$reach_s))*100

data_aux <- as.data.frame(cbind(postratification_ads1$freq_Age, poststrat_df_survey1$freq_Age))

colnames(data_aux) <- c("Ads", "OurData")
rownames(data_aux) <- c("18-24","25-34","35-44","45-54","55-64","65+")

positions <- c("18-24","25-34","35-44","45-54","55-64","65+")

FA1 <- ggplot(data_aux, aes(rownames(data_aux), Ads)) +
  geom_col(aes(color = "Ads"),  size = 1, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = OurData, color = "OurData"),  size = 1, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  scale_x_discrete(limits = positions) + 
  th_egypt +
  scale_color_manual(name = "Dataset",  labels = c("Facebook Ads", "Experimental"),
                     values= colors) +
  theme(legend.position = c(0.8,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14)  +
  ylab("Percentage of sample") +
  guides(colour=guide_legend(title.position="top", 
                             title.hjust =0.5))

ggsave(FA1, path = folder, filename = "FA1.pdf", device = cairo_pdf,
       width=7, height=5,dpi=300)

# Location ---------------------------------------------------------------------
postratification_ads2 <- postratification_ads %>% dplyr::select(Location, reach) %>% group_by(Location) %>% summarise_all(sum)
postratification_ads2$freq_location <- (postratification_ads2$reach / sum(postratification_ads2$reach))*100
poststrat_df_survey2 <- poststrat_df_survey %>% dplyr::select(Location, reach_s) %>% group_by(Location) %>% summarise_all(sum)
poststrat_df_survey2$freq_location <- (poststrat_df_survey2$reach_s / sum(poststrat_df_survey2$reach_s))*100

# Fill missing governorates in experimental sample 
poststrat_df_survey2 <- poststrat_df_survey2 %>% dplyr::add_row(
  Location = "Damietta",
  reach_s  = NA,
  freq_location = NA,
  .before = 7)

poststrat_df_survey2 <- poststrat_df_survey2 %>% dplyr::add_row(
  Location = "North Sinai",
  reach_s  = NA,
  freq_location = NA,
  .before = 16)

poststrat_df_survey2 <- poststrat_df_survey2 %>% dplyr::add_row(
  Location = "South sinai",
  reach_s  = NA,
  freq_location = NA,
  .before = 21)

# Remove NA column
poststrat_df_survey2 <- poststrat_df_survey2[-dim(poststrat_df_survey2)[1],]

locations <- c("Al Sharqia",
               "Alexandria",                               
               "Aswan", 
               "Asyut",                                     
               "Beni Suef",
               "Cairo, Giza, Qalyubia",                     
               "Damietta","Faiyum",                                    
               "Gharbia, Kafr El-sheikh, Dakahlia, Beheira", 
               "Ismailia",                      
               "Luxor",                        
               "Matrouh",                                   
               "Minya",                                      
               "Monufia",                                  
               "New Valley",                     
               "North Sinai",                   
               "Port Said",                                  
               "Qena",                                     
               "Red sea",                        
               "Sohag",                                    
               "South sinai",                    
               "Suez")

# Construct auxiliary dataframe
data_aux <- as.data.frame(cbind(postratification_ads2$freq_location, poststrat_df_survey2$freq_location))

colnames(data_aux) <- c("Ads", "OurData")
rownames(data_aux) <- locations

positions <- locations

FA2 <- ggplot(data_aux, aes(rownames(data_aux), Ads)) +
  geom_col(aes(color = "Ads"),  size = 1, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = OurData, color = "OurData"),  size = 1, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  scale_x_discrete(limits = positions, scale_x_discrete(position = "top") ) + 
  th_egypt +
  scale_color_manual(name = "Dataset",  labels = c("Facebook Ads", "Experimental"),
                     values= colors) +
  theme(legend.position = "none", legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.12, axis.text.y = final.text.14)  +
  ylab("Percentage of sample") +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(label = locations), position = position_dodge(width = 0.9), vjust = -0.5, 
            hjust = -0.1, angle = 90, fontface = "bold", size = 5)

ggsave(FA2, path = folder, filename = "FA2.pdf", device = cairo_pdf,
       width=10, height=7,dpi=300)

