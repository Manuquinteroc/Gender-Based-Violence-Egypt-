# Arab Barometer comparison analysis 

# Read the 2018 dataset
arab <- read.csv(file = 'Datasets/ArabBarometer/ABV_Release_Data2018.csv', fileEncoding = "latin1")

# Read the 2016 dataset
arab2 <- read.csv(file = 'Datasets/ArabBarometer/ABIV_English2016.csv', fileEncoding = "latin1")

# Read Experimental dataset (ours)
merged <- read.csv(file = 'Datasets/Finaldata/Finaldata.csv', fileEncoding = "latin1")

# Filter by country: Egypt, N = 2400
egypt <- dplyr::filter(arab, country == 5)
egypt_2016 <- dplyr::filter(arab2, country == "Egypt")

# Filter by gender: Female, N = 1200
egypt <- dplyr::filter(egypt, Q1002 == 2)
egypt_2016 <- dplyr::filter(egypt_2016, q2008 == "Female") 

# Create a SM for those that use the internet
egypt_sm18 <- dplyr::filter(egypt, Q409 != 6, Q409 != 98) 

# We need to code the internet variable for 2016
map_internet <- c("Daily" = 2, "I do not use the internet " = 6, "I am online almost all day" = 1, "Several times a week" = 3,
                  "Once a week" = 4,"Less than once a week" = 5)

egypt_2016$internet <- as.numeric(revalue(egypt_2016$q409, map_internet))
egypt_2016_sm <- filter(egypt_2016, internet != 6)

folder <- c('Figures')

# We define ggplot setup
legend.text.18 <- element_text(color = "black", size = 7, family = "Arial")
final.text.14 <- element_text(color = "black", size = 7, hjust = 0.5, family = "Arial")
legend.text.5 <- element_text(color = "black", size = 5, family = "Arial")

# Define colors for all graphs
colors <- c("#000000", "#0072B2","#D55E00")
th_egypt <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.title.x = final.text.14,
                  axis.title.y = final.text.14,
                  axis.text.x = legend.text.5, 
                  axis.text.y = legend.text.5,
                  panel.border = element_blank())

# set the background color for all graphs
fill_all <- c("white")
global_size <- 0.5

# Figures ----------------------------------------------------------------------
# Age  
egypt_age <- as.data.frame(cbind(c(egypt$Q1001, as.numeric(egypt_2016$q1001)), c(egypt$wt, egypt_2016$wt)))
colnames(egypt_age) <- c("Age", "wt")

egypt_sm_age <- as.data.frame(cbind(c(egypt_sm18$Q1001, as.numeric(egypt_2016_sm$q1001)), c(egypt_sm18$wt, egypt_2016_sm$wt)))
colnames(egypt_sm_age) <- c("Age", "wt")

g1A <- ggplot() +
  geom_density(aes(Age, color = "AB"), size = global_size, fill = fill_all, alpha = .4, data = egypt_age) +
  geom_density(aes(Age, color = "ABSM"),  size = global_size, fill = fill_all,  alpha = .4, data = egypt_sm_age) +
  geom_density(aes(age, color = "OurData"), size = global_size,   fill = fill_all,  alpha = .4, data = merged) +
  th_egypt +
  ylab("Density") + 
  xlab(expression(paste(bold("(A)"), " Age"))) + 
  scale_x_continuous(limits = c(18, 80)) +
  theme(legend.position = c(0.8,0.8), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14) + 
  scale_color_manual(name = "Dataset",  labels = c("Arab Barometer","Arab Barometer \ninternet users","Experimental"),
                     values= colors)  + 
  theme(legend.key.height = unit(0.5, "cm")) +
  geom_line() +
  guides(colour = guide_legend(override.aes = list(size = 0.3, linetype = 1)))
  

ggsave(g1A, path = folder, filename = "1A.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)

l1 <- sum(!is.na(egypt_age$Age))
l2 <- sum(!is.na(egypt_sm_age$Age))
l3 <- sum(!is.na(merged$age))
m1 <- weighted_mean(egypt_age$Age, w = egypt_age$wt)
m2 <- weighted_mean(egypt_sm_age$Age, w = egypt_sm_age$wt)
m3 <- weighted_mean(merged$age, w = merged$weight)
sd1 <- weighted_sd(egypt_age$Age, w = egypt_age$wt)
sd2 <- weighted_sd(egypt_sm_age$Age, w = egypt_sm_age$wt)
sd3 <- weighted_sd(merged$age, w = merged$weight)

summary_age <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_age) <- c("AB", "Restricted AB", "OurData")
rownames(summary_age) <- c( "Mean", "Standard Deviation", "Observations")

# Education 
# Map for merged
map <- c("No formal education" = 1, "Elementary" = 2, "Preparatory/basic" = 3,
         "General secondary" = 4, "Vocational secondary" = 4, "Higher technical institutes" = 4, "BA" = 6,
         "MA and above" = 7)

educ <- as.numeric(revalue(merged$education, map))


# Map for 2016
map_educ <- c("Illiterate/No formal education" = 1, "Elementary" = 2, 
              "Preparatory/Basic" = 3, "Mid-level diploma / professional or technical"= 4, 
              "Secondary" = 4, "BA" = 6, "MA and above" = 7)

egypt_2016$q1003_num <- as.numeric(revalue(egypt_2016$q1003, map_educ))
egypt_2016_sm$q1003_num <- as.numeric(revalue(egypt_2016_sm$q1003, map_educ))

educ_ab <- as.data.frame(cbind(c(egypt$Q1003, egypt_2016$q1003_num), c(egypt$wt, egypt_2016$wt)))
colnames(educ_ab) <- c("Education","wt")

educ_ab_sm <- as.data.frame(cbind(c(egypt_sm18$Q1003, egypt_2016_sm$q1003_num), c(egypt_sm18$wt, egypt_2016_sm$wt)))
colnames(educ_ab_sm) <- c("Education","wt")


l1 <- length(educ_ab$Education)
l2 <- length(educ_ab_sm$Education)
l3 <- length(educ)
m1 <- weighted_mean(educ_ab$Education, w = educ_ab$wt)
m2 <- weighted_mean(educ_ab_sm$Education, w = educ_ab_sm$wt)
m3 <- weighted_mean(educ, w = merged$weight)
sd1 <- weighted_sd(educ_ab$Education, w = educ_ab$wt)
sd2 <- weighted_sd(educ_ab_sm$Education, w = educ_ab_sm$wt)
sd3 <- weighted_sd(educ, w = merged$weight)


summary_educ <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_educ) <- c("AB", "Restricted AB", "OurData")
rownames(summary_educ) <- c( "Mean", "Standard Deviation", "Observations")


x1 <- fre(educ_ab$Education,  weight = educ_ab$wt)[,3][1:6]
y1 <- fre(educ_ab_sm$Education, weight = educ_ab_sm$wt)[,3][1:6]
z1 <- fre(educ, weight = merged$weight)[,3][1:6]
x1 <- round(as.data.frame(x1[[1]]), digits = 2)
y1 <- round(as.data.frame(y1[[1]]), digits = 2)
z1 <- round(as.data.frame(z1[[1]]), digits = 2)


data4 <- as.data.frame(cbind(x1,y1,z1))
colnames(data4) <- c("AB", "ABSM", "OurData")
rownames(data4) <- c("None", "Elem.", "Prep.",
                     "SS", "BA",
                     "MA and above")

positions <- c("None", "Elem.", "Prep.",
               "SS", "BA",
               "MA and above")

g1B <- ggplot(data4, aes(rownames(data4), AB)) +
  geom_col(aes(color = "AB"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  scale_x_discrete(limits = positions) + 
  geom_col(aes(y = OurData, color = "OurData"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.375)) +
  scale_x_discrete(limits = positions) + 
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(B)"), " Education level"))) + 
  ylim(0, 65) +
  scale_color_manual(name = "Dataset",  labels = c("AB","Restricted AB","OurData"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1B, path = folder, filename = "1B.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)


# Social Status 
map <- c("Married" = 4, "Separated" = 5.5, "Widowed" = 7, "Engaged"= 3, "Divorced" = 5.5, 
         "Single - living independently of family" = 1, 
         "Single - living with family" = 1)

socialstat <- as.numeric(revalue(merged$social_status, map))
socialstat_aux <- as.data.frame(socialstat)

ind1 <- which(egypt$Q1010 == 5)
ind2 <- which(egypt$Q1010 == 6)
egypt$Q1010[c(ind1,ind2)] <- 5.5

ind1 <- which(egypt_sm18$Q1010 == 5)
ind2 <- which(egypt_sm18$Q1010 == 6)
egypt_sm18$Q1010[c(ind1,ind2)] <- 5.5

map_marr <- c("Unmarried or Bachelor" = 1, "Engaged" = 3, "Married" = 4, 
              "Divorced or separated" = 5.5, "Widowed" = 7)

egypt_2016$q1010_num <- as.numeric(revalue(egypt_2016$q1010, map_marr))
egypt_2016_sm$q1010_num <- as.numeric(revalue(egypt_2016_sm$q1010, map_marr))

egypt_marr <- as.data.frame(cbind(c(egypt$Q1010, as.numeric(egypt_2016$q1010_num)), c(egypt$wt, egypt_2016$wt)))
colnames(egypt_marr) <- c("SocialStatus", "wt")

egypt_sm_marr <- as.data.frame(cbind(c(egypt_sm18$Q1010, as.numeric(egypt_2016_sm$q1010_num)), c(egypt_sm18$wt, egypt_2016_sm$wt)))
colnames(egypt_sm_marr) <- c("SocialStatus", "wt")

# Create dummies for each social status
egypt_marr <- cbind.data.frame(egypt_marr, dummy_cols(egypt_marr$SocialStatus, remove_selected_columns = TRUE)) 
egypt_sm_marr <- cbind.data.frame(egypt_sm_marr, dummy_cols(egypt_sm_marr$SocialStatus, remove_selected_columns = TRUE)) 
socialstat_aux <- cbind.data.frame(socialstat_aux, dummy_cols(socialstat_aux$socialstat, remove_selected_columns = TRUE)) 

# number of observations
l1 <- length(egypt_marr$SocialStatus)
l2 <- length(egypt_sm_marr$SocialStatus)
l3 <- length(socialstat_aux$socialstat)

# Single summary stats
w1 <- weighted_mean(egypt_marr$.data_1, w = egypt_marr$wt)
w2 <- weighted_mean(egypt_sm_marr$.data_1, w = egypt_sm_marr$wt)
w3 <- weighted_mean(socialstat_aux$.data_1, w = merged$weight)

s1 <- weighted_sd(egypt_marr$.data_1, w = egypt_marr$wt)
s2 <- weighted_sd(egypt_sm_marr$.data_1, w = egypt_sm_marr$wt)
s3 <- weighted_sd(socialstat_aux$.data_1, w = merged$weight)

summary_single <- round(rbind.data.frame(c(w1,w2,w3), c(s1,s2,s3), c(l1,l2,l3)), digits = 3)
colnames(summary_single) <- c("AB", "Restricted AB", "OurData")
rownames(summary_single) <- c( "Mean", "Standard Deviation", "Observations")

# Engaged summary stats
w1 <- weighted_mean(egypt_marr$.data_3, w = egypt_marr$wt)
w2 <- weighted_mean(egypt_sm_marr$.data_3, w = egypt_sm_marr$wt)
w3 <- weighted_mean(socialstat_aux$.data_3, w = merged$weight)

s1 <- weighted_sd(egypt_marr$.data_3, w = egypt_marr$wt)
s2 <- weighted_sd(egypt_sm_marr$.data_3, w = egypt_sm_marr$wt)
s3 <- weighted_sd(socialstat_aux$.data_3, w = merged$weight)

summary_engaged <- round(rbind.data.frame(c(w1,w2,w3), c(s1,s2,s3), c(l1,l2,l3)), digits = 3)
colnames(summary_engaged) <- c("AB", "Restricted AB", "OurData")
rownames(summary_engaged) <- c( "Mean", "Standard Deviation", "Observations")

# Married summary stats
w1 <- weighted_mean(egypt_marr$.data_4, w = egypt_marr$wt)
w2 <- weighted_mean(egypt_sm_marr$.data_4, w = egypt_sm_marr$wt)
w3 <- weighted_mean(socialstat_aux$.data_4, w = merged$weight)

s1 <- weighted_sd(egypt_marr$.data_4, w = egypt_marr$wt)
s2 <- weighted_sd(egypt_sm_marr$.data_4, w = egypt_sm_marr$wt)
s3 <- weighted_sd(socialstat_aux$.data_4, w = merged$weight)

summary_married <- round(rbind.data.frame(c(w1,w2,w3), c(s1,s2,s3), c(l1,l2,l3)), digits = 3)
colnames(summary_married) <- c("AB", "Restricted AB", "OurData")
rownames(summary_married) <- c( "Mean", "Standard Deviation", "Observations")

# Separated summary stats
w1 <- weighted_mean(egypt_marr$.data_5.5, w = egypt_marr$wt)
w2 <- weighted_mean(egypt_sm_marr$.data_5.5, w = egypt_sm_marr$wt)
w3 <- weighted_mean(socialstat_aux$.data_5.5, w = merged$weight)

s1 <- weighted_sd(egypt_marr$.data_5.5, w = egypt_marr$wt)
s2 <- weighted_sd(egypt_sm_marr$.data_5.5, w = egypt_sm_marr$wt)
s3 <- weighted_sd(socialstat_aux$.data_5.5, w = merged$weight)

summary_separated <- round(rbind.data.frame(c(w1,w2,w3), c(s1,s2,s3), c(l1,l2,l3)), digits = 3)
colnames(summary_separated) <- c("AB", "Restricted AB", "OurData")
rownames(summary_separated) <- c( "Mean", "Standard Deviation", "Observations")

# Widowed summary stats
w1 <- weighted_mean(egypt_marr$.data_7, w = egypt_marr$wt)
w2 <- weighted_mean(egypt_sm_marr$.data_7, w = egypt_sm_marr$wt)
w3 <- weighted_mean(socialstat_aux$.data_7, w = merged$weight)

s1 <- weighted_sd(egypt_marr$.data_7, w = egypt_marr$wt)
s2 <- weighted_sd(egypt_sm_marr$.data_7, w = egypt_sm_marr$wt)
s3 <- weighted_sd(socialstat_aux$.data_7, w = merged$weight)

summary_widowed <- round(rbind.data.frame(c(w1,w2,w3), c(s1,s2,s3), c(l1,l2,l3)), digits = 3)
colnames(summary_widowed) <- c("AB", "Restricted AB", "OurData")
rownames(summary_widowed) <- c( "Mean", "Standard Deviation", "Observations")

# Continuous variable summary stats
l1 <- length(egypt_marr$SocialStatus)
l2 <- length(egypt_sm_marr$SocialStatus)
l3 <- length(socialstat)
m1 <- weighted_mean(egypt_marr$SocialStatus, w = egypt_marr$wt)
m2 <- weighted_mean(egypt_sm_marr$SocialStatus, w = egypt_sm_marr$wt)
m3 <- weighted_mean(socialstat, w = merged$weight)
sd1 <- weighted_sd(egypt_marr$SocialStatus, w = egypt_marr$wt)
sd2 <- weighted_sd(egypt_sm_marr$SocialStatus, w = egypt_sm_marr$wt)
sd3 <- weighted_sd(socialstat,  w = merged$weight)

summary_ss <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_ss) <- c("AB", "Restricted AB", "OurData")
rownames(summary_ss) <- c( "Mean", "Standard Deviation", "Observations")

x1 <- fre(egypt_marr$SocialStatus, weight = egypt_marr$wt)[,3][1:5]
y1 <- fre(egypt_sm_marr$SocialStatus, weight = egypt_sm_marr$wt)[,3][1:5]
z1 <- fre(socialstat, weight = merged$weight)[,3][1:5]

x1 <- round(as.data.frame(x1[[1]]), digits = 2)
y1 <- round(as.data.frame(y1[[1]]), digits = 2)
z1 <- round(as.data.frame(z1[[1]]), digits = 2)

data5 <- as.data.frame(cbind(x1,y1,z1))

colnames(data5) <- c("AB", "ABSM", "OurData")
rownames(data5) <- c("Single", "Engaged", "Married", "Separated", "Widowed")

positions <- c("Single", "Engaged", "Married", "Separated", "Widowed")

g1C <- ggplot(data5, aes(rownames(data5), AB)) +
  geom_col(aes(color = "AB"),  size = global_size, fill = fill_all,
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  scale_x_discrete(limits = positions) + 
  geom_col(aes(y = OurData, color = "OurData"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.375)) +
  scale_x_discrete(limits = positions) + 
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(C)"), " Relationship status"))) + 
  ylim(0, 65) +
  scale_color_manual(name = "Dataset",  labels = c("AB","Restricted AB","OurData"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1C, path = folder, filename = "1C.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)


# Number of Children 
child <- merged$male_children + merged$female_children

egypt$Q1010B2 <- replace_na(egypt$Q1010B2, 0)
egypt_sm18$Q1010B2 <- replace_na(egypt_sm18$Q1010B2, 0)

egypt_2016$q1010b <- as.numeric(egypt_2016$q1010b) 
egypt_2016$q1010b <- replace_na(egypt_2016$q1010b, 0)

egypt_2016_sm$q1010b <- as.numeric(egypt_2016_sm$q1010b) 
egypt_2016_sm$q1010b <- replace_na(egypt_2016_sm$q1010b, 0)

child_ab <- as.data.frame(cbind(c(egypt$Q1010B2, as.numeric(egypt_2016$q1010b)), c(egypt$wt, egypt_2016$wt)))
colnames(child_ab) <- c("Children", "wt")

child_absm <-as.data.frame(cbind(c(egypt_sm18$Q1010B2, as.numeric(egypt_2016_sm$q1010b)), c(egypt_sm18$wt, egypt_2016_sm$wt)))
colnames(child_absm) <- c("Children", "wt")


l1 <- length(child_ab$Children)
l2 <- length(child_absm$Children)
l3 <- length(child)
m1 <- weighted_mean(child_ab$Children, w = child_ab$wt)
m2 <- weighted_mean(child_absm$Children, w = child_absm$wt)
m3 <- weighted_mean(child, w = merged$weight)
sd1 <- weighted_sd(child_ab$Children, w = child_ab$wt)
sd2 <- weighted_sd(child_absm$Children, w = child_absm$wt)
sd3 <- weighted_sd(child, w = merged$weight)


summary_child <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_child) <- c("AB", "Restricted AB", "OurData")
rownames(summary_child) <- c( "Mean", "Standard Deviation", "Observations")


x1 <- fre(child_ab$Children, weight = child_ab$wt)[,3][1:8]
y1 <- fre(child_absm$Children, weight = child_absm$wt)[,3][1:7]
z1 <- fre(child, weight = merged$weight)[,3][1:9]


x1 <- round(as.data.frame(x1[[1]]), digits = 2)
y1 <- round(as.data.frame(y1[[1]]), digits = 2)
z1 <- round(as.data.frame(z1[[1]]), digits = 2)
x1 <- x1 %>% add_row(`x1[[1]]` = 0, .before = 8)
y1 <- rbind(y1,0,0)


data6 <- as.data.frame(cbind(x1,y1,z1))

colnames(data6) <- c("AB", "ABSM", "OurData")
rownames(data6) <- c(0:8)

positions <- c(0:8)


g1D <- ggplot(data6, aes(rownames(data6), AB)) +
  geom_col(aes(color = "AB"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  geom_col(aes(y = OurData, color = "OurData"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.375)) + 
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(D)"), " Number of children"))) + 
  ylim(0, 65) +
  scale_color_manual(name = "Dataset",  labels = c("AB","Restricted AB","OurData"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1D,path = folder, filename = "1D.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)


# Which social media comparison 
egypt_2016$q4113_num <- ifelse(egypt_2016$q4113 == "Yes", 1, 0)
egypt_2016$q4114_num <- ifelse(egypt_2016$q4114 == "Yes", 1, 0)
egypt_2016$q4116_num <- ifelse(egypt_2016$q4116 == "Yes", 1, 0)

fbe <- as.data.frame(cbind(c(egypt$Q412A3, as.numeric(egypt_2016$q4113_num)),
                           c(egypt$Q412A4, as.numeric(egypt_2016$q4114_num)),
                           c(egypt$Q412A6, as.numeric(egypt_2016$q4116_num)),
                           c(egypt$wt, egypt_2016$wt)))

colnames(fbe) <- c("Facebook", "Twitter", "Instagram",  "wt")

fbe$Facebook <- replace_na(fbe$Facebook, 0)
fbe$Twitter <- replace_na(fbe$Twitter, 0)
fbe$Instagram <- replace_na(fbe$Instagram, 0)

# ABSM
egypt_2016_sm$q4113_num <- ifelse(egypt_2016_sm$q4113 == "Yes", 1, 0)
egypt_2016_sm$q4114_num <- ifelse(egypt_2016_sm$q4114 == "Yes", 1, 0)
egypt_2016_sm$q4116_num <- ifelse(egypt_2016_sm$q4116 == "Yes", 1, 0)

fbe_sm <- as.data.frame(cbind(c(egypt_sm18$Q412A3, as.numeric(egypt_2016_sm$q4113_num)), 
                              c(egypt_sm18$Q412A4, as.numeric(egypt_2016_sm$q4114_num)),
                              c(egypt_sm18$Q412A6, as.numeric(egypt_2016_sm$q4116_num)),
                              c(egypt_sm18$wt, egypt_2016_sm$wt)))

colnames(fbe_sm) <- c("Facebook", "Twitter", "Instagram",  "wt")

# FB summary Stats
l1 <- length(fbe$Facebook)
l2 <- length(fbe_sm$Facebook)
l3 <- length(merged$socmed_Facebook)
m1 <- weighted_mean(fbe$Facebook, w = fbe$wt)
m2 <- weighted_mean(fbe_sm$Facebook, w = fbe_sm$wt)
m3 <- weighted_mean(merged$socmed_Facebook, w = merged$weight)
sd1 <- weighted_sd(fbe$Facebook, w = fbe$wt)
sd2 <- weighted_sd(fbe_sm$Facebook, w = fbe_sm$wt)
sd3 <- weighted_sd(merged$socmed_Facebook, w = merged$weight)

summary_fb <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_fb) <- c("AB", "Restricted AB", "OurData")
rownames(summary_fb) <- c( "Mean", "Standard Deviation", "Observations")

# WhatsApp Summary Stats (only 2018)
egypt$Q412A8 <- replace_na(egypt$Q412A8, 0)

l1 <- length(egypt$Q412A8)
l2 <- length(egypt_sm18$Q412A8)
l3 <- length(merged$socmed_whatsapp)
m1 <- weighted_mean(egypt$Q412A8, w = egypt$wt)
m2 <- weighted_mean(egypt_sm18$Q412A8, w = egypt_sm18$wt)
m3 <- weighted_mean(merged$socmed_whatsapp, w = merged$weight)
sd1 <- weighted_sd(egypt$Q412A8, w = egypt$wt)
sd2 <- weighted_sd(egypt_sm18$Q412A8, w = egypt_sm18$wt)
sd3 <- weighted_sd(merged$socmed_whatsapp, w = merged$weight)

summary_wa <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_wa) <- c("AB", "Restricted AB", "OurData")
rownames(summary_wa) <- c( "Mean", "Standard Deviation", "Observations")

# Youtube Summary Stats (only 2018)
egypt$Q412A7 <- replace_na(egypt$Q412A7, 0)

l1 <- length(egypt$Q412A7)
l2 <- length(egypt_sm18$Q412A7)
l3 <- length(merged$socmed_YouTube)
m1 <- weighted_mean(egypt$Q412A7, w = egypt$wt)
m2 <- weighted_mean(egypt_sm18$Q412A7, w = egypt_sm18$wt)
m3 <- weighted_mean(merged$socmed_YouTube, w = merged$weight)
sd1 <- weighted_sd(egypt$Q412A7, w = egypt$wt)
sd2 <- weighted_sd(egypt_sm18$Q412A7, w = egypt_sm18$wt)
sd3 <- weighted_sd(merged$socmed_YouTube, w = merged$weight)

summary_yt <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_yt) <- c("AB", "Restricted AB", "OurData")
rownames(summary_yt) <- c( "Mean", "Standard Deviation", "Observations")

# Instagram Summary Stats
l1 <- length(fbe$Instagram)
l2 <- length(fbe_sm$Instagram)
l3 <- length(merged$socmed_Instagram)
m1 <- weighted_mean(fbe$Instagram, w = fbe$wt)
m2 <- weighted_mean(fbe_sm$Instagram, w = fbe_sm$wt)
m3 <- weighted_mean(merged$socmed_Instagram, w = merged$weight)
sd1 <- weighted_sd(fbe$Instagram, w = fbe$wt)
sd2 <- weighted_sd(fbe_sm$Instagram, w = fbe_sm$wt)
sd3 <- weighted_sd(merged$socmed_Instagram, w = merged$weight)

summary_ins <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_ins) <- c("AB", "Restricted AB", "OurData")
rownames(summary_ins) <- c( "Mean", "Standard Deviation", "Observations")

# Twitter summary Stats
l1 <- length(fbe$Twitter)
l2 <- length(fbe_sm$Twitter)
l3 <- length(merged$socmed_Twitter)
m1 <- weighted_mean(fbe$Twitter, w = fbe$wt)
m2 <- weighted_mean(fbe_sm$Twitter, w = fbe_sm$wt)
m3 <- weighted_mean(merged$socmed_Twitter, w = merged$weight)
sd1 <- weighted_sd(fbe$Twitter, w = fbe$wt)
sd2 <- weighted_sd(fbe_sm$Twitter, w = fbe_sm$wt)
sd3 <- weighted_sd(merged$socmed_Twitter, w = merged$weight)

summary_tw <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_tw) <- c("AB", "Restricted AB", "OurData")
rownames(summary_tw) <- c( "Mean", "Standard Deviation", "Observations")

# Snapchat Summary Stats (only 2018)
egypt$Q412A10 <- replace_na(egypt$Q412A10, 0)

l1 <- length(egypt$Q412A10)
l2 <- length(egypt_sm18$Q412A10)
l3 <- length(merged$socmed_Snapchat)
m1 <- weighted_mean(egypt$Q412A10, w = egypt$wt)
m2 <- weighted_mean(egypt_sm18$Q412A10, w = egypt_sm18$wt)
m3 <- weighted_mean(merged$socmed_Snapchat, w = merged$weight)
sd1 <- weighted_sd(egypt$Q412A10, w = egypt$wt)
sd2 <- weighted_sd(egypt_sm18$Q412A10, w = egypt_sm18$wt)
sd3 <- weighted_sd(merged$socmed_Snapchat, w = merged$weight)

summary_sp <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_sp) <- c("AB", "Restricted AB", "OurData")
rownames(summary_sp) <- c( "Mean", "Standard Deviation", "Observations")

# fb, wa, youtube, ins, tw, sc
# AB
x1 <- plyr::count(fbe, "Facebook", wt_var = "wt")$freq[2]/sum(plyr::count(fbe, "Facebook", wt_var = "wt")$freq)*100
x2 <- plyr::count(egypt, "Q412A8", wt_var = "wt")$freq[2]/sum(plyr::count(egypt, "Q412A7", wt_var = "wt")$freq)*100
x3 <- plyr::count(egypt, "Q412A7", wt_var = "wt")$freq[2]/sum(plyr::count(egypt, "Q412A7", wt_var = "wt")$freq)*100
x4 <- plyr::count(fbe, "Instagram", wt_var = "wt")$freq[2]/sum(plyr::count(fbe, "Instagram", wt_var = "wt")$freq)*100
x5 <- plyr::count(fbe, "Twitter", wt_var = "wt")$freq[2]/sum(plyr::count(fbe, "Twitter", wt_var = "wt")$freq)*100
x6 <- plyr::count(egypt, "Q412A10", wt_var = "wt")$freq[2]/length(egypt$Q412A10)*100

# ABSM
y1 <- plyr::count(fbe_sm, "Facebook", wt_var = "wt")$freq[2]/sum(plyr::count(fbe_sm, "Facebook", wt_var = "wt")$freq)*100
y2 <- plyr::count(egypt_sm18, "Q412A8", wt_var = "wt")$freq[2]/sum(plyr::count(egypt_sm18, "Q412A8", wt_var = "wt")$freq)*100
y3 <- plyr::count(egypt_sm18, "Q412A7", wt_var = "wt")$freq[2]/sum(plyr::count(egypt_sm18, "Q412A7", wt_var = "wt")$freq)*100
y4 <- plyr::count(fbe_sm, "Instagram", wt_var = "wt")$freq[2]/sum(plyr::count(fbe_sm, "Instagram", wt_var = "wt")$freq)*100
y5 <- plyr::count(fbe_sm, "Twitter", wt_var = "wt")$freq[2]/sum(plyr::count(fbe_sm, "Twitter", wt_var = "wt")$freq)*100
y6 <- plyr::count(egypt_sm18, "Q412A10", wt_var = "wt")$freq[2]/sum(plyr::count(egypt_sm18, "Q412A10", wt_var = "wt")$freq)*100


# OurData
z1 <- fre(merged$socmed_Facebook, w = merged$weight)[,3][2]
z2 <- fre(merged$socmed_whatsapp, w = merged$weight)[,3][2]
z3 <- fre(merged$socmed_YouTube, w = merged$weight)[,3][2]
z4 <- fre(merged$socmed_Instagram, w = merged$weight)[,3][2]
z5 <- fre(merged$socmed_Twitter, w = merged$weight)[,3][2]
z6 <- fre(merged$socmed_Snapchat, w = merged$weight)[,3][2]

z1 <- round(as.data.frame(z1[[1]]), digits = 2)
z2 <- round(as.data.frame(z2[[1]]), digits = 2)
z3 <- round(as.data.frame(z3[[1]]), digits = 2)
z4 <- round(as.data.frame(z4[[1]]), digits = 2)
z5 <- round(as.data.frame(z5[[1]]), digits = 2)
z6 <- round(as.data.frame(z6[[1]]), digits = 2)

data3 <- as.data.frame(cbind(as.numeric(c(x1,x2,x3,x4,x5,x6)), 
                             as.numeric(c(y1,y2,y3,y4,y5,y6)), 
                             as.numeric(c(z1,z2,z3,z4,z5,z6))))
colnames(data3) <- c("AB", "ABSM", "OurData")
rownames(data3) <- c("Facebook", "WhatsApp",  "Youtube", "Instagram", "Twitter", "Snapchat")
data3 <- round(data3, digits = 2)

positions <- c("Facebook", "WhatsApp",  "Youtube", "Instagram", "Twitter", "Snapchat")

data3 <- round(data3, digits = 2)

positions <- c("Facebook", "WhatsApp",  "Youtube", "Instagram", "Twitter", "Snapchat")

g1E <- ggplot(data3, aes(rownames(data3), AB)) +
  geom_col(aes(color = "AB"),  size = global_size, fill = fill_all,  
           width = 0.3,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"),  size = global_size, fill = fill_all, 
           width = 0.3,
           position = position_nudge(x = 0.075)) +
  scale_x_discrete(limits = positions) + 
  geom_col(aes(y = OurData, color = "OurData"),  size = global_size, fill = fill_all,
           width = 0.3,
           position = position_nudge(x = 0.375)) +
  scale_x_discrete(limits = positions) + 
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(E)"), " Social media use"))) + 
  ylim(0, 90) +
  scale_color_manual(name = "Dataset",  labels = c("AB","Restricted AB","OurData"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1E, path = folder, filename = "1E.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)


# Hours spent on social media comparison
# AB
egypt$Q424 <- replace_na(egypt$Q424, 1) # NAs as 0 hours 
egypt$Q424[which(egypt$Q424 == 98)] <- 3 # Dont know responses to the middle value

# AB Social Media 
egypt_sm18$Q424[which(egypt_sm18$Q424 == 98)] <- 3 # Dont know responses to the middle value

# merged (run only once to make it comparable with AB)
if (0 %in% unique(merged$hours_soc_med_num)) {
  merged$hours_soc_med_num <- merged$hours_soc_med_num + 1
}


l1 <- length(egypt$Q424)
l2 <- length(egypt_sm18$Q424)
l3 <- length(merged$hours_soc_med_num)
m1 <- weighted_mean(egypt$Q424, w = egypt$wt)
m2 <- weighted_mean(egypt_sm18$Q424, w = egypt_sm18$wt)
m3 <- weighted_mean(merged$hours_soc_med_num, w = merged$weight)
sd1 <- weighted_sd(egypt$Q424, w = egypt$wt)
sd2 <- weighted_sd(egypt_sm18$Q424, w = egypt_sm18$wt)
sd3 <- weighted_sd(merged$hours_soc_med_num, w = merged$weight)


summary_hours <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_hours) <- c("AB", "Restricted AB", "OurData")
rownames(summary_hours) <- c( "Mean", "Standard Deviation", "Observations")


# Frequency Table
aux1 <- fre(egypt$Q424, weight = egypt$wt)[,3][1:5]
aux2 <- fre(egypt_sm18$Q424, weight = egypt_sm18$wt)[,3][1:5]
aux3 <- fre(merged$hours_soc_med_num, weight = merged$weight)[,3][1:5]

data <- as.data.frame(round(cbind(aux1,aux2,aux3), digits = 2))

rownames(data) <- c("None", "Up to 2 hrs", "Up to 5 hrs", "Up to 10 hrs", "+10")
colnames(data) <- c("AB", "ABSM", "OurData")


positions <- c("None", "Up to 2 hrs", "Up to 5 hrs", "Up to 10 hrs", "+10")

g1F <- ggplot(data = data, aes(rownames(data), AB)) +
  geom_col(aes(color = "AB"), size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"), size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  scale_x_discrete(limits = positions) + 
  geom_col(aes(y = OurData, color = "OurData"), size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.375)) +
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(F)"), " Weekly hour spent on social media"))) + 
  #ylim(0, 90) +
  scale_color_manual(name = "Dataset",  labels = c("AB","Restricted AB","OurData"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g1F, path = folder, filename = "1F.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)

# Grid arrange for a single plot
grid_1 <- grid.arrange(g1A, g1B, g1C, g1D, g1E, g1F, nrow = 3)

ggsave(grid_1, path = folder, filename = "Figure1.pdf", device = cairo_pdf,
       width = 180, height = 210, unit = "mm", dpi = 350)


# Figure 6 ---------------------------------------------------------------------
# Attitudes / Culture
# Summary Stats
map_att <- c("I strongly agree" = 1, "I agree" = 2, "I disagree" = 4, "I strongly disagree" = 5)
map_agree <- c("Strongly agree" = 1, "Agree" = 2, "Neutral" =3, "Disagree" = 4, "Strongly disagree" = 5)

husb_final_say <- as.numeric(revalue(merged$husb_final_say, map_agree))
egypt_2016$q60118_num <- as.numeric(revalue(egypt_2016$q60118, map_att))

egypt_2016_sm$q60118_num <- as.numeric(revalue(egypt_2016_sm$q60118, map_att))

egypt$Q601_18_aux <- egypt$Q601_18
egypt_sm18$Q601_18_aux <- egypt_sm18$Q601_18

if (3 %in% unique(egypt$Q601_18_aux)) {
  egypt$Q601_18_aux[which(egypt$Q601_18_aux == 4)] <- 5
  egypt$Q601_18_aux[which(egypt$Q601_18_aux == 3)] <- 4
}

if (3 %in% unique(egypt_sm18$Q601_18_aux)) {
  egypt_sm18$Q601_18_aux[which(egypt_sm18$Q601_18_aux == 4)] <- 5
  egypt_sm18$Q601_18_aux[which(egypt_sm18$Q601_18_aux == 3)] <- 4
}


finalsay <- as.data.frame(cbind(c(egypt$Q601_18_aux, as.numeric(egypt_2016$q60118_num)), 
                                c(egypt$wt, egypt_2016$wt)))

colnames(finalsay) <- c("finalSay", "wt")

finalsay_sm <- as.data.frame(cbind(c(egypt_sm18$Q601_18_aux, as.numeric(egypt_2016_sm$q60118_num)), 
                                   c(egypt_sm18$wt, egypt_2016_sm$wt)))

colnames(finalsay_sm) <- c("finalSay", "wt")

finalsay <- finalsay %>% drop_na("finalSay")
finalsay <- finalsay[!finalsay$finalSay == 98,]
finalsay <- finalsay[!finalsay$finalSay == 99,]

l1 <- length(finalsay$finalSay)
l2 <- length(finalsay_sm$finalSay)
l3 <- length(husb_final_say)
m1 <- weighted_mean(finalsay$finalSay, w = finalsay$wt)
m2 <- weighted_mean(finalsay_sm$finalSay, w = finalsay_sm$wt)
m3 <- weighted_mean(husb_final_say, w = merged$weight)
sd1 <- weighted_sd(finalsay$finalSay, w = finalsay$wt)
sd2 <- weighted_sd(finalsay_sm$finalSay, w = finalsay_sm$wt)
sd3 <- weighted_sd(husb_final_say, w = merged$weight)

summary_finalsay <- as.data.frame(rbind( c(m1,m2,m3), c(sd1,sd2,sd3), c(l1,l2,l3)))
colnames(summary_finalsay) <- c("AB", "Restricted AB", "OurData")
rownames(summary_finalsay) <- c( "Mean", "Standard Deviation", "Observations")


# Histogram
map_att2 <- c("I strongly agree" = 1, "I agree" = 2, "I disagree" = 3, "I strongly disagree" = 4)
map_agree2 <- c("Strongly agree" = 1, "Agree" = 2, "Neutral" = 0, "Disagree" = 3, "Strongly disagree" = 4)
husb_final_say2 <- as.numeric(revalue(merged$husb_final_say, map_agree2))

# Assign proportional values to values Agree and Disagree
ind <- which(husb_final_say2 == 0)
freqs <- plyr::count(husb_final_say2)$freq[3:4]
total <- sum(freqs)
p1 <- freqs[1]/total
p2 <- freqs[2]/total

ind1 <- ind[1:floor(length(ind)*p1)]
ind2 <- ind[ceiling(length(ind)*p1):length(ind)]

husb_final_say2[ind1] <-2
husb_final_say2[ind2] <- 3

egypt_2016$q60118_num2 <- as.numeric(revalue(egypt_2016$q60118, map_att2))
egypt_2016_sm$q60118_num2 <- as.numeric(revalue(egypt_2016_sm$q60118, map_att2))

finalsay2 <- as.data.frame(cbind(c(egypt$Q601_18, as.numeric(egypt_2016$q60118_num2)), 
                                 c(egypt$wt, egypt_2016$wt)))

colnames(finalsay2) <- c("finalSay", "wt")

finalsay_sm2 <- as.data.frame(cbind(c(egypt_sm18$Q601_18, as.numeric(egypt_2016_sm$q60118_num2)), 
                                    c(egypt_sm18$wt, egypt_2016_sm$wt)))

colnames(finalsay_sm2) <- c("finalSay", "wt")

finalsay2 <- finalsay2 %>% drop_na("finalSay")
finalsay2 <- finalsay2[!finalsay2$finalSay == 98,]
finalsay2 <- finalsay2[!finalsay2$finalSay == 99,]


# Save proportions
x1 <- fre(finalsay2$finalSay, weight = finalsay2$wt)[,3][1:4]
y1 <- fre(finalsay_sm2$finalSay, weight = finalsay_sm2$wt)[,3][1:4]
z1 <- fre(husb_final_say2, weight = merged$weight)[,3][1:4]

x1 <- round(as.data.frame(x1[[1]]), digits = 2)
y1 <- round(as.data.frame(y1[[1]]), digits = 2)
z1 <- round(as.data.frame(z1[[1]]), digits = 2)

data7 <- as.data.frame(cbind(x1,y1,z1))

colnames(data7) <- c("AB", "ABSM", "OurData")
rownames(data7) <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")

positions <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")


g6A <- ggplot(data7, aes(rownames(data7), AB)) +
  geom_col(aes(color = "AB"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  geom_col(aes(y = OurData, color = "OurData"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.375)) + 
  scale_x_discrete(limits = positions) + 
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(A)"), " Husband final say"))) + 
  scale_fill_manual(name = "Dataset", labels = c("AB","Restricted AB","Our data"),
                    values = c(AB = "#d5cfce", ABSM = "#767676", OurData = "#373636")) +
  scale_color_manual(name = "Dataset",  labels = c("Arab Barometer",
                                                   "Arab Barometer \ninternet users",
                                                   "Experimental"),
                     values= colors) +
  theme(legend.position = c(0.25,0.85), legend.text = legend.text.18, legend.title = legend.text.18,
        axis.text.x = final.text.14, axis.text.y = final.text.14,
        legend.background = element_rect(colour = "transparent", fill = "transparent")) +
  coord_cartesian(ylim = c(0, 70)) +
  theme(legend.key.height = unit(0.5, "cm")) +
  guides(colour = guide_legend(override.aes = list(size = 0.3, linetype = 1)))

ggsave(g6A, path = folder, filename = "6A.pdf", device = cairo_pdf,
             width=7.6, height=5.25,dpi=300)

#Prioritize Education of Men 
# Summary Stats
map_att <- c("I strongly agree" = 1, "I agree" = 2, "I disagree" = 4, "I strongly disagree" = 5)
map_agree <- c("Strongly agree" = 1, "Agree" = 2, "Neutral" =3, "Disagree" = 4, "Strongly disagree" = 5)
prio_educ_merged <- as.numeric(revalue(merged$prioritize_educ, map_agree))

egypt_2016$q6014_num <- as.numeric(revalue(egypt_2016$q6014, map_att))
egypt_2016_sm$q6014_num <- as.numeric(revalue(egypt_2016_sm$q6014, map_att))

egypt$Q601_4_aux <- egypt$Q601_4
egypt_sm18$Q601_4_aux <- egypt_sm18$Q601_4

if (3 %in% unique(egypt$Q601_4_aux)) {
  egypt$Q601_4_aux[which(egypt$Q601_4_aux == 4)] <- 5
  egypt$Q601_4_aux[which(egypt$Q601_4_aux == 3)] <- 4
}

if (3 %in% unique(egypt_sm18$Q601_4_aux)) {
  egypt_sm18$Q601_4_aux[which(egypt_sm18$Q601_4_aux == 4)] <- 5
  egypt_sm18$Q601_4_aux[which(egypt_sm18$Q601_4_aux == 3)] <- 4
}


prio_educ <- as.data.frame(cbind(c(egypt$Q601_4_aux, as.numeric(egypt_2016$q6014_num)), 
                                 c(egypt$wt, egypt_2016$wt)))

colnames(prio_educ) <- c("PrioEduc", "wt")

prio_educ_sm <- as.data.frame(cbind(c(egypt_sm18$Q601_4_aux, as.numeric(egypt_2016_sm$q6014_num)), 
                                    c(egypt_sm18$wt, egypt_2016_sm$wt)))

colnames(prio_educ_sm) <- c("PrioEduc", "wt")

prio_educ <- prio_educ %>% drop_na("PrioEduc")
prio_educ <- prio_educ[!prio_educ$PrioEduc == 98,]
prio_educ <- prio_educ[!prio_educ$PrioEduc == 99,]

l1 <- length(prio_educ$PrioEduc)
l2 <- length(prio_educ_sm$PrioEduc)
l3 <- length(prio_educ_merged)
m1 <- weighted_mean(prio_educ$PrioEduc, w = prio_educ$wt)
m2 <- weighted_mean(prio_educ_sm$PrioEduc, w = prio_educ_sm$wt)
m3 <- weighted_mean(prio_educ_merged, w = merged$weight)
sd1 <- weighted_sd(prio_educ$PrioEduc, w = prio_educ$wt)
sd2 <- weighted_sd(prio_educ_sm$PrioEduc, w = prio_educ_sm$wt)
sd3 <- weighted_sd(prio_educ_merged, w = merged$weight)

summary_prio <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_prio) <- c("AB", "Restricted AB", "OurData")
rownames(summary_prio) <- c( "Mean", "Standard Deviation", "Observations")


# Histogram-----------------------------------------------------------------------------------
# 2016: q6014 = education of men 
# 2018: Q601_4

map_att2 <- c("I strongly agree" = 1, "I agree" = 2, "I disagree" = 3, "I strongly disagree" = 4)
map_agree2 <- c("Strongly agree" = 1, "Agree" = 2, "Neutral" = 0, "Disagree" = 3, "Strongly disagree" = 4)
prio_educ_merged2 <- as.numeric(revalue(merged$prioritize_educ, map_agree2))

# Assign proportional values to values Agree and Disagree
ind <- which(prio_educ_merged2 == 0)
freqs <- plyr::count(prio_educ_merged2)$freq[3:4]
total <- sum(freqs)
p1 <- freqs[1]/total
p2 <- freqs[2]/total

ind1 <- ind[1:floor(length(ind)*p1)]
ind2 <- ind[ceiling(length(ind)*p1):length(ind)]

prio_educ_merged2[ind1] <-2
prio_educ_merged2[ind2] <- 3

egypt_2016$q6014_num2 <- as.numeric(revalue(egypt_2016$q6014, map_att2))
egypt_2016_sm$q6014_num2 <- as.numeric(revalue(egypt_2016_sm$q6014, map_att2))

prio_educ2 <- as.data.frame(cbind(c(egypt$Q601_4, as.numeric(egypt_2016$q6014_num2)), 
                                  c(egypt$wt, egypt_2016$wt)))

colnames(prio_educ2) <- c("PrioEduc", "wt")

prio_educ_sm2 <- as.data.frame(cbind(c(egypt_sm18$Q601_4, as.numeric(egypt_2016_sm$q6014_num2)), 
                                     c(egypt_sm18$wt, egypt_2016_sm$wt)))

colnames(prio_educ_sm2) <- c("PrioEduc", "wt")

prio_educ2 <- prio_educ2 %>% drop_na("PrioEduc")
prio_educ2 <- prio_educ2[!prio_educ2$PrioEduc == 98,]
prio_educ2 <- prio_educ2[!prio_educ2$PrioEduc == 99,]

# Save proportions
x1 <- fre(prio_educ2$PrioEduc, weight = prio_educ2$wt)[,3][1:4]
y1 <- fre(prio_educ_sm2$PrioEduc, weight = prio_educ_sm2$wt)[,3][1:4]
z1 <- fre(prio_educ_merged2, weight = merged$weight)[,3][1:4]


x1 <- round(as.data.frame(x1[[1]]), digits = 2)
y1 <- round(as.data.frame(y1[[1]]), digits = 2)
z1 <- round(as.data.frame(z1[[1]]), digits = 2)


data8 <- as.data.frame(cbind(x1,y1,z1))

colnames(data8) <- c("AB", "ABSM", "OurData")
rownames(data8) <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")

positions <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree")


g6B <- ggplot(data8, aes(rownames(data8), AB)) +
  geom_col(aes(color = "AB"),  size = global_size, fill = fill_all,
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  geom_col(aes(y = OurData, color = "OurData"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.375)) + 
  scale_x_discrete(limits = positions) + 
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(B)"), " Male education priority"))) + 
  scale_color_manual(name = "Dataset",  labels = c("AB","Restricted AB","OurData"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) +
  coord_cartesian(ylim = c(0, 70)) 

ggsave(g6B, path = folder, filename = "6B.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)

# Support from: 
# female/male relative 
# Arab Barometer
egypt$sum <- egypt$Q851D_1 + egypt$Q851D_2
ind <- which(egypt$sum == 2)
egypt$sum[ind] <- 1

# Arab Barometer restricted to internet users
egypt_sm18$sum <- egypt_sm18$Q851D_1 + egypt_sm18$Q851D_2
ind <- which(egypt_sm18$sum == 2)
egypt_sm18$sum[ind] <- 1

# Experimental data
indexes_fam <- which(merged$talk_family_num == 4 | merged$talk_family_num == 5)
indexes_husb <- which(merged$talk_husband_num == 4 | merged$talk_husband_num == 5)

index1 <- c(indexes_fam, indexes_husb) # all fam and husb combined
index1 <- unique(index1) # only unique indexes
merged$aux1 <- 0
merged$aux1[index1] <- 1

l1 <- sum(!is.na(egypt$sum))
l2 <- sum(!is.na(egypt_sm18$sum))
l3 <- length(merged$talk_family_num)
m1 <- weighted_mean(egypt$sum, w = egypt$wt)
m2 <- weighted_mean(egypt_sm18$sum, w = egypt_sm18$wt)
m3 <- weighted_mean(merged$aux1, w = merged$weight)
sd1 <- weighted_sd(egypt$sum, w = egypt$wt)
sd2 <- weighted_sd(egypt_sm18$sum, w = egypt_sm18$wt)
sd3 <- weighted_sd(merged$aux1, w = merged$weight)


summary_relative <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_relative) <- c("AB", "Restricted AB", "OurData")
rownames(summary_relative) <- c( "Mean", "Standard Deviation", "Observations")


x1 <- round(m1*100,digits = 2)
x2 <- round(m2*100,digits = 2)
x3 <- round(m3*100,digits = 2)

# local police 
# Arab Barometer 
egypt$sum2 <- egypt$Q851D_3 + egypt$Q851D_4
inde <- which(egypt$sum2 == 2)
egypt$sum2[inde] <- 1

# Arab Barometer restricted to internet users
egypt_sm18$sum2 <- egypt_sm18$Q851D_3 + egypt_sm18$Q851D_4
inde2 <- which(egypt_sm18$sum2 == 2)
egypt_sm18$sum2[inde2] <- 1

# Experimental data
ind2 <- which(merged$report_authorities_num == 4 | merged$report_authorities_num == 5)
merged$aux2 <- 0
merged$aux2[ind2] <- 1

l1 <- sum(!is.na(egypt$sum2))
l2 <- sum(!is.na(egypt_sm18$sum2))
l3 <- length(merged$talk_family_num)
m1 <- weighted_mean(egypt$sum2, w = egypt$wt)
m2 <- weighted_mean(egypt_sm18$sum2, w = egypt_sm18$wt)
m3 <- weighted_mean(merged$aux2, w = merged$weight)
sd1 <- weighted_sd(egypt$sum2, w = egypt$wt)
sd2 <- weighted_sd(egypt_sm18$sum2, w = egypt_sm18$wt)
sd3 <- weighted_sd(merged$aux2, w = merged$weight)

summary_police <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_police) <- c("AB", "Restricted AB", "OurData")
rownames(summary_police) <- c( "Mean", "Standard Deviation", "Observations")

y1 <- round(m1*100,digits = 2)
y2 <- round(m2*100,digits = 2)
y3 <- round(m3*100,digits = 2)


# Contact Org

ind3 <- which(merged$contact_org_num == 4 | merged$contact_org_num == 5)

merged$aux3 <- 0
merged$aux3[ind3] <- 1

l1 <- sum(!is.na(egypt$Q851D_5))
l2 <- sum(!is.na(egypt_sm18$Q851D_5))
l3 <- length(merged$contact_org_num)
m1 <- weighted_mean(egypt$Q851D_5, w = egypt$wt)
m2 <- weighted_mean(egypt_sm18$Q851D_5, w = egypt_sm18$wt)
m3 <- weighted_mean(merged$aux3, w = merged$weight)
sd1 <- weighted_sd(egypt$Q851D_5, w = egypt$wt)
sd2 <- weighted_sd(egypt_sm18$Q851D_5, w = egypt_sm18$wt)
sd3 <- weighted_sd(merged$aux3, w = merged$weight)


summary_org <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_org) <- c("AB", "Restricted AB", "OurData")
rownames(summary_org) <- c( "Mean", "Standard Deviation", "Observations")


z1 <- round(m1*100,digits = 2)
z2 <- round(m2*100,digits = 2)
z3 <- round(m3*100,digits = 2)

# Histogram

data9 <- round(as.data.frame(cbind(as.numeric(c(x1,y1,z1)), as.numeric(c(x2,y2,z2)), as.numeric(c(x3,y3,z3)))), digits = 2)
colnames(data9) <- c("AB", "ABSM", "OurData")
rownames(data9) <- c("Relative", "Police/Authorities", "Organization")

positions <- c("Relative", "Police/Authorities", "Organization")

g6C <- ggplot(data9, aes(rownames(data9), AB)) +
  geom_col(aes(color = "AB"),  size = global_size, fill = fill_all,
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  geom_col(aes(y = OurData, color = "OurData"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.375)) + 
  scale_x_discrete(limits = positions) + 
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(C)"), " Support from"))) + 
  ylim(0, 90) +
  scale_color_manual(name = "Dataset",  labels = c("AB","Restricted AB","OurData"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g6C, path = folder, filename = "6C.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)

#Domestic violence -------------------------------------------------------------------
# Experience Violence Before

# Q851C: Is the person who experienced this type of behavior.?
# Arab Barometer 
egypt$aux <- 0
egypt$aux[which(egypt$Q851C == 2)] <- 1
egypt$aux[which(egypt$Q851C == 3)] <- 1

# Arab Barometer restricted to internet users
egypt_sm18$auxsm <- 0
egypt_sm18$auxsm[which(egypt_sm18$Q851C == 2)] <- 1
egypt_sm18$auxsm[which(egypt_sm18$Q851C == 3)] <- 1

# Experimental data
merged$hit_bcovid <- 0
merged$hit_bcovid[which(merged$bc19_hit != "Never")] <- 1

l1 <- length(egypt$aux)
l2 <- length(egypt_sm18$auxsm)
l3 <- length(merged$hit_bcovid)
m1 <- weighted_mean(egypt$aux, w = egypt$wt)
m2 <- weighted_mean(egypt_sm18$auxsm, w = egypt_sm18$wt)
m3 <- weighted_mean(merged$hit_bcovid, w = merged$weight)
sd1 <- weighted_sd(egypt$aux, w = egypt$wt)
sd2 <- weighted_sd(egypt_sm18$auxsm, w = egypt_sm18$wt)
sd3 <- weighted_sd(merged$hit_bcovid, w = merged$weight)

summary_violence <- as.data.frame(rbind(c(m1,m2,m3), c(sd1,sd2,sd3),c(l1,l2,l3)))
colnames(summary_violence) <- c("AB", "Restricted AB", "OurData")
rownames(summary_violence) <- c( "Mean", "Standard Deviation", "Observations")


# Histogram for Q851C and before COVID-19 Hitting
x1 <- fre(egypt$aux, weight = egypt$wt)[,3][2]
x2 <- fre(egypt_sm18$auxsm, weight = egypt_sm18$wt)[,3][2] 
x3 <- fre(merged$hit_bcovid, weight = merged$weight)[,3][2]

x1 <- round(as.data.frame(x1[[1]]), digits = 2)
x2 <- round(as.data.frame(x2[[1]]), digits = 2)
x3 <- round(as.data.frame(x3[[1]]), digits = 2)

data10 <- as.data.frame(cbind(x1,x2,x3))
colnames(data10) <- c("AB", "ABSM", "OurData")
rownames(data10) <- c("Yes")


g6D <- ggplot(data10, aes(rownames(data10), AB)) +
  geom_col(aes(color = "AB"),  size = global_size, fill = fill_all,
           width = 0.275,
           position = position_nudge(x = -0.225)) +
  geom_col(aes(y = ABSM, color = "ABSM"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.075)) +
  geom_col(aes(y = OurData, color = "OurData"),  size = global_size, fill = fill_all, 
           width = 0.275,
           position = position_nudge(x = 0.375)) + 
  th_egypt +
  ylab("Percentage of respondents") + 
  xlab(expression(paste(bold("(D)"), " Experienced violence"))) + 
  ylim(0, 90) +
  scale_color_manual(name = "Dataset",  labels = c("AB","Restricted AB","OurData"),
                     values= colors) +
  theme(legend.position = "none", axis.text.x = final.text.14, axis.text.y = final.text.14) 

ggsave(g6D, path = folder, filename = "6D.pdf", device = cairo_pdf,
             width=7, height=5,dpi=300)

# Grid arrange for a single plot
grid_6 <- grid.arrange(g6A, g6B, g6C, g6D, nrow = 2)

ggsave(grid_6, path = folder, filename = "Figure6.pdf", device = cairo_pdf,
       width = 180, height = 180, unit = "mm", dpi = 350)


# Generate Tables ---------------------------------------------------------------
# Auxiliary function to resize box
resizebox <- function(xtable) {
  
  xtable <- gsub("begin\\{tabular", "\\resizebox{\\\\textwidth}{!}{ \\\\begin{tabular", xtable)
  xtable <- gsub("end\\{tabular", "end\\{tabular}", xtable)
  
  return(xtable) 
}

# Auxiliary function to add notes
add_notes <- function(xtable, note) {
  
  xtable <- gsub("\\\\end\\{table", "\\\\justify {\\\\footnotesize \\\\textit{Notes:} %} \\\\end\\{table", xtable)
  xtable <- gsub("%", note, xtable)  
  
  return(xtable) 
}

# Generate table S4
names <- c("Age", "", "",
           "Education", "", "", 
           "Whether single", "", "",
           "Whether engaged", "", "",
           "Whether married", "", "",
           "Whether separated", "", "",
           "Whether widowed", "", "",
           "Social status", "", "",
           "Number of children", "", "",
           "Facebook", "", "",
           "WhatsApp", "", "",
           "YouTube", "", "",
           "Instagram", "", "",
           "Twitter", "", "",
           "Snapchat", "", "",
           "Hours spent on social media", "", "")

year <- c(rep(c("2016, 2018", "", ""), 10),
          rep(c("2018", "", ""), 2),
          rep(c("2016, 2018", "", ""), 2),
          rep(c("2018", "", ""), 2))

tableS31 <- rbind.data.frame(summary_age,
                            summary_educ,
                            summary_single,
                            summary_engaged,
                            summary_married,
                            summary_separated,
                            summary_widowed,
                            summary_ss,
                            summary_child,
                            summary_fb,
                            summary_wa,
                            summary_yt,
                            summary_ins,
                            summary_tw,
                            summary_sp,
                            summary_hours)


M = matrix(c(rep(0,3),rep(0,3), rep(c(3,3,0),3), rep(0,3)), nrow = 3)

M <- matrix(rep(t(M), 16), ncol = ncol(M), byrow = TRUE)

tableS31 <- cbind.data.frame(names, tableS31, year)
colnames(tableS31) <- c("", "Arab Barometer sample", "Arab Barometer internet user sample", "Experimental sample", "Arab Barometer survey years")

tableS31 <- xtable(tableS31, 
                  digits = M, 
                  label = "tab:abc_1",
                  align = "clcccc",
                  caption = "Summary statistics of comparable demographics both in the Arab Barometer sample, the Arab Barometer internet user sample, and the experimental sample")

names(tableS31)[2]<-"\\multicolumn{1}{>{\\centering}p{3cm}}{Arab Barometer sample}"
names(tableS31)[3]<-"\\multicolumn{1}{>{\\centering}p{3.5cm}}{Arab Barometer internet user sample}"
names(tableS31)[4]<-"\\multicolumn{1}{>{\\centering}p{3cm}}{Experimental sample}"
names(tableS31)[5]<-"\\multicolumn{1}{>{\\centering}p{3.5cm}}{Arab Barometer survey years}"

TableS31 <- print(tableS31, 
                 table.placement = "H",
                 caption.placement = "top",
                 include.rownames = FALSE,
                 comment = FALSE, 
                 sanitize.colnames.function=function(x){x},
                 hline.after = c(-1,seq(0,48, by = 3)))


TableS31 <- resizebox(TableS31)
note <- c("For every variable, each row shows the mean, standard deviation, and number of observations.")
TableS31 <- add_notes(TableS31, note)

# save table
cat(TableS31,"\n", file = 'Tables/S31.tex')


# Table S32
names <- c("Husband final say", "", "",
           "Prioritize the education of men", "", "", 
           "Support from a relative", "", "",
           "Support from local police/authority", "", "",
           "Support from organization", "", "",
           "Experienced violence", "", "")

year <- c(rep(c("2016, 2018", "", ""), 2),
          rep(c("2018", "", ""), 4))

tableS32 <- rbind.data.frame(summary_finalsay,
                             summary_prio,
                             summary_relative,
                             summary_police,
                             summary_org,
                             summary_violence)

M = matrix(c(rep(0,3),rep(0,3), rep(c(3,3,0),3), rep(0,3)), nrow = 3)

M <- matrix(rep(t(M), 6), ncol = ncol(M), byrow = TRUE)

tableS32 <- cbind.data.frame(names, tableS32, year)

colnames(tableS32) <- c("", "Arab Barometer sample", "Arab Barometer internet user sample", "Experimental sample", "Arab Barometer survey years")


tableS32 <- xtable(tableS32, 
                   digits = M, 
                   label = "tab:abc_2",
                   align = "clcccc",
                   caption = "Summary statistics of comparable outcomes both in the Arab Barometer sample, the Arab Barometer internet user sample, and the experimental sample")


names(tableS31)[2]<-"\\multicolumn{1}{>{\\centering}p{3cm}}{Arab Barometer sample}"
names(tableS31)[3]<-"\\multicolumn{1}{>{\\centering}p{3.5cm}}{Arab Barometer internet user sample}"
names(tableS31)[4]<-"\\multicolumn{1}{>{\\centering}p{3cm}}{Experimental sample}"
names(tableS31)[5]<-"\\multicolumn{1}{>{\\centering}p{3.5cm}}{Arab Barometer survey years}"


Tables32 <- print(tableS32, 
                  table.placement = "H",
                  caption.placement = "top",
                  include.rownames = FALSE,
                  comment = FALSE, 
                  sanitize.colnames.function=function(x){x},
                  scalebox = 1,
                  hline.after = c(-1,seq(0,18, by = 3)))

Tables32 <- resizebox(Tables32)

note <- c('For every variable, each row shows the mean, standard deviation, and number of observations.
The "Support from" variables differ in both surveys: the Arab Barometer survey asked whether respondents thought that a family member who was abused would be able to receive assistance from each of the actors, and our survey asked whether respondents would recommend a friend or family member who was abused to reach each of the actors. (2) The "Experienced violence" variable differs in both surveys: the Arab Barometer survey asked if in the last twelve months a female member of the household was abused by another member, and our survey asked whether, in the month before the COVID-19 pandemic, they heard of someone or themselves experienced being hit by a man.')

Tables32 <- add_notes(Tables32, note)

# Save table
cat(Tables32,"\n", file = 'Tables/S32.tex')

