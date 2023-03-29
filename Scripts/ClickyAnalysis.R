# Clicky Analysis

# Clean Actions dataset
a <- read.csv("Datasets/ClickyAnalysis/Actions.csv", fileEncoding = "latin1")

# This process gets rid of my own IP addresses
my_IPs <- a %>% group_by(IP.Address) %>% 
  count() %>% 
  arrange(desc(n)) %>%  
  filter(row_number()==1 | row_number()==3)

a <- a[!(a$IP.Address %in% my_IPs$IP.Address[1:5]),]

# Clean Visitors dataset
c <- read.csv("Datasets/ClickyAnalysis/Visitors.csv", fileEncoding = "latin1")

my_loc <- c %>% group_by(Geolocation) %>% 
  count() %>% 
  arrange(desc(n))

c <- c[!str_detect(c$Geolocation, "USA"),]

visits_latlong <- c %>% group_by(Latitude, Longitude) %>% count() %>% arrange(desc(n))
visits_geoloc <- c %>% group_by(Geolocation) %>% count() %>% arrange(desc(n))
# 4,149 visitors excluding visitors from the USA

# Merge Action and selected visitors
c_l <- dplyr::select(c, Total.Time, IP.Address, UID, Landing.page, Geolocation, Latitude, Longitude)
# there are 4,149 visitors
# there are 7124 actions

# this joins the actions with the selected visitors, as both contain different forms of data that are useful
a_c <- left_join(a, c_l, by = c("IP.Address" = "IP.Address"))

# when i join them there are 11453, suggesting every IP does match
# below i filter by unique visits and user id
# looking at unique users
UID <- a_c %>% group_by(UID) %>% dplyr::summarize(count = n(), 
                                           IP = IP.Address[1], 
                                           location = Geolocation[1]) %>% arrange(desc(count))

# I exclude the top visitors as these were us, who were moderating the data
a_c <- a_c[!(a_c$UID %in% UID$UID[1:6]),]

# i don't use this
c_a <- left_join(c_l, a, by = c("IP.Address" = "IP.Address"))

# 101 with no ID
UID <- a_c %>% group_by(UID) %>% count() %>% arrange(desc(n))

# just checking lengths
length(unique(a_c$IP.Address))
length(unique(c_l$IP.Address))
# 3,433

length(unique(a_c$IP.Address))
# 3,433 unique IP addresses

length(unique(a_c$UID))
# 1,552 UID

total.time <- a_c %>% group_by(UID) %>% summarize(total.time = sum(Total.Time))
summary(total.time$total.time)
# median time on the site was 241 second, 3rd quartile was 10 minutes

# Set up for ggplot -------------------------------------------------------------
folder <- c('Figures')
global_size <- 0.5

final.text.14 <- element_text(color = "black", size = 7, hjust = 0.5, family = "Arial")
legend.text.5 <- element_text(color = "black", size = 5, family = "Arial")

th_ca <-  theme(strip.text.x = final.text.14,
                strip.placement = "inside",
                strip.background = element_rect(colour = "black", fill = "white"),
                axis.text = legend.text.5,
                axis.title.x = final.text.14,
                axis.title.y = final.text.14,
                legend.text = final.text.14,
                legend.title = final.text.14,
                panel.border = element_rect(colour = "black", fill = NA),
                legend.position="bottom")

colors_ca <- c("#D55E00","#0072B2")

# Figure S2
# Retrieve clicks from the Facebook treatment
fb <- c[str_detect(c$Landing.page, "fb|http://www.mshlwa7dek.org/egypt/video-play.html"),]

names(fb)[2] <- "Time"

sorted_fb <- fb %>% group_by(Landing.page) %>% dplyr::summarise(time = Time[length(Time)], time_pretty = Time..pretty.[1], count = n()) %>% arrange(desc(time))
sorted_fb <- sorted_fb[-c(2,12:13),]
sorted_fb <- sorted_fb[c(1:7,9,11,10,8,12:15),]
sorted_fb <- sorted_fb[-11,]

sorted_fb$video_number <- c(14:1)
sorted_fb$Distribution <- c(rep("WhatsApp", 10), rep("Facebook", 4))

# Normalize the clikcs by the total number of individuals in the Facebook treatment group
sorted_fb$count_normalized <- sorted_fb$count/418

# Retrieve clicks from the WhtasApp individual treatment
wi <- c[str_detect(c$Landing.page, "wi"),] 

names(wi)[2] <- "Time"

sorted_wi <- wi %>% group_by(Landing.page) %>% dplyr::summarize(time = Time[length(Time)], time_pretty = Time..pretty.[1], count = n()) %>% arrange(desc(time))

sorted_wi <- sorted_wi[c(1:12,14,13),]

sorted_wi$video_number <- c(14:1)
sorted_wi$Distribution <- c(rep("WhatsApp", 10), rep("Facebook", 4))

# Normalize the clikcs by the total number of individuals in the WhatsApp Ind. treatment group
sorted_wi$count_normalized <- sorted_wi$count/824

aux1 <- rbind.data.frame(sorted_fb,sorted_wi)
aux <- cbind.data.frame(aux1, c(rep("Facebook Treatment", 14), rep("WhatsApp Ind. Treatment", 14) ))
colnames(aux) <- c(colnames(aux1), "wrap")

dist_change <- ggplot(aux, aes(y = count_normalized, x = video_number)) + 
  geom_bar(aes(color = Distribution), stat = "identity", size = global_size, fill = "white") +
  facet_grid(. ~ wrap, switch="both", labeller = labeller(variables = label_wrap_gen(25))) +
  xlab("Video number") + 
  ylab("Normalized total visits") + 
  scale_color_manual(values = colors_ca, name = "Distribution shift", 
                     labels = c("Before", "After")) + 
  geom_vline(xintercept = 4.5, size = global_size) + 
  theme_light() +
  th_ca +
  theme(strip.text.x = final.text.14,
        strip.placement = "inside",
        strip.background = element_rect(colour = "black", fill = "white"),
        panel.border = element_rect(colour = "black", fill = "NA"),
        legend.position="bottom") +
  scale_x_discrete(limits = c(1,14)) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) 


ggsave(dist_change, path = 'Figures', filename = "Extended_Data_Fig2.jpg", 
       width = 180, height = 130, units = "mm", dpi = 350)

# ggsave(dist_change, path = 'Figures', filename = "Extended_Data_Fig2.pdf", device = cairo_pdf, 
#        width = 180, height = 130, units = "mm", dpi = 350)


# ggsave(dist_change, path = 'Figures', filename = "S2.pdf", device = cairo_pdf, 
#                      width=7, height=5,dpi=300)


# Difference in difference analysis --------------------------------------------
# Prepare the data
dd_data <- aux[c("video_number", "count_normalized")]
dd_data <- cbind.data.frame(dd_data, c(rep("F",14), rep("WI",14)), c(rep(1,10),rep(0,4),rep(1,10),rep(0,4)),
                            c(rep(1,14), rep(0,14)))
colnames(dd_data) <- c(colnames(dd_data)[1:2], "Assignation", "post", "Treatment")

# Run the DID regression
# did_reg <- lm(count_normalized ~ post*Treatment + post + Treatment + factor(video_number), data = dd_data, singular.ok = TRUE)

# Regression without FE for the Figure with means.
did_reg2 <- lm(count_normalized ~ post*Treatment + post + Treatment, data = dd_data)

# The mean is written below for comparison
# Facebook post = 0
fb_before <- lincom(did_reg2, c("(Intercept) + Treatment"))[1:3]

# WhatsApp post = 0 
w_before <- lincom(did_reg2, c("(Intercept)"))[1:3]

# Facebook post = 1
fb_after <- lincom(did_reg2, c("(Intercept) + Treatment + post + post:Treatment"))[1:3]

# WhatsApp post = 1
w_after <- lincom(did_reg2, c("(Intercept) + post"))[1:3]

# Create the data
data_est <- rbind.data.frame(fb_before, w_before, fb_after, w_after)

data_est <- cbind.data.frame(data_est, c(rep("Before distribution shift",2), rep("After distribution shift",2)), 
                             c("Facebook", "WhatsApp", "Facebook", "WhatsApp"))

rownames(data_est) <- c("Facebook_b", "WhatsApp_b", "Facebook_a", "WhatsApp_a")
colnames(data_est) <- c("Estimate", "left", "right", "period", "Treatment")

positions <- c("Facebook", "WhatsApp")
data_est$period <- factor(data_est$period, levels = c("Before distribution shift", "After distribution shift"))
data_est$Treatment <- factor(data_est$Treatment, levels = positions)


dist_dotted <- ggplot(data_est, aes(Treatment, Estimate), color = factor(Treatment)) + 
  geom_point(aes(color = factor(Treatment)), size = 5, fill = "#d3d3d3") +
  geom_errorbar(aes(ymin= left, ymax= right), width = .5) +
  facet_grid(. ~ period, switch="both", labeller = labeller(variables = label_wrap_gen(25))) +
  scale_x_discrete(limits = positions) +
  theme_light() +
  th_ca + 
  scale_color_manual(name = "Treatment",  labels = c("Facebok","WhatsApp Ind."),
                     values= colors_ca) +
  ylab("Mean") + 
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  geom_hline(yintercept=0, size = 0.8)

ggsave(dist_dotted, path = 'Figures', filename = "S3.pdf", device = cairo_pdf, 
                     width=7, height=5,dpi=300)


# -------------------------------------------------------------------------------
# setup for ggplots
th_pages <- theme(axis.title.x = final.text.14,
                  axis.title.y = final.text.14,
                  axis.text = legend.text.5, 
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))

wg_ac <- a_c[str_detect(a_c$Action.URL, "wg"),]
wi_ac <- a_c[str_detect(a_c$Action.URL, "wi"),]
fb_ac <- a_c[str_detect(a_c$Action.URL, "fb|http://www.mshlwa7dek.org/egypt/video-play.html"),]
multi_site <- a_c %>% filter(str_detect(a_c$Action.URL, "wg&wi"))

user_time <- a_c %>% group_by(UID) %>% summarize(sum_user_time = sum(Total.Time), geo_loc = Geolocation[1]) %>%
  arrange(desc(-sum_user_time))
user_time <- user_time[-c(1:10),]
summary(user_time$sum_user_time)

# Generate Figure S1
all_users <- a_c %>% group_by(UID) %>% summarize(pages = length(unique(Action.URL))) %>% arrange(desc(pages))

# original gray scale: #7e7e7e
fill_all <- c("white")
mean_col <- c("#0072B2")
margin_col <- c("#000000")

# All treatments 
all_users_graph <- all_users %>% ggplot(aes(x = pages)) + 
  geom_histogram(bins = 15, fill = fill_all, col= margin_col, size = global_size) + 
  xlim(c(0,15)) +
  geom_vline(aes(xintercept = mean(pages)), col = mean_col, size = global_size) + 
  xlab(expression(paste(bold("(A)"), " All treatment pages"))) + 
  ylab("Frequency") + 
  annotate("text", x = 4.5, y = 600, label = "Mean = 2.6", family = "Arial", size = 2) +
  th_pages

ggsave(all_users_graph, path = 'Figures', filename = "S1_A.pdf", device = cairo_pdf, 
                         width=7, height=5,dpi=300)

# Facebook Treatment
page_a_fb <- fb_ac %>% group_by(UID) %>% summarize(pages = length(unique(Action.URL)))

p_fb_a <- page_a_fb %>% ggplot(aes(x = pages)) + 
  geom_histogram(bins = 15, fill = fill_all, col= margin_col, size = global_size) + 
  ylim(c(0,400)) + 
  geom_vline(aes(xintercept = mean(pages)), col = mean_col, size = global_size)  + 
  xlab(expression(paste(bold("(B)"), " Facebook treatment pages"))) + 
  ylab("") +
  annotate("text", x = 4.5, y = 300, label = "Mean = 2.3", family = "Arial", size = 2) +
  th_pages

ggsave(p_fb_a, path = 'Figures', filename = "S1_B.pdf", device = cairo_pdf, 
                width=7, height=5,dpi=300)

# Individual WhatsApp Treatment
page_a_wi <- wi_ac %>% group_by(UID) %>% summarize(pages = length(unique(Action.URL)))

p_wi_a <- page_a_wi %>% ggplot(aes(x = pages)) + 
  geom_histogram(bins = 15, fill = fill_all, col= margin_col, size = global_size) + 
  ylim(c(0,400)) + 
  geom_vline(aes(xintercept = mean(pages)), col = mean_col, size = global_size)  + 
  xlab(expression(paste(bold("(C)"), " WhatsApp individual treatment pages"))) + 
  ylab("Frequency") +
  annotate("text", x = 4.5, y = 350, label = "Mean = 2.8", family = "Arial", size = 2) +
  th_pages

ggsave(p_wi_a, path = 'Figures', filename = "S1_C.pdf", device = cairo_pdf, 
                width=7, height=5,dpi=300)

# Group WhatsApp Treatment
page_a_wg <- wg_ac %>% group_by(UID) %>% summarize(pages = length(unique(Action.URL))) %>% arrange(desc(pages))

pawg_a <- page_a_wg %>% ggplot(aes(x = pages)) + 
  geom_histogram(bins = 15, fill = fill_all, col= margin_col, size = global_size) + 
  geom_vline(aes(xintercept = mean(pages)), col = mean_col, size = global_size) + 
  xlab(expression(paste(bold("(D)"), " WhatsApp group treatment pages"))) + 
  ylim(c(0,400)) + 
  ylab("") +
  annotate("text", x = 4.5, y = 350, label = "Mean = 2.4", family = "Arial", size = 2) +
  th_pages

ggsave(pawg_a, path = 'Figures', filename = "S1_D.pdf", device = cairo_pdf, 
                width=7, height=5,dpi=300)

# Create a grid with four subplots
grid_S1 <- grid.arrange(all_users_graph, p_fb_a, p_wi_a, pawg_a, nrow = 2)


ggsave(grid_S1, path = folder, filename = "Extended_Data_Fig1.jpg",
       width = 180, height = 150, unit = "mm", dpi = 350)

# ggsave(grid_S1, path = folder, filename = "Extended_Data_Fig1.pdf", device = cairo_pdf,
#        width = 180, height = 150, unit = "mm", dpi = 350)

# Tables -----------------------------------------------------------------------
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

# Table S3
endline_responses <- read.csv("Datasets/Finaldata/Finaldata.csv", fileEncoding = "latin1")
baseline_responses <- read_excel("Datasets/InitialData/merged_baseline_endline_030921.xlsx")

baseline_responses <- baseline_responses %>% group_by(assignment.y) %>% count()
endline_responses <- endline_responses %>% group_by(assignment.y) %>% count()

baseline_responses <- baseline_responses[c(1,2,5,4,3),]
endline_responses <- endline_responses[c(1,2,5,4,3),]

#blocksF <- c(10,10,10,0,0)
ProbF <- c("1/5", "3/5", "1/5", "0", "0")
#blocksW <- c(50, 0, 50, 50, 50)
ProbW <- c("1/5", "0", "1/5", "2/5", "1/5")

names <- c("Control", "Facebook", "WhatsApp Individual", "WhatsApp Group", "TV Show Reminder")

TableS3 <- cbind.data.frame(names, baseline_responses$n, ProbF, ProbW, endline_responses$n)

colnames(TableS3) <- c("Treatment", "Baseline",  "Treatment probability", "Treatment probability", "Endline")

TableS3$`Response Rate` <- round(endline_responses$n/baseline_responses$n, digits = 3)

TableS3 <- rbind(TableS3, c("Total", sum(baseline_responses$n), NA, NA, NA, NA, sum(endline_responses$n), 
                            round(sum(endline_responses$n)/sum(baseline_responses$n), digits = 3)))

addtorow <- list(
  pos = list(-1,0, 5, 6), 
  command = c( "\\hline \\multicolumn{2}{l}{} & \\multicolumn{1}{c}{\\shortstack{With Facebook\\\\ account}} & \\multicolumn{1}{c}{\\shortstack{Only with WhatsApp \\\\account}} \\\\ \\cmidrule(l{2pt}r{2pt}){3-3} \\cmidrule(l{2pt}r{2pt}){4-4} ",
               "\\hline ",
               "\\hline \\hline ",
               "\\hline "))

TableS3 <- xtable(TableS3, 
                  label = "tab:randomization", 
                  align = "clccccc",
                  caption = "Block sizes, treatment probabilities and responses rates by treatment assignment")

#names(TableS3)[3]<-"\\multicolumn{1}{>{\\centering}p{2cm}}{Block size}"
names(TableS3)[3]<-"\\multicolumn{1}{>{\\centering}p{2cm}}{Treatment probability}"
#names(TableS3)[5]<-"\\multicolumn{1}{>{\\centering}p{2cm}}{Block size}"
names(TableS3)[4]<-"\\multicolumn{1}{>{\\centering}p{2cm}}{Treatment probability}"
names(TableS3)[5]<-"\\multicolumn{1}{>{\\centering}p{1cm}}{Endline}"
names(TableS3)[6]<-"\\multicolumn{1}{>{\\centering}p{2cm}}{Response rate}"

TableS3 <- print(TableS3, 
                 table.placement = "H",
                 caption.placement = "top",
                 include.rownames = FALSE,
                 comment = FALSE, 
                 add.to.row =  addtorow,
                 sanitize.colnames.function=function(x){x},
                 hline.after = NULL)


TableS3 <- resizebox(TableS3)
note <- c("We block randomized treatment assignment separately according to whether we could identify the Facebook account of the baseline survey respondent. 
          Blocks are of size 10 when Facebook accounts are available, and of size 50 when only WhatsApp accounts are available.” ")
TableS3 <- add_notes(TableS3, note)

# Table S13
unique_UID <- c(length(unique(fb_ac$UID)), length(unique(wi_ac$UID)), length(unique(wg_ac$UID)))
unique_IP <- c(length(unique(fb_ac$IP.Address)), length(unique(wi_ac$IP.Address)), length(unique(wg_ac$IP.Address)))
median_time <- c(median(fb_ac$Total.Time), median(wi_ac$Total.Time), median(wg_ac$Total.Time), median(a_c$Total.Time))
# total actions
fb_a <- a[str_detect(a$Action.URL, "fb|http://www.mshlwa7dek.org/egypt/video-play.html"),]
wg_a <- a[str_detect(a$Action.URL, "wg"),]
wi_a <- a[str_detect(a$Action.URL, "wi"),]
total_actions <- c(nrow(fb_a), nrow(wi_a), nrow(wg_a))

median_time <- c("4:02", "4:01", "3:57", "4:01")
rm(visits_assignment)

# imported from baseline_pap+11012020 page
ab_noe <- read.csv("Datasets/ClickyAnalysis/cleaned_baseline_wdupes.csv", , fileEncoding = "latin1")

assignment_no <- ab_noe %>% group_by(assignment.y) %>% count()
assignment_no <- assignment_no[c(2,5,4),]


visits_assignment <- cbind(unique_UID, unique_IP, assignment_no[,2], total_actions)
totals <- colSums(visits_assignment[,1:4])
visits_assignment <- rbind(visits_assignment, totals)
visits_assignment <- visits_assignment %>% mutate(Unique_IP = round(unique_IP/n, digits = 2))
visits_assignment$assignment <- c("Facebook", "WhatsApp Individual", "WhatsApp Group", "Total")
visits_assignment <- visits_assignment[,c(6,3,1:2,4:5)]
visits_assignment$median_time <- median_time

colnames(visits_assignment) <- c("Treatment assignment", "Assigned","Unique users", "Unique IPs", "Total visits", "IP/Tot.", "Avgerage visit time")

addtorow <- list(
  pos = list(3), 
  command=c("\\hline \\hline "))

TableS13 <- print(xtable(visits_assignment[c(1,2,4,3,5,7)], 
                        digits = c(0,0,0,0,0,0,0), 
                        align = "clccccc",
                        label = "tab:websiteStats",
                        caption = "Unique Ips, users, visits, and average visit time by treatment assignment"),
                 table.placement = "H",
                 caption.placement = "top",
                 include.rownames = FALSE,
                 add.to.row =  addtorow,
                 comment = FALSE)

TableS13 <- resizebox(TableS13)
note <- c("Website data provides the number of unique IPs, unique users, and total visits by treatment assignment. A Unique User is determined via cookies and thus corresponds to a specific individual in a particular device. Note that this table reports different treatment assignment numbers than Table S$1$ as it includes assignments to individuals who responded twice to the endline survey, and thus were excluded from the study.")

TableS13 <- add_notes(TableS13, note)

# Table S14
# Total views per video
video_views <- c()
total_hours <- c()
avg_duration <- c()
for(i in 1:13){
  vid_num <- str_c("v",i,"[a-z]{2}.html")
  video_views[i] <- dim(c[str_detect(c$Landing.page, vid_num),])[1]
  total_hours[i] <- round(sum(c[str_detect(c$Landing.page, vid_num),"Total.Time"])/(60*60), digits = 2)
  avg_duration[i] <- mean(c[str_detect(c$Landing.page, vid_num),"Total.Time"])
}

duration <- round(lubridate::seconds_to_period(avg_duration), digits = 0)
duration <- paste(hour(duration), minute(duration), second(duration), sep = ":")

# Importing YouTube
L <- readLines("Datasets/ClickyAnalysis/youtube.tex")
DF <- read.table(text = L, sep = "&", skip = 1, check.names = FALSE, quote="\"")
colnames(DF) <- c("Title", "Date", "YouTube Views", "YouTube Duration", "Avg. View")
DF$`Avg. View` <- gsub("\\\\", "", DF$`Avg. View`)

duration <- sub(':', ':0', duration)
DF <- as.data.frame(cbind(DF[,1], video_views, duration, DF[,c(3,5)]))
colnames(DF) <- c("Video", "Visits", "Average visit time","Views", "Average viewing time")

DF[14,] <- c("Total", sum(DF$`Visits`),"0:04:22", round(sum(DF$`Views`), digits = 2), "0:02:59" )


addtorow <- list(
  pos = list(-1, 0, 13, 14), 
  command = c( "\\hline \\multicolumn{1}{l}{} & \\multicolumn{2}{c}{Website} & \\multicolumn{2}{c}{YouTube} \\\\ \\cmidrule(l{2pt}r{2pt}){2-3} \\cmidrule(l{2pt}r{2pt}){4-5} ",
               "\\hline ",
               "\\hline \\hline ",
               "\\hline "))

tableS14 <- xtable(DF, 
                  align = "clcccc",
                  label = "tab:youtube",
                  caption = "Website and YouTube analytics")

TableS14 <- print(tableS14, 
                 table.placement = "H",
                 caption.placement = "top",
                 include.rownames = FALSE,
                 comment = FALSE, 
                 sanitize.colnames.function=function(x){x},
                 add.to.row =  addtorow,
                 hline.after = NULL)

TableS14 <- resizebox(TableS14)
note <- c("Website and YouTube analytics show that videos received a higher number of website visits and viewing time than YouTube views. The reason is that and the website measures total duration on the site, whereas YouTube measures time spent viewing the content and is much stricter in defining whether a video was viewed.")
TableS14 <- add_notes(TableS14, note)
TableS14 <- gsub("lcccc", "p{6cm}p{1.5cm}p{2cm}p{1.5cm}p{2cm}", TableS14)


# Table S12
groups_data <- readxl::read_excel("Datasets/Message tracking/message_tracking_no_pii.xlsx", 
                                  sheet = "Groups")

colnames(groups_data) <- c("ResponseId", "line.x", "assignment.x", "group_id", "group_name", "conversation_in_group",
                           "phone_left_group", "blank", "blank_2", "not_receiving_survey", "blank_3", "blank_4")

groups_data <- groups_data[-1,]

# basic cleaning
groups_data$conversation_in_group <- str_replace(groups_data$conversation_in_group, 
                                                 "active conversation before videos",
                                                 "active conversation")
groups_data$conversation_in_group <- str_replace(groups_data$conversation_in_group, 
                                                 " \\(5/8\\)",
                                                 "")
groups_data$conversation_in_group <- str_replace(groups_data$conversation_in_group, 
                                                 "no conversation / limited",
                                                 "limited conversation")

# data manipulation
groups_data_coded <- groups_data %>% filter(!is.na(conversation_in_group)) %>%
  dplyr::select(line.x, assignment.x, group_id, group_name, conversation_in_group) %>% 
  mutate(conversation_in_group = str_to_lower(conversation_in_group))

# getting unique observations
conversations_total <- groups_data_coded %>% group_by(group_name) %>% 
  summarize(researcher_observations = n(),
            conversations_per_observation = toString(unique(conversation_in_group), sep = " | ")) %>% 
  # arrange(desc(str_length(conversations_total))) %>% 
  arrange(desc(researcher_observations)) %>% 
  # mess way to replace this single "limited"
  mutate(conversations_per_observation = str_replace(conversations_per_observation, "limited, no conversation",
                                                     "limited conversation, no conversation"))

convo_total <- conversations_total %>% group_by(conversations_per_observation) %>% 
  summarize(count = n()) %>%
  # researcher_obs = mean(researcher_observations)) %>% 
  arrange(desc(count)) %>% adorn_totals("row")

convo_total$conversations_per_observation <- str_to_sentence(convo_total$conversations_per_observation)
colnames(convo_total) <- c("Level of Conversation", "Number of Groups")

convo_final <- cbind(convo_total[c(1,2,3,9),1], c(as.numeric(convo_total[1,2]),
                                                  as.numeric(convo_total[2,2] + convo_total[4,2] +  convo_total[5,2]),
                                                  as.numeric(convo_total[3,2] + convo_total[6,2] +  convo_total[7,2] + convo_total[8,2]), 
                                                  as.numeric(convo_total[9,2])))
description <- c("No one replying at all",
                 "Only one person replying with an elaborate feedback or one or more persons replying with a short feedback.",
                 "More than one person replying with an elaborate feedback or two members engaging in discussion",
                 "Two people getting into a heated argument or one or more persons attacking video content")

convo_final <- cbind.data.frame(convo_final, description)
colnames(convo_final) <- c("Level of conversation", "Number of groups", "Description")
convo_final <- rbind.data.frame(convo_final, c("Total", sum(as.numeric(convo_final$`Number of groups`)), ""))

addtorow <- list(
  pos = list(-1,0,4, 5), 
  command=c("\\hline ", "\\hline ", "\\hline \\hline ", "\\hline "))

TableS12 <- print(xtable(convo_final, 
                         caption = "Coding of conversations in WhatsApp groups", 
                         label = "table:group_conversations", 
                         align = "cp{5cm}p{2cm}p{8cm}"), 
                  table.placement = "H",
                  caption.placement = "top",
                  include.rownames = FALSE,
                  comment = FALSE,
                  add.to.row =  addtorow,
                  hline.after = NULL)

TableS12 <- resizebox(TableS12)

# Save tables 
cat(TableS3,"\n", file = 'Tables/S3.tex')
cat(TableS13,"\n", file = 'Tables/S13.tex')
cat(TableS14,"\n", file = 'Tables/S14.tex')
cat(TableS12,"\n", file = 'Tables/S12.tex')
