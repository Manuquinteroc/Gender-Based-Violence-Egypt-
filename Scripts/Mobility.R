# Mobility analysis plots 

# Read dataset
mobility <- read.csv('Datasets/Mobility/2020_EG_Region_Mobility_Report.csv', fileEncoding = "latin1") %>% dplyr::select(-c(1,3:8)) %>% 
  group_by(date, country_region) %>% summarise_all(mean, na.rm = T)

mobility$date_num <- 1:dim(mobility)[1]

# We define ggplot setup -------------------------------------------------------
legend.text.18 <- element_text(color = "black", size = 7, family = "Arial")
final.text.14 <- element_text(color = "black", size = 7, hjust = 0.5, family = "Arial")
legend.text.5 <- element_text(color = "black", size = 5, family = "Arial")



# Define colors for all graphs
colors <- c("#0072B2","#D55E00")
th_egypt <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.title.x = final.text.14,
                  axis.title.y = final.text.14,
                  axis.text.x = legend.text.5, 
                  axis.text.y = legend.text.5,
                  panel.border = element_blank())

#  -----------------------------------------------------------------------------
names <- names(mobility)[3:8]

maxs <- sapply(mobility[names], function(x) max(x) )  
mins <- sapply(mobility[names], function(x) min(x) )  

# Create a plot for each variable (From July 10, 2020 through September 5, 2020.)

ggplot(mobility, aes(date, retail_and_recreation_percent_change_from_baseline)) +
  geom_point(color = '#0072B2') +
  th_egypt +
  ylab('Percent change from baseline') +
  geom_vline(xintercept = c('2020-07-10', '2020-09-05'), color = '#D55E00') + # 147 and 204
  scale_x_discrete(breaks = c('2020-02-15', '2020-07-10', '2020-09-05', '2020-12-31'),
                   expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = final.text.14, axis.text.y = final.text.14) +
  ylim((mins[1]), (maxs[1] + 5))


# Create and store the plots
list_of_plots <- list()

# X titles
xlabs <- c(xlab(expression(paste(bold("(A)"), " Retail and recreation"))),
           xlab(expression(paste(bold("(B)"), " Grocery and pharmacy"))),
           xlab(expression(paste(bold("(C)"), " Parks"))),
           xlab(expression(paste(bold("(D)"), " Transit stations"))),
           xlab(expression(paste(bold("(E)"), " Workplaces"))),
           xlab(expression(paste(bold("(F)"), " Residential"))) )
  


for (i in 1:6) {
  
  name <- paste("plot_", i, " <-", sep = "")
  
  if (i %in% c(1,3,5)) {
    content <- paste0("ggplot(mobility, aes(date,", names[i], ")) +", 
                      "geom_point(color = '#0072B2', size = 0.75) +
                      th_egypt +
                      ylab('Percent change from baseline') +
                      xlab(", xlabs[i], ") +
                      geom_vline(xintercept = c('2020-07-10', '2020-09-05'), color = '#D55E00') + 
                      scale_x_discrete(breaks = c('2020-02-15', '2020-07-10', '2020-09-05', '2020-12-31'),
                      expand = expansion(mult = c(0, 0.1))) +
                      ylim(", (mins[i]), ",", (maxs[i] + 5), ")")
    } else {
      content <- paste0("ggplot(mobility, aes(date,", names[i], ")) +", 
      "geom_point(color = '#0072B2', size = 0.75) +
      th_egypt +
      theme(axis.title.y = element_blank()) + 
      xlab(", xlabs[i], ") +
      geom_vline(xintercept = c('2020-07-10', '2020-09-05'), color = '#D55E00') + 
      scale_x_discrete(breaks = c('2020-02-15', '2020-07-10', '2020-09-05', '2020-12-31'),
      expand = expansion(mult = c(0, 0.1))) +
      ylim(", (mins[i]), ",", (maxs[i] + 5), ")")
    }               

  eval(parse(text = paste0(name, content)))
  
  nam = substr(name, 1, nchar(name)-3)
  
  list_of_plots[[i]] <- get(nam, envir = globalenv())
  
}

# Save all plots created
save_names <- paste0("mobility_", 1:6, ".pdf")

for (i in 1:6) {
  ggsave(get(paste0("plot_", i), envir = globalenv()) , path = 'Figures/Mobility', filename = save_names[i], device = cairo_pdf, 
         width = 7, height = 5, dpi = 300)
}


# Create a single grid plot
# Grid arrange for a single plot
grid_1 <- grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, nrow = 3)

ggsave(grid_1, path = folder, filename = "Extended_Data_Fig3.jpg",
       width = 180, height = 210, unit = "mm", dpi = 350)
