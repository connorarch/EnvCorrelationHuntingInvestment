library(ggrepel)
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(stats)
library(rcarbon)

e <- exp(1)

#SU 1000-1300
#TB 725-1200
#UM 1000-1300
#PP 1150-1400
#MEY 900-1300
#CC 700-1220

#Faunal and Artifact Data
#-----------------
#import faunal data for tsq
faunal<-read.csv(file="faunal.csv")

#import data for tsq
artifacts<-read.csv(file="artifacts.csv")

#remove irrelevant columns
faunaljoin<- faunal %>%
  select(1,7,8,9) %>%
  filter(Artiodactyls!=0)
artifacts<- artifacts %>%
  select(1:9) %>%
  filter(Points!=0)

#rename Site.name
artifacts<-artifacts%>%
  rename(Site.name=Site.Name)
artifacts$District<-factor(artifacts$District)
artifacts$Pecos<-factor(artifacts$Pecos)
artifacts$Area<-factor(artifacts$Area)
artifacts$site_type<-factor(artifacts$site_type)
artifacts$Points<-as.integer(artifacts$Points)
artifacts$Sherds<-as.integer(artifacts$Sherds)


#left join faunal data to main data
sitedata<-left_join(artifacts,faunaljoin, by="Site.name")

#complete sitedata
sitedatacomp <- sitedata %>%
  filter(is.na(Artiodactyls)==F)

#no mv
sitedatacompnomv <- sitedatacomp%>%
  filter(District!="Mesa Verde")

#create columns for proportions with projectiles and grayware
sitedatacompnomv <- sitedatacompnomv %>%
  mutate(projctprop=Points/(Sherds+Points)) %>%
  mutate(artioctprop=Artiodactyls/(Sherds+Artiodactyls)) %>%
  mutate(projctrat=Points/(Sherds)) %>%
  mutate(artioctrat=Artiodactyls/(Sherds)) %>%
  mutate(ai=Artiodactyls/(Lagomorphs+Artiodactyls)) %>%
  mutate(li=Lagomorphs/(Lagomorphs+Artiodactyls)) %>%
  mutate(compindex=artioctprop/projctprop)%>%
  mutate(lagctprop=Lagomorphs/Lagomorphs+Sherds)%>%
  filter(Sherds>=0)

#filter for over 1000 sherds and >0 lagomorph NISP
sitedatafilt<- sitedatacompnomv %>%
  filter(Sherds>1000)

#everything runs on sitedata
sitedata <- sitedatafilt

#make columns for log transformed proportions
sitedata<-sitedata %>%
  mutate(artioctpropl = log(artioctprop)) %>%
  mutate(projctpropl = log(projctprop))

#characters in date range to numerical range
sitedata <- sitedata %>%
  separate(Date, into = c("start", "end"), sep = "-", remove = FALSE) %>%
  mutate(start = as.numeric(start),
         end = as.numeric(end))


#import ppt data by region
pp.ppt<-read.csv(file="PP.1150.1400.csv")
tb.ppt<-read.csv(file="TB.725.1200.csv")
mey.ppt<-read.csv(file="MEY.900.1300.csv")
su.ppt<-read.csv(file="SU.1000.1300.csv")
um.ppt<-read.csv(file="UM.1000.1300.csv")
cc.ppt<-read.csv(file="CC.700.1220.csv")

#region from character to factor
pp.ppt$region <- factor(pp.ppt$region)
tb.ppt$region <- factor(tb.ppt$region)
mey.ppt$region <- factor(mey.ppt$region)
su.ppt$region <- factor(su.ppt$region)
um.ppt$region <- factor(um.ppt$region)
cc.ppt$region <- factor(cc.ppt$region)

#combine precipitation data
precip_all <- bind_rows(
  pp.ppt, tb.ppt, mey.ppt, su.ppt, um.ppt, cc.ppt
)

#import mfn data by region
pp.mfn<-read.csv(file="pp.mfn.csv")
tb.mfn<-read.csv(file="tb.mfn.csv")
mey.mfn<-read.csv(file="mey.mfn.csv")
su.mfn<-read.csv(file="su.mfn.csv")
um.mfn<-read.csv(file="um.mfn.csv")
cc.mfn<-read.csv(file="CC.mfn.csv")

#region from character to factor
pp.mfn$region <- factor(pp.mfn$region)
tb.mfn$region <- factor(tb.mfn$region)
mey.mfn$region <- factor(mey.mfn$region)
su.mfn$region <- factor(su.mfn$region)
um.mfn$region <- factor(um.mfn$region)
cc.mfn$region <- factor(cc.mfn$region)

#combine precipitation data
mfn_all <- bind_rows(
  pp.mfn, tb.mfn, mey.mfn, su.mfn, um.mfn, cc.ppt
)

# Create a mapping between District codes and region names
district_to_region <- c(
  "Pajarito Plateau" = "PP",
  "Ute Mountain" = "UM",
  "Southern Utah" = "SU",
  "McElmo-Yellowjacket" = "MEY",
  "Tewa Basin" = "TB",
  "Chaco" = "CC"
)


#Nonfunctional code for extracting mfn and precip data-----

# Define regions and their corresponding file patterns
regions <- c("PP", "TB", "MEY", "SU", "UM", "CC")

# Function to read and process files for a given data type
read_and_process <- function(regions, file_pattern) {
  # Read all files
  data_list <- lapply(regions, function(region) {
    filename <- sprintf(file_pattern, region)
    df <- read.csv(file = filename)
    df$region <- factor(df$region)
    return(df)
  })
  
  # Combine all data
  combined_data <- bind_rows(data_list)
  return(combined_data)
}

# Import and combine precipitation data
ppt_files <- c("PP.1150.1400.csv", "TB.725.1200.csv", "MEY.900.1300.csv", 
               "SU.1000.1300.csv", "UM.1000.1300.csv", "CC.700.1220.csv")
precip_all <- lapply(ppt_files, function(f) {
  df <- read.csv(file = f)
  df$region <- factor(df$region)
  return(df)
}) %>% bind_rows()

# Import and combine mfn data (assuming consistent naming pattern)
mfn_files <- paste0(tolower(regions), ".mfn.csv")
mfn_all <- lapply(mfn_files, function(f) {
  df <- read.csv(file = f)
  df$region <- factor(df$region)
  return(df)
}) %>% bind_rows()



#

#----
#ACTUAL DYNAMIC PLOTS
#-----

# Add a column with the full region name
sitedata$region <- factor(district_to_region[as.character(sitedata$District)])

#function which adds a row in sitedata for the mean precip for each site's date range

sitedata$mean_ppt <- mapply(function(start, end, reg) {
  precip_all %>%                           # <-- This references the precip dataframe
    filter(region == reg, year >= start, year <= end) %>%
    summarise(mean_ppt = mean(ppt, na.rm = TRUE)) %>%
    pull(mean_ppt)
}, sitedata$start, sitedata$end, sitedata$region)

#function which adds a row in sitedata for the mean mfn for each site's date range

sitedata$mean_mfn <- mapply(function(start, end, reg) {
  mfn_all %>%                           # <-- This references the mfn dataframe
    filter(region == reg, year >= start, year <= end) %>%
    summarise(mean_mfn = mean(mfn, na.rm = TRUE)) %>%
    pull(mean_mfn)
}, sitedata$start, sitedata$end, sitedata$region)


plot_flexible_returns <- function(plot_title, 
                                  x_var_name, 
                                  y_var_name, 
                                  dataset, 
                                  x_label_text, 
                                  y_label_text,
                                  log_x = FALSE,
                                  log_y = FALSE,
                                  show_centroids = TRUE,
                                  show_regression = TRUE
) {
  
  # --- 1. Prepare Data and Aesthetics based on Log Parameters ---
  
  # Build aesthetic mappings dynamically
  if (log_x) {
    x_aes_string <- paste0("log(", x_var_name, ")")
    mean_x_val <- mean(log(dataset[[x_var_name]]), na.rm = TRUE)
  } else {
    x_aes_string <- x_var_name
    mean_x_val <- mean(dataset[[x_var_name]], na.rm = TRUE)
  }
  
  if (log_y) {
    y_aes_string <- paste0("log(", y_var_name, ")")
    mean_y_val <- mean(log(dataset[[y_var_name]]), na.rm = TRUE)
  } else {
    y_aes_string <- y_var_name
    mean_y_val <- mean(dataset[[y_var_name]], na.rm = TRUE)
  }
  
  # Wrap the title text
  wrapped_title <- str_wrap(plot_title, 65)
  
  # --- 2. Calculate Centroids ---
  
  # Create formula dynamically based on variable names
  centroid_formula <- as.formula(paste0("cbind(", x_var_name, ",", y_var_name, ") ~ District"))
  centroids <- aggregate(centroid_formula, dataset, mean)
  
  # Apply log transformation to centroids if needed
  if (log_x) {
    centroids[[x_var_name]] <- log(centroids[[x_var_name]])
  }
  if (log_y) {
    centroids[[y_var_name]] <- log(centroids[[y_var_name]])
  }
  
  # --- 3. Build the Plot ---
  
  fig <- ggplot(dataset, aes_string(y=y_aes_string, x=x_aes_string)) +
    
    # Add vertical and horizontal mean lines
    geom_vline(xintercept=mean_x_val, color='black', linetype="dashed", linewidth=1) +
    geom_hline(yintercept=mean_y_val, color='black', linetype="dashed", linewidth=1) +
    
    # Add scatter points
    geom_point(aes(shape=District), size=4) +
    scale_shape_manual(values=c(3, 17, 7, 16, 13, 5))
  
  # Add centroids if requested
  if (show_centroids) {
    fig <- fig + geom_point(data=centroids, 
                            aes_string(x=x_var_name, y=y_var_name, shape="District"), 
                            size = 9, 
                            show.legend = FALSE)
  }
  
  # Add linear regression smooth line if requested
  if (show_regression) {
    fig <- fig + geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black', level=.9, formula = 'y ~ x') +
      stat_regline_equation(family = "serif", size = 6,
                            label.y.npc = "top", label.x.npc = "left", aes(group = 1, label = ..eq.label..), show.legend = FALSE,
                            vjust = 1.5, hjust = 0) +
      stat_regline_equation(family = "serif", size = 6,
                            label.y.npc = "top", label.x.npc = "left", aes(group=1,label = ..rr.label..), show.legend = FALSE,
                            vjust = 3.0, hjust = 0)
  }
  
  # Apply themes and labels
  fig <- fig +
    theme(plot.title=element_text(hjust=.15)) +
    ggtitle(wrapped_title) +
    theme(text=element_text(size=20, face="bold", family="serif")) +
    ylab(y_label_text) +
    xlab(x_label_text) +
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          panel.background = element_rect(color = "black"))
  
  return(fig)
}

# Call the function for ppt, yes reg
my_plot <- plot_flexible_returns(
  plot_title = "Hunting Investment for Mean Water-Year Precipitation",
  x_var_name = "mean_ppt",
  y_var_name = "projctrat", # Function handles the log transformation internally
  dataset = sitedata,
  x_label_text = "Mean Water-Year Precipitation (mm)",
  y_label_text = "ln(points/grayware)",
  log_x = FALSE,  # New parameter: set to TRUE to log-transform x
  log_y = TRUE,
  show_centroids = TRUE,
  show_regression = TRUE
)

# Display the plot fo mfn, no reg
print(my_plot)

# Call the function
my_plot <- plot_flexible_returns(
  plot_title = "Hunting Investment for Mean % in Maize Niche",
  x_var_name = "mean_mfn",
  y_var_name = "projctprop", # Function handles the log transformation internally
  dataset = sitedata,
  x_label_text = "Mean % in Maize Niche",
  y_label_text = "ln(points/points + grayware)",
  log_x = FALSE,  # New parameter: set to TRUE to log-transform x
  log_y = TRUE,
  show_centroids = TRUE,
  show_regression = FALSE
)

# Display the plot
print(my_plot)

# Call the function
my_plot <- plot_flexible_returns(
  plot_title = "Artiodactyl Index for Relative Population Density",
  x_var_name = "Site_mean_dens",
  y_var_name = "artioctrat", # Function handles the log transformation internally
  dataset = sitedata_with_ckde,
  x_label_text = "Relative Population Density",
  y_label_text = "Artiodactyl Index",
  log_x = FALSE,  # New parameter: set to TRUE to log-transform x
  log_y = TRUE,
  show_centroids = TRUE,
  show_regression = TRUE
)+
  scale_x_continuous(labels = scales::label_number(accuracy = 0.000001))

# Display the plot
print(my_plot)


#break
###Manual figures---------

#make centroid df for instance, artio for prop
centroids <- aggregate(cbind(mean_ppt,artioctpropl)~District,sitedata,mean)


fig1text <- str_wrap("Returns on Hunting Investment for Mean Precipitation", 65)
fig1 <- ggplot(sitedata, aes(y=log(artioctprop),x=mean_ppt))
fig1 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(log(artioctprop))), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2.5, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.15))+
  ggtitle(fig1text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(artio NISP/artio NISP + grayware)")+
  xlab("mean precipitation")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))+
  geom_point(data=centroids, aes(mean_ppt, artioctpropl), size = 9, shape=c(3, 17, 7, 16, 13))


fig8text <- str_wrap("Log(Artiodactyls) for Mean Precipitation", 65)
fig8 <- ggplot(sitedata, aes(y=log(Artiodactyls),x=mean_ppt))
fig8 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(log(Artiodactyls))), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 0, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -0.5, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.9))+
  ggtitle(fig8text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(Artiodactyls)")+
  xlab("mean precipitation")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))

fig9text <- str_wrap("Log(Points) for Mean Precipitation", 65)
fig9 <- ggplot(sitedata, aes(y=log(Points),x=mean_ppt))
fig9 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(log(Points))), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2.5, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.15))+
  ggtitle(fig9text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(Points)")+
  xlab("mean precipitation")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))

fig10text <- str_wrap("Log(Points/Points+Grayware) for Mean Precipitation", 65)
fig10 <- ggplot(sitedata, aes(y=log(projctprop),x=mean_ppt))
fig10 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(log(projctprop))), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2.5, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.15))+
  ggtitle(fig10text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(Points/Points+Grayware)")+
  xlab("mean precipitation")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))

fig11text <- str_wrap("Log(Lagomorphs) for Mean Precipitation", 65)
fig11 <- ggplot(sitedata, aes(y=log(Lagomorphs),x=mean_ppt))
fig11 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(log(Lagomorphs))), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2.5, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.15))+
  ggtitle(fig11text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(Lagomorphs)")+
  xlab("mean precipitation")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))

fig12text <- str_wrap("Log(Sherds) for Mean Precipitation", 65)
fig12 <- ggplot(sitedata, aes(y=log(Sherds),x=mean_ppt))
fig12 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(log(Sherds))), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 9, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 9.5, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.15))+
  ggtitle(fig12text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(Sherds)")+
  xlab("mean precipitation")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))

fig13text <- str_wrap("AI for Mean Precipitation", 65)
fig13 <- ggplot(sitedata, aes(y=ai,x=mean_ppt))
fig13 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(ai)), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 0, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -.2, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.15))+
  ggtitle(fig13text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ai")+
  xlab("mean precipitation")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))

fig14text <- str_wrap("Artiodactyls for Mean Precipitation", 65)
fig14 <- ggplot(sitedata, aes(y=Artiodactyls,x=mean_ppt))
fig14 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(Artiodactyls)), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 700, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 800, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.9))+
  ggtitle(fig14text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("Artiodactyls")+
  xlab("mean precipitation")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))


#linear models for figs 3 and 4
proj.s.lm = lm(log(Points) ~ log(Sherds),data=sitedata)
artio.s.lm = lm(log(Artiodactyls) ~ log(Sherds),data=sitedata)
summary(proj.s.lm)
summary(artio.s.lm)

artio.ppt.lm = lm(log(Artiodactyls) ~ mean_ppt,data=sitedata)
summary(artio.ppt.lm)

projctprop.ppt.lm = lm(log(projctprop) ~ mean_ppt,data=sitedata)
summary(projctprop.ppt.lm)

#PLOTS!

#FIGURE 3 site data artio for sherds
fig3text <- str_wrap("Large Game Accumulation for Grayware Sherd Accumulation", 60)
fig3 <- ggplot(sitedata, aes(y=log(Artiodactyls), x=log(Sherds)))
fig3 +
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 7.5, label.x = 7, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 7, label.x = 7, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=0))+
  ggtitle(fig3text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(artiodactyl NISP)")+
  xlab("ln(grayware)")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))


#FIGURE 4 site data proj for sherds
fig4text <- str_wrap("Projectile Point Accumulation for Grayware Sherd Accumulation", 80)
fig4 <- ggplot(sitedata, aes(y=log(Points), x=log(Sherds)))
fig4 + 
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 5.5, label.x = 7, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = 5.2, label.x = 7, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=0.15))+
  ggtitle(fig4text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(projectile points)")+
  xlab("ln(grayware)")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))

#make centroid df for instance, artio for prop
centroids <- aggregate(cbind(projctpropl,artioctpropl)~District,sitedata,mean)

#FIGURE 5 site data artio for proj
fig5text <- str_wrap("Relative Ancestral Puebloan Hunting Investment Across Districts", 65)
fig5 <- ggplot(sitedata, aes(log(projctprop),log(artioctprop)))
fig5 +
  geom_vline(aes(xintercept=mean(log(projctprop))), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(log(artioctprop))), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2, label.x = -7.3, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2.5, label.x = -7.3, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.15))+
  ggtitle(fig5text)+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("ln(artio NISP/artio NISP + grayware)")+
  xlab("ln(projectiles/projectiles + grayware)")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))+
  geom_point(data=centroids, aes(projctpropl, artioctpropl), size = 9, shape=c(3, 17, 7, 16, 13))

#linear model of hunting returns for investment
ap.lm <- lm(log(artioctprop)~log(projctprop),sitedata)
summary(ap.lm)

#create column for residuals to linear model of investment and return for sites
sitedata$residuals <- ap.lm$residuals

#grouping by high and low hunting investment
highhunting<-sitedata %>%
  filter(District %in% c('Tewa Basin'))
lowhunting<-sitedata %>%
  filter(District %in% c('Pajarito Plateau', 'Ute Mountain', 'McElmo-YellowJacket'))

#comparing residuals to fit line for high and low hunting groups
ttest <- t.test(highhunting$residuals, lowhunting$residuals, var.equal = T)
ttest

#FIGURE 6- qqplot of residuals for linear model
qqtitle <- str_wrap("QQ Plot for Residuals of Linear Model", 80)
ggplot(sitedata, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.45))+
  ggtitle(qqtitle)+
  theme(plot.title = element_text(hjust = .35))+
  theme(text=element_text(size=20, face="bold",  family="serif"))+
  ylab("Theoretical Quantiles")+
  xlab("Sample Quantiles")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=16, angle=0),
        panel.background = element_rect(color = "black"))

lmtest::bptest(ap.lm)

#FIGURE 7 boxplot of residuals by district for linear model
t.resibp<-str_wrap("Log-scale Residuals of Linear Model for Study Region Sites by District", 75)
resibp<-ggplot(sitedata, aes(x=District,y=residuals, fill=District))
resibp+geom_boxplot(size=1)+
  scale_fill_grey(start=.3, end=1)+
  theme(plot.title=element_text(hjust=0.15))+
  ggtitle(t.resibp)+
  theme(text=element_text(size=16, face="bold",  family="serif"))+
  ylab("Residuals")+
  xlab("District")+
  theme(axis.text.x = element_text(face="bold", color="#000000", 
                                   size=13, angle=0),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=13, angle=0),
        panel.background = element_rect(color = "black"),
        panel.grid.major = element_line(color = "black", size = .25),
        panel.grid.minor = element_line(color = "black", size = .1, linetype = "dashed"))


#Radiocarbon

#Iteration 6
#-------





df1 <- read.csv("rrc.csv")

#filter out error >100
df1 <- df1 |>
  filter(SD < 100)
# 1. Map counties to regions
# San Juan is split by state: San Juan, CO = SJU; San Juan, NM = CHC
df1$Region <- dplyr::case_when(
  df1$County == "Montezuma, CO"  ~ "CMV",
  df1$County == "San Juan, UT"   ~ "SJU",
  df1$County == "San Juan, NM"   ~ "CHC",
  df1$County == "Los Alamos, NM" ~ "NRG",
  df1$County == "Santa Fe, NM"   ~ "NRG",
  df1$County == "Sandoval, NM"   ~ "NRG",
  df1$County == "Rio Arriba, NM" ~ "NRG",
  df1$County == "Taos, NM"       ~ "NRG",
  TRUE ~ NA_character_  # counties not in your study regions
)
df1 <- df1 %>% mutate(Site_Number = as.character(Site_Number))


# Check how many dates fall into each region vs excluded
print(table(df1$Region, useNA = "always"))

# Keep only dates assigned to a region
df_study <- df1[!is.na(df1$Region), ]

# filter to only dates within your window
df_study <- df_study %>%
  filter(Date_BP >= 550, Date_BP <= 1450)

# Calibrate
calibrated <- calibrate(
  x         = df_study$Date_BP,
  errors    = df_study$SD,
  calCurves = 'intcal20',
  normalised = F, 
  verbose = F
)

#thin to 20 dates per site
c14_thin <- calibrated[thinDates(
  ages = df_study$Date_BP, 
  errors = df_study$SD, 
  bins = df_study$Site_ID, 
  size = 20, method = 'random'
  )] 

set.seed(3491)

# 1,000 unique KDEs to account for uncertainty
all.randates <- sampleDates(c14_thin, nsim=1000, verbose=FALSE)

# 25 year smooth, date range 0 - 1350 CE
sw_ckde <- ckde(all.randates, timeRange=c(1450, 550), bw=25)

# C14 side 
c14_density <- sw_ckde$res.matrix


# Build SPD per region
dendro <- read.csv("dendro_data.csv")

dendro <- dendro %>%
  mutate(yearCE = 1950-Date_BP) %>%
  filter(yearCE >= 500, yearCE <= 1400) %>%
  mutate(
    Region = dplyr::case_when(
      County == "Montezuma, CO"  ~ "CMV",
      County == "San Juan, UT"   ~ "SJU",
      County == "San Juan, NM"   ~ "CHC",
      County == "Los Alamos, NM" ~ "NRG",
      County == "Santa Fe, NM"   ~ "NRG",
      County == "Sandoval, NM"   ~ "NRG",
      County == "Rio Arriba, NM" ~ "NRG",
      County == "Taos, NM"       ~ "NRG",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region))

# Dendro side - all study regions combined
set.seed(3491)

tr_simulation <- function(
    x,
    bw = 25,
    timeRange = c(500, 1400)
){
  # thin tree ring samples in each iteration
  # ie, we group the table by site (Trinomial),
  # take three samples from each group/site,
  # and then we pull the yearCE variable out of the table and into its own object
  # in this case we just replace the table x with the variable x
  samples <- x |> group_by(Site_Number) |> slice_sample(n = 3) |> pull(yearCE)
  # number of evenly spaced grid points within timeRange
  # determines where to calculate the density
  # need this to ensure that the tree ring densities line up with the c14 densities
  npts <- timeRange[2] - timeRange[1] + 1
  # now calculate density of all samples
  d <- density(
    samples,
    bw = bw,
    n = npts,
    from = timeRange[1],
    to = timeRange[2]
  )
  # return just the density estimates
  d$y
}

# dendro now filtered to study counties
tr_density <- replicate(1000, tr_simulation(dendro))

sw_density <- c14_density + tr_density

colnames(c14_density) <- 1:1000
c14_profile <- c14_density |>
  as_tibble() |>
  mutate(
    type = "Radiocarbon",
    year = 500:1400
  ) |>
  pivot_longer(
    c(-type, -year),
    names_to = "simulation",
    values_to = "density"
  )

colnames(tr_density) <- 1:1000
tr_profile <- tr_density |>
  as_tibble() |>
  mutate(
    type = "Tree ring",
    year = 500:1400
  ) |>
  pivot_longer(
    c(-type, -year),
    names_to = "simulation",
    values_to = "density"
  )

colnames(sw_density) <- 1:1000
sw_profile <- sw_density |>
  as_tibble() |>
  mutate(
    type = "Combined",
    year = 500:1400
  ) |>
  pivot_longer(
    c(-type, -year),
    names_to = "simulation",
    values_to = "density"
  )


all_density <- bind_rows(
  c14_profile,
  tr_profile,
  sw_profile
)


mean_trend <- all_density |>
  group_by(type, year) |>
  summarize(density = mean(density), .groups = "drop")


mean_trend_tbl <- summary(mean_trend)





ggplot() +
  geom_line(
    data = all_density,
    aes(year, density, group = simulation),
    color = alpha("gray80", 0.2),
    linewidth = 0.2
  ) +
  geom_line(
    data = mean_trend,
    aes(year, density),
    color = "darkred",
    linewidth = 0.8
  ) +
  facet_wrap(vars(type), nrow = 5) +
  scale_x_continuous(
    breaks = seq(500, 1400, by = 150),
    expand = expansion(0.02)
  ) +
  labs(
    x = "Year (CE)",
    y = "Population Estimate (CKDE)"
  ) +
  geom_text(
    data = tibble(
      type = unique(all_density$type),
      x = 500,
      y = max(all_density$density) |> round(3)
    ),
    aes(x, y, label = type),
    nudge_y = -0.0007,
    hjust = 0,
    vjust = 1,
    size = 12/.pt,
    fontface = "bold"
  ) +
  theme_bw(12) +
  theme(
    axis.title = element_text(size = rel(1)),
    panel.grid = element_blank(),
    strip.text = element_blank()
  )




fig1 <- ggplot() +
  geom_line(
    data = all_density,
    aes(year, density, group = simulation),
    color = alpha("gray80", 0.2), linewidth = 0.2
  ) +
  geom_line(
    data = mean_trend,
    aes(year, density),
    color = "darkred", linewidth = 0.8
  ) +
  facet_wrap(vars(type), nrow = 3) +
  scale_x_continuous(breaks = seq(725, 1400, by = 150), expand = expansion(0.02)) +
  labs(x = "Year (AD)", y = "Population Estimate (CKDE)") +
  theme_bw(12) +
  theme(panel.grid = element_blank())

print(fig1)

n_sites_by_region <- df_study %>%
  group_by(Region) %>%
  summarise(n_sites = n_distinct(Site_Number)) %>%
  deframe()  # turns it into a named vector

regions <- unique(df_study$Region)
spd_list        <- list()
normalized_spds <- list()


#----
#FIG 2!!!!!!!!!!!!!!!!!!!


regions <- unique(df_study$Region)

region_density <- lapply(regions, function(r) {
  
  # --- C14 for this region ---
  idx <- which(df_study$Region == r)
  cal_r <- calibrated[idx]
  
  thin_idx <- thinDates(
    ages   = df_study$Date_BP[idx],
    errors = df_study$SD[idx],
    bins   = df_study$Site_ID[idx],
    size   = 20,
    method = "random"
  )
  cal_r_thin <- cal_r[thin_idx]
  
  set.seed(3496)
  randates_r <- sampleDates(cal_r_thin, nsim = 1000, verbose = FALSE)
  ckde_r     <- ckde(randates_r, timeRange = c(1450, 550), bw = 25)
  c14_mat    <- ckde_r$res.matrix
  
  # --- Dendro for this region ---
  dendro_r <- dendro |> filter(Region == r)
  
  if (nrow(dendro_r) > 0) {
    set.seed(3496)
    tr_mat <- replicate(1000, tr_simulation(dendro_r))
<<<<<<< HEAD
    
=======
>>>>>>> ed16f3c567de710fd765a1a5f0a4d809dafa628a
    combined_mat <- c14_mat + tr_mat
  } else {
    combined_mat <- c14_mat  # no dendro for this region, use C14 only
    message("No dendro dates for region: ", r)
  }
  
<<<<<<< HEAD
  #count number of distinct sites
  #n_sites <- n_distinct(df_study$Site_Number[df_study$Region == r]) +
   # n_distinct(dendro$Site_Number[dendro$Region == r])
  
  #combined_mat <- combined_mat / n_sites
  
=======
>>>>>>> ed16f3c567de710fd765a1a5f0a4d809dafa628a
  colnames(combined_mat) <- 1:1000
  combined_mat |>
    as_tibble() |>
    mutate(Region = r, year = 500:1400) |>
    pivot_longer(c(-Region, -year), names_to = "simulation", values_to = "density")
})

region_all <- bind_rows(region_density)

region_mean <- region_all |>
  group_by(Region, year) |>
  summarize(density = mean(density), .groups = "drop")

fig2 <- ggplot() +
  geom_line(
    data = region_all,
    aes(year, density, group = simulation),
    color = alpha("gray80", 0.2), linewidth = 0.2
  ) +
  geom_line(
    data = region_mean,
    aes(year, density),
    color = "darkred", linewidth = 0.8
  ) +
  facet_wrap(vars(Region), nrow = 2) +
  scale_x_continuous(breaks = seq(500, 1400, by = 150), expand = expansion(0.02)) +
  labs(x = "Year (AD)", y = "Population Estimate (CKDE)") +
  theme_bw(12) +
  theme(panel.grid = element_blank())

print(fig2)








#Plot all regions

# Convert your normalized SPD list into a tidy data frame
spd_df <- map2_df(
  names(normalized_spds),
  normalized_spds,
  ~ tibble(
    Region = .x,
    Date_BP = .y$grid$calBP,
    Density = .y$grid$PrDens
  )
) %>%
  mutate(Year_AD = 1950 - Date_BP)   # <-- Convert BP to AD

# Your color palette
region_colors <- c(
  "CMV" = "blue",
  "SJU" = "red",
  "CHC" = "darkgreen",
  "NRG" = "orange"
)

# Plot in BP
ggplot(spd_df, aes(x = Date_BP, y = Density, color = Region)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = region_colors) +
  scale_x_reverse() +   # BP decreases to the right
  labs(
    title = "Area-Normalized SPD by Region",
    x = "Years cal BP",
    y = "Summed Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )


# Plot in AD
ggplot(spd_df, aes(x = Year_AD, y = Density, color = Region)) +
  geom_line(size = 1) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "Area-Normalized SPD by Region (Years AD)",
    x = "Calendar Year (AD)",
    y = "Summed Probability"
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.000001)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

# build lookup from regional combined densities
region_lookup <- region_all %>%
  group_by(Region, year) %>%
  summarize(density = mean(density), .groups = "drop")

# map sitedata regions to your new region codes
sitedata_with_ckde <- sitedata %>%
  mutate(
    Region_group = dplyr::case_when(
      region %in% c("UM", "MEY") ~ "CMV",
      region %in% c("TB", "PP")  ~ "NRG",
      region == "SU"             ~ "SJU",
      TRUE                       ~ NA_character_
    )
  )

# get mean density over each site's date range
sitedata_with_ckde <- sitedata_with_ckde %>%
  rowwise() %>%
  mutate(
    Site_mean_dens = {
      region_grid <- region_lookup %>% 
        filter(Region == Region_group)
      
      window <- region_grid %>%
        filter(year >= start, year <= end)
      
      mean(window$density, na.rm = TRUE)
    }
  ) %>%
  ungroup()

#SPD lookup
#----


spd_lookup <- map(normalized_spds, ~ as_tibble(.x$grid)) %>%
  imap(~ mutate(.x, Region = .y)) %>%
  bind_rows()

spd_lookup <- spd_lookup %>%
  mutate(calAD = 1950 - calBP)


sitedata_with_spd <- sitedata %>%
  mutate(
    Region_group = dplyr::case_when(
      region %in% c("UM", "MEY") ~ "CMV",   # Central Mesa Verde
      region %in% c("TB", "PP")  ~ "NRG",   # Northern Rio Grande
      region == "SU"             ~ "SJU",   # San Juan
      TRUE                       ~ NA_character_
    )
  )



sitedata_with_spd <- sitedata_with_spd %>%
  rowwise() %>%
  mutate(
    Site_Mean_SPD = {
      region_grid <- spd_lookup %>% filter(Region == Region_group)
      
      # restrict to the site's AD window
      window <- region_grid %>%
        filter(calAD >= start, calAD <= end)
      
      mean(window$PrDens, na.rm = TRUE)
    }
  ) %>%
  ungroup()
#-----

# 1. Extract the SJU SPD for the site's date range
sju_slice <- region_lookup %>%
  filter(Region == "SJU",
         year >= 1125, year <= 1175)

summarise(sju_slice,
          n_years = nrow(sju_slice),
          true_mean = mean(density),
          min_val = min(density),
          max_val = max(density))

sitedata_with_ckde %>%
  filter(Site.name == "Coombs Site") %>%
  left_join(spd_lookup, by = c("Region_group" = "region")) %>%
  filter(calAD >= start, calAD <= end) %>%
  count()


p1 <- ggplot(df_study_filtered, aes(x = 1950 - Date_BP)) +
  geom_histogram(bins = 50, fill = "gray60", color = "white") +
  labs(x = "Year CE", y = "Count", title = "Radiocarbon Dates") +
  theme_bw()

p1

p2 <- ggplot(dendro, aes(x = yearCE, group = Region)) +
  geom_histogram(bins = 50, fill = Region, color = "white") +
  labs(x = "Year CE", y = "Count", title = "Tree Ring Dates") +
  theme_bw()

p2

p1 <- ggplot(df_study_filtered, aes(x = 1950 - Date_BP, fill = Region)) +
  geom_histogram(bins = 50, color = "white") +
  facet_wrap(vars(Region), nrow = 2) +
  labs(x = "Year CE", y = "Count", title = "Radiocarbon Dates") +
  theme_bw() +
  theme(legend.position = "none", panel.grid = element_blank())

p2 <- ggplot(dendro, aes(x = yearCE, fill = Region)) +
  geom_histogram(bins = 50, color = "white") +
  facet_wrap(vars(Region), nrow = 2) +
  labs(x = "Year CE", y = "Count", title = "Tree Ring Dates") +
  theme_bw() +
  theme(legend.position = "none", panel.grid = element_blank())

p2

#-------
library(dplyr)
library(purrr)
library(tibble)

# 1. Build a lookup table of SPD grids by region
spd_lookup <- map(normalized_spds, ~ as_tibble(.x$grid)) %>%
  imap(~ mutate(.x, Region = .y)) %>%
  bind_rows()

calibrated <- calibrate(
  x = df_study$Date_BP,
  errors = df_study$SD,
  calCurves = rep("intcal20", nrow(df_study))
  
)

# 2. Compute mean SPD for each radiocarbon date and attach to df_study
df_study_with_spd <- map2_df(
  calibrated,
  seq_len(nrow(df_study)),
  function(cal, i) {
    
    region <- df_study$Region[i]
    
    # SPD grid for this region
    spd_grid <- spd_lookup %>% filter(Region == region)
    
    # Match calibrated years to SPD years
    merged <- tibble(
      calBP = cal$calBP,
      calPr = cal$PrDens
    ) %>%
      inner_join(spd_grid, by = c("calBP" = "calBP"))
    
    # Weighted mean SPD
    mean_spd <- sum(merged$calPr * merged$PrDens)
    
    # Return one row matching df_study
    df_study[i, ] %>%
      mutate(Mean_SPD = mean_spd)
  }
)

calibrated <- purrr::map2(
  df_study$Date_BP,
  df_study$SD,
  ~ calibrate(x = .x, errors = .y, calCurves = "intcal20")
)

df_study_with_spd <- map2_df(
  calibrated,
  seq_len(nrow(df_study)),
  function(cal, i) {
    
    region <- df_study$Region[i]
    
    # SPD grid for this region
    spd_grid <- spd_lookup %>% filter(Region == region)
    
    # Extract calibrated posterior from cal$grids[[1]]
    cal_df <- as_tibble(cal$grids[[1]]) %>%
      rename(calBP = calBP, calPr = PrDens)
    
    # Join posterior with SPD curve
    merged <- inner_join(cal_df, spd_grid, by = "calBP")
    
    # Weighted mean SPD
    mean_spd <- sum(merged$calPr * merged$PrDens)
    
    # Return row with Mean_SPD added
    df_study[i, ] %>%
      mutate(Mean_SPD = mean_spd)
  }
)
df_study_with_spd<-df_study_with_spd %>%
  filter(Mean_SPD > 0)
#------


#Border not working
#-----
region_colors <- c("CMV" = "blue", "SJU" = "red", "CHC" = "darkgreen", "NRG" = "orange")

max_density <- max(sapply(normalized_spds, function(x) max(x$grid$PrDens, na.rm = TRUE)))

regions_to_plot <- names(normalized_spds)

plot(normalized_spds[[regions_to_plot[1]]],
     ylim = c(0, max_density),
     main = "Area-Normalized SPD by Region")

for (r in regions_to_plot[-1]) {
  plot(normalized_spds[[r]], col = region_colors[r], add = TRUE)
}


plot(
  normalized_spds[[regions_to_plot[1]]],
  border = region_colors[regions_to_plot[1]],
  ylim = c(0, max_density),
  main = "Area-Normalized SPD by Region"
)
for (r in regions_to_plot[-1]) {
  plot(
    normalized_spds[[r]],
    border = region_colors[r],
    add = TRUE
  )
}
legend("topright",
       legend = regions_to_plot,
       border    = region_colors[regions_to_plot],
       lty    = 1)
#----

#iteration 4
#-------
library(rcarbon)
library(dplyr)
library(tibble)
library(purrr)
library(magrittr)
library(readr)
library(here)

df1 <- read.csv("rrc.csv")

# 1. Map counties to regions
df1 <- df1 %>%
  dplyr::mutate(
    `Study Area` = dplyr::case_when(
      County == "Montezuma, CO"  ~ "CMV",
      County == "San Juan, CO"   ~ "SJU",
      County == "San Juan, NM"   ~ "CHC",
      County == "Los Alamos, NM" ~ "NRG",
      County == "Santa Fe, NM"   ~ "NRG",
      County == "Sandoval, NM"   ~ "NRG",
      County == "Rio Arriba, NM" ~ "NRG",
      County == "Taos, NM"       ~ "NRG",
      TRUE ~ NA_character_
    ),
    SiteID = Site_Number  # rename to match pipeline expectations
  ) %>%
  dplyr::filter(!is.na(`Study Area`))

df1 <- df1 %>% mutate(SiteID = as.character(Site_Number))


# 2. Region areas in sq mi
region_areas <- c(
  "CMV"  = 2040,
  "SJU"  = 7933,
  "CHC"  = 5538,
  "NRG"  = 13736  # 109 + 1911 + 3716 + 5896 + 2204
)

# 3. Build SPDs using the pipeline
timerange <- c(10000, 0)

radiocarbon_spd <- df1 %>%
  split(.$`Study Area`) %>%
  imap(~ tibble(`Study Area` = .y, Dates = list(.x))) %>%
  bind_rows() %>%
  dplyr::bind_rows(tibble::tibble(
    `Study Area` = "All Regions",
    Dates = list(df1)
  )) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    Calibrated = list(
      rcarbon::calibrate(
        x         = Dates$Date_BP,
        errors    = Dates$SD,
        timeRange = timerange,
        calMatrix = TRUE,
        verbose   = FALSE
      )
    ),
    Bins = list(rcarbon::binPrep(
      sites  = Dates$SiteID,
      ages   = Dates$Date_BP,
      h      = 100,
      method = "single"
    )),
    SPD = list(rcarbon::spd(
      x            = Calibrated,
      bins         = Bins,
      timeRange    = timerange,
      spdnormalised = TRUE,
      verbose      = FALSE
    ))
  ) %>%
  dplyr::ungroup() %>%
  # 4. Extract density and normalize by area
  dplyr::mutate(
    Area = dplyr::case_when(
      `Study Area` %in% names(region_areas) ~ region_areas[`Study Area`],
      TRUE ~ NA_real_  # All Regions row gets no area normalization
    ),
    Density = purrr::map2(`Study Area`, SPD, function(region, s) {
      grid <- s$grid %>%
        tibble::as_tibble()
      
      # Normalize by area if available
      if (region %in% names(region_areas)) {
        grid <- grid %>%
          dplyr::mutate(PrDens = PrDens / region_areas[region])
      }
      
      grid %$%
        tibble::tibble(
          Type    = "Radiocarbon Density",
          Date_BP = calBP,
          Density = PrDens
        )
    })
  )

# 5. Plot
region_colors <- c(
  "CMV"         = "blue",
  "SJU"         = "red",
  "CHC"         = "darkgreen",
  "NRG"         = "orange",
  "All Regions" = "black"
)

regions_to_plot <- radiocarbon_spd %>%
  dplyr::filter(`Study Area` != "All Regions") %>%
  dplyr::pull(`Study Area`)

max_density <- radiocarbon_spd %>%
  dplyr::filter(`Study Area` != "All Regions") %>%
  dplyr::pull(Density) %>%
  purrr::map_dbl(~ max(.x$Density, na.rm = TRUE)) %>%
  max()

first_region <- regions_to_plot[1]
plot(
  radiocarbon_spd$SPD[[which(radiocarbon_spd$`Study Area` == first_region)]],
  col  = region_colors[first_region],
  ylim = c(0, max_density),
  main = "Area-Normalized SPD by Region"
)

for (r in regions_to_plot[-1]) {
  plot(
    radiocarbon_spd$SPD[[which(radiocarbon_spd$`Study Area` == r)]],
    col = region_colors[r],
    add = TRUE
  )
}

legend("topright",
       legend = regions_to_plot,
       col    = region_colors[regions_to_plot],
       lty    = 1)

#iteration 5
#----------
library(rcarbon)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(here)

#------------------------------------------------------------
# 1. Load and prepare data
#------------------------------------------------------------

df1 <- read.csv("rrc.csv") %>%
  mutate(
    `Study Area` = case_when(
      County == "Montezuma, CO"  ~ "CMV",
      County == "San Juan, CO"   ~ "SJU",
      County == "San Juan, NM"   ~ "CHC",
      County == "Los Alamos, NM" ~ "NRG",
      County == "Santa Fe, NM"   ~ "NRG",
      County == "Sandoval, NM"   ~ "NRG",
      County == "Rio Arriba, NM" ~ "NRG",
      County == "Taos, NM"       ~ "NRG",
      TRUE ~ NA_character_
    ),
    SiteID = as.character(Site_Number)
  ) %>%
  filter(!is.na(`Study Area`))

region_areas <- c(
  CMV = 2040,
  SJU = 7933,
  CHC = 5538,
  NRG = 13736
)

timerange <- c(10000, 0)

#------------------------------------------------------------
# 2. Build region table safely
#------------------------------------------------------------

region_tbl <- df1 %>%
  split(.$`Study Area`) %>%
  imap(~ tibble(`Study Area` = .y, Dates = list(.x))) %>%
  bind_rows() %>%
  add_row(`Study Area` = "All Regions", Dates = list(df1))

#------------------------------------------------------------
# 3. Compute calibration, bins, SPDs
#------------------------------------------------------------

region_tbl <- region_tbl %>%
  mutate(
    Calibrated = map(Dates, ~ calibrate(
      x         = .$Date_BP,
      errors    = .$SD,
      timeRange = timerange,
      calMatrix = TRUE,
      verbose   = FALSE
    )),
    Bins = map(Dates, ~ binPrep(
      sites  = .$SiteID,
      ages   = .$Date_BP,
      h      = 100,
      method = "single"
    )),
    SPD = map2(Calibrated, Bins, ~ spd(
      x             = .x,
      bins          = .y,
      timeRange     = timerange,
      spdnormalised = TRUE,
      verbose       = FALSE
    ))
  )

#------------------------------------------------------------
# 4. Extract density and normalize by area
#------------------------------------------------------------

region_tbl <- region_tbl %>%
  mutate(
    Area = region_areas[`Study Area`],
    Density = map2(`Study Area`, SPD, function(region, s) {
      grid <- as_tibble(s$grid)
      
      if (region %in% names(region_areas)) {
        grid <- grid %>% mutate(PrDens = PrDens / region_areas[region])
      }
      
      tibble(
        Type    = "Radiocarbon Density",
        Date_BP = grid$calBP,
        Density = grid$PrDens
      )
    })
  )

#------------------------------------------------------------
# 5. Save output
#------------------------------------------------------------

write_rds(
  region_tbl,
  here("analysis/data/derived_data/radiocarbon_spd.rds"),
  compress = "gz"
)

radiocarbon_spd <- region_tbl