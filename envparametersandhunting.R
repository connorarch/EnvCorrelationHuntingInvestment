library(ggrepel)
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(stats)

e <- exp(1)

#SU 1000-1300
#TB 725-1200
#UM 1000-1300
#PP 1150-1400
#MEY 900-1300

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
  mutate(compindex=artioctprop/projctprop)%>%
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

#region from character to factor
pp.ppt$region <- factor(pp.ppt$region)
tb.ppt$region <- factor(tb.ppt$region)
mey.ppt$region <- factor(mey.ppt$region)
su.ppt$region <- factor(su.ppt$region)
um.ppt$region <- factor(um.ppt$region)

#combine precipitation data
precip_all <- bind_rows(
  pp.ppt, tb.ppt, mey.ppt, su.ppt, um.ppt
)

#import mfn data by region
pp.mfn<-read.csv(file="pp.mfn.csv")
tb.mfn<-read.csv(file="tb.mfn.csv")
mey.mfn<-read.csv(file="mey.mfn.csv")
su.mfn<-read.csv(file="su.mfn.csv")
um.mfn<-read.csv(file="um.mfn.csv")

#region from character to factor
pp.mfn$region <- factor(pp.mfn$region)
tb.mfn$region <- factor(tb.mfn$region)
mey.mfn$region <- factor(mey.mfn$region)
su.mfn$region <- factor(su.mfn$region)
um.mfn$region <- factor(um.mfn$region)

#combine precipitation data
mfn_all <- bind_rows(
  pp.mfn, tb.mfn, mey.mfn, su.mfn, um.mfn
)

# Create a mapping between District codes and region names
district_to_region <- c(
  "Pajarito Plateau" = "PP",
  "Ute Mountain" = "UM",
  "Southern Utah" = "SU",
  "McElmo-Yellowjacket" = "MEY",
  "Tewa Basin" = "TB"
)

# Define regions and their corresponding file patterns
regions <- c("PP", "TB", "MEY", "SU", "UM")

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
               "SU.1000.1300.csv", "UM.1000.1300.csv")
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

# Create a mapping between District codes and region names
district_to_region <- c(
  "Pajarito Plateau" = "PP",
  "Ute Mountain" = "UM",
  "Southern Utah" = "SU",
  "McElmo-Yellowjacket" = "MEY",
  "Tewa Basin" = "TB"
)

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
    scale_shape_manual(values=c(3, 17, 7, 16, 13))
  
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

# Call the function
my_plot <- plot_flexible_returns(
  plot_title = "Hunting Investment for Mean Water-Year Precipitation",
  x_var_name = "mean_ppt",
  y_var_name = "projctprop", # Function handles the log transformation internally
  dataset = sitedata,
  x_label_text = "Mean Water-Year Precipitation (mm)",
  y_label_text = "ln(points/points + grayware)",
  log_x = FALSE,  # New parameter: set to TRUE to log-transform x
  log_y = TRUE,
  show_centroids = TRUE,
  show_regression = TRUE
)

# Display the plot
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
  show_centroids = TRUE
)

# Display the plot
print(my_plot)

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

