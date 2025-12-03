library(ggrepel)
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lmtest)

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

#characters in date range to numerical range
sitedata <- sitedata %>%
  separate(Date, into = c("start", "end"), sep = "-", remove = FALSE) %>%
  mutate(start = as.numeric(start),
         end = as.numeric(end))

# Create a mapping between District codes and region names
district_to_region <- c(
  "Pajarito Plateau" = "PP",
  "Ute Mountain" = "UM",
  "Southern Utah" = "SU",
  "McElmo-Yellowjacket" = "MEY",
  "Tewa Basin" = "TB"
)

# Add a column with the full region name
sitedata$region_name <- factor(district_to_region[as.character(sitedata$District)])

sitedata$mean_ppt <- mapply(function(start, end, region_name) {
  precip_all %>%                           # <-- This references the precip dataframe
    filter(region_name == region, year >= start, year <= end) %>%
    summarise(mean_ppt = mean(ppt, na.rm = TRUE)) %>%
    pull(mean_ppt)
}, sitedata$start, sitedata$end, sitedata$region_name)

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
        panel.background = element_rect(color = "black"))

fig8text <- str_wrap("Log(Artiodactyls) for Mean Precipitation", 65)
fig8 <- ggplot(sitedata, aes(y=log(Artiodactyls),x=mean_ppt))
fig8 +
  geom_vline(aes(xintercept=mean(mean_ppt)), color='black', linetype="dashed", linewidth=1)+
  geom_hline(aes(yintercept=mean(log(Artiodactyls))), color='black', linetype="dashed", linewidth=1)+
  geom_point(data=sitedata, aes(shape=District), size=4)+
  scale_shape_manual(values=c(3, 17, 7, 16, 13))+
  geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2, label.x = 200, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
  stat_regline_equation(family = "serif", size = 6,
                        label.y = -2.5, label.x = 200, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
  theme(plot.title=element_text(hjust=.15))+
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

#grouping by high and low hunting investment
highhunting<-sitedata %>%
  filter(District %in% c('Tewa Basin', 'Southern Utah'))
lowhunting<-sitedata %>%
  filter(District %in% c('Pajarito Plateau', 'Ute Mountain'))

#comparing residuals to fit line for high and low hunting groups
ttest <- t.test(highhunting$residuals, lowhunting$residuals, var.equal = T)
ttest

#linear model of hunting returns for investment
ap.lm <- lm(log(projctprop)~log(artioctprop),sitedata)
summary(ap.lm)

#create column for residuals to linear model of investment and return for sites
sitedata$residuals <- ap.lm$residuals

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
  theme(plot.title=element_text(hjust=0.45))+
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

