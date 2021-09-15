setwd("C:/OneDrive/C1_LitReview/Data_Updates_Feb_2017")

library(vegan)
library(doBy)
library(rgdal)
library(clustsig)
library(gdata)
library(ggplot2)
library(scales)
library(ggrepel)
library(RColorBrewer)
library(reshape2)
library(EnvStats)
library(dplyr)
library(plyr)


data <- read.csv("LitReviewData_2017.csv", header = TRUE)
data$Year <- as.factor(data$Year)
data_nodupe <- read.csv("LitReviewDataNoCountryDuplicates_2017.csv", header = TRUE)
data_nodupe$Year <- as.factor(data_nodupe$Year)


barplot(table(data_nodupe$Year.Category)/sum(table(data_nodupe$Year.Category)/100),
        xlab= "Year",
        ylab = "Proportion of Studies (%)",
        font.lab = 2,
        ylim = c(0,60),
        cex.names = 1.5,
        cex.lab = 1.5) 




###############################
#### YEAR PROPORTION PLOT ####
###############################

# FIGURE 3

fig3 <- ggplot(data_nodupe, aes(x=Year.Category)) + 
        geom_bar(aes(y= (..count..)/sum(..count..))) +
        scale_y_continuous(labels = percent, limits = c(0, 0.6)) +
        theme_bw() +
        theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
        labs(x = "Year", y = "Proportion of studies (%)") +
        theme(axis.title.x = element_text(face="bold")) +
        theme(axis.title.y = element_text(face="bold"))
fig3

setwd("C:/OneDrive/C1_LitReview/Figures")
tiff("Figure3.tiff", width = 170, height = 100, units = 'mm', res = 500)
fig3
dev.off()


#################################
#### STUDY FOCUS BY YEAR PLOT ####
#################################



ggplot(data_nodupe, aes(x=Year.Category, fill = Study.Focus.NEW)) + 
  geom_bar(position = "fill", colour = "black") +
  scale_y_continuous(labels = percent) +
  scale_fill_grey(start = 0, end = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Year", y = "Proportion of studies (%)") +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  guides(fill = guide_legend(reverse=FALSE, title= NULL))


Focus <- data_nodupe$Study.Focus.NEW

ggplot(data_nodupe, aes(x=Year.Category, fill = Study.Focus.NEW)) + 
  geom_bar(position = "fill", colour = "black") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(Focus, values = getPalette(colourCount)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Year", y = "Proportion of studies (%)") +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  guides(fill = guide_legend(reverse=FALSE, title= NULL))



colours <- c("Black", "gray40", "gray", "navy", "blue", "cyan4", "cyan", "lightskyblue1","darkgreen",
             "green4","chartreuse3","palegreen4", "palegreen2", "olivedrab2",
             "red", "purple", "gold", "tan2", "khaki3", "magenta", 
             "darkorange", "red4", "pink", "greenyellow", "brown")


# FIGURE 5
fig5 <- ggplot(data_nodupe, aes(x=Year.Category, fill = Study.Focus.NEW)) + 
        geom_bar(position = "fill", colour = "black") +
        scale_y_continuous(labels = percent) +
        # scale_fill_manual(Focus, values = colours) +
        scale_fill_brewer(palette="PRGn") +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(x = "Year", y = "Proportion of studies (%)") +
        theme(axis.title.x = element_text(face="bold")) +
        theme(axis.title.y = element_text(face="bold")) +
        guides(fill = guide_legend(reverse=FALSE, title= NULL))

setwd("C:/OneDrive/C1_LitReview/Figures")
tiff("Figure5.tiff", width = 170, height = 100, units = 'mm', res = 500)
fig5
dev.off()


#############################
#### METHOD BY YEAR PLOT ####
############################

method <- data_nodupe[c(1:29)]
method<-melt(method, id=c(1:9))
method<- method[c(3,10,11)]
method$variable<-gsub("\\.", " ", method$variable) 
method <- method[complete.cases(method),]


# Data exploration 
aggregate <- aggregate(method$value, by=list(method=method$variable), FUN=sum)
aggregate <- aggregate[order(aggregate$x),]
aggregate

require(reshape2)
df_melt <- melt(method, id = c("Year.Category", "variable", "value"))
df_cast<- dcast(df_melt, variable + Year.Category ~ value, sum)
# write.csv(df_cast, "methodsummary.csv")

# put in order by year
method$Year.Category <- factor(method$Year.Category, levels=levels(method$Year.Category)[order(levels(method$Year.Category), decreasing = TRUE)])

# put in order if not flipping axis
method$variable <- factor(method$variable, levels=c("SCUBA", "Labwork  ID   Single Measurements", "Submersible", "Labwork  Genetics",
                                                    "ROV","Acoustics","Labwork  Experimental","Towed-Video", "AUV",
                                                     "Epibenthic Sled", "Modelling", "Grabs","BRUV", "Tags", "Corers",
                                                    "Fishing Techniques","Water Profiler", "Satellite Imagery",
                                                    "Light Traps", "Recruitment Tiles"))



ggplot(na.omit(method), aes(x=variable)) + 
  geom_bar(aes(y= (..count..)/sum(..count..), fill=Year.Category), colour = "black") +
  scale_y_continuous(labels = percent, position = "right") +
  scale_fill_grey(start = 0, end = 0.9) +
  theme_bw() +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Method", y = "Proportion of studies (%)") +
  theme(axis.title.x = element_text(face="bold", size = 14)) +
  scale_x_discrete(labels=c("SCUBA", "Labwork: ID / Single Measurements", "Submersible", "Labwork: Genetics",
                            "ROV","Acoustics","Labwork: Experimental","Towed-Video", "AUV",
                            "Epibenthic Sled", "Modelling", "Grabs","BRUV", "Tags", "Corers",
                            "Fishing Techniques","Water Profiler", "Satellite Imagery",
                            "Light Traps", "Recruitment Tiles")) +
  theme(axis.title.y = element_text(face="bold", size = 14)) +
  guides(fill = guide_legend(reverse=FALSE, title= NULL)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14))

# put in order if flipping axis
method$variable <- factor(method$variable, levels=c("Light Traps","Recruitment Tiles", "Satellite Imagery",
                                                    "Water Profiler", "Corers", "Fishing Techniques", "Tags", "BRUV", "Grabs",
                                                    "Modelling", "Epibenthic Sled", "AUV", "Towed Video", "Labwork  Experimental",
                                                    "Acoustics", "ROV", "Labwork  Genetics", "Submersible",
                                                    "Labwork  ID   Single Measurements", "SCUBA"))



ggplot(na.omit(method), aes(x=variable)) + 
  geom_bar(aes(y= (..count..)/sum(..count..), fill=Year.Category), colour = "black") +
  scale_y_continuous(labels = percent, position = "right") +
  scale_fill_grey(start = 0, end = 1.0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Proportion of studies") +
  scale_x_discrete(labels=c("Recruitment Tiles", "Light Traps", "Satellite Imagery",
                            "Water Profiler", "Corers", "Fishing Techniques", "Tags", "BRUV", "Grabs",
                            "Modelling", "Epibenthic Sled", "AUV", "Towed-Video", "Labwork: Experimental",
                            "Acoustics", "ROV", "Labwork: Genetics", "Submersible",
                            "Labwork: ID / Single Measurements", "SCUBA")) +
  theme(axis.title.x = element_text(face="bold", size = 12)) +
  theme(axis.title.y = element_blank()) +
  guides(fill = guide_legend(reverse=FALSE, title= NULL)) +
  theme(axis.text.x = element_text(size = 10)) +
  coord_flip()



# IN COLOUR


fig6 <- ggplot(na.omit(method), aes(x=variable)) + 
        geom_bar(aes(y= (..count..)/sum(..count..), fill=Year.Category), colour = "black") +
        scale_y_continuous(labels = percent, position = "right") +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        # scale_fill_brewer() +
        # scale_fill_manual(values=c("white","cyan","cyan4", "blue", "navy")) +
        scale_fill_brewer(palette="PRGn") +
        # scale_fill_brewer(palette="GnBu") +
        labs(y = "Proportion of studies") +
        scale_x_discrete(labels=c("Recruitment Tiles", "Light Traps", "Satellite Imagery",
                                  "Water Profiler", "Corers", "Fishing Techniques", "Tags", "BRUV", "Grabs",
                                  "Modelling", "Epibenthic Sled", "AUV", "Towed-Video", "Labwork: Experimental",
                                  "Acoustics", "ROV", "Labwork: Genetics", "Submersible",
                                  "Labwork: ID / Single Measurements", "SCUBA")) +
        theme(axis.title.x = element_text(face="bold", size = 12)) +
        theme(axis.title.y = element_blank()) +
        guides(fill = guide_legend(reverse=FALSE, title= NULL)) +
        theme(axis.text.x = element_text(size = 10)) +
        coord_flip()

setwd("C:/OneDrive/C1_LitReview/Figures")
tiff("Figure6.tiff", width = 170, height = 100, units = 'mm', res = 500)
fig6
dev.off()


#############################
#### Shallow v deep plot ####
############################

svd <- read.csv("shallowvdeep.csv", header = TRUE)

ggplot(svd, aes(x = Shallow, y = Deep)) +
  geom_point(aes(color = Species, shape = Region, size = 1)) +
  scale_size(guide = 'none') +
  scale_shape_manual(values=c(15,18,16,17)) +
  geom_text(aes(label= Location),hjust= -0.25, vjust=-0.15, size = 3) +
  scale_color_grey() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Shallow species richness", y = "Deep species richness") +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1000)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 185)) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.title=element_blank())


# Log Scale

ggplot(svd, aes(x = Shallow, y = Deep)) +
  geom_point(aes(color = Species, shape = Region, size = 1)) +
  scale_size(guide = 'none') +
  scale_shape_manual(values=c(15,18,16,17)) +
  geom_text(aes(label= Location),hjust= -0.25, vjust=-0.15, size = 3.05) +
  scale_color_grey() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Shallow species richness", y = "Deep species richness") +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1500)) + scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.title=element_blank()) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1e1, 1e3)) +
  stat_function(fun = function(x) 1*x, geom='line',colour = 'red') +
  geom_text_repel(data = svd, aes(label = Location))


# with label repel

ggplot(svd, aes(x = Shallow, y = Deep)) +
  geom_point(aes(color = Species, shape = Region, size = 1)) +
  scale_size(guide = 'none') +
  scale_shape_manual(values=c(15,18,16,17)) +
  scale_color_grey() + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Shallow species richness", y = "Deep species richness") +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(legend.text=element_text(size=12)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1500)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  guides(colour = guide_legend(override.aes = list(size=5)), shape = guide_legend(override.aes = list(size=5))) +
  theme(legend.title=element_blank()) +
  theme(axis.title.x = element_text(face="bold", size = 14)) +
  theme(axis.title.y = element_text(face="bold", size = 14)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1e1, 1e3)) +
  stat_function(fun = function(x) 1*x, geom='line',colour = 'red') +
  geom_text_repel(data = svd, aes(label = Location), size = 5, nudge_x = 0.15)
 


#################
#### GENETICS ####
#################

setwd("C:/OneDrive/C1_LitReview/Data_Updates_Feb_2017")
genetics <- read.csv("genetics.csv", header = TRUE)

gen <- genetics[!is.na(genetics$Depth.occurred), ]


gg<- ggplot(gen, aes(Genera, y = Depth.occurred)) +
  geom_boxplot() +
  scale_x_discrete(position = "top") +
  labs(x = "Genera", y = "Depth (m)") +
  scale_y_reverse(breaks = seq(0, 90, by = 10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y = element_text(face="bold", size = 12)) +
  theme(axis.title.x = element_text(face="bold", size = 12)) +
  geom_hline(yintercept=30, linetype="dashed", color = "red") +
  theme(axis.ticks.x=element_blank()) +
  stat_n_text(size = 3, y.pos = 1, fontface = "italic") +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size = 9, vjust = 1)) 

gen2 <- genetics
gen2<- gen2[!gen2$Genera == "Acropora", ]
gen2<- gen2[!gen2$Genera == "Helioseris", ]
gen2<- gen2[!gen2$Genera == "Montipora", ]
gen2<- gen2[!gen2$Genera == "Eusmilia", ]
gen2<- gen2[!gen2$Genera == "Mycetophyllia", ]
gen2<- gen2[!gen2$Genera == "Meandrina", ]
factors <- as.factor(gen2$Genera)


# FIGURE 7
  
fig7 <- ggplot(gen2, aes(Genera, y = Depth.occurred)) +
        geom_boxplot() +
        geom_hline(yintercept=30, linetype="dashed", color = "gray", size = 2) +
        geom_vline(xintercept = 0, color = "black") +
        stat_n_text(size = 2.5, y.pos = 5, fontface = "italic") +
        labs(y = "Depth (m)") +
        scale_x_discrete(position = "top") +
        scale_y_reverse(breaks = seq(0, 90, by = 10)) +
        theme_bw() +
        # scale_fill_brewer(palette="PRGn") +
        scale_fill_manual(values = c("purple2", "green4")) +
        theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_text(face="bold", size = 12), 
        axis.title.x = element_blank(),
        axis.ticks.x=element_blank()) +
        theme(axis.text.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90)) 
        # geom_text(aes(x=Genera, y=-2, label=Genera, fontface = "bold"), size = 2)
        # theme(legend.title=element_text(face="bold"))
        # theme(legend.position="none") +
        # coord_flip()

setwd("C:/OneDrive/C1_LitReview/Figures")
tiff("Figure7.tiff", width = 170, height = 120, units = 'mm', res = 500)
fig7
dev.off()




ggplot(gen2, aes(Genera, y = Depth.occurred)) +
  geom_boxplot(aes(color = reproduction)) +
  scale_color_manual(guide = guide_legend(title = "Reproductive Mode"),values=c("purple2", "green4")) +
  geom_hline(yintercept=30, linetype="dashed", color = "red", size = 1.5) +
  geom_vline(xintercept = 0, color = "black") +
  stat_n_text(size = 3, y.pos = 0, fontface = "italic") +
  labs(y = "Depth (m)") +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks = seq(0, 90, by = 10)) +
  theme_bw() +
  # scale_fill_brewer(palette="PRGn") +
  # scale_fill_manual(values = c("purple2", "green4")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_text(face="bold", size = 12), 
        axis.title.x = element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(x=Genera, y=-2, label=Genera, fontface = "bold")) +
  theme(legend.title=element_text(face="bold"))




######################################
#### COMMUNITY / TRANSITION DEPTHS ####
#####################################

community <- read.csv("community.csv", header = TRUE)

summary <- aggregate(community$Depth.occurred, by=list(method=community$RaR.Region), FUN=mean)
summary

meso <- subset(community, community$Mesophotic.v.Mesophotic == 1)
summeso <- aggregate(meso$Depth.occurred, by=list(method=meso$RaR.Region), FUN=mean)
summeso

ggplot(community, aes(RaR.Region, y = Depth.occurred)) +
  geom_boxplot(aes(fill = Focus.Organism))


##################
#### DIVERSITY ####
##################

diversity <- read.csv("diversity.csv", header = TRUE)

ggplot(diversity, aes(RaR.Region, y = SpDiv)) +
  geom_boxplot(aes(fill = Organism))
