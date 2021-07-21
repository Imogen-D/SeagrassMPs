#Script for final plots + stats

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotrix)
library(scales)
library(dunn.test)
library(ggsci)

#Import Data
Info <- read.csv("~/MEME/Archipelagos/Data/Info.csv", stringsAsFactors = FALSE)
names(Info)[1] <- 'site'
Info$site <- str_replace(Info$site, "Kat", "D")
samples <- read.csv("samples.csv")
samples <- samples[-1]

SedimentInfo <- read.csv("~/MEME/Archipelagos/Data/SedimentInfo.csv", stringsAsFactors = FALSE)
colnames(SedimentInfo)[1] <- "Name"
sedimentsamples <- read.csv("sedimentsamples.csv")
sedimentsamples <- sedimentsamples[-1]

#TOTAL MPS
length(sedimentsamples$loc) #174
length(samples$Site) #total MPs across all incl Blank = 1613 -> already removed ornage frags so 1576

##For Seagrasses
#then just use dataset w/o blanks
sampleswoblank <- samples[samples$Site != "Blank",]

#Check normality of size
shapiro.test(sampleswoblank$Size) #< 2.2e-16 NOT NORMAL

#Making dataframe accessable for tests and calculating number of MPs per site
siteswoblank <- unique(sampleswoblank$Site)

MPswoblank <- c()
for (i in siteswoblank) {
  x <- length(which(sampleswoblank$Site==i))
  MPswoblank <- append(MPswoblank, x)
}
location <- str_sub(siteswoblank, start = 1, end = 1)
replicates <- str_sub(siteswoblank, start = 1, end = 2)
rep <- str_sub(siteswoblank, start = 2, end = 2)
numMPframewoblank <- data.frame(site = siteswoblank, MPswoblank, location, replicates, rep)

Info2woblank <- merge(numMPframewoblank, Info, by.x="site", by.y="site")

#making MP frame with blanks for later
sites <- unique(samples$Site)
MPs <- c()
for (i in sites) {
  x <- length(which(samples$Site==i))
  MPs <- append(MPs, x)
}
location <- str_sub(sites, start = 1, end = 1)
replicates <- str_sub(sites, start = 1, end = 2)
numMPframe <- data.frame(site = sites, MPs, location, replicates)

#Checking normaility of number of MPs
shapiro.test(numMPframewoblank$MPswoblank) #1.857e-09 NOT NORMAL

#comparing number of MPs between sites and locations
kruskal.test(MPswoblank ~ rep, data = numMPframewoblank) #p-value = 0.5872
#so group locations?
kruskal.test(MPswoblank ~ location, data = numMPframewoblank) #p-value = 0.6405
x <- dunn.test(numMPframewoblank$MPswoblank, numMPframewoblank$loc) #none significant
write.csv(x, file = "SGMPLocComp.csv")

kruskal.test(Size~loc, data = sampleswoblank) #p = 0.0004959 = SIGNFICANT
x <- dunn.test(sampleswoblank$Size, sampleswoblank$loc) #lotsSIG
write.csv(x, file = "SGSizeLocComp.csv")

#need overall average length w/ mean and std dev
mean(Info2woblank$Length) #384.6667
std.error(Info2woblank$Length) #20.84554
#NUMBER OF MPs PER MM OF BLADE
sum(numMPframewoblank$MPs)/sum(Info2woblank$Length) #0.1355

#number of MPs per blade w/ mean and standard error
mean(numMPframewoblank$MPs) #52.13
std.error(numMPframewoblank$MPs) #9.42

##For sediments
#Check normality of sizes
shapiro.test(sedimentsamples$Size) #< 2.2e-16 NOT NORMAL

#Tests for sites/repliates and sizes
kruskal.test(Size~Coll, data = sedimentsamples) #p=0.4293
x <- dunn.test(sedimentsamples$Size, sedimentsamples$Coll) #no sig
write.csv(x, file = "SedSizesitesComp.csv")

kruskal.test(Size~loc, data = sedimentsamples) #p=0.0009556
x <- dunn.test(sedimentsamples$Size, sedimentsamples$loc) #are sig
write.csv(x, file = "SedSizeLocComp.csv")


#Making dataframe accessable for tests and calculating number of MPs per site
#Making coloumn of locationsite
SedimentMPs <- c()
Sedimentsites <- unique(sedimentsamples$Site)
for (i in Sedimentsites) {
  x <- length(which(sedimentsamples$Site==i))
  SedimentMPs <- append(SedimentMPs, x)
}

sedimentMPframe <- data.frame(site = Sedimentsites, MPs = SedimentMPs)

SedimentInfo2 <- merge(sedimentMPframe, SedimentInfo, by.x="site", by.y="Name")

#Test normality of number of MPs
shapiro.test(SedimentInfo2$MPs) #0.07485 = normal?? but not currently doing stats on these

#number of MPs per sample w/ mean and std dev (or standard error?)
mean(sedimentMPframe$MPs) #8.7
std.error(sedimentMPframe$MPs) #0.7681146

kruskal.test(SedimentInfo2$MPs ~ SedimentInfo2$Loc)
x <- dunn.test(SedimentInfo2$MPs, SedimentInfo2$Loc) #no sig
write.csv(x, file = "SedMPLocComp.csv")

kruskal.test(SedimentInfo2$MPs ~ SedimentInfo2$Site)

#PLOTS##

#SEAGRASSES

#number of MPs amongst sites - want blanks in all of this
ggplot(data = numMPframe, aes(x=replicates, y=MPs, color = location)) +
  geom_bar(stat = "identity") +
  labs(y= "Number of MPs", x = "Locations", title = "Number of MPs amongst sites") +
  scale_color_hue(labels = c("Blank", "Katsadia", "Kampos", "Platis", "Vroulia", "Xerokampos"))

samples$location <- str_sub(samples$Site, start = 1, end = 1)
samples$replicates <- str_sub(samples$Site, start = 1, end = 2)
ggplot(data=samples, aes(x=replicates, y=Size, color = location)) +
  geom_boxplot(stat = "boxplot") +
  labs(y= "Size of MPs", x = "Locations", title = "Size of MPs amongst sites in SG") +
  scale_color_hue(labels = c("Blank", "Katsadia", "Kampos", "Platis", "Vroulia", "Xerokampos")) +
  stat_summary(fun.y=mean, geom="point", shape=1, size=2)

#Number of MP over length (no blanks in this one)
ggplot(data = Info2woblank, aes(x=Length, y=MPswoblank, color = location)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(y= "Number of MPs", x = "Length(mm)", title = "Number of MPs over SG length") +
  scale_color_hue(labels = c("Katsadia", "Kampos", "Platis", "Vroulia", "Xerokampos"))
#remove V3A?
LengthwoVr <- Info2woblank[-23,]
ggplot(data = LengthwoVr, aes(x=Length, y=MPswoblank, color = location)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(y= "Number of MPs", x = "Length(mm)", title = "Number of MPs over SG length") +
  scale_color_hue(labels = c("Katsadia", "Kampos", "Platis", "Vroulia", "Xerokampos"))

#Plots of MP type
perc_typeSG <- samples %>% 
  count(Type, loc) %>% 
  group_by(loc) %>% 
  mutate(percent = n/sum(n))

ggplot(perc_typeSG, aes(x = loc, y = percent, fill = Type)) + 
  geom_col(position = "fill") + 
  labs(y= "Percentage of MPs", x = "Locations", title = "MP types among sites")

#Or not a %
ggplot(data = perc_typeSG, aes(x = loc, y = n, color = Type)) + 
  geom_bar(stat = "identity")+
  labs(y= "Number of MPs", x = "Locations", title = "MP types among sites")

#Plots of colours

perc_colourSG <- samples %>% 
  count(Colour, loc) %>% 
  group_by(loc) %>% 
  mutate(percent = n/sum(n))

cola <- c("Black"= "#000000","Blue" = "#0000FF", "Transparent" = NA,
            "Green" = "#33CC33", "Red" = "#FF0000", "White" = "#FFFFFF", "Other" = "#FF3399")

ggplot(data = perc_colourSG, aes(x = loc, y = n, fill = Colour)) + 
  geom_bar(stat = "identity", colour = "Black")+
  labs(y= "Number of MPs", x = "Locations", title = "MP colours among sites") +
  scale_fill_manual(values = col) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500))

ggplot(data = perc_colourSG, aes(x = loc, y = percent, fill = Colour)) + 
  geom_bar(stat = "identity", colour = "Black")+
  labs(y= "Number of MPs", x = "Locations", title = "MP colours among sites") +
  scale_fill_manual(values = cola) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.00))

#PLOTS FOR SEDIMENTS

#number of MPs amongst sites
ggplot(data = SedimentInfo2, aes(x=Loc, y=MPs, color = Site)) +
  geom_bar(stat = "identity") +
  labs(y= "Number of MPs", x = "Locations", title = "Number of MPs amongst sites")

#want percentage of above
perc_MP_sed <- sedimentsamples %>% 
  count(Site, loc) %>% 
  group_by(loc) %>% 
  mutate(percent = n/sum(n))

ggplot(data = perc_MP_sed, aes(x=loc, y=percent, color = loc)) +
  geom_bar(stat = "identity") +
  labs(y= "Number of MPs", x = "Locations", title = "Number of MPs amongst sites in sediment") 
#actually want colour via A/B/1/2?

#Plot of size of sediment MPs
ggplot(data=sedimentsamples, aes(x=Site, y=Size, color = loc)) +
  geom_boxplot(stat = "boxplot") +
  labs(y= "Size of MPs", x = "Locations", title = "Size of MPs amongst sites in sediment") 
#maually change sample numbers

#merge locations and put types in boxplots-> percentage contributions of type
perc_typesed <- sedimentsamples %>% 
  count(Type, loc) %>% 
  group_by(loc) %>% 
  mutate(percent = n/sum(n))

ggplot(perc_typesed, aes(x = loc, y = percent, fill = Type)) + 
  geom_col(position = "fill") + 
  labs(y= "Percentage of MPs", x = "Locations", title = "MP types among sites in sediments")

ggplot(data = perc_typesed, aes(x = loc, y = n, color = Type)) + 
  geom_bar(stat = "identity")+
  labs(y= "Number of MPs", x = "Locations", title = "MP types among sites in sediments")

perc_colour_sed <- sedimentsamples %>% 
  count(Colour, loc) %>% 
  group_by(loc) %>% 
  mutate(percent = n/sum(n))

ggplot(data = perc_colour_sed, aes(x = loc, y = n, fill = Colour)) + 
  geom_bar(stat = "identity")+
  labs(y= "Number of MPs", x = "Locations", title = "MP colours among sites in sediments") +
  scale_fill_manual(values = cola)

ggplot(data = perc_colour_sed, aes(x = loc, y = percent, fill = Colour)) + 
  geom_bar(stat = "identity", colour = "Black")+
  labs(y= "Number of MPs", x = "Locations", title = "MP colours among sites in sediments") +
  scale_fill_manual(values = colaz2) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.00))


#Combining data

sedsamples <- subset(sedimentsamples, select = -c(Coll))
allMPslist <- rbind(samples, sedsamples)

#PLOTS FOR BOTH
#Colour Plots for both SG and Sediment
allColours <- allMPslist %>%
  count(Colour, p) %>%
  group_by(p) %>% 
  mutate(percent = n/sum(n))

ggplot(data = allColours, aes(x = p, y = n, fill = Colour)) + 
  geom_bar(stat = "identity", colour = "Black")+
  labs(y= "Number of MPs", x = "Locations", title = "MP colours among locations") +
  scale_fill_manual(values = cola) +
  scale_y_continuous(expand = c(0,0), limits = c(0,500)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = allColours, aes(x = p, y = percent, fill = Colour)) + 
  geom_bar(stat = "identity", colour = "Black")+
  labs(y= "Number of MPs", x = "Locations", title = "MP colours among locations") +
  scale_fill_manual(values = cola) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.00)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

allTypes <- allMPslist %>%
  count(Type, p) %>%
  group_by(p) %>% 
  mutate(percent = n/sum(n))

ggplot(data = allTypes, aes(x = p, y = n, fill = Type)) + 
  geom_bar(stat = "identity", colour = "Black")+
  labs(y= "Number of MPs", x = "Locations", title = "MP type among locations") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_npg() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

ggplot(data = allTypes, aes(x = p, y = percent, fill = Type)) + 
  geom_bar(stat = "identity", colour = "Black")+
  labs(y= "Number of MPs", x = "Locations", title = "MP type among locations") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_npg() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

#COMPARISON GRAPHS
SedimentInfo3 <- SedimentInfo2[c(1,2,11, 12)]
SedimentInfo3$Type <- "Sediment"
numMPframe$Type <- "SG"
colnames(SedimentInfo3) <- colnames(numMPframe)
allMPs <- rbind(SedimentInfo3, numMPframe)

ggplot(data = allMPs, aes(y=MPs, x = Type, color = location)) +
  geom_bar(stat = "identity") +
  labs(y= "Number of MPs", x = "Collection", title = "Number of MPs amongst locations")

ggplot(allMPs, aes(fill=location, y=MPs, x=Type)) + 
  geom_bar(position="fill", stat="identity") +
  labs(y= "% of MPs", x = "Collection", title = "Number of MPs amongst locations") +
  scale_fill_npg(labels = c("Blank", "Katsadia", "Kampos", "Platis", "Vroulia", "Xerokampos")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  
  scale_fill_viridis(option = "D", discrete = TRUE)

ggplot(data=allMPslist, aes(x=p, y=Size, color = loc)) +
  geom_boxplot(stat = "boxplot") +
  labs(y= "Size of MPs (mm)", x = "Locations", title = "Size of MPs amongst locations") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12)) +
  scale_color_npg(labels = c("Blank", "Katsadia", "Kampos", "Platis", "Vroulia", "Xerokampos"))

sum(SedimentInfo2$MPs[which(SedimentInfo2$Loc == "K")])
sum(SedimentInfo2$MPs[which(SedimentInfo2$Loc == "D")])
sum(SedimentInfo2$MPs[which(SedimentInfo2$Loc == "V")])
sum(SedimentInfo2$MPs[which(SedimentInfo2$Loc == "X")])
sum(SedimentInfo2$MPs[which(SedimentInfo2$Loc == "P")])


sum(sedimentsamples$Size[which(sedimentsamples$loc == "D")])
sum(sedimentsamples$Size[which(sedimentsamples$loc == "K")])
sum(sedimentsamples$Size[which(sedimentsamples$loc == "P")])
sum(sedimentsamples$Size[which(sedimentsamples$loc == "V")])
sum(sedimentsamples$Size[which(sedimentsamples$loc == "X")])

mean(allMPs$MPs[which(allMPs$SiteType == "P SG")])


c <- allMPs %>%
  group_by(SiteType) %>%
  summarize(mean_MPs = mean(MPs))


mean(c(27, 33, 56, 40, 24)) #Number of MPs in V SG wo outlier

allMPs %>%
  group_by(SiteType) %>%
  summarize(median_MPs = median(MPs))

allMPslist %>%
  group_by(p) %>%
  summarise(median_size = median(Size))

allMPslist %>%
  group_by(p) %>%
  summarise(median_size = mean(Size))

length(which(allMPslist$Colour == "Black")) #/1750 = 37.09
length(which(allMPslist$Colour == "Blue")) #29.14
length(which(allMPslist$Colour == "Green")) #8.86
length(which(allMPslist$Colour == "Red"))

max(samples$Size)
min(samples$Size)
max(sedsamples$Size)
min(sedsamples$Size)

length(which(samples$Size >= 5.67))
summary(sampleswoblank$Size)
IQR(sampleswoblank$Size)
summary(sedsamples$Size)
IQR(sedsamples$Size)

length(which(allMPslist$Type == "Fibre")) #1577 = 90.11
length(which(allMPslist$Type == "Fragment")) #157 = 8.97 #59 from sediemnts
length(which(allMPslist$Type == "Foam")) #16 = 0.91
length(allMPslist$Type)
foams <- which(allMPslist$Type == "Foam")
allMPslist[foams,]
frag <- which(allMPslist$Type == "Fragment")
frags <- allMPslist[frag,]
length(which(frags$x == "Sediment"))
y <- str_count(frags$x, "Sediment")
length(which(y == 1))
