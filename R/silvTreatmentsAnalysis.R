### Analysis of stand treatments
# A. Clason 
# February, 2023

#------------------------------ Load libraries----------------------------------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)
#create colour palette
paper_pal <- c("#E69F00", "#56B4E9", "#000000", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#--------------1. read in csvs of silvicultural treatments and dNBR--------------------#
FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")

sfiles <- list.files("./Inputs/", "silv", full.names = TRUE)
silv_csv_l <- map(sfiles,fread)
silv_csv_n <- str_extract(sfiles, FiresOfInterest)
for(i in 1:length(FiresOfInterest)){
  silv_csv_l[[i]] <- silv_csv_l[[i]][,ID := silv_csv_n[i]]
}
silv_csv <- rbindlist(silv_csv_l, fill=TRUE)
ID_dt <- data.table(FireName = c("Chutanli","Nadina","Shovel","Island","Verdun","Tezzeron"),
                    FireID = c("G41607","R21721","R11498","R11921","R11796","G51632"))
silv_csv <- merge(silv_csv,ID_dt, by.x="ID",by.y="FireID", all = TRUE)


#--------------2. select treatments of interest and create equal sampling --------------------#
silv_var_int <- c("BroadBurn","Disc","Brushed","Spaced")

#-------- BROADCAST BURNING --------
bb_all <- list()
for(ii in 1:length(FiresOfInterest)){
  bb_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="BroadBurn"]
  bb_0 <- silv_csv[ID==FiresOfInterest[ii]& BroadBurn==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] & BroadBurn==0]),
                                        bb_N[BroadBurn==1]$N),]
  bb_1 <- silv_csv[ID==FiresOfInterest[ii] & BroadBurn==1]
  bb_all[[ii]] <- rbind(bb_0[,.(dNBR,BroadBurn,FireName,ID)],bb_1[,.(dNBR,BroadBurn,FireName,ID)])
}
bb_all <- do.call(rbind,bb_all)
bb_all[,BroadBurn := as.factor(BroadBurn)]
bb_all[, shapiro.test(dNBR), by=c("FireName","BroadBurn")]
#distributions pretty much non-normal, so non-parametric Wilcox test

#-------- DISC TRENCHING --------
d_all <- list()
for(ii in 1:length(FiresOfInterest)){
  d_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="Disc"]
  if(nrow(d_N[Disc==1])>0){
    d_0 <- silv_csv[ID==FiresOfInterest[ii]& Disc==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] & Disc==0]),
                                                             d_N[Disc==1]$N), ]
    d_1 <- silv_csv[ID==FiresOfInterest[ii] & Disc==1]
  }
  d_all[[ii]] <- rbind(d_0[,.(dNBR,Disc,FireName,ID)],d_1[,.(dNBR,Disc,FireName,ID)])
}
d_all <- do.call(rbind,d_all)
d_all[,Disc := as.factor(Disc)]
d_all[, shapiro.test(dNBR), by=c("FireName","Disc")]
#distributions pretty much non-normal, so non-parametric Wilcox test

#-------- BRUSHING --------
b_all <- list()
for(ii in 1:length(FiresOfInterest)){
  b_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="Brushed"]
  if(nrow(b_N[Brushed==1])>0){
    b_0 <- silv_csv[ID==FiresOfInterest[ii]& Brushed==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] & Brushed==0]),
                                                                b_N[Brushed==1]$N), ]
    b_1 <- silv_csv[ID==FiresOfInterest[ii] & Brushed==1]
  }
  b_all[[ii]] <- rbind(b_0[,.(dNBR,Brushed,FireName,ID)],b_1[,.(dNBR,Brushed,FireName,ID)])
}
b_all <- do.call(rbind,b_all)
b_all[,Brushed := as.factor(Brushed)]
b_all[, shapiro.test(dNBR), by=c("FireName","Brushed")]
#distributions pretty much non-normal, so non-parametric Wilcox test

#-------- SPACING --------
s_all <- list()
for(ii in 1:length(FiresOfInterest)){
  s_N <- silv_csv[ID==FiresOfInterest[ii],.N,by="Spaced"]
  if(nrow(s_N[Spaced==1])>0){
    s_0 <- silv_csv[ID==FiresOfInterest[ii]& Spaced==0][sample(nrow(silv_csv[ID==FiresOfInterest[ii] & Spaced==0]),
                                                               s_N[Spaced==1]$N), ]
    s_1 <- silv_csv[ID==FiresOfInterest[ii] & Spaced==1]
  }
  s_all[[ii]] <- rbind(s_0[,.(dNBR,Spaced,FireName,ID)],s_1[,.(dNBR,Spaced,FireName,ID)])
}
s_all <- do.call(rbind,s_all)
s_all[,Spaced := as.factor(Spaced)]
s_all[, shapiro.test(dNBR), by=c("FireName","Spaced")]
#distributions pretty much non-normal, so non-parametric Wilcox test



#------------------------------ 3. Analyse and plot  ------------------------------#

#-------- BROADCAST BURNING --------
bb_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(bb_all[ID==FiresOfInterest[[j]]])>0){
    bb_WT <- wilcox.test(dNBR ~ BroadBurn, data=bb_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = bb_WT$statistic, p =  bb_WT$p.value, 
                      ID = unique(bb_all[ID==FiresOfInterest[[j]]]$FireName))
    bb_WT_dt <- rbind(bb_WT_dt,wdt)
  }
}
ggplot(bb_all)+
  geom_boxplot(aes(x=BroadBurn, y= dNBR, group=BroadBurn, fill = BroadBurn))+
  xlab("Broadcast burning")+
  theme_minimal()+
  scale_fill_manual(values= paper_pal[1:2])+
  scale_colour_manual(values= paper_pal[1:2])+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none")+
  facet_wrap("FireName")
ggsave(filename = "Fig5a.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#Chutanli, Verdun, Island, Nadina sig diff, Tezzeron and Shovel not.
bb_all[,.N, by=c("FireName","BroadBurn")]

#-------- DISC TRENCHING --------

d_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(d_all[ID==FiresOfInterest[[j]]])>0){
    d_WT <- wilcox.test(dNBR ~ Disc, data=d_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = d_WT$statistic,p =  d_WT$p.value, 
                      ID = unique(d_all[ID==FiresOfInterest[[j]]]$FireName))
    d_WT_dt <- rbind(d_WT_dt,wdt)
  }
}
ggplot(d_all)+
  geom_boxplot(aes(x=Disc, y= dNBR, group=Disc, fill = Disc))+
  xlab("Disc trenching")+
  theme_minimal()+
  scale_fill_manual(values= paper_pal[1:2])+
  scale_colour_manual(values= paper_pal[1:2])+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none")+
  facet_wrap("FireName")
ggsave(filename = "Fig5b.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#Chutanli and Nadina significant decline in dNBR, others no difference
d_all[,.N, by=c("FireName","Disc")]

#-------- BRUSHING --------

b_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(b_all[ID==FiresOfInterest[[j]]])>0){
    b_WT <- wilcox.test(dNBR ~ Brushed, data=b_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = b_WT$statistic,p =  b_WT$p.value, 
                      ID = unique(b_all[ID==FiresOfInterest[[j]]]$FireName))
    b_WT_dt <- rbind(b_WT_dt,wdt)
  }
}
ggplot(b_all)+
  geom_boxplot(aes(x=Brushed, y= dNBR, group=Brushed, fill = Brushed))+
  xlab("Brushing")+
  theme_minimal()+
  scale_fill_manual(values= paper_pal[1:2])+
  scale_colour_manual(values= paper_pal[1:2])+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none")+
  facet_wrap("FireName")
ggsave(filename = "Fig5c.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#All fires except Shovel had significant decline in dNBR
b_all[,.N, by=c("FireName","Brushed")]

#-------- SPACING --------

s_WT_dt <- data.table()
for(j in 1:length(FiresOfInterest)){
  if(nrow(s_all[ID==FiresOfInterest[[j]]])>0){
    s_WT <- wilcox.test(dNBR ~ Spaced, data=s_all[ID==FiresOfInterest[[j]]],
                        paired=FALSE, exact=FALSE, conf.int=TRUE)
    wdt <- data.table(W = s_WT$statistic,p =  s_WT$p.value, 
                      ID = unique(s_all[ID==FiresOfInterest[[j]]]$FireName))
    s_WT_dt <- rbind(s_WT_dt,wdt)
  }
}
ggplot(s_all)+
  geom_boxplot(aes(x=Spaced, y= dNBR, group=Spaced, fill = Spaced))+
  xlab("Spacing")+
  theme_minimal()+
  scale_fill_manual(values= paper_pal[1:2])+
  scale_colour_manual(values= paper_pal[1:2])+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=21))+
  theme(legend.position = "none")+
  facet_wrap("FireName")
ggsave(filename = "Fig5d.jpg",path = "./Outputs/Figures/", device='jpeg', dpi=300, bg="white")
#Shovel had a higher dNBR with spacing, Chutanli, Verdun, Nadina and Island all had significant decline in dNBR
s_all[,.N, by=c("FireName","Spaced")]

