#Fire history reconstruction using burnr and FHAES
#K. Hoffman
#Jan/Feb 2022


# Download the library from cran
install.packages("burnr")

# load the library
library(burnr)
library(ggplot2)


# FHX2 files stored on the hard drive:

tes1<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla1c.fhx") 
tes2<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 2.fhx") 
tes3<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 3.fhx") 
tes4<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 4.fhx") 
tes5<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 5.fhx") 
tes6<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 6.fhx") 
tes7<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 7.fhx") 
tes8<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 8.fhx") 
tes9<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 9.fhx") 
tes10<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 10.fhx") 
tes11<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 11.fhx") 

#' These data can be viewed in several ways.
View(tes1)
head(tes)

# tree-level summaries are provided by
head(series_stats(tes1))

tes_stats<-(series_stats(tes1))
#' series (tree) names are provided by
series_names(tes1) 

#' FHX objects are rather simple, with 3 columns for "year", "series", and "rec_type"
#' The "rec_type" is a catch-all for the tree-ring attribute for a given year/series
#' There are currently 19 rec_type levels
levels(tes$rec_type)


#' Combine all three sites into a single fhx object, and save the new FHX2 file
tes_all <- tes1c + tes2 + tes3 + tes4 + tes5 + tes6 + tes7 + tes8 + tes9 + tes10 + tes11
write_fhx(tes_all,"/Users/Kira/Desktop/BVRCfire/Tesla/Output/Tesla_allSites.fhx")


tes_all<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Output/Tesla_allSites.fhx") 

# Basic plotting ----------------------------------------------------------

data(tes_all)

plot(tes_all)
tes_all


# try a different sorting
tes_all <- sort(tes_all, decreasing = FALSE, sort_by = 'first_year')

plot(tes_all, ylabels = FALSE)

# add a composite rug to the plot
plot(tes_all, ylabels=FALSE, composite_rug=TRUE)

# add a legend
plot(tes_all, ylabels=FALSE, composite_rug=TRUE, plot_legend = TRUE)


# Color by species using a separate metadata file

Tes_meta <-read.csv("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tes_meta.csv") 
Tes_meta

head(Tes_meta)


plot(tes_all, color_group = Tes_meta$SpeciesID, color_id = Tes_meta$TreeID,
     plot_legend = TRUE, ylabels=FALSE)


#' far more functionality using ggplot options with plot_demograph() 
library(ggplot2)


p <- plot_demograph(tes_all, color_group = Tes_meta$SpeciesID, color_id = Tes_meta$TreeID,
                    plot_legend = TRUE, ylabels=FALSE)
p

p <- plot_demograph(tes_all, color = Tes_meta$SpeciesID, color_id = Tes_meta$TreeID,
                    plot_legend = TRUE, ylabels=FALSE)
p
#' Change colors
levels(Tes_meta$SpeciesID)

cols <- c('purple','blue', 'green4')

levels

p + scale_color_manual(values = cols)

# Annotate patterns

p + scale_color_manual(values = cols) +
  annotate('rect', xmin = 1200, xmax = 2020, ymin = 0, ymax = 45, alpha = .3,
           fill = 'blue2')


# FHAES-style graph -------------------------------------------------------

library(burnr)
library(ggpubr)

library(ggrepel) # For labeling
library(scales)


#' Start with percent trees scarred timeseries

tes_perc <- percent_scarred(tes_all)

# add year labels to identify certain fire events, based on filtering
yr_labs <- tes_perc[tes_perc$num_scars > 1 & tes_perc$percent_scarred >= 10, ] 
wide_labs <- tes_perc[tes_perc$num_scars > 1 & tes_perc$percent_scarred >= 25, ] 

# The graph is actually 3 mashed-together plots, each one needs to be drawn separately

p <- ggplot() + 
  geom_col(data=tes_perc, aes(x=year, y=percent_scarred)) +
  xlim(1200, 2020) +
  geom_text(data=wide_labs, aes(x=year, y=percent_scarred+5, label=year, angle=35),
            size=2, nudge_x = 5) +
  scale_y_continuous(name = "% trees\nscarred", limits=c(0, 100), expand=c(0, 0)) +
  theme_bw() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

f <- plot_demograph(tes_all, yearlims = c(1150, 2020), composite_rug = TRUE,
                    filter_prop = 0.25, filter_min_events = 2, ylabels = FALSE,
                    plot_legend = TRUE) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = c(.15, .75))

l <- ggplot() +
  geom_text_repel(data=yr_labs, aes(x=year, 
                                    y=rep(1, nrow(yr_labs)),
                                    label = year, angle = -85), size=2.5,
                  direction = "y", segment.size = 0.5, segment.alpha=.5,
                  force=10) +
  scale_x_continuous(name="Year", limits = c(1150, 2020), breaks = seq(1100, 2000, 100)) +
  ylab("") + xlab("Year") +
  scale_y_continuous(limits=c(0, 1), expand=c(0, 0), breaks = c(0, 1), labels = NULL, 
                     minor_breaks = NULL, name = "\n\nComposite\nfire years") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        panel.background = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.grid.major.x = element_line(linetype=1, color="grey90", size=.5),
        axis.line.x.bottom = element_line())

# Stack them up, with adjustable heights
# Then add annotation

fh <- ggarrange(p, f, l, nrow=3, align='v', heights=c(1, 6, 1))
annotate_figure(fh, top=text_grob("Tesla Sites", face='bold', size=14),
                left = text_grob("Fire-scarred\ntrees", size=11, rot=90, x=1.75, y=0.6, hjust=0.5))

ggsave('Output/FHAES-style.tiff', width=8.5, height=6, units='in', dpi=150, device = 'tiff')

#########################
#for making site specific data
#having trouble with some of these categoies!

# FHX2 files 

tes1<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla1c.fhx") 
tes2<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 2.fhx") 
tes3<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 3.fhx") 
tes4<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 4.fhx") 
tes5<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 5.fhx") 
tes6<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 6.fhx") 
tes7<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 7.fhx") 
tes8<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 8.fhx") 
tes9<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 9.fhx") 
tes10<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 10.fhx") 
tes11<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tesla 11.fhx") 


#' Read in associated metadata
Tes_meta <-read.csv("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tes_meta.csv") 

#' sort objects
tes1 <- sort(tes1c, decreasing=FALSE, sort_by = "first_year")
tes2 <- sort(tes2, decreasing=FALSE, sort_by = "first_year")
tes3 <- sort(tes3, decreasing=FALSE, sort_by = "first_year")
tes4 <- sort(tes4, decreasing=FALSE, sort_by = "first_year")
tes5 <- sort(tes5, decreasing=FALSE, sort_by = "first_year")
tes6 <- sort(tes6, decreasing=FALSE, sort_by = "first_year")
tes7 <- sort(tes7, decreasing=FALSE, sort_by = "first_year")
tes8 <- sort(tes8, decreasing=FALSE, sort_by = "first_year")
tes9 <- sort(tes9, decreasing=FALSE, sort_by = "first_year")
tes10 <- sort(tes10, decreasing=FALSE, sort_by = "first_year")
tes11 <- sort(tes11, decreasing=FALSE, sort_by = "first_year")

#' combine files
all.fhx <- tes1 + tes2 + tes3 + tes4 + tes5 + tes6 + tes7 + tes8 + tes9 + tes10 + tes11
write_fhx(all.fhx, "/Users/Kira/Desktop/BVRCfire/Tesla/Data/all.fhx") 


all<- read_fhx("/Users/Kira/Desktop/BVRCfire/Tesla/Data/all.fhx") 
all.fhx

# Identify sites for faceting and filter metadata
Tes_meta$SiteID <- substr(Tes_meta$TreeID, start=1, stop=3)
Tes_meta <- Tes_meta[Tes_meta$TreeID %in% series_names(all.fhx), ]

#' Create site composites
tes1.comp <- composite(tes1, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes1c")
tes2.comp <- composite(tes2, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes2")
tes3.comp <- composite(tes3, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes3")
tes4.comp <- composite(tes4, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes4")
tes5.comp <- composite(tes5, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes5")
tes6.comp <- composite(tes6, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes6")
tes7.comp <- composite(tes7, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes7")
tes8.comp <- composite(tes8, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes8")
tes9.comp <- composite(tes9, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes9")
tes10.comp <- composite(tes10, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes10")
tes11.comp <- composite(tes11, filter_min_rec = 2, filter_min_events = 2, filter_prop = .10, injury_event = TRUE, comp_name = "tes11")

#' combine composites
all.comp <- tes1.comp + tes2.comp + tes3.comp + tes4.comp + tes5.comp + tes6.comp + tes7.comp + tes8.comp + tes9.comp + tes10.comp + tes11.comp

all <- all.fhx + all.comp

#' Add composite metadata to trees table
comps <- data.frame(series = c("tes1", "tes2", "tes3", "tes4", "tes5", "tes6", "tes7", "tes8", "tes9", "tes10", "tes11"),
                    SpeciesID = "Tes_meta", SamplePurpose = NA, Latitude = NA,
                    Longitude = NA, Easting = NA, Northing = NA, Elevation = NA)
comps
trees.all <- rbind(Tes_meta, comps)

#' Organize metadata, now that "COMP" is a Species ID
trees.all$SpeciesID <- factor(trees.all$SpeciesID)
levels(trees.all$SpeciesID)
#' Assign colors for each species
col.sel <- c("#8B5A2B", "#4DAF4A")
#' Organize the site levels for plotting
trees.all$SiteID <- factor(trees.all$SiteID, levels = c("tes1c", "tes2", "tes3", "tes4", "tes5", "tes6", "tes7","tes8", "tes9", "tes10", "tes11", "comp"))

#' Make the graph
p2 <- plot_demograph(all, color_group=trees.all$SpeciesID, color_id=trees.all$series,
                     facet_group=trees.all$SiteID, facet_id=trees.all$series,
                     event_size = c(1.75,1,1), #size of fs, inj, pith/bark
                     composite_rug=FALSE, rugdivide_pos = 0, #plot composite = YES and location
                     plot_legend = TRUE, ylabels=FALSE, #no treeID labels
                     yearlims= c(1200,2020),
                     facet_type='grid')
p2 + scale_color_manual(values=col.sel, breaks = c("PIAL", "PICO", "ABLA")) + 
  guides(linetype = "none", shape = "none", size = "none") +
  theme(legend.justification = "center", legend.position = "top",
        legend.direction="horizontal", legend.background=element_rect(fill='white'),
        legend.margin=margin(0, unit="pt"),
        strip.text.y = element_text(size = 6)) +
  scale_x_continuous(sec.axis = dup_axis())

# If you'd like to save the graph
ggsave("/Users/Kira/Desktop/BVRCfire/Tesla/Output/all.fhx", device="tiff", width=4, height=6, dpi=300)



# Intervals analysis -------------------------------------------------------------

# See some basic stats for each tree (series)
head(series_stats(tes_all))

#' Site-level analyses generally require compositing the tree-level data to generate a 
#' single site fire history, or "master chronology"
#' Burnr allows for a multitude of filtering methods in composite()

# Create a composite with all scars
tes.comp <- composite(tes_all, filter_min_events = 1, filter_prop = 0, filter_min_rec = 1,
                      comp_name = 'tes_all')
plot(tes.comp)

## Interval analysis on the composite
intervals(tes.comp)

# Note that the warnings are generated by the curve-fitting algorithms (MASS::fitdistr)
# Composite name: tes_all
#Events range: 1377 to 1957

#Total intervals: 49
#Mean interval: 11.8
#Median interval: 5
#Weibull median: 5.5
#Standard deviation: 29.7
#Minimum interval: 1
#Maximum interval: 203

#' The default theoretical fit is a weibull, but log-normal is also available
intervals(tes.comp, densfun = "lognormal")

# Make it an intervals object and plot the data
tes_interv <- intervals(tes.comp)

plot(tes_interv, binwidth=5)

boxplot(tes_interv$intervals)


# Superposed Epoch Analysis -----------------------------------------------

# Read in PDSI timeseries for tesla

tes_pdsi <-read.csv("/Users/Kira/Desktop/BVRCfire/Tesla/Data/Tes_pdsi.csv") 

tes_pdsi
#' This PDSI timeseries came from the North American Drought Atlas
#' https://iridl.ldeo.columbia.edu/SOURCES/.LDEO/.TRL/.NADA2004/.pdsi-atlas.html

head(tes_pdsi)

#' Run SEA with the sea function, and make an object for later visuals
tes.sea <- sea(x = tes_pdsi, event = tes.comp, nbefore = 4, nafter = 2)

#' Basic (and FAST) graphic
plot(tes.sea)

source("R/SEA_Helper_FUNS.R")

pgm_sea_sig <- sea_sig(pgm.sea$departure)

ggplot(pgm_sea_sig, aes(x=lag)) + geom_col(aes(y=mean, fill=sig), width=.7) +
  scale_fill_manual(values=sea_cols, guide="none") + 
  scale_x_continuous(name="Lag year", breaks=seq(-4, 2, 1), labels=seq(-4, 2, 1)) +
  ylab("PDSI departure") +
  geom_line(aes(y=upper_95_perc)) + geom_line(aes(y=lower_95_perc)) + 
  geom_line(aes(y=upper_99_perc), linetype=3) + geom_line(aes(y=lower_99_perc), linetype=3) +
  ylim(-2, 2) + theme_bw() +
  theme(panel.grid.minor=element_blank(), axis.text = element_text(size=16), axis.title=element_text(size=18),
        strip.text = element_text(size=11), plot.title=element_text(size=18))
ggsave("Output/TES_SEA.tiff", device="tiff", width=4, height=4, dpi=300)


## Some useful functions for SEA graphics
require(dplyr)

n_events <- function(df){
  n.val <- nrow(df)
  out <- substitute(italic(n)~"="~n.val,
                    list(n.val = format(n.val)))
  as.character(as.expression(out))
}

sea_sig <- function(x){
  out <- x %>% mutate(sig = if_else(mean <= lower_95_perc, "sig_neg",
                                    if_else(mean > lower_95_perc & mean <=0, "neg",
                                            if_else(mean > 0 & mean < upper_95_perc, "pos", "sig_pos"))))
  out$sig <- factor(out$sig, levels=c("sig_neg", "neg", "pos", "sig_pos"))
  return(out)
}

sea_cols <- c('#ca0020', '#f4a582', '#92c5de', '#0571b0')
names(sea_cols) <- c("sig_neg", "neg", "pos", "sig_pos")


