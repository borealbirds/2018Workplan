#Startup

# Packages
require(ggplot2)
require(reshape)
require(knitr)
require(xts)


# Functions
myfacettheme <- theme_grey() + 
  theme(text=element_text(size=12), 
        axis.ticks = element_line(colour = "black"),             
        axis.text = element_text(size=12),
        axis.title.y = element_text(angle=90, vjust=1),
        axis.text.x = element_text(size=11, angle=90, hjust=1,vjust=0.5),
        
        legend.key = element_blank(), 
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        
        panel.background = element_rect(fill="white", colour=NA), 
        panel.border = element_rect(fill = NA, colour = "grey50"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_line(colour="grey98", size=0.5), 
        
        strip.background = element_rect(fill="grey80", colour="grey50"),
        strip.text.x= element_text(size=11)
  ) 

namean <- function(x) {mean(x, na.rm=T)}
nasd <- function(x) {sd(x, na.rm=T)}