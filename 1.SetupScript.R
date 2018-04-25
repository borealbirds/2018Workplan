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


F.hierarcplot <- function(dat,
                          col1L = 0, 
                          col2L = 0.1, 
                          col3L = 0.2, 
                          col4L = 0.35,
                          col5L = 0.6,
                          col5R = 1,
                          wrapsize1=9,
                          wrapsize2=12,
                          wrapsize3=15,
                          wrapsize4=35,
                          wrapsize5=45,
                          fontsize1=1, 
                          fontsize2=1,
                          fontsize3=1,
                          fontsize4=0.75,
                          fontsize5=0.5,
                          domcol="DomainColour", 
                          thmcol="DomainColour", 
                          cluscol="white", 
                          projcol="ProximityColour", 
                          detcol="StatusColour",
                          numcols=5, 
                          statustoinclude=c("STALLED", "COMPLETE", "ABANDONED", "IDEA", "ACTIVE", "MERGED"))

{
  # dat is a data frame containing the hierarchically-organized information. 
  # column coordinates are set  based on expected content. Default values are what last worked for mine. 
  # note this function is written for 4 or 5 columns and isn't flexible to other options.
  
  ### Subset for status I care about
  
  dat <- dat[dat$projectSTATUS %in% statustoinclude,]
  
  
  #PROJECT COLUMN
  # determine number of rows and sizes for project column
  projcoor <- data.frame(L=rep(col4L, times=nrow(dat)),
                         R=rep(col5L, times=nrow(dat)),
                         B=rep(NA, times=nrow(dat)),
                         T=rep(NA, times=nrow(dat)))
  
  for (i in 1:nrow(projcoor)) {
    projcoor[i,3] <- 1 - (i / nrow(projcoor)) # bottom coordinate
    projcoor[i,4] <- 1 - ((i-1)/nrow(projcoor)) # top coordinate
  }
  
  # determine number of rows and sizes for project detail column
  detcoor <- data.frame(L=rep(col5L, times=nrow(dat)),
                        R=rep(col5R, times=nrow(dat)),
                        B=rep(NA, times=nrow(dat)),
                        T=rep(NA, times=nrow(dat)))
  
  for (i in 1:nrow(detcoor)) {
    detcoor[i,3] <- 1 - (i / nrow(detcoor)) # bottom coordinate
    detcoor[i,4] <- 1 - ((i-1)/nrow(detcoor)) # top coordinate
  }
  
  
  # determine number of rows and sizes for  cluster column
  uniqueclus <- length(unique(dat$clusterNAME))
  cluscoor <- data.frame(L=rep(col3L, times=uniqueclus),
                         R=rep(col4L, times=uniqueclus),
                         B=rep(NA, times=uniqueclus),
                         T=rep(NA, times=uniqueclus))
  
  for (i in 1:nrow(cluscoor)) {
    if (i == 1) {
      nproj.in.prevclus <- 0
      ithclus <- unique(dat$clusterNAME)[i]
      nproj.in.ithclus <-  length(dat$PROJECTname[dat$clusterNAME %in% ithclus])
    } else {
      prevclus <- unique(dat$clusterNAME)[1:(i-1)] # clusters previously populated 
      nproj.in.prevclus <-  length(dat$PROJECTname[dat$clusterNAME %in% prevclus]) # number of projects within previously populated clusters
      
      ithclus <- unique(dat$clusterNAME)[i] # cluster to populate on ith iteration
      nproj.in.ithclus <- length(dat$PROJECTname[dat$clusterNAME %in% ithclus]) # number of projects within this cluster
    }
    
    cluscoor[i,3] <- 1 - ((nproj.in.ithclus + nproj.in.prevclus) / nrow(dat)) # bottom coordinate
    
    cluscoor[i,4] <- 1 - (nproj.in.prevclus/nrow(dat))  # top coordinate
  }
  
  # determine number of rows and sizes for  theme column
  uniquetheme <- length(unique(dat$themeNAME))
  themecoor <- data.frame(L=rep(col2L, times=uniquetheme),
                          R=rep(col3L, times=uniquetheme),
                          B=rep(NA, times=uniquetheme),
                          T=rep(NA, times=uniquetheme))
  
  for (i in 1:nrow(themecoor)) {
    if (i == 1) {
      nproj.in.prevtheme <- 0
      iththeme <- unique(dat$themeNAME)[i]
      nproj.in.iththeme <-  length(dat$PROJECTname[dat$themeNAME %in% iththeme])
    } else {
      prevtheme <- unique(dat$themeNAME)[1:(i-1)] # themes previously populated 
      nproj.in.prevtheme <-  length(dat$PROJECTname[dat$themeNAME %in% prevtheme]) # number of projects within previously populated themes
      
      iththeme <- unique(dat$themeNAME)[i] # cluster to populate on ith iteration
      nproj.in.iththeme <- length(dat$PROJECTname[dat$themeNAME %in% iththeme]) # number of projects within this theme
    }
    
    themecoor[i,3] <- 1 - ((nproj.in.iththeme + nproj.in.prevtheme) / nrow(dat)) # bottom coordinate
    
    themecoor[i,4] <- 1 - (nproj.in.prevtheme/nrow(dat))  # top coordinate
  }
  
  # determine number of rows and sizes for  domain column
  uniquedomain <- length(unique(dat$domainNAME))
  domaincoor <- data.frame(L=rep(col1L, times=uniquedomain),
                           R=rep(col2L, times=uniquedomain),
                           B=rep(NA, times=uniquedomain),
                           T=rep(NA, times=uniquedomain))
  
  for (i in 1:nrow(domaincoor)) {
    if (i == 1) {
      nproj.in.prevdomain <- 0
      ithdomain <- unique(dat$domainNAME)[i]
      nproj.in.ithdomain <-  length(dat$PROJECTname[dat$domainNAME %in% ithdomain])
    } else {
      prevdomain <- unique(dat$domainNAME)[1:(i-1)] # themes previously populated 
      nproj.in.prevdomain <-  length(dat$PROJECTname[dat$domainNAME %in% prevdomain]) # number of projects within previously populated domains
      
      ithdomain <- unique(dat$domainNAME)[i] # cluster to populate on ith iteration
      nproj.in.ithdomain <- length(dat$PROJECTname[dat$domainNAME %in% ithdomain]) # number of projects within this domain
    }
    
    domaincoor[i,3] <- 1 - ((nproj.in.ithdomain + nproj.in.prevdomain) / nrow(dat)) # bottom coordinate
    
    domaincoor[i,4] <- 1 - (nproj.in.prevdomain/nrow(dat))  # top coordinate
  }
  
  # COMBINE ALL COORDINATES INTO ONE BIG MATRIX.
  
  if (numcols == 4) {
    allcoor <- rbind(themecoor, cluscoor, projcoor, detcoor)
    
    
    texttoprint <- c(as.character(dat$themeNAME[!duplicated(dat$themeNAME)]),
                     as.character(dat$clusterNAME[!duplicated(dat$clusterNAME)]),
                     as.character(dat$PROJECTname),
                     as.character(dat$PROJECTpurpose))
    
    wraplength <- c(rep(wrapsize2, times=length(unique(dat$themeNAME))), #theme
                    rep(wrapsize3, times=length(unique(dat$clusterNAME))), #cluster
                    rep(wrapsize4, times=length(dat$PROJECTname)), #project
                    rep(wrapsize5, times=length(dat$PROJECTpurpose))) #purpose
    
    fontsize <- c(rep(fontsize2, times=length(unique(dat$themeNAME))), #theme
                  rep(fontsize3, times=length(unique(dat$clusterNAME))), #cluster
                  rep(fontsize4, times=length(dat$PROJECTname)), #project
                  rep(fontsize5, times=length(dat$PROJECTpurpose))) #purpose
    
    thmcoldf <- data.frame(Theme=dat$themeNAME, ThemeColour=dat[[thmcol]])
    thmcoldf <- thmcoldf[!duplicated(thmcoldf$Theme),]
    
    bgcolour <- c(as.character(thmcoldf$ThemeColour), #theme
                  rep(cluscol, times=length(unique(dat$clusterNAME))), #cluster
                  as.character(dat[[projcol]]), #project
                  as.character(dat[[detcol]])) #purpose
    
  } 
  
  if (numcols == 5){
    allcoor <- rbind(domaincoor, themecoor, cluscoor, projcoor, detcoor)  
    
    texttoprint <- c(as.character(dat$domainNAME[!duplicated(dat$domainNAME)]),
                     as.character(dat$themeNAME[!duplicated(dat$themeNAME)]),
                     as.character(dat$clusterNAME[!duplicated(dat$clusterNAME)]),
                     as.character(dat$PROJECTname),
                     as.character(dat$PROJECTpurpose))
    
    wraplength <- c(rep(wrapsize1, times=length(unique(dat$domainNAME))), # domain
                    rep(wrapsize2, times=length(unique(dat$themeNAME))), #theme
                    rep(wrapsize3, times=length(unique(dat$clusterNAME))), #cluster
                    rep(wrapsize4, times=length(dat$PROJECTname)), #project
                    rep(wrapsize5, times=length(dat$PROJECTpurpose))) #purpose
    
    fontsize <- c(rep(fontsize1, times=length(unique(dat$domainNAME))), # domain
                  rep(fontsize2, times=length(unique(dat$themeNAME))), #theme
                  rep(fontsize3, times=length(unique(dat$clusterNAME))), #cluster
                  rep(fontsize4, times=length(dat$PROJECTname)), #project
                  rep(fontsize5, times=length(dat$PROJECTpurpose))) #purpose
    
    domcoldf <- data.frame(Domain=dat$domainNAME, DomainColour=dat[[domcol]])
    domcoldf <- domcoldf[!duplicated(domcoldf$Domain),]
    
    thmcoldf <- data.frame(Theme=dat$themeNAME, ThemeColour=dat[[thmcol]])
    thmcoldf <- thmcoldf[!duplicated(thmcoldf$Theme),]
    
    bgcolour <- c(as.character(domcoldf$DomainColour), #domain
                  as.character(thmcoldf$ThemeColour), #theme
                  rep(cluscol, times=length(unique(dat$clusterNAME))), #cluster
                  as.character(dat[[projcol]]), #project
                  as.character(dat[[detcol]])) #purpose
    
  }
  
  split.screen(as.matrix(allcoor))
  
  for(i in 1:nrow(allcoor)) {
    par(bg=bgcolour[i])
    screen(i)
    par(mar = c(0, 0, 0, 0))
    text(.5,.5,paste(strwrap(texttoprint[i],wraplength[i]), collapse="\n"), cex=fontsize[i])
    box()
    par(bg="white")
  }
  
  close.screen(all.screens = TRUE)
}

