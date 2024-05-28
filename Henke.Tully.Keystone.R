# FINAL PROJECT 
# TULLY HENKE
# DECEMBER 19, 2018
# STATS W/ R - VEEN

# NOTE: On PC collapse all with Alt+O and expand all with Alt+Shift+O

## VERSION AND LIBRARIES ## ----

R.version.string
  # "R version 3.5.1 (2018-07-02)"
library(vegan)
library(lme4)

## LOADING DATA AND DETERMINING RESPONSE VARIABLES ## ----
# All data
all.data.t <- read.csv("Required Files/Keystone.All.Data.Test.csv")
head(all.data.t)
str(all.data.t)
# Filling in gaps in species data with zeros
head(all.data.t)
all.data.t[is.na(all.data.t)] <- 0

# Check that it worked
head(all.data.t)

## Defining a response gradient ## ====
# Designate a vector for site names
site.names <- c("TE","BR","ET","GT","TM","RM")

# Boxplots 
{
        # Richness
        par(mfrow=c(2,3))
        for (i in 1:length(site.names)){
          plot(all.data.t$Dist..Trail[all.data.t$Location==site.names[i]], all.data.t$Richness[all.data.t$Location==site.names[i]], xlab="Distance from trail", ylab="Richness")
        }

        # Percent Cover
        par(mfrow=c(2,3))
        for (i in 1:length(site.names)){
          plot(all.data.t$Dist..Trail[all.data.t$Location==site.names[i]], all.data.t$X.Cover..Total.Veg.[all.data.t$Location==site.names[i]], xlab="Distance from trail", ylab="Percent Cover")
        }
        
        # Richness * Percent Cover
        par(mfrow=c(2,3))
        for (i in 1:length(site.names)){
          plot(all.data.t$Dist..Trail[all.data.t$Location==site.names[i]], all.data.t$X.Cover..Total.Veg.[all.data.t$Location==site.names[i]]*all.data.t$Richness[all.data.t$Location==site.names[i]], xlab="Distance from trail", ylab="Richness * Percent Cover")
        }
}

## NMDS ## ---- 

# Loading data
  # Species file
  NMDS.spp.2 <- read.csv("Required Files/Keystone.NMDS.Data.Test.Spp.2.csv")
  # Site characteristic file
  NMDS.sites <- read.csv("Required Files/Keystone.NMDS.Data.Test.Sites.csv")

# Filling in NA values in species data with zeros
  head(NMDS.spp.2)
  # Assign NA to 0
  NMDS.spp.2[is.na(NMDS.spp.2)] <- 0

# Check that it worked
  head(NMDS.spp.2)

# Run NMDS analysis using "metaMDS" from "vegan" library
  NMDS.results.2 <- metaMDS(NMDS.spp.2, trymax = 1000, k = 3)

# Generate a stressplot of NMDS results
  par(mfrow=c(1,1))
  stressplot(NMDS.results.2)

# Pull the NMDS points out of the results from "metaMDS"
  nmds_pts.2<-as.data.frame(NMDS.results.2$points)
  write.table(nmds_pts.2, "nmds_pts.2.csv")

# Create a new object that you can add back site characteristics to that contains the values from the saved NMDS table
  nmds_info.2<-read.table("nmds_pts.2.csv")
  nmds_info.2

# Add column with site info
  new.row.t <- read.csv("Required Files/Keystone.NMDS.Data.Test.Site.Vect.csv")
  new.row.t
  # Add site column
  nmds_info.2$site <- new.row.t
  # Check
  head(nmds_info.2)

# Make a site specific ordination plot
  par(mfrow=c(1,1))
  {
  plot(nmds_info.2$MDS2[nmds_info.2$site == "TE"] ~ nmds_info.2$MDS1[nmds_info.2$site == "TE"], pch = 19, col = "red", xlim = c(-1.5, 1.4), ylim = c(-1.2, 1.2), xlab = "NMDS1", ylab = "NMDS2")
  par(new=T)
  plot(nmds_info.2$MDS2[nmds_info.2$site == "BR"] ~ nmds_info.2$MDS1[nmds_info.2$site == "BR"], pch = 19, col = "blue", xlim = c(-1.5, 1.4), ylim = c(-1.2, 1.2), xlab = "", ylab = "")
  par(new = T)
  plot(nmds_info.2$MDS2[nmds_info.2$site == "ET"] ~ nmds_info.2$MDS1[nmds_info.2$site == "ET"], pch = 19, col = "black", xlim = c(-1.5, 1.4), ylim = c(-1.2, 1.2), xlab = "", ylab = "")
  par(new = T)
  plot(nmds_info.2$MDS2[nmds_info.2$site == "GT"] ~ nmds_info.2$MDS1[nmds_info.2$site == "GT"], pch = 19, col = "green", xlim = c(-1.5, 1.4), ylim = c(-1.2, 1.2), xlab = "", ylab = "")
  par(new = T)
  plot(nmds_info.2$MDS2[nmds_info.2$site == "TM"] ~ nmds_info.2$MDS1[nmds_info.2$site == "TM"], pch = 2, col = "blue", xlim = c(-1.5, 1.4), ylim = c(-1.2, 1.2), xlab = "", ylab = "")
  par(new = T)
  plot(nmds_info.2$MDS2[nmds_info.2$site == "RM"] ~ nmds_info.2$MDS1[nmds_info.2$site == "RM"], pch = 2, col = "red", xlim = c(-1.5, 1.4), ylim = c(-1.2, 1.2), xlab = "", ylab = "")
  par(new = T)
  }
  # Add legend
  legend(0.9, 0.7, c("TE","BR","ET","GT","TM","RM"), pch = c(19, 19, 19, 19, 2, 2), col = c("red", "blue", "black", "green", "blue", "red"), cex=1.2)

  
  
## DATA SIMULATION ## ----
  # Assign site characteristic column values
  sim.loc <- c(rep("P",128),rep("S",128),rep("W",128))
  sim.site <- c(rep("TE",64),rep("BR",64),rep("ET",64),rep("GT",64),rep("TM",64),rep("RM",64)) 
  sim.use <- c(rep("HIKE",64),rep("BIKE",64),rep("BIKE",64),rep("HIKE",64),rep("HIKE",64),rep("BIKE",64))
  sim.distt <- c(rep(c(1,2,3,4),96))
  sim.elev <- all.data.t$Elevation
  sim.transect <- c(rep(c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12,-1,-1,-1,-1,-2,-2,-2,-2,-3,-3,-3,-3),6))
  
  # Create data frame to input simulated response data
  sim.data <- data.frame(sim.loc, sim.site, sim.use, sim.distt, sim.elev, sim.transect)
  head(sim.data)
          
  # Define response variables that correlate specific with different site characteristics
    {
          # Richness as function of elevation increasing from trail edge
          sim.richness <- sim.distt*(sim.elev/1000) + rnorm(n = 384,mean = 1,sd = 0.1)
          
          # Richness slope dependent on elevation
          sim.richness.2 <- sim.distt*(sim.elev/c(5:388)) + rnorm(n = 384,mean = 1,sd = 0.1)
          
          # Slope dependent on use
          bike.hike.binary <- as.numeric(as.factor(sim.data$sim.use))
          sim.richness.3 <- sim.distt*(sim.elev/1000) + bike.hike.binary*3 + rnorm(n = 384,mean = 1,sd = 0.1)
          
          # Richness slope dependent on use w/ interaction
          bike.hike.binary.2 <- replace(bike.hike.binary, bike.hike.binary==2, -1)
          bike.hike.binary.2
          
          sim.richness.4 <- sim.distt*(sim.elev/(bike.hike.binary.2*1000)) + 10 + rnorm(n = 384,mean = 1,sd = 0.1)
      }
  
  # Assign response variables to new columns
  
  sim.data$sim.richness <- sim.richness
  
  sim.data$sim.richness.2 <- sim.richness.2
  
  sim.data$sim.richness.3 <- sim.richness.3
  
  sim.data$sim.richness.4 <- sim.richness.4
  
  # Remove center of trail quadrats for analysis
  sim.data.no.c <- sim.data[-which(sim.data$sim.distt==1),]
  head(sim.data.no.c)
  
# Slopes ----
  
  # Define a vector to be used for the slope transect inputs
    # NOTE: These are used for each analysis so therefore slopes will be overwritten from one analysis to the next
  sim.te.slopes.t <- c(1:16)
  sim.br.slopes.t <- c(1:16)
  sim.et.slopes.t <- c(1:16)
  sim.gt.slopes.t <- c(1:16)
  sim.tm.slopes.t <- c(1:16)
  sim.rm.slopes.t <- c(1:16)

# sim.richness
  {  
    # TE ----
    for (j in -3:12){
      i <- 1
      # j <- 0
      slope.t <- lm(sim.data.no.c$sim.richness[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.te.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    # BR ----
    for (j in -3:12){
      i <- 2
      # j <- 12
      slope.t <- lm(sim.data.no.c$sim.richness[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.br.slopes.t[(j+4)] <- slope.t$coefficients[2]
    }
    
    # ET ----
    for (j in -3:12){
      i <- 3
      # j <- 1
      slope.t <- lm(sim.data.no.c$sim.richness[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.et.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    # GT ----
    for (j in -3:12){
      i <- 4
      # j <- 1
      slope.t <- lm(sim.data.no.c$sim.richness[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.gt.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    # TM ----
    for (j in -3:12){
      i <- 5
      # j <- 1
      slope.t <- lm(sim.data.no.c$sim.richness[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.tm.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    # RM ----
    for (j in -3:12){
      i <- 6
      # j <- 1
      slope.t <- lm(sim.data.no.c$sim.richness[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.rm.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    
    # Slopes ----
    sim.te.slopes.t
    sim.br.slopes.t
    sim.et.slopes.t
    sim.gt.slopes.t
    sim.tm.slopes.t
    sim.rm.slopes.t
    
    # #### SIMULATION PLOT SLOPES ####
    {
      par(mfrow=c(2,3))
      plot(sim.te.slopes.t[4:16])
      plot(sim.br.slopes.t[4:16])
      plot(sim.et.slopes.t[4:16])
      plot(sim.gt.slopes.t[4:16])
      plot(sim.tm.slopes.t[4:16])
      plot(sim.rm.slopes.t[4:16])
    }
    
  }
# sim.richness.2
  {  
  # TE ----
  for (j in -3:12){
    i <- 1
    # j <- 0
    slope.t <- lm(sim.data.no.c$sim.richness.2[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.te.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  # BR ----
  for (j in -3:12){
    i <- 2
    # j <- 12
    slope.t <- lm(sim.data.no.c$sim.richness.2[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.br.slopes.t[(j+4)] <- slope.t$coefficients[2]
  }
  
  # ET ----
  for (j in -3:12){
    i <- 3
    # j <- 1
    slope.t <- lm(sim.data.no.c$sim.richness.2[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.et.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  # GT ----
  for (j in -3:12){
    i <- 4
    # j <- 1
    slope.t <- lm(sim.data.no.c$sim.richness.2[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.gt.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  # TM ----
  for (j in -3:12){
    i <- 5
    # j <- 1
    slope.t <- lm(sim.data.no.c$sim.richness.2[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.tm.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  # RM ----
  for (j in -3:12){
    i <- 6
    # j <- 1
    slope.t <- lm(sim.data.no.c$sim.richness.2[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.rm.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  
  # Slopes ----
  sim.te.slopes.t
  sim.br.slopes.t
  sim.et.slopes.t
  sim.gt.slopes.t
  sim.tm.slopes.t
  sim.rm.slopes.t
  
  # #### SIMULATION PLOT SLOPES ####
  {
    par(mfrow=c(2,3))
    plot(sim.te.slopes.t[4:16])
    plot(sim.br.slopes.t[4:16])
    plot(sim.et.slopes.t[4:16])
    plot(sim.gt.slopes.t[4:16])
    plot(sim.tm.slopes.t[4:16])
    plot(sim.rm.slopes.t[4:16])
  }
  
  }
# sim.richness.3  
  {
  # Use Analysis ====
  # TE ----
  for (j in -3:12){
    i <- 1
    # j <- 0
    slope.t <- lm(sim.data.no.c$sim.richness.3[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.te.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  # BR ----
  for (j in -3:12){
    i <- 2
    # j <- 12
    slope.t <- lm(sim.data.no.c$sim.richness.3[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.br.slopes.t[(j+4)] <- slope.t$coefficients[2]
  }
  
  # ET ----
  for (j in -3:12){
    i <- 3
    # j <- 1
    slope.t <- lm(sim.data.no.c$sim.richness.3[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.et.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  # GT ----
  for (j in -3:12){
    i <- 4
    # j <- 1
    slope.t <- lm(sim.data.no.c$sim.richness.3[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.gt.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  # TM ----
  for (j in -3:12){
    i <- 5
    # j <- 1
    slope.t <- lm(sim.data.no.c$sim.richness.3[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.tm.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  # RM ----
  for (j in -3:12){
    i <- 6
    # j <- 1
    slope.t <- lm(sim.data.no.c$sim.richness.3[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
    sim.rm.slopes.t[j+4] <- slope.t$coefficients[2]
  }
  
  
  # Slopes ----
  sim.te.slopes.t
  sim.br.slopes.t
  sim.et.slopes.t
  sim.gt.slopes.t
  sim.tm.slopes.t
  sim.rm.slopes.t
  
  # #### SIMULATION PLOT SLOPES ####
  {
    par(mfrow=c(2,3))
    plot(sim.te.slopes.t[4:16])
    plot(sim.br.slopes.t[4:16])
    plot(sim.et.slopes.t[4:16])
    plot(sim.gt.slopes.t[4:16])
    plot(sim.tm.slopes.t[4:16])
    plot(sim.rm.slopes.t[4:16])
  }
  
  
  }
# sim.richness.4
  {
    # Use Analysis ====
    # TE ----
    for (j in -3:12){
      i <- 1
      # j <- 0
      slope.t <- lm(sim.data.no.c$sim.richness.4[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.te.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    # BR ----
    for (j in -3:12){
      i <- 2
      # j <- 12
      slope.t <- lm(sim.data.no.c$sim.richness.4[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.br.slopes.t[(j+4)] <- slope.t$coefficients[2]
    }
    
    # ET ----
    for (j in -3:12){
      i <- 3
      # j <- 1
      slope.t <- lm(sim.data.no.c$sim.richness.4[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.et.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    # GT ----
    for (j in -3:12){
      i <- 4
      # j <- 1
      slope.t <- lm(sim.data.no.c$sim.richness.4[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.gt.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    # TM ----
    for (j in -3:12){
      i <- 5
      # j <- 1
      slope.t <- lm(sim.data.no.c$sim.richness.4[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.tm.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    # RM ----
    for (j in -3:12){
      i <- 6
      # j <- 1
      slope.t <- lm(sim.data.no.c$sim.richness.4[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)] ~ sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)])
      sim.rm.slopes.t[j+4] <- slope.t$coefficients[2]
    }
    
    
    # Slopes ----
    sim.te.slopes.t
    sim.br.slopes.t
    sim.et.slopes.t
    sim.gt.slopes.t
    sim.tm.slopes.t
    sim.rm.slopes.t
    
    # #### SIMULATION PLOT SLOPES ####
    {
      par(mfrow=c(2,3))
      plot(sim.te.slopes.t[4:16])
      plot(sim.br.slopes.t[4:16])
      plot(sim.et.slopes.t[4:16])
      plot(sim.gt.slopes.t[4:16])
      plot(sim.tm.slopes.t[4:16])
      plot(sim.rm.slopes.t[4:16])
    }
    
  }
# RICHNESS BY USE PLOTS ====
  # Define vectors for location names, use colors, reordered site names, and transect colors
  loc.names <- c("P","S","W")
  use.col <- c("red","blue","blue","red","red","blue")
  site.names.2 <- c("TE","GT","TM","BR","ET","RM")
  use.col.2 <- c("red","red","red","blue","blue","blue")
  col.transect <- c("red", "blue", "green", "black", "pink","black","orange","purple","red", "blue", "green", "black", "pink","black","orange","purple")
  pch.transect <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  
  par(mfrow=c(1,3))
  for (i in 1:length(loc.names)){
    plot(sim.data.no.c$sim.distt[sim.data.no.c$sim.loc==loc.names[i]], sim.data.no.c$sim.richness.3[sim.data.no.c$sim.loc==loc.names[i]], ylim = c(6,14))
    abline(lm(sim.data.no.c$sim.richness.3[sim.data.no.c$sim.site==site.names[(2*i)-1]] ~ sim.data.no.c$sim.distt[sim.data.no.c$sim.site==site.names[i]]), col=use.col[(2*i)-1])
    abline(lm(sim.data.no.c$sim.richness.3[sim.data.no.c$sim.site==site.names[2*i]] ~ sim.data.no.c$sim.distt[sim.data.no.c$sim.site==site.names[2*i]]), col=use.col[2*i])
  }
  
  par(mfrow=c(2,3), mar=c(4,4,4,2))
  for (i in 1:length(site.names)){
    par(new=F)
    for (j in -3:12){
      # i <- 1
      # j <- 1
      plot(sim.data.no.c$sim.richness.2[(sim.data.no.c$sim.site == site.names.2[i]) & (sim.data.no.c$sim.transect == j)] ~ (sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names.2[i]) & (sim.data.no.c$sim.transect == j)]), pch = pch.transect[j], col = col.transect[j], main = site.names.2[i], xlab = "Dist. from Trail", ylab = "Richness",axes=T, xlim=c(2,4), ylim=range(sim.data.no.c$sim.richness.2))
      # axis(side = 2, at = c(0,4,8,12),labels = c(0,4,8,12))
      # axis(side = 1,at = c(1,2,3),labels = c(0,2,4))
      par(new=T)
      abline(lm(sim.data.no.c$sim.richness.2[(sim.data.no.c$sim.site == site.names.2[i]) & (sim.data.no.c$sim.transect == j)] ~ (sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names.2[i]) & (sim.data.no.c$sim.transect == j)])),col = col.transect[j])
      par(new=T)
    }
  }
  
  par(mfrow=c(2,3), mar=c(4,4,4,2))
  for (i in 1:length(site.names)){
    par(new=F)
    for (j in -3:12){
      # i <- 1
      # j <- 1
      plot(sim.data.no.c$sim.richness.4[(sim.data.no.c$sim.site == site.names.2[i]) & (sim.data.no.c$sim.transect == j)] ~ (sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names[i]) & (sim.data.no.c$sim.transect == j)]), pch = pch.transect[j], col = col.transect[j], main = site.names.2[i], xlab = "Dist. from Trail", ylab = "Richness",axes=T, xlim=c(2,4),ylim=range(sim.data.no.c$sim.richness.4))
      # axis(side = 2, at = c(0,4,8,12),labels = c(0,4,8,12))
      # axis(side = 1,at = c(1,2,3),labels = c(0,2,4))
      par(new=T)
      abline(lm(sim.data.no.c$sim.richness.4[(sim.data.no.c$sim.site == site.names.2[i]) & (sim.data.no.c$sim.transect == j)] ~ (sim.data.no.c$sim.distt[(sim.data.no.c$sim.site == site.names.2[i]) & (sim.data.no.c$sim.transect == j)])),col = col.transect[j])
      par(new=T)
    }
  }

  # Test w/ real data #########
  #Filter out "C" transects and make distance numeric
  all.data.filter <- all.data.t[-which(all.data.t$Dist..Trail=="C"),]
  all.data.filter$Dist..Trail <- as.numeric(all.data.filter$Dist..Trail)
  head(all.data.filter)
  par(mfrow=c(2,3), mar=c(4,4,4,2))
  for (i in 1:length(site.names.2)){
    par(new=F)
    for (j in -3:12){
      # i <- 1
      # j <- 1
      plot(all.data.filter$Richness[(all.data.filter$Location == site.names.2[i]) & (all.data.filter$Position == j)] ~ (all.data.filter$Dist..Trail[(all.data.filter$Location == site.names.2[i]) & (all.data.filter$Position == j)]), pch = pch.transect[j], col = col.transect[j], xlim = c(0, 4), ylim = c(0, 12), main = site.names.2[i], xlab = "Dist. from Trail", ylab = "Richness")
      # axis(side = 2, at = c(0,4,8,12),labels = c(0,4,8,12))
      # axis(side = 1,at = c(1,2,3),labels = c(0,2,4))
      par(new=T)
      abline(lm(all.data.filter$Richness[(all.data.filter$Location == site.names.2[i]) & (all.data.filter$Position == j)] ~ (all.data.filter$Dist..Trail[(all.data.filter$Location == site.names.2[i]) & (all.data.filter$Position == j)])), col = col.transect[j])
      par(new=T)
    }
  }

## REGRESSIONS FOR RICHNESS AND ELEVATION ## ====
  
  # Simulated
  par(mfrow=c(1,1))
  plot(sim.data.no.c$sim.richness[sim.data.no.c$sim.distt==2] ~ sim.data.no.c$sim.elev[sim.data.no.c$sim.distt==2])
  
  # Real
  par(mfrow=c(1,1))
  plot(all.data.t$Richness[all.data.t$Dist..Trail==2] ~ all.data.t$Elevation[all.data.t$Dist..Trail==2])
  
  # Together
  par(mfrow=c(1,2))
  plot(sim.data.no.c$sim.richness[sim.data.no.c$sim.distt==2] ~ sim.data.no.c$sim.elev[sim.data.no.c$sim.distt==2], xlab="Elevation", ylab="Richness for Quadrat 2", main="Simulated")
  abline(lm(sim.data.no.c$sim.richness[sim.data.no.c$sim.distt==2] ~ sim.data.no.c$sim.elev[sim.data.no.c$sim.distt==2]))
  
  plot(all.data.t$Richness[all.data.t$Dist..Trail==2] ~ all.data.t$Elevation[all.data.t$Dist..Trail==2], xlab="Elevation", ylab="Richness for Quadrat 2", main="Actual")
  abline(lm(all.data.t$Richness[all.data.t$Dist..Trail==2] ~ all.data.t$Elevation[all.data.t$Dist..Trail==2]))

  
## RESPONSE VARIABLES ## ----
  # Setup a response variable data frame including relevant site characteristics
  resp.var.data.frame <- data.frame(all.data.t$Location,all.data.t$Dist..Trail)
  resp.var.data.frame$Richness <- all.data.t$Richness
  resp.var.data.frame$X.Cover..Total.Veg. <- all.data.t$X.Cover..Total.Veg.
  resp.var.data.frame$Cover.Height <- all.data.t$Cover.Height
  resp.var.data.frame$Richness..Forb. <- all.data.t$Richness..Forb.
  resp.var.data.frame$Richness..Gram. <- all.data.t$Richness..Gram.
  resp.var.data.frame$Richness..Shrub. <- all.data.t$Richness..Shrub.
  resp.var.data.frame$Richness..Novasc. <- all.data.t$Richness..Novasc.
  resp.var.data.frame$Richness..Lichen. <- all.data.t$Richness..Lichen.
  
  str(resp.var.data.frame$all.data.t.Dist..Trail)
  # Convert distance from trail to numeric in new column titled "DistT"
  resp.var.data.frame$DistT <- as.numeric(resp.var.data.frame$all.data.t.Dist..Trail)
  str(resp.var.data.frame$DistT)
  
  # Add column for percent cover times richness and for transect number
  resp.var.data.frame$X.Cover.Richness <- resp.var.data.frame$Richness * resp.var.data.frame$X.Cover..Total.Veg.
  resp.var.data.frame$Transect <- all.data.t$Position
  head(resp.var.data.frame)
  
  # Remove rows for center of the trail quadrats to do analysis on remaining three
  rows.2.remove <- which(resp.var.data.frame$all.data.t.Dist..Trail=="C")
  resp.var.data.frame.no.c <- resp.var.data.frame[-rows.2.remove,]
  head(resp.var.data.frame.no.c)
  
  # Shorten response variable data frame name for coding ease
  rv.df <- resp.var.data.frame.no.c
  # Define color vector for transects
  col.transect <- c("red", "blue", "green", "black", "pink","black","orange","purple","red", "blue", "green", "black", "pink","black","orange","purple")
  # Define shape vector for transects
  pch.transect <- c(rep(1,8),rep(2,8))
  
  # Plots for quadrat richness by transect and site
  par(mfrow=c(3,2), mar=c(4,4,4,2))
  for (i in 1:length(site.names)){
    par(new=F)
    for (j in -3:12){
      # i <- 1
      # j <- 1
      plot(rv.df$Richness[(rv.df$all.data.t.Location == site.names[i]) & (rv.df$Transect == j)] ~ jitter(rv.df$DistT[(rv.df$all.data.t.Location == site.names[i]) & (rv.df$Transect == j)]), pch = pch.transect[j], col = col.transect[j], xlim = c(1, 3), ylim = c(0, 12), main = site.names[i], xlab = "Dist. from Trail", ylab = "Richness")
      # axis(side = 2, at = c(0,4,8,12),labels = c(0,4,8,12))
      # axis(side = 1,at = c(1,2,3),labels = c(0,2,4))
      par(new=T)
      abline(lm(rv.df$Richness[(rv.df$all.data.t.Location == site.names[i]) & (rv.df$Transect == j)] ~ rv.df$DistT[(rv.df$all.data.t.Location == site.names[i]) & (rv.df$Transect == j)]), col = col.transect[j])
      par(new=T)
    }
  }
  
    
  