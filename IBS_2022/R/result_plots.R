# Result plots

#========================
# Spatial structure
#========================

# Libraries
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggpubr)

# Metrics
BITH_metrics_QC <- readRDS("/Users/victorcameron/Documents/Git/Metapop_ms/SDM/results/BITH_metrics_QC.RDS")

# Colors
cols <- c(QC_biomass = "darkblue", QC_RCP = "blue", RL_biomass = "darkgreen", RL_RCP = "green", ET_biomass = "darkred", ET_RCP = "red")

# Best intensity cutoff value to set the breeding range limit
RL_cutoff <- 0.00625 # 1 indv / km2

#### Figure format ####

# Save plot in file
png('./assets/img/spatial_configuration.png', width = 180, height = 60, units='mm', res = 300, bg = "transparent")

# Par
par(mfrow=c(1,3), mar=c(2,3,5,1.5))

#### Patch number ####

# Extract number of patches
if(!exists("n_QC")){
    n_QC <- data.frame(n = BITH_metrics_QC$n, Time = c(2020, 2040, 2070, 2100), scenario = c(rep("Climate-only",4),rep("Forest composition",4)))
}

# Plot number of patches
plot(n_QC$Time[n_QC$scenario=="Climate-only"], n_QC$n[n_QC$scenario=="Climate-only"], type = "b", col = "darkblue", ylim = c(1000, 7000),
    ylab = "", main = "Number of patches", xlab = "", cex.main = 1.5,lwd=2, axes=FALSE)
box(bty="l")
axis(1)
axis(2)
lines(n_QC$Time[n_QC$scenario=="Forest composition"], n_QC$n[n_QC$scenario=="Forest composition"], type = "b", lwd=2, col = "darkorange")
legend(2060, 7000, legend=c("Climate-only", "Forest change"), col=c("darkblue","darkorange"), lty=1, lwd=2, cex=0.7, box.lty=0)


#### Patch area ####

if(!exists(area_QC)){
    # Create Time variable
    area_QC <- data.frame(area = BITH_metrics_QC$patchArea$area, Time = BITH_metrics_QC$patchArea$scenario, scenario = BITH_metrics_QC$patchArea$scenario)
    area_QC$Time[area_QC$Time %in% c("BITH_RCP45_2020","BITH_biomass_2020")] <- 2020
    area_QC$Time[area_QC$Time %in% c("BITH_RCP45_2040","BITH_biomass_2040")] <- 2040
    area_QC$Time[area_QC$Time %in% c("BITH_RCP45_2070","BITH_biomass_2070")] <- 2070
    area_QC$Time[area_QC$Time %in% c("BITH_RCP45_2100","BITH_biomass_2100")] <- 2100

    # Aggregate data do get median
    area_QC <- aggregate(area_QC$area, by = list(area_QC$scenario, area_QC$Time), FUN = median) 
    area_QC$scenario[area_QC$Group.1 %in% c("BITH_RCP45_2020","BITH_RCP45_2040","BITH_RCP45_2070","BITH_RCP45_2100")] <- "Climate-only"
    area_QC$scenario[area_QC$Group.1 %in% c("BITH_biomass_2020","BITH_biomass_2040","BITH_biomass_2070","BITH_biomass_2100")] <- "Forest composition"

    # Rename some variables
    area_QC$Time <- area_QC$Group.2
    area_QC$area <- area_QC$x
}

# Plot patch area median
plot(area_QC$Time[area_QC$scenario=="Climate-only"], area_QC$area[area_QC$scenario=="Climate-only"], type = "b", col = "darkblue", ylab =expression("km"^2*""), main = "Median patch area (km2)", xlab = "", ylim = c(0.1,0.35), cex.main = 1.5,lwd=2, axes=FALSE)
box(bty="l")
axis(1)
axis(2)
lines(area_QC$Time[area_QC$scenario=="Forest composition"], area_QC$area[area_QC$scenario=="Forest composition"], type = "b", lwd=2, col = "darkorange")

#### Inter-patch distance ####

if(!exists(dist_QC)){
    # Function to extract distance
    extract.dist <- function(metrics){
        dist <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("dist", "scenario"))
        for(i in seq_along(metrics$n)){
            if(metrics$n[i] == 0) next
            dij <- metrics$d_ij[[i]][lower.tri(metrics$d_ij[[i]], diag = FALSE)]
            scenario <- names(metrics$d_ij[i])

            dist <- rbind(dist, data.frame(dij, scenario))
        }
        colnames(dist) <- c("dist", "scenario")
        #dist$scenario <- as.factor(dist$scenario)
        
        return(dist)
    }

    # Extract distance
    dist_QC <- cbind(extract.dist(BITH_metrics_QC), Time = 2020)

    # Create Time column
    dist_QC$Time[dist_QC$scenario %in% c("BITH_RCP45_2020","BITH_biomass_2020")] <- 2020
    dist_QC$Time[dist_QC$scenario %in% c("BITH_RCP45_2040","BITH_biomass_2040")] <- 2040
    dist_QC$Time[dist_QC$scenario %in% c("BITH_RCP45_2070","BITH_biomass_2070")] <- 2070
    dist_QC$Time[dist_QC$scenario %in% c("BITH_RCP45_2100","BITH_biomass_2100")] <- 2100

    # Aggregate data do get median
    dist_QC <- aggregate(dist_QC$dist, by = list(dist_QC$scenario, dist_QC$Time), FUN = median)
    dist_QC$scenario[dist_QC$Group.1 %in% c("BITH_RCP45_2020","BITH_RCP45_2040","BITH_RCP45_2070","BITH_RCP45_2100")] <- "Climate-only"
    dist_QC$scenario[dist_QC$Group.1 %in% c("BITH_biomass_2020","BITH_biomass_2040","BITH_biomass_2070","BITH_biomass_2100")] <- "Forest composition"

    # Rename some variables
    dist_QC$Time <- dist_QC$Group.2
    dist_QC$dist <- dist_QC$x
}

# Plot patch dist median
plot(dist_QC$Time[dist_QC$scenario=="Climate-only"], dist_QC$dist[dist_QC$scenario=="Climate-only"], type = "b", col = "darkblue", ylab = "km", main = "Median inter-patch \ndistance (km)", xlab = "", ylim = c(215,280), cex.main = 1.5, lwd=2, axes=FALSE)
box(bty="l")
axis(1)
axis(2)
lines(dist_QC$Time[dist_QC$scenario=="Forest composition"], dist_QC$dist[dist_QC$scenario=="Forest composition"], type = "b", lwd=2, col = "darkorange")

# Close file
dev.off()






#========================
# Persistence
#========================
persistence_plot <- function() {
    #### Area ####

    totalArea_QC <- data.frame(totalArea = BITH_metrics_QC$totalArea, scenario = names(BITH_metrics_QC$totalArea), Time = 2020)

    # Create Time column
    totalArea_QC$Time[totalArea_QC$scenario %in% c("BITH_RCP45_2020","BITH_biomass_2020")] <- 2020
    totalArea_QC$Time[totalArea_QC$scenario %in% c("BITH_RCP45_2040","BITH_biomass_2040")] <- 2040
    totalArea_QC$Time[totalArea_QC$scenario %in% c("BITH_RCP45_2070","BITH_biomass_2070")] <- 2070
    totalArea_QC$Time[totalArea_QC$scenario %in% c("BITH_RCP45_2100","BITH_biomass_2100")] <- 2100

    # Rename scenarios
    totalArea_QC$scenario[totalArea_QC$scenario %in% c("BITH_RCP45_2020","BITH_RCP45_2040","BITH_RCP45_2070","BITH_RCP45_2100")] <- "Climate-only"
    totalArea_QC$scenario[totalArea_QC$scenario %in% c("BITH_biomass_2020","BITH_biomass_2040","BITH_biomass_2070","BITH_biomass_2100")] <- "Forest composition"

    # Rescale taotalArea
    totalArea_QC$totalArea <- totalArea_QC$totalArea/totalArea_QC$totalArea[1]

    #### Capacity ####

    capacity_QC <-  cbind(BITH_metrics_QC$capacity, Time = 2020)

    # Create Time column
    capacity_QC$Time[capacity_QC$scenario %in% c("BITH_RCP45_2020","BITH_biomass_2020")] <- 2020
    capacity_QC$Time[capacity_QC$scenario %in% c("BITH_RCP45_2040","BITH_biomass_2040")] <- 2040
    capacity_QC$Time[capacity_QC$scenario %in% c("BITH_RCP45_2070","BITH_biomass_2070")] <- 2070
    capacity_QC$Time[capacity_QC$scenario %in% c("BITH_RCP45_2100","BITH_biomass_2100")] <- 2100

    # Rename scenarios
    capacity_QC$scenario[capacity_QC$scenario %in% c("BITH_RCP45_2020","BITH_RCP45_2040","BITH_RCP45_2070","BITH_RCP45_2100")] <- "Climate-only"
    capacity_QC$scenario[capacity_QC$scenario %in% c("BITH_biomass_2020","BITH_biomass_2040","BITH_biomass_2070","BITH_biomass_2100")] <- "Forest composition"

    # Subset data
    capacity_QC <- capacity_QC[capacity_QC$alpha %in% c(1,0.002),]

    # Rescale capacity
    capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==1] <- capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==1]/capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==1][1]

    capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==0.002] <- capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==0.002]/capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==0.002][1]

    capacity_QC$capacity[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==1] <- capacity_QC$capacity[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==1]/capacity_QC$capacity[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==1][1]

    capacity_QC$capacity[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==0.002] <- capacity_QC$capacity[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==0.002]/capacity_QC$capacity[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==0.002][1]


    #### Plot1 ####

    # Save plot in file
    png('./assets/img/capacity1.png', width = 200, height = 90, units='mm', res = 300, bg = "transparent")

    # Par
    par(mfrow=c(1,2), mar=c(4,2.5,1,1))

    # Plot climate-only
    plot(totalArea_QC$Time[totalArea_QC$scenario=="Climate-only"], totalArea_QC$totalArea[totalArea_QC$scenario=="Climate-only"], type="b", col="darkblue", ylab="", xlab="Time", cex.lab=1.5, lwd=3, yaxt='n', axes=FALSE, ylim=c(0,2), main = "Climate-only", cex.main = 1.5)
    box(bty="l")
    axis(1)
    title(ylab="Persistence", line=1, cex.lab = 1.5)
    #legend(2065, 2, legend=c("Habitat amount", "Capacity", "1 km dispersal", "500 km dispersal"), col=c("darkblue","darkgrey","darkgrey","darkgrey"), lty=c(1,1,2:3), lwd=2, cex=0.7, box.lty=0)
    legend(2065, 2, legend=c("Habitat amount"), col=c("darkblue"), lty=1, lwd=3, cex=0.7, box.lty=0)

    plot(totalArea_QC$Time[totalArea_QC$scenario=="Forest composition"], totalArea_QC$totalArea[totalArea_QC$scenario=="Forest composition"], type = "b", col = "darkorange", ylab="", xlab="Time", cex.lab=1.5, lwd=3, yaxt='n', axes=FALSE, ylim=c(0,2), main = "Forest change", cex.main = 1.5)
    box(bty="l")
    axis(1)

    # Close plot
    dev.off()


    #### Plot2 ####

    # Save plot in file
    png('./assets/img/capacity2.png', width = 200, height = 90, units='mm', res = 300, bg = "transparent")

    # Par
    par(mfrow=c(1,2), mar=c(4,2.5,1,1))

    # Plot climate-only
    plot(totalArea_QC$Time[totalArea_QC$scenario=="Climate-only"], totalArea_QC$totalArea[totalArea_QC$scenario=="Climate-only"], type="b", col="darkblue", ylab="", xlab="Time", cex.lab=1.5, lwd=3, yaxt='n', axes=FALSE, ylim=c(0,2), main = "Climate-only", cex.main = 1.5)
    box(bty="l")
    axis(1)
    title(ylab="Persistence", line=1, cex.lab = 1.5)
    #legend(2065, 2, legend=c("Habitat amount", "Capacity", "1 km dispersal", "500 km dispersal"), col=c("darkblue","darkgrey","darkgrey","darkgrey"), lty=c(1,1,2:3), lwd=2, cex=0.7, box.lty=0)
    legend(2065, 2, legend=c("Habitat amount", "Capacity (500 km)"), col=c("darkblue", "darkgrey"), lty=c(1,3), lwd=3, cex=0.7, box.lty=0)
    par(new=TRUE)
    plot(capacity_QC$Time[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==0.002], capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==0.002], type="b", col="darkgrey", lty=3, lwd=3, axes=FALSE, bty="n", xlab="", ylab="", ylim=c(-7,9))

    plot(totalArea_QC$Time[totalArea_QC$scenario=="Forest composition"], totalArea_QC$totalArea[totalArea_QC$scenario=="Forest composition"], type = "b", col = "darkorange", ylab="", xlab="Time", cex.lab=1.5, lwd=3, yaxt='n', axes=FALSE, ylim=c(0,2), main = "Forest change", cex.main = 1.5)
    box(bty="l")
    axis(1)
    par(new=TRUE)
    plot(capacity_QC$Time[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==0.002], capacity_QC$capacity[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==0.002], type="b", col="darkgrey", lty=3, lwd=3, axes=FALSE, bty="n", xlab="", ylab="", ylim=c(-2.5,4.5))

    # Close plot
    dev.off()


    #### Plot3 ####

    # Save plot in file
    png('./assets/img/capacity3.png', width = 200, height = 90, units='mm', res = 300, bg = "transparent")

    # Par
    par(mfrow=c(1,2), mar=c(4,2.5,1,1))

    # Plot climate-only
    plot(totalArea_QC$Time[totalArea_QC$scenario=="Climate-only"], totalArea_QC$totalArea[totalArea_QC$scenario=="Climate-only"], type="b", col="darkblue", ylab="", xlab="Time", cex.lab=1.5, lwd=3, yaxt='n', axes=FALSE, ylim=c(0,2), main = "Climate-only", cex.main = 1.5)
    box(bty="l")
    axis(1)
    title(ylab="Persistence", line=1, cex.lab = 1.5)
    #legend(2065, 2, legend=c("Habitat amount", "Capacity", "1 km dispersal", "500 km dispersal"), col=c("darkblue","darkgrey","darkgrey","darkgrey"), lty=c(1,1,2:3), lwd=2, cex=0.7, box.lty=0)
    legend(2065, 2, legend=c("Habitat amount", "Capacity (500 km)", "Capacity (1 km)"), col=c("darkblue","darkgrey","darkgrey"), lty=c(1,3,2), lwd=3, cex=0.7, box.lty=0)
    par(new=TRUE)
    plot(capacity_QC$Time[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==1], capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==1], type="b", col="darkgrey", lty=2, lwd=3, axes=FALSE, bty="n", xlab="", ylab="", ylim=c(-6,8))
    par(new=TRUE)
    plot(capacity_QC$Time[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==0.002], capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==0.002], type="b", col="darkgrey", lty=3, lwd=3, axes=FALSE, bty="n", xlab="", ylab="", ylim=c(-7,9))

    plot(totalArea_QC$Time[totalArea_QC$scenario=="Forest composition"], totalArea_QC$totalArea[totalArea_QC$scenario=="Forest composition"], type = "b", col = "darkorange", ylab="", xlab="Time", cex.lab=1.5, lwd=3, yaxt='n', axes=FALSE, ylim=c(0,2), main = "Forest change", cex.main = 1.5)
    box(bty="l")
    axis(1)
    par(new=TRUE)
    plot(capacity_QC$Time[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==1], capacity_QC$capacity[capacity_QC$scenario=="Climate-only" & capacity_QC$alpha==1], type="b", col="darkgrey", lty=2, lwd=3, axes=FALSE, bty="n", xlab="", ylab="", ylim=c(-5,7))
    par(new=TRUE)
    plot(capacity_QC$Time[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==0.002], capacity_QC$capacity[capacity_QC$scenario=="Forest composition" & capacity_QC$alpha==0.002], type="b", col="darkgrey", lty=3, lwd=3, axes=FALSE, bty="n", xlab="", ylab="", ylim=c(-2.5,4.5))

    # Close plot
    dev.off()
}