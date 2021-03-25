#=======================
# Function to plot system range limits in landscape
# May 28, 2020
# Victor Cameron
#=======================

#########################
# Parameters
# limits: Object containing the temporal series of range limits
# landscape: width and height of landscape
##########################

plot_distribution <- function(limits,landscape){

  with(as.list(c(limits,landscape)),{

    # New window for plot
    quartz(height = 5, width = 6)

    # Parameters
    LimitB <- limits[,1]
    LimitT <- limits[,2]
    LimitS <- limits[,3]

    # Define Landscape height
    ymax <- height


    # Time sequence
    timeSteps <- seq(1,length(LimitB),by=1)

    # Plot
    plot(timeSteps,LimitB,type = "l", xlab = "Time", ylab = "Position (patch height)", cex.lab = 1.5, cex.axis = 1.25,ylim=c(ymax,0))
    lines(timeSteps,LimitT,lty=2)
    lines(timeSteps,LimitS,lty=1,col="red")

    # Legend
    legend("topright",
           legend = c("State B","State T","Species"),
           lty = c(1,2,1),
           col=c("black","black","red"),
           lwd = 3)

  })
}


#=======================
# Function to plot species distribution in landscape
# May 18, 2020
# Victor Cameron
#=======================

#########################
# Parameters
# limits: Object containing the temporal series of range limits
# landscape: width and height of landscape
##########################

plot_species_distribution <- function(limits,landscape){

  with(as.list(c(limits,landscape)),{

    # New window for plot
    quartz(height = 5, width = 6)

    # Parameters
    LimitS_warm <- limits[,3]
    LimitS_cold <- limits[,4]

    # Define Landscape height
    ymax <- height

    # Time sequence
    timeSteps <- seq(1,length(LimitS_warm),by=1)

    # Plot
    plot(timeSteps,LimitS_warm,type = "l", xlab = "Time", ylab = "Position (patch height)", cex.lab = 1.5, cex.axis = 1.25, ylim=c(ymax,0))
    lines(timeSteps,LimitS_cold,type = "l")
    polygon(c(timeSteps,rev(timeSteps)), c(LimitS_cold,rev(LimitS_warm)),col=gray(0.8))

  })
}


#=======================
# Function to plot habitat range limits in landscape
# May 18, 2020
# Victor Cameron
#=======================

#########################
# Parameters
# limits: Object containing the temporal series of range limits
# landscape: width and height of landscape
##########################

plot_habitat_distribution <- function(limits,landscape){

  with(as.list(c(limits,landscape)),{

    # New window for plot
    quartz(height = 5, width = 6)

    # Parameters
    LimitB <- limits[,1]
    LimitT <- limits[,2]

    # Define Landscape height
    ymax <- height

    # Time sequence
    timeSteps <- seq(1,length(LimitB),by=1)

    # Plot
    plot(timeSteps,LimitB,type = "l", xlab = "Time", ylab = "Position (patch height)", cex.lab = 1.5, cex.axis = 1.25,ylim=c(ymax,0))
    lines(timeSteps,LimitT,lty=2)

    # Legend
    legend("topright",
           legend = c("State B","State T"),
           lty = c(1,2),
           lwd = 3)

  })
}


#=======================
# Function to plot species distribution
# Output is a gif image
# August 17, 2020
# Victor Cameron
#=======================

#########################
# Parameters
# Presence: Object containing the distribution at InitTimetoEq, InitTimeToEq + ClimateChangetime, and nsteps
# landscape: width and height of landscape
##########################

plot_distribution.gif <- function(Presence){

  # New window for plot
  #quartz(height = 5, width = 6)

  # Extra parameters
  height <- nrow(Presence[[1]])
  width <- ncol(Presence[[1]])

  # Store values for each time step
  rasters <- list()
  for(layer in 1:length(Presence)){

    # # Initiate raster
    assign('r', raster::raster(nrow=height, ncol=width, xmn=0, xmx=width, ymn=0, ymx=height))

    # # Set values to raster objects
    raster::values(r) <- Presence[[layer]]
    rasters[layer] <- r
  }

  # Stack raster objects
  stack = raster::stack(rasters)

  # Plot gif object
  timeStep <- 100/(length(Presence) - 2)
  raster::animate(stack, pause=0.2, n=4, axes=FALSE, box=FALSE,
                  main = round(c(0, seq(2020, 2120, by = timeStep))),
                  legend = FALSE)

}


#=======================
# Function to plot habitat distribution
# Output is a gif image
# May 18, 2020
# Victor Cameron
#=======================

#########################
# Parameters
# Presence: Object containing the habitat distribution at t=0, at t=100, and at t=nsteps
##########################

plot_hab_distribution.gif <- function(Habitat){

  # Extra parameters
  height <- nrow(Habitat[[1]])
  width <- ncol(Habitat[[1]])

  # New window for plot
  #quartz(height = 5, width = 6)

  # Habitat matrices converted to numerical matrices
  for(i in 1:length(Habitat)){
    Habitat[[i]][Habitat[[i]]=="B"] <- 1
    Habitat[[i]][Habitat[[i]]=="M"] <- 2
    Habitat[[i]][Habitat[[i]]=="T"] <- 3
    Habitat[[i]][Habitat[[i]]=="R"] <- 4
    m <- mapply(Habitat[[i]], FUN=as.numeric)
    Habitat[[i]] <- matrix(data=m, ncol=width, nrow=height)
  }

  # Store values for each time step
  rasters <- list()
  for(layer in 1:length(Habitat)){

    # # Initiate raster
    assign('r', raster::raster(nrow=height,ncol=width, xmn=0, xmx=width, ymn=0, ymx=height))

    # # Set values to raster objects
    raster::values(r) <- Habitat[[layer]]
    rasters[layer] <- r
  }

  # Stack rasters
  stack = raster::stack(rasters)

  # Animate
  raster::animate(stack,pause=0.8,n=4, col = c("darkcyan", "palegreen3", "orange", "black"),axes=FALSE,box=FALSE,legend=FALSE)

}
