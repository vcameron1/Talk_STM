#########################
# Figures des projections
# de distribution de BITH
# Victor Cameron
# Mai 2022
#########################

#========================
# map parameters
#========================

# Plot bg
bg = "transparent"

# Colors
cols = c("grey80", "darkgreen")
cols_inverse = c("grey50", "orange")

# colNA
colNA = NA

# Title cex
title_cex = 2


#========================
# map_BITH_2020
#========================

BITH_map_2020 <- function(scenarios = c("RCP45_2020"),
                    main = c("2020"),
                    RL_cutoff = 0.00625, extent = c(xmin = -514009, xmax = 356398, ymin = 110389, ymax = 633143), 
                    file_name = "./assets/img/map_BITH_2020.png") {


    # 1 - Import projections --------------------------------------------------


    #### Scenarios ####
    filenames <- paste0("/Users/victorcameron/Documents/Git/Metapop_ms/SDM/results/BITH_", scenarios, ".tif")


    # 2 - Draw the map --------------------------------------------------------

    
    #### Loop through scenarios and plot them ####

    # Save plot in file
    png(file_name, width = 250, height = 150, units='mm', res = 200, bg = bg)

    # Set up plot layout
    par(mar=c(3,3,1,1))

    # Loop
    i=1
    for (map in filenames){

        # # Import projection
        r <- raster::raster(map)

        # # Limit analysis to current distribution
        e <- raster::extent(extent)
        r_sub <- raster::crop(r, e)

        # # Draw the map
        raster::plot(r_sub>=log(RL_cutoff), legend = FALSE, bty = "o", yaxs="i", xaxs="i", axes=FALSE, col = cols, colNA = colNA, 
        main = main[i], cex.main = title_cex)

        i = i + 1
    }

    # Close file
    dev.off()
}


#========================
# map_BITH_2020_ET
# Zoom on Eastern Townships
#========================

BITH_map_2020_ET <- function(scenarios = c("RCP45_2020"),
                    main = c("2020"),
                    RL_cutoff = 0.00625, extent = c(xmin = -356488, xmax = -115085, ymin = 111680, ymax = 234873), 
                    file_name = "./assets/img/map_BITH_2020_ET.png") {


    # 1 - Import projections --------------------------------------------------


    #### Scenarios ####
    filenames <- paste0("/Users/victorcameron/Documents/Git/Metapop_ms/SDM/results/BITH_", scenarios, ".tif")


    # 2 - Draw the map --------------------------------------------------------

    
    #### Loop through scenarios and plot them ####

    # Save plot in file
    png(file_name, width = 250, height = 122, units='mm', res = 200, bg = bg)

    # Set up plot layout
    par(mar=c(3,3,1,1))

    # Loop
    i=1
    for (map in filenames){

        # # Import projection
        r <- raster::raster(map)

        # # Limit analysis to current distribution
        e <- raster::extent(extent)
        r_sub <- raster::crop(r, e)

        # # Draw the map
        raster::plot(r_sub>=log(RL_cutoff), legend = FALSE, bty = "o", yaxs="i", xaxs="i",  col = cols_inverse, colNA = colNA, 
        main = main[i], cex.main = title_cex)

        i = i + 1
    }

    # Close file
    dev.off()
}



#========================
# map_BITH_2100
#========================

BITH_map_2100 <- function(scenarios = c("RCP45_2100", "biomass_2100"),
                    main = c("Climate-only 2100", "Forest change 2100"),
                    RL_cutoff = 0.00625, extent = c(xmin = -514009, xmax = 356398, ymin = 110389, ymax = 633143), 
                    file_name = "./assets/img/map_BITH_2100.png") {


    # 1 - Import projections --------------------------------------------------


    #### Scenarios ####
    filenames <- paste0("/Users/victorcameron/Documents/Git/Metapop_ms/SDM/results/BITH_", scenarios, ".tif")


    # 2 - Draw the map --------------------------------------------------------

    
    #### Loop through scenarios and plot them ####

    # Save plot in file
    png(file_name, width = 500, height = 170, units='mm', res = 200, bg=bg)

    # Set up plot layout
    par(mfrow=c(1,2), mar=c(3,3,3,1))

    # Loop
    i=1
    for (map in filenames){

        # # Import projection
        r <- raster::raster(map)

        # # Limit analysis to current distribution
        e <- raster::extent(extent)
        r_sub <- raster::crop(r, e)

        # # Draw the map
        raster::plot(r_sub>=log(RL_cutoff), legend = FALSE, bty = "o", yaxs="i", xaxs="i", axes=FALSE, col = cols, colNA = colNA, 
        main = main[i], cex.main = title_cex)

        i = i + 1
    }

    # Close file
    dev.off()
}


#========================
# Hypothetical contraction of high elevation habitat
#========================

# Dependencies
library(elevatr)
library(raster)
source("/Users/victorcameron/Documents/Git/Metapop_ms/conceptual_fig/patchArea_metrics.R")

#### Load data ####

# Québec shapefile map
topo <- raster("/Users/victorcameron/Documents/Git/Metapop_ms/data_clean/templateRaster.tif")

# Québec contour map
mask <- sf::read_sf("/Users/victorcameron/Documents/Git/Metapop_ms/data_clean/quebec_nad83.shp")
mask_proj <- sf::st_transform(mask, crs(topo))

# Get elevation
elevation <- get_elev_raster(topo, z = 5)
elevation_c <- raster::mask(elevation, mask_proj)

#### Redefine map extent ####

# Crop map extent
extent = c(xmin = -514009, xmax = 356398, ymin = 110389, ymax = 633143)
e <- raster::extent(extent) # LatLong limits
elevation_reduced <- raster::crop(elevation, e)
elevation_reduced <- raster::mask(elevation_reduced, mask_proj)
# elevation_reduced[elevation_reduced<0] <- 0

# Get more precise elevation
#elevation2 <- get_elev_raster(elevation_reduced, z = 10)


#### Compute landscape metrics ####

land <- patchArea_metrics(elevation_reduced)

patchArea700 <- land$patchArea700
patchArea800 <- land$patchArea800

#### Draw the map ####

# Save plot in file
png('./assets/img/map_elevation.png', width = 250, height = 167, units='mm', res = 700, bg="transparent")

# South of Quebec base plot
raster::plot(elevation_reduced > 0, legend=F,
             bty = "o", yaxs="i", xaxs="i", col = cols[1], colNA = colNA)
# plot(elevation_reduced, legend.only=TRUE,
#         legend.args = list(text='Elevation (m)', line = 0))


# raster::plot(elevation_reduced<=0, bty="n", box=FALSE, legend = F, axes = F,  col = c('transparent', 'white'), add=T)

# Scale bar
# scalebar(d=200000, xy=c(0, 2e+5), type='bar', divs=4, lonlat=FALSE, below = 'm')

arrows(x0=2e+5, x1=2e+5, y0=2e+5, y1=2.5e+5,
        length=0.15, lwd=4)
text(x=2e+5, y=1.8e+5, label = "N", cex=1, font=2)

# 700m altitude contour
contour(elevation_reduced>=700, lwd=4, add=T, drawlabels=F)

# 800m altitude contour
contour(elevation_reduced>=800, col='red', lwd=2, add=T, drawlabels=F)

# Distribution of patch area
#par(fig=c(0.55,0.8,.2,0.65), lwd=3, new=TRUE)
#hist(log(patchArea800$area), angle=45, density=5, col='red', main='', xlab='', ylab='')
#mtext(side=1, 'Log patch area', line=2)
#mtext(side=2, 'Frequency', line=2)
#mtext(side=3, 'Distribution of patch area', line=0, cex=1.2)
#hist(log(patchArea700$area), col='black', angle=135, density=5, border='black', add=T, fill='transparent', cex=2)

# Close file
dev.off()
