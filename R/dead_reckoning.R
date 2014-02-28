dead_reckoning <-
function(speed, heading, angle = "degree", ret = TRUE, depth = NULL,
         pitch = NULL, startcoords=c(0,0), endcoords=NULL, coordsystem = "Cartesian",
         speedhorizontal = "corrected"){

  #--------------------------------------------------------------------------------------------------
  #exception handling
  if (angle != "degree" & angle != "radian") stop("heading must be in either degree or radian") 
  if (speedhorizontal != "corrected" & speedhorizontal != "pitch" & speedhorizontal != "depth") stop("speedhorizontal is mispecified") 
  if (length(speed) != length(heading)) stop("speed and heading must be the same length")
  
  #--------------------------------------------------------------------------------------------------
  #parse the utm zone inputs
  utmutm <- tolower(substr(coordsystem,1,3))
  
  if (utmutm == "utm"){
    if (nchar(coordsystem) != 6) stop("utm zone not specified correctly. example: 'utm18n', for UTM zone 18 north")
    # utm zone
    utmnum <- substr(coordsystem,4,5)
    utmnumint <- as.integer(utmnum)
    zones <- 1:60
    if (any(zones==utmnumint) == FALSE) stop("utm zone not equal to 1-60")
    # hemisphere
    utmhemis <- tolower(substr(coordsystem,6,6))
    if (utmhemis != "n" & utmhemis != "s") stop("hemisphere for utm zone must be specified as either 'n' or 's'")

    # asign coordsystem indicator
    coorsys <- "utm"
    
    # build the proj4 string
    if (utmhemis == "n"){
      projutm <- sprintf("+proj=utm +zone=%s +ellps=WGS84 +datum=WGS84 +units=m +no_defs",utmnum)
    }
    if (utmhemis == "s"){
      projutm <- sprintf("+proj=utm +zone=%s +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",utmnum)
    }    

  }
  
  if (coordsystem == "Cartesian"){coorsys <- "Cartesian"}
  if (coordsystem == "Geographic"){coorsys <- "Geographic"}
  
  if (coorsys == "Geographic"){
    
    # you must search for the correct UTM zone
    data(utmzones)
    print("hey")
    askpoint <- data.frame('x' = startcoords[1],'y' = startcoords[2])
    coordinates(askpoint) <- c("x","y")
    proj4string(askpoint) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    print("hey1")
    zoneinfo <- over(askpoint, utmzones)
    #SWLON SWLAT HEMISPHERE ZONE  CM Zone_Hemi
    #1   162   -80          s   58 165      58,s
    #make your utm zone string
    if (zoneinfo$HEMISPHERE == "n"){
      projutm <- sprintf("+proj=utm +zone=%s +ellps=WGS84 +datum=WGS84 +units=m +no_defs",zoneinfo$ZONE)
      print("heynorth")
    }
    if (zoneinfo$HEMISPHERE == "s"){
      projutm <- sprintf("+proj=utm +zone=%s +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",zoneinfo$ZONE)
      print("heysouth")
    }
  }

  #--------------------------------------------------------------------------------------------------
  # horizontal speed
  dive_len <- length(speed)
  
  # Calculate horizontal speed values.
  if (speedhorizontal == "pitch" & is.null(pitch) == FALSE){
    speed <- cos(pitch)*speed
    print("horizontal speed is being calculated using cos(pitch)*speed")
  }
  if (speedhorizontal == "depth" & is.null(depth) == FALSE){
    speed <- cos(abs(pitch))*speed
    deltadepth <- c(0,abs(diff(depth)))
    speed <- ifelse(speed < deltadepth,deltadepth,sqrt(speed^2 - deltadepth^2))
    print("horizontal speed is being calculated using speed and change in depth")
  }

  #--------------------------------------------------------------------------------------------------
  # 1) Course Steered, by dead reckoning
  print("hey2")
  if (angle == "degree"){ xx <- speed*sin(heading/(180/pi)) ; yy <- speed*cos(heading/(180/pi)) }
  if (angle == "radian"){ xx <- speed*sin(heading) ; yy <- speed*cos(heading)  }    
  xx[1] <- 0
  yy[1] <- 0
  xs <- cumsum(xx)
  ys <- cumsum(yy)
  print("hey3")
  #--------------------------------------------------------------------------------------------------
  # 2) Course Steered
  
  # 2.1 )Drift values
  # For a return dive (i.e. animal returns to the start location)
  # For a non-return dive (i.e. animal travels to a location other than the start location)
  # If the location is known (e.g. satellite tag location fix) then the "endcoords" will be used as the end location.
  # If it is a non-return dive, with an unkown end location, course made good cannot be calculated and a course steered is
  # the only option. This will result in CMGx and CMGy values "NA". 
  
  #-------------------------
  # If the coordinates are geographic, and the animal does not return to the start point, unproject the endcoords
  if (coorsys == "Geographic" & ret == FALSE){
    
    startutm <- project(cbind(startcoords[1],startcoords[2]),projutm) #project to utm
    endutm <- project(cbind(endcoords[1],endcoords[2]),projutm) #project to utm
    endcoords <- c((endutm[1] - startutm[1]),(endutm[2] - startutm[2]))
  }  
   
  
  if (ret == TRUE){endcoords <- c(xs[1],ys[1])}
  
  driftx <- (xs[dive_len] - endcoords[1])/dive_len ; drifty <- (ys[dive_len] - endcoords[2])/dive_len 
  drift <-  sqrt((xs[dive_len] - endcoords[1])^2 + (ys[dive_len] - endcoords[2])^2)/dive_len
  errordistance <- sqrt((xs[dive_len] - endcoords[1])^2 + (ys[dive_len] - endcoords[2])^2)  
  set <- atan2((endcoords[2] - ys[dive_len]),(endcoords[1] - xs[dive_len])) 
  set <- ((2*pi) - (set-(pi/2))) %% (2*pi)
  print("hey4")
  #--------------------------------------------------------------------------------------------------
  # Course Made Good
  
  #Provide initial values for the course made good
  xcmg <- xs[1]
  ycmg <- ys[1]
  
  #Calculate course made good
  tty <- 1:dive_len
  xcmg <- xs - (driftx*tty)
  ycmg <- ys - (drifty*tty)
  xcmg[1] <- 0
  ycmg[1] <- 0
  print("hey5")
  #--------------------------------------------------------------------------------------------------
  #Speed made good
  speedmg <- c(0,sqrt(diff(xcmg)^2 + diff(ycmg)^2))
  if (is.null(depth) == FALSE){speedmg <- c(0,sqrt(diff(xcmg)^2 + diff(ycmg)^2 + diff(depth)^2))}
  
  print("hey6")
  #--------------------------------------------------------------------------------------------------
  # If the user had geographic coordinates for start and/or endpoints, we can convert. This is a common
  # situation given the use of gps units and tag technology.
  
  # If the user input is geographic start and endpoints, or just starting point. Transform the coordinates from
  # cartesian to utm to geographic
  if (coorsys == "Geographic"){
    print("hey6.0")
    print(projutm)
    utmpts <- project(cbind(startcoords[1],startcoords[2]),projutm) #project to utm
    utmdf <- as.data.frame(utmpts)
    colnames(utmdf) <- c("x","y")
    print("hey6.1")
    cs.df <- data.frame("x" = xs + utmdf$x, "y" = ys + utmdf$y) # get the cs into utm 
    cmg.df <- data.frame("x" = xcmg + utmdf$x, "y" = ycmg + utmdf$y)  # get the cmg into utm
    print("hey6.2")
    coordinates(cs.df) <- c("x","y") # make it spatial points
    coordinates(cmg.df) <- c("x","y") # make it spatial points
    print("hey6.3")
    proj4string(cs.df) <- CRS(projutm) # project in utm
    proj4string(cmg.df) <- CRS(projutm) # project in utm
    print("hey6.4")
    cs.geo <- spTransform(cs.df,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # spatial transform from utm to wgs
    cmg.geo <- spTransform(cmg.df,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # spatial transform from utm to wgs
    print("hey6.4")
    navlist <- list("CSx" = cs.geo$x,"CSy" = cs.geo$y, "CMGx" = cmg.geo$x, "CMGy" = cmg.geo$y, "depth" = depth, "speedh" = speed, "speedmg" = speedmg,
                    "drift" = drift, "errordistance" = errordistance, "set" = set, "coordsystem" = coordsystem)  
    print("hey7")
  }
  
  # if the user input is utm 
  if (coorsys == "utm"){
    utmpts <- project(cbind(startcoords[1],startcoords[2]),projutm)
    utmdf <- as.data.frame(utmpts)
    colnames(utmdf) <- c("x","y")
    cs.df <- data.frame("x" = xs + utmdf$x, "y" = ys + utmdf$y)
    cmg.df <- data.frame("x" = xcmg + utmdf$x, "y" = ycmg + utmdf$y)
    navlist <- list("CSx" = cs.df$x,"CSy" = cs.df$y, "CMGx" = cmg.df$x, "CMGy" = cmg.df$y, "depth" = depth, "speedh" = speed, "speedmg" = speedmg,
                    "drift" = drift, "errordistance" = errordistance, "set" = set, "coordsystem" = coordsystem)  
    
  }  
  if (coorsys == "Cartesian"){
    navlist <- list("CSx" = xs + startcoords[1],"CSy" = ys+ startcoords[2], "CMGx" = xcmg + startcoords[1], "CMGy" = ycmg + startcoords[2], "depth" = depth, "speedh" = speed, "speedmg" = speedmg,
                    "drift" = drift, "errordistance" = errordistance, "set" = set, "coordsystem" = coordsystem)  
  }  
  
  class(navlist) <- "dr"
  return(navlist)
  
}


print.dr <- function(x, ...){
  dr <- x
  atbs <- attributes(dr)
  cat("\n","'dr' class object properties:", "\n\n")
  cat("Object Information--------------------------", "\n")
  cat("Names of dr class object: 'CSx' 'CSy' 'CMGx' 'CMGy' 'depth' 'speedh' 'speedmg' 'drift' 'errordistance' 'set' 'coordsystem'", "\n")
  cat("Duration (length) of track: ", length(dr$CSx), "\n\n")
  cat("Navigation Information----------------------", "\n")
  cat("set (radians): ", dr$set, "\n")
  cat("drift (error/time): ",dr$drift, "\n")
  cat("errordistance: ", dr$errordistance, "\n\n")
  cat("Spatial Information-------------------------", "\n")
  cat("coordinate system: ", dr$coordsystem, "\n\n")  
}

plot.dr <- function(x, ...){
  dr <- x
  minx <- min(dr$CSx, dr$CMGx)
  maxx <- max(dr$CSx, dr$CMGx)
  miny <- min(dr$CSy, dr$CMGy)
  maxy <- max(dr$CSy, dr$CMGy)
  end <- length(dr$CSx)
  
  if (dr$coordsystem == "Cartesian"){
    xlabel = "X-coordinate (unprojected)"
    ylabel = "Y-coordinate (unprojected)"
  }
  if (dr$coordsystem == "Geographic"){
    xlabel = "Latitude (decimal degrees)"
    ylabel = "Longitude (decimal degrees)"
  }
  if (grepl("utm", dr$coordsystem, perl=TRUE)){
    xlabel = "Easting (m)"
    ylabel = "Northing (m)"
  }  
  
  ellipsis <- list(...)
  
  # if the ellipsis is empty
  if (length(ellipsis) == 0){

    
    plot(dr$CSx, dr$CSy, type='l',lty=2, col='gray',xlab=xlabel,
         ylab=ylabel,xlim=c(minx,maxx), ylim=c(miny,maxy), asp=1)
    lines(dr$CMGx,dr$CMGy,col='black')
    #arrows(dr$CSx[end],dr$CSy[end], dr$CSx[1],dr$CSy[1], length = 0.25, angle = 30,
           #code = 2, col ='cornflowerblue')
    t.set <- paste("Set Angle: ",as.character(round(dr$set*(180/pi),2)))
    t.drift <- paste("Drift: ",as.character(round(dr$drift,2))," m/s")
    t.error <- paste("Error Distance: ",as.character(round(dr$errordistance,2))," m")
    title(paste(t.set," , ",t.drift,"\n",t.error, "\n","Course Steered (gray dash) & Course Made Good (black)"))
    grid()
  
  }
  
  # if the user passes a list
  if (length(ellipsis) > 0){
    
    if (is.null(ellipsis$xlab) == FALSE){xlabel = ellipsis$xlab}
    if (is.null(ellipsis$ylab) == FALSE){ylabel = ellipsis$ylab}
    if (is.null(ellipsis$asp) == FALSE){uasp = ellipsis$asp}
    
    
    plot(dr$CSx, dr$CSy, type='l',lty=2, col='gray',xlab=xlabel,
         ylab=ylabel,xlim=c(minx,maxx), ylim=c(miny,maxy), asp=uasp)
    lines(dr$CMGx,dr$CMGy, ...)
    t.set <- paste("Set Angle: ",as.character(round(dr$set*(180/pi),2)))
    t.drift <- paste("Drift: ",as.character(round(dr$drift,2))," m/s")
    t.error <- paste("Error Distance: ",as.character(round(dr$errordistance,2))," m")
    title(paste(t.set,"\n",t.drift,"\n",t.error))
    grid()
    
  }
  
  
  
}





