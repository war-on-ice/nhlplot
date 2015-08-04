

#########################################################################
#
# Goldsberry plot of NHL shot data
#
 

#######################################################################
#Make hexagons.

#turn90 <- function (nby2) cbind(nby2[,2], -nby2[,1])

three.superblocks <- function () c(1,1,1,
                                   1,2,2,2,1,
                                   1,2,3,3,2,1,
                                   1,1)

ordinal.maker <- function (vec, cuts=quantile(vec, c(0, 0.35, 0.7), na.rm=TRUE)) {
  #vec=0:20; cuts=cutoffs
  apply(1*outer (vec, cuts, ">="), 1, sum, na.rm=TRUE)
}

ordinal.maker.zeroes <- function (vec, cuts=quantile(vec[vec>0], c(0, 0.35, 0.7))) {
  #vec=0:20; cuts=cutoffs
  r1 <- 0*vec
  r1[vec>0] <- apply(1*outer (vec[vec>0], cuts, ">="), 1, sum, na.rm=TRUE)
  r1
}

hexagon.coords.single <- function (x=0, y=0, radius=2.01) {
  coords <- t(radius*rbind(x=c(1,1,0,-1,-1,0),
                           y=c(1/sqrt(3), -1/sqrt(3), -2/sqrt(3), -1/sqrt(3), 1/sqrt(3), 2/sqrt(3))) + c(x,y))
  coords
}
hexagon.coords.auto <- function (xygrid, relative.size=0.8) {

  output <- matrix(NA, nrow=nrow(xygrid)*7, ncol=2)
  new.size <- relative.size*max(abs(xygrid[1,1] - xygrid[2,1]), abs(xygrid[1,2] - xygrid[2,2]))/2
  for (kk in 1:nrow(xygrid))
    output[1:6 + (kk-1)*7,] <- hexagon.coords.single (xygrid[kk,1], 
                                                      xygrid[kk,2],
                                                      new.size)
  return(output)
}

hexagon.coords.ordinal <- function (scatter.grid, sizes) {
    #print(sizes)
   
  new.size <- rep(0, length(sizes))
  new.size[sizes >= 1] <- new.size[sizes >= 1] + 0.3
  new.size[sizes >= 2] <- new.size[sizes >= 2] + 0.3
  new.size[sizes >= 3] <- new.size[sizes >= 3] + 0.3
  new.size <- new.size * scatter.grid$radius
  output <- matrix(NA, nrow=length(sizes)*7, ncol=2)
  for (kk in 1:length(sizes))
    output[1:6 + (kk-1)*7,] <- hexagon.coords.single (scatter.grid$grid[kk,1], 
                                                      scatter.grid$grid[kk,2],
                                                      new.size[kk])
  return(output)
}
hexagon.coords <- function (...) hexagon.coords.ordinal(...)

#######################################################################
#Bin into quadrilaterals.

in.triangle <- function (xyp, tr) {
  area <- (-tr[5]*tr[3] + tr[4]*(tr[3]-tr[2]) + tr[1]*(-tr[6]+tr[5]) + tr[2]*tr[6])/2
  ss <- 1/2/area * (tr[4]*tr[3] - tr[1]*tr[6] + (tr[6]-tr[4])*xyp[,1] + (tr[1]-tr[3])*xyp[,2])
  tt <- 1/2/area * (tr[1]*tr[5] - tr[4]*tr[2] + (tr[4]-tr[5])*xyp[,1] + (tr[2]-tr[1])*xyp[,2])
  output <- (ss > 0 & tt > 0 & 1 > ss+tt)
  return(output)
}
in.tri.rev <- function (tr=matrix(c(0,0.5,1, 0,1,0), nrow=3), xy.points) in.triangle (xy.points, tr)

pick.section.side <- function (xy.points) {
  data(quadsarrayplot)
  
  in.1 <- apply(quadsarrayplot[1:3,,], 3, in.tri.rev, xy.points)
  in.2 <- apply(quadsarrayplot[c(1,3,4),,], 3, in.tri.rev, xy.points)
  picks <- in.1 | in.2
  picks[is.na(picks)] <- FALSE
  
  picker <- function (row) if (sum(row)>0) min(which(row)) else 0
  sections <- apply (picks, 1, picker)
  return(sections)
}


############################################################################
# Point grid for binning.
point.grid <- function (xbins=31, xrange=c(-42.5, 42.5), yrange=c(-1,100)) {

  radius <- 85/(2*xbins)
  initial.x <- seq(xrange[1]+radius, xrange[2]-radius, length=xbins)
  shifted.x <- seq(xrange[1], xrange[2], length=xbins+1)
  yvalue <- yrange[1]+0.01
  scatter.grid <- NULL
  while (yvalue < yrange[2]) {
    scatter.grid <- rbind(scatter.grid,
                          cbind(initial.x, yvalue),
                          cbind(shifted.x, yvalue + sqrt(3)*radius))
    yvalue <- yvalue + sqrt(3)*radius*2
  }
  scatter.grid <- data.frame(plotting.x=scatter.grid[,1], plotting.y=scatter.grid[,2])
  scatter.grid <- subset(scatter.grid, plotting.y < yrange[2])

  uniques <- t(expand.grid(xx=(-42):42, yy=0:100))
  dist2 <- function (single.plot) which.min((scatter.grid[,1] - single.plot[1])^2 +
                                            (scatter.grid[,2] - single.plot[2])^2)
  bins <- apply(uniques, 2, dist2)

  zone.section <- pick.section.side (scatter.grid)
  zone.section.bigblock <- three.superblocks()[zone.section]
  
  output <- list(grid=scatter.grid,
                 uniques=cbind(t(uniques), bins),

                 zone.section=zone.section,
                 zone.unique=1:max(zone.section),

                 zone.section.bigblock=zone.section.bigblock,
                 zone.bigblock.unique=1:max(zone.section.bigblock),

                 xbins=xbins,
                 radius=radius)
                 
  return(output)

}

###################################################################################
# Actual binning.
# must go faster!

hexbin.quick <- function (xycoords, scatter.grid) {
    #xycoords = subdata[,c("newyc","newxc")]
    code.u <- scatter.grid$uniques[,1] + 100*scatter.grid$uniques[,2]
    code.xy <- xycoords[,1] + 100*xycoords[,2]
    scatter.grid$uniques[match(code.xy, code.u),3]
  ##return(table.complete(bins.2, nrow(scatter.grid$grid)))
}




table.complete <- function (vec, count) sapply(1:count, function (cc) sum(vec==cc, na.rm=TRUE))

nhl.hexbin <- function (xycoords, scatter.grid) { 
  code.u <- scatter.grid$uniques[,1] + 100*scatter.grid$uniques[,2]
  code.xy <- xycoords[,1] + 100*xycoords[,2]
  bins.2 <- scatter.grid$uniques[match(code.xy, code.u),3]
  return(table.complete(bins.2, nrow(scatter.grid$grid)))
}


nhl.zonebin.prime <- function (bin.counts, scatter.grid, use.superblocks=FALSE) {
  #sum up over bins in each zone. Redistribute over bins.

    if (use.superblocks) {
        retval <- sapply(scatter.grid$zone.bigblock.unique, function(bb)
                         sum(bin.counts[scatter.grid$zone.section.bigblock == bb], na.rm=TRUE))
    } else {
        retval <- sapply(scatter.grid$zone.unique, function(bb)
                         sum(bin.counts[scatter.grid$zone.section == bb], na.rm=TRUE))
    }
    return(retval)
    
}

nhl.zonebin <- function (bin.counts, scatter.grid, use.superblocks=FALSE) {
  #sum up over bins in each zone. Redistribute over bins.
    bin.overall <- nhl.zonebin.prime (bin.counts, scatter.grid, use.superblocks)
    output <- rep(NA, nrow(scatter.grid$grid))
    if (use.superblocks) {
        for (bb in 1:length(bin.overall)) output[scatter.grid$zone.section.bigblock == bb] <- bin.overall[bb]
    } else {
        for (bb in 1:length(bin.overall)) output[scatter.grid$zone.section == bb] <- bin.overall[bb]
    }
    output
}



rink.hexplot <- function (scatter.grid, sizes, colors, bordercolor=NA, ...) {

  #sizes is now ordinal: 0=none, 1=0.4, 2=0.8, 3=0.8 black border
    par(mar=c(0, 0, 3, 0))
    rink.plot(...) #...)
    hex.coords <- hexagon.coords.ordinal(scatter.grid, sizes)
    colors[sizes==0] <- 0
    
    bordercolor <- rep(bordercolor, length(colors)); bordercolor[sizes == 0] <- NA
    polygon(hex.coords, col=colors, border=bordercolor, lwd=2)

}

rink.hexplot.auto <- function (scatter.grid, sizes, colors, ...) {
    rink.hexplot (scatter.grid, sizes=ordinal.maker.zeroes(sizes), colors, ...)
}

shot.bin.set <- function (event.df,
                          scatter.grid=point.grid(),
                          coordnames=c("ycoord","xcoord")) {
    ## event.df=player.events; coordnames=coordset()
  #message(coordnames)
  all.shots <- nhl.hexbin (event.df[,coordnames], scatter.grid)
  all.goals <- nhl.hexbin (subset(event.df, etype=="GOAL")[,coordnames], scatter.grid)
  
  bin.shots <- nhl.zonebin (all.shots, scatter.grid)
  bin.goals <- nhl.zonebin (all.goals, scatter.grid)

  superbin.shots <- nhl.zonebin (all.shots, scatter.grid, use.superblocks=TRUE)
  superbin.goals <- nhl.zonebin (all.goals, scatter.grid, use.superblocks=TRUE)
  

  return(cbind(all.shots=all.shots,
               all.goals=all.goals,
               
               binned.frac=bin.goals/bin.shots,
               
               bin.shots=bin.shots,
               bin.goals=bin.goals,

               superbin.shots=superbin.shots,
               superbin.goals=superbin.goals

               ))
  
}

shot.bin.set.blocks <- function (event.df,
                                 scatter.grid=point.grid(),
                                 coordnames=c("ycoord","xcoord")
                                 , use.superblocks=FALSE
                                 ) {

  #message(coordnames)
  all.shots <- nhl.zonebin.prime(nhl.hexbin (event.df[,coordnames], scatter.grid),
                                 scatter.grid, use.superblocks)
  all.goals <- nhl.zonebin.prime(nhl.hexbin (subset(event.df, etype=="GOAL")[,coordnames], scatter.grid),
                                 scatter.grid, use.superblocks)
  
  return(cbind(all.shots=all.shots,
               all.goals=all.goals))
  
}









