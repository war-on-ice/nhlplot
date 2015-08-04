



comparisons <- function () {

  current.season <- subset(shots, season=="20132014")
  xbins <- 31
  scatter.grid <- point.grid(xbins)

  master.colors <- colorRampPalette(c("green", "orange", "red", "pink"))(100)

  pdf ("all-stuff-1.pdf", width=10, height=8)
  
  hexbins.total <- nhl.hexbin (current.season[,c("ycoord","xcoord")], scatter.grid)
  hexbins.goals <- nhl.hexbin (current.season[current.season$etype=="GOAL",c("ycoord","xcoord")], scatter.grid)
  frac <- hexbins.goals/hexbins.total; frac[is.na(frac)] <- 0
  rink.hexplot (scatter.grid, sizes=hexbins.total, colors=master.colors[frac*100+1],
                saturat=0.2, main="All")
  dev.off()
  for (tt in sort(unique(current.season$ev.team))) {
    team.for <- subset(current.season, ev.team==tt)
    hexbins.total <- nhl.hexbin (team.for[,c("ycoord","xcoord")], scatter.grid)
    hexbins.goals <- nhl.hexbin (team.for[d.slapshots$etype=="GOAL",c("ycoord","xcoord")], scatter.grid)
    frac <- hexbins.goals/hexbins.total
    frac[is.na(frac)] <- 0
    rink.hexplot (scatter.grid, sizes=hexbins.total, colors=master.colors[frac*100+1], saturat=0.5, main=paste(tt,"For"))

    
    team.against <- subset(current.season, (hometeam==tt | awayteam==tt) & ev.team!=tt)
    hexbins.total <- nhl.hexbin (team.against[,c("ycoord","xcoord")], scatter.grid)
    hexbins.goals <- nhl.hexbin (team.against[d.slapshots$etype=="GOAL",c("ycoord","xcoord")], scatter.grid)
    frac <- hexbins.goals/hexbins.total
    frac[is.na(frac)] <- 0
    rink.hexplot (scatter.grid, sizes=hexbins.total, colors=master.colors[frac*100+1], saturat=0.5, main=paste(tt,"Against"))
   
  }

  dev.off()




}









stuff <- function () {
  
  current.season <- subset(shots, season=="20132014")
  table(current.season$etype)
  xbins <- 61
  scatter.grid <- point.grid(xbins)

  master.colors <- colorRampPalette(c("green", "orange", "red", "pink"))(100)

  hexbins.total <- nhl.hexbin (current.season[,c("ycoord","xcoord")], scatter.grid)
  hexbins.goals <- nhl.hexbin (current.season[current.season$etype=="GOAL",c("ycoord","xcoord")], scatter.grid)
  frac <- hexbins.goals/hexbins.total
  frac[is.na(frac)] <- 0
  rink.hexplot (scatter.grid, sizes=hexbins.total, colors=rgb(frac^(1/5),0,0), saturat=0.2, main="All")
  
  rink.hexplot (scatter.grid, sizes=rep(1,nrow(scatter.grid)), colors=master.colors[frac*100+1], radius, saturat=1, main="All Slapshots")

  
  pdf("shot-comparisons.pdf")
  
  slapshots <- subset(current.season, type=="Slap")
  hexbins.total <- nhl.hexbin (slapshots[,c("ycoord","xcoord")], scatter.grid)
  hexbins.goals <- nhl.hexbin (slapshots[slapshots$etype=="GOAL",c("ycoord","xcoord")], scatter.grid)
  frac <- hexbins.goals/hexbins.total
  frac[is.na(frac)] <- 0
  
  rink.hexplot (scatter.grid, sizes=hexbins.total, colors=rgb(frac^(1/5),0,0), saturat=0.2, main="All Slapshots")
  
  rink.hexplot (scatter.grid, sizes=rep(1,nrow(scatter.grid)), colors=master.colors[frac*100+1], radius, saturat=1, main="All Slapshots")


  
  
  d.slapshots <- subset(slapshots, ev.team=="N.J")
  hexbins.total <- nhl.hexbin (d.slapshots[,c("ycoord","xcoord")], scatter.grid)
  hexbins.goals <- nhl.hexbin (d.slapshots[d.slapshots$etype=="GOAL",c("ycoord","xcoord")], scatter.grid)
  frac <- hexbins.goals/hexbins.total
  frac[is.na(frac)] <- 0
  rink.hexplot (scatter.grid, sizes=hexbins.total, colors=rgb(frac^(1/5),0,0), saturat=0.5, main="NJD Slapshots")


  
  wristshots <- subset(current.season, type=="Snap")
  hexbins.total <- nhl.hexbin (wristshots[,c("ycoord","xcoord")], scatter.grid)
  hexbins.goals <- nhl.hexbin (wristshots[wristshots$etype=="GOAL",c("ycoord","xcoord")], scatter.grid)
  frac <- hexbins.goals/hexbins.total
  frac[is.na(frac)] <- 0
  rink.hexplot (scatter.grid, sizes=hexbins.total, colors=rgb(frac^(1/5),0,0), saturat=0.4, main="All Wristshots")

  
  d.wristshots <- subset(wristshots, ev.team=="TOR")
  hexbins.total <- nhl.hexbin (d.wristshots[,c("ycoord","xcoord")], scatter.grid)
  hexbins.goals <- nhl.hexbin (d.wristshots[d.wristshots$etype=="GOAL",c("ycoord","xcoord")], scatter.grid)
  frac <- hexbins.goals/hexbins.total
  frac[is.na(frac)] <- 0
  rink.hexplot (scatter.grid, sizes=hexbins.total, colors=rgb(frac^(1/5),0,0), saturat=0.4, main="NJD Wristshots")

  dev.off()
  
}













                       
