
team.rosters.by.year.old <- function (mega.file="nhlscrapr-probs.RData",
                                  output.file="common-data/hextally-roster-players.RData",
                                  season.pick=c("20082009", "20092010", "20102011", "20112012", "20122013", "20132014", "20142015")
                                  ) {

  load (mega.file)
  
  roster.unique <- roster.master[match(1:max(roster.master$player.id), roster.master$player.id),]
  teams <- sort(unique(c(grand.data$hometeam, grand.data$awayteam)))

  home.cols <- c("h1","h2","h3","h4","h5","h6", "home.G")
  away.cols <- c("a1","a2","a3","a4","a5","a6", "away.G")

  season.rosters <- list()
  for (ss in 1:length(season.pick)) {
    message(season.pick[ss])
    team.roster <- list()
    subs <- subset(grand.data, season==season.pick[ss])

    for (tt in 1:length(teams)) 
      team.roster[[tt]] <- 
        unique(c(as.matrix(subs[subs$hometeam==teams[tt], home.cols]),
                 as.matrix(subs[subs$awayteam==teams[tt], away.cols])))
    names(team.rosters) <- teams
    
    season.rosters[[ss]] <- team.roster
  }
  
  save(teams, season.pick, season.rosters, file=output.file)
  
}
                                  


get.shots.players.old <- function (mega.file="nhlscrapr-probs.RData",
                               output.file="common-data/shots-players.RData",
                               do.es=TRUE,
                               do.pp=TRUE,
                               do.four=TRUE) {

  load (mega.file)
  season.pick <- c("20082009", "20092010", "20102011", "20112012", "20122013", "20132014")
  roster.unique <- roster.master[match(1:max(roster.master$player.id), roster.master$player.id),]
  grand.data$gcode <- as.character(grand.data$gcode)

  home.cols <- c("h1","h2","h3","h4","h5","h6", "home.G")
  away.cols <- c("a1","a2","a3","a4","a5","a6", "away.G")
  player.cols <- c(home.cols, away.cols)

  
  es.shots.prep <- function (datablock,
                             playerblock) {
    ## datablock = subset(grand.data, season %in% season.pick & home.skaters==6 & away.skaters==6 & home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"); playerblock=subset(grand.data, season %in% season.pick & substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)]
    
  ## get time on ice for each player in each season.
    time.off.ice.home <- time.off.ice.away <- time.on.ice.home <- time.on.ice.away <- matrix(0, nrow=nrow(roster.unique), ncol=length(season.pick))
  # games
    home.played <- away.played <- list(); away.played[[nrow(roster.unique)+1]] <- 1; home.played[[nrow(roster.unique)+1]] <- 1

    total.time.by.game <- matrix(0, nrow=length(unique(datablock$gcode)), ncol=length(season.pick))
    rownames(total.time.by.game) <- as.character(unique(datablock$gcode))
    
    for (ss in 1:length(season.pick)) {
      ss.reduced <- subset(datablock, season==season.pick[ss])
      pl.reduced <- subset(playerblock, season==season.pick[ss])
      
      fullgames <- by(ss.reduced, ss.reduced$gcode, identity)
      game.players <- by(pl.reduced, pl.reduced$gcode, identity)
      
      t1 <- sapply (fullgames, function(gg) {

        thisone <- paste0(season.pick[ss], gg$gcode[1])
        cat(as.character(gg$gcode[1]), " ")
        ggplcol <- as.matrix(gg[,player.cols])
        gghomecol <- as.matrix(gg[,home.cols])
        ggawaycol <- as.matrix(gg[,away.cols])

        obj <- game.players[[which(names(game.players) == gg$gcode[1])]]
        
        home.players <- unique(c(as.matrix(obj[,home.cols])))
        for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
        
        away.players <- unique(c(as.matrix(obj[,away.cols])))
        for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)
        
        time.on.home <- sapply(home.players, function(pp) sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
        time.on.away <- sapply(away.players, function(pp) sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
        
        time.off.home <- sum(gg$event.length) - time.on.home
        time.off.away <- sum(gg$event.length) - time.on.away
        
        time.on.ice.home[home.players,ss] <<- time.on.ice.home[home.players,ss] + time.on.home
        time.on.ice.away[away.players,ss] <<- time.on.ice.away[away.players,ss] + time.on.away
        
        time.off.ice.home[home.players,ss] <<- time.off.ice.home[home.players,ss] + time.off.home
        time.off.ice.away[away.players,ss] <<- time.off.ice.away[away.players,ss] + time.off.away
      })

      time.by.game <- tapply (ss.reduced$event.length, as.character(ss.reduced$gcode), sum)
      total.time.by.game[match(names(time.by.game), rownames(total.time.by.game)), ss] <- time.by.game
    }
    
    shots <- subset(datablock, !is.na(xcoord) & etype %in% c("GOAL","MISS","SHOT","BLOCK"))
    flips <- which(shots$xcoord<0)
    shots$xcoord[flips] <- -shots$xcoord[flips]; shots$ycoord[flips] <- -shots$ycoord[flips]
    flips <- which(shots$newxc<0)
    shots$newxc[flips] <- -shots$newxc[flips]; shots$newyc[flips] <- -shots$newyc[flips]

  
    current.seasons.es <- shots[,c("season", "gcode", "period", "etype", "ev.team", "ev.player.1",
                                   "type", "xcoord", "ycoord", "loc.section", "awayteam", "hometeam",
                                   "newxc", "newyc", "new.loc.section",
                                   "away.score", "home.score",
                                   "away.G", "home.G", "home.skaters", "away.skaters", player.cols)]
    scatter.grid <- point.grid()
  
    seasonal.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set, scatter.grid)
    seasonal.bin.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set.blocks, scatter.grid)
    seasonal.superbin.counts <- by(current.seasons.es, as.character(current.seasons.es$season), shot.bin.set.blocks, scatter.grid, use.superblocks=TRUE)

    #overall.counts <- shot.bin.set (current.seasons.es, scatter.grid)
    #overall.bin.counts <- shot.bin.set.blocks (current.seasons.es, scatter.grid)
    
    quants <- list(shots=current.seasons.es,
                      
                   time.on.ice.home.O=time.on.ice.home,
                   time.on.ice.home.D=time.on.ice.home,
                   time.on.ice.away.O=time.on.ice.away,
                   time.on.ice.away.D=time.on.ice.away,
                      
                   time.off.ice.home.O=time.off.ice.home,
                   time.off.ice.home.D=time.off.ice.home,
                   time.off.ice.away.O=time.off.ice.away,
                   time.off.ice.away.D=time.off.ice.away,

                   total.time.by.game.home.O=total.time.by.game,
                   total.time.by.game.away.O=total.time.by.game,
                   
                   home.played=home.played,
                   away.played=away.played,
                      
                   scatter.grid=scatter.grid,
                   
                   seasonal.counts=seasonal.counts,
                   seasonal.bin.counts=seasonal.bin.counts,
                   seasonal.superbin.counts=seasonal.superbin.counts
                      
                      #,overall.counts=overall.counts
                      #,overall.bin.counts=overall.bin.counts
                   )
    
    return(quants)
  }

  if (do.es) {
    es.quants.s <- es.shots.prep(subset(grand.data, season %in% season.pick &
                                        home.skaters==6 & away.skaters==6 &
                                        home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                 subset(grand.data, season %in% season.pick &
                                        substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])
    
    es.quants.p <- es.shots.prep(subset(grand.data, season %in% season.pick &
                                        home.skaters==6 & away.skaters==6 &
                                        home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                 subset(grand.data, season %in% season.pick &
                                        substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
  } else {es.quants.s <- es.quants.p <- NULL}

  if (do.four) {
    four.quants.s <- es.shots.prep(subset(grand.data, season %in% season.pick &
                                          home.skaters==5 & away.skaters==5 &
                                          home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                   subset(grand.data, season %in% season.pick &
                                          substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])

    four.quants.p <- es.shots.prep(subset(grand.data, season %in% season.pick &
                                          home.skaters==5 & away.skaters==5 &
                                          home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                   subset(grand.data, season %in% season.pick &
                                          substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
  } else {four.quants.s <- four.quants.p <- NULL}

  pp.shots.prep <- function (datablock,
                             playerblock) {
  
    datablock.home <- subset(datablock, home.skaters == 6)
    datablock.away <- subset(datablock, home.skaters < 6)
        
    time.off.ice.home.O <- time.off.ice.away.O <- time.on.ice.home.O <- time.on.ice.away.O <-
      time.off.ice.home.D <- time.off.ice.away.D <- time.on.ice.home.D <- time.on.ice.away.D <-
        matrix(0, nrow=nrow(roster.unique), ncol=length(season.pick))
  # games
    home.played <- away.played <- list(); away.played[[nrow(roster.unique)+1]] <- 1; home.played[[nrow(roster.unique)+1]] <- 1

    total.time.by.game.home.O <- matrix(0, nrow=length(unique(datablock$gcode)), ncol=length(season.pick))
    rownames(total.time.by.game.home.O) <- as.character(unique(datablock$gcode))
    total.time.by.game.away.O <- total.time.by.game.home.O
    
    for (ss in 1:length(season.pick)) {
      pl.reduced <- subset(playerblock, season==season.pick[ss])
      game.players <- by(pl.reduced, pl.reduced$gcode, identity)

      ss.reduced.home <- subset(datablock.home, season==season.pick[ss])
      ss.reduced.away <- subset(datablock.away, season==season.pick[ss])

      t1 <- by (ss.reduced.home, ss.reduced.home$gcode, function(gg) {

        thisone <- paste0(season.pick[ss], gg$gcode[1])
        cat(as.character(gg$gcode[1]), " ")
        ggplcol <- as.matrix(gg[,player.cols])
        gghomecol <- as.matrix(gg[,home.cols])
        ggawaycol <- as.matrix(gg[,away.cols])

        obj <- game.players[[which(names(game.players) == gg$gcode[1])]]
        home.players <- unique(c(as.matrix(obj[,home.cols])))
        for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
        away.players <- unique(c(as.matrix(obj[,away.cols])))
        for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)
        
        time.on.home <- sapply(home.players, function(pp) sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
        time.on.away <- sapply(away.players, function(pp) sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
        
        time.off.home <- sum(gg$event.length) - time.on.home  #.O
        time.off.away <- sum(gg$event.length) - time.on.away  #.D
        
        time.on.ice.home.O[home.players,ss] <<- time.on.ice.home.O[home.players,ss] + time.on.home
        time.on.ice.away.D[away.players,ss] <<- time.on.ice.away.D[away.players,ss] + time.on.away
        
        time.off.ice.home.O[home.players,ss] <<- time.off.ice.home.O[home.players,ss] + time.off.home
        time.off.ice.away.D[away.players,ss] <<- time.off.ice.away.D[away.players,ss] + time.off.away
      
      })
      time.by.game <- tapply (ss.reduced.home$event.length, as.character(ss.reduced.home$gcode), sum)
      total.time.by.game.home.O[match(names(time.by.game), rownames(total.time.by.game.home.O)), ss] <- time.by.game

      t1 <- by (ss.reduced.away, ss.reduced.away$gcode, function(gg) {
        ##gg <- subset(ss.reduced.away, gcode == "20001")
        thisone <- paste0(season.pick[ss], gg$gcode[1])
        cat(as.character(gg$gcode[1]), " ")
        ggplcol <- as.matrix(gg[,player.cols])
        gghomecol <- as.matrix(gg[,home.cols])
        ggawaycol <- as.matrix(gg[,away.cols])

        obj <- game.players[[which(names(game.players) == gg$gcode[1])]]
        home.players <- unique(c(as.matrix(obj[,home.cols])))
        for (kk in home.players) home.played[[kk]] <<- c(home.played[[kk]], thisone)
        away.players <- unique(c(as.matrix(obj[,away.cols])))
        for (kk in away.players) away.played[[kk]] <<- c(away.played[[kk]], thisone)

        time.on.home <- sapply(home.players, function(pp)
                               sum(gg$event.length[apply(gghomecol==pp, 1, sum) > 0]))
        time.on.away <- sapply(away.players, function(pp)
                               sum(gg$event.length[apply(ggawaycol==pp, 1, sum) > 0]))
        
        time.off.home <- sum(gg$event.length) - time.on.home
        time.off.away <- sum(gg$event.length) - time.on.away
        
        time.on.ice.home.D[home.players,ss] <<- time.on.ice.home.D[home.players,ss] + time.on.home
        time.on.ice.away.O[away.players,ss] <<- time.on.ice.away.O[away.players,ss] + time.on.away
        
        time.off.ice.home.D[home.players,ss] <<- time.off.ice.home.D[home.players,ss] + time.off.home
        time.off.ice.away.O[away.players,ss] <<- time.off.ice.away.O[away.players,ss] + time.off.away

      })
      time.by.game <- tapply (ss.reduced.away$event.length, as.character(ss.reduced.away$gcode), sum)
      total.time.by.game.away.O[match(names(time.by.game), rownames(total.time.by.game.away.O)), ss] <- time.by.game
     
    }
    
    shots <- subset(datablock, !is.na(xcoord) & etype %in% c("GOAL","MISS","SHOT","BLOCK"))
    flips <- which(shots$xcoord<0)
    shots$xcoord[flips] <- -shots$xcoord[flips]; shots$ycoord[flips] <- -shots$ycoord[flips]
    flips <- which(shots$newxc<0)
    shots$newxc[flips] <- -shots$newxc[flips]; shots$newyc[flips] <- -shots$newyc[flips]

  
    current.seasons.pp <- shots[,c("season", "gcode", "period", "etype", "ev.team", "ev.player.1",
                                   "type", "xcoord", "ycoord", "loc.section", "awayteam", "hometeam",
                                   "newxc", "newyc", "new.loc.section",
                                   "away.score", "home.score",
                                   "away.G", "home.G", "home.skaters", "away.skaters", player.cols)]
    scatter.grid <- point.grid()
  
    seasonal.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season),
                          shot.bin.set, scatter.grid, c("newyc","newxc"))
    seasonal.bin.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season),
                              shot.bin.set.blocks, scatter.grid, c("newyc","newxc"))
    seasonal.superbin.counts <- by(current.seasons.pp, as.character(current.seasons.pp$season), shot.bin.set.blocks, scatter.grid, c("newyc","newxc"), use.superblocks=TRUE)

    quants <- list(shots=current.seasons.pp,
                      
                   time.on.ice.home.O=time.on.ice.home.O,
                   time.on.ice.home.D=time.on.ice.home.D,
                   time.on.ice.away.O=time.on.ice.away.O,
                   time.on.ice.away.D=time.on.ice.away.D,
                      
                   time.off.ice.home.O=time.off.ice.home.O,
                   time.off.ice.home.D=time.off.ice.home.D,
                   time.off.ice.away.O=time.off.ice.away.O,
                   time.off.ice.away.D=time.off.ice.away.D,
                      
                   total.time.by.game.home.O=total.time.by.game.home.O,
                   total.time.by.game.away.O=total.time.by.game.away.O,
                   
                   home.played=home.played,
                   away.played=away.played,
                      
                   scatter.grid=scatter.grid,
                   
                   seasonal.counts=seasonal.counts,
                   seasonal.bin.counts=seasonal.bin.counts,
                   seasonal.superbin.counts=seasonal.superbin.counts
                      
                      #,overall.counts=overall.counts
                      #,overall.bin.counts=overall.bin.counts
                   )
    
    return(quants)
  }

  if (do.pp) {
    pp.quants.s <- pp.shots.prep(subset(grand.data, season %in% season.pick &
                                        ((home.skaters == 6 & away.skaters < 6) | (home.skaters < 6 & away.skaters == 6)) &
                                        home.G > 1 & away.G > 1 & substr(gcode,1,1) == "2"),
                                 subset(grand.data, season %in% season.pick & substr(gcode,1,1) == "2")[,c("season","gcode",player.cols)])
    
    pp.quants.p <- pp.shots.prep(subset(grand.data, season %in% season.pick &
                                        ((home.skaters == 6 & away.skaters < 6) | (home.skaters < 6 & away.skaters == 6)) &
                                        home.G > 1 & away.G > 1 & substr(gcode,1,1) == "3"),
                                 subset(grand.data, season %in% season.pick & substr(gcode,1,1) == "3")[,c("season","gcode",player.cols)])
  } else {pp.quants.s <- pp.quants.p <- NULL}
          
  save(pp.quants.s, pp.quants.p,
       es.quants.s, es.quants.p,
       four.quants.s, four.quants.p,
       roster.unique,
       file=output.file)

}




#library(data.table)
#library(doMC); registerDoMC(4)
reprocess.players <- function (input.stem="common-data/hextally-players-",
                               output.folder="common-data/hextally-players",
                               redo.players=TRUE) {

    player.cols <- c("h1","h2","h3","h4","h5","h6", "home.G", "a1","a2","a3","a4","a5","a6", "away.G")
    load (input.file)
    
    splits <- function (shots1) by(shots1, paste0(shots1$season, shots1$gcode), identity)
    
    drop.to.n <- function (pp, object, index) {
        sgc <- paste0(object$shots$season, object$shots$gcode)
        list(shots=object$shots[(sgc %in% object$home.played[[pp]]) | (sgc %in% object$away.played[[pp]]), ],
             
             time.on.ice.home.O=object$time.on.ice.home.O[pp,,drop=FALSE],
             time.on.ice.home.D=object$time.on.ice.home.D[pp,,drop=FALSE],
             time.on.ice.away.O=object$time.on.ice.away.O[pp,,drop=FALSE],
             time.on.ice.away.D=object$time.on.ice.away.D[pp,,drop=FALSE],
             
             time.off.ice.home.O=object$time.off.ice.home.O[pp,,drop=FALSE],
             time.off.ice.home.D=object$time.off.ice.home.D[pp,,drop=FALSE],
             time.off.ice.away.O=object$time.off.ice.away.O[pp,,drop=FALSE],
             time.off.ice.away.D=object$time.off.ice.away.D[pp,,drop=FALSE],
             
             home.played=object$home.played[[pp]],
             away.played=object$away.played[[pp]])
    }
    
                                        #foreach (kk = 2:nrow(roster.unique)) %dopar% {0
    library(doMC)
    registerDoMC(1)
    if (redo.players) foreach (kk = 2:nrow(roster.unique)) %do% {
        this.record <- list(
            pp.quants.s=drop.to.n(kk, pp.quants.s, 1),
            pp.quants.p=drop.to.n(kk, pp.quants.p, 2),
            es.quants.s=drop.to.n(kk, es.quants.s, 3),
            es.quants.p=drop.to.n(kk, es.quants.p, 4),
            four.quants.s=drop.to.n(kk, four.quants.s, 5),
            four.quants.p=drop.to.n(kk, four.quants.p, 6)
            )
        save(this.record, file=paste0(output.folder,"/hextally-p", kk, ".RData"))
        print(kk)
    }
    
    drop.to.main <- function (object)
        list(total.time.by.game.home.O=object$total.time.by.game.home.O,
             total.time.by.game.away.O=object$total.time.by.game.away.O,
             
             scatter.grid=object$scatter.grid,
             time.off.ice.home.O=object$time.off.ice.home.O,
             
             seasonal.counts=object$seasonal.counts,
             seasonal.bin.counts=object$seasonal.bin.counts,
             seasonal.superbin.counts=object$seasonal.superbin.counts)
    
    
    pp.quants.s <- drop.to.main(pp.quants.s)
    pp.quants.p <- drop.to.main(pp.quants.p)
    es.quants.s <- drop.to.main(es.quants.s)
    es.quants.p <- drop.to.main(es.quants.p)
    four.quants.s <- drop.to.main(four.quants.s)
    four.quants.p <- drop.to.main(four.quants.p)
    
    save (pp.quants.s, pp.quants.p,
          es.quants.s, es.quants.p,
          four.quants.s, four.quants.p,
          roster.unique,
          file=paste0(output.folder,"/hextally-p-core.RData"))
    
}

