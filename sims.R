# Batter simulations
b_sim <- function(n, wks, df) {
  df <- df[df$PA > 50, ]
  abSim <- rep(df$AB / wks, n)
  hSim <- rpois(n*length(df$HR), df$H / (df$PA / wks))
  hrSim <- NULL
  for (i in hSim){
    hrSim <- rbind(hrSim, df$HR / df$H)
  }
  hrSim <- rbinom(1, hSim, df$HR / df$H) 
  # Apply a rough flattening?
  # Assume games per week and PAs per game are constant
  # Simulate Hits per PA df$H / (df$PA / wks)
  # Simulate HRs per Hit (df$HR / df$H) * hSim
  # Simulate other ways to get on base - get (OBP * PA - H) / (df$PA / wks)
  # Simulate SB per all ways to get on base (df$SB / df$onbase) * obSim
  # Simulate Runs per all ways to get on base (df$R / df$onbase) * obSim
  # Simulate RBIs per all ways to get on base (df$RBI / df$onbase) * obSim
  
  rSim <- rpois(n*length(df$HR), df$R / (df$PA / wks))
  rbiSim <- rpois(n*length(df$HR), df$RBI / (df$PA / wks))
  sbSim <- rpois(n*length(df$HR), df$SB / (df$PA / wks))
  hSim <- rpois(n*length(df$HR), df$H / (df$PA / wks))
  SimNum <- rep(1:n, length(df$HR))
  df <- df[as.numeric(rep(row.names(df), n)), ] 
  return(cbind(SimNum, df, hrSim, rSim, rbiSim, sbSim, hSim, abSim))
}

set.seed(1)
df <- dcb[dcb$PA > 50, ]
hSim <- rpois(100*length(df$HR), df$H / (df$PA / wks))
a <- NULL
for (i in hSim){
  a <- rbind(a, rbinom(1, i, 0.25))
}


# Pitcher simulations
c("IP", "W", "SV", "SO", "ER", "BB_H")
p_sim <- function(n, wks, df) {
  df <- df[df$IP > 10, ]
  wSim <- rpois(n*length(df$W), df$W / (df$IP / wks))
  svSim <- rpois(n*length(df$W), df$SV / (df$IP / wks))
  soSim <- rpois(n*length(df$W), df$SO / (df$IP / wks))
  erSim <- rpois(n*length(df$W), df$ER / (df$IP / wks))
  bb_hSim <- rpois(n*length(df$W), df$BB_H / (df$IP / wks))
  ipSim <- rep(df$IP / wks, n)
  SimNum <- rep(1:n, length(df$W))
  df <- df[as.numeric(rep(row.names(df), n)), ] 
  return(cbind(SimNum, df, wSim, svSim, soSim, erSim, bb_hSim, ipSim))
}

set.seed(2)
c <- b_sim(1000, 24, dcb)
d <- p_sim(1000, 24, dcp)

#example data
df <- data.frame(country=c("a", "b", "c"), 
                 mean=c(1, 10, 100), 
                 sd=c(1, 2, 10))

#function which returns a matrix, and takes column vectors as arguments for mean and sd
normv <- function( n , mean , sd ){
  out <- rpois( n*length(mean) , mean)
  df <- df[as.numeric(rep(row.names(df), n)), ]
  return( cbind(df, out))
}

#reproducible result (note order of magnitude of rows and input sample data)
set.seed(1)
b <- normv( 5 , df$mean , df$sd )
