set.seed(1)
n <- 10000
wks <- 24

# PAs are fixed evenly each week
bSims <- dcb[as.numeric(rep(row.names(dcb), n)), ] %>%
  filter(PA > 50) %>%
  mutate(paSim = round(PA / wks))

# Simulate ABs
abSim <- rhyper(length(bSims$PA), bSims$AB, bSims$PA - bSims$AB, bSims$paSim)
bSims <- cbind(bSims, abSim)

# Not getting an AB will be a proxy for getting on base
noabSim <- bSims$paSim - bSims$abSim 
bSims <- cbind(bSims, noabSim)

# Simulate hits from ABs
hSim <- rhyper(length(bSims$PA), bSims$H, bSims$AB - bSims$H, bSims$abSim)
bSims <- cbind(bSims, hSim)

# Simulate home runs from hits
hrSim <- rhyper(length(bSims$PA), bSims$HR, bSims$H - bSims$HR, bSims$hSim)
bSims <- cbind(bSims, hrSim)

# Simulate steals from hits and the proxy for reaching base
sbSim <- rpois(bSims$hSim + bSims$noabSim, 
               bSims$SB / (bSims$H + bSims$PA - bSims$AB))

# Simulate runs from hits and the proxy for reaching base (not realistic)
rSim <- rpois(bSims$hSim + bSims$noabSim, 
               bSims$R / (bSims$H + bSims$PA - bSims$AB))

# Simulate rbis from hits and the proxy for reaching base (not realistic)
rbiSim <- rpois(bSims$hSim + bSims$noabSim, 
               bSims$RBI / (bSims$H + bSims$PA - bSims$AB))
bSims <- cbind(bSims, rbiSim, rSim, sbSim)




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
c <- b_sim(100, 24, dcb)
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
