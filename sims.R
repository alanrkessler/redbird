set.seed(1)
n <- 10000

## Batters

bSims <- dcb[as.numeric(rep(row.names(dcb), n)), ] %>%
  filter(PA > 50)

# PAs are rounded up or down with probability based on decimal remaining
paSim <- floor(bSims$PA / wks) + rbinom(length(bSims$PA), 1, 
                                      bSims$PA / wks - floor(bSims$PA / wks))
bSims <- cbind(bSims, paSim)

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

# Simulate remaining from proxy (not very realistic)
rSim <- rpois(length(bSims$PA), 
              (bSims$hSim + bSims$noabSim) * bSims$R / (bSims$H + bSims$PA - bSims$AB))

rbiSim <- rpois(length(bSims$PA), 
                (bSims$hSim + bSims$noabSim) * bSims$RBI / (bSims$H + bSims$PA - bSims$AB))

sbSim <- rpois(length(bSims$PA), 
               (bSims$hSim + bSims$noabSim) * bSims$SB / (bSims$H + bSims$PA - bSims$AB))

bSims <- cbind(bSims, rSim, rbiSim, sbSim)

# Clean up workspace
rm(dcb, abSim, hrSim, hSim, noabSim, paSim, rbiSim, rSim, sbSim)

## Pitchers

pSims <- dcp[as.numeric(rep(row.names(dcp), n)), ] %>%
  filter(IP > 10)

# Games are rounded up or down with probability based on decimal remaining
gSim <- floor(pSims$G / wks) + rbinom(length(pSims$G), 1, 
                                      pSims$G / wks - floor(pSims$G / wks))
pSims <- cbind(pSims, gSim)

# Simulate wins from games
wSim <- rbinom(length(pSims$G), pSims$gSim, pSims$W / pSims$G)
pSims <- cbind(pSims, wSim)

# Simulate saves from non win games
svSim <- rbinom(length(pSims$G), pSims$gSim - pSims$wSim, pSims$SV / pSims$G)
pSims <- cbind(pSims, svSim)

# Simulate IP for each game
ipSim <- rpois(length(pSims$G), pSims$gSim * pSims$IP / pSims$G)
pSims <- cbind(pSims, ipSim)

# Simulate strikeouts from IP
soSim <- rbinom(length(pSims$G), pSims$ipSim * 3, pSims$SO / (pSims$IP * 3))

# Simulate ER for each IP
erSim <- rpois(length(pSims$G), pSims$ipSim * pSims$ER / pSims$IP)

# Simulate BB_H for each IP
bb_hSim <- rpois(length(pSims$G), pSims$ipSim * pSims$BB_H / pSims$IP)
pSims <- cbind(pSims, soSim, erSim, bb_hSim)

# Clean up workspace
rm(dcp, bb_hSim, erSim, gSim, ipSim, soSim, svSim, wSim)