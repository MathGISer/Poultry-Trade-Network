# Modified radiation model ------------------------------------------------
# To estimate trade flows among different cities
# Author: Lingcai Kong, konglc@ncepu.edu.cn
# 2022 Aug. 14



library(stplanr)
## od_radiation
# From stplanr v0.4.0 by Robin Lovelace
#



### A function to predict poultry trade flows (proxy variable) based on radiation model
### product_var : product of poultry for each city
### demand_var:   demand of poultry for each city
### sij:          the total demand in the circle of radius rij centered at i (excluding the source and destination of the trade flow)

radiation4tradeflows <-  function(p,
                                  product_var = "product",
                                  demand_var = "demand",
                                  proportion = 1)  {
  .Deprecated(msg = "See the od package")
  l <- points2flow(p)
  l$flow <- NA

  for (i in 1:nrow(p)) {
    for (j in 1:nrow(p)) {
      if (i == j)
        (next)()
      m <- p[[product_var]][i]
      n <- p[[demand_var]][j]
      sel_flow <- which(l$O == p@data[i, 1] & l$D == p@data[j,1])
      radius <- gprojected(shp = l[sel_flow, ], fun = rgeos::gLength)
      s_circle <- geo_buffer(shp = p[i, ], width = radius)
      p@proj4string <- s_circle@proj4string

      ps <- p[-c(i, j),][s_circle,]
      s <- sum(ps[[product_var]])        # demand_

      l$flow[sel_flow] <- p[[product_var]][i] * proportion *
        ((m * n) / ((m + s) * (m + n + s)))
    }
  }
  l
}

##--------------------------------------------------

