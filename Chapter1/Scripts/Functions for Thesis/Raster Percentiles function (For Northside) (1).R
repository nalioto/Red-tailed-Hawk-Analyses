### ------------------------------- ###
### Function for Raster Percentiles ###
### By: Nick Jaffe, For: N. Alioto  ###
### ------------------------------- ###

library(raster)

## Inputs 
# r: your raster (should sum to 1)
# prc: a vector of percentiles (between 0:1)

UD.Perc <- function(r, prc){
  
  r.stk = stack()
  
  for(i in prc){
    
    thrsh = sort(r[])[max(which(cumsum(sort(r[])) <= 1-i))]
    r.stk = stack(r.stk,(r > thrsh))

  }
  
  names(r.stk) <- paste("P",prc*100,sep="")
  return(r.stk)
  
}

# (Bad) Example 
r1 = raster(volcano); r1 = r1/sum(r1[])
op = UD.Perc(r=r1, prc = c(0.95,0.75,0.5))

plot(op) # Note the raster names in the plot titles
plot(sum(op)) # Can combine all three (note: you will prob. have to edit the labels)


