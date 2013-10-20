# plot using saved data

load("res.dat")
#save(s, pstar, v, f, c, b, all_time, file="res.dat")

# --- plot ---
library(rgl)
# xvec = rep(s, each=pstar_size)
# yvec = rep(pstar, times=s_size)
# plot3d(xvec, yvec, v[ , , 1], xlab="s", ylab="pstar", zlab="v", box = TRUE, axes = TRUE)
# lines3d(xvec, yvec, v[ , , 1], xlab="s", ylab="pstar", zlab="v", box = TRUE, axes = TRUE)

xvec = s
yvec = pstar
zmat = c[ , , 1]
zlim = range(zmat)
zlen = zlim[2] - zlim[1] + 1
colorlut = terrain.colors(zlen) # height color lookup table
col = colorlut[ zmat-zlim[1]+1 ] # assign colors to heights for each point
surface3d(xvec, yvec, zmat, color=col, back="lines", xlab="s", ylab="pstar", zlab="v", box = TRUE, axes = TRUE)
decorate3d(main = "s-pstar-v", aspect=TRUE)
rgl.postscript( "s-pstar-v.eps", fmt="eps", drawText=TRUE )
rgl.snapshot("s-pstar-v.png", fmt="png", top=TRUE )