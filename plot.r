# plot using saved data

# NDIM = 500
# NTIMEUP = 80
# NTIMELOW = 20
# STEP = 10

# load("res_1102.dat")
# print("load done")

# save(s, pstar, v, f, c, b, optimal_stock_holding, file = "res_tiny.dat")

# --- plot ---
library(rgl)


tvec = seq(1, NTIMEUP-1, by=1)
s_idx = round(0.2 / (1/NDIM)) + 1 

# xvec = rep(pstar, each=pstar_size)
# yvec = rep(tvec, times=length(tvec))
# plot3d(xvec, yvec, c[ s_idx, , tvec], xlab="s", ylab="pstar", zlab="v", box = TRUE, axes = TRUE)
# lines3d(xvec, yvec, c[ s_idx, , tvec], xlab="s", ylab="pstar", zlab="v", box = TRUE, axes = TRUE)

# --- optimal consumption policy ---
# check zmat to determine x and y 
# xvec_range = tvec
# yvec_range = pstar
# zmat = c[ s_idx, , tvec]
# zmat = aperm(zmat)
# persp3d(xvec_range, yvec_range, zmat, col = 'skyblue',aspect=TRUE, xlab="t", ylab="pstar", zlab="c", box = TRUE, axes = TRUE) 
# surface3d(xvec_range, yvec_range, zmat, front="lines", back="lines")
# # rgl.postscript( "t-pstar-c.eps", fmt="eps", drawText=TRUE )
# rgl.snapshot("t-pstar-c.png", fmt="png", top=TRUE )

# --- optimal stock holding ---
xvec_range = tvec
yvec_range = pstar
zmat = optimal_stock_holding[s_idx, , tvec]
zmat = aperm(zmat)
persp3d(xvec_range, yvec_range, zmat, col = 'lightblue',aspect=TRUE, xlab="t", ylab="pstar", zlab="optimal_stock_holding", box = TRUE, axes = TRUE) 
surface3d(xvec_range, yvec_range, zmat, front="lines", back="lines")
# rgl.postscript( "t-pstar-optimal_stock_holding.eps", fmt="eps", drawText=TRUE )
rgl.snapshot("t-pstar-optimal_stock_holding.png", fmt="png", top=TRUE )



