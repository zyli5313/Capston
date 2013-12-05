# plot using saved data

NDIM = 500
NTIMEUP = 80
NTIMELOW = 20
STEP = 1000

load("res_children_s=05_NSTEP=1000_NDIM=500.dat")
print("load done")
 
# save(s, pstar, v, f, c, b, optimal_stock_holding, file = "res_tiny.dat")

# --- plot ---
library(rgl)

tvec = seq(1, NTIMEUP-1, by=1)
s_idx = 1
s = c(0.5)

# s_idx = round(0.5 / (1/NDIM)) + 1 
# s = seq(0, 1.0, by=1/NDIM)

# xvec = rep(pstar, each=pstar_size)
# yvec = rep(tvec, times=length(tvec))
# plot3d(xvec, yvec, c[ s_idx, , tvec], xlab="s", ylab="pstar", zlab="v", box = TRUE, axes = TRUE)
# lines3d(xvec, yvec, c[ s_idx, , tvec], xlab="s", ylab="pstar", zlab="v", box = TRUE, axes = TRUE)

# --- optimal consumption policy ---
# check zmat to determine x and y 
xvec_range = tvec + 20
yvec_range = pstar
zmat = c[ s_idx, , tvec]
zmat = aperm(zmat)
persp3d(xvec_range, yvec_range, zmat, col = 'skyblue',aspect=TRUE, xlab="Age", ylab="Basis-price ratio", zlab="Consumption-wealth ratio", box = TRUE, axes = TRUE) 
surface3d(xvec_range, yvec_range, zmat)
# surface3d(xvec_range, yvec_range, zmat, front="lines", back="lines")
# rgl.postscript( "t-pstar-c.eps", fmt="eps", drawText=TRUE )
rgl.snapshot("t-pstar-c.png", fmt="png", top=TRUE )

# --- optimal stock holding ---
# xvec_range = tvec + 20
# yidxes = seq(1: ceiling(length(pstar)*0.75))
# yvec_range = pstar[yidxes]
# zmat = optimal_stock_holding[s_idx, yidxes, tvec]
# zmat = aperm(zmat)
# # persp3d(xvec_range, yvec_range, zmat, col = 'lightblue',aspect=TRUE, xlab="Age", ylab="Basis-price ratio", zlab="Optimal stock holding", box = TRUE, axes = TRUE) 
# persp3d(xvec_range, yvec_range, zmat,zlim = c(0.0,0.8), col = 'lightblue',aspect=TRUE, xlab="Age", ylab="Basis-price ratio", zlab="Optimal stock holding", box = TRUE, axes = TRUE) 
# surface3d(xvec_range, yvec_range, zmat)
# # surface3d(xvec_range, yvec_range, zmat, front="lines", back="lines")
# # rgl.postscript( "t-pstar-optimal_stock_holding.eps", fmt="eps", drawText=TRUE )
# rgl.snapshot("t-pstar-optimal_stock_holding.png", fmt="png", top=TRUE )

# --- optimal stock holding at age 20 ---
# xvec_range = s
# yvec_range = pstar
# zmat = optimal_stock_holding[ , , 1]
# zmat = aperm(zmat)
# persp3d(xvec_range, yvec_range, zmat, col = 'lightblue',aspect=TRUE, xlab="s", ylab="pstar", zlab="optimal_stock_holding", box = TRUE, axes = TRUE) 
# surface3d(xvec_range, yvec_range, zmat, front="lines", back="lines")
# rgl.snapshot("s-pstar-optimal_stock_holding.png", fmt="png", top=TRUE )

# --- plot 2d graph in CREF cover ---
# --- optimal stock holding vs investor's age ---
# s_idx_cur = round(0.5 / (1/NDIM)) + 1 
# s_idx_cur = 1
# pidxes = c(1, round(0.8 / (2/NDIM)) + 1, round(1.0 / (2/NDIM)) + 1)
# xvec_range = tvec + 20
# yvec_range = t(optimal_stock_holding[s_idx_cur , pidxes, tvec])
# matplot(xvec_range, yvec_range, xlab="t", ylab="optimal_stock_holding",type = "o", col = rainbow(ncol(yvec_range)))


# --- gdb info
ages = c(21,30,40,50,60,70,80,90,99) - 20
print("consumtion ratio, equity proportion")
print(c[s_idx, ceiling(length(pstar)*0.25), ages])
print(optimal_stock_holding[s_idx, ceiling(length(pstar)*0.25), ages])


