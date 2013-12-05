# single risky asset, replicate ctax paper


# return mortality rate in age = [1, 100]
get_mortality_rate_vec = function() {
	life_len_raw = read.table("mortality_rate_1980.txt", header=TRUE)
	life_len = c(0.5 * (life_len_raw[ , 2] + life_len_raw[ , 3]), 
							0.5 * (life_len_raw[ , 5] + life_len_raw[ , 6]), 
							0.5 * (life_len_raw[ , 8] + life_len_raw[ , 9]) )

	morta_rate = 1 - life_len[-c(length(life_len))] / (life_len[-c(1)] + 1)	# age [0, 100]
	morta_rate = morta_rate[2:101] # age [1, 100]

	return(morta_rate)

	# TODO: GDB: infinate horizon (age doesn't affect decision)
	# return (rep(0.01, 100))
}

#--- constant ---
# NDIM = 500
# NTIMEUP = 80
# NTIMELOW = 20
# STEP = 100

NDIM = 10
NTIMEUP = 80
NTIMELOW = 20
STEP = 100

#--- parameters ---

i = 0.035 #annual inflation rate
r = 0.06 # nominal pretax interest rate on riskless bond
d = 0.02 # nominal dividend yield
tau_d = 0.36 # tax rate on dividend and interest
tau_g = 0.36 # tax rate on capital gain and losses
beta = 0.96 # annual subjective discount factor
gamma = 3.0 # risk aversion parameter
# H very sensitive
# H = 5
H = NTIMEUP*20   # num of period of annuity for the benefit of investor's beneficiary 
bmean = 1.07	# pretax nomial capital gain return binomial process mean
bvar = 0.207^2	# pretax nomial capital gain return binomial process variance
up = ( (1 + bmean^2 + bvar) + ((1 + bmean^2 + bvar)^2 - 4*bmean^2)^0.5 ) / (2*bmean) # up factor == g_H + 1 (pretax nomial capital gain return)
probUp = (bmean - 1/up) / (up - 1/up) # probability of binomial process going up

rstar = ((1-tau_d) * r - i) / (1 + i) # after tax real bond return 
A_H = (rstar * (1+rstar)^H) / ((1+rstar)^H - 1) # lim_H->\infty(A_H) = rstar

# states xt
# TODO: set s as a one element vector
# s = seq(0, 1.0, by=1/NDIM)
s = c(0.5)
pstar = seq(0, 2.0, by=1/NDIM)
# s = seq(1/NDIM, 1.0, by=1/NDIM)
# pstar = seq(1/NDIM, 2.0, by=1/NDIM)

s_size = length(s)
pstar_size = length(pstar)

# get mortality rate
lambda = get_mortality_rate_vec()

# --- main ---
ptm = proc.time() # time all calculation
# value function (0-NTIMELOW not used)
v = array(0, dim=c(s_size, pstar_size, NTIMEUP))

# initialization
# TODO: init correct?
v[ , , NTIMEUP] = (beta * (1-beta^H) * A_H^(1-gamma)) / ((1-beta) * (1-gamma))
# v[ , , NTIMEUP] = ( (1-exp(-lambda[NTIMELOW + NTIMEUP])) * beta * (1-beta^H) * A_H^(1-gamma)) / ((1-beta) * (1-gamma))
# vector to save optimal comsuption ratio
c = array(0, dim=c(s_size, pstar_size, NTIMEUP))
# vector to save equity ration in total wealth after time t
f = array(0, dim=c(s_size, pstar_size, NTIMEUP))
# vector to save bond ration in total wealth after time t
b = array(0, dim=c(s_size, pstar_size, NTIMEUP))
# optimal stock holding
optimal_stock_holding = array(0, dim=c(s_size, pstar_size, NTIMEUP))
# bequested wealth from parent (an annuity)
wT = array(0, dim=c(s_size, pstar_size))

# iterate only once to calc parent bequest wealth growth ratio
t = NTIMEUP-1
lambdat = lambda[t + NTIMELOW] # lambda range [1, 100]

# calc wT (donor wealth ratio)
for(is in 1:length(s)) {
	for(ipstar in 1:length(pstar)) {
		# current states: st, pstar_tm1 (scalar)
		st = s[is]
		pstar_tm1 = pstar[ipstar]

		# print(pstar_tm1)

		# current policies: ft, bt, ct (vector)
		# curbase = seq(0, 1.0, by=1/STEP)
		curbase = seq(1/STEP, 1.0, by=1/STEP)
		# generate each posible (ft[i], bt[i]) pair. Can calculate ct[i] using eq 14

		ft = rep(curbase, each=STEP)
		bt = rep(curbase, times=STEP)

		# TODO: IMP, bt can < 0
		# valid_idx = ft + bt <= 1 & ft + bt > 0
		# ft = ft[valid_idx]
		# bt = bt[valid_idx]

		# part 1 vector, IMP: pmax instead of max, pmax returns a vector, while max returns a scalar 
		# calc ct
		# (vector) fraction of beginning-of-period wealth that is taxable as realized capital gain in period t
		deltat = ((pstar_tm1 > 1) * st + (pstar_tm1 <= 1) * pmax(st-ft, 0)) * (1 - pstar_tm1)
		ct = 1 - tau_g * deltat - ft - bt

		# get valid exploration set
		# valid_idx = ft > 0 & ct > 0 
		valid_idx = ct > 0 
		# valid_idx[0] = FALSE # bt==ft==0
		ct = ct[valid_idx]
		ft = ft[valid_idx]
		bt = bt[valid_idx]
		deltat = deltat[valid_idx]
		# print("ft")
		# print(ft)
		# print("bt")
		# print(bt)

		res1 = exp(-lambdat) * ct^(1-gamma) / (1-gamma)

		# part 2 scalar
		res2 = ((1-exp(-lambdat)) * beta * (1-beta^H) * A_H^(1-gamma)) / ((1-beta) * (1-gamma))

		# part 3 vector
		# TODO: need to represent g_tp1
		# g_tp1 = 1 / pstar_tm1 - 1
		# g_tp1 = 0.0314 # g_tp1 = (1+0.07) * (1+d) - 1
		
		# nomial capital gain return on stock follows binomial process
		# (vector) gross nomial return from t to t+1 (paid dividend tax, but not paid capital gain tax)
		R_tp1_H = (ft * (1+(1-tau_d)*d) * up + (1+(1-tau_d)*r) * bt) / (ft + bt)
		# vector 100x1
		w_tp1_H = ( (R_tp1_H / (1+i)) * (1 - tau_g * deltat - ct) )^(1-gamma)
		
		R_tp1_T = (ft * (1+(1-tau_d)*d) * (1/up) + (1+(1-tau_d)*r) * bt) / (ft + bt)
		# vector 100x1
		w_tp1_T = ( (R_tp1_T / (1+i)) * (1 - tau_g * deltat - ct) )^(1-gamma)

		# (vector) calc expected t+1 value function (probUp*scalar*vector + (1-probUp)*scalar*vector)
		E_t = probUp * mean(v[ , 1:ceiling(pstar_size/2), t+1]) * w_tp1_H + (1-probUp) * mean(v[ , (1+ceiling(pstar_size/2)):pstar_size, t+1]) * w_tp1_T
		# vector
		res3 = exp(-lambdat) * beta * E_t
		# print(res3)

		# update value function v
		vt = res1 + res2 + res3
		# print("res3 ratio")
		# print(res3/vt)
		# print("res")
		# print(res1)
		# print(res2)
		# print(res3)

		# print(max(vt))
		# print(vt)

		# IMP: array indexing: v[i,j,k]. Not v[i][j][k]
		v[is, ipstar, t] = max(vt)
		# find the optimal policy
		idx = which.max(vt)

		# bequested wealth growth ratio from parent
		wT[is, ipstar] = probUp * w_tp1_H[idx] + (1-probUp) * w_tp1_T[idx]
		
		print("res3 ratio")
		print(res3[idx]/vt[idx])

		f[is, ipstar, t] = ft[idx]
		c[is, ipstar, t] = ct[idx]
		b[is, ipstar, t] = bt[idx]

		optimal_stock_holding[is, ipstar, t] = ft[idx] / (ft[idx] + bt[idx])
		print("ft")
		print(ft[idx])
		print(bt[idx])
	}
}
print("wT")
print(wT)

# iteration for children
for(t in seq(NTIMEUP-1, 1, by=-1)) {
	lambdat = lambda[t + NTIMELOW] # lambda range [1, 100]

	# wrong!
	# (scalar) E_t[v_{t+1}(x_{t+1})]
	#Et_vtp1 = mean(v[ , , t+1]);

	for(is in 1:length(s)) {
		for(ipstar in 1:length(pstar)) {
			# current states: st, pstar_tm1 (scalar)
			st = s[is]
			pstar_tm1 = pstar[ipstar]

			# receiver annuity ratio
			at = A_H * wT[is, ipstar] / (1+i)^(t+1)
			# A_H_child = (rstar * (1+rstar)^NTIMEUP) / ((1+rstar)^NTIMEUP - 1) 
			# at = A_H_child * wT[is, ipstar] / (1+i)^t
			print("at")
			print(at)

			# current policies: ft, bt, ct (vector)
			# curbase = seq(0, 1.0, by=1/STEP)
			curbase = seq(1/STEP, 1.0, by=1/STEP)
			# generate each posible (ft[i], bt[i]) pair. Can calculate ct[i] using eq 14

			ft = rep(curbase, each=STEP)
			bt = rep(curbase, times=STEP)

			# TODO: IMP, bt can < 0
			# valid_idx = ft + bt <= 1 & ft + bt > 0
			# ft = ft[valid_idx]
			# bt = bt[valid_idx]

			# part 1 vector, IMP: pmax instead of max, pmax returns a vector, while max returns a scalar 
			# calc ct
			# (vector) fraction of beginning-of-period wealth that is taxable as realized capital gain in period t
			deltat = ((pstar_tm1 > 1) * st + (pstar_tm1 <= 1) * pmax(st-ft, 0)) * (1 - pstar_tm1)
			# allow more consumption (from parent's bequest)
			ct = 1 - tau_g * deltat - ft - bt + at

			# get valid exploration set
			# valid_idx = ft > 0 & ct > 0 
			valid_idx = ct > 0 
			# valid_idx[0] = FALSE # bt==ft==0
			ct = ct[valid_idx]
			ft = ft[valid_idx]
			bt = bt[valid_idx]
			deltat = deltat[valid_idx]
			# print("ft")
			# print(ft)
			# print("bt")
			# print(bt)

			res1 = exp(-lambdat) * ct^(1-gamma) / (1-gamma)

			# part 2 scalar
			res2 = ((1-exp(-lambdat)) * beta * (1-beta^H) * A_H^(1-gamma)) / ((1-beta) * (1-gamma))

			# part 3 vector
			# TODO: need to represent g_tp1
			# g_tp1 = 1 / pstar_tm1 - 1
			# g_tp1 = 0.0314 # g_tp1 = (1+0.07) * (1+d) - 1
			
			# nomial capital gain return on stock follows binomial process
			# (vector) gross nomial return from t to t+1 (paid dividend tax, but not paid capital gain tax)
			R_tp1_H = (ft * (1+(1-tau_d)*d) * up + (1+(1-tau_d)*r) * bt) / (ft + bt)
			# vector 100x1
			w_tp1_H = ( (R_tp1_H / (1+i)) * (1 - tau_g * deltat - ct + at) )^(1-gamma)
			
			R_tp1_T = (ft * (1+(1-tau_d)*d) * (1/up) + (1+(1-tau_d)*r) * bt) / (ft + bt)
			# vector 100x1
			w_tp1_T = ( (R_tp1_T / (1+i)) * (1 - tau_g * deltat - ct + at) )^(1-gamma)

			# (vector) calc expected t+1 value function (probUp*scalar*vector + (1-probUp)*scalar*vector)
			E_t = probUp * mean(v[ , 1:ceiling(pstar_size/2), t+1]) * w_tp1_H + (1-probUp) * mean(v[ , (1+ceiling(pstar_size/2)):pstar_size, t+1]) * w_tp1_T
			# vector
			res3 = exp(-lambdat) * beta * E_t
			# print(res3)

			# update value function v
			vt = res1 + res2 + res3
			# print("res3 ratio")
			# print(res3/vt)
			# print("res")
			# print(res1)
			# print(res2)
			# print(res3)

			# print(max(vt))
			# print(vt)

			# IMP: array indexing: v[i,j,k]. Not v[i][j][k]
			v[is, ipstar, t] = max(vt)
			# find the optimal policy
			idx = which.max(vt)

			print("res3 ratio")
			print(res3[idx]/vt[idx])

			f[is, ipstar, t] = ft[idx]
			c[is, ipstar, t] = ct[idx]
			b[is, ipstar, t] = bt[idx]

			optimal_stock_holding[is, ipstar, t] = ft[idx] / (ft[idx] + bt[idx])
			print("ft")
			print(ft[idx])
			print(bt[idx])
		}
	}
	print(t)
	flush.console()
}

all_time = proc.time() - ptm
print(all_time)
save(s, pstar, v, f, c, b, optimal_stock_holding, file = "res_tiny_children.dat")


# --- plot ---
source("plot.r")


