# single risky asset, replicate ctax paper


# return mortality rate in age = [1, 100], filter 0,101,102
get_mortality_rate_vec = function() {
	life_len_raw = read.table("mortality_rate_1980.txt", header=TRUE)
	life_len = c(0.5 * (life_len_raw[ , 2] + life_len_raw[ , 3]), 
							0.5 * (life_len_raw[ , 5] + life_len_raw[ , 6]), 
							0.5 * (life_len_raw[ , 8] + life_len_raw[ , 9]) )

	morta_rate = 1 - life_len[-c(length(life_len))] / (life_len[-c(1)] + 1)
	morta_rate = morta_rate[1:100]

	return(morta_rate)
}

#--- constant ---
# NDIM = 500
# NTIMEUP = 80
# NTIMELOW = 20
# STEP = 10

NDIM = 4
NTIMEUP = 5
NTIMELOW = 1
STEP = 10

#--- parameters ---

i = 0.035 #annual inflation rate
r = 0.06 # nominal pretax interest rate on riskless bond
d = 0.02 # nominal dividend yield
tau_d = 0.36 # tax rate on dividend and interest
tau_g = 0.36 # tax rate on capital gain and losses
beta = 0.96 # annual subjective discount factor
gamma = 3.0 # risk aversion parameter
H = NTIMEUP * 10 # num of period of annuity for the benefit of investor's beneficiary 

rstar = ((1-tau_d) * r - i) / (1 + i) # after tax real bond return 
A_H = (rstar * (1+rstar)^H) / ((1+rstar)^H - 1) 

# states xt
s = seq(1/NDIM, 1.0, by=1/NDIM)
pstar = seq(1/NDIM, 2.0, by=1/NDIM)

s_size = length(s)
pstar_size = length(pstar)

# get mortality rate
lambda = get_mortality_rate_vec()

# --- main ---
ptm = proc.time() # time all calculation
# value function (0-NTIMELOW not used)
v = array(0, dim=c(s_size, pstar_size, NTIMEUP))

# initialization
v[ , , NTIMEUP] = (beta * (1-beta^H) * A_H^(1-gamma)) / ((1-beta) * (1-gamma))
# vector to save optimal comsuption ratio
c = array(0, dim=c(s_size, pstar_size, NTIMEUP))
# vector to save equity ration in total wealth after time t
f = array(0, dim=c(s_size, pstar_size, NTIMEUP))
# vector to save bond ration in total wealth after time t
b = array(0, dim=c(s_size, pstar_size, NTIMEUP))
# optimal stock holding
optimal_stock_holding = array(0, dim=c(s_size, pstar_size, NTIMEUP))

# TODO: not yet included eq 13
# v[st][pstar_tm1][t]
for(t in seq(NTIMEUP-1, 1, by=-1)) {
	lambdat = lambda[t + NTIMELOW] # lambda range [1, 100]

	# (scalar) E_t[v_{t+1}(x_{t+1})]
	Et_vtp1 = mean(v[ , , t+1]);

	for(is in 1:length(s)) {
		for(ipstar in 1:length(pstar)) {
			# current states: st, pstar_tm1 (scalar)
			st = s[is]
			pstar_tm1 = pstar[ipstar]

			# print(pstar_tm1)

			# current policies: ft, bt, ct (vector)
			curbase = seq(1/STEP, 1, by=1/STEP)
			# generate each posible (ft[i], bt[i]) pair. Can calculate ct[i] using eq 14

			# TODO ft == s_t+1! BUG!
			ft = rep(curbase, each=STEP)
			bt = rep(curbase, times=STEP)
			valid_idx = ft + bt <= 1
			ft = ft[valid_idx]
			bt = bt[valid_idx]

			# part 1 vector
			# calc ct
			# (vector) fraction of beginning-of-period wealth that is taxable as realized capital gain in period t
			deltat = ((pstar_tm1 > 1) * st + (pstar_tm1 <= 1) * max(st-ft, 0)) * (1 - pstar_tm1)
			ct = 1 - tau_g * deltat - ft - bt
			print(ct)

			res1 = exp(-lambdat) * ct^(1-gamma) / (1-gamma)

			# part 2 scalar
			res2 = ((1-exp(-lambdat)) * beta * (1-beta^H) * A_H^(1-gamma)) / ((1-beta) * (1-gamma))

			# part 3 vector
			# TODO: need to represent g_tp1
			g_tp1 = 1 / pstar_tm1 - 1
			# (vector) gross nomial return from t to t+1 (paid dividend tax, but not paid capital gain tax)
			R_tp1 = (ft * (1+(1-tau_d)*d) * (1+g_tp1) + (1+(1-tau_d)*r) * bt) / (ft + bt)
			# vector 100x1
			w_tp1 = R_tp1 * (1 - tau_g * deltat - ct)

			# w_{t+1} is independent of E_t[v_{t+1}(x_{t+1})] 
			res3 = exp(-lambdat) * beta * Et_vtp1 * w_tp1^(1-gamma)

			# update value function v
			vt = res1 + res2 + res3
			
			# print(max(vt))
			# print(ipstar)

			# IMP: array indexing: v[i,j,k]. Not v[i][j][k]
			v[is, ipstar, t] = max(vt)
			# find the optimal policy
			idx = which.max(vt)
			
			f[is, ipstar, t] = ft[idx]
			c[is, ipstar, t] = ct[idx]
			b[is, ipstar, t] = bt[idx]
			optimal_stock_holding[is, ipstar, t] = ft[idx] / (ft[idx] + bt[idx])
		}
	}
	print(t)
	flush.console()
}

all_time = proc.time() - ptm
print(all_time)
save(s, pstar, v, f, c, b, optimal_stock_holding, file = "res_tiny.dat")


# --- plot ---
source("plot.r")


