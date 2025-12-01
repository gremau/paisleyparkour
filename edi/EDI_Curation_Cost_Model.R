# Overall model for data publications
# By Paul Hanson 2025-12-01

nYears = 3 # number of years for the projection
moPerYear = 12 # months per year
T_tot = nYears*moPerYear # total number of time steps
t = 1:T_tot # time steps

# Total monthly costs = baseCost + C1 * notCurated + C2 * Curated
# where C1 and C2 are $/pub and noCurated; Curated are pubs/month; baseCost is $/mo

# Base Costs
baseCost = 500000 / 12 # $/mo, monthly base cost to keep EDI alive

# notCurated packages
#nNotCuratedPackages = seq(200,300,10) # packages per month
C0 = 10 # current cost per non curated publication
Cf = 10 # target cost per non curated publication
# C1, Model for costs per curated publication
C1 = C0*(Cf/C0)^(t/T_tot) # cost per publication at time T
notCuratedPackages = 200 + t*5

# Curated packages
C0 = 200 # current cost per curated publication
Cf = 100 # target cost per curated publication
# C2, Model for costs per curated publication
C2 = C0*(Cf/C0)^(t/T_tot) # cost per publication at time T
CuratedPackages = 40 + t*2

TotalMonthlyCosts = baseCost + C1 * notCuratedPackages + C2 * CuratedPackages
marginalGrowthCosts = TotalMonthlyCosts - baseCost
CostsPerPub = marginalGrowthCosts / (notCuratedPackages + CuratedPackages)


