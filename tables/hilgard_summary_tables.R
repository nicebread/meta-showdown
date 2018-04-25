# Express results of simulation in a way that humans can read (I hope)

load("summ.RData") # 4748 rows is a lot to consider

# View(summ) # Consider using spread() to compare ME or MSE across methods

# everything does well when delta = 0.8, right?
# Well, no; PET, PETPEESE have bad downward bias when tau = .4,
# 3PSM Seems to show downward bias too, especially when tau, selProp, QRP are all bad
  

summ.arrange <- arrange()