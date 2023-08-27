library(TransPhylo)

ss <- TransPhylo::simulateOutbreak(off.r=2,dateStartOutbreak=2010,dateT=2015)

tt <- TransPhylo::makeTTree(off.r = 50, #size
                      off.p = 0.7, #probability of success in each trial
                      pi = 1,
                      w.shape = 9,
                      w.scale = 0.33,
                      maxTime = 50)

#dnbinom(x, size, prob, mu, log = FALSE)
#rnbinom(1, off.r, 1 - off.p)

ss <-
  TransPhylo::simulateOutbreak(off.r = 50, #size
                               off.p = 0.7, #probability of success in each trial
                               pi = 1,
                               w.shape = 9,
                               w.scale = 0.33,
                               dateStartOutbreak=2010,dateT=2015)
