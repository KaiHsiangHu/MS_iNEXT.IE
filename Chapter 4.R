library(ggpubr)
library(ggh4x)
library(iNEXT.IE)
library(ggpubr)


source("../Proposal/qIE function.r")


scientific_first_row <- function(x){
  ifelse(x >= 50000, 
         parse(text = gsub("e\\+", " %*% 10^", scales::scientific(x, digits = 1))),
         x)
  }



## =========================== Figure 4-1 ~ 4-2 ===================================
## 700 x 600
int = 0.1 
N1 = c(4, 4, 8, 2)
N2 = c(4, 4, 8)
N3 = c(2, 2, 4)
N4 = c(1, 1, 6)
# plot(seq(0, sum(N1), int), log2(IE(N1, seq(0, sum(N1), int))), 
#      type = 'l', 
#      xlab = 'Order q', ylab = expression(paste(log[2], '(Inter-specific encounter)')),
#      cex.lab = 1.3,
#      ylim = c(min(log2(IE(N1, seq(0, sum(N1), int)))), 
#               max(log2(IE(N1, seq(0, sum(N1), int))))),
#      lwd = 2)
# 
# lines(seq(0, sum(N2), int), log2(IE(N2, seq(0, sum(N2), int))), type = 'l', col = 'blue',  lwd = 2)
# lines(seq(0, sum(N3), int), log2(IE(N3, seq(0, sum(N3), int))), type = 'l', col = 'red',   lwd = 2)
# lines(seq(0, sum(N4), int), log2(IE(N4, seq(0, sum(N4), int))), type = 'l', col = 'cyan2', lwd = 2)
# legend("topright", legend = c("(4, 4, 8, 2)", "(4, 4, 8)", 
#                               "(2, 2, 4)", "(1, 1, 6)"), 
#        lty = 1, col = c("black", "blue", "red", "cyan2"), lwd = 2)

plot(seq(0.5, 5, int), IE(N1, seq(0.5, 5, int)), 
     type = 'l', 
     xlab = 'Order q', ylab = "Inter-specific encounter",
     cex.lab = 1.3,
     ylim = c(min(IE(N1, seq(0.5, sum(N1), int))), 
              max(IE(N1, seq(0.5, sum(N1), int)))),
     lwd = 2)

lines(seq(0.5, 5, int), IE(N2, seq(0.5, 5, int)), type = 'l', col = 'blue',  lwd = 2)
lines(seq(0.5, 5, int), IE(N3, seq(0.5, 5, int)), type = 'l', col = 'red',   lwd = 2)
lines(seq(0.5, 5, int), IE(N4, seq(0.5, 5, int)), type = 'l', col = 'cyan2', lwd = 2)
legend("topright", legend = c("(4, 4, 8, 2)", "(4, 4, 8)", 
                              "(2, 2, 4)", "(1, 1, 6)"), 
       lty = 1, col = c("black", "blue", "red", "cyan2"), lwd = 2)


## 
## 700 x 600
int = 0.1 
N1 = rep(6, 6)
N2 = c(31, 1, 1, 1, 1, 1)
# N2 = c(17.99, rep(0.01/5, 5))

par(mfrow = c(1, 2))

# plot(seq(0, sum(N1), int), IE(N1, seq(0, sum(N1), int)), 
#      type = 'l', 
#      xlab = 'Order q', ylab = 'Inter-specific encounter',
#      cex.lab = 1.3,
#      lwd = 2)
# 
# title("(a)", adj = 0)
# lines(seq(0, sum(N2), int), IE(N2, seq(0, sum(N2), int)), type = 'l', col = 'red',  lwd = 2)
# legend("topright", legend = c("(3, 3, 3, 3, 3, 3)", "(13, 1, 1, 1, 1, 1)"), 
#        lty = 1, col = c("black", "red"), lwd = 2)

plot(seq(0.5, 5, int), IE(N1, seq(0.5, 5, int)), 
     type = 'l', 
     xlab = 'Order q', ylab = 'Inter-specific encounter',
     cex.lab = 1.3,
     lwd = 2)

title("(a)", adj = 0)
lines(seq(0.5, 5, int), IE(N2, seq(0.5, 5, int)), type = 'l', col = 'red',  lwd = 2)
legend("topright", legend = c("(6, 6, 6, 6, 6, 6)", "(31, 1, 1, 1, 1, 1)"), 
       lty = 1, col = c("black", "red"), lwd = 2)


## 700 x 600
int = 0.1 
N1 = rep(1, 12)
N2 = rep(2, 6)
N3 = rep(3, 4)
N4 = rep(4, 3)
N5 = rep(6, 2)

# plot(seq(0, sum(N1), int), IE(N1, seq(0, sum(N1), int)), 
#      type = 'l', 
#      xlab = 'Order q', ylab = 'Inter-specific encounter',
#      cex.lab = 1.3,
#      lwd = 2)
# 
# title("(b)", adj = 0)
# lines(seq(0, sum(N2), int), IE(N2, seq(0, sum(N2), int)), type = 'l', col = 'blue',  lwd = 2)
# lines(seq(0, sum(N3), int), IE(N3, seq(0, sum(N3), int)), type = 'l', col = 'red',   lwd = 2)
# lines(seq(0, sum(N4), int), IE(N4, seq(0, sum(N4), int)), type = 'l', col = 'cyan2', lwd = 2)
# lines(seq(0, sum(N5), int), IE(N5, seq(0, sum(N5), int)), type = 'l', col = 'hotpink',  lwd = 2)
# legend("topright", legend = c("(1, 1, 1, 1, 1, 1,\n 1, 1, 1, 1, 1, 1)", 
#                                 "(2, 2, 2, 2, 2, 2)", "(3, 3, 3, 3)", "(4, 4, 4)", "(6, 6)"), 
#        lty = 1, col = c("black", "blue", "red", "cyan2", "hotpink"), lwd = 2)

plot(seq(0.5, 3, int), IE(N1, seq(0.5, 3, int)), 
     type = 'l', 
     xlab = 'Order q', ylab = 'Inter-specific encounter',
     cex.lab = 1.3,
     lwd = 2)

title("(b)", adj = 0)
lines(seq(0.5, 3, int), IE(N2, seq(0.5, 3, int)), type = 'l', col = 'blue',  lwd = 2)
lines(seq(0.5, 3, int), IE(N3, seq(0.5, 3, int)), type = 'l', col = 'red',   lwd = 2)
lines(seq(0.5, 3, int), IE(N4, seq(0.5, 3, int)), type = 'l', col = 'cyan2', lwd = 2)
lines(seq(0.5, 3, int), IE(N5, seq(0.5, 3, int)), type = 'l', col = 'hotpink',  lwd = 2)
legend("topright", legend = c("(1, 1, 1, 1, 1, 1,\n 1, 1, 1, 1, 1, 1)", 
                              "(2, 2, 2, 2, 2, 2)", "(3, 3, 3, 3)", "(4, 4, 4)", "(6, 6)"), 
       lty = 1, col = c("black", "blue", "red", "cyan2", "hotpink"), lwd = 2)



## ==================== Figure 4-3 ~ 4-8 ===================================
rho = c(0.1, 0.2, 0.3, 0.4, 0.5)
species = 100


# Homogeneous
prob = rep(1, species)
Ni_homo = round(2000 * prob / sum(prob))
Ni_homo[Ni_homo == 0] = 1
names(Ni_homo) = paste('t', 1:species, sep = '')


# Uniform
set.seed(2022)
prob = runif(species)
Ni_unif = round(2000 * prob / sum(prob))
Ni_unif[Ni_unif == 0] = 1
names(Ni_unif) = paste('t', 1:species, sep = '')


# Exponential
set.seed(2022)
prob = rexp(species)
Ni_exp = round(2000 * prob / sum(prob))
Ni_exp[Ni_exp == 0] = 1
names(Ni_exp) = paste('t', 1:species, sep = '')


# LogNormal
set.seed(2022)
prob = rlnorm(species)
Ni_lnorm = round(2000 * prob / sum(prob))
Ni_lnorm[Ni_lnorm == 0] = 1
names(Ni_lnorm) = paste('t', 1:species, sep = '')


# Zipf-Mandelbrot
prob = 1 / (1:species)
Ni_zipf = round(2000 * prob / sum(prob))
Ni_zipf[Ni_zipf == 0] = 1
names(Ni_zipf) = paste('t', 1:species, sep = '')


# Power-decay
prob = 1 / (1:species)^2
Ni_decay = round(2000 * prob / sum(prob))
Ni_decay[Ni_decay == 0] = 1
names(Ni_decay) = paste('t', 1:species, sep = '')



sd(Ni_homo)   / mean(Ni_homo)
sd(Ni_unif)   / mean(Ni_unif)
sd(Ni_exp)    / mean(Ni_exp)
sd(Ni_lnorm)  / mean(Ni_lnorm)
sd(Ni_zipf)   / mean(Ni_zipf)
sd(Ni_decay)  / mean(Ni_decay)


sum(Ni_homo > 0);  sum(Ni_homo)
sum(Ni_unif > 0);  sum(Ni_unif)
sum(Ni_exp > 0);   sum(Ni_exp)
sum(Ni_lnorm > 0); sum(Ni_lnorm)
sum(Ni_zipf > 0);  sum(Ni_zipf)
sum(Ni_decay > 0); sum(Ni_decay)



cpu.cores <- detectCores() - 2
# cpu.cores <- 12
cl <- makeCluster(cpu.cores)
clusterExport(cl, c("Ni_homo", "Ni_unif", "Ni_exp", "Ni_lnorm", "Ni_zipf", "Ni_decay", "IE", "Asy.IE", "iNEXT.IE", "Chat.func", "bootstrap", "iNEXT.IE.simu", "invC.hat"))
clusterEvalQ(cl, c(library(dplyr), library(iNEXT.3D), library(reshape2), library(future.apply), library(abind), library(iNEXT.wor)))


start1 = Sys.time()
iNEXT.result.homo = parLapply(cl, rho, function(r) iNEXT.IE.simu(Ni_homo, q = c(0.5, 1, 2), r, nboot = 100, conf = 0.95, simu = 200)) %>% do.call(rbind,.)
end1 = Sys.time()
end1 - start1


start2 = Sys.time()
iNEXT.result.unif = parLapply(cl, rho, function(r) iNEXT.IE.simu(Ni_unif, q = c(0.5, 1, 2), r, nboot = 100, conf = 0.95, simu = 200)) %>% do.call(rbind,.)
end2 = Sys.time()
end2 - start2


start3 = Sys.time()
iNEXT.result.exp = parLapply(cl, rho, function(r) iNEXT.IE.simu(Ni_exp, q = c(0.5, 1, 2), r, nboot = 100, conf = 0.95, simu = 200)) %>% do.call(rbind,.)
end3 = Sys.time()
end3 - start3


start4 = Sys.time()
iNEXT.result.lnorm = parLapply(cl, rho, function(r) iNEXT.IE.simu(Ni_lnorm, q = c(0.5, 1, 2), r, nboot = 100, conf = 0.95, simu = 200)) %>% do.call(rbind,.)
end4 = Sys.time()
end4 - start4


start5 = Sys.time()
iNEXT.result.zipf = parLapply(cl, rho, function(r) iNEXT.IE.simu(Ni_zipf, q = c(0.5, 1, 2), r, nboot = 100, conf = 0.95, simu = 200)) %>% do.call(rbind,.)
end5 = Sys.time()
end5 - start5


start6 = Sys.time()
iNEXT.result.decay = parLapply(cl, rho, function(r) iNEXT.IE.simu(Ni_decay, q = c(0.5, 1, 2), r, nboot = 100, conf = 0.95, simu = 200)) %>% do.call(rbind,.)
end6 = Sys.time()
end6 - start6


stopCluster(cl)




## ========================================================================= ##

## Figure 4-3 ~ 4-8

## 1400 x 800
iNEXT.qIE.figure(iNEXT.result.homo, type = 1)
ggsave("Chap4_fig/abun.homo.size.png", dpi = 300, width = 14, height = 8)
iNEXT.qIE.figure(iNEXT.result.homo, type = 3) + 
  coord_cartesian(xlim = c(0.85, 1), ylim = c(0, NA))
ggsave("Chap4_fig/abun.homo.cov.png",  dpi = 300, width = 14, height = 8)

iNEXT.qIE.figure(iNEXT.result.unif, type = 1)
ggsave("Chap4_fig/abun.unif.size.png", dpi = 300, width = 14, height = 8)
iNEXT.qIE.figure(iNEXT.result.unif, type = 3) + 
  coord_cartesian(xlim = c(0.85, 1), ylim = c(0, NA))
ggsave("Chap4_fig/abun.unif.cov.png",  dpi = 300, width = 14, height = 8)

iNEXT.qIE.figure(iNEXT.result.exp, type = 1)
ggsave("Chap4_fig/abun.exp.size.png", dpi = 300, width = 14, height = 8)
iNEXT.qIE.figure(iNEXT.result.exp, type = 3) + 
  coord_cartesian(xlim = c(0.85, 1), ylim = c(0, NA))
ggsave("Chap4_fig/abun.exp.cov.png",  dpi = 300, width = 14, height = 8)

iNEXT.qIE.figure(iNEXT.result.lnorm, type = 1)
ggsave("Chap4_fig/abun.lnorm.size.png", dpi = 300, width = 14, height = 8)
iNEXT.qIE.figure(iNEXT.result.lnorm, type = 3) + 
  coord_cartesian(xlim = c(0.85, 1), ylim = c(0, NA))
ggsave("Chap4_fig/abun.lnorm.cov.png",  dpi = 300, width = 14, height = 8)

iNEXT.qIE.figure(iNEXT.result.zipf, type = 1)
ggsave("Chap4_fig/abun.zipf.size.png", dpi = 300, width = 14, height = 8)
iNEXT.qIE.figure(iNEXT.result.zipf, type = 3) + 
  coord_cartesian(xlim = c(0.85, 1), ylim = c(0, NA))
ggsave("Chap4_fig/abun.zipf.cov.png",  dpi = 300, width = 14, height = 8)

iNEXT.qIE.figure(iNEXT.result.decay, type = 1)
ggsave("Chap4_fig/abun.decay.size.png", dpi = 300, width = 14, height = 8)
iNEXT.qIE.figure(iNEXT.result.decay, type = 3) + 
  coord_cartesian(xlim = c(0.86, 1), ylim = c(0, NA))
ggsave("Chap4_fig/abun.decay.cov.png",  dpi = 300, width = 14, height = 8)


##
cbind(iNEXT.result.homo %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE, SC) %>% rename(True = qIE),
      iNEXT.result.homo %>% filter(Type == "Proposed") %>% select(qIE, SC, Method, Bias, RMSE, size_CI_rate, cov_CI_rate) %>% rename(Proposed = qIE)) %>% 
  .[seq(7, 21, 2) + rep(21 * (0:14), each = 8),] %>% write.csv(., file = "../homo.csv")

cbind(iNEXT.result.homo %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE) %>% rename(True = qIE)     %>% .[seq(21, nrow(.), 21),],
      iNEXT.result.homo %>% filter(Type == "MLE")      %>% select(qIE, Bias, RMSE)      %>% rename(MLE = qIE),
      iNEXT.result.homo %>% filter(Type == "Proposed") %>% select(qIE, Bias, RMSE)      %>% rename(Proposed = qIE) %>% .[seq(21, nrow(.), 21),]) %>% 
  write.csv(., file = "../homo.MLE.csv")

##
cbind(iNEXT.result.unif %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE, SC) %>% rename(True = qIE),
      iNEXT.result.unif %>% filter(Type == "Proposed") %>% select(qIE, SC, Method, Bias, RMSE, size_CI_rate, cov_CI_rate) %>% rename(Proposed = qIE)) %>% 
  .[seq(7, 21, 2) + rep(21 * (0:14), each = 8),] %>% write.csv(., file = "../unif.csv")

cbind(iNEXT.result.unif %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE) %>% rename(True = qIE)     %>% .[seq(21, nrow(.), 21),],
      iNEXT.result.unif %>% filter(Type == "MLE")      %>% select(qIE, Bias, RMSE)      %>% rename(MLE = qIE),
      iNEXT.result.unif %>% filter(Type == "Proposed") %>% select(qIE, Bias, RMSE)      %>% rename(Proposed = qIE) %>% .[seq(21, nrow(.), 21),]) %>% 
  write.csv(., file = "../unif.MLE.csv")

##
cbind(iNEXT.result.exp %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE, SC) %>% rename(True = qIE),
      iNEXT.result.exp %>% filter(Type == "Proposed") %>% select(qIE, SC, Method, Bias, RMSE, size_CI_rate, cov_CI_rate) %>% rename(Proposed = qIE)) %>% 
  .[seq(7, 21, 2) + rep(21 * (0:14), each = 8),] %>% write.csv(., file = "../exp.csv")

cbind(iNEXT.result.exp %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE) %>% rename(True = qIE)     %>% .[seq(21, nrow(.), 21),],
      iNEXT.result.exp %>% filter(Type == "MLE")      %>% select(qIE, Bias, RMSE)      %>% rename(MLE = qIE),
      iNEXT.result.exp %>% filter(Type == "Proposed") %>% select(qIE, Bias, RMSE)      %>% rename(Proposed = qIE) %>% .[seq(21, nrow(.), 21),]) %>% 
  write.csv(., file = "../exp.MLE.csv")

##
cbind(iNEXT.result.lnorm %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE, SC) %>% rename(True = qIE),
      iNEXT.result.lnorm %>% filter(Type == "Proposed") %>% select(qIE, SC, Method, Bias, RMSE, size_CI_rate, cov_CI_rate) %>% rename(Proposed = qIE)) %>% 
  .[seq(7, 21, 2) + rep(21 * (0:14), each = 8),] %>% write.csv(., file = "../lnorm.csv")

cbind(iNEXT.result.lnorm %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE) %>% rename(True = qIE)     %>% .[seq(21, nrow(.), 21),],
      iNEXT.result.lnorm %>% filter(Type == "MLE")      %>% select(qIE, Bias, RMSE)      %>% rename(MLE = qIE),
      iNEXT.result.lnorm %>% filter(Type == "Proposed") %>% select(qIE, Bias, RMSE)      %>% rename(Proposed = qIE) %>% .[seq(21, nrow(.), 21),]) %>% 
  write.csv(., file = "../lnorm.MLE.csv")

##
cbind(iNEXT.result.zipf %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE, SC) %>% rename(True = qIE),
      iNEXT.result.zipf %>% filter(Type == "Proposed") %>% select(qIE, SC, Method, Bias, RMSE, size_CI_rate, cov_CI_rate) %>% rename(Proposed = qIE)) %>% 
  .[seq(7, 21, 2) + rep(21 * (0:14), each = 8),] %>% write.csv(., file = "../zipf.csv")

cbind(iNEXT.result.zipf %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE) %>% rename(True = qIE)     %>% .[seq(21, nrow(.), 21),],
      iNEXT.result.zipf %>% filter(Type == "MLE")      %>% select(qIE, Bias, RMSE)      %>% rename(MLE = qIE),
      iNEXT.result.zipf %>% filter(Type == "Proposed") %>% select(qIE, Bias, RMSE)      %>% rename(Proposed = qIE) %>% .[seq(21, nrow(.), 21),]) %>% 
  write.csv(., file = "../zipf.MLE.csv")

##
cbind(iNEXT.result.decay %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE, SC) %>% rename(True = qIE),
      iNEXT.result.decay %>% filter(Type == "Proposed") %>% select(qIE, SC, Method, Bias, RMSE, size_CI_rate, cov_CI_rate) %>% rename(Proposed = qIE)) %>% 
  .[seq(7, 21, 2) + rep(21 * (0:14), each = 8),] %>% write.csv(., file = "../decay.csv")

cbind(iNEXT.result.decay %>% filter(Type == "True")     %>% select(rho, Order.q, m, qIE) %>% rename(True = qIE)     %>% .[seq(21, nrow(.), 21),],
      iNEXT.result.decay %>% filter(Type == "MLE")      %>% select(qIE, Bias, RMSE)      %>% rename(MLE = qIE),
      iNEXT.result.decay %>% filter(Type == "Proposed") %>% select(qIE, Bias, RMSE)      %>% rename(Proposed = qIE) %>% .[seq(21, nrow(.), 21),]) %>% 
  write.csv(., file = "../decay.MLE.csv")





## ==================== Figure 4-9 =======================================

## BCI
load("BCI/bci.tree8.rData")
bci.tree8 = bci.tree8[bci.tree8$quadrat != "",]

bci_abun = acast(bci.tree8, sp ~ quadrat, fun.aggregate = length)

## Danum
Danum = read.table("../Without-replacement/ForestGEO data/Danum/PlotDataReport08-22-2023_Census2_All Stems.txt", header = TRUE, fill = TRUE)
Danum = Danum %>% filter(No. %in% 1:Danum$No.[nrow(Danum)])
Danum = Danum[!is.na(as.numeric(Danum$PX)),]
Danum = Danum[!is.na(as.numeric(Danum$Quadrat)),]

Danum_abun = acast(Danum, Latin ~ Quadrat, fun.aggregate = length)

## Wanang
Wanang = read.table("../Without-replacement/ForestGEO data/Wanang/PlotDataReport08-22-2023_Census4_Main Stems Only.txt", header = TRUE, fill = TRUE)
Wanang = Wanang[seq(1, nrow(Wanang)-1, 2),]

Wanang_abun = acast(Wanang, Latin ~ Quadrat, fun.aggregate = length)

length(unique(bci.tree8$quadrat))
length(unique(Danum$Quadrat))
length(unique(Wanang$Quadrat))

total = 1250


rhos = seq(0.02, 1, 0.02)

out = lapply(rhos, function(i) {
  
  simu = 1000
  
  samp.data = lapply(1:simu, function(j) {
    
    bci_samp    = bci_abun[,   sample(1:ncol(bci_abun),    round(total * i), replace = F)] %>% rowSums
    Danum_samp  = Danum_abun[, sample(1:ncol(Danum_abun),  round(total * i), replace = F)] %>% rowSums
    Wanang_samp = Wanang_abun[,sample(1:ncol(Wanang_abun), round(total * i), replace = F)] %>% rowSums
    
    list("BCI" = bci_samp, "Danum" = Danum_samp, "Wanang" = Wanang_samp)
  })
  
  tmp = sapply(samp.data, function(x) sapply(1:length(x), function(k) c(IE(x[[k]], c(0.5, 1, 2)), 
                                                                        ObsAsy3D(x[[k]], q = c(0, 1, 2), nboot = 0, method = "Observed")$qTD)) )
  
  data.frame(Order.q = c(0.5, 1, 2, 0, 1, 2),
             qD = rowMeans(tmp),
             Method = rep(c('qIE', 'Hill'), each = 3),
             UCL = rowMeans(tmp) + qnorm(0.975) * apply(tmp, 1, sd),
             LCL = rowMeans(tmp) - qnorm(0.975) * apply(tmp, 1, sd),
             rho = i,
             obs.S = sapply(samp.data, function(x) sapply(x, function(y) sum(y > 0))) %>% rowMeans %>% as.vector,
             N = sapply(samp.data, function(x) sapply(x, function(y) sum(y))) %>% rowMeans %>% as.vector,
             Data = rep(names(samp.data[[1]]), each = 3 * 2)
             )
  
  }) %>% do.call(rbind,.)


## Figure 4-9(a)
## 800 x 400 
ggplot(out %>% filter(Method == 'Hill')) + 
  geom_line(aes(x = rho, y = qD, colour = Data), size = 1.1) +
  scale_colour_manual(values = c("red", "blue", "purple")) +
  facet_wrap( ~ paste("Order q = ", Order.q, sep = ""), scales = "free_y" ) +
  labs(x = expression(rho), y = 'Hill number') +
  theme_bw() +
  theme(legend.position = 'bottom', legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, -5, -10),
        text = element_text(size = 10),
        plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
        axis.text = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        legend.key.width = unit(2, "cm"))
ggsave("Chap4_fig/ForestGEO.Hill.png", dpi = 300, width = 8, height = 4)


## Figure 4-9(b)
ggplot(out %>% filter(Method == 'qIE')) + 
  geom_line(aes(x = rho, y = log2(qD), colour = Data), size = 1.1) +
  scale_colour_manual(values = c("red", "blue", "purple")) +
  facet_wrap( ~ paste("Order q = ", Order.q, sep = ""), scales = "free_y" ) +
  # labs(x = expression(rho), y = 'Inter-specific encounter') +
  labs(x = expression(rho), y = expression(paste(log[2], '(Inter-specific encounter)'))) +
  scale_y_continuous(labels = scientific_first_row) +
  theme_bw() +
  theme(legend.position = 'bottom', legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, -5, -10),
        text = element_text(size = 10),
        plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
        axis.text = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        legend.key.width = unit(2, "cm"))
ggsave("Chap4_fig/ForestGEO.qIE.png", dpi = 300, width = 8, height = 4)




## ==================== Figure 4-10 ~ 4-13 ==================================
# library(iNEXT)
# data = read.table("Proposal/Brazil_Data.txt")
load("spider.rda")

DataInfo3D(spider)

Hill.spider = ObsAsy3D(spider, q = seq(0, 2, 0.1), method = "Observed", nboot = 0) %>% .[,1:3]
IE.spider = lapply(1:length(spider), function(i) data.frame(Assemblage = names(spider)[i],
                                                            Order.q = seq(0.5, 2, 0.1),
                                                            qTD = IE(spider[[i]], seq(0.5, 2, 0.1)))
                   ) %>% do.call(rbind,.)

## Figure 4-10(a)
fig_Hill_spider = 
  Hill.spider %>% mutate(Type = "Taxonomic diversity") %>% 
  ggplot() + 
  geom_line(aes(x = Order.q, y = qTD, colour = Assemblage), size = 1.2) +
  labs(x = "Order q", y = 'Hill numbers') +
  theme_bw() + themes + theme(legend.box.margin = margin(0, 0, 0, 0)) 


## Figure 4-10(b)
fig_IE_spider = 
  IE.spider %>% mutate(Type = "Inter-specific encounter") %>% 
  ggplot() + 
  geom_line(aes(x = Order.q, y = log2(qTD), colour = Assemblage), size = 1.2) +
  labs(x = "Order q", y = expression(paste(log[2], '(Inter-specific encounter)'))) +
  theme_bw() + themes + theme(legend.box.margin = margin(0, 0, 0, 0)) 


## Figure 4-10
## 1000 x 600
ggarrange(fig_Hill_spider + labs(title = "(a) Hill numbers"), 
          fig_IE_spider + labs(title = "(b) Inter-specific encounter"), 
          ncol = 2, common.legend = TRUE,
          legend = "bottom")

Hill.spider %>% filter(Order.q %in% c(0, 1, 2)) %>% acast(Assemblage ~ Order.q) %>% round(., 3)
IE.spider %>% filter(Order.q %in% c(0.5, 1, 2)) %>% acast(Assemblage ~ Order.q) %>% round(., 3)




cpu.cores <- detectCores() - 2
cl <- makeCluster(cpu.cores)
clusterExport(cl, varlist = c("spider", "IE", "Asy.IE", "iNEXT.IE", "iNEXT_qIE_real_data", "Chat.func", "invC.hat", "bootstrap"), envir = environment())
clusterEvalQ(cl, c(library(dplyr), library(iNEXT.3D), library(reshape2), library(iNEXT.wor), library(abind)))

start = Sys.time()

real_data_out = parLapply(cl, c(0.2, 0.4, 0.6, 0.8, 1), function(y) 
  
  lapply(1:length(spider), function(i) iNEXT_qIE_real_data(spider[[i]], q = c(0.5, 1, 2), rho = y, nboot = 500) %>% mutate(Site = names(spider)[i])) %>%
    do.call(rbind,.)
  
  ) %>% do.call(rbind,.)

end = Sys.time()
end - start

stopCluster(cl)


# options(scipen = 999)

## Figure 4-11
## 1350 x 800
ggplot() + 
  geom_point(data = real_data_out %>% filter(Method == "Observed"),
             aes(x = m, y = qIE, colour = Site), size = 4) +
  geom_line(data = real_data_out %>% filter(Method != "Observed"),
            aes(x = m, y = qIE, lty = Method, colour = Site), size = 1.1) + 
  # geom_hline(data = real_data_out %>% filter(Type == "MLE"),
  #            aes(yintercept = qIE, colour = Site), lty = 3, size = 1.1) +
  geom_ribbon(data = real_data_out,
              aes(x = m, ymin = qIE.LCL, ymax = qIE.UCL, fill = Site), linetype = 0, alpha = 0.2) + 
  facet_grid(paste("Order q = ", Order.q, sep = "") ~ paste("rho = ", rho, sep = ""), scales = "free") +
  labs(x = "Number of individuals", y = "Inter-specific encounter") +
  scale_linetype_manual(values = c(2, 1)) + 
  theme_bw() + 
  themes +
  guides(linetype = guide_legend(keywidth = 2.5))
ggsave("Chap4_fig/Spider.size.png", dpi = 300, width = 13.5, height = 8)


## Figure 4-12
## 1350 x 500
ggplot() + 
  geom_point(data = real_data_out %>% filter(Method == "Observed", Order.q == 1),
             aes(x = m, y = SC, colour = Site), size = 4) +
  geom_line(data = real_data_out %>% filter(Method != "Observed", Order.q == 1),
            aes(x = m, y = SC, lty = Method, colour = Site), size = 1.1) + 
  geom_ribbon(data = real_data_out %>% filter(Order.q == 1),
              aes(x = m, ymin = SC.LCL, ymax = SC.UCL, fill = Site), linetype = 0, alpha = 0.2) + 
  facet_grid(. ~ paste("rho = ", rho, sep = ""), scales = "free") +
  labs(x = "Number of individuals", y = "Sample coverage") +
  scale_linetype_manual(values = c(2, 1)) + 
  theme_bw() + 
  themes +
  guides(linetype = guide_legend(keywidth = 2.5)) + 
  coord_cartesian(ylim = c(0.85, 1))
ggsave("Chap4_fig/Spider.size.cov.png", dpi = 300, width = 13.5, height = 5)


## Figure 4-13
## 1350 x 800
ggplot() + 
  geom_point(data = real_data_out %>% filter(Method == "Observed"),
             aes(x = SC, y = qIE, colour = Site), size = 4) +
  geom_line(data = real_data_out %>% filter(Method != "Observed"),
            aes(x = SC, y = qIE, lty = Method, colour = Site), size = 1.1) + 
  # geom_hline(data = real_data_out %>% filter(Type == "MLE"),
  #            aes(yintercept = qIE, colour = Site), lty = 3, size = 1.1) +
  geom_ribbon(data = real_data_out,
              aes(x = SC, ymin = qIE.covLCL, ymax = qIE.covUCL, fill = Site), linetype = 0, alpha = 0.2) + 
  facet_grid(paste("Order q = ", Order.q, sep = "") ~ paste("rho = ", rho, sep = ""), scales = "free") +
  labs(x = "Sample coverage", y = "Inter-specific encounter") +
  scale_linetype_manual(values = c(2, 1)) + 
  theme_bw() + 
  themes +
  guides(linetype = guide_legend(keywidth = 2.5)) + 
  coord_cartesian(xlim = c(0.85, 1))
ggsave("Chap4_fig/Spider.cov.png", dpi = 300, width = 13.5, height = 8)


# options(scipen = 0)

real_data_out %>% filter(Order.q == 0.5, SC == 1) %>% arrange(Site)
real_data_out %>% filter(Order.q == 0.5, Method == "Observed") %>% arrange(Site) %>% .$SC %>% round(., 3)

real_data_out %>% filter(SC == 1) %>% arrange(Order.q, Site) %>% mutate(qIE = round(qIE)) %>% .$qIE %>% matrix(., nrow = 10) %>% t



## =========================== Figure 4-16 ===================================

Brazil = read.table("Brazil_Data.txt") %>% as.matrix
tree = read.tree("Brazil_Tree.txt")
distM = read.table("Brazil_dist.txt", header = TRUE)


DataInfo3D(Brazil)
DataInfo3D(Brazil, diversity = "PD", PDtree = tree)
DataInfo3D(Brazil, diversity = "FD", FDdistM = distM)

treeT = get.rooted.tree.height(tree)


## ==================================================================================== ##
# q = seq(0, 2, 0.1)
q = seq(0.5, 2, 0.1)

TD.out = ObsAsy3D(Brazil, q = seq(0, 2, 0.1), method = "Observed", nboot = 0) %>% .[,1:3]
IE.out = lapply(1:ncol(Brazil), function(i) data.frame(Assemblage = colnames(Brazil)[i],
                                                       Order.q = q,
                                                       qIE = IE(Brazil[,i], q))
                ) %>% do.call(rbind,.)

fig_TD = 
  ggplot(TD.out) + 
  geom_line(aes(x = Order.q, y = qTD, colour = Assemblage), size = 1.2) +
  labs(x = "Order q", y = 'Taxonomic diversity') +
  theme_bw() + themes +
  theme(legend.title = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0))


fig_IE =
  ggplot(IE.out) +
  geom_line(aes(x = Order.q, y = log2(qIE), colour = Assemblage), size = 1.2) +
  labs(x = "Order q", y = expression(paste(log[2], '(Inter-specific Encounter)'))) +
  theme_bw() + themes +
  theme(legend.title = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0))

# fig_IE = 
#   ggplot(IE.out) + 
#   geom_line(aes(x = Order.q, y = qIE, colour = Assemblage), size = 1.2) +
#   labs(x = "Order q", y = "Inter-specific Encounter") +
#   theme_bw() + themes +
#   theme(legend.title = element_blank())

ggarrange(fig_TD, fig_IE, ncol = 2, common.legend = TRUE,
          legend = "bottom")


## ==================================================================================== ##
PD.out = ObsAsy3D(Brazil, diversity = "PD", q = seq(0, 2, 0.1), method = "Observed", nboot = 0, PDtree = tree, PDtype = "meanPD") %>% .[,1:3]
PIE.out = lapply(1:ncol(Brazil), function(i) data.frame(Assemblage = colnames(Brazil)[i],
                                                        Order.q = q,
                                                        qPIE = PIE(Brazil[,i] %>% set_names(rownames(Brazil)), tree = tree, q))
                 ) %>% do.call(rbind,.) ## %>% mutate(qPIE = qPIE / treeT)

fig_PD = 
  ggplot(PD.out) + 
  geom_line(aes(x = Order.q, y = qPD, colour = Assemblage), size = 1.2) +
  # labs(x = "Order.q", y = "Mean phylogenetic diversity")
  labs(x = "Order q", y = 'Mean phylogenetic diversity') +
  theme_bw() + themes +
  theme(legend.title = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0))


fig_PIE = 
  ggplot(PIE.out) + 
  geom_line(aes(x = Order.q, y = log2(qPIE), colour = Assemblage), size = 1.2) +
  # labs(x = "Order q", y = expression(paste(log[2], '(Mean Phylogenetic Inter-specific Encounter)'))) +
  labs(x = "Order q", y = expression(paste(log[2], '(Phylogenetic Inter-specific Encounter)'))) +
  theme_bw() + themes +
  theme(legend.title = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0))

ggarrange(fig_PD, fig_PIE, ncol = 2, common.legend = TRUE,
          legend = "bottom")


# time_out  = lapply(seq(10, treeT, 10), function(t) data.frame(Assemblage = colnames(Brazil)[1],
#                                                               Order.q = q,
#                                                               qPIE = PIE(Brazil[,1] %>% set_names(rownames(Brazil)), tree = tree, q, refT = t),
#                                                               time = t)
#                    ) %>% do.call(rbind,.)
# 
# ggplot(time_out ) +
#   geom_line(aes(x = time, y = log2(qPIE)), size = 1.2) +
#   labs(x = "Time", y = expression(paste(log[2], '(Mean Phylogenetic Inter-specific Encounter)'))) +
#   facet_grid(. ~ Order.q) +
#   theme_bw() + themes +
#   theme(legend.title = element_blank())




## ==================================================================================== ##
FD.out = ObsAsy3D(Brazil, diversity = "FD", q = seq(0, 2, 0.1), method = "Observed", nboot = 0, FDdistM = distM) %>% .[,1:3]
FIE.out = lapply(1:ncol(Brazil), function(i) data.frame(Assemblage = colnames(Brazil)[i],
                                                      Order.q = q,
                                                      qFIE = FIE(Brazil[,i] %>% set_names(rownames(Brazil)), q,
                                                                 tau = seq(0, 1, length.out = 101), dist = as.matrix(distM)))
                 ) %>% do.call(rbind,.)


fig_FD = 
  ggplot(FD.out) + 
  geom_line(aes(x = Order.q, y = qFD, colour = Assemblage), size = 1.2) +
  labs(x = "Order q", y = 'Functional diversity') +
  theme_bw() + themes +
  theme(legend.title = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0))


fig_FIE = 
  ggplot(FIE.out) + 
  geom_line(aes(x = Order.q, y = log2(qFIE), colour = Assemblage), size = 1.2) +
  labs(x = "Order q", y = expression(paste(log[2], '(Functional Inter-specific Encounter)'))) +
  theme_bw() + themes +
  theme(legend.title = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0))

ggarrange(fig_FD, fig_FIE, ncol = 2, common.legend = TRUE,
          legend = "bottom")


ggarrange(fig_TD, fig_PD, fig_FD, ncol = 3, common.legend = TRUE,
          legend = "bottom")
ggsave("Chap4_fig/AD.png", dpi = 300, width = 15, height = 6)

ggarrange(fig_IE, fig_PIE, fig_FIE, ncol = 3, common.legend = TRUE,
          legend = "bottom")
ggsave("Chap4_fig/AIE.png", dpi = 300, width = 15, height = 6)


cbind(TD.out %>% filter(Order.q %in% c(0, 1, 2)) %>% acast(Assemblage ~ Order.q),
      PD.out %>% filter(Order.q %in% c(0, 1, 2)) %>% acast(Assemblage ~ Order.q),
      FD.out %>% filter(Order.q %in% c(0, 1, 2)) %>% acast(Assemblage ~ Order.q)) %>% write.csv(., file = "value.csv")

cbind(IE.out  %>% filter(Order.q %in% c(0.5, 1, 2)) %>% acast(Assemblage ~ Order.q),
      PIE.out %>% filter(Order.q %in% c(0.5, 1, 2)) %>% acast(Assemblage ~ Order.q),
      FIE.out %>% filter(Order.q %in% c(0.5, 1, 2)) %>% acast(Assemblage ~ Order.q)) %>% write.csv(., file = "value.csv")



## =========================== Figure 4-17 ~ 4-18 ===========================

# cpu.cores <- detectCores() - 2
cpu.cores <- 5
cl <- makeCluster(cpu.cores)
clusterExport(cl, varlist = c("Brazil", "tree", "distM",
                              "Chat.func", "invC.hat", "iNEXT_qIE_real_data", 
                              "IE",  "iNEXT.IE",  "bootstrap",     "Asy.IE",
                              "PIE", "iNEXT.PIE", "bootstrap.PIE", "Asy.PIE",
                              "FIE", "iNEXT.FIE", "bootstrap.FIE", "Asy.FIE"), envir = environment())
clusterEvalQ(cl, c(library(dplyr), library(iNEXT.3D), library(reshape2), library(iNEXT.wor), library(abind), library(phyclust)))

start = Sys.time()

IE.iNEXT.out = parLapply(cl, c(0.1, 0.2, 0.3, 0.4, 0.5), function(y) 
  
  lapply(1:ncol(Brazil), function(i) iNEXT_qIE_real_data(Brazil[,i], diversity = "TD", q = c(0.5, 1, 2), rho = y, nboot = 400) %>% 
           
           mutate(Site = colnames(Brazil)[i])) %>% do.call(rbind,.)) %>% do.call(rbind,.)

end = Sys.time()
end - start

start = Sys.time()

PIE.iNEXT.out = parLapply(cl, c(0.1, 0.2, 0.3, 0.4, 0.5), function(y) 
  
  lapply(1:ncol(Brazil), function(i) iNEXT_qIE_real_data(Brazil[,i], diversity = "PD", q = c(0.5, 1, 2), rho = y, nboot = 200, PDtree = tree) %>% 
           
           mutate(Site = colnames(Brazil)[i])) %>% do.call(rbind,.)) %>% do.call(rbind,.)

end = Sys.time()
end - start

start = Sys.time()

FIE.iNEXT.out = parLapply(cl, c(0.1, 0.2, 0.3, 0.4, 0.5), function(y) 
  
  lapply(1:ncol(Brazil), function(i) iNEXT_qIE_real_data(Brazil[,i], diversity = "FD", q = c(0.5, 1, 2), rho = y, nboot = 50, FDdistM = distM, FDtau = seq(1e-08, 1, length.out = 101)) %>% 
           
           mutate(Site = colnames(Brazil)[i])) %>% do.call(rbind,.)) %>% do.call(rbind,.)

end = Sys.time()
end - start

stopCluster(cl)


#
ggplot(IE.iNEXT.out, aes(x = m, y = qIE, colour = Site)) + 
  geom_point(data = subset(IE.iNEXT.out, Method == "Observed"), size = 4) +
  geom_line(data = IE.iNEXT.out %>% mutate(Method = recode(Method, "Observed" = "Rarefaction")), 
            aes(lty = Method), size = 1.2) +
  geom_ribbon(aes(x = m, ymin = qIE.LCL, ymax = qIE.UCL, fill = Site), linetype = 0, alpha = 0.2) +
  labs(x = "Number of individuals", y = 'Inter-specific Encounter', title = "(a) Inter-specific Encounter") +
  facet_grid(paste("Order q = ", Order.q, sep = "") ~ paste("rho = ", rho, sep = ""), scales = "free") + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(labels = scientific_first_row) + 
  theme_bw() + 
  themes +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 13))

ggsave("Chap4_fig/Ch4_3D (a)IE size.png", dpi = 300, width = 15, height = 9)

ggplot(IE.iNEXT.out, aes(x = SC, y = qIE, colour = Site)) + 
  geom_point(data = subset(IE.iNEXT.out, Method == "Observed"), size = 4) +
  geom_line(data = IE.iNEXT.out %>% mutate(Method = recode(Method, "Observed" = "Rarefaction")), 
            aes(lty = Method), size = 1.2) +
  geom_ribbon(aes(x = SC, ymin = qIE.covLCL, ymax = qIE.covUCL, fill = Site), linetype = 0, alpha = 0.2) +
  labs(x = "Sample coverage", y = 'Inter-specific Encounter', title = "(a) Inter-specific Encounter") +
  facet_grid(paste("Order q = ", Order.q, sep = "") ~ paste("rho = ", rho, sep = ""), scales = "free") + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(labels = scientific_first_row) + 
  theme_bw() + 
  themes +
  theme(legend.title = element_blank()) + 
  coord_cartesian(xlim = c(0.93, 1))

ggsave("Chap4_fig/Ch4_3D (a)IE cov.png", dpi = 300, width = 15, height = 9)

#
ggplot(PIE.iNEXT.out, aes(x = m, y = qIE, colour = Site)) + 
  geom_point(data = subset(PIE.iNEXT.out, Method == "Observed"), size = 4) +
  geom_line(data = PIE.iNEXT.out %>% mutate(Method = recode(Method, "Observed" = "Rarefaction")), 
            aes(lty = Method), size = 1.2) +
  geom_ribbon(aes(x = m, ymin = qIE.LCL, ymax = qIE.UCL, fill = Site), linetype = 0, alpha = 0.2) +
  labs(x = "Number of individuals", y = 'Phylogenetic Inter-specific Encounter', title = "(b) Phylogenetic Inter-specific Encounter") +
  facet_grid(paste("Order q = ", Order.q, sep = "") ~ paste("rho = ", rho, sep = ""), scales = "free") + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(labels = scientific_first_row) + 
  theme_bw() + 
  themes +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 13))

ggsave("Chap4_fig/Ch4_3D (b)PIE size.png", dpi = 300, width = 15, height = 9)

ggplot(PIE.iNEXT.out, aes(x = SC, y = qIE, colour = Site)) + 
  geom_point(data = subset(PIE.iNEXT.out, Method == "Observed"), size = 4) +
  geom_line(data = PIE.iNEXT.out %>% mutate(Method = recode(Method, "Observed" = "Rarefaction")), 
            aes(lty = Method), size = 1.2) +
  geom_ribbon(aes(x = SC, ymin = qIE.covLCL, ymax = qIE.covUCL, fill = Site), linetype = 0, alpha = 0.2) +
  labs(x = "Sample coverage", y = 'Phylogenetic Inter-specific Encounter', title = "(b) Phylogenetic Inter-specific Encounter") +
  facet_grid(paste("Order q = ", Order.q, sep = "") ~ paste("rho = ", rho, sep = ""), scales = "free") + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(labels = scientific_first_row) + 
  theme_bw() + 
  themes +
  theme(legend.title = element_blank()) + 
  coord_cartesian(xlim = c(0.93, 1))

ggsave("Chap4_fig/Ch4_3D (b)PIE cov.png", dpi = 300, width = 15, height = 9)

#
ggplot(FIE.iNEXT.out, aes(x = m, y = qIE, colour = Site)) + 
  geom_point(data = subset(FIE.iNEXT.out, Method == "Observed"), size = 4) +
  geom_line(data = FIE.iNEXT.out %>% mutate(Method = recode(Method, "Observed" = "Rarefaction")), 
            aes(lty = Method), size = 1.2) +
  geom_ribbon(aes(x = m, ymin = qIE.LCL, ymax = qIE.UCL, fill = Site), linetype = 0, alpha = 0.2) +
  labs(x = "Number of individuals", y = 'Functional Inter-specific Encounter', title = "(c) Functional Inter-specific Encounter") +
  facet_grid(paste("Order q = ", Order.q, sep = "") ~ paste("rho = ", rho, sep = ""), scales = "free") + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(labels = scientific_first_row) + 
  theme_bw() + 
  themes +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 13))

ggsave("Chap4_fig/Ch4_3D (c)FIE size.png", dpi = 300, width = 15, height = 9)

ggplot(FIE.iNEXT.out, aes(x = SC, y = qIE, colour = Site)) + 
  geom_point(data = subset(FIE.iNEXT.out, Method == "Observed"), size = 4) +
  geom_line(data = FIE.iNEXT.out %>% mutate(Method = recode(Method, "Observed" = "Rarefaction")), 
            aes(lty = Method), size = 1.2) +
  geom_ribbon(aes(x = SC, ymin = qIE.covLCL, ymax = qIE.covUCL, fill = Site), linetype = 0, alpha = 0.2) +
  labs(x = "Sample coverage", y = 'Functional Inter-specific Encounter', title = "(c) Functional Inter-specific Encounter") +
  facet_grid(paste("Order q = ", Order.q, sep = "") ~ paste("rho = ", rho, sep = ""), scales = "free") + 
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(labels = scientific_first_row) + 
  theme_bw() + 
  themes + 
  theme(legend.title = element_blank()) + 
  coord_cartesian(xlim = c(0.93, 1))

ggsave("Chap4_fig/Ch4_3D (c)FIE cov.png", dpi = 300, width = 15, height = 9)





