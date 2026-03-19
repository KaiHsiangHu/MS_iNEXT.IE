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



## =========================== Figure 1 ~ 2 ===================================
## 700 x 600
int = 0.1 
N1 = c(4, 4, 8, 2)
N2 = c(4, 4, 8)
N3 = c(2, 2, 4)
N4 = c(1, 1, 6)

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



## ==================== Figure 3 ~ 4, appendix S7 ===================================
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

## Figure 3 ~ 4, appendix S7

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





## ==================== Figure 5, 6 =======================================

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


sp_ind = data.frame("sp" = c(colSums(bci_abun > 0), colSums(Danum_abun > 0), colSums(Wanang_abun > 0)),
                    "ind" = c(colSums(bci_abun), colSums(Danum_abun), colSums(Wanang_abun)),
                    "Data" = rep(c("BCI", "Danum Valley", "Wanang"), each = 1250))

## Figure 5(a)
ggplot(sp_ind) + 
  geom_point(aes(x = ind, y = sp, colour = Data), size = 0.8) + 
  scale_colour_manual(values = c("red", "blue", "purple")) +
  facet_grid(. ~ Data) +
  labs(x = "Number of individuals", y = "Number of species", title = "(a)") +
  theme_bw() +
  themes +
  # guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = "none")
ggsave("../(a).png", dpi = 300, width = 9, height = 3.8)


## Figure 5(b)
ggplot(sp_ind) + 
  geom_histogram(aes(x = sp, fill = Data), bins = 50, colour = "white") + 
  scale_fill_manual(values = c("red", "blue", "purple")) +
  facet_grid(. ~ Data, scales = "free_x") +
  labs(x = "Number of species", y = "區塊數", title = "(b) Histogram of species richness per quadrat") +
  theme_bw() +
  themes +
  theme(legend.position = "none")
ggsave("../(b).png", dpi = 300, width = 9, height = 3.8)

## Figure 5(c)
ggplot(sp_ind) + 
  geom_histogram(aes(x = ind, fill = Data), bins = 50, colour = "white") + 
  scale_fill_manual(values = c("red", "blue", "purple")) +
  facet_grid(. ~ Data, scales = "free_x") +
  labs(x = "Number of individuals", y = "區塊數", title = "(c) Histogram of individual counts per quadrat") +
  theme_bw() +
  themes
ggsave("../(c).png", dpi = 300, width = 9, height = 4)



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


## Figure 6(a)
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


## Figure 6(b)
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




## ==================== Figure 7 ~ 10 ==================================
# library(iNEXT)
# data = read.table("Proposal/Brazil_Data.txt")
load("spider.rda")

DataInfo3D(spider)

Hill.spider = ObsAsy3D(spider, q = seq(0, 2, 0.1), method = "Observed", nboot = 0) %>% .[,1:3]
IE.spider = lapply(1:length(spider), function(i) data.frame(Assemblage = names(spider)[i],
                                                            Order.q = seq(0.5, 2, 0.1),
                                                            qTD = IE(spider[[i]], seq(0.5, 2, 0.1)))
                   ) %>% do.call(rbind,.)

## Figure 7(a)
fig_Hill_spider = 
  Hill.spider %>% mutate(Type = "Taxonomic diversity") %>% 
  ggplot() + 
  geom_line(aes(x = Order.q, y = qTD, colour = Assemblage), size = 1.2) +
  labs(x = "Order q", y = 'Hill numbers') +
  theme_bw() + themes + theme(legend.box.margin = margin(0, 0, 0, 0)) 


## Figure 7(b)
fig_IE_spider = 
  IE.spider %>% mutate(Type = "Inter-specific encounter") %>% 
  ggplot() + 
  geom_line(aes(x = Order.q, y = log2(qTD), colour = Assemblage), size = 1.2) +
  labs(x = "Order q", y = expression(paste(log[2], '(Inter-specific encounter)'))) +
  theme_bw() + themes + theme(legend.box.margin = margin(0, 0, 0, 0)) 


## Figure 7
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

## Figure 8
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


## Figure 9
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


## Figure 10
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



