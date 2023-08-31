#install.packages("DiceDesign")
library(DiceDesign)
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)

##Testing with 2 dimensions as it is easy to visualize
dim = 2

##Samples_2 should have higher coverage than samples_1
samples_1 <- data.frame(x = sample(1:100, 10), y = sample(1:100, 10))
samples_2 <- data.frame(x = sample(1:100, 100), y = sample(1:100, 100))


dt = data.table(rbind(data.table(samples_1)[,n := 10], data.table(samples_2)[,n := 50]))
dt <- dt[,.(x,y,n)]
ggplot(dt, aes(x, y)) + geom_point() + facet_wrap(~n) + 
  geom_text(data = data.table(x = 0, y = 0, n = 10), aes(label = round(DiceDesign::coverage(samples_1),3)), col = 'red') +
  geom_text(data = data.table(x = 0, y = 0, n = 50), aes(label = round(DiceDesign::coverage(samples_2),3)), col = 'red') 
  
##For a regular mesh, coverage = 0. Thus, a small value of
##the coverage measure means that the design is close to a regular grid.
##AKA: you want this to be low
##file:///C:/Users/mwalters/Downloads/v65i11.pdf
DiceDesign::coverage(samples_1)
DiceDesign::coverage(samples_2)

##How far you are deviating from a perfectly uniform design
##I think you want this to be low
DiceDesign::discrepancyCriteria(samples_1)
DiceDesign::discrepancyCriteria(samples_2)

##Want this to be near one
DiceDesign::meshRatio(samples_1)
DiceDesign::meshRatio(samples_2)

##Min dist, want this to be low
DiceDesign::mindist(samples_1)
DiceDesign::mindist(samples_2)


###################
#Compare LHS and BRS
###################
##BRS
brs_design = data.frame(x = rnorm(n = 1e3, mean = 1, sd = 0.05),
                        y = rnorm(n = 1e3, mean = 1, sd = 0.05),
                        z = rnorm(n = 1e3, mean = 1, sd = 0.05),
                        w = rnorm(n = 1e3, mean = 1, sd = 0.05))
DiceDesign::coverage(brs_design)
DiceDesign::discrepancyCriteria(brs_design)
DiceDesign::meshRatio(brs_design)
DiceDesign::mindist(brs_design)

##Latin hypercube design
lhs = DiceDesign::lhsDesign(1e3, dimension = 4, randomized = TRUE, seed = 925)
###transform lhs from percentiles to actual numbers
lhs <- data.table(lhs$design)
lhs <- data.frame(apply(lhs, c(1,2), qnorm, mean = 1, sd = 0.05))
DiceDesign::coverage(lhs)
DiceDesign::discrepancyCriteria(lhs)
DiceDesign::meshRatio(lhs)
DiceDesign::mindist(lhs)

##Conclusions: should maybe base on coverage, discrepancy, and mindist 


ggpairs(brs_design, title="Basic random sampling") 
ggpairs(lhs, title="Latin hypercube design") 


get_sampling_summary_stats <- function(iter, method){
  if(method == 'lhs'){
    lhs = DiceDesign::lhsDesign(iter, dimension = 4, randomized = TRUE, seed = 925)
    df <- data.table(lhs$design)
    df <- data.frame(apply(df, c(1,2), qnorm, mean = 1, sd = 0.05))
  }
  if(method == 'brs'){
    df = data.frame(x = rnorm(n = iter, mean = 1, sd = 0.05),
                            y = rnorm(n = iter, mean = 1, sd = 0.05),
                            z = rnorm(n = iter, mean = 1, sd = 0.05),
                            w = rnorm(n = iter, mean = 1, sd = 0.05))
  }

  
  coverage <- DiceDesign::coverage(df)
  mesh <- DiceDesign::meshRatio(df) %>% unname()
  mindist <- DiceDesign::mindist(df)
  
  sd <- apply(df, 2, sd)
  mean <- colMeans(df)
  
  sum_stats <- list(coverage = coverage, mesh_ratio = mesh, min_dist = mindist, sd = sd, mean = mean)
  return(sum_stats)
}


# out_lhs <- lapply(5:1e3, get_sampling_summary_stats, method = 'lhs')
# 
# out_brs <- lapply(5:1e3, get_sampling_summary_stats, method = 'brs')

saveRDS(list(lhs = out_lhs, brs = out_brs), 'C:/Users/mwalters/Documents/Projects/Paed uncertainty/sampling_uncertainty.RDS')
out_lhs <- readRDS('C:/Users/mwalters/Documents/Projects/Paed uncertainty/sampling_uncertainty.RDS')$lhs
out_brs <- readRDS('C:/Users/mwalters/Documents/Projects/Paed uncertainty/sampling_uncertainty.RDS')$brs



sd_lhs <- sapply(out_lhs, FUN = get, x = 'sd')
sd_brs <- sapply(out_brs, FUN = get, x = 'sd')
rownames(sd_lhs) <- c('x', 'y', 'z', 'w')
sd_dt <- rbind(data.table(melt(sd_lhs))[,method := 'lhs'],
               data.table(melt(sd_brs))[,method := 'brs'])

sd_dt[,Var2 := Var2 + 4]

ggplot(sd_dt, aes(Var2, value, col = as.factor(method))) + geom_line() + facet_wrap(~Var1) + 
  labs(x  = 'Number of samples', y = 'Standard deviation', color = 'Sampling method') + theme_bw() +
  geom_hline(yintercept  = 0.05)


cov_lhs <- sapply(out_lhs, FUN = get, x = 'coverage')
cov_brs <- sapply(out_brs, FUN = get, x = 'coverage')
cov_lhs <- cbind(Var2 = 5:1e3, cov = cov_lhs)
cov_brs <- cbind(Var2 = 5:1e3, cov = cov_brs)

cov_dt <- rbind(data.table((cov_lhs))[,method := 'lhs'],
               data.table((cov_brs))[,method := 'brs'])

ggplot(cov_dt, aes(Var2, cov, col = as.factor(method))) + geom_line() +  
  labs(x  = 'Number of samples', y = 'Coverage', color = 'Sampling method') + theme_bw()


min_dist_lhs <- sapply(out_lhs, FUN = get, x = 'min_dist')
min_dist_brs <- sapply(out_brs, FUN = get, x = 'min_dist')
min_dist_lhs <- cbind(Var2 = 5:1e3, min_dist = min_dist_lhs)
min_dist_brs <- cbind(Var2 = 5:1e3, min_dist = min_dist_brs)

min_dist_dt <- rbind(data.table((min_dist_lhs))[,method := 'lhs'],
                data.table((min_dist_brs))[,method := 'brs'])

ggplot(min_dist_dt, aes(Var2, min_dist, col = as.factor(method))) + geom_line() +  
  labs(x  = 'Number of samples', y = 'min_dist', color = 'Sampling method') + theme_bw()

mesh_ratio_lhs <- sapply(out_lhs, FUN = get, x = 'mesh_ratio')
mesh_ratio_brs <- sapply(out_brs, FUN = get, x = 'mesh_ratio')
mesh_ratio_lhs <- cbind(Var2 = 5:1e3, mesh_ratio = mesh_ratio_lhs)
mesh_ratio_brs <- cbind(Var2 = 5:1e3, mesh_ratio = mesh_ratio_brs)

mesh_ratio_dt <- rbind(data.table((mesh_ratio_lhs))[,method := 'lhs'],
                     data.table((mesh_ratio_brs))[,method := 'brs'])

ggplot(mesh_ratio_dt, aes(Var2, mesh_ratio, col = as.factor(method))) + geom_line() +  
  labs(x  = 'Number of samples', y = 'mesh_ratio', color = 'Sampling method') + theme_bw()



##########################
###Takeaways
##########################
##SD: converges closer to the input SD when LHS is used (LHS is done at around 125 samples)
##Coverage, mesh ratio, and min dist are all about the same (very noisy)
############Not a great instance of a plateau, maybe around 500 samples?



