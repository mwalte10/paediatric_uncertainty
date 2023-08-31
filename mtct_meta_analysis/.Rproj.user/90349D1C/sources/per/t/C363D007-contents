# install.packages('meta')
library(meta)
library(data.table)
library(ggplot2)
library(stringr)


get_ci <- function(tt.x, data){
  dt = data[tt == tt.x]
  main <- metaprop(event = event.e, n = n.e, data = dt, studlab = author,
                   method = "GLMM", sm = "PLOGIT",  
                   fixed = FALSE, random = TRUE, hakn = TRUE,
                   # to help with convergence issues
                   control=list(stepadj=0.5, maxiter=1000))
  est = meta:::backtransf(main$TE.random, sm = 'PLOGIT')
  lower = meta:::backtransf(main$lower.random, sm = 'PLOGIT')
  upper = meta:::backtransf(main$upper.random, sm = 'PLOGIT')
  
  return(list(tt = tt.x, est = est, lower = lower, upper = upper))
}

dt_peri <- fread('mtct_perinatal_estimates.csv')
out <- lapply(unique(dt_peri$tt)[1:4], get_ci, data = dt_peri)

dt_new_peri <- data.table(tt = unlist(lapply(out, '[[', 1)),
           est = unlist(lapply(out, '[[', 2)),
           lower = unlist(lapply(out, '[[', 3)),
           upper = unlist(lapply(out, '[[', 4)),
           time = 'peri')
dt_new_peri[tt =='sdnvp', tt := 'sdnvp_peri']


dt_bf <- fread('bf_studies.csv')
dt_bf <- dt_bf[,1:5]
out <- lapply(unique(dt_bf$tt), get_ci, data = dt_bf)

dt_new <- data.table(tt = unlist(lapply(out, '[[', 1)),
                     est = unlist(lapply(out, '[[', 2)),
                     lower = unlist(lapply(out, '[[', 3)),
                     upper = unlist(lapply(out, '[[', 4)),
                     time = 'bf')



dt <- rbind(dt_new_peri, dt_new)
dt[,lower_alt := est - (est * 0.05) * 1.96]
dt[,upper_alt := est + (est * 0.05) * 1.96]

dt$tt <- factor(dt$tt, levels = c('preg_inf', 'bf_inf', 'bf_existing_lte350',
                                  'sdnvp_peri','sdnvp_350lte', 'sdnvp_350gte', 
                                  'opt_a', 'option_a', 'optbplus_bfpreg',
                                  'optb_4plus', 'optb_onart'))

ggplot(dt, aes(tt, est)) +geom_point() + facet_wrap(~time, scales = 'free_x') + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_errorbar(aes(ymin = lower_alt, ymax = upper_alt, col = 'red'), show.legend = F) + 
  labs(y = 'Transmission rate', x = 'Treatment type')+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 3))






