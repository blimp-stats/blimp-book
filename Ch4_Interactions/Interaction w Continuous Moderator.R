# installation guide at https://github.com/blimp-stats/fsem/blob/main/README.md
# requires blimp installation at www.appliedmissingdata.com/blimp
# install.packages('ggplot2')
# install.packages('remotes')
# remotes::install_github('blimp-stats/rblimp')

# load packages
library(rblimp)
library(ggplot2)

# load data from the internet
connect <- url('https://raw.githubusercontent.com/craigenders/ies/main/behaviorachievement.Rdata', 'rb')
load(connect); close(connect)

# analysis 1: regression with all numeric variables
model1 <- rblimp(
  data = reading,
  model = 'read9 ~   lrnprob1', # automatic background models for incomplete predictors
  seed = 90291,
  burn = 10000,
  iter = 10000)

# summarize results
output(model1)
posterior_plot(model1,'read9')

# analysis 2: moderated regression w simple slopes
model2 <- rblimp(
  data = reading, 
  nominal = 'atrisk', # automatic dummy coding with lowest score as the reference 
  center = 'read1 lrnprob1',
  model = 'read9 ~ read1 lrnprob1 atrisk read1*lrnprob1', # product in focal model    
  simple = 'read1 | lrnprob1', # conditional effects of read1 at different levels of lrnprob1        
seed = 90291,                                              
burn = 10000,                                              
iter = 10000)                                                

# summarize results
output(model2)
posterior_plot(model2,'read9')

# graph simple slopes and Johnson-Neyman regions of significance
simple_plot(read9 ~ read1 | lrnprob1, model2)
jn_plot(read9 ~ read1 | lrnprob1, model2)

