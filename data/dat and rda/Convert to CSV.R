library(fdir)

set()

load('alcoholuse.rda')
load('smokingcomplete.rda')

summarytools::freq(alcoholuse$alcdays)

alcoholuse$drinkingfreq[alcoholuse$alcdays <= 0] <- 0
alcoholuse$drinkingfreq[alcoholuse$alcdays >= 16] <- 2
alcoholuse$drinkingfreq[alcoholuse$alcdays >= 1 & alcoholuse$alcdays <= 15] <- 1

summarytools::freq(alcoholuse$drinkingfreq)

write.csv(alcoholuse, 'alcoholuse.csv', row.names = F)
write.csv(smokingcomplete, 'smoking_complete.csv', row.names = F)
