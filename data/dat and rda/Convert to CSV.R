library(fdir)

set()

load('alcoholuse.rda')
load('smokingcomplete.rda')

summarytools::freq(alcoholuse$alcdays)

alcoholuse$drinkingfreq[alcoholuse$alcdays <= 3] <- 1
alcoholuse$drinkingfreq[alcoholuse$alcdays >= 16] <- 3
alcoholuse$drinkingfreq[alcoholuse$alcdays >= 4 & alcoholuse$alcdays <= 15] <- 2

summarytools::freq(alcoholuse$drinkingfreq)

write.csv(alcoholuse, 'alcoholuse.csv', row.names = F)
write.csv(smokingcomplete, 'smoking_complete.csv', row.names = F)
