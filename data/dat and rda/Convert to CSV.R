library(fdir)

set()

load('math.rda')
load('mathcomplete.rda')

write.csv(math, 'math.csv', row.names = F)
write.csv(mathcomplete, 'math_complete.csv', row.names = F)

summarytools::freq(alcoholuse$alcdays)

# National Institute on Alcohol Abuse and Alcoholism
alcoholuse$drinkingfreq[alcoholuse$alcdays <= 0] <- 0
alcoholuse$drinkingfreq[alcoholuse$alcdays >= 8] <- 2
alcoholuse$drinkingfreq[alcoholuse$alcdays >= 1 & alcoholuse$alcdays <= 7] <- 1

alcoholuse$drinker[alcoholuse$alcdays <= 0] <- 0
alcoholuse$drinker[alcoholuse$alcdays >= 1] <- 1

summarytools::freq(alcoholuse$drinkingfreq)
summarytools::freq(alcoholuse$drinker)


write.csv(alcoholuse, 'alcoholuse.csv', row.names = F)
write.csv(smokingcomplete, 'smoking_complete.csv', row.names = F)
