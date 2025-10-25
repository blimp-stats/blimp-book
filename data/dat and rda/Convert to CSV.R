library(fdir)

set()

load('smoking.rda')
load('smokingcomplete.rda')

write.csv(smoking, 'smoking.csv', row.names = F)
write.csv(smokingcomplete, 'smoking_complete.csv', row.names = F)
