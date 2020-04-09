setwd("D:/Dropbox/OneDrive/publish/libin_zhang/R/")
#Stratified Sampling
## split data into a train and test set
require(sampling)

sub_train=strata(csf,stratanames=("Species"),size=rep(18,31),method="srswor")
head(sub_train)
trainset=csf[sub_train$ID_unit,]
testset=csf[-sub_train$ID_unit,]