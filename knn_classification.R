library(kknn)
obs <- seq(1:14)
temperature <- c(85,80,83,70,68,65,64,72,69,75,75,72,81,71)
humidity <- c(85,90,86,96,80,70,65,95,70,80,70,90,75,91)
play <- c('no','no','yes','yes','yes','no','yes','no','yes','yes','yes','yes','yes','no')
length(play)
# only one in testset
golf <- data.frame(obs = obs, temperature =temperature,humidity = humidity,play = play)
golf.train <- golf[c(-1,-2,-3),]
golf.test <- golf[1,2:3]
golf.kknn <- kknn(play ~., golf.train, golf.test, k =3, scale = F, distance = 1, kernel = "rectangular")
golf.kknn$CL #neighbour's class
golf.kknn$D #distance
golf.kknn$C #obs number

golf.tkknn <- train.kknn(play ~.,golf[-1,c(2:3,4)],kernel = c("rectangular", "triangular", "epanechnikov", "optimal"),distance=2,scale=T)
plot(golf.tkknn)
golf.tkknn$MISCLASS   #显示错误率
golf.tkknn  #输出最优参数情况
