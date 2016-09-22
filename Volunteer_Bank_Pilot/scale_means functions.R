


### An example of when it may be useful- bases per at bat ###
AB1= c(1, 0, 2, 4, 0, NA, 1, 3, 0)
names(AB1)= c("smolinsky", "valencia", "vogt", "davis", "alonso", "healy", "eibner", "muncy", "pinder")
AB1
AB2= c(0, 4, 0, NA, 1, 1, 2, 1, 0)
names(AB2)= c("smolinsky", "valencia", "vogt", "davis", "alonso", "healy", "eibner", "muncy", "pinder")
AB3= c(1, 4, 2, 3, 0, 0, 0, 1, 1)
names(AB3)=c("smolinsky", "valencia", "vogt", "davis", "alonso", "healy", "eibner", "muncy", "pinder")
game=as.data.frame(cbind(AB1, AB2, AB3))


#Function for scale means#

#Version 1- requires entering a vector of variable names
scale.means = function (df, vars) {
  mean_vars=rowMeans(df[,vars])
  return(mean_vars)
}
game$avg_3=scale.means(game, c("AB1", "AB2"))

#Version 2- just enter variables names
scale.means= function (df, ..., na.rm=FALSE) {
  vars=unlist(list(...))
  mean_vars=rowMeans(df[,vars], na.rm=na.rm)
  return(mean_vars)
}
game$avg_4=scale.means(game, "AB1", "AB2", "AB3", na.rm=T)
