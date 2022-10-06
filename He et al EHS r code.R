#data explore and fig 3

#explore the data

hist(FarmVM$BF)
hist(FarmVM0$BF)
hist(FarmVM1$BF)
hist(FarmVM$pAvgR)
hist(FarmVM0$pAvgR)
hist(FarmVM1$pAvgR)

shapiro.test(FarmVM$BF)
shapiro.test(FarmVM0$BF)
shapiro.test(FarmVM1$BF)
shapiro.test(FarmVM$pAvgR)
shapiro.test(FarmVM0$pAvgR)
shapiro.test(FarmVM1$pAvgR)


#wilcox test

wilcox.test(FarmVM1msb$BF~FarmVM1msb$PhelpedMe)
wilcox.test(FarmVM1msb$breedingF0712~FarmVM1msb$PhelpedMe)
wilcox.test(FarmVM1msb$pAvgR~FarmVM1msb$PhelpedMe)
wilcox.test(FarmVM1msb$BFPartner~FarmVM1msb$PhelpedMe)


#fig 3
# breeding female (not reproductive-age women)
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,3))
#FarmVM1ms$PhelpedMe <- as.factor(FarmVM1ms$PhelpedMe)
boxplot (FarmVMP1ms$BF ~ FarmVMP1ms$PhelpedMe, title= "",
         #ylim = c(0, 8),
         ylab = "Reproductive-age women in my household",
         xlab = "Husband helped my household")
boxplot (FarmVM1msb$breedingF0712 ~ FarmVM1msb$PhelpedMe,
         #ylim = c(0, 5),
         ylab = "Breeding females in my household",
         xlab = "Husband helped my household")

boxplot (FarmVM1msb$pAvgR ~ FarmVM1msb$PhelpedMe,
         #ylim = c(0, 0.5),
         ylab = "Average relatedness of spouse to my household",
         xlab = "Husband helped my household")
par(opar)




