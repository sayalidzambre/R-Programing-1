
lunCap <- read.csv("C:\\Users\\OWNER\\Desktop\\BDAP\\LungCapData.csv")

attach(lunCap)
attributes(lunCap)["names"]

plot(Height, LungCap, main = "Height vs Length",
     cex = 0.5, asp = plot.window, pch = 8, col = 4)


plot(Height, Age, main = "Height vs Age",
     cex = 0.5, asp = plot.window, pch = 8, col = 4)
abline(lm(Age ~ Height))


plot(LungCap, Age, main = "LungCap vs Age",
     cex = 0.5, asp = plot.window, pch = 8, col = 4)
abline(lm(Age ~ LungCap))


plot(LungCap, Height, main = "LungCap vs Age",
     cex = 0.5, asp = plot.window, pch = 8, col = 4)
abline(lm(Height ~ LungCap))


hist(LungCap, axes = T, breaks = c(0,2,4,6,8, 10, 12, 14, 16),
      freq = F, ylim = c(0, .15))
lines(density(LungCap), col = 2, lwd = 3)


barplot(table(Gender), horiz = T, col = 3)

dotchart(LungCap)

boxplot(Age, main = "BoxPlot for Age")

boxplot(LungCap~Smoke)
boxplot(LungCap[Gender == "female"], LungCap[Gender == "male"])


