library(HistData)
data(Nightingale)
library(reshape)
library(ggplot2)
install.packages('reshape')

Night<- Nightingale[,c(1,8:10)]
melted <- melt(Night, "Date")
names(melted) <- c("Date", "Cause", "Deaths")
melted$Cause <- sub("\\.rate", "", melted$Cause)
melted$Regime <- ordered( rep(c(rep('Before', 12), rep('After', 12)), 3), 
                          levels=c('Before', 'After'))
Night <- melted

# subsets, to facilitate separate plotting
Night1 <- subset(Night, Date < as.Date("1855-04-01"))
Night2 <- subset(Night, Date >= as.Date("1855-04-01"))

# sort according to Deaths in decreasing order
Night1 <- Night1[order(Night1$Deaths, decreasing=TRUE),]
Night2 <- Night2[order(Night2$Deaths, decreasing=TRUE),]

# merge the two sorted files
Night <- rbind(Night1, Night2)

# Before plot
cxc1 <- ggplot(Night1, aes(x = factor(Date), y=Deaths, fill = Cause)) +
  geom_bar(width = 1, position="identity", stat="identity", color="black") +
  scale_y_sqrt() 
# A coxcomb plot = bar chart + polar coordinates
cxc1 + coord_polar(start=3*pi/2) + 
  ggtitle("APRIL 1854 - MARCH 1855") + 
  xlab("")

# After plot
cxc2 <- ggplot(Night2, aes(x = factor(Date), y=Deaths, fill = Cause)) +
  geom_bar(width = 1, position="identity", stat="identity", color="black") +
  scale_y_sqrt()
cxc2 + coord_polar(start=3*pi/2) +
  ggtitle("APRIL 1855 - MARCH 1856") + 
  xlab("")
