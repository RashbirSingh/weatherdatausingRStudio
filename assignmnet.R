library(ggplot2)

df = read.csv('110-tavg-all-1-1999-2019.txt')
df_clean = df[-c(1:4),]
timeseries = rownames(df_clean)
colnames(df_clean)[1] <- format(df[4,1])
colnames(df_clean)[2] <- 'Average'
timeseries = paste(substr(timeseries,1,4), substr(timeseries,5,6), '01', sep = "-")
df_clean = data.frame(as.integer(df_clean$Value), as.integer(df_clean$Average), ts(timeseries))
colnames(df_clean)[1] <- format(df[4,1])
colnames(df_clean)[2] <- 'Average'
colnames(df_clean)[3] <- 'Timeseries'
rownames(df_clean) = c(1:nrow(df_clean))
#rownames(df_clean) = ts(timeseries)

write.csv(df_clean, 'output.csv')

###################################################
# EXPLORING THE DATA                              #
###################################################

print(paste0("Mean = ",mean(df_clean$Value)))
print(paste0("Variance = ", var(df_clean$Value)))
print(paste0(c('Minimum Temp = ', 'Average = ', 'Year of Minimum Temp = '), 
             df_clean[c(df_clean$Value == min(df_clean$Value)),]))
print(paste0(c('Maximum Temp = ', 'Average = ', 'Year of Maximum Temp = '),
             df_clean[c(df_clean$Value == max(df_clean$Value)),]))

###################################################
# PLOTTING                                        #
###################################################

### HISTOGRAM ###
hist(df_clean$Value, 
     xlab = 'Time',
     xaxt='n',
     yaxt='n',
     main = 'Temperature from year 1999-2019 in Fahrenheit',
     cex.main=0.8)
axis(side=1, at=c(1:nrow(df_clean)), labels=c(df_clean$Timeseries),
     lwd=0.1,
     line = NA,
     cex.axis=0.4)
axis(side=2, lwd=0.1,
     cex.axis=0.4)

### Desnity Plot ###
plot(density(df_clean$Value))

### Weather Plot ###
plot(c(1:nrow(df_clean)), 
     df_clean$Value,
     xlab = 'Time',
     ylab = 'Value',
     type = 'S',
     xaxt = 'n',
     yaxt = 'n')
axis(side=1, at=c(1:nrow(df_clean)), labels=c(df_clean$Timeseries),
     lwd=0.1,
     cex.axis=0.4)
axis(side=2, at=c(1:228), labels=seq(min(df_clean$Value), max(df_clean$Value), 1),
     lwd=0.1,
     cex.axis=0.4)
abline(h=mean(df_clean$Value), col='blue')
abline(h=min(df_clean$Value), col='red')
abline(h=max(df_clean$Value), col='green')

add_legend <- function(...) {
        opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
                    mar=c(0, 0, 0, 0), new=TRUE)
        on.exit(par(opar))
        plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
        legend(...)
} ## Reference - https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics

add_legend("topright", legend=c("Min", "Max", "Mean"), pch=20, 
           col=c("red", "green", "blue"),
           horiz=TRUE, bty='n', cex=0.8)