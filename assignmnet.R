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

print(paste0("Mean = ",mean(df_clean$Value)))
print(paste0("Variance = ", var(df_clean$Value)))

###################################################
# PLOTTING                                        #
###################################################

par(mfrow = c(1, 2))
hist(df_clean$Value, 
     xlab = 'Time')
plot(density(df_clean$Value))