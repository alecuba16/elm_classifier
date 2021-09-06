# Set R timezone
Sys.setenv(TZ='UTC') # To avoid visualization conflits (time zone is not sent by the server)

# Changes the order of the levels in a factor
dfx$alarm <- factor(dfx$alarm, levels(dfx$alarm)[c(2, 1)])

# Use plotly to plot ggplot graphics. E.g. with a density plot
qplot(wcnv_sdv_GriPhV_phsA, data = dfx, geom = "density", color = alarm)
ggplotly()

# Save a copy of the current plot
with( ss, plot(date, (as.numeric(wt_off) - 1)*(na+1), main = pos, ylab = "scode alarm", pch = 20) )
file_name <- paste0(file_patter, "_WT_OFF_pos_", ie, "th_Length_", format(wtoffxh$length[ie], digits = 0), "h.png")
dev.copy(png, file = file_name, width = 1280, height = 800, units = "px")
dev.off()

# Padding integers with leading 0 
i=1
wt <- sprintf("t%02d", i)
formatC(i, width = 2, flag = "0")

# Change java memory 
options(java.parameters = "-Xmx8000m")

# Reshaping code http://www.statmethods.net/management/reshape.html
mydata <- data.frame(id = c(1,1, 2,2),	time = c(1,2,1,2),	x1=c(5,3,6,2),	x2=c(6,5,1,4))
mydata
# example of melt function 
library(reshape)
mdata <- melt(mydata, id=c("id","time"))
mdata

# cast the melted data
# cast(data, formula, function) 
subjmeans <- cast(mdata, id~variable, mean)
subjmeans
timemeans <- cast(mdata, time~variable, mean)
timemeans

# Check time consistency
library(lubridate)
by(minute(dfs$date), dfs$model, table)

#### Make pairwise scatterplots
# download.file(url = "https://www.dropbox.com/s/b3nv38jjo5dxcl6/nba_2013.csv?dl=0#",
#               destfile = "nba_2013.csv")
nba <- read.csv("nba_2013.csv")
library(GGally)
ggpairs(nba[,c("ast", "fg", "trb")])

### Plotly from if or for into chunks
library(plotly)
b <- lapply(
    setdiff(names(iris), c("Sepal.Length","Species")),
    function(x) {
        as.widget(plot_ly(iris, 
                          x = iris[["Sepal.Length"]],
                          y = iris[[x]], 
                          mode = "markers"))
    }
)
htmltools::tagList(b)

########### Moving average ###############
# http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
set.seed(993)
x <- 1:300
y <- sin(x/20) + rnorm(300,sd=.1)
y[251:255] <- NA

# Plot the unsmoothed data (gray)
plot(x, y, type="l", col=grey(.5))
# Draw gridlines
grid()

# Smoothed with lag:
# average of current sample and 19 previous samples (red)
(f20 <- rep(1/20, 20))

y_lag <- filter(y, f20, sides=1)
lines(x, y_lag, col="red")

# Smoothed symmetrically:
# average of current sample, 10 future samples, and 10 past samples (blue)
(f21 <- rep(1/21,21))

y_sym <- filter(y, f21, sides=2)
lines(x, y_sym, col="blue")


