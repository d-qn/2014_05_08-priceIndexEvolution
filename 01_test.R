############################################################################################
###		SETTINGS
############################################################################################

#http://dw-swissinfo.s3-website-us-west-2.amazonaws.com/PE5if/1/

source("~/swissinfo/_helpers/helpers.R")

data <- read.csv("IPC_suisse_00_14.csv", header = T, row.names = 1)

dates <- as.POSIXct(gsub("^X", "", colnames(data)), format = "%m.%d.%y")
colnames(data) <- dates

data <- cbind(biens = rownames(data), data)

data.r <- reshape(data, direction = "long", varying = list(colnames(data)[-1]))

data.r$time <- dates[data.r$time]
colnames(data.r)[3] <- 'value'


data <- ddply(data.r, .(biens), summarize, pc = ((value - value[1]) / value[1]) * 100, time = time)

#ggplot(data = data, aes(time, pc)) + geom_line(aes(group = biens, color = biens)) + ggtheme_ygrid


library(rCharts)
data2 <- data
data2$time <- as.Date(data2$time)
data2$pc <- round(data2$pc,1)
pricePlot <- nPlot(
	pc ~ time,
	data = data2,
	group = "biens",
	type = "lineChart", height=500,width=700)

pricePlot$xAxis(tickFormat="#!function(d) {return d3.time.format('%Y')(new Date( d * 86400000 ));}!#", axisLabel = "Year")
pricePlot$yAxis(axisLabel = "Price change in %", width = 55)
#pricePlot$chart(color = swi_22palette)
#pricePlot

pricePlot$chart(tooltipContent = "#! function(key, x, y){
      return '<h3>' + key + '</h3>' +
      '<p>' + y + '% in ' + x + '</p>'
      } !#")

ids <- unique(data2$biens)
defaultLines <- as.logical(ids %in% c('Habillement et chaussures', 'Autres biens et services',
'Equipement ménager et entretien courant', 'Transport', 'Santé'))
pricePlot$set(disabled = defaultLines)

pricePlot$publish("line chart CPI ", host = "rpubs")





