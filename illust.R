needs(scales)


needs(gridExtra)

plot1 <- dat.training%>%
filter(node=="rnd_1368.volume", datetime>=as.POSIXct("2017-08-01", tz="UTC"),datetime<as.POSIXct("2017-08-02", tz="UTC"))%>%
mutate(node="original")%>%
ggplot(aes(x = datetime, y = value, col=node, group=node)) + labs(y="original", col="")+
geom_line()+scale_x_datetime(labels = date_format("%H:%M"),expand = c(0, 0), date_breaks ="3 hours")

plot2 <- dat.training%>%
filter(node=="rnd_1368.volume", datetime>=as.POSIXct("2017-08-01", tz="UTC"),datetime<as.POSIXct("2017-08-02", tz="UTC"))%>%
mutate(node="detrended")%>%
ggplot(aes(x = datetime, y = prepared, col=node, group=node)) + labs(y="detrended", col="")+
geom_line()+scale_x_datetime(labels = date_format("%H:%M"),expand = c(0, 0), date_breaks ="3 hours")


grid.arrange(plot1, plot2, nrow=2)


dat.training%>%
filter(node=="rnd_95345.volume", datetime>=as.POSIXct("2017-08-01", tz="UTC"),datetime<as.POSIXct("2017-08-02", tz="UTC"))%>%
ggplot(aes(x = datetime, y = prepared, col=node, group=node)) + 
geom_line()+scale_x_datetime(labels = date_format("%H:%M"),expand = c(0, 0), date_breaks ="3 hours")