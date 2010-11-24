


actlocator <- function(act) {
	
	try(dev.off(), silent = TRUE)
	print(act)

	day = trellis.focus()$row

	id = panel.identify( n= 1, threshold = 18*2)

	loc = act$panel.args[[day]]
	loc = c(loc$x[id] , loc$y[id] )

	date_ = names(act$packet.sizes[day])

	datetime_ = as.POSIXct(as.POSIXlt(as.Date(date_))) + loc[1]*3600
	
	data.frame(act = loc[2], datetime_ = datetime_)
	

	
}


# data(sesa)
# a = actogram(activity ~ datetime_,  dat = sesa, subset = ID == 2, strip.left = TRUE, main = "actogram")
# actlocator(a)




