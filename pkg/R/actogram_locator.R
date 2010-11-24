


actlocator <- function(act, ...) {
	
	print(act)

	 plocs = trellis.currentLayout()

	 o = list()
	for( i in 1:nrow(plocs) ) {
		trellis.focus("panel", column = 1, row  = i)
		id = panel.identify(...)
		pn = act$panel.args[[i]]
		pn = data.frame(x = pn$x, y = pn$y)
		pn = pn[id, ]
		date_ = names(act$packet.sizes[i])
		pn$x = as.POSIXct(as.POSIXlt(as.Date(date_))) + pn$x*3600
		o[[i]] = pn
		}
	
	   return(do.call('rbind',  o))

}




# data(sesa)
#a = actogram(activity ~ datetime_,  dat = sesa, subset = ID == 2, strip.left = TRUE, main = "actogram")
#  x = actlocator(a)




