# TODO; COG, circular mean, etc
# TODO; day, night bar
# TODO: avoid data doubling for doublePlot = TRUE 

actogram <- function(formula, 
					dat, 
					groups, 
					strip.left.classes, 
					strip.left.cex = .8, 
					doublePlot = TRUE,
					strip.left = TRUE,
					scale = 2,
					xlab = "Hours", 
					ylab = "Activity", ...) {

dat = dat

if(!inherits(formula, "formula")) stop("not a formula object.")

if(length(all.vars(formula)) !=2) stop("Formula must be of form: activity ~ time")
x = deparse(formula[[3L]])         #datetime
y = deparse(formula[[2L]])         #activity

dat[, x] = as.POSIXct(dat[, x])

dat$day  = as.Date(dat[, x] )
dat$Time = as.numeric(difftime(dat[, x], trunc(dat[, x], "day"), units = "hours"))

# double data
if(doublePlot) {
	aa = dat
	aa$Time = aa$Time + 24
	dat = rbind(dat,aa)
	}


#groups
groups = if(!missing(groups)) dat[, groups] else NULL

# xy scales
scales = list( x = list(at = 0 : (if(doublePlot) 48 else 24), 
				labels = rep(format(seq.POSIXt(trunc(Sys.time(), "day"), 
						trunc(Sys.time(), "day") +24*3600, "hours"),  "%H:%M"), 2),
				rot = 90,
				cex = .7,
				limits = c(0,(if(doublePlot) 48 else 24))),
				y = list(draw = FALSE)		
			)	

# strip.left color factor
if(missing(strip.left.classes)) { 
	strip.left.classes   = "noClass"
	dat$noClass = "a"
	}

sl = dat[!duplicated(dat$day), c("day", strip.left.classes)]
z = data.frame(table(sl[, strip.left.classes]))
z$cols = trellis.par.get("superpose.polygon")$col[1:nrow(z)]
sl =  merge(sl, z, by.x = strip.left.classes, by.y = 'Var1', all.x = TRUE )
sl = sl[order(sl$day), ]

#strip.left
if(strip.left)			
strip.left	= function(which.panel, ...) {
					LAB = sl[which.panel, "day"]
					grid.rect(gp = gpar(fill = sl[which.panel, "cols"] ))
					ltext(.5, .5, cex = strip.left.cex, LAB )
		      }

# panel
panel = function(x,y,...) {
				   y = y/scale
				   panel.xyplot(x,y,..., type = "h", cex = .5)
				   panel.rug(x, regular = FALSE,  col = 2) # non- missing data
				   panel.abline(v = 24, col = "grey")
				    # panel.number() 
				   }			  
# legend

if(!is.null(groups) & nrow(z) > 1) {
	groupLevels = levels(factor(groups))
	stripLevels = levels(factor(z$Var1))
			  
	key = list(..., adj = 1,
				text = list(stripLevels),
				rectangles = list(col = as.character(z$cols)),
				text = list(groupLevels) , 
				points = list(pch = 15, cex = 2, col =  trellis.par.get("superpose.symbol")$col[1:length(groupLevels)]),
				rep = FALSE
				)
	}	else if
	(!is.null(groups) & nrow(z) == 1) {
	groupLevels = levels(factor(groups))
	key = list(..., adj = 1,
				text = list(groupLevels) , 
				points = list(pch = 15, cex = 2, col =  trellis.par.get("superpose.symbol")$col[1:length(groupLevels)])
				)
	} else if
	(is.null(groups) & nrow(z) > 1) {
	stripLevels = levels(factor(z$Var1))
	key = list(..., adj = 1,
				text = list(stripLevels),
				rectangles = list(col = as.character(z$cols))
				)
	} else
	key = NULL
	
	

#xyplot
xyplot(..., as.formula(paste(y, "~ Time|day")), data = dat, 
			lattice.options = list(layout.widths = list(strip.left = list(x = max(nchar(dat$day)) ))),
			layout = c(1, length(unique(dat[, "day"]))), 	
			as.table = TRUE,
			groups = groups ,
			strip = FALSE,
			strip.left = strip.left,
			panel = panel,	
			xlab = xlab, 
			ylab = ylab,
			scales = scales,
			key = key
	)	

}		
