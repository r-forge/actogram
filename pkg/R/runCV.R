

runCV = function(x, datetime, periods) {
	x = fts(data = x , dates = datetime)
	msd = moving.sd(x, periods = periods)
	mm = moving.mean(x, periods = periods)
	data.frame(datetime = attributes(msd)$dates, CV = as.numeric(msd[,1])/as.numeric(mm[,1]) )
}


