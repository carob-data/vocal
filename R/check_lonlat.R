
check_lonlat <- function(x) {

	answ <- data.frame(check="", msg="")[0,]

	if (!all(c("longitude", "latitude") %in% colnames(x))) {
		answ[nrow(answ)+1, ] <- c("lonlat", paste0("longitude/latitude are missing"))
		return(answ)
	}
#	path <- system.file(package="carobiner")
	p <- file.path(rappdirs::user_data_dir(), ".carob")

#	wres <- ifelse(res=="high", 1, 5)
	wres <- 1
	w <- geodata::world(path=p, res=wres)
	x <- unique(stats::na.omit(x[, c("country", "longitude", "latitude")]))
	e <- terra::extract(w, x[, c("longitude", "latitude")])
	e$country <- x$country
	e <- e[, c("NAME_0", "country")]
	i <- is.na(e$NAME_0)
	if (any(i)) {
		u <- unique(e$country[i])
		bad <- paste(u, collapse=", ")
		answ[nrow(answ)+1, ] <- c("coordinates not on land", bad)
	} 
	e <- unique(stats::na.omit(e))
	i <- e$NAME_0 != e$country
	if (any(i)) {
		u <- apply(e[i, ,drop=FALSE], 1, paste, collapse="/")
		bad <- paste(u, collapse=", ")
		answ[nrow(answ)+1, ] <- c("coordinates in wrong country", bad)
	}
	
	locvars <- c("country", paste0("adm", 1:5), "location", "site", "longitude", "latitude")
	i <- which(locvars %in% colnames(x))
	locs <- unique(x[, locvars[i]])
	xy <- unique(x[, c("longitude", "latitude")])
	if (nrow(xy) < (0.9 * nrow(locs))) {
		answ[nrow(answ)+1, ] <- c("duplicate coordinates", "fewer coordinates than locations")
	}
	return(answ)		
}

