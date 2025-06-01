
check_ranges <- function(x, trms, answ) {
	nms <- colnames(x)
	trms <- trms[match(nms, trms[,1]), ]

	trmsna <- trms[!is.na(trms$NAok), ]
	trmsna <- trmsna[trmsna$NAok == "no", ]

	bad <- NULL
	for (i in 1:nrow(trmsna)) {
		v <- x[[trmsna$name[i]]]
		if (any(is.na(v))) {
			bad <- c(bad, trmsna$name[i])
		}
	}
	if (!is.null(bad)) {
		answ[nrow(answ)+1, ] <- c("NA terms", paste("NA in:", paste(bad, collapse=", ")))
		bad <- NULL
	}
	
	trms <- stats::na.omit(trms[, c("name", "valid_min", "valid_max"), ])
	if (nrow(trms) == 0) return(answ)
	for (i in 1:nrow(trms)) {
		rng <- unlist(trms[i,c("valid_min", "valid_max")])
		v <- stats::na.omit(x[[trms$name[i]]])
 		if ( any((v < rng[[1]]) | (v > rng[2])) ) {
			ok <- FALSE
			vrng <- range(v, na.rm=TRUE)
			if (is.numeric(vrng)) vrng <- round(vrng, 3)
			msg  <- paste0(trms$name[i], " (", vrng[1], ", ", vrng[2], ")")
			bad  <- c(bad, msg)
		}
	}
	if (!is.null(bad)) {
		answ[nrow(answ)+1, ] <- c("bounds", paste("out of bounds:", paste(bad, collapse=", ")))
		bad <- NULL
	}
	
	if (!is.null(bad)) {
		bad <- paste(bad, collapse=", ")
		answ[nrow(answ)+1, ] <- c("invalid", paste0("invalid: ", bad))
		bad <- NULL
	}

}


check_type_range <- function(x, trms) {
	answ <- data.frame(check="", msg="")[0,]
	nms <- colnames(x)
	trs <- trms[match(nms, trms[,1]), ]
	cls <- sapply(x, class)
	if (is.list(cls)) {
		i <- sapply(cls, length)
		i <- names(i[i>1])
		stop(paste("    bad datatype:", paste(i, collapse=", ")))
	}
	cls <- cbind(cls, trs$type, nms)
	cls <- cls[cls[,2] != "", ]
	i <- (cls[,1] == "integer") & (cls[,2] == "numeric")
	cls[i, 1] <- "numeric"
	i <- cls[,1] != cls[,2]
	if (any(i)) {
		bad <- paste(cls[i,3], collapse=", ")
		answ[nrow(answ)+1, ] <- c("bad datatype", paste("bad datatype:", bad))
	} else {
		answ <- check_ranges(x, trms, answ)
	}	
	answ
}



check_caps <- function(x, locvars, minchar=5, frac=0) {
	answ <- data.frame(check="", msg="")[0,]
	stopifnot(frac >= 0 && frac <= 1)
	for (v in locvars) {
		m <- stats::na.omit(x[[v]])
		if (minchar > 0) {
			m <- m[nchar(m) >= 5]
		}
		i <- sum(toupper(m) == m)
		if (i > (frac * length(m))) {
			answ[nrow(answ)+1, ] <- c("all uppercase",
				paste0("names in uppercase: ", v))
		}
	}
	answ
}



check_date <- function(x, name, voc=NULL) {

	answ <- data.frame(check="", msg="")[0,]

	x <- stats::na.omit(x[[name]])
	if (length(x) == 0) return(answ)
	if (any(grepl(";", x))) {
		if (!is.null(voc$name)) {
			i <- match(name, voc$name)
			if (length(i) == 1) {
				if (isTRUE(voc$multiple_allowed[i] != "yes")) {
					answ[nrow(answ)+1, ] <- c("date", paste0("multiple dates in: ", name))
				}
			}
		}
		x <- unlist(strsplit(x, ";|; "))
	}
	
	n <- nchar(x)
	if (any(!(n %in% c(4, 7, 10)))) {
		answ[nrow(answ)+1, ] <- c("date", paste0("invalid date format(s) in: ", name))
	}
	today <- as.Date(Sys.time())
	ymd <- x[n==10]
#	if (any(is.na(ymd))) {
#		return(FALSE)
#	}
	if (length(ymd) > 0) {
		d <- as.Date(ymd)
		if (any(ymd < as.Date("1960-01-01"))) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1960 in: ", name))
		}
		if (any(ymd > today)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("future date(s) in: ", name))
		}
		m <- as.numeric(substr(ymd, 6, 7))
		if (any((m < 1) | (m > 12))) {
			answ[nrow(answ)+1, ] <- c("date", paste0("months not between 1 and 12): ", name))
		} 
	}
	
	thisyear <- as.numeric(format(today, "%Y"))
	today <- as.character(today)
	ym <- x[n==7]
	if (length(ym) > 0) {
		d <- substr(ym, 5, 5)
		if (any(d != "-")) {
			answ[nrow(answ)+1, ] <- c("date", paste0("bad date(s) in: ", name))
		}
		y <- as.numeric(substr(ym, 1, 4))
		if (any(y < 1960)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1960 in: ", name))
		} 
		if (any(y > thisyear)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) after ", thisyear, " in: ", name))
		}

		m <- as.numeric(substr(ym, 6, 7))
		if (any((m < 1) | (m > 12))) {
			answ[nrow(answ)+1, ] <- c("date", paste0("months not between 1 and 12): ", name))
		} 
	}

	y <- x[n==4]
	if (length(y) > 0) {
		y <- as.numeric(y)
		if (any(y < 1960)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1960 in: ", name))
		} 
		if (any(y > thisyear)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("date(s) after ", thisyear, " in: ", name))
		}
	}
	answ
}


check_datespan <- function(x, startdate, enddate, smin=0, smax=366) {

	answ <- data.frame(check="", msg="")[0,]

	dstart <- x[[startdate]]
	dend <- x[[enddate]]
	if (is.null(dstart) || is.null(dend)) {
		return(answ)
	}
	i <- which((nchar(dstart) == 10) & (nchar(dend) == 10))
	if (length(i) == 0) return(NULL)
	s <- as.Date(dstart[i])
	e <- as.Date(dend[i])
	d <- as.numeric(e - s)
	i <- d < smin #45
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("datespan", paste(sum(i), " end date(s) within ", smin, " days after start date(s)"))
	} 
	i <- d > smax #365
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("datespan", paste(sum(i), " end date(s) more than ", smax, " days after start date(s)"))
	} 
	answ
}


check_span <- function(x, start, end, smin=0, smax=366) {
	answ <- data.frame(check="", msg="")[0,]

	s <- x[[start]]
	e <- x[[end]]
	if (is.null(s) || is.null(e)) {
		return(answ)
	}
	d <- (e - s)
	i <- d < smin #45
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("span", paste(sum(i), " end(s) within ", smin, " of start(s)"))
	} 
	i <- d > smax #365
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("span", paste(sum(i), " end(s) more than ", smax, " days after start(s)"))
	} 
	answ
}


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
		answ[nrow(answ)+1, ] <- c("not on land",
				paste0("coordinates not on land for: ", bad))
	} 
	e <- unique(stats::na.omit(e))
	i <- e$NAME_0 != e$country
	if (any(i)) {
		u <- apply(e[i, ,drop=FALSE], 1, paste, collapse="/")
		bad <- paste(u, collapse=", ")
		answ[nrow(answ)+1, ] <- c("wrong country",
				paste0("coordinates/country conflict: ", bad))
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


.check_empty <- function(x, answ) {
	bad <- rep(FALSE, ncol(x))
	chars <- sapply(x, is.character)
	for (i in which(chars)) {
		x[,i] <- trimws(x[,i])
		bad[i] <- any(stats::na.omit(x[,i]) == "")
	}
	if (any(bad)) {
		b <- paste0(colnames(x)[bad], collapse= ", ")
		answ[nrow(answ)+1, ] <- c("whitespace variable", paste("   whitespace in variable: ", b))
	}
	answ
}

#d = data.frame(a = 1:3, b=letters[1:3], c=c(" A ", "", "D"))
#x = check_empty(d)

