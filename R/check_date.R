
check_date <- function(x, name, trms=NULL) {

	answ <- data.frame(check="", msg="")[0,]

	x <- stats::na.omit(x[[name]])
	if (length(x) == 0) return(answ)
	if (any(grepl(";", x))) {
		if (!is.null(trms$name)) {
			i <- match(name, trms$name)
			if (length(i) == 1) {
				if (isTRUE(trms$multiple_allowed[i] != "yes")) {
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
		answ[nrow(answ)+1, ] <- c("datespan", paste(sum(i), "records with", enddate, "within", smin, "days of", startdate))
	} 
	i <- d > smax #365
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("datespan", paste(sum(i), enddate, "more than", smax, "days after", startdate))
	} 
	answ
}

