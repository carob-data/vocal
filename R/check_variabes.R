

check_known <- function(x, trms) {
	answ <- data.frame(check="", msg="")[0,]
	nms <- names(x)
	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		answ[nrow(answ)+1, ] <- c("unknown variables", paste(xnms, collapse=", "))
	}
	answ
}

check_required <- function(x, trms, type="") {
	answ <- data.frame(check="", msg="")[0,]
	
	req <- trms[trms$required == "yes", "name"]  #| trms$required == group
	r <- req[!(req %in% names(x))]
	if (length(r) > 0) {
		if (type != "") type <- paste0(type, ": ")
		answ[nrow(answ)+1, ] <- c("missing variables", paste0(type, paste(r, collapse=", ")))
	}
	answ
}

check_dups <- function(x) {
	answ <- data.frame(check="", msg="")[0,]
	nms <- names(x)
	tnms <- table(nms)
	if (any(tnms>1)) {
		tnms <- paste(tnms[tnms>1], collapse=", ")
		answ[nrow(answ)+1, ] <- c("duplicate variables", tnms)		
	}
	answ
}

check_variables <- function(x, trms, required=TRUE, duplicates=TRUE) {
	known <- check_known(x, trms)
	if (required && duplicates) {
		rbind(known, check_required(x, trms), check_dups(x))
	} else if (required) {
		rbind(known, check_required(x, trms))
	} else if (duplicates) {
		rbind(known, check_dups(x))	
	} else {
		known
	}
}

