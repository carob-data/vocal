
.vocal_environment <- new.env(parent=emptyenv())


vocabulary_path <- function(voc) {
	if (grepl("^github:", voc)) {
		voc <- gsub("^github:", "", voc)
		file.path(rappdirs::user_data_dir(), ".vocal", voc)
	} else {
		voc
	}
}


exists_vocabulary <- function() {
	TRUE
}


valid_vocabulary <- function() {
	TRUE
}

set_vocabulary <- function(voc) {
	oldvoc <- .vocal_environment$name
	if (!isTRUE(identical(voc, oldvoc))) {
		.vocal_environment$name <- voc
		.vocal_environment$checked <- FALSE
		.vocal_environment$read <- FALSE
		check_vocabulary()
		d <- try(read_vocabulary())
		if (!inherits(d, "try-error")) {
			.vocal_environment$voc <- d
			.vocal_environment$read <- TRUE
		}
	}
}

get_vocabulary <- function() {
	voc <- .vocal_environment$name
	if (is.null(voc)) {
		voc <- "github:carob-data/terminag"
		set_vocabulary(voc)
		warning("No vocabulary. Setting it to 'github:carob-data/terminag'", call. = FALSE)
	}
	voc
}


read_one_voc <- function(voc) {
		
	p <- ifelse(grepl("github:", voc), vocabulary_path(voc), voc)

	ff <- list.files(file.path(p, "variables"), pattern=paste0("^variables_.*\\.csv$"), full.names=TRUE)
	gg <- gsub("^variables_|\\.csv$", "", basename(ff))
	v <- lapply(1:length(ff), \(i) data.frame(group=gg[i], utils::read.csv(ff[i])))
	v <- do.call(rbind, v)
		
	ff <- list.files(file.path(p, "values"), pattern=paste0("^values_.*\\.csv$"), full.names=TRUE)
	values <- lapply(ff, utils::read.csv)
	names(values) <- gsub("^values_|\\.csv$", "", basename(ff))
	
	list(variables=v, values=values)
}


read_vocabulary <- function() {
	vocs <- get_vocabulary()
	if (length(vocs) == 1) {
		return(read_one_voc(vocs))
	}
	v <- lapply(vocs, read_one_voc)
	out <- v[[1]]
	for (i in 2:length(v)) {
		if (!is.null(v[[i]]$variables)) {
			out$variables <- dplyr::bind_rows(out$variables, v[[i]]$variables)
		}
		vnm <- names(v[[i]]$values)
		if (length(vnm) < 1) next
		outnm <- names(out$values)
		k <- vnm %in% outnm
		if (any(k)) {
			for (j in which(k)) {
				h <- match(vnm[j], outnm)
				out$values[[h]] <- dplyr::bind_rows(out$values[[h]], v[[i]]$values[[j]])
			}
		}
		if (any(!k)) {
			out$values <- c(out$values, v[[i]]$values[!k])
		}
	}
	out
}


clone_github <- function(name, path) {
	fgz <- tempfile()
	url <- paste0("https://api.github.com/repos/", name, "/tarball/HEAD")
	utils::download.file(url, fgz, mode="wb", quiet = TRUE)
	dzip <- tempfile()
	utils::untar(fgz, exdir=dzip)
	ff <- list.files(dzip, recursive=TRUE, full.names=TRUE)
	relff <- list.files(dzip, recursive=TRUE)
	rem <- strsplit(relff[1], "/")[[1]][1]
	outf <- file.path(path, name, gsub(rem, "", relff))
	outd <- unique(dirname(outf))
	for (d in outd) dir.create(d, FALSE, TRUE)
	exf <- list.files(path, recursive=TRUE, full.names=TRUE)
	file.remove(exf)	
	all(file.rename(ff, outf))
}


is_up2date <- function(gsha, gvoc) {
	pvoc <- vocal:::vocabulary_path(gvoc)
	f <- file.path(pvoc, "sha.txt")
	if (file.exists(f)) {
		rsha <- readLines(f)
		if (gsha == rsha) {
			return(TRUE)
		}
	}
	return(FALSE)
}




check_one_vocabulary <- function(gvoc, update, force, quiet) {

		if (!grepl("^github:", gvoc)) {
			return( TRUE)
		} 

		voc <- gsub("^github:", "", gvoc)
		pth <- vocal:::vocabulary_path(gvoc)
		
		burl <- file.path("https://api.github.com/repos", voc)
		# use GET instead to make sure it exists
		v <- readLines(file.path(burl, "commits/main"))
		gsha <- jsonlite::fromJSON(v)$sha
		
		up2d <- try(is_up2date(gsha, gvoc))
		if (inherits(up2d, "try-error")) {
			if (!quiet) message("cannot update vocabulary")
			return(FALSE)		
		}
		if (up2d) {
			if (!quiet) message("vocabulary is up-to-date")
			return(TRUE)
		}
		if (!update) {
			if (!quiet) message("the vocabulary is not up-to-date")
			return(FALSE)	
		}
		if (!quiet) message("checking for updated vocabulary")
		if (!quiet) message(paste("updating", voc, "to version", gsha)); utils::flush.console()
		updated <- try(clone_github(voc, vocabulary_path("github:")))
		if (isTRUE(updated)) {
			writeLines(gsha, file.path(pth, "sha.txt"))	
			result <- TRUE
		} else {
			if (!quiet) message("update failed"); utils::flush.console()
			result <- FALSE
		}
		result
}


check_vocabulary <- function(update=TRUE, force=FALSE, quiet=FALSE) {

	if ((!force) && isTRUE(.vocal_environment$checked)) {
		return(TRUE)
	}
	voc <- get_vocabulary()
	out <- rep(FALSE, length(voc))
	for (i in 1:length(voc)) {
		out[i] <- check_one_vocabulary(voc[i], update=update, force=force, quiet=quiet)
	}
	if (all(out)) {
		.vocal_environment$checked <- TRUE
		TRUE
	} else {
		FALSE
	}
}


obsolete_add_local <- function(voc, local_terms=NULL) {
 
	if (is.null(local_terms)) return()
	
	voc_path <- vocabulary_path(voc)
   	lf <- list.files(local_terms, recursive = TRUE) 
	if (length(lf) > 0) {
	   	pf <- list.files(voc_path, recursive = TRUE)
		for (i in 1:length(lf)) {
		  if (basename(lf[i]) %in% basename(pf)) {
		    v1 <- utils::read.csv(file.path(voc_path, pf[grepl(basename(lf[i]), pf)]))
		    if (!is.null(v1$local)) {
				v1 <- v1[!v1$local, ]
			}
			v1$local <- FALSE
			v2 <- utils::read.csv(file.path(local_terms, lf[i]))
			v2$local <- TRUE
		    v <- NULL
		    v <- try(rbind(v1, v2))
		    if (!is.null(v)) {
				d <- duplicated(v$name)	
				if (any(d)) {
					warning(paste("removing duplicate names:", paste0(d, collapse=", ")))
					v <- v[!d, ]
				}
				utils::write.csv(v, file.path(voc_path, lf[i]), row.names=FALSE)
			}
		  } else {
		    nt <- file.path(local_terms, lf[i])
		    ot <- file.path(voc_path, lf[i])
		    file.copy(nt, ot, overwrite=TRUE)
		  }
		}
	}
}




..old_update_terms <- function(voc, quiet=FALSE, force=FALSE, local_terms=NULL) {
	
	pvoc <- vocabulary_path(voc)
	dir.create(file.path(pvoc, "variables"), FALSE, TRUE)
	dir.create(file.path(pvoc, "values"), FALSE, TRUE)

	burl <- file.path("https://api.github.com/repos", voc)
   	v <- readLines(file.path(burl, "commits/main"))
	gsha <- jsonlite::fromJSON(v)$sha

	f <- file.path(pvoc, "sha.txt")
	continue <- TRUE
	if (!force && file.exists(f)) {
		rsha <- readLines(f)
		if (gsha == rsha) {
			if (!quiet) message("terms were up-to-date")
			continue <- FALSE
		}
	}
	git_updated <- FALSE
	if (continue) {
		message(paste("updating", voc, "to version", gsha)); utils::flush.console()
		writeLines(gsha, file.path(pvoc, "sha.txt"))	
		req <- httr::GET(file.path(burl, "git/trees/main?recursive=1"))
		httr::stop_for_status(req)
		ff <- sapply(httr::content(req)$tree, function(i) i$path)
		ff <- grep("\\.csv$", ff, value = TRUE)
   		rurl <- file.path("https://raw.githubusercontent.com", voc)
		ff <- file.path(rurl, "main", ff)
		i <- grepl("variables_", ff)
		pva <- c("values", "variables")[i+1]
		pva <- file.path(pvoc, pva, basename(ff))
		for (i in 1:length(ff)) {
			utils::download.file(ff[i], pva[i], quiet=TRUE)
		}
		git_updated <- TRUE
		#gv <- readLines("https://raw.githubusercontent.com/carob-data/terminag/main/version.txt", warn = FALSE)
		#gv <- trimws(unlist(strsplit(gv[grep("version", gv)], "="))[2])
		#f <- system.file("terms/version.txt", package="carobiner")
		#if (!file.exist(f)) return(TRUE)
		#rv <- readLines(f)
		#rv <- trimws(unlist(strsplit(rv[grep("version", rv)], "="))[2])
	}

	if (git_updated) {
		add_local(pvoc, local_terms)
	}
	invisible(git_updated)
}

