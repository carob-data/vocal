

terms_path <- function(voc) {
	file.path(rappdirs::user_data_dir(), ".vocal", voc)
   #list(
#	   git_path="carob-data/terminag",
 #      local_path= local_terms
 #  )
}


old_update_terms <- function(voc, quiet=FALSE, force=FALSE, local_terms=NULL) {
	
	pvoc <- terms_path(voc)
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


update_terms <- function(voc, quiet=FALSE, force=FALSE, local_terms=NULL) {
	
	pvoc <- terms_path(voc)
	#dir.create(file.path(pvoc, "variables"), FALSE, TRUE)
	#dir.create(file.path(pvoc, "values"), FALSE, TRUE)

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
		if (clone_github(voc, terms_path(""))) {
			writeLines(gsha, file.path(pvoc, "sha.txt"))	
			git_updated <- TRUE
		} else {
			message("update failed"); utils::flush.console()
		}
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




add_local <- function(voc, local_terms=NULL) {
 
	if (is.null(local_terms)) return()
	
	voc_path <- terms_path(voc)
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


