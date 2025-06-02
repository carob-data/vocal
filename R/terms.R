

#get_groups <- function(path) {
get_groups <- function() {
#	f <- file.path(path, "terms", "groups.csv")
#	if (!isTRUE(file.exists(f))) {
	path <- system.file("terms", package="carobiner")
	f <- file.path(path, "groups.csv")
#	}
	if (!file.exists(f)) {
		stop("the groups file is missing")
	}
	utils::read.csv(f)	
}


get_variables <- function(group, voc="carob-data/terminag") {
	p <- terms_path(voc)
	f <- file.path(p, "variables", paste0("variables_", group, ".csv"))		
	if (file.exists(f)) {
		utils::read.csv(f)	
	} else {
		NULL
	}
}

get_variable_group_names <- function(voc="carob-data/terminag") {
	p <- terms_path(voc)
	gsub("^variables_|\\.csv$", "", list.files(file.path(p, "variables"), pattern="variables_.*.\\.csv$"))
}

#get_terms <- function(type, group, path) {
accepted_variables <- function(type, group, voc="carob-data/terminag") {
	if (type=="metadata") {
		trms <- get_variables("metadata", voc)
	} else if (type=="weather") {
		trms <- get_variables("all", voc)
		if (is.null(trms)) {
			stop("Please first install the standard terms with 'carobiner::update_terms()'", call. = FALSE)
		}
		wth <- get_variables("weather", voc)
		loc <- get_variables("location")
		trms <- rbind(trms, loc, wth)
	} else { #"records", "timerecs"
		
		trms <- get_variables("all", voc)
		if (is.null(trms)) {
			stop("Please first install the standard terms with 'carobiner::update_terms()'", call. = FALSE)
		}
		grps <- get_groups()
		include <- unique(grps$include[grps$name == group])
		include <- include[include != ""]
		
		if (length(include) == 0) {
			include <- c("location", "crop", "soil", "weather")
			vnms <- get_variable_group_names()
			i <- vnms == gsub("^varieties_", "", group)
			if (any(i)) {
				include <- unique(c(include, vnms[i]))
			}
		} else {
			include <- trimws(unlist(strsplit(include, ";")))
		}

		if (length(include) > 0) {
			add <- lapply(include, function(inc) get_variables(inc, voc))
			trms <- rbind(trms, do.call(rbind, add))
			
			#for (inc in include) {
			#	add <- get_variables(inc, voc)
			#	trms <- rbind(trms, add)
			#}
		}
	}
	trms
}


accepted_values <- function(name, voc="carob-data/terminag") {
	p <- terms_path(voc)
#	p <- file.path(rappdirs::user_data_dir(), ".carob/terminag")
#	path <- system.file("terms", package="carobiner")
	f <- file.path(p, "values", paste0("values_", name, ".csv"))
	if (file.exists(f)) {
		utils::read.csv(f)
	} else {
		NULL
	}
}

