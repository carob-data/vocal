

get_variables <- function(group, voc="carob-data/terminag") {
	p <- terms_path(voc)
	f <- file.path(p, "variables", paste0("variables_", group, ".csv"))		
	if (file.exists(f)) {
		data.frame(group=group, utils::read.csv(f)	)
	} else {
		warning(paste(group, " variables do not exist"))
		NULL
	}
}

get_variable_group_names <- function(voc) {
	p <- terms_path(voc)
	gsub("^variables_|\\.csv$", "", list.files(file.path(p, "variables"), pattern="variables_.*.\\.csv$"))
}


accepted_variables <- function(voc="carob-data/terminag", include=NULL) {
	if (is.null(include)) {
		include <- get_variable_group_names(voc)
		include <- gsub("variables_|\\.csv$", "", include)
	}
	v <- lapply(include, function(inc) get_variables(inc, voc))
	return(do.call(rbind, v))	
}



accepted_values <- function(name, voc="carob-data/terminag") {
	p <- terms_path(voc)
	f <- file.path(p, "values", paste0("values_", name, ".csv"))
	if (file.exists(f)) {
		utils::read.csv(f)
	} else {
		NULL
	}
}

