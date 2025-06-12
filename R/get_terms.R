


get_variables <- function(group, path) {
	f <- file.path(path, "variables", paste0("variables_", group, ".csv"))		
	if (file.exists(f)) {
		data.frame(group=group, utils::read.csv(f)	)
	} else {
		warning(paste(group, " variables do not exist"))
		NULL
	}
}


get_variable_group_names <- function(path) {
	gsub("^variables_|\\.csv$", "", list.files(file.path(path, "variables"), pattern="variables_.*.\\.csv$"))
}


accepted_variables <- function(include=NULL) {
	voc <- get_vocabulary()
	p <- vocabulary_path(voc)
	if (is.null(include)) {
		include <- get_variable_group_names(p)
		include <- gsub("variables_|\\.csv$", "", include)
	}
	v <- lapply(include, function(inc) get_variables(inc, p))
	return(do.call(rbind, v))	
}



accepted_values <- function(name) {
	p <- vocabulary_path(get_vocabulary())
	f <- file.path(p, "values", paste0("values_", name, ".csv"))
	if (file.exists(f)) {
		utils::read.csv(f)
	} else {
		NULL
	}
}

