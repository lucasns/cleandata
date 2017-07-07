read_dataset = function(file) {
    dataset = read.csv(file)
    return(dataset)
}


filter_factor = function(dataset, var, val) {
    dataset[dataset[[var]] %in% val & !is.na(dataset[[var]]),]
}


filter_num = function(data, var, begin, end) {
    data[data[[var]] >= begin & data[[var]] <= end & !is.na(data[[var]]),]
}


get_info = function(dataset) {
    types = colnames(dataset)
    info = list()
    for (t in colnames(dataset)) {
        col = dataset[[t]]
        if (is.numeric(col)) {
            info[[t]] = list(type = "numeric",
                             values = c(min(col, na.rm = TRUE),
                                        max(col, na.rm = TRUE))
            )

        } else if(is.factor(col)) {
            info[[t]] = list(type = "factor", values = levels(col))
        }
    }

    return(info)
}


get_summary = function(dataset) {
    numeric_summary = function(x, na.rm=TRUE){
        result = list(Type = class(x),
                      Mean=mean(x, na.rm=na.rm),
                      SD=sd(x, na.rm=na.rm),
                      Median=median(x, na.rm=na.rm),
                      Min=min(x, na.rm=na.rm),
                      Max=max(x, na.rm=na.rm),
                      Missing=paste0(sum(is.na(x)), " (",
                                     round(100*sum(is.na(x)/length(x)),2),
                                     "%)")

        )
    }

    factor_summary = function(x){
        result = list(Type = class(x),
                      Levels = nlevels(x),
                      Missing=paste0(sum(is.na(x)), " (",
                                     round(100*sum(is.na(x)/length(x)),2),
                                     "%)")
        )
    }

    total_summary = function(dataset){
        result = list("Nº types" = nrow(table(sapply(test, class))),
                      Missing= nrow(dataset[!complete.cases(dataset),])
        )
    }

    num_cols = sapply(dataset, is.numeric)
    factor_cols = !num_cols

    num_info = sapply(dataset[, num_cols], numeric_summary)
    factor_info = sapply(dataset[, factor_cols], factor_summary)
    total_info = matrix(total_summary(dataset),
                        dimnames = list(names(a), "Total"))

    return(list(numeric = num_info, factor = factor_info, total = total_info))
}


apply_modifier = function(x, modifier) {
    if (modifier == "log") {
        return(log(x))
    }
}


sub_dataset = function(dataset_x, dataset_y) {
    dataset_x = cbind("idx_aux" = as.numeric(rownames(dataset_x)), dataset_x)
    dataset_y = cbind("idx_aux" = as.numeric(rownames(dataset_y)), dataset_y)

    new_data = dplyr::anti_join(dataset_x, dataset_y, by = "idx_aux")
    new_data = dplyr::arrange(new_data, idx_aux)
    rownames(new_data) = new_data$idx_aux
    return(new_data[, !(colnames(new_data) == "idx_aux")])
}


delete_column = function(dataset, var) {
    dataset[var] = NULL
    return(dataset)
}


get_complete_rows = function(dataset) {
    if (is.null(dataset)) return()
    return(dataset[complete.cases(dataset), ])
}


get_unique_rows = function(dataset) {
    if (is.null(dataset)) return()
    return(unique(dataset))
}
