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


delete_column = function(dataset, var) {
    dataset[var] = NULL
    return(dataset)
}


get_info = function(dataset) {
    types = colnames(dataset)
    info = list()
    for (t in colnames(dataset)) {
        col = dataset[[t]]
        if (is.numeric(col)) {
            info[[t]] = list(type = "numeric", values = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))

        } else if(is.factor(col)) {
            info[[t]] = list(type = "factor", values = levels(col))
        }
    }

    return(info)
}


get_summary = function(dataset) {
    s = ""
    retunr(s)
}


apply_modifier = function(x, modifier) {
    if (modifier == "log") {
        return(log(x))
    }
}
