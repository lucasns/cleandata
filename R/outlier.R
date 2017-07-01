library(aplpack)
library(asbio)
library(dplyr)


outlier_boxplot = function(dataset, col, rm_na = TRUE) {
    bp = boxplot(dataset[[col]])
    out = data.frame(bp$out)
    names(out) = col
    return(out)
}


outlier_bagplot = function(dataset, col1, col2) {
    bp = aplpack::bagplot(dataset[[col1]], dataset[[col2]])
    out = data.frame(bp$pxy.outlier)
    names(out) = c(col1, col2)
    return(out)
}


outlier_bv_boxplot = function(dataset, col1, col2) {
    bp = asbio::bv.boxplot(dataset[[col1]], dataset[[col2]])
    out = bp$out
    names(out) = c(col1, col2)
    return(out)
}


univar_func = list(
    boxplot = outlier_boxplot
)


bivar_func = list(
    bagplot = outlier_bv_boxplot,
    bvboxplot = outlier_bv_boxplot
)


univariate_outliers = function(dataset, var, type = "boxplot", rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var]])),]
    }

    info = ""
    out = univar_func[[type]](dataset, var)

    return(list(outliers = out, info = info))
}


bivariate_outliers = function(dataset, var1, var2, type = "bvboxplot", rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var1]])) & !(is.na(dataset[[var2]])),]
    }

    info = ""
    out = bivar_func[[type]](dataset, var1, var2)

    return(list(info = info, outliers = out, info = info))
}


remove_outliers = function(dataset, outliers) {
    dplyr::anti_join(dataset, outliers, by = names(outliers))
}
