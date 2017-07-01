library(aplpack)
library(asbio)
library(dplyr)


outlier_boxplot = function(x, rm_na = TRUE) {
    bp = boxplot(x)
    out = data.frame(bp$out)
    return(out)
}


outlier_bagplot = function(x, y) {
    bp = aplpack::bagplot(x, y)
    out = data.frame(bp$pxy.outlier)
    return(out)
}


outlier_bv_boxplot = function(x, y) {
    bp = asbio::bv.boxplot(x, y)
    out = bp$out
    return(out)
}


univar_func = list(
    boxplot = outlier_boxplot
)


bivar_func = list(
    bagplot = outlier_bagplot,
    bvboxplot = outlier_bv_boxplot
)


univariate_outliers = function(dataset, var, type = "boxplot", rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var]])),]
    }

    info = ""

    x = dataset[[var]]
    out = univar_func[[type]](x)
    names(out) = var

    return(list(outliers = out, info = info))
}


bivariate_outliers = function(dataset, var1, var2, type = "bvboxplot", rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var1]])) & !(is.na(dataset[[var2]])),]
    }

    info = ""

    x = dataset[[var1]]
    y = dataset[[var2]]
    out = bivar_func[[type]](x, y)
    names(out) = c(var1, var2)

    return(list(info = info, outliers = out, info = info))
}


remove_outliers = function(dataset, outliers) {
    dplyr::anti_join(dataset, outliers, by = names(outliers))
}
