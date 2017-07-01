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

    x = dataset[[var]]
    out = univar_func[[type]](x)
    names(out) = var

    info = paste(nrow(out), "outliers detected.")

    return(list(outliers = out, info = info))
}


bivariate_outliers = function(dataset, var1, var2, type = "bvboxplot", rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var1]])) & !(is.na(dataset[[var2]])),]
    }

    x = dataset[[var1]]
    y = dataset[[var2]]
    out = bivar_func[[type]](x, y)
    names(out) = c(var1, var2)

    info = paste(nrow(out), "outliers detected.")

    return(list(outliers = out, info = info))
}


remove_outliers = function(dataset, var, type) {
    if (length(var) == 1) { # Univar
        outliers = univariate_outliers(dataset, var, type)$outliers
    } else if (length(var) == 2) { # Bivar
        outliers = bivariate_outliers(dataset, var[1], var[2], type)$outliers
    }

    clean_data = dplyr::anti_join(dataset, outliers, by = names(outliers))
    clean_data = dplyr::arrange(clean_data, idx_)
    rownames(clean_data) <- clean_data$idx_

    info = paste(nrow(outliers), "rows removed.")

    return(list(data = clean_data, info = info))
}


