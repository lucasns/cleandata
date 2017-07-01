plot_univar = function(dataset, var, type="histogram", modifier = NULL, rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na == TRUE) {
        dataset = dataset[!(is.na(dataset[[var]])),]
    }

    x = dataset[[var]]

    if (!is.null(modifier)) {
        x = apply_modifier(x, modifier)
    }

    plot_func = switch(type,
                      plot = plot,
                      histogram = hist,
                      boxplot = boxplot
    )

    plot_func(x)
}


plot_bivar = function(dataset, var1, var2, type="bvboxplot", modifier = NULL, rm_na = TRUE) {
    if (is.null(dataset)) return()

    if (rm_na) {
        dataset = dataset[!(is.na(dataset[[var1]]) | is.na(dataset[[var2]])),]
    }

    x = dataset[[var1]]
    y = dataset[[var2]]

    if (!is.null(modifier)) {
        x = apply_modifier(x, modifier)
        y = apply_modifier(y, modifier)

        aux_df = data.frame(x,y)
        aux_df = aux_df[is.finite(aux_df$x) & is.finite(aux_df$y), ]

        x = aux_df[['x']]
        y = aux_df[['y']]
    }

    plot_func = switch(type,
                      bvboxplot = function(x, y) {
                          bv.boxplot(x, y, bg = 'blue', bg.out = 'red')
                      },
                      plot = plot
    )

    plot_func(x, y)
}
