function (x, name = deparse(substitute(x)), type = "candlesticks", 
    subset = "", TA = "", pars = chart_pars(), theme = chart_theme(), 
    clev = 0, ...) 
{
    cs <- new.replot()
    line.col <- theme$col$line.col
    up.col <- theme$col$up.col
    dn.col <- theme$col$dn.col
    up.border <- theme$col$up.border
    dn.border <- theme$col$dn.border
    format.labels <- theme$format.labels
    if (is.null(theme$grid.ticks.on)) {
        xs <- x[subset]
        major.grid <- c(years = nyears(xs), months = nmonths(xs), 
            days = ndays(xs))
        grid.ticks.on <- names(major.grid)[rev(which(major.grid < 
            30))[1]]
    }
    else grid.ticks.on <- theme$grid.ticks.on
    label.bg <- theme$col$label.bg
    cs$subset <- function(x) {
        if (FALSE) {
            set_ylim <- get_ylim <- set_xlim <- Env <- function() {
            }
        }
        if (missing(x)) {
            x <- ""
        }
        Env$xsubset <<- x
        set_xlim(c(1, NROW(Env$xdata[Env$xsubset])))
        ylim <- get_ylim()
        for (y in seq(2, length(ylim), by = 2)) {
            if (!attr(ylim[[y]], "fixed")) 
                ylim[[y]] <- structure(c(Inf, -Inf), fixed = FALSE)
        }
        lapply(Env$actions, function(x) {
            frame <- abs(attr(x, "frame"))
            fixed <- attr(ylim[[frame]], "fixed")
            if (frame%%2 == 0 && !fixed) {
                lenv <- attr(x, "env")
                if (is.list(lenv)) 
                  lenv <- lenv[[1]]
                min.tmp <- min(ylim[[frame]][1], range(na.omit(lenv$xdata[Env$xsubset]))[1], 
                  na.rm = TRUE)
                max.tmp <- max(ylim[[frame]][2], range(na.omit(lenv$xdata[Env$xsubset]))[2], 
                  na.rm = TRUE)
                ylim[[frame]] <<- structure(c(min.tmp, max.tmp), 
                  fixed = fixed)
            }
        })
        set_ylim(ylim)
    }
    environment(cs$subset) <- environment(cs$get_asp)
    if (is.character(x)) 
        stop("'x' must be a time-series object")
    if (is.OHLC(x)) {
        cs$Env$xdata <- OHLC(x)
        if (has.Vo(x)) 
            cs$Env$vo <- Vo(x)
    }
    else cs$Env$xdata <- x
    cs$Env$xsubset <- subset
    cs$Env$cex <- pars$cex
    cs$Env$mar <- pars$mar
    cs$set_asp(3)
    cs$set_xlim(c(1, NROW(cs$Env$xdata[subset])))
    cs$set_ylim(list(structure(range(na.omit(cs$Env$xdata[subset])), 
        fixed = FALSE)))
    cs$set_frame(1, FALSE)
    cs$Env$clev = min(clev + 0.01, 1)
    cs$Env$theme$bbands <- theme$bbands
    cs$Env$theme$shading <- theme$shading
    cs$Env$theme$line.col <- theme$col$line.col
    cs$Env$theme$up.col <- up.col
    cs$Env$theme$dn.col <- dn.col
    cs$Env$theme$up.border <- up.border
    cs$Env$theme$dn.border <- dn.border
    cs$Env$theme$rylab <- theme$rylab
    cs$Env$theme$lylab <- theme$lylab
    cs$Env$theme$bg <- theme$col$bg
    cs$Env$theme$grid <- theme$col$grid
    cs$Env$theme$grid2 <- theme$col$grid2
    cs$Env$theme$labels <- "#333333"
    cs$Env$theme$label.bg <- label.bg
    cs$Env$format.labels <- format.labels
    cs$Env$ticks.on <- grid.ticks.on
    cs$Env$grid.ticks.lwd <- theme$grid.ticks.lwd
    cs$Env$type <- type
    cs$Env$axis_ticks <- function(xdata, xsubset) {
        ticks <- diff(axTicksByTime2(xdata[xsubset], labels = FALSE))/2 + 
            last(axTicksByTime2(xdata[xsubset], labels = TRUE), 
                -1)
        if (!theme$coarse.time || length(ticks) == 1) 
            return(unname(ticks))
        if (min(diff(ticks)) < max(strwidth(names(ticks)))) {
            ticks <- unname(ticks)
        }
        ticks
    }
    cs$add(expression(atbt <- axTicksByTime2(xdata[xsubset]), 
        segments(atbt, get_ylim()[[2]][1], atbt, get_ylim()[[2]][2], 
            col = theme$grid, lwd = grid.ticks.lwd), axt <- axis_ticks(xdata, 
            xsubset), text(as.numeric(axt), par("usr")[3] - 0.2 * 
            min(strheight(axt)), names(axt), xpd = TRUE, cex = 0.9, 
            pos = 3)), clip = FALSE, expr = TRUE)
    cs$set_frame(-1)
    cs$add_frame(0, ylim = c(0, 1), asp = 0.2)
    cs$set_frame(1)
    cs$add(expression(if (NROW(xdata[xsubset]) < 400) {
        axis(1, at = 1:NROW(xdata[xsubset]), labels = FALSE, 
            col = theme$grid2, tcl = 0.3)
    }), expr = TRUE)
    cs$add(expression(axt <- axTicksByTime(xdata[xsubset], format.labels = format.labels), 
        axis(1, at = axt, labels = names(axt), las = 1, lwd.ticks = 1, 
            mgp = c(3, 1.5, 0), tcl = -0.4, cex.axis = 0.9)), 
        expr = TRUE)
    cs$Env$name <- name
    text.exp <- c(expression(text(1 - 1/3, 0.5, name, font = 2, 
        col = "#444444", offset = 0, cex = 1.1, pos = 4)), expression(text(NROW(xdata[xsubset]), 
        0.5, paste(start(xdata[xsubset]), end(xdata[xsubset]), 
            sep = " / "), col = 1, adj = c(0, 0), pos = 2)))
    cs$add(text.exp, env = cs$Env, expr = TRUE)
    cs$set_frame(2)
    cs$Env$axis_labels <- function(xdata, xsubset, scale = 5) {
        axTicksByValue(na.omit(xdata[xsubset]))
    }
    cs$Env$make_pretty_labels <- function(ylim) {
        p <- pretty(ylim, 10)
        p[p > ylim[1] & p < ylim[2]]
    }
    cs$add(expression(assign("alabels", make_pretty_labels(get_ylim(get_frame())[[2]]))), 
        expr = TRUE)
    cs$set_frame(-2)
    cs$add(expression(if (diff(range(xdata[xsubset], na.rm = TRUE)) < 
        50) segments(1, seq(min(xdata[xsubset]%/%1, na.rm = TRUE), 
        max(xdata[xsubset]%/%1, na.rm = TRUE), 1), length(xsubset), 
        seq(min(xdata[xsubset]%/%1, na.rm = TRUE), max(xdata[xsubset]%/%1, 
            na.rm = TRUE), 1), col = theme$grid2, lty = "dotted")), 
        expr = TRUE)
    cs$set_frame(2)
    cs$add(expression(segments(1, alabels, NROW(xdata[xsubset]), 
        alabels, col = theme$grid)), expr = TRUE)
    if (theme$lylab) {
        cs$add(expression(text(1 - 1/3 - max(strwidth(alabels)), 
            alabels, noquote(format(alabels, justify = "right")), 
            col = theme$labels, offset = 0, cex = 0.9, pos = 4, 
            xpd = TRUE)), expr = TRUE)
    }
    if (theme$rylab) {
        cs$add(expression(text(NROW(xdata[xsubset]) + 1/3, alabels, 
            noquote(format(alabels, justify = "right")), col = theme$labels, 
            offset = 0, cex = 0.9, pos = 4, xpd = TRUE)), expr = TRUE)
    }
    cs$set_frame(2)
    cs$add(expression(range.bars(xdata[xsubset], type, 1, fade(theme$line.col, 
        clev), fade(theme$up.col, clev), fade(theme$dn.col, clev), 
        fade(theme$up.border, clev), fade(theme$dn.border, clev))), 
        expr = TRUE)
    assign(".chob", cs, .plotEnv)
    if (!is.null(TA) && nchar(TA) > 0) {
        TA <- parse(text = TA, srcfile = NULL)
        for (ta in 1:length(TA)) {
            if (length(TA[ta][[1]][-1]) > 0) {
                cs <- eval(TA[ta])
            }
            else {
                cs <- eval(TA[ta])
            }
        }
    }
    assign(".chob", cs, .plotEnv)
    cs
}
<environment: namespace:quantmod>
