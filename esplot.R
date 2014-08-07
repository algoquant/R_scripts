

library(grid)

esplot <-
    function(x, y = NULL,
             newpage = TRUE,
             xlab = deparse(substitute(x)),
             ylab = deparse(substitute(y)),
             xlim = extend(range(x)),
             ylim = extend(range(y)),
             main = NULL,
             sub = NULL,
             axes = TRUE,
             border = TRUE,
             ...)
{
    extend <- function(rng) rng + c(- 0.2, 0.1) * diff(rng)
    if (is.null(y))
    {
        y <- x
        x <- seq(along = y)
        if (missing(ylab)) ylab <- xlab
        if (missing(ylab)) xlab <- "Index"
    }
    layout.widths <-
        list(x =
             c(ylab = if (is.null(ylab)) 0 else 2,
               axis = if (axes) 3 else 1,
               panel = 1,
               density = 0.1),
             units =
             c(ylab = if (is.null(ylab)) "null" else "strheight",
               axis = "grobwidth",
               panel = "null",
               density = "snpc"),
             data =
             list(ylab = ylab,
                  axis = textGrob(format(pretty(y)), 0, 0),
                  panel = NULL,
                  density = NULL))
    iw <-
        c(ylab = 1, axis = 2, panel = 3, density = 4)

    layout.heights <-
        list(x =
             c(main = if (is.null(main)) 0 else 2,
               density = 0.1,
               panel = 1,
               axis = if (axes) 3 else 1,
               xlab = if (is.null(xlab)) 0 else 2,
               sub = if (is.null(sub)) 0 else 2),
             units =
             c(main = if (is.null(main)) "null" else "strheight",
               density = "snpc",
               panel = "null",
               axis = "lines",
               xlab = if (is.null(xlab)) "null" else "strheight",
               sub = if (is.null(sub)) "null" else "strheight"),
             data =
             list(main = main,
                  density = NULL,
                  panel = NULL,
                  axis = NULL,
                  xlab = xlab,
                  sub = sub))
    ih <-
        c(main = 1, density = 2, panel = 3,
          axis = 4, xlab = 5, sub = 6)

    my.layout <-
        grid.layout(nrow = length(ih),
                    ncol = length(iw),
                    heights = do.call(unit, layout.heights),
                    widths = do.call(unit, layout.widths))

    if (newpage) grid.newpage()
    pushViewport(viewport(layout = my.layout))

    ## panel
    pushViewport(viewport(layout.pos.row = ih["panel"],
                          layout.pos.col = iw["panel"],
                          xscale = xlim,
                          yscale = ylim))
    if (border) grid.rect()
    grid.points(x = x, y = y,
                default.units = "native")

    ## axes
    if (axes)
    {
        grid.xaxis()
        grid.yaxis()
    }

    ## rug
    grid.segments(x0 = unit(x, "native"),
                  y0 = unit(0, "npc"),
                  x1 = unit(x, "native"),
                  y1 = unit(0.03, "npc"),
                  gp = gpar(col = "grey"))
    grid.segments(y0 = unit(y, "native"),
                  x0 = unit(0, "npc"),
                  y1 = unit(y, "native"),
                  x1 = unit(0.03, "npc"),
                  gp = gpar(col = "grey"))
    upViewport(1)

    ## densities
    xdens <- density(x)
    maxh <- max(xdens$y)
    pushViewport(viewport(layout.pos.row = ih["density"],
                          layout.pos.col = iw["panel"],
                          clip = "on",
                          xscale = xlim,
                          yscale = c(-0.05, 1.05) * maxh))
    grid.polygon(x = xdens$x, y = xdens$y,
                 gp =
                 gpar(col = "transparent",
                      fill = "grey"),
                 default.units = "native")
    upViewport(1)

    ydens <- density(y)
    maxh <- max(ydens$y)
    pushViewport(viewport(layout.pos.row = ih["panel"],
                          layout.pos.col = iw["density"],
                          clip = "on",
                          yscale = ylim,
                          xscale = c(-0.05, 1.05) * maxh))
    grid.polygon(y = ydens$x, x = ydens$y,
                 gp =
                 gpar(col = "transparent",
                      fill = "grey"),
                 default.units = "native")
    upViewport(1)
    
    ## ylab
    if (!is.null(ylab))
    {
        pushViewport(viewport(layout.pos.row = ih["panel"],
                              layout.pos.col = iw["ylab"]))
        grid.text(ylab, rot = 90)
        upViewport(1)
    }

    ## xlab
    if (!is.null(xlab))
    {
        pushViewport(viewport(layout.pos.row = ih["xlab"],
                              layout.pos.col = iw["panel"]))
        grid.text(xlab)
        upViewport(1)
    }
    ## main
    if (!is.null(main))
    {
        pushViewport(viewport(layout.pos.row = ih["main"],
                              layout.pos.col = iw["panel"]))
        grid.text(main)
        upViewport(1)
    }
    ## sub
    if (!is.null(sub))
    {
        pushViewport(viewport(layout.pos.row = ih["sub"],
                              layout.pos.col = iw["panel"]))
        grid.text(sub)
        upViewport(1)
    }
}

with(faithful,
     esplot(jitter(eruptions), jitter(waiting),
            xlab = "Eruption time (minutes)",
            ylab = "Time to next eruption",
            main = "Old faithful eruptions"))


with(faithful,
     esplot(jitter(eruptions), jitter(waiting),
            xlab = "Eruption time (minutes)",
            ylab = "Time to next eruption",
            main = "Old faithful eruptions",
            axes = FALSE, border = FALSE))
