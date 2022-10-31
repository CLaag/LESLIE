#'@title Plot the enhanced colour profile
#'
#'@description CIELAB colour space
#'
#'@description Process colour data to display an enhanced colour profile
#'
#'@param data [data.frame] [matrix] (**required**): input data frame with four columns, and each
#'row defining depth (1 column), and Lab (3 columns) colours. More columns are silently removed.
#'
#'@param depth_top [numeric] (*optional*): top depth of each layer
#'
#'@param depth_bottom [numeric] (*optional*): bottom depth of each layer
#'
#'@param cycles [numeric] (*with default*): parameter for the number of colour enhancement cycles, it also determines the number of profiles for the plot output plotted.
#'The highest number always determines the number of colour enhancement cycles,
#'while the vector sequence sets the number of profiles shown in the plot.
#'If the original sequence (no enhancement) is not wanted in the plot, this should be indicated by `-1`.
#'For instance: '`cycles = c(-1,3)` shows only the 3rd enhancement cycle in the plot.
#'
#'@param orientation [character] (*with default*): sets the orientation of the plot (`portrait`, `landscape`)
#'
#'@param plot [logical] (*with default*): enables/disables plot output. If disables, the function
#'returns a [list] with [matrix] objects with the colour enhanced profiles
#'
#'@param ... additional parameters to control the plot output, supported are `ylim`,`ylab`, `main`, `border_lwd`
#'
#'@return Returns a plot or if `plot = FALSE` a [list] with [matrix] objects
#'
#'@examples
#'data(LESLIE_profile, envir = environment())
#'plot_colProfile(LESLIE_profile, cycles = 3)
#'
#'@md
#'@export
plot_colProfile <- function(
    data,
    depth_top = NULL,
    depth_bottom = NULL,
    cycles = 1:5,
    orientation = "portrait",
    plot = TRUE,
    ...
){

# Input  ------------------------------------------------------------
  ## data
  if (!inherits(data, "data.frame") && !inherits(data, "matrix") || ncol(data) < 4 || nrow(data) < 1)
    stop("[plot_colProfile()] data must be a data.frame or matrix with at least 4 columns and 1 row!",
         call. = FALSE)

  ## clip and convert to matrix[,1:4]
  data <- as.matrix(data)

# Process -----------------------------------------------------------------
  ## extract depths
  if (is.null(depth_top))
    depth_top <- data[, 1]

  if (is.null(depth_bottom))
    depth_bottom <- c(depth_top[-1], depth_top[length(depth_top)] + mean(diff(depth_top)))

  ## convert colours from Lab to sRGB
  m <- .convertColor(data[, 2:4])

  ## we two types of cycles
  ## 1. run_cycles are the number used for the iteration and colour enhancement
  ## 2. plot_cycles is the vector
  run_cycles <- 2:(floor(max(1, abs(cycles))) + 1)
  plot_cycles <- na.exclude(run_cycles[sort(cycles[cycles > 0])])
  if (cycles[1] != -1)   plot_cycles <- c(1, plot_cycles)

# Enhance colours ---------------------------------------------------------
  l <- list()
  l[[1]] <- m
  for (i in run_cycles)
    l[[i]] <- .colour_enhancer(l[[i - 1]], TRUE)

  if (!plot) {
    ## add depth values to each cycle
    l <- lapply(l, function(x) {
      colnames(x) <- c("R", "G", "B")
      cbind(depth_top, depth_bottom, x)

    })

    ## adjust list names
    names(l) <- c("Original", paste0("Cycle #", 1:(cycles[1])))
    return(l)
  }

# Plotting ----------------------------------------------------------------
  ## allow a few ... plot arguments
  plot_settings <- modifyList(
    x = list(
      ylim = if (orientation != "landscape") {
        c(depth_bottom[length(depth_bottom)], depth_top[1])

        } else {
         c(depth_top[1], depth_bottom[length(depth_bottom)])

        },
      ylab = "Depth [m]",
      main = NULL,
      border_lwd = 2
    ), val = list(...), keep.null = TRUE)


  ## Portrait -------
  ## span plot area
  if (orientation[1] != "landscape") {
    ## set area settings
    old.par <- graphics::par(
      oma = c(0,0,0,0),
      mar = if (is.null(plot_settings$main)) c(0.1,4,0.1,0.1) else c(0.1,4,1.1,0.1),
      omi = c(0.05,0.05,0.05,0.05))
    on.exit(graphics::par(old.par))

    ## open plot area
    plot(
      x = NULL,
      y = NULL ,
      ylim = plot_settings$ylim,
      xlim = c(0.1, length(plot_cycles)),
      xlab = "",
      ylab = plot_settings$ylab,
      xaxt = "n",
      bty = "n",
      las = 2)

    if (!is.null(plot_settings$main))
      graphics::title(plot_settings$main)

    ## add a few more ticks
    graphics::axis(
      side = 2,
      at = unique(floor(depth_top)),
      labels = FALSE,
      tck = -.018,
      lwd.ticks = 0.5,
      lwd = 0)

    ## add profiles
    for (c in 1:length(plot_cycles)) {
      graphics::text(
        x = c - 1 + 0.1,
        y = max(graphics::par()$usr[1], plot_settings$ylim[2]),
        labels = if (plot_cycles[c] == 1) "Original" else paste0("Cycle: #", plot_cycles[c] - 1),
        adj = c(0,-0.7),
        cex = 0.6)

      ## plot super rectangle
      graphics::rect(
        xright = c,
        xleft = c - 1 + 0.1,
        ybottom = depth_bottom[length(depth_bottom)],
        ytop = depth_top[1],
        lwd = plot_settings$border_lwd
      )

      ## internal loop
      for (i in 1:length(depth_top)) {
        ## fill super rectangle
        graphics::rect(
          xleft = c - 1 + 0.1,
          ybottom = depth_bottom[i],
          xright = c,
          ytop = depth_top[i],
          density = NULL,
          col = grDevices::rgb(
            l[[plot_cycles[c]]][i, 1],
            l[[plot_cycles[c]]][i, 2],
            l[[plot_cycles[c]]][i, 3],
            alpha = 1),
          border = NA,
          lwd = 0)
      }
    }
  } else {
    ## Landscape -------

    ## set area settings
    old.par <- graphics::par(
      oma = c(0,0,0,0),
      mar = if (is.null(plot_settings$main)) c(4,0.1,0.1,0.1) else c(4,0.1,1.1,0.1),
      omi = c(0.05,0.05,0.05,0.05))
    on.exit(graphics::par(old.par))

    ## open plot area
    plot(
      x = NULL,
      y = NULL ,
      xlim = plot_settings$ylim,
      ylim = c(0.1, length(plot_cycles)),
      ylab = "",
      xlab = plot_settings$ylab,
      yaxt = "n",
      bty = "n",
      las = 1)

    if (!is.null(plot_settings$main))
      graphics::title(plot_settings$main)

    ## add a few more ticks
    graphics::axis(
      side = 1,
      at = unique(floor(depth_top)),
      labels = FALSE,
      tck = -.018,
      lwd.ticks = 0.5,
      lwd = 0)

    ## add profiles
    for (c in 1:length(plot_cycles)) {
      graphics::text(
        y = c - 1 + 0.1,
        x = min(graphics::par()$usr[1], plot_settings$xlim[2]),
        labels = if (plot_cycles[c] == 1) "Original" else paste0("Cycle: #", plot_cycles[c] - 1),
        adj = c(0,2),
        srt = 90,
        cex = 0.6)

      ## plot super rectangle
      graphics::rect(
        xright = depth_bottom[length(depth_bottom)],
        xleft = depth_top[1],
        ybottom = c,
        ytop = c - 1 + 0.1,
        lwd = plot_settings$border_lwd
      )

      ## internal loop
      for (i in 1:length(depth_top)) {
        ## fill super rectangle
        graphics::rect(
          xleft = depth_top[i],
          ybottom = c - 1 + 0.1,
          xright = depth_bottom[i],
          ytop = c,

          density = NULL,
          col = grDevices::rgb(
            l[[plot_cycles[c]]][i, 1],
            l[[plot_cycles[c]]][i, 2],
            l[[plot_cycles[c]]][i, 3],
            alpha = 1),
          border = NA,
          lwd = 0)
      }
     }
    }##end else

}
