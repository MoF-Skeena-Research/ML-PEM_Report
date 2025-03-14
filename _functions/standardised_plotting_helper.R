# example of themes and theme functiosn use in State of Environment
# note you eill not be able to run this script withough downloading packages and data for greenhous 
# indicator but you can view how the themes work for standar graphics
  
# written by Gen Perkins - September 2021





# Copyright 2016 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


## Loading R libraries for script
library(ggplot2) #plotting
library(envreportutils) #for theme_soe and theme_soe_facet
library(scales) #for label = comma
library(forcats) # fct_rev() for stacking order
library(RColorBrewer) #for colour palette
library(dplyr) #data munging


## Read in plotting data from 02_clean.R if not already in environment
if (!exists("bc_ghg_sum")) load("tmp/clean_data.RData")


# these functions are from the envreportutils package: 

#' Default theme for EnvReportBC facetted graphs and plots

theme_soe <- function(base_size=12, base_family="Verdana") {
  thm <- theme_soe_foundation(base_size = base_size, base_family = base_family)
  thm
}  

#' Default theme for EnvReportBC facetted graphs and plots

theme_soe_facet <- function(base_size = 12, base_family = "Verdana") {
  
  theme_soe_foundation(base_size = base_size, base_family = base_family) + 
    theme(
      panel.spacing = unit(.6,"lines"),
      panel.border = element_rect(colour = "black", fill = NA),
      strip.background = element_rect(colour = "black", fill = "grey85"))
  
}

theme_soe_foundation <- function(base_size, base_family) {
  theme_grey() + theme(
    text = element_text(colour = "black"),
    line = element_line(colour = "black", size = 0.5,
                        linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black",
                        size = 0.5, linetype = 1),
    axis.line = element_line(colour = "black"),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.text = element_text(colour = 'black'),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_blank(),
    plot.title = element_text(vjust = 2),
    legend.title = element_text(face = "plain"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey80",size = 0.5),
    axis.title.y = element_text(vjust = 1, angle = 90),
    axis.title.x = element_text(vjust = 0),
    panel.spacing = unit(0.25, "lines"),
    plot.background = element_blank(),
    legend.key = element_blank(),
    complete = TRUE)
  
}

#' Create png for retina display
#' 
#' `png_retina` is a drop-in replacement for the \code{\link[grDevices]{png}} function 
#' for creating images for the web for retina devices. Internally, it simply 
#' doubles the width, height, and resolution specified. The intention is then
#' that in the webpage that you would specify the \code{width} and \code{height} 
#' attributes in the html at the original resolution.
#'
#' @inheritParams grDevices::png
#'
#' @return A plot device is opened: nothing is returned to the R interpreter
#' @export
#' 
#' @seealso \code{\link[grDevices]{png}}
#'
#' @examples
#' 
#' # You want to display at 500 * 500 in the web:
#' png_retina("myplot.svg", width = 500, height = 500)
#' plot(x = 1:500, y = rnorm(500))
#' dev.off()
#' 
#' # Although the output image will be 1000 * 1000, in the html you would put:
#' # <img src="myplot.png", width="500", height="500" />
#' 
png_retina <- function(filename = "Rplot%03d.png", width = 480, height = 480, 
                       units = "px", pointsize = 12, bg = "white",  res = NA, 
                       ..., type = c("cairo", "cairo-png", "Xlib", "quartz"), 
                       antialias) {
  
  width <- width * 2
  height <- height * 2
  res <- ifelse(is.na(res), 144, res * 2)
  
  grDevices::png(filename = filename, width = width, height = height, units = units, 
                 pointsize = pointsize, bg = bg, res = res, ..., 
                 type = type, antialias = antialias)
}

#' Create svg for the web, specifying size in pixels
#' 
#' This is a thin wrapper around \code{\link[svglite]{svglite}} 
#' for creating images for the web in svg format, but enables you specify the 
#' size in pixels, which is more useful for web work. Internally, it simply 
#' converts the width and height to inches at the resolution specified. 
#'
#' @inheritParams svglite::svglite
#' 
#' @param width The width of the plot in pixels. Default \code{800}.
#' @param height The height of the plot in pixels. Default \code{800}.
#' @param res The resolution at which it is displayed. Default \code{72}. 
#' You should rarely have to change this.
#'
#' @return A plot device is opened: nothing is returned to the R interpreter
#' @export
#' 
#' @seealso \code{\link[svglite]{svglite}}
#'
#' @examples
#' 
#' # You want to display at 500 * 500 in the web:
#' svg_px("myplot.png", width = 500, height = 500)
#' plot(x = 1:500, y = rnorm(500))
#' dev.off()
svg_px <- function(file = "Rplots.svg", width = 800, height = 800, res = 72, bg = "white", 
                   pointsize = 12, standalone = TRUE, system_fonts = list(), 
                   user_fonts = list()) {
  
  if (!(is.numeric(width) && is.numeric(height) && is.numeric(res))) {
    stop("width, height, and res must be numeric")
  }
  
  if (width < 50 || height < 50) {
    warning("This seems like a very small value for width or height. These values represent pixels.")
  }
  
  
  width <- width / res
  height <- height / res
  
  svglite::svglite(file = file, width = width, height = height, bg = bg, 
                   pointsize = pointsize, standalone = standalone, 
                   system_fonts = system_fonts, user_fonts = user_fonts)
}

#' Save a ggplot to a png for retina display
#' 
#' `save_png_retina` is a wrapper function around `png_retina` analagous to 
#' [ggplot2::gsave()]
#'
#' @param x a ggplot2 object
#' @inheritParams png_retina
#' @describeIn png_retina
#'
#' @return NULL
#' @export
#' 
#' @examples 
#' if (suppressPackageStartupMessages(require("ggplot2", quietly = TRUE))) {
#' p <- ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()
#' file <- tempfile(fileext = ".png")
#' save_png_retina(p, file)
#' }
save_png_retina <- function(x, filename = "Rplot%03d.png", width = 480, height = 480, 
                            units = "px", pointsize = 12, bg = "white",  res = NA, 
                            ..., type = c("cairo", "cairo-png", "Xlib", "quartz"), 
                            antialias) {
  on.exit(graphics_exit(grDevices::dev.cur()))
  png_retina(filename = filename, width = width, height = height, units = units,
             pointsize = pointsize, bg = bg, res = res, ..., type = type, 
             antialias = antialias)
  graphics::plot(x)
  invisible(NULL)
}

#' Save a ggplot to an svg file using pixel dimensions
#' 
#' `save_svg_px` is a wrapper function around `svg_px` analagous to 
#' [ggplot2::gsave()]
#'
#' @param x a ggplot2 object
#' @inheritParams svg_px
#' @describeIn svg_px
#'
#' @return NULL
#' @export
#' 
#' @examples 
#' if (suppressPackageStartupMessages(require("ggplot2", quietly = TRUE))) {
#' p <- ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()
#' file <- tempfile(fileext = ".svg")
#' save_svg_px(p, file)
#' }
save_svg_px <- function(x, file = "Rplots.svg", width = 800, height = 800, 
                        res = 72, bg = "white", pointsize = 12, 
                        standalone = TRUE, system_fonts = list(), 
                        user_fonts = list()) {
  on.exit(graphics_exit(grDevices::dev.cur()))
  svg_px(file = file, width = width, height = height, res = res, bg = bg, 
         pointsize = pointsize, standalone = standalone, 
         system_fonts = system_fonts, user_fonts = user_fonts)
  graphics::plot(x)
  invisible(NULL)
}

graphics_exit <- function(old_dev) {
  utils::capture.output({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
  })
}


# the rest of script shows how to use these functions in graphics; 

# manual aqdjustment of plots 


## Line plot theme
theme_lineplots <- theme(
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16,
                              margin = margin(t = 0, r = 10, b = 0, l = 0,
                                              unit = "pt")),
  plot.title = element_text(size = 17, hjust = 0.5),
  plot.margin = unit(c(6,6,6,6),"mm")
)

# Set plotting parameters common to many plots:
x_scale <- scale_x_continuous(limits = c(1990, max_ghg_yr + 1), 
                              breaks = seq(1992, max_ghg_yr + 1, 5), 
                              expand = c(0,0))

## Line plot of total GHG emissions over time in British Columbia
ghg_time <- ggplot(data = bc_ghg_sum, aes(x = year, y = ghg_estimate)) + 
  geom_line(colour = "#e41a1c", size = 1.5) + 
  labs(title = "Total GHG Emissions") +
  xlab(NULL) + 
  ylab(bquote(Mt~CO[2]*e)) +
  scale_y_continuous(limits = c(50, 72), breaks = seq(50, 72, 2),
                     expand = c(0,0), labels = comma) +
  x_scale +
  theme_soe() +
  theme_lineplots
plot(ghg_time)


## Line plot of normalised GHG emissions, GDP and population change over time 
#colour palette for 3 measures
normpal <- c("norm_gdp" = "#e41a1c",
             "norm_ghg" = "#377eb8",
             "norm_population" = "#4daf4a")

norm_base <- ggplot(data = normalized_measures, 
                    aes(x = year, y = estimate, group = measure, 
                        colour = measure)) + 
  geom_line(size = 1.5) +
  scale_y_continuous(limits = c(.9,2.1), breaks = seq(.9, 2, .1),
                     expand = c(0,0)) +
  x_scale +
  labs(title = "Relative GHG Emissions, GDP & Population Size") +
  xlab(NULL) + ylab("Values Indexed Relative to 1990") +
  scale_colour_manual(name="", values = normpal, guide = FALSE) +
  theme_soe() +
  theme_lineplots 


## Stacked area chart of GHG emissions over time by sector
#colour palette for sector plot
sector.order <- rev(levels(ghg_sector_sum$sector))
sector.no <- length(sector.order) + 1
sector.pal <- brewer.pal(sector.no, "Set1")
names(sector.pal) <- sector.order


ghg_stack <- ggplot(data = ghg_sector_sum, 
                    aes(x = year, y = sum, fill = fct_rev(sector))) + 
  geom_area(size = .2, alpha = .6) + 
  geom_line(data = bc_ghg_sum, aes(x = year, y = ghg_estimate),
            colour = "black", size = 1, show.legend = FALSE) +
  xlab(NULL) +  ylab(bquote(Mt~CO[2]*e)) +
  scale_y_continuous(limits = c(0,70), minor_breaks = waiver(),
                     breaks = seq(0, 70, 10), expand = c(0,0), 
                     labels = comma) +
  x_scale +
  scale_fill_manual(name = "Sector", values = sector.pal,
                    breaks = sector.order) +
  theme_soe() +
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16), 
        legend.background = element_rect(colour = "white"))
plot(ghg_stack)


## Facetted line plot of GHG emissions over time by Energy Source
#creating a list for Energy subsector order 
subsector.order <- c("Transport","Stationary Combustion Sources", 
                     "Fugitive Sources")

#creating colour palette for Energy subsector graphs
subsector.no <- length(subsector.order)
subsector.pal <- brewer.pal(subsector.no, "Set1")
names(subsector.pal) <- subsector.order

#generate a background dataset
ghg_energy_group_bg <- ghg_energy_group %>% 
  ungroup() %>% 
  select(-subsector_level1) %>% 
  rename(general_source_line = general_source)

# generate the plot order based on highest values in most recent year
plot.order <- ghg_energy_group %>%
  filter(year == max_ghg_yr) %>%
  arrange(desc(sum)) %>%
  pull(general_source)


ghg_energy_group$general_source_f = factor(ghg_energy_group$general_source, 
                                           levels = plot.order)

#facet plot
ghg_energy_trends <- ggplot(data = ghg_energy_group,
                            aes(x = year, y = sum, colour = subsector_level1)) + 
  geom_line(data = ghg_energy_group_bg, aes(group = general_source_line),
            size = .8, colour = "grey", alpha = 0.5) +
  geom_line(size = 1) +
  facet_wrap( ~ general_source_f, ncol = 4, 
              labeller = label_wrap_gen(width = 25, multi_line = TRUE)) + 
  xlab(NULL) + ylab(bquote(Mt~CO[2]*e)) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0, 20, 4), 
                     labels = comma) +
  scale_x_continuous(limits = c(1990, max_ghg_yr + 1), breaks = seq(1992, max_ghg_yr, 5), 
                     expand = c(0,0)) +
  scale_colour_manual(name = "Energy Subsectors:", values = subsector.pal,
                      breaks = subsector.order) +
  theme_soe_facet() +
  theme(legend.position = ("bottom"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                               unit = "pt")),
        plot.margin = unit(c(5,5,0,2),"mm"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank())
plot(ghg_energy_trends)


## Create tmp folder if not already there and store plot objects in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(ghg_time, ghg_pop, gdp_time, norm, norm_print,
     ghg_stack, ghg_energy_trends, file = "tmp/plots.RData")


## Create a folder in directory called out for image files
if (!exists("out"))  dir.create('out', showWarnings = FALSE)


## Printing plots for web in SVG formats (and PNG)
#total ghg over time
svg_px("./out/ghg_plot.svg", width = 500, height = 400)
plot(ghg_time)
dev.off()

png_retina(filename = "./out/ghg_plot.png", width = 500, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(ghg_time)
dev.off()

