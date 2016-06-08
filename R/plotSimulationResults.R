#' Function to Calculate Numbers to Label X-axis
#' 
#' @param x vector of data
#' @export

xscale <- function(dig=2, ...) {
	function(x) {
		x <- x[order(x)]
		round(unique(x), digits=dig)
	}
}

# defining the breaks function, 
# s is the scaling factor (cf. multiplicative expand)
x_equal_breaks <- function(n = 3, s = 0.05, round.n=round.n, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    c(round(seq(min(x)+d, max(x)-d, length=n), round.n),0)
  }
}

# defining the breaks function, 
# s is the scaling factor (cf. multiplicative expand)
y_equal_breaks <- function(n = 3, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    c(round(seq(min(x)+d, max(x)-d, length=n), 0),0)
  }
}

#' Plot Simulation Results
#' 
#' @param dataset Simulation data on sampling of the multiple patch realizations.
#' @param variable The variable to be plotted. Default value is \code{NULL}.
#' @param x_variable The variable to be plotted on the x-axis. Default value is \code{NULL}.
#' @param y_variable The variable to be plotted to be plotted on the y-axis. Default value is \code{NULL}.
#' @param grouping_variable The variable dividing the data into subgroups.
#' @param legend_name The name given to the \code{grouping_variable} in the legend.
#' @param xlab_name The x-axis name. Default value is \code{NULL}.
#' @param ylab_name The y-axis name. Default value is \code{NULL}.
#' @param SD=FALSE 
#' @param facet=facet 
#' @param legendposition="bottom" 
#' @param legendcolor="black" 
#' @param x_breaks 
#' @param x_angle=360
#' @param nlength Number of breaks on the x-axis. Default value is \code{6}.
#' @param roundn 
#' @return Plot of the true variable parameter values (x-axis) versus variable bias (y-axis). The plot is divided into three facets: Horvitz-Thompson estimators, SRSWOR estimators (excluding information from adaptively sampled plots), and SRSWOR estimators applied to the entire dataset (ignoring the sampling design). Error bars indicate the standard deviation.
#' @export
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes

plotSimulationResults <- function(
	dataset, 
	variable=NULL, 
	x_variable=NULL, 
	y_variable=NULL, 
	grouping_variable, 
	legend_name, 
	xlab_name=NULL,
	ylab_name=NULL, 
	SD=FALSE, 
	facet=facet, 
	legendposition="bottom", 
	legendcolor="black", 
	#x_breaks, 
	x_angle=360,
	xlength=6,
	ylength=4,
	roundn=2
) {
	# x_variable=variable
	Plots <- Restricted 							<- NULL
	grouping.variable 	<- variable_error_estimate 	<- NULL
	pd 												<- position_dodge(0.1)
	
	if (is.null(y_variable)) {
		y_variable 		<- paste(variable, "_mean", "_bias", sep="")
	}
	if (is.null(x_variable)) {
		if (length(grep("_on_", variable)) > 0) {
			x_variable 	<- paste(variable, "_ratio_mean", sep="")
		} else {
			x_variable 	<- paste(variable, "_mean", sep="")
		}
	}
	x = dataset %>% 
		as.data.table %>%
		setnames(y_variable, "y.var") %>%
		setnames(x_variable, "x.var") %>%
		setnames(grouping_variable, "group.var") %>%
		#setnames(variable, "x.variable") %>%
		as.data.frame %>%
		filter(y.var!=-Inf) %>%
		group_by(Species, group.var, x.var)
	
	if (SD == TRUE) {
			x %<>%
			summarise(
				y.var.estimate 	= mean(y.var, na.rm=T),
				ymax 			= mean(y.var, na.rm=T) + 
									sd(y.var, na.rm=T),
				ymin 			= mean(y.var, na.rm=T) - 									sd(y.var, na.rm=T),
				nsims 			= length(y.var)
			)
			
			bias_y_scale <- x %>%
				arrange(y.var.estimate) %$%
				summary(y.var.estimate) %>%
				round(-1) %>%
				unique
			bias_y_scale <- seq(
				min(bias_y_scale), 
				max(bias_y_scale), 
				length.out=5
			) %>% round(0)
			
			p = ggplot(
				x, 
				aes(
					x 		= x.var, 
					y 		= y.var.estimate, 
					shape 	= factor(group.var), 
					group 	= factor(group.var), 
					ymax 	= max(ymax)*1.05
				)
			) +
			geom_hline(aes(yintercept=0)) +  
			geom_point(
				alpha=0.6, 
				size=8, 
				na.rm=T
			) + 
			geom_errorbar(
				aes(ymax=ymax, ymin=ymin), 
			) +
			geom_line(
				aes(linetype=factor(group.var)), 
				na.rm=T
			) +
			facet_grid(eval(parse(text=facet)), scales="free_x") + 
			guides(
				shape=guide_legend(title=legend_name), 
				linetype=guide_legend(title=legend_name)
			) +
			xlab(xlab_name) +
			ylab(ylab_name) +
			theme(
				legend.position=legendposition,
				legend.key = element_rect(colour=NA),
				panel.border = element_rect(colour = "black", fill=NA, 
					size=3),
				panel.background = element_rect(linetype="solid", 
					fill="white"),
				strip.text = element_text(face="bold", size=24),
				strip.background = element_rect(fill="white", 
					colour="black", size=3)
			)
		} else {
			x %<>%
			summarise(
				y.var.estimate = mean(y.var, na.rm=T),
				nsims 			= length(y.var)
			)
			bias_y_scale <- x %>%
				arrange(y.var.estimate) %$%
				summary(y.var.estimate) %>%
				round(-1) %>%
				unique
			y_breaks <- seq(
				min(bias_y_scale), 
				max(bias_y_scale), 
				length.out=5
			) %>% round(0)
			p = ggplot(
				x, 
				aes(
					x 		= x.var, 
					y 		= y.var.estimate, 
					shape 	= factor(group.var), 
					group 	= factor(group.var)
				)
			) +
			geom_hline(aes(yintercept=0)) +  
			geom_point(
				alpha=0.75, 
				size=8, 
				na.rm=T
			) + 
			geom_line(
				aes(linetype=factor(group.var)), 
				na.rm=T
			) +
			facet_grid(eval(parse(text=facet)), scales="free_x", labeller=label_parsed) + 
			guides(
				shape = guide_legend(title=legend_name), 
				linetype = guide_legend(title=legend_name)
			) +
			xlab(xlab_name) +
			# scale_x_continuous(breaks=x_equal_breaks(n=xlength, round.n=roundn)) +
			scale_y_continuous(breaks=y_equal_breaks(n=ylength)) +
			ylab(ylab_name) +
			theme(
				legend.position=legendposition,
				legend.key = element_rect(colour=NA),
				legend.text = element_text(colour=legendcolor), #
				legend.title = element_text(colour=legendcolor), #
				panel.border = element_rect(colour = "black", fill=NA, 
					size=3),
				panel.background = element_rect(linetype="solid", 
					fill="white"),
				strip.text = element_text(face="bold", size=24),
				strip.background = element_rect(fill="white", 
					colour="black", size=3),
				axis.text.x = element_text(angle=x_angle, hjust=0.9)
			)
		}
	return(p)
}
