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
#' @param SD Defaults to FALSE 
#' @param facet
#' @param legendposition Defaults to "bottom" 
#' @param legendcolor Defaults to "black" 
#' @param x_breaks 
#' @param x_angle Defaults to 360
#' @param roundn 
#' @param xlength
#' @param ylength 
#' @param y_breaks 
#' @return Plot of the true variable parameter values (x-axis) versus variable bias (y-axis). The plot is divided into three facets: Horvitz-Thompson estimators, SRSWOR estimators (excluding information from adaptively sampled plots), and SRSWOR estimators applied to the entire dataset (ignoring the sampling design). Error bars indicate the standard deviation.
#' @export
#' @importFrom ggplot2 position_dodge geom_hline geom_point geom_line facet_grid guides guide_legend xlab ylab theme aes scale_y_continuous element_rect element_text 

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
	roundn=2,
	y_breaks=NULL
) {
	# x_variable=variable
	grouping.variable 	<- variable_error_estimate 	<- NULL
	pd 												<- position_dodge(0.1)
	
	if (is.null(y_variable)) {
		y_variable 		<- paste(variable, "_mean_RB", sep="")
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
		setnames(y_variable, "y_variable") %>%
		setnames(x_variable, "x_variable") %>%
		setnames(grouping_variable, "grouping_variable") %>%
		#setnames(variable, "x.variable") %>%
		as.data.frame %>%
		filter(y_variable!=-Inf)
	if (is.null(y_breaks)==FALSE) {
		p = ggplot(
			x, 
			aes(
				x 		= factor(x_variable), 
				y 		= y_variable, 
				shape 	= factor(grouping_variable), 
				group 	= factor(grouping_variable)
			)
		) +
		geom_hline(aes(yintercept=0)) +  
		geom_point(
			aes(shape 	= factor(grouping_variable), 
				group 	= factor(grouping_variable)
				),
				alpha=0.6, 
			size=4, 
			na.rm=T
		) +
		geom_line(
			aes(linetype=factor(grouping_variable)), 
			na.rm=T
		) +
		facet_grid(eval(parse(text=facet)), scales="free_x", labeller=label_parsed) + 
		guides(
			shape=guide_legend(title=legend_name), 
			linetype=guide_legend(title=legend_name)
		) +
		scale_y_continuous(breaks=y_breaks) +
		xlab(xlab_name) +
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
			strip.text = element_text(face="bold", size=20),
			strip.background = element_rect(fill="white", 
				colour="black", size=3),
			axis.text.x = element_text(angle=x_angle, hjust=0.9)
		)
				
	} else {
		p = ggplot(
			x, 
			aes(
				x 		= factor(x_variable), 
				y 		= y_variable, 
				shape 	= factor(grouping_variable), 
				group 	= factor(grouping_variable)
			)
		) +
		geom_hline(aes(yintercept=0)) +  
		geom_point(
			aes(shape 	= factor(grouping_variable), 
				group 	= factor(grouping_variable)
				),
				alpha=0.6, 
			size=4, 
			na.rm=T
		) +
		geom_line(
			aes(linetype=factor(grouping_variable)), 
			na.rm=T
		) +
		facet_grid(eval(parse(text=facet)), scales="free_x", labeller=label_parsed) + 
		guides(
			shape=guide_legend(title=legend_name), 
			linetype=guide_legend(title=legend_name)
		) +
		#scale_y_continuous(breaks=y_equal_breaks(n=ylength)) +
		xlab(xlab_name) +
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
			strip.text = element_text(face="bold", size=20),
			strip.background = element_rect(fill="white", 
				colour="black", size=3),
			axis.text.x = element_text(angle=x_angle, hjust=0.9),
			text=element_text(family="serif")
		)
				
	}
	return(p)
}
