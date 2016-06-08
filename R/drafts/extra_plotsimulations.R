
if (variable=="Cactus") {
	xlab_name = "True Cactus Occupancy"
	ylab_name =  "Relative Bias of the Cactus Occupancy Estimate"
} else
if (variable=="Stricta" | variable=="Pusilla") {
	xlab_name = bquote(
		"True"~
		italic(Opuntia)~
		italic(.(tolower(variable)))~
		"Occupancy"
	)
	ylab_name =  bquote(
		"Relative Bias of the Occupancy Estimate of"~
		italic(Opuntia)~
		italic(.(tolower(variable)))
	)
} else 
if (length(grep("_on_", variable)) > 0) {
	if (length(grep("CACA_", variable)) > 0) {
		xlab_name = bquote(
			"True Occupancy of Invasive Moth on"~
			italic(Opuntia)~
			italic(.(sub(".*_on_", "", tolower(variable))))
		)
		ylab_name =  bquote(
			"Relative Bias of the Occupancy Estimate of Invasive Moth on"~
			italic(Opuntia)~
			italic(.(sub(".*_on_", "", tolower(variable))))
		)
	}
	if (length(grep("MEPR_", variable)) > 0) {
		xlab_name = bquote(
			"True Occupancy of Native Moth on"~
			italic(Opuntia)~
			italic(.(sub(".*_on_", "", tolower(variable))))
		)
		ylab_name =  bquote(
			"Relative Bias of the Occupancy Estimate of Native Moth on"~
			italic(Opuntia)~
			italic(.(sub(".*_on_", "", tolower(variable))))
		)
	}
}