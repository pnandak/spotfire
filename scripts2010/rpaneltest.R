require(rpanel)

calc=function(panel) {
  if (panel$low<=panel$pct5 &&
      panel$pct5<=panel$pct95 &&
      panel$pct95<=panel$high &&
      panel$low<=panel$mode && panel$mode<=panel$high) {
    cat("Elicitation is consistant.\n")
    results<<-c(panel$low,panel$pct5,panel$mode,panel$pct95,panel$high)
  } else {
    cat("Elicitation is inconsistant.\n")
    results<<-NULL
  }
  panel
}

nothing=function(panel) {
  return(panel)
}


# Loop until elicitation is consistent
results=NULL
tried=FALSE
while (!tried || is.null(results)) {
  mypanel=rp.control(title="Elicitation", size=c(250,200))
  rp.slider(mypanel, low, 0, 4, action=nothing, showvalue=T, resolution=0.1, 
            title="Minimum", initval=2)
  rp.slider(mypanel, high, 0, 4, action=nothing, showvalue=T, resolution=0.1,
            title="Maximum", initval=2)
  rp.slider(mypanel, mode, 0, 4, action=nothing, title="Mode", initval=2)
  rp.slider(mypanel, pct5, 0, 4, action=nothing, title="5th percentile", initval=2)
  rp.slider(mypanel, pct95, 0, 4, action=nothing, title="95th percentile", initval=2)
  rp.image(mypanel, "spacer.gif", nothing)
  rp.button(panel=mypanel, action=calc, title="OK", quitbutton=TRUE)
  rp.block(mypanel)
  tried=TRUE
  flush.console()
}
print(results) # or do any calculations on the results

