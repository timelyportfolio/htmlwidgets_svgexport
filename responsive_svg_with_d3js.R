library(SVGAnnotation)
library(htmltools)

respXML <- function( svg_xml, height = NULL, width = "100%", print = T, ... ){
  # svg_xml should be an XML document
  library(htmltools)
  library(XML)
  
  svg <- structure(
    ifelse(
      length(getDefaultNamespace(svg_xml)) > 0
      ,getNodeSet(svg_xml,"//x:svg", "x")
      ,getNodeSet(svg_xml,"//svg")
    )
    ,class="XMLNodeSet"
  )
     
  xmlApply(
    svg
    ,function(s){
     a = xmlAttrs(s)
     removeAttributes(s)
     xmlAttrs(s) <- a[-(1:2)]
     xmlAttrs(s) <- c(
       style = paste0(
         "height:100%;width:100%;"
       )
     )
    }
  )
 
  svg <- HTML( saveXML( svg_xml) )
    
  svg <- tags$div(
      style = paste(
        sprintf('width:%s;',width)
        ,ifelse(!is.null(height),sprintf('height:%s;',height),"")
      )
      ,svg
    )
  
  if(print) html_print(svg) 
  
  return( invisible( svg ) )
}

# make our plot here
# since we will need to manipulate to add a g container
# for smoother d3 pan/zoom
sP = respXML( 
  svgPlot(
    dotchart(
      t(VADeaths)
      , xlim = c(0,100)
      , main = "Death Rates in Virginia - 1940"
    )
  )
  , height = "100%"
  , print = F
)

# parse the plot html with rvest
sP = html(as.character(sP))
# add a g node to contain the plot
#  for smoother d3 pan / zoom
g = newXMLNode("g")
# add the old g to our new g container
addChildren(g, html_nodes(sP,"svg > g"))
# add our new g container to our svg
addChildren(html_nodes(sP,"svg")[[1]],g)

html_print(attachDependencies(
  tagList(
    # get the div with our modified svg
    HTML(saveXML(html_nodes(sP,"div")[[1]]))
    , tags$script(
      HTML(
        '
        var g = d3.select("svg > g");
        var zoom = d3.behavior.zoom().scaleExtent([1, 8]).on("zoom", zoomed);
        g.call(zoom);
        
        function zoomed() {
          g.select("g")
          .attr(
          "transform",
          "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")"
          );
        }
        '
      )
      )
      )
  ,htmlDependency(
    name="d3"
    ,version="3.0"
    ,src=c("href"="http://d3js.org/")
    ,script="d3.v3.js"
  )
))