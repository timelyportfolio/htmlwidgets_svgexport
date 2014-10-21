

library(XML)
library(pipeR)
library(rlist)
library(htmltools)
library(SVGAnnotation)
library(ggplot2)

data.frame(x=1:10,y=1:10) %>>%
  ggplot(aes(x=x,y=y)) %>>%
  + geom_point() %>>%
  {gridSVG::grid.export()} %>>%
  {xmlRoot(.[[1]])} %>>%
  saveXML %>>%
  HTML %>>%
  attachDependencies(
    htmlDependency(
      name="d3"
      ,version="3.0"
      ,src=c("href"="http://d3js.org/")
      ,script="d3.v3.js"
    )
  ) %>>%
  html_print  


#even better make it responsive
#use this post as a guide
#http://demosthenes.info/blog/744/Make-SVG-Responsive
respXML <- function( svg_xml, height = NULL, width = "100%", print = T, ... ){
  # svg_xml should be an XML document
  library(htmltools)
  library(pipeR)
  library(XML)
  
  tags$div(
    style = paste(
      sprintf('width:%s;',width)
      ,ifelse(!is.null(height),sprintf('height:%s;',height),"")
      ,"display: inline-block;"
      ,"position: relative;"
      ,"padding-bottom: 100%;"
      ,"vertical-align: middle;"
      ,"overflow: hidden;"
    )
    , ...
    ,svg_xml %>>%
      (~svg ~
         structure(ifelse(
           length(getDefaultNamespace(svg)) > 0
           ,getNodeSet(svg,"//x:svg", "x")
           ,getNodeSet(svg,"//svg")
         ),class="XMLNodeSet") %>>%
         xmlApply(
          function(s){
             a = xmlAttrs(s)
             removeAttributes(s)
             xmlAttrs(s) <- a[-(1:2)]
             xmlAttrs(s) <- c(
               style = paste0(
                 #"height:100%;width:100%;"
                 "display: inline-block;"
                 #post says use these but will not fit viewer
                 #,"position: absolute;"
                 #,"top: 0;"
                 #,"left: 0;"
               )
               #,preserveAspectRatio="xMidYMid meet"
             )
           }
         )
      ) %>>%
      saveXML %>>%
      HTML
  ) %>>%
    ( ~ if(print) html_print(.) ) %>>% 
    ( return( invisible( . ) )  )
}


# example using dotchart documentation
# in R graphics package
# ?graphics::dotchart
svgPlot(
  dotchart(
    t(VADeaths)
    , xlim = c(0,100)
    , main = "Death Rates in Virginia - 1940"
  )
) %>>%
  respXML

# now let's use the Samurai.svg
# from the post that showed us how to do this
library(rvest)

"http://demosthenes.info/blog/744/Make-SVG-Responsive" %>>%
  html %>>%
  html_nodes("div.maincontent.language-markup > div > figure > svg") %>>%
  ( xmlDoc( .[[1]] ) ) %>>%
  respXML(print=F, height="90%") %>>%
  (
    tagList(
      tags$h1(
        style = "font-family:sans-serif;color:gray;"
        ,"Example From "
        ,tags$a(
          href="http://demosthenes.info/blog/744/Make-SVG-Responsive"
          ,"demosthenes.info"
        )
      )
      , .
    )
  ) %>>% html_print()




doc %>>%
  #xmlRoot %>>%
  saveXML %>>%
  HTML %>>%
  attachDependencies(
    htmlDependency(
      name="d3"
      ,version="3.0"
      ,src=c("href"="http://d3js.org/")
      ,script="d3.v3.js"
    )
  ) %>>%
  html_print



exportlist[[1]] %>>%
  xmlRoot %>>%
  saveXML %>>%
  HTML %>>%
  attachDependencies(
    htmlDependency(
      name="d3"
      ,version="3.0"
      ,src=c("href"="http://d3js.org/")
      ,script="d3.v3.js"
    )
  ) %>>%
  tagList(
    tags$script(
      sprintf(
        "var data = %s;
        var latticeInfo = %s;
        %s"
        ,data
        ,list.filter(as.list(unclass(p1)),f(x)->!(class(x) %in% c("function","formula","call"))) %>>% rjson::toJSON()
        ,"
        var svg = d3.select('svg');
        var line = d3.svg.line()
        .x(function (d) {
          return +d.x;
        })
        .y(function (d) { return +d.y; })
        .interpolate('basis');
        
        var bisectX = d3.bisector(function (d) { return +d.x; }).left;
        
        function pointsToArray(points) {
          var pointsArray = new Array();
          
          pointsArray = points.match(/[^ ]+/g);
          
          pointsArray = pointsArray.map(
            function (d, i) {
              return {
                x: d.split(',')[0],
                y: d.split(',')[1],
              }
            }
          );
          
          return pointsArray;
        }
        
        var g = svg.selectAll('.lines');
        
        var pointsdata = [];
        var pointsline = [];
        d3.entries(data.data).forEach(
          function (d,i) {
            pointsline = pointsToArray(d3.select(g[0][i]).select('polyline').attr('points'));
            d.value.groups.forEach(
              function (dd, ii) {
                pointsdata.push(
{
  x:pointsline[ii].x, y:pointsline[ii].y,
  data: {'x': d.value.x[ii],
         'y': d.value.y[ii],
         'group': d.value.groups[ii],
         'strip': d.key } 
}
                )
              }
            )
          }
        )


//assign data to the line
g.data(d3.nest().key(function (d) { return d.data.group }).entries(pointsdata));

//loop through each polyline and add a path to contain the data for tootips/hover
g[0].forEach(function(d) {   
  var mypath = d3.select(d).append('path')
  mypath
  .datum(mypath.datum().values)
  .attr('d',line)
  .attr('stroke','white')
})

var focus = svg.selectAll('.focus')
.data(d3.entries(data.groups)).enter().append('g')
.attr('class', 'focus')
.attr('id', function (d, i) { return 'focus-' + i; })
.style('display', 'none');

focus.append('circle')
.attr('r', 4.5)
.attr('stroke', 'black')
.attr('fill-opacity', '0');

focus.append('text')
.attr('x', 9)
.attr('dy', '.35em');

svg.select('#gridSVG').append('path')
.attr('class', 'yieldcurve')

svg
.on('mouseover', function () {
  focus.style('display', null);
  svg.selectAll('.yieldcurve').style('display',null)
})
.on('mouseout', function () {
  focus.style('display', 'none');
  svg.selectAll('.yieldcurve').style('display','none')
})
.on('mousemove', mousemove);

function mousemove() {
  var x0 = d3.mouse(this)[0];
  var i;
  var yieldcurve = [];
  
  for(i1 = 0; i1<data.groups.length; i1++) {
    groupdata = d3.select(g.select('path')[0][i1]).datum().values
    if(d3.max(groupdata, function(d){return d.x}) > x0) {
      i = bisectX(groupdata, x0, 1);
      break;
    }
  }
  
  d3.entries(data.groups).forEach(function (group, i1) {
    groupdata = d3.select(g.select('path')[0][i1]).datum().values
    
    var d;
    d = groupdata[i];
    //if (Boolean(d1)) { d = x0 - d0.x > d1.x - x0 ? d1 : d0 } else { d = d0 };
    d3.select('#focus-' + i1)
    .attr('transform', 'translate(' + d.x + ',' + ((+d3.select('rect').attr('height')) - d.y) + ')')
    .attr('fill', 'black'); //color(0));
  d3.select('#focus-' + i1).select('text')
  .text(d.data.group + ': ' + d.data.y)
  // .attr('fill', 'black'); //color(0));
yieldcurve.push({x:d.x,y:d.y})
  });
svg.selectAll('.yieldcurve')
.datum(yieldcurve)
.attr('d',line)
.attr('fill','none')
.attr('stroke','black');
//.attr('transform','scale(1,-1)')
}
"    ) %>>% HTML
  ) ) %>>%
  html_print

