#javascript to displayall names in a network
customJS <- '
function(el,x) { 
    var link = d3.selectAll(".link")
    var node = d3.selectAll(".node")

    var options = { opacity: 1,
                    clickTextSize: 10,
                    opacityNoHover: 0.1,
                    radiusCalculation: "Math.sqrt(d.nodesize)+6"
                  }

    var unfocusDivisor = 4;

    var links = HTMLWidgets.dataframeToD3(x.links);
    var linkedByIndex = {};

    links.forEach(function(d) {
      linkedByIndex[d.source + "," + d.target] = 1;
      linkedByIndex[d.target + "," + d.source] = 1;
    });

    function neighboring(a, b) {
      return linkedByIndex[a.index + "," + b.index];
    }

    function nodeSize(d) {
            if(options.nodesize){
                    return eval(options.radiusCalculation);
            }else{
                    return 6}
    }

    function mouseover(d) {
      var unfocusDivisor = 4;

      link.transition().duration(200)
        .style("opacity", function(l) { return d != l.source && d != l.target ? +options.opacity / unfocusDivisor : +options.opacity });

      node.transition().duration(200)
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });

      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d)+5;});

      node.select("text").transition()
        .duration(750)
        .attr("x", 13)
        .style("stroke-width", ".5px")
        .style("font", 24 + "px ")
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? 1 : 0; });
    }

    function mouseout() {
      node.style("opacity", +options.opacity);
      link.style("opacity", +options.opacity);

      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d);});
      node.select("text").transition()
        .duration(1250)
        .attr("x", 0)
        .style("font", options.fontSize + "px ")
        .style("opacity", 0);
    }

    d3.selectAll(".node").on("mouseover", mouseover).on("mouseout", mouseout);
}
'