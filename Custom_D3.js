// Used this as a guide example - https://datatricks.co.uk/animated-d3-js-bar-chart-in-r
// Remove existing chart elements. 
svg.selectAll('*').remove();

//Set some initial values
var margin = options.margin,
    width = width-(2*margin),
    height = height-(4*margin),
    barPadding = options.barPadding*(width/data.length),
    barWidth = (width-(data.length*barPadding))/data.length,
    xmax = d3.max(data, function(d) { return d.location_description; }),
    xmin = d3.min(data, function(d) { return d.location_description; }),
    ymax = d3.max(data, function(d) { return d.count; });

//Create the x axis
var x = d3.scaleBand()
    .domain(data.map(function(d) { return d.location_description; }))
    .range([margin, margin+width]);

var xAxis = svg.append("g")
    .attr("transform", "translate(" + 0 + "," + (height+margin) + ")")
    .call(d3.axisBottom(x));

// Animate the x-axis labels
xAxis.selectAll("text")
    .style("text-anchor", "start")
    .attr("dx", "-.8em")
    .attr("dy", ".15em")
    .attr("transform", "rotate(45 -10 10)")
    .style("opacity", 0) // Hide the labels initially
    .transition()
    .delay(function(d, i) { return i * 100; }) // Delay the animation for each label
    .duration(1000) // Animation duration
    .style("opacity", 1); // Fade in the labels

//Create the y axis
var y = d3.scaleLinear()
    .range([height, 0])
    .domain([0, ymax]);
svg.append("g")
    .attr("transform", "translate(" + margin + ", " + margin + ")")
    .call(d3.axisLeft(y).tickFormat(d3.format(".0f")));
svg.append("text")
    .attr("transform", "translate(" + (margin - 50) + " ," + ((height + 2 * margin) / 2) + ") rotate(-90)") // Adjust the positioning
    .style("text-anchor", "middle")
    .style("font-family", "Tahoma, Geneva, sans-serif")
    .style("font-size", "14pt")
    .text(options.yLabel);

//Create the chart title
svg.append("text")
    .attr("x", (width / 2) + 30)
    .attr("y", margin / 2)
    .attr("text-anchor", "middle")
    .attr("dx", "1em")
    .style("font-size", "16pt")
    .style("font-family", "Tahoma, Geneva, sans-serif")
    .text(options.chartTitle);

//Create the chart
svg.selectAll('rect')
    .data(data)
    .enter()
    .append('rect')
    .attr('width', barWidth)
    .attr('x', function(d, i) { return (margin+((i+0.5)*barPadding)+(i*barWidth)); })
    .attr('y', height + margin)
    .attr('fill', options.color)
    .attr('stroke', '#00FFFB')  
    .attr('stroke-width', '2px');

//Transition animation on load
svg.selectAll('rect')
    .transition()
    .delay(function(d, i) { return i * 100; })
    .duration(function(d, i) { return 1000 + (i * 200); })
    .attr('height', function(d) { return d.count / ymax * height; })
    .attr('y', function(d) { return (height + margin - (d.count / ymax * height)); });

// Create a tooltip
var tooltip = d3.select('body')
  .append('div')
  .attr("class", "tooltip")
  .style('position', 'absolute')
  .style('background-color', '#142730')
  .style('border-radius', '5px')
  .style('padding', '5px')
  .style('opacity', 0)
  .style("font-family", "Tahoma, Geneva, sans-serif")
  .style("font-size", "12pt");

// Mouseover effects for tooltip
var mouseover = function(event, d) {
  tooltip
    .style('opacity', 1)
    .style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)');
  d3.select(this)
    .attr('fill', '#FFA500');
};

var mousemove = function(event, d) {
  var incidentText = (d.count === 1) ? ' incident' : ' incidents';
  tooltip
    .html(d.location_description + ': ' + d.count + incidentText)
    .style("left", (event.pageX + 30) + "px")
    .style("top", (event.pageY + 30) + "px");
};

var mouseleave = function(event, d) {
  tooltip
    .style("opacity", 0);
  d3.select(this)
    .attr('fill', options.color);
};

svg.selectAll('rect')
  .on("mouseover", mouseover)
  .on("mousemove", mousemove)
  .on("mouseleave", mouseleave);

