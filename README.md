# scalajs-canvas-pandemic-simulation
This is a Scala.js/canvas demo. Since I have been teaching myself Scala.js during the Covid-19 quarantine, the demo
is simple simulation of a pandemic outbreak.  

<H4>Requirements</H4>
Installed: sbt and node.js

jsdom (https://github.com/jsdom/jsdom) is needed for DOM support for node.js: `npm install jsdom`

For stack traces to be resolved: `npm install source-map-support`

<H4>Run</H4>
1. Run `sbt fastOptJS`

2. open index-dev.html in browser