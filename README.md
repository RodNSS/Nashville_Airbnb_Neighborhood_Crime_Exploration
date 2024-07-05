# Nashville-Airbnb-Neighborhood-Crime-Exploration

![](airbnb.gif)

This application aims to function as both a crime map for Nashville, TN as well as a visualization of Airbnb properties
in relation to surrounding crime incidents. Currently, the app pulls about 3 months of crime data from the API and 
plots the instances on a map in clusters that can be filtered by crime type and date of occurrence. The dark purple 
to light green dots on the map are Airbnb rentals that are color coded based on the amount of crimes that have 
taken place within a quarter mile of each property. When you click a property on the map, the app returns a summary of 
the amount/type of crime surrounding that particular Airbnb within the given distance threshold.

For best viewing experience, enter full screen mode on your browser and uncheck "Always Show Toolbar in Full Screen." I built
the app using a 15 inch screen (2880 x 1800) and a 27 inch external monitor (3840 X 2160) but I realize the layout may look
less than ideal on different sized screens. I plan on fixing this and adding features that enhance the user experinece in future 
updates. I also plan on possibly expanding this idea to other cities where current crime data is available.

Link to app: https://roderick.shinyapps.io/Nashville-Airbnb-Crime-Map/

Below is the original summary I wrote for my Midcourse project at Nashville Software School.

<b>Executive Summary</b>

Airbnb is a popular online platform for travelers to find and reserve unique
accommodations around the globe. However, the platform has faced criticism for not
adequately addressing safety concerns for its users. By incorporating neighborhood
crime data into a map of Airbnb properties along with Google Street View, we can
provide users with a more comprehensive understanding of the safety of their potential
lodging and surrounding area. While providing valuable information for renters, it may
also provide valuable insight for policymakers and urban planners, who could use the
data to plan for city growth and address possible safety concerns.

<b>Motivation</b>

Initially, I wanted to visualize exactly where all Airbnb properties were in the Nashville,
TN area. The functionality of the map on Airbnb’s website is limited so I thought
combining the map with Google Street View would enhance the experience of the user.
By incorporating crime data into the visualization, users can see more easily which
neighborhoods have a higher risk of crime and make better informed decisions about
where to stay.

<b>Data Question</b>

What areas in the city have the most/least Airbnb listings? What do crime rates look like
around a particular Airbnb listing? What does the surrounding neighborhood look like?

<b>Minimum Viable Product (MVP)</b>

The intended audiences are Airbnb users or anyone who is curious about where Airbnb
properties are located. The app will provide an interactive map of all Airbnb properties
in Nashville, TN. Users will be able to search by property name or click on a property
that links directly to the listing on Airbnb’s website as well as links to the coordinates for
Google Street viewing. The user will also be able to visualize crime data for the
surrounding area.

<b>Data Sources</b>

http://insideairbnb.com/get-the-data

https://datanashvillegov-nashville.hub.arcgis.com/maps/d747436243e9439e968fce056545016a

<b>Known Issues and Challenges</b>

- Depending on the Airbnb host’s particular location settings, the coordinates may
not be exact. If location is set to “specific” there is a higher likelihood the
coordinates will match the exact property. If the location is set to “general” then
the coordinates only link to the general area the property is located in within 450 feet 
according to http://insideairbnb.com/data-assumptions. It’s still possible to find the exact 
property if the listing has an exterior photo or permit number but this is not ideal.

- There are lots of crime incidents which will require proper filtering and viewing
options in order to not overwhelm the map and make relevant for an Airbnb user.
