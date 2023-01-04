# Nashville-Airbnb-Neighborhood-Crime-Exploration

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

https://data.nashville.gov/Police/Metro-Nashville-Police-Department-Incidents/2u6v-ujjs

<b>Known Issues and Challenges</b>

- Depending on the Airbnb host’s particular location settings, the coordinates may
not be exact. If location is set to “specific” there is a higher likelihood the
coordinates will match the exact property. If the location is set to “general” then
the coordinates only link to the general area the property is located in within a
half mile radius. It’s still possible to find the exact property if the listing has an
exterior photo or permit number but this is not ideal.

- There are lots of crime incidents which will require proper filtering and viewing
options in order to not overwhelm the map and make relevant for an Airbnb user.
