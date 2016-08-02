Twitter Analytics
============

This repo contains a set of twitter analytics code that intends to help users better understand twitter data.

- Location Modelling tries to make predictions about where a twitter user is located at based on their network graph. We use the Microsoft Bing Map API (for converting locations into Lat/Lng coordiates), Azure DocDB (for storing Bing Map API queries) and Geonames API (for converting predicted Lat/Lng back to the more populous city in that area.

- Anomaly Detection tries to predict when outbursts/usually follower activities occur for a given twitter handle.

- Velocity Predictor tries to understand and model what kinds of content (e.g. images, videos, publish times) would viral and how much traction they would generate among followers.
