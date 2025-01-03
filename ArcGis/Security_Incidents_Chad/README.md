# Security Incidents Analysis in Chad

This project is a spatial analysis of security incidents in Chad aimed at identifying and visualizing high-risk areas. By combining incident data with road networks, I created an interactive map to help humanitarian and local authorities assess and manage security risks more effectively.

👉 [Check security incidents in Chad](https://tchadima.maps.arcgis.com/apps/mapviewer/index.html?webmap=8d5b23c7b11c460da984b500454264b4)

## Project Overview
### Loading Data
I started by importing the CSV files containing the recorded security incidents in Chad. Each incident point was carefully added as a layer to the basemap to provide a clear geographical context.

### Adding Chad Routes Layer
I downloaded the Chad routes layer from the Humanitarian OpenStreetMap dataset, which provided a detailed road network across the country. This layer was crucial for analyzing how incidents impact nearby roads.

### Buffer Analysis
Using a buffer around each incident point, I identified areas within a certain radius that are potentially at risk. This step was essential in defining the scope of the danger zones surrounding each incident.

### Intersect and Join Tools
Next, I applied spatial tools such as Intersect and Join to find the segments of roads that fall within the high-risk areas. This allowed me to precisely highlight the portions of roads that might pose a danger to travelers.

### Symbology for Risk Visualization
To make the map visually intuitive:

### Dangerous roads were colored red.
Safe roads were colored green.
This symbology approach ensures that users can easily distinguish between high-risk and safe road segments.
### How to Use
Load the project in your GIS software (tested with QGIS and ArcGIS).
Ensure the Chad routes layer and incident data are correctly loaded.
Adjust the buffer radius and symbology settings as needed for your specific analysis.
Use the final map to plan safer travel routes or identify high-risk zones for further investigation.
### Data Sources
Security Incidents CSV: Provided by local security reports.
Chad Routes Layer: Downloaded from Humanitarian OpenStreetMap.
## Results
The analysis produced a clear, actionable map showing dangerous and safe road segments across Chad. This map can support humanitarian missions, aid workers, and local authorities in planning safe routes and avoiding high-risk areas.


## Conclusion
This project highlights the value of spatial analysis in addressing real-world problems. By combining publicly available data with spatial tools like buffers and intersections, we can turn raw incident reports into insightful, practical solutions.

Feel free to contribute to this project or reach out if you have any suggestions or questions.

License
This project is open-source and free to use under the MIT License.

Contact
If you have questions or feedback, you can reach me at eliasoumar@gmail.com. I'd love to hear how this project helps or any ideas for improving it!

