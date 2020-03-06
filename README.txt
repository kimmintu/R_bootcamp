README

You will find in this directory two sub-directories corresponding to the two assignments.

Assignment 1
./list-of-function/
In this directory, a list of all functions discussed during the R Bootcamp can be found in three formats, Excel, Rmd and HTML. The HTML page is generated from the Rmd file.

Assignment 2
./bike-share-analysis/
The data analysis project is located within in this directory. This directory is generated via RStudio. To run the project, it is best to open this RStudio project via opening the "bike-share-analysis.Rproj" project file. To reproduce the result (generate the report.pdf), please do the following 3 steps:
1. Run the file ./Rscript/process_bike_dataset.R to generate d.bike.rds
2. Run the file ./Rscript/process_weather_dataset.R to generate d.weather.rds
3. Run the file ./Rscript/report.Rmd with the option "Knit to PDF" to generate the report.pdf

The project is structured in the following sub-folders.
./bike-share-analysis.Rproj	-> the RStudio project file
./Rscript/ -> the source code files for the project
./Rscript/api_weather_json_to_csv.ipynb -> this file is a Python Jupyter notebook, running this file will download JSON weather data from the API endpoint [https://api.weather.com/v1/location/KLGA:9:US/observations/historical.json], convert and store the weather data in ./data/New_York_Weather_Hourly_2016.csv
./Rscript/process_bike_dataset.R -> run this file to process the CSV raw bike sharing data source and store the processed data in the ./data/d.bike.rds
./Rscript/process_bike_dataset.R -> run this file to process the CSV raw weather data source (generated from the Python Jupyter Notebook) and store the processed data in the ./data/d.weather.rds
./Rscript/report.Rmd -> the analysis is in this R Markdown file, the report.pdf is generated from this file
./data/NYC-CitiBike-2016.csv -> this is the bike sharing source data, downloaded from [https://www.kaggle.com/samratp/bikeshare-analysis#NYC-CitiBike-2016.csv]
./data/New_York_Weather_Hourly_2016.csv -> this is the weather data source generated via the Python Jupyter Notebook
./data/d.bike.rds -> this is the processed bike data generated via running the ./Rscript/process_bike_dataset.R
./data/d.weather.rds -> this is the processed weather data generated via running the ./Rscript/process_weather_dataset.R
./data/d.total.rds -> this is the merged data from the ./data/d.bike.rds and ./data/d.weather.rds, generated via the analysis R Markdown file
./output/repord.pdf -> this is the analysis report file is generated from the ./Rscript/report.Rmd R Markdown file
