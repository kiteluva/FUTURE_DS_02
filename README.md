# Ad Campaign Performance Analysis (Facebook)

## Project Overview

This repository contains the data and Power BI dashboard for analyzing the performance of Facebook ad campaign. 
The goal of this project is to provide a comprehensive view of campaign effectiveness, engagement, and return on investment, enabling data-driven optimization decisions.

## Dashboard Features

The Power BI dashboard allows for in-depth analysis of ad campaign data across various dimensions. Key features include:

* **Overall Performance Metrics**: KPI cards display total clicks, impressions, spend, conversions, approved conversions, CTR, ROAS, and ROI.
* **Performance Trends**: A line chart visualizes clicks, impressions, and spend over time to identify daily/weekly trends and anomalies.
* **Audience Insights**: Bar charts break down clicks by age group and gender, helping to understand demographic engagement.
* **Campaign-Level Deep Dive**: A detailed table provides performance metrics (impressions, clicks, spend, conversions, CTR, ROAS, ROI) for each individual Facebook campaign ID, allowing for direct comparison and identification of top/bottom performers.
* **Interest-Based Analysis**: A bar chart shows clicks by interest group, helping to gauge the effectiveness of interest-based targeting.

## Repository Contents

* `cleaned_data.csv`: The raw ad campaign data used for this analysis.
* `ad_campaign_data_with_measures.csv` (optional: if you prefer to upload the pre-calculated CSV): The dataset including calculated measures like CTR, ROAS, and ROI.
* `Ad_Campaign_Analytics_Dashboard.pbix` The Power BI desktop file containing the data model, transformations, DAX calculations, and the dashboard visuals.

## Key Metrics Calculated

The following key performance indicators (KPIs) and metrics were calculated to provide deeper insights:

* **Click-Through Rate (CTR)**:
  $$ CTR = Clicks/ Impressions |times| 100 $$
  
    *Calculated using DAX in Power BI to prevent division by zero.*

* **Revenue from Ads**:
   $$ Revenue from Ads = Approved Conversions |times| Average Revenue per Conversion $$
  
    *For this analysis, an average revenue of $100 per approved conversion was assumed.*

* **Return On Ad Spend (ROAS)**:
    $$ ROAS = Revenue from Ads/ Spend $$
  
    *Indicates how much revenue is generated for every dollar spent on ads.*

* **Return On Investment (ROI)**:
    $$ ROI = (Revenue from Ads - Spend)/ Spend |times| 100 $$
  
    *Measures the percentage of profit generated relative to the ad spend.*


## Data Source

The analysis is based on ad campaign data from Facebook, provided in the `cleaned_data.csv` file. The dataset has been retrived from kaggle.
The data contains details such as ad IDs, campaign IDs, demographics, impressions, clicks, spend, and conversion metrics.

