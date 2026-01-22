---
title: "Features"
date: "2025-04-05"
layout: "single"
outputs: ["HTML"]
---

# Features  
Here are the key functionalities of the Bus Route Demand Visualizer.

## Simple and Flexible
- Just simple entering of numbers, bus service and/or specific stops (not yet imlemented), just need to press a button to generate.
- You can either choose to import your own CSV or import from LTA Datamall.

## Data Visualisation
- The output is a large image of a table with the colour intensity indicating the amount of demand from one origin stop to another destination stop on a bus service or specific stops (not yet implemented).
- The size of the image scales with the length of the bus route, to ensure that the size of the cells are similar.
- You can split the table to see the 1st half, 2nd half or the whole route. It's not exactly half, there're 3 stops added to the end of 1st half or the start of 2nd half so you can see a bit into the other half.

---

# Steps  
Follow these steps to get started with the app.

## Step 1: Import your data
- You can either choose uploading your data, or import from LTA Datamall. If you are uploading, read step 1a. If you are importing, read step 1b.

## Step 1a: Upload file
- Choose a LTA origin_destination_bus_YYYYMM.csv file from your file system.

## Step 1b: Import from Datamall
- Put in the year and the month, then press import. Do note that you need to put a leading zero for single-digit months.

## Step 2: Enter your bus service
- Only for existing bus services. This does not work for bus routes that used to exist.

## Step 3: Split the table
- Do you want to only see the first half or only see the second half? 0 is full route, 1 is 1st half, 2 is second half.

## Step 4: Select your direction
- Direction 1 and 2, makes sense. You might not be able to know where it begins and terminates, so you might need to check online or generate and see. Leave as 1 for loop services.

## Step 5: Click generate!
- The image generated is rather big, so you might need to save the image locally to get a better view.