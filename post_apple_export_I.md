---
title: "Post Apple Export Part I"
author: "John Goldin"
date: '2020-01-25'
categories:
  - R quantified_self
slug: working-with-apple-health-export
lastmod: '2019-10-06T08:50:22-04:00'
layout: post
type: post
highlight: no
draft: yes
output: 
  html_document:
    keep_md: yes
---



This post is Part I of a dive into the contents of the Apple Health Export. 
We will work through the mechanics of moving data from the Apple Health app
out of your iPhone and into R where you can analyze it. It will describe in detail the problem of 
adjusting the time stamps for daylight savings time and travel across time zones.
Unfortunately the topic of time zones and the Apple Health Export is a complicated subject.

##### First, export the data from the Health app

From the Health app on your iPhone, one can export all of the data you are able to view via the Health app. 
Open the Health app on your iPhone. 
To export, you need to first go to your personal settings by
clicking on your icon near the upper right corner of the Browse screen. 
(See the the first screenshot below.)
Click on the icon and you will see some of your personal settings. 
You will need to scroll to the bottom of this page, where you will
see a clickable line "Export All Health Data", as shown in the second screenshot below. 

<style> 
.row {
  display: flex;
}
.column {
  flex: 100%;
  padding: 5px;
}
</style>
<div class="row">
  <div class="column">
    <figure>
      <figcaption> Browse Health app</figcaption>
      <img src="health_app.PNG" alt="health app screenshot1" width="75%" height="100%" align="center"/>
    </figure>
  </div>
  <div class="column">
    <figure>
      <figcaption>Export All Health Data</figcaption>
      <img src="health_export_dialog.PNG" alt="health export screenshot" width="75%" height="100%" align="center"/>
    </figure>
  </div>
</div>
Once you click OK to go ahead with the export, it may take a significant amount of time.
On my iPhone 8 it takes more than five minutes. Once it is complete, you'll get a 
dialog that asks where to send the exported data. I use AirDrop to send it to the
Mac where I am running RStudio. It ends up in the Downloads folder on that Mac.
If you need to move the data to a Windows computer, you may need to send it
via email or Dropbox.
The exported file is named `export.zip`. If you double-click on that file it 
will expand into a folder called `apple_health_export`. The uncompressed file is huge
in comparison with the size of the zip file. In my case, `export.zip` is about
79 megabytes which becomes an `apple_health_export` folder that is 2.45 gigabytes!
In my R code, I uncompress the file into my Downloads folder, which is excluded
from my Time Machine backups.

##### R code to expand the export file and import it as XML data

The R code below shows how to decompress `export.zip` and follow some
basic steps to import it into R. I'm following in the footsteps of
several people who have published code to accomplish these steps.
See  work by [Ryan Praskievicz](https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d), [Taras Kaduk](https://taraskaduk.com/2019/03/23/apple-health/), [Raul Eulogio](https://www.inertia7.com/projects/149), and [Deepankar Datta](https://github.com/deepankardatta/AppleHealthAnalysis) (who has
created a package called AppleHealthAnalysis). I'm sure there are other examples
using R, and there are quite a number of examples using python (e.g., by [Mark Koester](http://www.markwk.com/data-analysis-for-apple-health.html)).

The R code uncompresses the zip file and replaces the `apple_health_export` folder.
(Because of the size of this folder, I try to avoid having multiple copies and also
avoid having it saved to my disk backup.)
The big file inside that folder is `export.xml`. Following the examples sited above, I
use the XML package to covert the major elements of the XML file into
tidy data frames.




```r
rc <- unzip("~/Downloads/export.zip", exdir = "~/Downloads", overwrite = TRUE)
```

```
## Warning in unzip("~/Downloads/export.zip", exdir = "~/Downloads", overwrite =
## TRUE): error 1 in extracting from zip file
```

```r
if (length(rc) != 0) {
  file.remove("~/Downloads/export.zip")
  # once unzipped, delete export.zip. Otherwise, the next time Air Drop sends export.zip
  # to your mac it will be renamed as export2.zip and you may accidentally process
  # an out-of-date set of data.
  
  # takes a bit more than 20 seconds on my iMac
  health_xml <- xmlParse("~/Downloads/apple_health_export/export.xml")
  # takes about 70 seconds on my iMac
  health_df <- XML:::xmlAttrsToDataFrame(health_xml["//Record"], stringsAsFactors = FALSE) %>%
    as_tibble() %>% mutate(value = as.numeric(value))
  
  activity_df <- XML:::xmlAttrsToDataFrame(health_xml["//ActivitySummary"], stringsAsFactors = FALSE) %>% 
    as_tibble()
  workout_df <-  XML:::xmlAttrsToDataFrame(health_xml["//Workout"], stringsAsFactors = FALSE) %>% 
    as_tibble
  clinical_df <- XML:::xmlAttrsToDataFrame(health_xml["//ClinicalRecord"]) %>% 
    as_tibble()
} else print(load("interim_save.RData"))
```

```
## [1] "health_xml"  "health_df"   "activity_df" "workout_df"  "clinical_df"
```

I won't go into the details of the XML structure of the health export.
For most purposes, the Record, ActivitySummary, Workout, and Clinical data types
will provide all that you are looking for. My expanded Apple Health Export
folder also includes workout GPX files, electrocardiograms, and clinical
records imported from my health system's medical records system.

I have a bit over two years of Apple Watch data in my iPhone. 
After a full career working
as a data analyst, this is the largest number of data points
I have ever dealt with. Extracting the "Record" data  from `export.xml` produces
3.4 million rows and takes about 70 seconds on my 2019 iMac. 

The counts by "type" describe the breadth and quantity of data:

```r
health_df2 <- health_df %>% 
  mutate(source = case_when(
    str_detect(sourceName, "Phone") ~ "Phone",
    str_detect(sourceName, "Watch") ~ "Watch",
    str_detect(sourceName, "Lose It") ~ "Lose It!",
    TRUE ~ "Other"),
    source = factor(source, levels = c("Watch", "Phone", "Lose It!", "Other")))
health_df2 %>% 
  janitor::tabyl(type, source) %>% 
  janitor::adorn_totals("col") %>%  arrange(desc(Total)) %>% 
  janitor::adorn_totals("row") %>% # do column total after arrange
  kable(format.args = list(decimal.mark = " ", big.mark = ","),
        # caption = "Frequency by Type and Data Source", format = "markdown") %>% 
        caption = "Frequency by Type and Data Source")
```

<table>
<caption>Frequency by Type and Data Source</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> type </th>
   <th style="text-align:right;"> Watch </th>
   <th style="text-align:right;"> Phone </th>
   <th style="text-align:right;"> Lose It! </th>
   <th style="text-align:right;"> Other </th>
   <th style="text-align:right;"> Total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierActiveEnergyBurned </td>
   <td style="text-align:right;"> 1,340,677 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1,340,678 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierBasalEnergyBurned </td>
   <td style="text-align:right;"> 825,438 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 825,438 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDistanceWalkingRunning </td>
   <td style="text-align:right;"> 598,252 </td>
   <td style="text-align:right;"> 138,351 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 736,604 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierHeartRate </td>
   <td style="text-align:right;"> 596,861 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1,539 </td>
   <td style="text-align:right;"> 598,400 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierStepCount </td>
   <td style="text-align:right;"> 52,252 </td>
   <td style="text-align:right;"> 133,420 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 185,673 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierAppleExerciseTime </td>
   <td style="text-align:right;"> 49,885 </td>
   <td style="text-align:right;"> 337 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 50,222 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierFlightsClimbed </td>
   <td style="text-align:right;"> 12,504 </td>
   <td style="text-align:right;"> 11,275 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 23,780 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKCategoryTypeIdentifierAppleStandHour </td>
   <td style="text-align:right;"> 18,947 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 18,954 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierAppleStandTime </td>
   <td style="text-align:right;"> 5,779 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5,779 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietaryFatTotal </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4,590 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4,590 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierHeartRateVariabilitySDNN </td>
   <td style="text-align:right;"> 4,135 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4,135 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietaryFatSaturated </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4,060 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4,060 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietaryEnergyConsumed </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,984 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,984 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierEnvironmentalAudioExposure </td>
   <td style="text-align:right;"> 3,975 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,975 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietaryProtein </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,738 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,738 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietaryFiber </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,685 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,685 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietarySodium </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,683 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,683 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietarySugar </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,611 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3,612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietaryCholesterol </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,463 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,463 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKCategoryTypeIdentifierSleepAnalysis </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3,252 </td>
   <td style="text-align:right;"> 3,252 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierBloodPressureDiastolic </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2,138 </td>
   <td style="text-align:right;"> 2,138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierBloodPressureSystolic </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2,138 </td>
   <td style="text-align:right;"> 2,138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDistanceCycling </td>
   <td style="text-align:right;"> 1,415 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1,415 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierRestingHeartRate </td>
   <td style="text-align:right;"> 804 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 804 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierWalkingHeartRateAverage </td>
   <td style="text-align:right;"> 725 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 725 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierBodyMass </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 214 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 216 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierVO2Max </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKCategoryTypeIdentifierMindfulSession </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierHeight </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietaryCaffeine </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierDietaryCarbohydrates </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierHeadphoneAudioExposure </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HKQuantityTypeIdentifierNumberOfTimesFallen </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Total </td>
   <td style="text-align:right;"> 3,511,853 </td>
   <td style="text-align:right;"> 283,393 </td>
   <td style="text-align:right;"> 31,028 </td>
   <td style="text-align:right;"> 9,076 </td>
   <td style="text-align:right;"> 3,835,350 </td>
  </tr>
</tbody>
</table>

```r
  # clipr::write_clip(allow_non_interactive = TRUE)
```

Most of the data is collected via my Apple Watch rather than my iPhone. 
Some basic movement data is collected by the iPhone. 
I have been using the *Lose It!* app on my iPhone for about six months to count calories,
and that produces a noticeable amount of data. 
The free version of the app which I am using does not display much beyond
basic calorie counts. 
It's interesting to see that the more detailed nutrition breakdowns are passed
into the Health app. I haven't attempted to look at any of the nutrition information.

As far as the "Other" category, I'm using an Omron blood pressure cuff 
that can transfer readings to the Omron app on the iPhone via Bluetooth. Those
readings are then updated in the Apple Health database. 
There are a few odds and ends contributed by apps on my iPhone
such as AllTrails, Breathe, and AutoSleep.

Sometimes the same data may be repeated on multiple sources
so it is important to pay attention to `sourceName`.
Note that step counts, flights climbed, and distance walking/running
come from both the Watch and the iPhone. 
On any particular day you probably want to include only one of the sources.
Otherwise you risk double counting. 
Generally I am focused on the Watch data, but I have almost three years of
data from my iPhone before I started wearing the Apple Watch.

The other major XML categories are ActivitySummary, Workout, and ClinicalRecord. 
For ActivitySummary I have one row per
day which basically summarizes some of the intra-day activity data. 
Workout has one row per workout. If I were
still running I would focus much more on that data frame. 
For each workout it shows type, distance, duration, 
and energy burned Most of my workouts are outdoor walks. 
Quite often I forget to end the workout when I end the walk,
which certainly reduces the usefulness of the data. 
But I would imagine that for a runner or a swimmer or a cyclist, 
the Workout information would be interesting and useful.

ClinicalRecord is a bit tricky. 
I have set things up so that my health organization shares my health records with the
Apple Health app.


```r
clinical_df %>% count(type) %>% kable(format = "markdown", caption = "Types of Clinical Items")
```



|type                |   n|
|:-------------------|---:|
|AllergyIntolerance  |   1|
|Condition           |  12|
|DiagnosticReport    |  63|
|Immunization        |  15|
|MedicationOrder     |   7|
|MedicationStatement |  13|
|Observation         | 694|
|Patient             |   1|
|Procedure           |   7|

In the clinical data frame there is a column called `resourceFilePath` that contains 
the path to a json dataset in the `Apple Health Export/clinical records` folder. Presumably
this would allow you to retrieve items such as individual lab test results. 
I haven't made any attempt to get into this data. I only know what's available
here because I can view it via the Apple Health app.

### The Problem of Time Zones and Daylight Savings

When I first looked at day by day data for resting heart rate I bumped into
problems caused by the issue of time zones. I have about 745 rows of data of type
`HKQuantityTypeIdentifierRestingHeartRate` which should be one per day. 
I quickly discovered I had several days where there were two values in a single day, 
which were related to
occasions when I traveled by air to a different time zone.

This leads to a long digression on the subject of computer time stamps and time zones. 
(Here is an [entertaining video](https://www.youtube.com/watch?v=-5wpm-gesOY) that describes
the general problem of time stamps and time zones, but it doesn't relate to the specific
problems that I will get into here. It's fun if you want to nerd out on this topic.)

Each item in the records of the health dataset has a creation, start, and end time stamp. In the export dataset
they appear as a character string that looks like this: "2019-04-10 07:10:34 -0400". 
The "-0400" on the end
is because as I write this local time is Eastern Daylight Savings which is four hours 
earlier than UTC (universal
time code). At first I thought the time code info at the end of the text string would take care of everything. 
In fact, it is useless.
As near as I can tell, the internal data has no indication of time zone.[^1]
The UTC offset is attached to the datetime information when the data is exported. 
Every single time stamp in the exported dataset has the same "-0400" offset,
which merely represent my local offset at the time the export was done. 
If I re-exported that data after the
switch to Eastern Standard Time, all of the offsets will appear as "-0500". 
In fact, the exported data has no information about time zone or daylight savings. 
When I did a workout in San Diego in January, the time stamp that was attached to that workout
was the local time when I did the workout. In the export file now it has a UTC offset of "-0400". 
In reality, local time when I did the workout was offset from UTC by "-0800" (i.e. 8 hours). 
I can tell that this is an issue of how the Apple Health data stores 
the date and time because I can see the issue via the Activity app on my phone,
not just in the export. 
When I go into the Activity app on my iPhone and see that it claims I started a walk in England 
on 9/1/2019 at 05:51:58. I'm not that much of an early riser. 
I know from my travel diary that I actually got started four hours later than that
at 09:51:58 local time. 
If I went back to look at the same workout after the change from daylight savings to standard
time, it would appear as 04:51:58 rather than 05:51:58.

[^1]: The documentation for the Apple Health Kit does offer a way for developers to store [time zone meta data](https://developer.apple.com/documentation/healthkit/hkmetadatakeytimezone?language=objc) via the `HKMetadataKeyTimeZone` object. It appears that [not even Apple uses this feature](https://stackoverflow.com/questions/49250964/hkmetadatakeytimezone-is-always-nil-for-health-data-which-is-created-by-apples). And the Apple documentation only suggests using it for sleep data. It would be impractical to try to attach this meta data to every single observation.

When does this matter? 
With a multi-time-zone shift, it's not hard to end up with datetime stamps that appear in
the wrong day. 
Compounding the problem is that items such as resting heart rate are not always
saved in the data at the same hour of the day. 
A number of items like resting heart rate are recorded once per day.
But because of time zone issues, you can end up with two on one day and none on another. 
Also, there may be situations where you want to look at patterns over the course of a day. 
At one point I wanted to look at whether there were periods when my heart rate was 
unusually high during normal sleeping hours. 
I looked for a heart rate above 120 between the hours of 11PM and 6AM. 
I got hits for when I was hiking in a different time zone because the 
datetime stamp appeared to be during those night time hours when in fact the local time
was shifted five or seven hours because of the different time zone.

This is a tricky problem. I use the `lubridate` package to deal with the datetime stamps. 
R relies on a Unix-based standard for dates and time called 
[POSIX](https://en.wikipedia.org/wiki/POSIX) that is implemented as a class 
called POSIXct. You can see lots of references to POSIXct in the `lubridate` documentation. 
The `as_datetime` function in `lubridate` allows you to add a `tz` parameter that 
specifies the time zone. 
The trick is that the time zone is stored as an *attribute* of the vector 
rather than as part of the data. 
If you have a vector of `datetime` data, 
the time zone attribute applies to the entire vector, not to individual elements
in the vector. 
If you want to store time zones that vary within the vector, you need to store them in a separate
vector, and that's not part of the R standard for handling dates and times. 
You're on your own. The `lubridate` package includes some functions to help convert vectors 
from one time zone to another and to deal somewhat with
daylight savings. 
But it does not automatically help with a vector that contains datetime information from
varying time zones (as well as different daylight savings issues). 
(See [Clayton Yochum](https://blog.methodsconsultants.com/posts/timezone-troubles-in-r/) 
for a more detailed discussion of general time zone messiness in R.)

As I searched the web for tips on how to approach this issue, I discovered that
there's a population of people who are working hard to maintain a streak 
in filling their activity rings in the Apple Activity app.
Some of those individuals get frustrated because they are tripped up
by movement across time zones or even changes to daylight savings.
There are a number of tips out there for activity tracking in the face of 
[crossing time zones](https://9to5mac.com/2018/04/02/how-to-fill-apple-watch-activity-rings-while-traveling-timezones/). 

#### My Treatment of Time Zones and the Apple Health Export

Here I describe a solution to the time zone issue, but it is not very elegant.

First I wrote a function `get_my_time_zone` that identifies in what time zone I was 
located during the two year period for which I need time zone info to interpret watch data. The
function hard codes when I landed in a different time zone and therefore changed
the time zone on my watch. That's the aspect of this solution that is not very elegant. 
The function hard codes my personal travel history. It will only work for me. If I
travel across a time zone I need to remember to edit the function with my travel details.
 

```r
get_my_time_zone <- function(dt) {
  # What I'm going for is the time zone used by my watch. 
  # I'm assuming my watch got the local clock time about the
  # same time as the scheduled arrival for my flight.
  time_zone <- case_when(
    (dt >= as_datetime("2018-01-31 16:00:00")) & # trip to RStudio conference
      (dt <= as_datetime("2018-02-07 13:01:00")) ~ "America/Los_Angeles",
    (dt >= as_datetime("2018-04-18 08:00:00")) & # trip to Amsterdam
      (dt <= as_datetime("2018-04-20 13:50:00")) ~  "Europe/Amsterdam",
    (dt >= as_datetime("2018-04-20 13:50:00")) & # trip to Athens
      (dt <= as_datetime("2018-04-30 15:52:00")) ~  "Europe/Athens",
    (dt >= as_datetime("2019-06-21 03:45:00")) & # trip to SW England
      (dt <= as_datetime("2019-07-05 13:25:00")) ~  "Europe/London",
    (dt >= as_datetime("2019-08-28 06:30:00")) & # trip to Manchester
      (dt <= as_datetime("2019-09-10 12:40:00")) ~  "Europe/London",
    TRUE ~ "America/New_York" # good old Eastern time, home sweet home
  )
  return(time_zone)
}
get_my_time_zone <- compiler::cmpfun(get_my_time_zone)
```

Once I have a time zone column that corresponds to the time zone for 
each row, I need to use `lubridate` functions to adjust the Apple Health export 
time stamps so that the hour corresponds to the local time I actually experienced.


```r
# I will be applying this function to nearly a million times so it's important
# that it be vectorized.
UTC_to_clock_by_tz <- function(dt, time_zone) {
  # adjust a vector of datetime to a specific time zone and report as though it were utc
  tz(dt) <- Sys.timezone()    # make sure the attribute of the vector is set to my current local time zone
  utc <- with_tz(dt, tzone = "UTC")   # what is the datetime in terms of UTC
  # with_tz is the key lubridate function that I am relying on. Handles daylight savings as well.
  local <- with_tz(utc, time_zone) # now adjust utc to the time zone I want
  tz(local) <- "UTC"    # treat everything as if it were UTC, even if it isn't, because the whole vector has to be one arbitrary time zone when I bind rows together
  # Although the vector is marked as UTC, I will treat the hour as being whatever the local
  # time was that I experienced then.
  return(local)
}
```

Next I will apply the `UTC_to_clock_by_tz` function to the character time stamps
in the Apple Health Export. The function needs to be applied to a vector with
the same time zone for all elements in the vector. By doing `group_by(start_time_zone)`
before I use the function inside `mutate`, the function will be applied with a 
different time zone for each group. 
That way the function is vectorized for each group and is reasonably fast. 
I did not group the time zones separately for the start date and the end date. Usually
they would be in the same tine zone, and even if they are not I want to handle them as if they were. 


```r
system.time(
  health_df <- health_df %>% 
    mutate(startDate = as_datetime(str_sub(startDate, 1, 19)),
           endDate = as_datetime(str_sub(endDate, 1, 19)),
           creationDate = as_datetime(str_sub(creationDate, 1, 19)),
           start_time_zone = get_my_time_zone(startDate)) %>% 
    group_by(start_time_zone) %>% 
    # assume end_date is in the same time zone as start_date
    mutate(start_date = UTC_to_clock_by_tz(startDate, first(start_time_zone)),
           end_date = UTC_to_clock_by_tz(endDate, first(start_time_zone))) %>% 
    # mutate(end_time_zone = get_my_time_zone(endDate)) %>% 
    # group_by(end_time_zone) %>% 
    # mutate(end_date = UTC_to_clock_by_tz(endDate, first(end_time_zone))) %>% 
    ungroup() %>% 
    mutate(date = as_date(start_date), hour = hour(start_date)) %>% 
    arrange(type, start_date) %>% 
    ungroup()
)
```

```
##    user  system elapsed 
##  42.378   3.232  45.977
```

```r
# Here I'll adjust time for workout_df as well
workout_df <- workout_df %>% 
  mutate(startDate = as_datetime(str_sub(startDate, 1, 19)),
         endDate = as_datetime(str_sub(endDate, 1, 19)),
         creationDate = as_datetime(str_sub(creationDate, 1, 19)),
         start_time_zone = get_my_time_zone(startDate)) %>% 
  group_by(start_time_zone) %>% 
  mutate(start_date = UTC_to_clock_by_tz(startDate, first(start_time_zone)),
         end_date = UTC_to_clock_by_tz(endDate, first(start_time_zone))) %>% 
  # mutate(end_time_zone = get_my_time_zone(endDate)) %>% 
  # group_by(end_time_zone) %>% 
  # mutate(end_date = UTC_to_clock_by_tz(endDate, first(end_time_zone))) %>% 
  ungroup()
# I'm going to focus on health_df and workout_df, but I could adjust times in the other df's as well
```

Save some stuff so that I can skip the slow steps above:


```r
save(health_xml, health_df, activity_df, workout_df, clinical_df, file = "interim_save.RData")

# print(load("interim_save.RData"))
```

##### Apple, If You're Listening...

At this point there's no way that Apple will add time zone (or even UTC offset) to each data point.
But there is a relatively simple item of data they could add to the health data that would
make it easier (and more accurate) to execute the adjustments for time zone described in this post.
Whenever you change the time zone on the watch or phone, surely there is a log of that event. If
those change events were added to the health dataset as a separate item, then one could construct
an effective way to adjust all the time stamps in the datasets. I don't know whether there are 
logs to accomplish this retrospectively, but even if it was added only for present time changes
that would be a big help. It would add very little data. Travel and daylight savings are the 
only events that cause me to change the time on my phone or watch. The fancy solution from Appple
would be if they added time change events to the data and then used that data to adjust time stamps
at the time they produced the Apple Health Export. That would be a great help and make the data
less confusing. 

#### Using TripIt Data to Track Your Plane Flights

The `get_my_time_zone` function gets the job done, but would not work well for somneone who
does a lot of travel. 

A jazzier approach is to get a history of flights via a TripIt API. This
was inspired by a talk by Hadley Wickham. The point of the talk was data 
visuallization, but one of his [examples](https://github.com/hadley/vis-eda/blob/master/vis-eda.pdf) 
was based on relationship between
when he is traveling and number of commits on GitHub. 
What I needed was the history of my airplane flights.

I started with [Hadley's code](https://github.com/hadley/vis-eda/blob/master/travel.R) 
to fetch history from TripIt. There's a TripIt [developer page](https://www.tripit.com/developer) that points you to
more information. TripIt has an OAuth API for full-strength applications. That was more
than I needed. Hadley used a simpler for of authorization. Here's the [TripIt description](http://tripit.github.io/api/doc/v1/index.html):

> TripIt API offers a very simple way of authenticating the API that should only be used for testing and development purposes.... Note that this authentication scheme is off by default for every TripIt user. If you want to have this turned on for your account so you can use it for development purposes please send email to support@tripit.com.    
TripIt support responded the next day,
and from there I could use `httr` functions to get the data from TripIt via information
from documentation of the TripIt API.

I did one `GET` call from `httr` to get a list of my TripIt trips. Next I used the `purrr` package
to extract data from the nested JSON lists returned by TripIt. In particular, I used the `map` function
to get TripIt "air" objects for each trip ID. Individual airplane flights are "segments"
with each air object. For example, a trip might be two connecting flights to get to the 
destination and two flights to return home, each represented by a "segment". 
I always feel like I'm a few steps away from thorough understanding of
`purrr` and tend to rely on a certain amount of trial and error to get a sequence of `flatten`
and `map` call that extract what I need. The code is available from a repo I made to support
this post: [https://github.com/johngoldin/applehealth1/R/tripit_functions.R].

I end up with a 
data frame that has the scheduled departure and arrival for each fight and conveniently provides
the time zone for each airport. Note that in practice this data might not be perfect. It is scheduled
flights only and would not account for cancelled fligts or even the time of a delayed flight. So don't 
try to use this data to examine whether your heart rate is elevated during takeoffs and landings.

I took a quick detour and explored whether I could use the FlightAware API to get the actual arrival
times. It is now easy to get a free access to limited FlightAware data. But the API calls are
oriented to retrieving current rather than historical data.




Once I have the trip ID's, I use trip ID to fetch the flight
information. I used the RStudio `View()` function to exame the
the results I got back from calls to the TripIt API. At this point I
my `purrr` skills are shakey enough that I tend to do a fair amount of
trial and error to figure out how to pick out what I need from the
nested lists returned by the API. I get one trip at a time, fetch the
air segments, and then bind them together with `purrr::map_dfr`.



```r
GET_air <- function(trip_id) {
  atrip <-
    GET_tripit(
      paste0(
        "https://api.tripit.com/v1/get/trip/id/",
        trip_id,
        "/include_objects/true"
      ) )
  air_trip <- atrip[["AirObject"]][["Segment"]]
  flights <- dplyr::tibble(
    trip_id = trip_id,
    trip_start = atrip[["Trip"]][["start_date"]],
    start_date = air_trip %>% purrr::map("StartDateTime") %>% map_chr("date"),
    start_time =  air_trip %>% purrr::map("StartDateTime") %>% map_chr("time"),
    start_timezone =  air_trip %>% purrr::map("StartDateTime") %>% map_chr("timezone"),
    start_city = air_trip %>%  purrr::map_chr("start_city_name"),
    end_date = air_trip %>% purrr::map("EndDateTime") %>% map_chr("date"),
    end_time =  air_trip %>% purrr::map("EndDateTime") %>% map_chr("time"),
    end_timezone =  air_trip %>% purrr::map("EndDateTime") %>% map_chr("timezone"),
    end_city = air_trip %>%  purrr::map_chr("end_city_name"),
    airline = air_trip %>%  purrr::map_chr("marketing_airline"),
    code = air_trip %>%  purrr::map_chr("marketing_airline_code"),
    number = air_trip %>%  purrr::map_chr("marketing_flight_number"),
    aircraft = air_trip %>%  purrr::map_chr("aircraft_display_name"),
    distance = air_trip %>%  purrr::map_chr("distance"),
    duration = air_trip %>% purrr::map_chr("duration")
  )
}
GET_air_mem <- memoise::memoise(GET_air)
```
```r
flying <- trip_ids %>% map(GET_air_mem)
```

I ended up with a tibble with one row per flight. I focused on the ending time and location
of each flight. That's when I figure my watch gets changed to a new time zone and 
life in that time zone begins. Life in that time zone ends when life in the next time
zone begins, so `until = lead(end), until_timezone = lead(end_timezone)`.
I set the last `until` to `now()`

I was missing one trip in my TripIt data so I manually added a three-row table to include
that data. That's what one would have to do to allow for travel to another
time zone by car or train. Careful, if data documenting the transition from one time zone to
another is missing the whole table will be off kilter.


```r
arrivals <- flying %>%
  mutate(start = ymd_hms(paste0(start_date, start_time)),
         end = ymd_hms(paste0(end_date, end_time))) %>% 
  filter(start_timezone != end_timezone) %>% # only trips that change time zone matter
  select(trip_start, end, end_timezone, end_city) %>%
  # manual additions here
  bind_rows(tibble(
    trip_start = rep("2018-04-18", 3),
    end = ymd_hm(c("2018-04-20 21:00", "2018-04-30 15:23", "2018-04-18 09:15")),
    end_timezone = c("Europe/Athens", "America/New_York", "Europe/Amsterdam"),
    end_city = c("Athens", "New York", "Amsterdam")
  )) %>%
  arrange(end) %>%
  mutate(until = lead(end), until_timezone = lead(end_timezone))
  arrivals$until[nrow(arrivals)] <- now()
  arrivals$until_timezone[nrow(arrivals)] <- Sys.timezone()

  # adjust flight times to local time zone of export (to line it up with the export)
  arrivals <- arrivals %>% mutate(arrival_time = double_to_datetime(map2_dbl(end, end_timezone, local_to_sys_time)),
                            until_time = double_to_datetime(map2_dbl(until, until_timezone, local_to_sys_time)))
```

The final step was to associate a time zone with each of the 3.5+ million rows in `health_df`.
I thought this would be difficult and very slow. I was wrong! The `fuzzyjoin` package
by Dave Robinson
came to the rescue. I can't do a simple join between flights and data rows because there is
not an exact match for the time stamps. Instead I want to join the time stamps in `health_df`
with a start and end time stamp for each row in the flights table. It turns out
`fuzzyjoin` has that covered. 

The `fuzzyjoin` package provides a variety of special joins that do not rely on an exact match.
In this case, what I needed was the `interval_left_join` function which joins tables based
on overlapping intervals. The help for `interval_left_join` explains that this function requires
the IRanges package available from Bioconductor and points to 
[instructions for installation](https://bioconductor.org/packages/release/bioc/html/IRanges.html).
This was the first time I have used anything from the Bioconductor repository. I'm
impressed by the speed of `interval_left_join`. I thought it would be impractical to run it
on the full dataset, but it feels instantaneous. I also used it to relate rows in 
`health_df` to rows in `workout_df`, but I'll describe that in Part II.



```r
  health_df <- health_df %>%
    filter(!is.na(start_date)) %>%
    mutate(start = start_date, end = start_date) %>%
    interval_left_join(arrivals %>% select(start = end, end = until, timezone = end_timezone, end_city))
```

```
## Joining by: c("start", "end")
```
  
Just to be clear, let's work through a concrete example of how the flight
information is used.
There is a flight to Paris on 2014-10-08 that leaves at
20:45 NY time, duration of flight is 6 hours 40 minutes. Therefore
arrival is scheduled as 03:25 the next morning NY time which is
09:25 Paris time. 09:25 Paris time is 07:25 UTC.
If Export times are all converted from UTC to
eastern time, then the plane's arrival would appear to be
03:25 (same as above). So if I see a time in the data that
is after 03:25 and (before the return to New York) it needs
to be adjusted to local Paris time rather than to New York time.
