# Victims' Access To Justice

This is the code that builds 5 R shiny Applications for the visualisation of victim data. Geberally, the aim of these apps is to understand and model confidence with the Criminal Justice System in England and Wales. More specifically:

* Two applications produce stacked barcharts, where users can cross attitudes towards the CJS with a variety of demographic traits and variables measuring crime.
* One application produces a time series plot, where users can visualise averaged levels of confidence/statisfaction across the years based on particular traits of the respondents.
* One application uses a clustering algorithm to understand similiarities among victims.
* One application models confidence via the use of logistic regression.

The code that produce the functionality and interface of the applications are in files:

* App_1.R
* App_2.R
* App_3.R
* App_4.R
* App_5.R

The files used for the tutorial sections of the apps are:

* tutorial_app1.Rmd
* tutorial_app2.Rmd
* tutorial_app3.Rmd
* tutorial_app4.Rmd
* tutorial_app5.Rmd

Deatils of statistical procedures (clustering and regression) are contained in the following files:

* kproto.Rmd
* The_model.Rmd

Further information about the project, the datatset and further resources can be found in these files:

* about.Rmd
* dataset_info.Rmd
* resources.Rmd