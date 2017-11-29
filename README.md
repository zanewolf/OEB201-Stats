# OEB201-Stats
Code and stuffs for OEB201 class project where the goal was to analyze a data set and construct a bayesian model, along with faking data. 

The goal at the onset of this project was to be able to answer this question: Given a time and location, would I be able to predict what type of crime was occuring? 

Due to time restrictions and the enormity of the dataset, I narrowed my question to: Given a time and location, was Criminal Mischief or Miscellaneous Offenses more likley? 

I chose these two crimes because a) they were both suitable 'miscellaneous' crimes, and b) they were committed in roughly the same frequencies throughout the dataset. 

I then subsetted the year 2010 because they were the most equal in numbers (42,000 Mischief and 41,000 Misc.), and randomly sampled 10,000 data points. I built my model using this subsetted data. 

The model was a multilevel logistic model: Crime ~ Hour(0:23) + Location(Outside/Inside) + Hour:Location + (1|Neighborhood). 

The model is able to find the trends in both the real and simulated data, but does a bad job of giving back the exact coefficients I used to create the simulated data. 

Looking back on my work, I realized I did a varying intercepts model, but perhaps a varying slopes model would have been more appropriate. 

#####################################

The script cleaning_NYC_Crime_data.R is all the work I did to clean the dataset and select some data to do my final analysis on. This code takes in the original data file (https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i) and outputs the csv file NYPD_Crime_Data_CLEAN_2010.csv. 

The script modeling_NYC_Crime_data.R takes the CLEAN_2010.csv file and visualizes the real data, runs the model, makes the simulated data, visualizes the simulated data for comparison with the real data, and then runs the model with the simulated data. 

I also included the final presentation for the class, just because it shows some graphs and goes through the original data set in a way that I eventually deleted from the scripts. 

#####################################

I don't really have the intention of working on this project again, since I'll need to apply the skills I learned doing this project to my actual research data. 

However, I would definitely be interested if someone takes this and does some more work on it. Feel free to contact me or whatnot, also, if you have any questions about what I did and why. 

Please, also keep in mind that this was my first time ever doing a statistical modeling project, let alone a bayesian project. I'm sure there are dozens of idiot mistakes in there. Don't judge me too harshly. 

Cheers!
Z
  
  

