Capstone - Predict the Next Word   
========================================================
author: Don Martin 
date: 9/12/2017
autosize: true
font-family: 'Times'
transition: rotate

Introduction
========================================================

Collecting and integrating data is now a commodity.  What you do with it provides value! 

As an example, applications derived by statistically modeling collections of text samples have vastly improved smart phone and search engine interfaces.

A Swift-Key keyboard predicts the next word as a device user texts or emails on a mobile device. Writing tasks are now easier leveraging predictive technology.  

The data science capstone objective is to create a shiny app that takes an input phrase (multiple words) and predict the next word.  



Tasks Performed to Accomplish Goal
========================================================

A frequency dictionary is created from collected text data (corpus), which is used to predict the next word(s).

- Data Collection & Download
  * Swift key Corpus 
  * Profane words (from http://www.bannedwordlist.com/ )
- Data (Corpus) Cleansing
- Tokenization
- Exploratory Analysis
- Predictive Modeling
- Shiny Application Development


Methods & Models
========================================================

- A corpus sample was cleansed by folding characters to lower case, removing punctuation, numbers, links, white, space, special characters, and profanity.

- N-grams are used to create frequency dictionaries, used by the Shiny application to predict the next word
https://en.wikipedia.org/wiki/N-gram

- The sample was tokenized into Uni-Grams, Bi-Grams, and Tri-Grams.

- Katz's back-off is leveraged to return the most common word matched.
https://en.wikipedia.org/wiki/Katz%27s_back-off_model
 
Finally!
========================================================
 
The **Next Word** shiny app is easy to use! 
- Try the **Next Word** app here: https://dmartin60.shinyapps.io/nextword/
- Enter a partial phrase into the **input text box**
- Predicted Next word(s) are produced under **Next word prediction**
- Can change parameters in the **settings panel** to customize the output
- A milestone report documenting the Exploratory Analysis can be found here: http://rpubs.com/dmartin60/307195
- The shiny application code and related scripts, can be found in this GitHub repo: https://github.com/dmartin60/Capstone


 
