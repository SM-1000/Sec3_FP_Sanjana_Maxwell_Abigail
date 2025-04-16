# Stat 184 Final Project--Spring 2025

This repo will serve as the template file for the course project. Further, this README file contains the project guidelines as a checklist, key dates to be aware of, the list of learning outcomes being assessed with this project, and a few suggestions. Be sure to read through all portions of the README carefully.

## Project Purpose

The purpose of this project is to provide you with an opportunity to put into practice *everything* that you have learned over the course of the semester and to push yourselves. To this end, you'll work in teams to conduct your own Exploratory Data Analysis, posing and answering your own research question(s).

### Examples

We've curated a couple of examples of past project that can help you. While their requirements were slightly different than yours, they can still serve as great starting points.

- [IPL Batting Performance Analysis by Roahn and Pratham](https://github.com/Stat184-Fall2024/Sec1_FP_Rohan_Pratham)
- [Recent Customer Shopping Trends by Ava, Sydney and Kacie](https://github.com/Stat184-Fall2024/Sec4_FP_KacieRohn_SydneyHolt_AvaCascario)
- [IMDB Movie Data Analysis by Sanjana, Nishita, and Jess](https://github.com/Stat184-Fall2024/Sec4_FP_SanjanaBandaru_NishitaBhanushali_JesaBurleson)
s

## Key Dates

This project has several key dates that you need to be aware of.

1) Course Project Guidelines Template Repo becomes available--April 16th, 2025
2) Teams get finalized (in class)
  - Sec. 1 & Sec. 3: April 21st, 2025
  - Sec. 2 & Sec. 4: April 23rd, 2025
3) Teams sign-up for their Work-in-Progress Presentation Time Slots (in class)
  - Sec. 1 & Sec. 3: April 23rd, 2025
  - Sec. 3 & Sec. 4: April 25th, 2025
4) Work-in-Progress Presentations will be the week of April 28th--May 2nd, 2025. Everyone is expected to attend and be good audience members.
5) Project Report Due--__Wednesday, May 7th, 2025 by 11:59pm__ via Canvas Submission Portal
6) Self & Peer Evaluations for the Project--__Thursday, May 8th, 2025 by 11:59pm__ MyOpenMath assignment.

## Project Guidelines 

We present the guidelines as a checklist in this file. This means that your team can edit this file and check items off the list by placing an x inside of the square brackets at the start of each item (i.e., [x]). This will help your team keep track of what you've completed and what you have left to do.

- [ ] Read through the Project Guidelines.
- [ ] Find 1 or 2 other individuals from the same section of Stat 184 who you want to work with and form a team. (We will help you form teams if you do not already have one by the team finalization date for your section.)
- [ ] Create a repo using this template repo. Name your repo `Sec#_FP_Name1_Name2_Name3` where `Sec#` is your section of Stat184 (i.e., `Sec1`, `Sec2`, `Sec3`, or `Sec4`) and `Name#` is replaced with the names of each team member.
  - [ ] The owner of the repo should be `Stat184-Spring2025`. 
- [ ] Make sure that each team member has access to the repo.
- [ ] Come up with a topic and set of research questions your team will explore.
- [ ] Make a plan for your work. We suggested putting together your work plan ASAP.
- [ ] Locate appropriate data sources for your project.
  - [ ] Your main data source may __not__ be one that we used in class. If you are interested in using data found in an R package, please consult with your instructor first.
  - [ ] Supplementary data sources may come from anywhere.
- [ ] Read in your data and perform any necessary data tidying, wrangling, and cleaning.
- [ ] Conduct Exploratory Data Analysis.
- [ ] Prepare a reproducible report.
  - [ ] Use a QMD file; the output type is PDF.
  - [ ] The report should be well organized with section headings.
  - [ ] Code should __only__ be found in a Code Appendix at the end for a PDF, not in the body of your report.
    - __TIP!__ The following code chunk can be added to a QMD file and will automatically build a section of code.
````
      ```{r codeAppend, ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE}
      ```
````
  - [ ] Describe the provenance of your data. That is, where did you get the data, who collected the data, for what purpose, who/what make up the cases.
  - [ ] Explain how your data meet the FAIR and/or CARE Principles.
  - [ ] Describe what attributes you'll focus your analysis on (mention if they are part of your data sets or if you created them out of your data sets).
  - [ ] Create multiple data visualizations (tables and figures) that assist both the team and readers in understanding the data.
    - [ ] Data visualizations should show a variety of your skills and geometries.
    - [ ] __Optional__: If your explorations and data make sense to do so, try creating a map.
    - [ ] Data visualizations should be appropriately sized--not too small and not too big.
    - [ ] Figures and Tables should have appropriate captions and appropriately cross-referenced in the body of your report.
    - [ ] Your team must produce at least one table that is *not* a display of raw data or a listing of data cases or columns.
    - [ ] Your team must produce at least one plot/graph.
    - [ ] There should be narrative text helping readers to better understand what each visualization helps them to learn about the data and context.
  - [ ] Your report should contain narrative text (beyond explaining tables and figures) that explains the overall data story or context and helps the reader make sense of what is going on. That is, Take the reader on a journey.
  - [ ] __Optional:__ For those who want to challenge themselves further, feel free to include a section on using other statistical methods such as hypothesis testing, regression, ANOVA, or machine learning--see Chapter 18 of the Data Computing eBook. Keep in mind that this will not grant your group any extra credit.
  - [ ]  You should properly cite any work you reference (including data) according to your choice of citation style. We've included files for APA7 and MLA9 as part of this template. If you want to use a different citation style, you will need download the CSL file from the [Zotero Style Respository](https://www.zotero.org/styles/) and include it in your team's repo.
    - __Fall Back Plan__ You can also put your citations as footnotes and make your own reference list at the end of your document.
- [ ] Use GitHub to share changes and edits; there should be multiple commits to your repo.
  - [ ] Each team should update the included README file to match their project.
  - [ ] Each team member must have at least two (2) commits in the repo's history.
  - [ ] There should be a Main branch and at least one development ("dev") branch for the repo.
  - [ ] There should be at must be at least one Pull Request that resulted in a successful merging of two branches.
  - [ ] One team member should create the Pull Request while a *different* team member should review and complete the Pull Request.
  - [ ] Ensure that your instructor has access to your repo.
  - [ ] __Optional:__ Use the repo's Issues system to track and address any problems, core tasks, etc. 
- [ ] All code should be written according to a Style Guide of your choice. List this Style Guide as a code comment in your first code chunk.
  - __Possible Style Guides:__ There are several different coding Style Guides you can follow; here are a few. [The BOAST Style Guide](https://educationshinyappteam.github.io/Style_Guide/coding.html), [The Tidyverse Style Guide](https://style.tidyverse.org/), [Google's R Style Guide](https://google.github.io/styleguide/Rguide.html)
- [ ] Sign up for your Work-in-Progress Presentation
- [ ] Complete your Work-in-Progress Presentation (details below)
- [ ] Finalize your work and submit your report as a __PDF__ to the appropriate submission portal in Canvas by the deadline.
  - [ ] As a comment on your submission, include a link to your team's project repo.
- [ ] Each team member must complete their Peer & Self Evaluations for working together on this project. This will be an assignment in MyOpenMath and needs to be completed by the deadline.

## Work-in-Progress Presentations

In the last week of classes (April 28th--May 2nd, 2025), each group will give a __short/brief__ presentation to the class (<mark>approximately 3-5 minutes</mark>). In the presentation, each team will share 1) what they are exploring, 2) __one (1)__ insight they have had so far, and 3) __one (1)__ challenge they have encountered as they work (if any). As the name suggests, these presentations are not for teams to present polished and completed reports but instead to give a snapshot of what they are currently doing to help inspire other teams.

## Learning Objectives and Outcomes Assessed
This project is meant to provide one the last opportunities for each student to demonstrate their growth and development. In particular, this project provides us with data on the following learning objectives and outcomes.

+ Computational Thinking: Students will develop ways of thinking that make use of R’s computing power to solve problems.
  + CT.1: The student will learn to use statistical software (R) to solve problems. 
  + CT.2: The student will learn to apply the PCIP System to their work.
  + CT.3: The student will learn to plan their functions and code out in advance of writing syntax. 
  + CT.4: The student will learn to check code for effectiveness and issues. 
+ Programming: Students will develop their skills in programming (creating code) with statistical software.
  + Prog. 3: The student will learn to apply the core programming principles to their work. 
  + Prog. 4: The student will learn to write code that works for achieving their goals. 
  + Prog. 5: The student will learn to organize their code to assist with the code’s readability. 
  + Prog. 6: The student will learn to implement a coding style for their code.
+ Data Analysis: Students will develop their skills in using statistical software to engage in data analysis.
  + DA.1: The student will learn to import files into R from a variety of sources and file types.
  + DA.2: The student will learn to wrangle (clean, prepare) data for further analysis.
  + DA.5: The student will learn to create data visualizations that support data analysis.
  + DA.6: The student will learn to generate the values of descriptive statistics that support data analysis. 
+ Reproducibility: Students will develop their skills in creating reproducible code and data analyses.
  + Repro.2: The student will learn to apply the principles of Open Science to their work. 
  + Repro.3: The student will learn to assess whether data collections meet the FAIR/CARE principles.
  + Repro.4: The student will learn to utilize version control tools as a means of creating reproducible and collaborative work. 
  + Repro.5: The student will learn to create reproducible and reusable code. 
  + Repro.6: The student will learn to create reproducible analysis reports. 
+ Communication: Students will develop their communication skills as related to statistical programing and data analysis.
  + Comm.1. The student will learn to generate documentation for their code that not only they, but others can use to help make sense of the code.
  + Comm.3: The student will learn to meaningfully discuss data visualizations (e.g., plots, tables) to support others in their learning about the current context. 
+ Professionalism: Students will develop their professional identity through self-reflection and working with others.
  + Prof.2: The student will learn to attribute coding and data analysis work to different paradigms/perspectives such as Exploratory vs. Confirmatory Data Analysis, Tidyverse vs. Base R, etc.
  + Prof.3: The student will demonstrate that they can work/collaborate effectively with others.
  + Prof.5: The student will demonstrate that they can keep their (professional) commitments through actions such as attending class (on time), engaging with the class, and submitting work in a timely fashion.


## FAQs
+ __I want to work alone. Can I do this project by myself?__
  + No. Statistics and Data Science are deeply collaborative fields and you need to learn how to work effectively in a team setting. As such this project was designed with collaboration and team work as a core component. 
+ __Can we use a past project from another course for this project?__
  + No. Submitted work that you already received a grade in another course is a violation of Academic Integrity.
+ __Can we use a project from another (current) course for this project?__
  + No. There is not enough time for the faculty to meet and negotiate how one project will fully satisify the requirements for the separate courses.
+ __One of the team members works in a research lab. Can we use data from that lab for our project?__
  + YES! Provided you have permission from the lab's Principle Investigator (PI).
+ __Can we use the data from another course's project?__
  + Tentatively, yes. Your team will need to disclose this as part of your data provenance and check with your instructor early on in the process. While you may use the data, you *may not* use the same analysis as what was used in the other course's project. Be prepared to show the other course's project upon instructor request.   
+ __What does a good example of the Final Project look like?__
  - Check out the various projects listed at the end of the Data Computing eBook as well as the example report. Those as well as the activities, especially towards the later parts of the course can help give you a sense of the scope for this project.
+ __Can I use Python (or another language)?__
  - STAT 184 is an R programming course, and the project is intended to evaluate learning objectives of this course so you should mostly be using R and your entire analysis must be self-contained in a single QMD.  However, if you want to do something in the project that we have not learned about in class (using R) and prefer to use Python or some other language for that purpose it's fine to include some Python chunks in your QMD file.
+ __Where can we get ideas for good data sources?__
  - https://www.data.gov/ --home of US government open data initiative.  Similarly, many states & communities have their own open data websites as well if you search the Internet.
  - https://github.com/fivethirtyeight/data --FiveThirtyEight (https://fivethirtyeight.com/) is a media outlet known for doing some excellent data analysis in many of their articles and stories.  Much of their data is shared in a GitHub Repo.
  - https://github.com/nytimes --New York Times GitHub Repo with supporting data from many stories they have published.
  - https://github.com/rfordatascience/tidytuesday --Your primary data set must NOT come from an R package.  You can usually download a CSV of the data hosted by Tidy Tuesday (if needed for your primary--they give you the `read_csv` code) or you can use their R package directly for a secondary source.
  - https://scorenetwork.org/ --The SCORE Network is a national network for developing and disseminating projects and data at the intersection of Sports and Data Science.
  - https://www.kaggle.com/datasets --Kaggle has a variety of data sets available for use. __Caution:__ not all of the data sets on Kaggle are *real* data but rather manufactured data. Do your due diligence.
  - Your instructor also has some data files you might be able to use.
  - Keep in mind that if any team submits a project that looks too similar to work done by someone else it would be an academic integrity violation.  
+ __We're really stuck on what to do. Help?__
  - What shared interests does your team have? Do you all like a particular sport? Do you like a particular type of music? Do you like to play video games?
  - Come talk to your instructor. We can help you come up with an idea.
