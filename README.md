# paper-IEEETLT-orchestrationload

Data analyses, manuscript and (links to) datasets for the article on the usage of eyetracking to track orchestration load (submitted to [IEEE Trasactions on Learning Technologies](https://www.computer.org/web/tlt)).

The project is structured as follows:

* `./analyses` contains the analytical code and reporting of the main results of the studies mentioned in the paper (using [R Markdown](http://rmarkdown.rstudio.com/)). To reproduce the paper's results, after downloading the whole project, open the [./analyses/Prietoetal_OrchestrationLoadEyetracking.Rmd](https://github.com/chili-epfl/paper-IEEETLT-orchestrationload/blob/master/analyses/Prietoetal_OrchestrationLoadEyetracking.Rmd) file in [RStudio](https://www.rstudio.com/), and ['knit it'](http://rmarkdown.rstudio.com/lesson-2.html) (this will in turn download the raw datasets from their public locations, preprocess them and reproduce the analyses and results described in the paper).
* `./manuscript` contains the files for the paper manuscript, following IEEE TLT's official LaTex format. To reproduce the paper itself, please compile the [Prietoetal_OrchestrationLoadEyetracking.tex](https://github.com/chili-epfl/paper-IEEETLT-orchestrationload/blob/master/manuscript/Prietoetal_OrchestrationLoadEyetracking.tex) file.
