
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TagR

<!-- badges: start -->
<!-- badges: end -->

## What is TagR?

TagR is SHARE’s R package for topic tagging using machine learning.
Using predefined topics and a labelled data set, it allows the user to
find the probability that a post belongs to a particular topic.

This package is used by running through the following steps:

-   Pre-process the text data
-   Conduct exploratory data analysis
-   Create document-term matrices
-   Bind any numerical variables to the document-term matrices
-   Find an optimal set of parameters for the Xgboost model for each
    topic
-   Train a model for each topic and predict the probabilities that each
    post in the unlabelled data set belongs to such topic
-   Build an explainer that helps the user visualise how the Xgboost
    model makes its predictions

## Installation

You can install the released version of TagR from
[CRAN](https://CRAN.R-project.org) with:

    install.packages("TagR")

And the development version from [GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("rosepeglershare/TagR")

## Example

This is a basic example which shows you how to solve a common problem:

    library(TagR)
    ## basic example code

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

    summary(cars)
    #>      speed           dist       
    #>  Min.   : 4.0   Min.   :  2.00  
    #>  1st Qu.:12.0   1st Qu.: 26.00  
    #>  Median :15.0   Median : 36.00  
    #>  Mean   :15.4   Mean   : 42.98  
    #>  3rd Qu.:19.0   3rd Qu.: 56.00  
    #>  Max.   :25.0   Max.   :120.00

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<a href="https://github.com/r-lib/actions/tree/master/examples" class="uri">https://github.com/r-lib/actions/tree/master/examples</a>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
