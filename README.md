## Statistical Analysis of Network Data with R, 2nd Edition

The new edition of this book provides an easily accessible introduction to the statistical analysis of network data using R. It has been fully revised and can be used as a stand-alone resource in which multiple R packages are used to illustrate how to conduct a wide range of network analyses, from basic manipulation and visualization, to summary and characterization, to modeling of network data. The central package is igraph, which provides extensive capabilities for studying network graphs in R. The new edition of this book includes an overhaul to recent changes in igraph and a new chapter on networked experiments.  (If you are looking for code for the first edition of the book, you may find it here.)

[<img src="https://images.springer.com/sgw/books/medium/978-3-030-44128-9.jpg" alt="" width="200px">](http://www.amazon.com/Statistical-Analysis-Network-Data-Use/dp/1493909827/)

### Where to buy?

* [Springer](https://www.springer.com/gp/book/9783030441289)
* [Amazon](https://www.amazon.com/Statistical-Analysis-Network-Data-Use/dp/3030441288/ref=sr_1_3?dchild=1&keywords=Statistical+Analysis+of+Network+Data+with+R&qid=1592934546&sr=8-3)
* [Bookshop](https://bookshop.org/books/statistical-analysis-of-network-data-with-r-9783030441289/9783030441289)

### The sand package

The `sand` package contains a collection of data sets used in the book.  It also contains all code found in the book, organized by code chunks, and executable in an interactive fashion.  It will be available shortly from CRAN, from where it can be installed with:

```
install.packages("sand")
```
Alternatively, it can be installed directly from here on github with:
```
devtools::install_github("kolaczyk/sand/sand")
```
(Note:  You may need to install `devtools` first if it is not already installed.)

### The code

1. Introduction
2. [Manipulating Network Data](sand/inst/code/chapter2.R)
3. [Visualizing Network Data](sand/inst/code/chapter3.R)
4. [Descriptive Analysis of Network Graph Characteristics](sand/inst/code/chapter4.R)
5. [Mathematical Models for Network Graphs](sand/inst/code/chapter5.R)
6. [Statistical Models for Network Graphs](sand/inst/code/chapter6.R)
7. [Network Topology Inference](sand/inst/code/chapter7.R)
8. [Modeling and Prediction for Processes on Network Graphs](sand/inst/code/chapter8.R)
9. [Analysis of Network Flow Data](sand/inst/code/chapter9.R)
10. [Networked Experiments](sand/inst/code/chapter10.R)
11. [Dynamic Networks](sand/inst/code/chapter11.R)

You can run the code interactively from within R, using the `sand` package,
see `?sand` for the details.

### Feedback

You can leave a comment, or ask a question in our
[issue tracker](https://github.com/kolaczyk/sand/issues).

