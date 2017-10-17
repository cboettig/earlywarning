##
##  Use:
##
##    docker run -w /earlywarning/inst/examples -d \
##      --name earlywarning cboettig/earlywarning \
##      Rscript -e 'rmarkdown::render("simulation.Rmd")'
##    docker cp earlywarning:/earlywarning/inst/examples/simulation.html .

FROM rocker/verse:3.3.1
MAINTAINER Carl Boettiger cboettig@ucsc.edu

RUN apt-get update && apt-get install -y \
    libgsl0-dev libnlopt-dev \
    && install2.r snow rmpi desolve psych \
    && installGithub.r cboettig/populationdynamics
RUN git clone https://github.com/cboettig/earlywarning.git /home/rstudio/earlywarning
RUN Rscript -e 'devtools::install("/home/rstudio/earlywarning")'


