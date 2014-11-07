FROM rocker/hadleyverse

RUN apt-get update && apt-get install -y \
    libgsl0-dev \
    r-cran-snow \
    r-cran-rmpi \
    r-cran-desolve \
    r-cran-psych \
    && installGithub.r cboettig/populationdynamics
RUN git clone https://github.com/cboettig/earlywarning.git
RUN Rscript -e 'devtools::install("/earlywarning")'
RUN chown -R docker:docker /earlywarning

##  Use:
##     docker run -v $(pwd):/earlywarning -w /earlywarning \
        --rm -it cboettig/earlywarning \
        Rscript -e 'rmarkdown::render("simulation.Rmd")'

