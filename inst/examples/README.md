## Dockerfile for earlywarning package

To run any of the analyses from this paper, [install docker]()

For instance, we can run the `simulation.Rmd` script using:

```bash
docker run -w /earlywarning/inst/examples -d \
  --name earlywarning cboettig/earlywarning \
  Rscript -e 'rmarkdown::render("simulation.Rmd")'
```
and simply replace `simulation.Rmd` with `chemostat.Rmd` or `glaciation.Rmd` to 
run the corresponding analysis.


Copy the resulting (stand-alone) html files to the host for viewing:

```bash
docker cp earlywarning:/earlywarning/inst/examples/simulation.html .
```
