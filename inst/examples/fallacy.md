

# Code for Prosecutors Fallacy 
`FALSE`
This code is written in the `R` language for statistical computing.  
Population dynamics are simulated using the `populationdynamics` package


```

Error in eval(expr, envir, enclos) : could not find function "citep"

```

 for exact simulations of 
discrete birth-death processes in continuous time using the Gillespie
agorithm 

```

Error in eval(expr, envir, enclos) : could not find function "citep"

```

.  Early warning signals
of variance and autocorrelation, as well as the model-based estimate
of 

```

Error in eval(expr, envir, enclos) : could not find function "citet"

```

 are estimated using the 
`earlywarning` package 

```

Error in parse(text = code[i]) : 2:0: unexpected end of input
1: citep(citation("earlywarning")
  ^

```

.  These
packages can be installed from Github using the `devtools` R package

```r
library(devtools)
install_github("populationdynamics", "cboettig")
install_github("earlywarning", "cboettig")
```

For the individual-based simulation, the population dynamics are given by

<div>
\begin{align}
  \frac{dP(n,t)}{dt} &= b_{n-1} P(n-1,t) + d_{n+1}P(n+1,t) - (b_n+d_n) P(n,t)  \label{master}, \\
    b_n &= \frac{e K n^2}{n^2 + h^2}, \\
    d_n &= e n + a,
\end{align}
</div>

which is provided by the `saddle_node_ibm` model in `populationdynamics`. 

We also consider the discrete time version of the model of 

```

Error in eval(expr, envir, enclos) : could not find function "citet"

```

,

<div>
\begin{equation}
X_{t+1} =     X_t  \exp\left( r \left(1 - \frac{ X_t }{  K } \right) - \frac{ a * X_t ^ {Q - 1} }{s ^ Q + H ^ Q} \right) 
\end{equation}

We will use parameters r = .75, k = 10, a=1.7, H=1, Q = 3.  In this model Q is a parameter that will force the system through a bifurcation point at a = 2.  

For each of the warning signal statistics in question, 
we need to generate the distibution over all replicates
and then over replicates which have been selected conditional 
on having experienced a crash.  

We begin by running the simulation of the process for all replicates.  

Load the required libraries
 



























