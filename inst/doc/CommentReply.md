
<!-- Intro: recap claims -->
[@Boettiger2012c] demonstrated that conditioning on observing a purely stochastic transition from one stable basin to another would tend to generate timeseries patterns that could be mistaken for an early warning signal of a critical transition, or bifurcation, when in fact the transition is due to nothing but chance.  While the goal was to highlight a potential danger in mining historical records for patterns showing sudden shifts when seeking to test early warning techniques, Drake [@Drake2013] draws attention to a potentially more interesting consequence of our analysis.  [@Drake2013] argues that the bias observed could be used to forecast purely stochastic transitions -- a task previously thought to be impossible [See @Lenton2011 for a review of some of this literature].  Unfortunately, I feel this interpretation too generous and must agree with the prevailing opinion that early warning signals for purely stochastic transitions do not exist.  Drake demonstrates how the statistics presented in @Boettiger2012c show an indisputable difference between the timeseries that transition by chance and the much larger population of replicate timeseries driven by the identical mechanism that do not transition by chance.  Consequently, my position takes some explaining.  

<!-- Thesis -->
The crux of the matter lies in a question fundemental to early warning signals at large -- what is meant by having found evidence of an early warning signal?  This may sound like philosophy but is in fact a question of statistics. We can only define this relative to some null distribution or baseline indicating what we might expect when no warning signal is present. This question has so far recieved insufficient attention, empirical and theoretical, throughout the recent literature on the subject [@Boettiger2013].  Drake's analysis presents the choice of baseline or null distribution as the population of all replicates experiencing the identical dynamics.  I argue instead that it the rare trajectory that does experience a truly stochastic transition is indeed statistically distinguishable from 


<!-- numerical example --> 

To illustrate this difference, consider repeating the analysis of @Boettiger2012c using timeseries produced by an OU process, a stochastic differential equation in which there is only a single optimum whose strength is proportional to the displacement,

$$ dX = - \alpha X dt + \sigma dBt $$

Instead of conditioning on trajectories that experience a large deviation, we condition on trajectories that experience a very large deviation.  In Figure 1 we.  This should help illustrate that observations we reported are driven by the large deviation preceding the transition, and are simply evidence of this fact and not of an impending transition per se.  While trajectories already far from the origin are more likely to transition than ones close to the average, such events can be trivially identified by comparing their states to the historical average.  


<!-- Analytic proofs -->

The realization that such large deviations have statistical properties 

.  @Boettiger2012c observes that any trajectory that does manage to escape by chance does so rather quickly, resulting in a path that is highly autocorrelated and marked by a rapid increase in variance as it departs from the mean.  We can be more precise about the expected time for such trajectories to execute their escape from the vicinity of the stable point to the tipping point. 


