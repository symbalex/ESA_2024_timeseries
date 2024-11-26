# Ecological Time Series Analysis and Forecasting in R

### Ecological Society of Australia 2024 Conference

https://esa2024.org.au/

### Nicholas J Clark

#### 13th December, 2024

## WORKSHOP OVERVIEW
Time series analysis and forecasting are standard goals in applied ecology. In this course, you will learn how to wrangle, visualise and explore ecological time series. You will also learn to use the {`mvgam`} package to analyse a diversity of ecological time series to gain useful insights and produce accurate forecasts.

This workshop will cover:

- Introduction to time series and time series visualization
- Generalised LInear Models (GLMs) and hierarchical models (GLMMs)
- Generalized Additive Modela (GAMs) for nonlinear effects and complex random effects
- Dynamic GLMs and Dynamic GAMs
- Multivariate modeling strategies
- Forecasting and forecast evaluation

## TARGET AUDIENCE AND ASSUMED BACKGROUND
This workshop is aimed at higher degree and early career ecologists who are interested in making better predictions with their statistical models. The strategies to be covered are extendable well beyond time series and participants will leave this workshop with a better understanding of strategies to capture the types of complex, nonlinear effects that dominate ecological data.

## LEARNING OUTCOMES
1.    Understand how dynamic GLMs and GAMs work to capture both nonlinear covariate effects and temporal dependence
2.    Be able to fit dynamic GLMs and GAMs in R using the {`mvgam`} package
3.    Understand how to critique, visualize and compare fitted dynamic models
4.    Know how to produce forecasts from dynamic models and evaluate their accuracies using probabilistic scoring rules

## COURSE PREPARATION

**Please be sure to have at least version 4.2 &mdash; and preferably version 4.3 or above &mdash; of `R` installed**. Note that `R` and `RStudio` are two different things: it is not sufficient to just update `RStudio`, you also need to update `R` by installing new versions as they are released.

To download `R` go to the [CRAN Download](https://cran.r-project.org/) page and follow the links to download `R` for your operating system:

* [Windows](https://cran.r-project.org/bin/windows/)
* [MacOS X](https://cran.r-project.org/bin/macosx/)
* [Linux](https://cran.r-project.org/bin/linux/)

To check what version of `R` you have installed, you can run

```r
version
```

in `R` and look at the `version.string` entry (or the `major` and `minor` entries).

We will make use of several `R` packages that you'll need to have installed. Prior to the start of the course, please run the following code to update your installed packages and then install the required packages:

```r
# update any installed R packages
update.packages(ask = FALSE, checkBuilt = TRUE)

# packages to install for the course
pkgs <- c("tidyverse", "gratia", "ggplot2",
          "marginaleffects", "janitor", "mvgam")

# install packages
install.packages(pkgs)
```

### INSTALLING AND CHECKING STAN
When working in R, there are two primary interfaces we can use to fit models with Stan (`rstan` and `CmdStan`). Either interface will work, however it is highly recommended that you use the `Cmdstan` backend, with the `{cmdstanr}` interface, rather than using `{rstan}`. More care, however, needs to be taken to ensure you have an up to date version of Stan. **For all `mvgam` functionalities to work properly, please ensure you have at least version 2.29 of Stan installed**. The GitHub development versions of `rstan` and `CmdStan` are currently several versions ahead of this, and both of these development versions are stable. The exact version you have installed can be checked using either `rstan::stan_version()` or `cmdstanr::cmdstan_version()`

Compiling a Stan program requires a modern C++ compiler and the GNU Make build utility (a.k.a. "gmake"). The correct versions of these tools to use will vary by operating system, but unfortunately most standard Windows and MacOS X machines do not come with them installed by default. The first step to installing Stan is to update your C++ toolchain so that you can compile models correctly. [There are detailed instructions by the Stan team on how to ensure you have the correct C++ toolchain to compile models](https://mc-stan.org/docs/cmdstan-guide/installation.html#cpp-toolchain), so please refer to those and follow the steps that are relevant to your own machine. Once you have the correct C++ toolchain, you'll need to install `Cmdstan` and the relevant `R` package interface. First install the `R` package by running the following command in a fresh `R` environment:

```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

`{cmdstanr}` requires a working installation of [CmdStan](https://mc-stan.org/users/interfaces/cmdstan.html), the shell interface to Stan. If you don't have CmdStan installed then `{cmdstanr}` can install it for you, assuming you have a suitable C++ toolchain. To double check that your toolchain is set up properly you can call
the `check_cmdstan_toolchain()` function:

```r
check_cmdstan_toolchain()
```

If your toolchain is configured correctly then CmdStan can be installed by calling the
[`install_cmdstan()`](https://mc-stan.org/cmdstanr/reference/install_cmdstan.html) function:

```r
install_cmdstan(cores = 2)
```

You should now be able to follow the remaining instructions on the [Getting Started with CmdStanR page](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) to ensure that Stan models can successfully compile on your machine. A quick way to check this would be to run this script:

```r
library(mvgam)
simdat <- sim_mvgam()
mod <- mvgam(y ~ s(season, bs = 'cc', k = 5) +
               s(time, series, bs = 'fs', k = 8),
             data = simdat$data_train)
```

But issues can sometimes occur when:

1. [you don't have write access to the folders that CmdStan uses to create model executables](https://discourse.mc-stan.org/t/problem-running-cmdstan-on-computing-cluster/34747/5)
2. [you are using a university- or company-imposed syncing system such as One Drive, leading to confusion about where your make file and compilers are located](https://discourse.mc-stan.org/t/system-command-make-failed-models-wont-compile/30528)
3. [you are using a university- or company-imposed firewall that is aggressively deleting the temporary executable files that CmdStan creates when compiling](https://discourse.mc-stan.org/t/trouble-with-cmdstan-toolchain-with-rtools42-on-windows-10-enterprise/28444)

If you run into any of these issues, it is best to consult with your IT department for support.

## WORKSHOP MATERIALS
[Live questions and code sharing](https://docs.google.com/document/d/1xd3icf1wxGxO3SVt2AmKO8CkeKv1QpsxgqK7rR15U08/edit?usp=sharing)
<br>
[Lecture slides](https://nicholasjclark.github.io/ESA_2024_timeseries/ESA_slidedeck.html#1) 
<br>
[Live code example 1](https://github.com/nicholasjclark/ESA_2024_timeseries/blob/main/live_code_examples/casestudy1_kestrel.R)
<br>
[Live code example 1](https://github.com/nicholasjclark/ESA_2024_timeseries/blob/main/live_code_examples/casestudy2_aphids.R)
