--- 
title: "Principal Component Analysis for Data Science (PCA4DS)"
author: "Tomas Aluja-Banet<br>Alain Morineau<br>Gaston Sanchez"
description: "This book will teach you what is Principal Component Analysis and how you can use it for a variety of data analysis purposes: description, exploration, visualization, pre-modeling, dimension reduction, and data compression."
github-repo: gastonstat/pca4ds
site: bookdown::bookdown_site
bibliography: [book.bib]
biblio-style: apalike
link-citations: yes
always_allow_html: yes
documentclass: book
output: bookdown::gitbook
---


# Preface {-}


This book aims to provide an introduction to Principal Component Analysis for
Data Science.

© 1998-2020 Aluja, Morineau, Sanchez. All Rights Reserved.

<!--chapter:end:index.rmd-->


# Terminology {-}

This section will briefly introduce you to the terms that we will use 
throughout this book. 

- __Data table__: rectangular array formed by rows and columns. In each of
the table entries (i.e. intersection of one row and one column), we find a
datum, typically codified in numeric form. We also use the term _data matrix_
as a synonym term. We represent the data table with upper case bold letters
e.g. $\mathbf{X}$, with $n$ rows and $p$ columns.

- __Individual__: an individual is a row of the data table. The word individual
refers to any type of object: person, animal, country, company, planet, etc.

- __Variable__: we use the word "variable" to refer to any column in the data table. For each individual, we observe the same aggregate, the same measure, the same question, etc. Instead of variable we can also talk about "measure", attribute, characteristic, property, etc.

- __Continuous Variable__: we talk about continuous variable when the measurement is quantitative. To be more precise, a variable is continuous when calculating the mean (or average) makes sense.

- __Nominal Variable__: we talk about a nominal variable when its values are names of categories (or qualities). Examples of nominal variables are _civil status_ (e.g. single, married, divorced, widowed), _geographc region_ (e.g. north, south, west, etc). We also use the terms categorical or qualitative variable as synonyms of nominal.

- __Modality__: the modalities or categories are the values taken by a nominal variable. For example the variable _gender_ (typical) has two modalities: male and female. A modalitiy is also known as group, class, or category.

- __Cloud of Points__: on a plane or in a three-dimensional space, the notion of "cloud of points" has to do with how the points are positioned according to a series of coordinates, which are based on a set of orthogonal axes. Given the coordinates of the points, it is easy to calculate distance between them. When we have more than three axes, even though the cloud of points exist, we cannot visualize them.

- __Distance__: the distance between two points in a given cloud coincides with the usual notion of distance (which can be calculated based on the coordinates of the points via the famous Pythogorean theorem).

- __Inertia__: inertia is a borrowed term from Mechanics (in Physics) which is entirely equivalent to the statistical concept of variance. The inertia gives an idea of the spread in a cloud of weighted points. If the individuals have the same weight (same importance), the direction with the larger inertia of the cloud is the direction of its major axis.

- __Center of Gravity__: this is the average point (or central point) in a cloud of weighted points. Therefore, there is an equivalence between the mechanical notion of center of gravity and the statistical notion of average point.

- __Factorial Analysis__: optimal visualization (in a certain sense) of a cloud of points in some multidimensional space.

- __Factorial Axes__: these are the axes, sorted by importance, which we use to visualize the cloud of points in a factorial analysis. They are defined by the directions of larger stretching (inertia) of the cloud of poitns.

- __PCA__: Principal Component(s) Analysis.

- __Active Variables__: set of variables that are used in the computation of the axes for the factorial planes.

- __Supplementary Variables__: set of variables that are NOT used in the computation of the factorial axes, but that can be used as aid in the interpretation of results.

- __Contribution__: measures the participation of an element (e.g. category, variable, frequency, or individual) in the construction of a factorial axis.

- __Squared Cosine__: measures the quality of the projection of an element (e.g. category, variable, frequency, or individual) along a factorial axis.

- __V-test__: measures, in number of standard deviations, the distance between an observed value and its theoretical value under a null hypothesis. This test is used to characterize the axes, the categories, the classes, etc.

<!--chapter:end:00-glossary.Rmd-->


# Basic Elements {#basic}

Principal Component Analysis (PCA) is a statistical technique with a strong
descriptive flavor that can be used to get an approximate visualization
(optimal in a certain sense) of the information contained in a data table.
Simply put, PCA allows us to simultaneously describe the association between 
variables, as well as the ressemblance among individuals. PCA can also be 
regarded to as a dimension reduction technique of quantitative variables, often 
employed as an intermediate step towards a subsequent model building phase.

In this chapter, we describe PCA as an exploratory tool that will allows us
to visualize and gain insight into the structure of a data set.


## Data and Goals

Behind a Principal Component Analysis, the analyst has to deal with several 
continuous variables measured on a number of individuals. The goal is to learn and gain insight about the available data. For instance, a common application of PCA has to do
with building an economic index that measures the economic capacity of a group
of individuals; PCA can also be used to obtain an optimal subset of points
in order to control the polution in a certain geographic region; or to segment
a population in terms of preference evaluations given to a group of similar
products in a certain market.

Often, PCA can be used as an intermediate step in which its outputs will be 
part of a subsequent analysis such as regression, clustering, or 
classification. Likewise, it is also possible to employ PCA as a data 
compression methodology.

The starting point is a data set in which a number of continuous variables
have been measured on a group of individuals. Sometimes, qualitative variables 
may also be present in the data.

The typical convention is to have a data set in a tabular format like in a 
spreadsheet (e.g. rows and columns). Virtually in all cases, the dimensions of 
the table will make it impossible to observe, by simple inspection, which 
individuals are similar, or which variables are measuring similar features 
among the individuals. 
In other words, the association structure of the variables, as well as the 
configuration of the similarities among individuals, remains hidden.





Let's consider a simple example that will allows us to settle the various
concepts underlying a Principal Component Analysis. 
The goal is to compare a given number of cities according to the mean
salary-level of a dozen of occupations. The aim is to contrast the coherence 
of the description against our global economic knowledge.

The data set pertains to the year 1994, and it consists of 51 cities around the world, on which 40 economic variables have been measured. The cities are grouped in 10 regions around the world. 

| Num | Variable                | Description                          |
|:----|:------------------------|:-------------------------------------|
|  1  | `city`                  | Name of the city                     |
|  2  | `region`                | Region of the world                  |
|  3  | `price_index_no_rent`   | Index of prices without renting cost |
|  4  | `price_index_with_rent` | Index of prices with renting cost    |
|  5  | `gross_salaries`        | Index of gross salaries              |
|  6  | `net_salaries`          | Index of net salaries                |
|  7  | `work_hours_year`       | Yearly worked hours                  |
|  8  | `paid_vacations_year`   | Yearly paid vacations                |
|  9  | `gross_buying_power`    | Gross buying power                   |
| 10  | `net_buying_power`      | Net buying power                     |
| 11  | `bread_kg_work_time`    | Worked time to buy 1 kg of bread     |
| 12  | `burger_work_time`      | Worked time to buy a burger          |
| 13  | `food_expenses`         | Food expenses                        |
| 14  | `shopping_basket`       | Cost of shopping basket (groceries)  |
| 15  | `women_apparel`         | Cost of women apparel                |
| 16  | `men_apparel`           | Cost of men apparel                  |
| 17  | `bed4_apt_furnished`    | Cost of 4-bedroom appt. furnished    |
| 18  | `bed3_apt_unfurnished`  | Cost of 3-bedroom appt. unfurnished  |
| 19  | `rent_cost`             | Cost of house rent                   |
| 20  | `home_appliances`       | Cost of home appliances              |
| 21  | `public_transportation` | Public transportation (bus, train, metro) |
| 22  | `taxi`                  | Cost of taxi                         |
| 23  | `car`                   | Cost of car                          |
| 24  | `restaurant`            | Cost of restaurant                   |
| 25  | `hotel_night`           | Cost of one hotel night              |
| 26  | `various_services`      | Cost of various services             |
| 27  | `tax_pct_gross_salary`  | Taxes as percentage of gross salary  |
| 28  | `net_hourly_salary`     | Net hourly salary                    |
| 29  | `teacher`               | Salary of School teacher             |
| 30  | `bus_driver`            | Salary of Bus driver                 |
| 31  | `mechanic`              | Salary of Car mechanic               |
| 32  | `construction_worker`   | Salary of Construction worker        |
| 33  | `metalworker`           | Salary of Metalworker                |
| 34  | `cook_chef`             | Salary of Cook chef                  |
| 35  | `departmental_head`     | Salary of Departmental head          |
| 36  | `engineer`              | Salary of Engineer                   |
| 37  | `bank_clerk`            | Salary of Bank clerk (cashier)       |
| 38  | `executive_secretary`   | Salary of Executive secretary        |
| 39  | `salesperson`           | Salary of Salesperson (sales associate) |
| 40  | `textile_worker`        | Salary of Textile worker             |



In the data table, the rows correspond to the _individuals_, which in this case
have to do with the cities. In turn, the columns correspond to the _variables_ 
which have to do with the characteristics measured on the cities.

<div class="figure" style="text-align: center">
<img src="images/figure-1-1.png" alt="Standard format of a data matrix" width="50%" />
<p class="caption">(\#fig:fig-1-1)Standard format of a data matrix</p>
</div>

Before performing the actual PCA, we should always carry out an exploratory
analysis. This analysis refers to computing summary statistics such as maximum
values, miminum values, range, measures of center, measures of spread, 
looking at the distribution of the variables (e.g. boxplots, histograms), etc.
This preliminary analysis could help us identify outliers, errors, or other
major anomalie in the data that can disturb that analysis and make the results
worthless.



### Active Variables

The data set of cities and economic variables is relatively small. However, 
the information contained in this data is very rich. There is a wide number
of variables, which is typical of this type of applications. The variables
can be grouped by topics. For instance, there is a series of variables that
correspond to expenses (in clothes, home rent, vehicles, utilities, etc.).
that reflect the cost of living in each city. Other variables involve 
information about the salary, broken down into 12 professions. Likewise, other
variables convey information about the quality of life, such as taxes, 
payed vacations, work days, and so on.

To compare the cities, we can certainly take all the (continuous) variables
and perform a Principal Component Analysis. Notice that this task will lead us
to compare the cities in terms of prices, salaries, taxes, work-hours necessary
to buy a hamburger, etc. The observed differences among cities are difficult 
to interpret; they can have multiple causes, and have values of very different nature.

Instead of selecting all the available variables, it is preferable to select a group of variables, more homogeneous according
to a certain topic, and more aligned with the goals of the analysis. In this sense,
what we call a _topic_ is a group of variables which defines a certain 
standpoint, chosen by the analyst, to compare the cities. In this way, the
interpretation of the proximities among cities will be easier.

<div class="figure" style="text-align: center">
<img src="images/figure-1-2.png" alt="Selection of active variables and supplementary variables" width="50%" />
<p class="caption">(\#fig:fig-1-2)Selection of active variables and supplementary variables</p>
</div>

The chosen variables, called _active variables_, comprise the unique elements
that will be used to compare the cities among them. The rest of the variables 
that are not active are called _supplementary variables_. This does not mean 
that the information of the supplementary variables will not be used. We will
use the supplementary variables as additional information that may help us to
explain the observed (dis)similarities among the cities.

In our example, we will take as active variables the net income, measured in 
dollars, for the 12 selected professions. Two cities will be close to each 
other if the incomes of these 12 professions are very similar, independently
of any other variables that may make them different (e.g. size, prices, 
altitude, etc.). In the following list we provide the 12 available professions:

- `Teacher`
- `Bus driver`
- `Car Mechanic`
- `Construction worker`
- `Metalworker`
- `Cook chef`
- `Factory manager`
- `Engineer`
- `Bank clerk`
- `Executive secretary`
- `Salesperson`
- `Textile worker`

The rest of the variables will be considered supplementary and they will be
employed during the interpretation of the results.


## Analysis of Distances

The results obtained in a PCA will allows us to get a visualization of the
differences among the 51 cities, according to the net salaries of the
chosen professions, as well as a visualization of the global association 
among such professions.

To get such visual displays, we utilize a geometric approximation that is 
fairly simple and intuitive. As a matter of fact, this type of approximation is
the foundation of all component-based exploratory methods, which consists of
regarding a data matrix from the dual point of view of rows and columns. 
Each perspective involves considering a _cloud of points_, one for the rows,
and one for the variables.


### Cloud of Row-Points

We can regard each row of the data table (each city) as one point with 12 
coordinates. Each coordinate corresponding to each of the 12 professions.

If we had only recorded three professions, the values taken by each city would
be located in a three dimensional space, defining a cloud of 51 city-points.
We can generalize this idea to any number of professions. If we consider our
12 selected professions, then the cloud of points will be located in a 
12-dimensional space.

In general, the rows of a data matrix will form a cloud of $n$ points in a 
$p$-dimensional space (as many dimensions as active variables). We call
this cloud the _cloud of row-points_ or simply the _cloud of individuals_.

<div class="figure" style="text-align: center">
<img src="images/figure-1-3.png" alt="Cloud of n Row-points" width="484" />
<p class="caption">(\#fig:fig-1-3)Cloud of n Row-points</p>
</div>


In this cloud, two points that are close to each other will indicate two cities
with similar values in each of the 12 professions. In contrast, two points that
are further apart will indicate two cities with distinct salary levels.

To measure the notion of _proximity_ between row-points (the cities) we need
to define a measure of distance. The most intuitive distance measure is the
__euclidean__ distance between two points given by:

$$
d^2(i, i') = \sum_{j=1}^{p} (x_{ij} - x_{i'j})^2
(\#eq:1)
$$

The main problem with visualizing the distance among the points, resides in 
the high dimensionality of the cloud of points (12 dimensions in our example)
which makes it impossible to our human vision to visualize.


### Cloud of Column-Points

In a similar way to the cloud of row-points, we can geometrically represent 
the $p$ columns of the data matrix in an $n$-dimensional space (one dimension
for each individual). The $n$ coordinates of a column-point are given by the 
$n$ values of the corresponding variable in the data table.

<div class="figure" style="text-align: center">
<img src="images/figure-1-4.png" alt="Cloud of p Column-points" width="484" />
<p class="caption">(\#fig:fig-1-4)Cloud of p Column-points</p>
</div>


The interesting part of this cloud of variable-points lies in the fact that 
it is a representation of the associations between the variables. Each of them
measures an observed characteristic on the cities. Consequently, we can see
which variables measure similar things among the cities. Analogous to the 
distance between cities, we need to define a distance between the variable-points
that captures the intensity and the nature of the association between the 
variables.

Two variable-points that are close to each other, will indicate two variables
that take related values in the entire set of cities: if we know the values
of one variable, we can know the values of the other one.

A very common measure to quantify the association between variables is the
__linear correlation coefficient__. If we used this coefficient as a distance,
then the visualization of the variables becomes a visual display of the 
matrix of correlations among variables.


## How to see the distances between points

Because both types of clouds---row-points and column-points---are located in
high dimensional spaces, we cannot observe them directly. The essence of
Principal Component Analysis involves searching for a plane on which we project
the cloud of points in such a way that the obtained configuration is as close
as possible to the original configuration of the cloud in the high-dimensional
space. We call this plane the _factorial plane_.

The way in which we obtain the desired plane, is by making the overall distances
between projected points as close as possible to the real distances between
points in the space of origin.

Let's consider in first place the cloud of $n$ individual-points located in
the space where each axis corresponds to a variable. The following figure
depicts this idea when we have only three variables.

<div class="figure" style="text-align: center">
<img src="images/figure-1-5.png" alt="Cloud of row-points in first factorial plane" width="90%" />
<p class="caption">(\#fig:fig-1-5)Cloud of row-points in first factorial plane</p>
</div>

The problem consists of finding the factorial plane such that the set of
of all pairs of distances $d_F(i,i')$ between points, is as close as possible
to the real distances $d_X(i,i')$ measured in the space of origin.


###  How to find the projection planes

Our goal has to do with finding a subspace of reduced dimension that conserves
tha maximum of information from the original configuration of the cloud. For
instance, let's pretend that the original cloud has the shape of a mug, like
in the following figure:

<img src="images/mug-shaped-cloud.png" width="30%" style="display: block; margin: auto;" />

Furthermore, let's assume that we can only observe the projection of the mug
on a plane of reduced dimension. The question is: Which plane should we choose?

<div class="figure" style="text-align: center">
<img src="images/figure-1-6.png" alt="Three projections of a mug-shaped cloud points" width="685" />
<p class="caption">(\#fig:fig-1-6)Three projections of a mug-shaped cloud points</p>
</div>

We can consider of projecting this mug cloud over different planes. 
As you can tell, the projection on the plane $H_A$ is much more informative 
that the projection on the plane $H_B$. At least we can see that the figure
on $H_A$ has to do with a lengthened object, and that one of its ends is wider 
than the other end. In contrast, all the points of the projected cloud on 
the plane $H_B$ are confounded, and it does not convey a clear idea the
original cloud, except for the shadow of the handle. However, the best 
projection among the three planes is that of $H_C$.

We obtain the plane $H_C$ by searching for the plane that makes the dispersion
of the projected points as large as possible: 

$$
Max_H \sum_i \sum_{i'} d^{2}_{H} (i, i')
(\#eq:2)
$$

where $H$ represents the subspace of the projection.

Searching for the maximum can be written as:

$$
Max_H \sum_i \sum_{i'} d^{2}_{H} (i, i') = Max_H \left \{ 2n \sum_i d^{2}_{H} (i, G) \right \}
(\#eq:3)
$$

The problem of preserving the projected distances between all pairs of points
becomes a problem of preserving the distances between each point and the center of
gravity $G$.

<div class="figure" style="text-align: center">
<img src="images/figure-1-7.png" alt="Decomposition of the distance between a row-point and the center of gravity" width="60%" />
<p class="caption">(\#fig:fig-1-7)Decomposition of the distance between a row-point and the center of gravity</p>
</div>

The formula in equation \@ref(eq:4) is actually an expression for the Pythagorean theorem. The spread of the cloud of points can be decomposed into two terms: the spread in the projection plane, and another (orthogonal) term given by the sum of the distances of the points to the projection plane:

$$
\sum_{i} d^{2} (i, G) = \sum_i d^{2}_{H} (i, G) + \sum_i d^{2}_{\bar{H}} (i, G)
(\#eq:4)
$$

In this way, the projection plane that guarantees the maximum dispersion between the points, is also the plane that gets as close as possible to the original cloud (in the sense of least squares criterion). This is expressed in the following relation \@ref(eq:5)

$$
Max \hspace{2mm} \sum_{i} d^{2}_{H} (i, G) \quad \Longleftrightarrow \quad Min \hspace{2mm} d^{2}_{\bar{H}} (i, G)
(\#eq:5)
$$



### How to take into account the importance of individuals

Sometimes we may be interested in assigning weights to the individuals based on their relative importance or relevance. When all the individuals have the same importance, we can give a weight equal to $1/n$ to each of them. Thus, the fit criterion becomes:

$$
Max \hspace{2mm} \sum_{i} \frac{1}{n} d^{2}_{H} (i, G) = Max \hspace{2mm} \frac{\sum_i (x_{iH} - \bar{x}_H)^2}{n}
(\#eq:6)
$$

In the general case where each individual has its own weight $p_i$ with $\sum_i p_i = 1$, then the fit criterion is expressed as:

$$
Max \hspace{2mm} \sum_{i} p_i \hspace{1mm} d^{2}_{H} (i, G)
(\#eq:7)
$$

The product of the weight of a point, $p_i$, times the squared of its distance to the center of gravity, $d^{2}_{H} (i, G)$, is known as _inertia of the point_. In this case, the problem involves looking for the projection plane that maximizes the projected inertia.



### Inertia Decomposition

The total inertia is defined as:

$$
I = \sum_{i=1}^{n} p_i \hspace{1mm} d^2(i,G)
(\#eq:8)
$$


The total inertia can be broken down into two additive terms:

- projected inertia on a subspace $H$
- inertia orthogonally projected on a subspace $\bar{H}$

$$
I = I_H + I_{\bar{H}}
(\#eq:9)
$$


The problem of searching for the subspace that makes the dispersion of the projected points as large as possible can also be put in terms of inertias. Namely, we look for a plane $H$ that maximizes the projected inertia (see figure below).

<div class="figure" style="text-align: center">
<img src="images/figure-1-8.png" alt="Successive directions of maximum inertia" width="60%" />
<p class="caption">(\#fig:fig-1-8)Successive directions of maximum inertia</p>
</div>

In order to find the optimal subspace, we begin by looking for a one-dimensional space (i.e. a line) of maximum projected inertia. If all individuals have the same weight, then this first subspace coincides with the direction of maximum stretch of the cloud.

Having found the first one-dimensional subspace, the next step involves finding a two-dimensional subspace (i.e. a plane) with maximum projected inertia. Then, we look for a three-dimensional space, and so on and so forth. At each step, we look for a higher dimensional space such that the projected inertia is as large as possible.

It can be proved that the two-dimensional plane must contain the one-dimensional space (i.e. the line) of maximum projected inertia. Having found the first direction of maximum inertia, one needs to find another line, orthogonal to the first one, such that the plane formed by them has maximum inertia. Analogously, the subspace of three dimensions is formed with the two-dimensional space by adding an orthogonal direction to this plane (see figure \@ref(fig:fig-1-8)).

Because the inertia is additive in orthogonal directions, PCA involves the decomposition of the total inertia in $p$ additive components, as many as the number of dimensions in the original space.

$$
I = I_1 + I_2 + \dots + I_p
(\#eq:10)
$$

Interestingly, the inertias (the dispersions) are decreasingly ordered, which means that subsequent orthogonal directions become smaller, that is, $I_1 \geq I_2 \geq \dots \geq I_p$.

This implies that the configuration of the projected cloud on the first fatorial plane is as close as possible (in the least squares sense) to the original configuration.

For example, the figure below shows the plane of maximum projected inertia for in the data of the Cities.

<div class="figure" style="text-align: center">
<img src="images/figure-1-9.png" alt="Cloud of variable-points in the standardized PCA" width="80%" />
<p class="caption">(\#fig:fig-1-9)Cloud of variable-points in the standardized PCA</p>
</div>

This is the representation of the cities, in a factorial plane, that best resembles the existing distances between the cities according to the salary level. For instance, we can tell that Tokyo, Zurich and Geneva are close to each other, indicating similar salaries among the analyzed professions. The same can be said about Manila, Mumbai, Lagos, and Prague; these cities have similar salaries among them, but they have a contrasting difference with the former group (Tokyo, Zurich, and Geneva).

We can also assume a certain similarity between northen cities such as Stockholm, and Copenhagen, whereas there seems to be a substantial salary difference between Paris and American cities (Los Angeles, Chicago, Toronto, Houston).

The center of the graph represents the average point of the cloud. This corresponds to cities with salaries closer to the mean values.

Notice the difference in scale utilized in the x-axis and the y-axis. Actually, the cloud of points is very extended along the x-axis $F_1$.



### Visualizing association between variables.

Let's discuss what the analysis involves regarding the cloud of variable-points. Recall that the cloud of variable-points is defined by the columns of the data matrix $\mathbf{X}$. Without loss of generality, we will assume that the variables are mean-centered and normalized by the standard deviation.

The first thing to do is to define a measure of distance between variables. One way to represent a certain notion of proximity among variables is given by the following formula:

$$
d^2(j, j') = 2 (1 - cor(j, j'))
(\#eq:11)
$$


If two variables $j$ and $j'$ measure the same thing (in the sense of having a linear correlation of 1) then their points will be overlapped.

In the case where two variables $j$ and $j'$ have a linear correlation equal to -1 (when one increases, the other decreases), the two variable-points will have a maximum distance in opposite directions.

When one variable does not provide any information about the other one, we have a situation when their correlation coefficient will be zero. This would correspond to an intermediate distance in which the variables form an orthogonal angle. These cases are illustrated in figure \@ref(fig:fig-1-10).

<div class="figure" style="text-align: center">
<img src="images/figure-1-10.png" alt="Cloud of variable-points in the standardized PCA" width="70%" />
<p class="caption">(\#fig:fig-1-10)Cloud of variable-points in the standardized PCA</p>
</div>

If the correlation is equal to 1, the squared distance is zero. Likewise, if the correlation is -1 the distance is 4, and if the correlation is zero then the distance is 2. This formulae define the so-called standardized Principal Components Analysis.

It can be shown that all the variable-points are located within a one-unit distance from the origin (inside a hypersphere of radius 1). The cloud of variable-points is defined by a set of vectors that start from the center of the sphere. The correlation coefficient between two variables coindices with the cosine of the angle formed by two corresponding vectors (associated to the variables).

Two variables that are close to each other will form a small angle, which corresponds to a large correlation coefficient. Two independent variables will have a zero correlation coefficient, thus forming a square angle between them. In turn, two opposed variables will have a correlation coefficient close to -1 and thus will appear on two opposite locations in the sphere.

Analogously to the cloud of row-points, we also seek to find a projection plane providing the largest amount of information about the associations between the variables. More precisely, we look for a plane that provides information about the angles between variables, that is, about their correlations.

In the case of the 12 variables observed on the 51 cities, we obtain the configuration on the first factorial plane, displayed in figure \@ref(fig:fig-1-11).

<div class="figure" style="text-align: center">
<img src="images/figure-1-11.png" alt="Circle of correlations on the first factorial plane" width="70%" />
<p class="caption">(\#fig:fig-1-11)Circle of correlations on the first factorial plane</p>
</div>

Notice that in this case all the variables are positively correlated. Moreover, from the circle of correlations, it is possible to observe two groups of variables. One group is formed by `departmental_head`, `engineers`, `bank_clerk`, and `cook_chef`. The other group is formed by the rest of the professions.

In the matrix of correlations (see table \@ref(tab:table-1-3)) we can see the magnitude of the association between the variables. As you can tell, all the correlations are positive and with large value tanging from 0.59 to 0.96.


Table: (\#tab:table-1-3)Matrix of correlations.

        tea    bus    mec    con    met    coo    dep    eng    ban    exe    sal    tex
----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
tea    1.00   0.96   0.84   0.83   0.91   0.75   0.78   0.81   0.82   0.92   0.88   0.88
bus    0.96   1.00   0.89   0.88   0.94   0.76   0.74   0.82   0.80   0.93   0.89   0.92
mec    0.84   0.89   1.00   0.95   0.93   0.80   0.64   0.74   0.70   0.88   0.89   0.89
con    0.83   0.88   0.95   1.00   0.93   0.72   0.59   0.70   0.64   0.86   0.86   0.92
met    0.91   0.94   0.93   0.93   1.00   0.76   0.69   0.80   0.72   0.92   0.88   0.94
coo    0.75   0.76   0.80   0.72   0.76   1.00   0.82   0.82   0.79   0.80   0.85   0.71
dep    0.78   0.74   0.64   0.59   0.69   0.82   1.00   0.87   0.89   0.80   0.79   0.65
eng    0.81   0.82   0.74   0.70   0.80   0.82   0.87   1.00   0.85   0.87   0.85   0.81
ban    0.82   0.80   0.70   0.64   0.72   0.79   0.89   0.85   1.00   0.87   0.85   0.73
exe    0.92   0.93   0.88   0.86   0.92   0.80   0.80   0.87   0.87   1.00   0.94   0.93
sal    0.88   0.89   0.89   0.86   0.88   0.85   0.79   0.85   0.85   0.94   1.00   0.89
tex    0.88   0.92   0.89   0.92   0.94   0.71   0.65   0.81   0.73   0.93   0.89   1.00

The fact that all the variables are positively correlated implies that if one type of salary in a city is high, then the other salaries in that city will also be high.

In later sections of the book we will emphasize the idea that  Principal Component Analysis can be approached from the point of view of the variable-points having a distance based on the correlation between the variables. In other words, a PCA on the row-points is not an independent analysis from a PCA on the variable-points. Quite the opposite in fact, it is possible to obtain the results of a PCA on a set of points (e.g. variables) given the results of the other set (e.g. the rows). This, as we'll see, provides a set of rules extremely valuable in the interpretation of results.



### Normalized PCA or non-normalized PCA?

As we've seen, Principal Component Analysis consists of decomposing the inertia of the original cloud, based on some orthogonal directions. Each of the obtained orthodonal directions accounts for a proportion of the original inertia.

But, what is the contribution of each variable to the original inertia?

The distance between variable-points defined in equation \@ref(eq:11) implies that the contribution, of each variable, to the total inertia is the same for all variables, equal to $1/p$.

The inertia of the cloud of variable-points with respect to the origin coincides with the number of active variables.

$$
I_T = \sum_{j=1}^{p} d^2(j, O) = p
(\#eq:12)
$$

In order for equation \@ref(eq:12) to make sense, we need to use standardized values (mean-centered and unit-variance):

$$
z_{ij} = \frac{x_{ij} - \bar{x}_j}{s_j}
(\#eq:13)
$$

where $\bar{x}_j$ is the mean of variable $j$ and $s_j$ is the corresponding standard deviation. In this case we talk about _Normalized Principal Component Analysis_.

With standardized data, the distance of each variable and the origin is equal to 1:

$$
d^2(j, O) = \sum_{j=1}^{n} \frac{1}{n} \left ( \frac{x_{ij} - \bar{x}_j}{s_j} \right )^2 = \frac{\frac{1}{n} \sum_j (x_{ij} - \bar{x}_j)^2}{s_{j}^{2}} = 1
(\#eq:14)
$$

We should mention that the use of Normalized PCA is not always justified. For example, if a PCA is performed by an analyst working for a bank, if may be more interesting to assign a larger weight to the products that contribute to the volume of the deposits. That is, the importance given to different variables should be done by taking into account the goal of the analysis.

If we use the _raw_ data (i.e. mean-centered only, no scaling by the standard deviation), we could see what the contribution of each variable is to the total inertia. The squared distance of one variable to the origin is given by:

$$
d^2(j,O) = \sum_{j=1}^{n} \frac{1}{n} (x_{ij} - \bar{x}_j)^2 = var(j)
(\#eq:15)
$$

With non-normalized data, the variables are no longer found inside a sphere of radious 1. Instead, the length of the segment (i.e. the vector) of each variable is equal to its standard deviation. We can then think of the cloud of variable-points as a set of vectors, each one of length equal to the standard deviation of the variable, and forming angles defined by the correlation coefficient between the variables.

This type of analysis is called _Non-Normalized Principal Component Analysis_.

With this type of PCA, the distance between two variables depends on the correlation (i.e. the angle between them), as well as on their variances.

<div class="figure" style="text-align: center">
<img src="images/figure-1-12.png" alt="Cloud of variable-points in a non-normalized PCA" width="50%" />
<p class="caption">(\#fig:fig-1-12)Cloud of variable-points in a non-normalized PCA</p>
</div>


The total inertia of the cloud of variable-points is given by the sum of the variances of each variable:

$$
I_T = \sum_{j=1}^{p} d^2(j,O) = \sum_{j=1}^{p} var(j)
(\#eq:16)
$$


The contribution of each variable to the total inertia is given by:

$$
ctr_j = \frac{var(j)}{\sum_j var(j)}
(\#eq:17)
$$

The variance is a function of the unit of measurement in which a variable is measured. This provides a valuable degree of freedom to tune the importance of each variable in the analysis.

In practice, it is preferable to assigne the same importance to all the variables. This is a requirement for when the active variables have different units of measurement (e.g. euros, grams, meters, etc).

In our working example with the data about salaries in various cities, the summary statistics for the active variables are displayed in the following table \@ref(tab:table-1-4)


Table: (\#tab:table-1-4)Summary statistics of active variables.

                       weight       mean      stdev    min     max
--------------------  -------  ---------  ---------  -----  ------
teacher                    51   16801.96   13375.19    600   56800
bus_driver                 51   14311.76   10927.24    400   46100
mechanic                   51   12384.31    8605.61    700   30500
construction_worker        51   10343.14    8321.81    200   28000
metalworker                51   15145.10   10346.23    800   38700
cook_chef                  51   15615.69    8855.67    500   33900
factory_manager            51   30933.33   21462.03   1500   95000
engineer                   51   24664.71   14158.57   1600   59700
bank_clerk                 51   18749.02   13547.30   1200   58800
executive_secretary        51   13311.76    7645.12   1400   31500
salesperson                51    9658.82    6124.87    400   24700
textile_worker             51    9247.06    6493.76    300   23800

From the table above, we see that professions `factory manager` and `engineer` are the ones that have, on average, the higher salaries. Then we have `bank clerk`, `teacher`, `cook chef`, and `metalworker`. And in the last two places there is `salesperson` and `textile worker`, which are considered to be low-skills professions mostly performed by women.

Because all active variables are measured in the same unit (in dollars), we could carry out a non-normalized PCA. However, if we did this type of analysis, it would imply giving more importance to those professions with higher salary. Why? Because these are the variables that have a larger spread. If our goal is to compare the cities by giving the same importance to all professions, then it is more suitable to apply a normalized PCA. This, in turn, let us focus on the matrix of correlations among the active variables.

Interestingly, both cloud of points---individuals and variables---have the same inertia. On one hand, the inertia of the cloud of row-points is the sum of the squared distances between each point and the center of gravity, weighed by the importance of each individual. This inertia can be expressed with respect to each axis in the original space (in which each axis corresponds to a variable):

$$
I_T = \sum_{i=1}^{n} \frac{1}{n} \sum_{j=1}^{p} (x_{ij} - \bar{x}_j)^2 =
\sum_{j=1}^{p} \sum_{i=1}^{n} \frac{1}{n} (x_{ij} - \bar{x}_j)^2 = \sum_{j=1}^{p} var(j)
(\#eq:18)
$$

The variance along a given axis is the spread of the projected coud on that axis. Because the axes are orthogonal, the total inertia is equal to the sum of the variances of the variables. Therefore, the inertias of both clouds are the same.



### Distance Matrices

The rows (i.e. the cities) are located in a space where we measure distance in the classic sense. In a normalized PCA we have:

$$
d^2(i, i') = \sum_{j=1}^{p} \left ( \frac{x_{ij} - x_{i'j}}{s_j} \right )^2
(\#eq:19)
$$


And in the case of a non-normalized PCA:

$$
d^2(i, i') = \sum_{j=1}^{p} (x_{ij} - x_{i'j})^2
(\#eq:20)
$$


In the cloud of variable-points, the distance is defined by the formula \@ref(eq:11) when we are carrying out a normalized PCA. In the case of a non-normalized PCA then distance becomes:

$$
d^2(j, j') = var(j) + var(j') - 2 cov(j,j')
(\#eq:21)
$$


All these distances can be organized in square matrices: an $n \times n$ distance matrix for the distances between individuals, and a $p \times p$ matrix for the distances between variables.

If we don't defined how to measure the distances between points, then the clouds of points remain undefined. In our approximation, we assume that the rows (the cities) are located in a metric (Euclidean) space, which means that the distance is measured by the classic formula of the sum of squared differences:

$$
d^2 (i, i') = \sum_{j=1}^{p} (x_{ij} - x_{i'j})^2 = (x_i - x_{i'}) \mathbf{I} (x_i - x_{i'})
(\#eq:22)
$$


Notice that in this case, calculating the distance between two individuals takes the form of a scalar product, where the metric matrix is the identity matrix. This corresponds to a non-normalized PCA.

In contrast, when we carry out a normalized PCA, the distance between two individuals is measured by the formula:

$$
d^2(i, i') = \sum_{j=1}^{p} \left ( \frac{x_{ij} - x_{i'j}}{s_j} \right )^2 = 
(x_i - x_{i'}) \mathbf{S}^{-2} (x_i - x_{i'})
(\#eq:23)
$$

where:

$$
\mathbf{S}^{-2} = \left(\begin{array}{cccc}
\dots & 0 & 0 & 0  \\
0 & 1/s_{j}^{2} & 0 & 0  \\
0 & 0 & \dots & 0 \\
0 & 0 & 0 & \dots  \\
\end{array}\right)
$$

In this case, the metric matrix is given by $\mathbf{S}^{-2}$ (diagonal matrix of inverses of variable variances). Notice that all euclidean metrics can be expressed in a canonical form by using a change of coordinates. When using $\mathbf{S}^{-2}$ such change of coordinates involves dividing the coordinates of the points by the standard deviation of each variable.

Regarding the cloud of variable-points, the distance between variables is defined based on the correlation between two variables. In general, this distance is given by the scalar product between two vectors:

$$
cor(j,j') = \sum_{i=1}^{n} \frac{1}{n} \left ( \frac{x_{ij} - \bar{x}_j}{s_j} \right ) \left ( \frac{x_{ij'} - \bar{x}_{j'}}{s_{j'}} \right ) = \mathbf{z_{j}^{\mathsf{T}} N z_j}
(\#eq:24)
$$

The natural metric matrix to be used in this case is the diagonal matrix $\mathbf{N}$ of weights for individuals ($1/n$ or $p_i$). Thus, the distance between variables is defined as:

$$
d^2(j,j') = <z_j, z_{j'}> + <z_j, z_{j'}> - 2<z_j, z_{j'}>
(\#eq:25)
$$

which coincides with \@ref(eq:17) when using mean-centered data and standardized (normalized PCA), or with \@ref(eq:16) when using non-centenred data (non-normalized PCA).

<!--chapter:end:01-introduction.Rmd-->


# How Does PCA Work? {#mechanics}

At its heart, performing a Principal Component Analysis involves taking a data table that contains the information about a certain phenomenon, in order to transform such data into a set of visual representations in some optimal sense. During the transformation process part of the information is "lost". However, PCA seeks to minimize this loss of information. There is a tradeoff between the amount of information that is lost, in exchange of gaining understanding and insight. We go from a raw data table to a set of graphical representations that should be easier to understand. In order to be able to _read_ the results and graphics obtained from a PCA, we need to discuss the mechanics of this technique and its underlying rationale.



## Principal Components

Let's consider the cloud of row-points, also known as the cloud of individuals. As we've mentioned, we are interested in decomposing the inertia (i.e. the spread) of this cloud in terms of a series of orthogonal directions.

The first step consists of looking for the most basic type of subspace, namely, a line. Geometrically, a line can be defined by a vector $\mathbf{u}$ of unit norm. Based on the discusion from the previous chapter, we will attempt to define $\mathbf{u}$ in such a way that the projected points on this direction have maximum inertia (see figure \@ref(fig:fig-2-1)). In other words, $\mathbf{u}$ will be defined such that the distances between pairs of projected points are as close as possible to the original distances.


<div class="figure" style="text-align: center">
<img src="images/figure-2-1.png" alt="Projection of a row-point on the direction defined by a unit vector" width="75%" />
<p class="caption">(\#fig:fig-2-1)Projection of a row-point on the direction defined by a unit vector</p>
</div>


The projection (or coordinate) of a row-point on the direction defined by $\mathbf{u}$ is given by:

$$
\psi_i = \sum_{j=1}^{p} (x_{ij} - \bar{x}_j) u_j
(\#eq:2-1)
$$


The inertia (or variance) of all the projected points on $\mathbf{u}$ is then:

$$
\sum_{i=1}^{n} = p_i \hspace{1mm} \psi_{i}^{2} = \lambda
(\#eq:2-2)
$$

The goal is to look for a line $\mathbf{u}$ that maximizes the value $\lambda$.

Let $\mathbf{X}$ be the mean-centered data matrix. Obtaining $\mathbf{u}$ implies diagonalizing the cross-product matrix $\mathbf{X^\mathsf{T} X}$. This matrix is the correlation matrix in a normalized PCA, whereas in a non-normalized PCA this matrix becomes the covariance matrix.

It turns out that the unit vector $\mathbf{u}$ is the eigenvector associated to the largest eigenvalue from diagonalizing $\mathbf{X^\mathsf{T} X}$.

Analogously, the orthogonal direction to $\mathbf{u}$ that maximizes the projected inertia in this new direction corresponds to the eigenvector associated to the second largest eigenvalue from diagonalizing $\mathbf{X^\mathsf{T} X}$. This maximized inertia is equal to the second eigenvalue, so on and so forth.

The eigenvalues provide the projected inertias on each of the desired directions. Moreover, the sum of the eigenvalues is the sum of the inertias on the orthogonal directions, and this sum is equal to the global inertia of the cloud of points.

| Eigenvalues | Eigenvectors   |
|:-----------:|:--------------:|
| $\lambda_1$ | $\mathbf{u_1}$ |
| $\lambda_2$ | $\mathbf{u_2}$ |
|   $\dots$   |   $\dots$      |
| $\lambda_p$ | $\mathbf{u_p}$ |


$$
I_T = \lambda_1 + \lambda_2 + \dots + \lambda_p = \begin{cases}
  0 & \text{in normalized PCA} \\
  \sum_{j=1}^{p} var(j) & \text{in non-normalized PCA}
\end{cases}
(\#eq:2-3)
$$

The eigenvectors give the directions of maximum inertia y we call them factorial axes.

On these directions we project the individuals, obtaining what is called the __principal components__ (see formula \@ref(eq:2-3)). As we can tell, each component is obtained as a linear combination of the original variables:

$$
\boldsymbol{\psi}_{\alpha} = u_1 \mathbf{x_1} + \dots + u_p \mathbf{x_p}
$$

Likewise, each component has a variance equal to its associated eigenvalue:

$$
var(\boldsymbol{\psi}_{\alpha}) = \lambda_\alpha
$$

In summary, a Principal Component Analysis can be seen as a technique in which we go from $p$ original variables $\mathbf{x_j}$, each having an importance given by its variance, into $p$ new variables $\boldsymbol{\psi}_{\alpha}$. These new variables are linear combination of the original variables, and have an importance given by their variance which turns out to be their eigenvalues (see figure \@ref(fig:fig-2-2)).

<div class="figure" style="text-align: center">
<img src="images/figure-2-2.png" alt="Change of basis and dimension reduction" width="75%" />
<p class="caption">(\#fig:fig-2-2)Change of basis and dimension reduction</p>
</div>



### Interpreting the Inertia Proportions

In our working examples with the data about the cities, we obtain the following 12 eigenvalues:


Table: (\#tab:table-2-1)Distribution of eigenvalues.

 num   eigenvalues   percentage   cumulative
----  ------------  -----------  -----------
   1       10.1390        84.49        84.49
   2        0.8612         7.18        91.67
   3        0.3248         2.71        94.37
   4        0.1715         1.43        95.80
   5        0.1484         1.24        97.04
   6        0.0973         0.81        97.85
   7        0.0682         0.57        98.42
   8        0.0525         0.44        98.86
   9        0.0505         0.42        99.28
  10        0.0332         0.28        99.55
  11        0.0309         0.26        99.81
  12        0.0226         0.19       100.00

Notice that we obtain a first principal component that stands out from the rest. 

The column `eigenvalue` provides the explained inertia for each direction. The sum of all of the inertias corresponds to the global inertia of the cloud of cities. Observe that this global inertia is equal to 12, which is the number of variables. Recall that this is property from a normalized PCA.

The column `percentage`, in turn, expresses the porpotion of the explained inertia by each axis. As we can tell from the table, the first direction explains about 85% of the global inertia, which is contained in a 12-dimensional space. Because of the very large value of this principal component, one could be tempted to neglect the rest of the components. However, we'll see that such an attitude is not excempt of risks. This does not imply that the rest of the components are useless or uninteresting. Quite the opposite, they may help reveal systematic patterns of variation in the data.

The last column of table \@ref(tab:table-2-1) provides the cumulative percentage of inertia. With the first three factorial axes we summarize about 95% of the inertia (or spread) of the cloud of points.



### How many axes to retain?

From the previous results, we've seen that with the first principal components, we get to recover or capture most of the spread in the cloud of points. A natural question arises: How many axes should we keep?

This is actually not an easy question, and the truth is that there is no definitive answer. In order to attempt answering this question, we have to consider another inquiry: What will the axes be used for? Let's see some examples.

__Example 1.__ One possibility involves using the axes to obtain a simple graphic representation of the data. In this case, the conventional number of axes to retain is 2, which are used to graph a scatter diagram: say we call these axes $F_1$ and $F_2$. With a third axis, we could even try to get a three-dimensional representation ($F_1$, $F_2$, and $F_3$). Beyond three dimensions, we can't get any visual representations.

Optionally, we could try to look at partial displays of the $p$-dimensional space. For instance, we can get a scatterplot with $F_2$ and $F_3$, and then another scatterplot with $F_1$ and $F_4$. Keep in mind that all these partial views require a considerable "intelectual" effort. Why? Because of the fact that in any of these partial configurations, the distances between points come from compressed spaces in which some directions have dissapeared. If the goal is to simply obtain a two-dimensional visualization, it usually suffices with looking at the first factorial plane ($F_1$ and $F_2$). To look "beyond" this plane, we will use outputs from clustering methods.

__Example 2.__ If the purpose is to keep the factorial axes as an intermediate stage of a clustering procedure, then this changes things drastically. In this situation, we want to retain several axes (so that we get to keep as much of the spread of the original variables). Usually we would discard those directions associated to the smallest eigenvalues. The reason to do this is because such directions typically reflect random fluctuations---"noise"---and not really a signal in the data.

__Example 3.__ If the goal is to use the factorial axes as explanatory variables in a regression model or in a classification model, we will try to keep a reduced number of axes, although not necessarily the first ones. It is certainly possible to find discriminant directions among axes that are not in the first positions.

As you can tell, deciding on the number of axes to retain is not that simple. This is a decision that is also linked to the stability of results.

We recommend not to blindly trust in automatic rules of thumb for deciding the number of directions to be kept. Our experience tells us that it is possible to find stable factorial axes with relatively small eigenvalue.

_Note:_ To decrease the percentage of inertia of each axis, one can add new uncorrelated variables to the data table (i.e. white noise). Doing so should not have an effect on the first factorial axes, which should still be able to capture most of the summarized "information".



### Coordinates of row-points

The table below contains the results about the cities with respect to the first three factorial axes:


Table: (\#tab:table-2-2)Results of the individuals

     city             wgt   disto   coord1   coord2   contr1   contr2   cosqr1   cosqr2
---  -------------  -----  ------  -------  -------  -------  -------  -------  -------
1    AbuDhabi        1.96   27.16     2.17     4.61     0.91    48.42     0.17     0.78
2    Amsterdam       1.96    3.22     1.58    -0.36     0.48     0.29     0.77     0.04
3    Athens          1.96    4.24    -1.91    -0.51     0.71     0.58     0.86     0.06
4    Bangkok         1.96    9.87    -2.97     0.82     1.71     1.53     0.89     0.07
5    Bogota          1.96    7.14    -2.47     0.66     1.18     0.99     0.86     0.06
6    Mumbai          1.96   21.11    -4.56    -0.31     4.03     0.22     0.99     0.00
7    Brussels        1.96    0.74     0.61    -0.17     0.07     0.07     0.50     0.04
8    Budapest        1.96   17.74    -4.19    -0.24     3.39     0.13     0.99     0.00
9    BuenosAires     1.96    5.39    -0.89     1.23     0.15     3.43     0.15     0.28
11   Caracas         1.96   18.07    -4.24    -0.06     3.48     0.01     1.00     0.00
12   Chicago         1.96   23.64     4.42    -1.25     3.77     3.55     0.82     0.07
13   Copenhagen      1.96    7.43     2.37    -0.83     1.09     1.58     0.76     0.09
14   Dublin          1.96    0.79    -0.27    -0.19     0.01     0.08     0.10     0.05
15   Dusseldorf      1.96    8.32     2.72     0.24     1.43     0.13     0.89     0.01
16   Frankfurt       1.96   10.12     3.05     0.63     1.80     0.90     0.92     0.04
17   Geneva          1.96   42.20     6.36    -0.30     7.82     0.21     0.96     0.00
18   Helsinki        1.96    0.49     0.03    -0.51     0.00     0.60     0.00     0.53
19   Hongkong        1.96    3.61    -1.03     0.54     0.21     0.66     0.30     0.08
20   Houston         1.96   15.21     3.45    -0.78     2.30     1.37     0.78     0.04
21   Jakarta         1.96   16.92    -4.08    -0.20     3.22     0.09     0.98     0.00
22   Johannesburg    1.96    4.88    -2.08    -0.02     0.84     0.00     0.88     0.00
24   Lagos           1.96   23.54    -4.81    -0.43     4.47     0.42     0.98     0.01
25   Lisbon          1.96    5.00    -2.17    -0.28     0.91     0.18     0.94     0.02
26   London          1.96    0.76    -0.02    -0.63     0.00     0.91     0.00     0.52
27   LosAngeles      1.96   18.89     3.64    -1.80     2.57     7.39     0.70     0.17
28   Luxembourg      1.96   32.79     5.24     0.69     5.30     1.07     0.84     0.01
29   Madrid          1.96    0.89    -0.06     0.00     0.00     0.00     0.00     0.00
30   Manama          1.96    7.17    -0.82     2.05     0.13     9.60     0.09     0.59
31   Manila          1.96   16.51    -4.05    -0.03     3.17     0.00     0.99     0.00
32   Mexico          1.96    8.63    -2.83    -0.07     1.55     0.01     0.93     0.00
33   Milan           1.96    0.69     0.02    -0.34     0.00     0.26     0.00     0.17
34   Montreal        1.96    5.68     2.17    -0.77     0.91     1.34     0.83     0.10
35   Nairobi         1.96   23.45    -4.82    -0.26     4.50     0.15     0.99     0.00
36   NewYork         1.96   23.01     4.60    -0.30     4.09     0.20     0.92     0.00
37   Nicosia         1.96    3.56    -1.78    -0.27     0.61     0.16     0.89     0.02
38   Oslo            1.96    3.98     1.66    -0.73     0.53     1.21     0.69     0.13
39   Panama          1.96    5.97    -2.22     0.62     0.96     0.88     0.83     0.06
40   Paris           1.96    5.31     1.65     1.41     0.53     4.50     0.51     0.37
41   Prague          1.96   18.69    -4.29    -0.31     3.56     0.22     0.98     0.01
42   RiodeJaneiro    1.96   12.22    -3.40     0.28     2.24     0.18     0.95     0.01
43   SaoPaulo        1.96   10.33    -3.18    -0.01     1.96     0.00     0.98     0.00
44   Seoul           1.96    0.69    -0.61    -0.04     0.07     0.00     0.54     0.00
45   Singapore       1.96    2.64    -1.16    -0.16     0.26     0.06     0.51     0.01
46   Stockholm       1.96    2.16     0.67    -0.98     0.09     2.20     0.21     0.45
47   Sidney          1.96    0.62     0.03    -0.21     0.00     0.10     0.00     0.07
48   Taipei          1.96    6.07     1.64    -0.27     0.52     0.17     0.45     0.01
49   Tel-Aviv        1.96    3.35    -1.41     0.00     0.38     0.00     0.59     0.00
50   Tokyo           1.96   46.73     6.72     0.72     8.72     1.18     0.97     0.01
51   Toronto         1.96    4.86     1.77    -1.06     0.60     2.53     0.64     0.23
52   Vienna          1.96    4.07     1.86    -0.11     0.67     0.03     0.85     0.00
53   Zurich          1.96   65.45     7.90     0.29    12.08     0.20     0.95     0.00


The column `wgt` indicates the weight of each city, which in this case is a uniform weight of 100 / 51 = 1.96.

The column `disto` provides the squared distance of each city to the center of gravity. This column allows us to find which cities are the _typical_ cities (i.e. the closest ones to the center of gravity), such as Helsinki. Likewise, it allows us to identify the _unusual_ or _unique_ cities (i.e. those that have a large distance to the center of gravity) like Zurich or Tokyo. In general, the distance to the center of gravity is a criterion of the _singularity_ of each city.

The third and fourth columns correspond to the coordinates obtained from projecting the cities onto the first two factorial axes. The representation on the first factorial plane is obtained with these coordinateas ($F_1$ and $F_2$), given in figure \@ref(fig:fig-1-9).

It is important to mention that the orientation of a factorial axis is arbitrary: the important trait is the direction. We could change the orientation of an axis by changing the sign of the coordinates on this axis. Graphically, this means that all symmetries are possible. The user has to make the decision of which orientation is the most convenient.



### Interpretation Tools

#### Contributions {-}

The next two columns, `contr1` and `contr2`, provide the _contributions_ (in percentages) of the cities to the explained inertia by each axis. The inertia of an axis is obtained via formula \@ref(eq:2-2). Thus, we can measure the part of an axis' inertia that is due to a given row-point by means of the quotient:

$$
CTR(i, \alpha) = \frac{p_i \psi^{2}_{i \alpha}}{\lambda_{\alpha}} \times 100
(\#eq:2-4)
$$


The above quotient is the contribution of a point $i$ to the construction of axis $\alpha$.

We can use the contributions to identify the cities that contribute the most to the construction of the factorial axes.

If all cities had the same contribution, this would have a value of about 2% (100/51). Consequently, all those cities with contributions greater than 2% can be considered to have influence above the average.

_When is a contribution considered to be "high"?_

The answer to this question is not straightforward. A contribution will be considered "high" when, compared to the rest of contributions, it has an unusual large value.

For example, the city that contributes the most to the second axis is Abu Dhabi (48%). Almost half of the inertia of this axis is due to this city alone. Abu Dhabi is clearly very influent in the construction of this axis. We can actually ask about the stability of this axis, meaning, how much the result would change if Abu Dhabi were to be eliminated?

The next figure \@ref(fig:fig-2-3) shows the cities with a size proportional to their contributions on the first factorial plane (sum of the contributions of the first two axes).

<div class="figure" style="text-align: center">
<img src="images/figure-2-3.png" alt="Contributions of the cities in the first factorial plane" width="80%" />
<p class="caption">(\#fig:fig-2-3)Contributions of the cities in the first factorial plane</p>
</div>

All the active points play a role in the construction of an axis. We can check that the sum of all the contributions in an axis add up to 100.

$$
\sum_{i=1}^{n} CTR(i, \alpha) = 100
(\#eq:2-5)
$$



#### Squared Cosines {-}

The last columns of the table of results, `cosqr1` and `cosqr2`, contain the values of the __squared cosines__. These are used to assess the quality of the obtained factorial configuration when compared to the original configuration of the row-points.

Because the obtained representations are an approximation of the real distances between points, it is expected that some distances between pairs of points will be better represented whereas other distances will not reliably reflect the real distance between two points.

The goal is to have a good idea of how close is a point with respect to the factorial plane. If two points are close to the factorial plane, then the projected distance will be a good approximation to the actual distance in the original space. However, if at least one point is further away from the projection plane, then the real distance can be very different from that represented in the factorial plane.

This proximity to the factorial plane is measured with the squared cosine of each point to the factorial axes. 

<div class="figure" style="text-align: center">
<img src="images/figure-2-4.png" alt="The squared cosine as a measure of proximity" width="60%" />
<p class="caption">(\#fig:fig-2-4)The squared cosine as a measure of proximity</p>
</div>

The figure \@ref(fig:fig-2-4) illustrates the definition given in equation \@ref(eq:2-6)

$$
COS^2(i, \alpha) = \frac{\psi^{2}_{i \alpha}}{d^2(i, G)}
(\#eq:2-6)
$$


A squared cosine of means that the city is on the factorial axis (i.e. the angle $\omega$ is zero), whereas a squared cosine of 0 indicates that the city is on an orthogonal direction to the axis.

Notice that the sum of the squared cosines over all $p$ factorial axes is equal to 1. This has to do with fact that all axes are needed in order to have the exact position of a point in the entire space.

$$
\sum_{\alpha = 1}^{p} COS^2(i, \alpha) = 1
(\#eq:2-7)
$$

Interestingly, the sum of the squared cosines of a given point over the first axes provides, in percentage, the "quality" of representation of the point in the subspace defined by these axes.

_What value of a squared cosine indicates that a point is "well represented" on a factorial plane?_

Similar to the contributions, the answer to the above question is not straightforward. A squared cosine (or the sum on the first two axes of the factorial plane) has to be compared with the rest of the squared cosines in order to determine if it is large or small.

In our working example, the cities are in general well represented on the first factorial plane. The sum of the squared cosines over the first two axes is close to 1. However, cities such as Dublin, Madrid, Sidney or Milan, which are close to the center, are not well represented. In contrast, Mumbai or Caracas are perfectly represented. Figure \@ref(fig:fig-2-5) shows the cities with a size proportional to their squared cosine on the first factorial plane.

<div class="figure" style="text-align: center">
<img src="images/figure-2-5.png" alt="Squared cosines of the cities on the first factorial plane" width="80%" />
<p class="caption">(\#fig:fig-2-5)Squared cosines of the cities on the first factorial plane</p>
</div>

The cities that are deficiently represented in this plane are the "average" cities. We can only interpret the proximity between cities if they are well represented on the factorial plane.



## Projections of Variables

Just like row-points can be represented on a low-dimensional factorial space that preserves as much as possible the original distances, we can do the same with the column-points (i.e. the variables).

Mathematically, working with the column-points implies diagonalizing the cross-product matrix $\mathbf{X X^\mathsf{T}}$.

<div class="figure" style="text-align: center">
<img src="images/figure-2-6.png" alt="Matrices to be diagonalized depending on the type of points" width="75%" />
<p class="caption">(\#fig:fig-2-6)Matrices to be diagonalized depending on the type of points</p>
</div>

Analogously to the row-points, we can obtain the decomposition of the inertia depending on the directions defined by the eigenvalues of the matrix $\mathbf{X X^\mathsf{T}}$. The projected inertia on each direction is equal to its associated eigenvalue.

<div class="figure" style="text-align: center">
<img src="images/figure-2-7.png" alt="Cloud of variables and factorial axes in the space of individuals" width="70%" />
<p class="caption">(\#fig:fig-2-7)Cloud of variables and factorial axes in the space of individuals</p>
</div>

The line of maximum inertia is given by the eigenvector $\mathbf{v}$ (defining the direction $F_1$), associated to the largest eigenvalue. The plane of maximum inertia is formed by adding the line that defines the direction $F_2$. This second direction corresponds to the eigenvector associated to the second largest eigenvalue, and so on.

The representation of the variables on an axis is obtained by projecting the variable-points onto a unit vector $\mathbf{v}$ that defines the direction of the axis.

Let $\varphi_{j \alpha}$ be the coordinate of the $j$ variable on the axis $\alpha$

$$
\varphi_{j \alpha} = \sum_{i=1}^{n} \frac{x_{ij} - \bar{x}_j}{s_j} \hspace{1mm} v_{i \alpha}
(\#eq:2-8)
$$

where $\bar{x}_j$ is the mean of the $j$-th variable.

<div class="figure" style="text-align: center">
<img src="images/figure-2-8.png" alt="Projection of the variables on the first factorial plane" width="85%" />
<p class="caption">(\#fig:fig-2-8)Projection of the variables on the first factorial plane</p>
</div>

The inertia on an axis is given by the sum of the inertias of each variable-point projected onto the axis. In PCA there is not an explicit  weight for the variable-points. However, each variable can play a role more or less important by changing the unit of measurement, which in turn will increase (or decrease) its variance in a non-normalized PCA.

$$
\sum_{j=1}^{p} \varphi^{2}_{j \alpha} = \lambda_{\alpha}
(\#eq:2-9)
$$

Notice that the inertia of the variable-points that are projected onto an axis is the same as the inertia of the row-points projected on the axis of same rank.

Among the factorial axes of both clouds of points, the factorial axes in one space are related to the factorial axes in the other space. Thiese relationships allow us to obtain the directions of one space taking into account the directions of the other space. Such relationships are known as _transition relationships_.

By using the transition relationships, we just need to perform a PCA on one of the spaces (e.g. the rows), and then use these results to derive the results of the other space (e.g. the columns), without having to diagonalize two different cross-products.

In general, we perform the analysis on the cross-product with the smaller dimensions. Usually, this involves working with the matrix $\mathbf{X^\mathsf{T} X}$, assuming that the data matrix has more rows than columns: $n > p$. In this scenario, we obtain the projection of the row-points given in equation \@ref(eq:2-1). The projection of the variables is then calculated from the directions $\mathbf{u}$, which define the factorial axes of the cloud of row-points.

$$
\varphi_{j \alpha} = \sqrt{\lambda_{\alpha}} \hspace{1mm} u_{j \alpha}
(\#eq:2-10)
$$

The above formula allows us to interpret the simultaneous representation of both the cities and the professions.

In our working example, the results about the variables are displayed in table 2.3


Table: (\#tab:table-2-3)Results of the variables

                       coord1   coord2   coord3   cor1    cor2    cor3
--------------------  -------  -------  -------  -----  ------  ------
teacher                  0.94    -0.04    -0.21   0.94   -0.04   -0.21
bus_driver               0.96    -0.13    -0.15   0.96   -0.13   -0.15
mechanic                 0.92    -0.27     0.19   0.92   -0.27    0.19
construction_worker      0.90    -0.37     0.11   0.90   -0.37    0.11
metalworker              0.95    -0.24    -0.02   0.95   -0.24   -0.02
cook_chef                0.87     0.24     0.40   0.87    0.24    0.40
factory_manager          0.84     0.49    -0.01   0.84    0.49   -0.01
engineer                 0.90     0.27    -0.03   0.90    0.27   -0.03
bank_clerk               0.88     0.38    -0.13   0.88    0.38   -0.13
executive_secretary      0.97     0.00    -0.10   0.97    0.00   -0.10
salesperson              0.96     0.01     0.08   0.96    0.01    0.08
textile_worker           0.94    -0.25    -0.10   0.94   -0.25   -0.10


The coordinates of all the variables in the first axis have the same sign, which indicates that the cloud of points is not centered.

Notice also that, in the case of normalized analysis, the coordinate coincides with the correlation of a variable and the principal component (projection of the points onto the factorial axis of same rank):

$$
\varphi_{j \alpha} = cor(\mathbf{x_j}, \boldsymbol{\psi_{\alpha}})
(\#eq:2-11)
$$

This formula plays an important role in the interpretation of the results because it connects the representation of the row-points with the representation of the column-points.

A high correlation implies that the configuration of the individuals on a factorial axis resembles the positions of the individuals on that variable (a unit correlation would indicate that the principal component is a linear function of the variable). A correlation close to zero indicates that there is no _linear_ association between the principal component and the variable.



### Size Effect

As we've described it, the first principal component arises from the high correlation that exists among all the variables, which geometrically forms a very homogeneous array of vectors. This first component approximately corresponds to the bisector of this array of vectors, and will therefore be highly correlated to the original variables.

How can we interpret this phenomenon? Broadly speaking, for any city, if a salary is high in a given profession, then this is also true for the set of professions in that city. This general phenomenon is actually present in the entire data table as a structural pattern, which generates the first factor. And it is for this reason that we call the first component the __size factor__ or __size effect__.


Table: (\#tab:table-2-4)Correlation matrix ordered based on the size factor

        exe    sal    bus    met    tea    tex    mec    eng    con    ban    coo   dep
----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----
exe                                                                                    
sal    0.94                                                                            
bus    0.93   0.89                                                                     
met    0.92   0.88   0.94                                                              
tea    0.92   0.88   0.96   0.91                                                       
tex    0.93   0.89   0.92   0.94   0.88                                                
mec    0.88   0.89   0.89   0.93   0.84   0.89                                         
eng    0.87   0.85   0.82   0.80   0.81   0.81   0.74                                  
con    0.86   0.86   0.88   0.93   0.83   0.92   0.95   0.70                           
ban    0.87   0.85   0.80   0.72   0.82   0.73   0.70   0.85   0.64                    
coo    0.80   0.85   0.76   0.76   0.75   0.71   0.80   0.82   0.72   0.79             
dep    0.80   0.79   0.74   0.69   0.78   0.65   0.64   0.87   0.59   0.89   0.82      


Having a first principal component that captures the _size effect_ is a common phenomenon in PCA. A distinctive trait is that the matrix of correlations between the variables can be arranged based on the correlations with the first principal component. As we can tell from table 2.4, there are high correlations close to the diagonal, and then they decrease as one moves away from this diagonal.


#### Interpretation of the Axes {-}

To better interpret a factorial axis we should take into account the variables that have a relative high correlation with the axis.

We have seen that the first axis is interpreted as a factor of size: differentiating cities according to their overall salary levels. The subsequent principal components comprise factors that are orthogonal to the first one.

The first principal component (projection of the cities onto the first direction of the cloud of row-points) provides an ordination of the cities depending on their salary levels. This first component opposes Swiss cities (Zurich and Geneva) and Tokyo to cities like Mumbai, Manila and Nairobi.

The second factorial axis, showing lower correlations with the original variables, opposes the professions `factory_manager`, `engineer`, `bank_clerk`, and `cook_chef` to `textile_worker`, `construction_worker`, `mechanic`, and `metalworker`. In other words, the second axis has to do with the fact that, independently from the salaries of a city, certain professions have a higher salaries than others.

The projection onto the second axis allows to distinguish cities with similar overall level of salaries: certain cities tend to value the _managerial_ jobs, whereas other cities tend to value the professions less socially appreciated.


### Tools for Interpreting Components

#### Cosine Squares {-}

In a similar fashion to the analysis of the individuals (i.e. the cities), we can also define a set of coefficients, squared cosines, and contributions, that will help us in the interpretation of results in the analysis of the variables.

The squared cosines are defined as the quotient between the projected squared distance on an axis and the squared distance to the origin.

We know that the squared distance of a variable to the origin is equal to its variance:

$$
COS^2(j, \alpha) = \frac{\varphi^{2}_{j \alpha}}{var(j)}
(\#eq:2-12)
$$


The sum of the squared cosines over all the axes is always equal to one:

$$
\sum_{\alpha = 1}^{p} COS^2(j, \alpha) = 1
(\#eq:2-13)
$$

In the normalized PCA, the variances are equal to one; thus the squared cosines will be squared of the coordinates of the variables:

$$
COS^2(j, \alpha) = \varphi_{j \alpha}^{2} \qquad \text{in normalized PCA}
$$

In general, we have that:

$$
COS^2(j, \alpha) = CORR^2(\text{variable}, \text{factor})
$$


#### Contributions {-}

The contribution of each variable to the inertia of an axis is the part of the inertia accounted for the variable. The inertia of an axis (see eq \@ref(eq:2-9)) is expressed as:

$$
\lambda_{\alpha} = \sum_{j=1}^{p} \varphi_{j \alpha}^{2}
$$

The contribution of a variable to the construction of an axis is:

$$
CTR(j, \alpha) = \frac{\varphi_{j \alpha}^{2}}{\lambda_{\alpha}} = \frac{(\sqrt{\lambda_{\alpha}} \hspace{1mm} u_{j \alpha})^2}{\lambda_{\alpha}} = u_{j \alpha}^{2}
(\#eq:2-14)
$$

where $u_{j \alpha}^{2}$ is the coordinate of the former unit axis associated to the variable $j$, projected on the axis $\alpha$.

We have the following result:

$$
CTR(j, \alpha) = (\text{former unit axis})^2
$$

To find how much a variable contributes to the construction of an axis, it suffices to square each component of the vector $\mathbf{u}$. These contributions indicate which variables are responsible for the construction of each axis. The sum of all the contributions to an axis is equal to 1 (or 100 in percentage).

$$
\sum_{j=1}^{p} CTR(j, \alpha) = 100
(\#eq:2-15)
$$

The elements of $\mathbf{u}$ define the linear combinations of the original variables, orthogonal among each other, and of maximum variance (i.e. the principal components). For example, the linear combination of the first component is:

$$
\boldsymbol{\psi_1} = 0.30 \texttt{ teacher} + 0.30 \texttt{ bus_driver} + 0.29 \texttt{ mechanic} \\
+ 0.28 \texttt{ construction_worker} + 0.30 \texttt{ metalworker} + 0.27 \texttt{ cook_chef} + \\
+0.26 \texttt{ factory_manager} + 0.28 \texttt{ engineer} + 0.28 \texttt{ bank_clerk} + \\
+ 0.31 \texttt{ executive_secretary} + 0.30 \texttt{ salesperson} + 0.29 \texttt{ textile_worker}
$$

where each variable has been mean-centered and standardized (because we have performed a normalized PCA).

The first component is therefore defined by a set of coefficients, very similar to each other. In this particular case, the first component is not that different from the average of all the profession salaries.

The set of components $u_{j \alpha}$ also define the projection of the former unit axes onto the new factorial axes.



## Beyond the First Factor {#size-factor}

The most striking fact from the results discussed so far is the so-called _size factor_ (or size effect) which is extremely dominant in our working example. This size factor, reflected in the first principal component, exclusively shows the salary inequality among the cities. The rest of the factors are somewhat "squeezed" by the strength of this phenomenon in the data table.

When a size factor dominates the analysis, it is interesting to rerun a Principal Component Analysis but controlling (eliminating) for the size effect. This involves taking into account the previous knowledge about the salaries in the cities in order to go beyond their salary inequality.

A simple way to achieve this consists of dividing the salaries of each profession by the mean salary in each city, thus eliminating the aforementioned effect of salary inequality. The mean salary of a city can be obtained as the average of the salaries in that city. However, we have decided to obtain the mean salaries of cities in a slightly different form: as the product of the _net hourly salary_ times the _number of hours worked per year_ (i.e. the product of columns `net_hourly_salary` and `work_hours_year`).

At the same time, we have decided to create two new artificial variables (for illustrative purposes). One indicates the "salary inequality" of each city: as the difference between its higher salary and its lower salary, with respect to mean salary of the city:

$$
\text{salary inequality} = \frac{\text{salary}_{max} - \text{salary}_{min}}{\text{mean salary of the city}}
$$

Another variable about the salary of the manual jobs with a certain qualification, is obtained as the mean salary of `mechanic` and `metalworker`:

$$
\text{qualified manual jobs} = \frac{\texttt{mechanic} + \texttt{metalworker}}{2 \times \text{mean salary of the city}}
$$

The new data table contains 12 new variables of salaries (quotient between the salary of a profession relative to the mean salary of the city). We can then perform a Principal Component Analysis on this new table. The table 2.5 contains the summary statistics of these new 12 active variables:


Table: (\#tab:table-2-5)Summary Statistics of the second analysis

                        weight   mean   stdev    min    max
---------------------  -------  -----  ------  -----  -----
teacher2                    51   1.19    0.37   0.37   2.07
bus_driver2                 51   1.04    0.25   0.46   1.69
mechanic2                   51   0.96    0.24   0.14   1.47
construction_worker2        51   0.72    0.27   0.13   1.17
metalworker2                51   1.17    0.22   0.50   1.88
cook_chef2                  51   1.40    0.62   0.48   3.50
factory_manager2            51   2.63    1.32   1.37   6.96
engineer2                   51   2.12    0.76   1.14   4.37
bank_clerk2                 51   1.51    0.62   0.77   3.50
executive_secretary2        51   1.13    0.28   0.76   2.07
salesperson2                51   0.76    0.16   0.37   1.13
textile_worker2             51   0.68    0.18   0.22   1.09

<br>

We can see that the best paid profession is `factory_manager` which, in average, is two and half times above the mean salary. In contrast, the worst paid profession if `textile_worker` with a salary of 2/3 the mean salary.

When looking at the matrix of correlations (see table \@ref(tab:table-2-6)) we now have values that are very different from table \@ref(tab:table-1-3). These new correlations are actually partial correlations, because we have removed the effect that is due to the salary level per city.


Table: (\#tab:table-2-6)Correlations

         tea2    bus2    mec2    con2    met2    coo2    dep2    eng2    ban2    exe2   sal2   tex2
-----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  -----  -----
tea2     1.00                                                                                      
bus2     0.38    1.00                                                                              
mec2    -0.33    0.00    1.00                                                                      
con2     0.24    0.34    0.31    1.00                                                              
met2     0.06    0.14    0.15    0.08    1.00                                                      
coo2    -0.12   -0.42   -0.26   -0.51   -0.38    1.00                                              
dep2    -0.12   -0.24   -0.41   -0.63   -0.19    0.37    1.00                                      
eng2    -0.15   -0.40   -0.35   -0.72   -0.16    0.52    0.57    1.00                              
ban2    -0.08   -0.16   -0.38   -0.37   -0.51    0.32    0.59    0.38    1.00                      
exe2    -0.39   -0.50   -0.37   -0.62   -0.33    0.48    0.38    0.54    0.42    1.00              
sal2    -0.02   -0.07   -0.09   -0.16   -0.36   -0.03    0.06    0.19    0.10    0.12   1.00       
tex2     0.25    0.38   -0.14    0.41    0.06   -0.51   -0.48   -0.31   -0.35   -0.26   0.05      1

<br>

Observe that we now have negative correlations, indicating that when a profession is well paid in a given city, it is detrimental to other profession (values below 0.28 in absolute value, indicate correlations non-significantly different from zero).

The diagonalization of the correlation matrix provides the eigenvalues displayed in table \@ref(tab:table-2-7):


Table: (\#tab:table-2-7)Distribution of eigenvalues.

 num   eigenvalues   percentage   cumulative
----  ------------  -----------  -----------
   1        4.4910        37.43        37.43
   2        1.7148        14.29        51.72
   3        1.2989        10.82        62.54
   4        1.0396         8.66        71.20
   5        0.8699         7.25        78.45
   6        0.7831         6.53        84.98
   7        0.5309         4.42        89.40
   8        0.3874         3.23        92.63
   9        0.3210         2.67        95.31
  10        0.2561         2.13        97.44
  11        0.2021         1.68        99.12
  12        0.1052         0.88       100.00

<img src="_main_files/figure-html/unnamed-chunk-7-1.png" width="70%" style="display: block; margin: auto;" />

The projection of the new (transformed) variables on the three axes obtained in the second analysis, are contained in the table \@ref(tab:table-2-8) shown below 


Table: (\#tab:table-2-8)Results for the variables in the second analysis

                        coord1   coord2   coord3    cor1    cor2    cor3
---------------------  -------  -------  -------  ------  ------  ------
teacher2                 -0.31     0.69     0.30   -0.31    0.69    0.30
bus_driver2              -0.56     0.47     0.13   -0.56    0.47    0.13
mechanic2                -0.42    -0.72    -0.25   -0.42   -0.72   -0.25
construction_worker2     -0.81     0.04    -0.22   -0.81    0.04   -0.22
metalworker2             -0.42    -0.31     0.69   -0.42   -0.31    0.69
cook_chef2                0.72    -0.08     0.06    0.72   -0.08    0.06
factory_manager2          0.75     0.17     0.28    0.75    0.17    0.28
engineer2                 0.79     0.05     0.15    0.79    0.05    0.15
bank_clerk2               0.66     0.33    -0.11    0.66    0.33   -0.11
executive_secretary2      0.77    -0.07    -0.14    0.77   -0.07   -0.14
salesperson2              0.19     0.26    -0.66    0.19    0.26   -0.66
textile_worker2          -0.57     0.43    -0.19   -0.57    0.43   -0.19

<br>

The first component shows the opposition of `executive_secretary`, `factory_manager`, `engineer`, `cook_chef`, and `bank_clerk` against the rest of the professions, and in particular with `construction_worker`. 

The second axis is formed by the professions `mechanic`, and `metal_worker`, opposed to `teacher`, `bus_driver`, and `textile_worker`.

Having eliminated the size factor in this second analysis, we can say that the first factorial plane provides the structure of association among the professions with a constant global salary, with respect to all the cities (see figure \@ref(fig:fig-2-9)).

<div class="figure" style="text-align: center">
<img src="images/figure-2-9.png" alt="Circle of correlations on the first factorial plane of the second analysis" width="70%" />
<p class="caption">(\#fig:fig-2-9)Circle of correlations on the first factorial plane of the second analysis</p>
</div>

The results regarding the cities are shown in table \@ref(tab:table-2-9)


Table: (\#tab:table-2-9)Results for the cities in the second analysis

     city             wgt   disto   coord1   coord2   contr1   contr2   cosqr1   cosqr2
---  -------------  -----  ------  -------  -------  -------  -------  -------  -------
1    AbuDhabi        1.96   55.78     6.11     2.41    16.28     6.65     0.67     0.10
2    Amsterdam       1.96    6.40    -1.67     1.30     1.22     1.93     0.44     0.26
3    Athens          1.96    6.94    -1.62     0.77     1.15     0.68     0.38     0.09
4    Bangkok         1.96   36.42     4.45    -0.44     8.65     0.22     0.54     0.01
5    Bogota          1.96   19.81     2.85    -1.18     3.56     1.59     0.41     0.07
6    Mumbai          1.96    6.23     0.39     0.27     0.07     0.09     0.02     0.01
7    Brussels        1.96    2.32    -0.92     0.25     0.37     0.07     0.36     0.03
8    Budapest        1.96    2.73    -0.15     0.17     0.01     0.03     0.01     0.01
9    BuenosAires     1.96   38.32     4.21    -0.04     7.76     0.00     0.46     0.00
11   Caracas         1.96   37.27     4.46    -0.03     8.68     0.00     0.53     0.00
12   Chicago         1.96   12.31    -2.61    -0.86     2.98     0.85     0.55     0.06
13   Copenhagen      1.96    5.26    -1.53    -0.85     1.02     0.83     0.44     0.14
14   Dublin          1.96    3.58    -0.85     1.22     0.31     1.70     0.20     0.42
15   Dusseldorf      1.96    4.51    -0.99     1.60     0.42     2.93     0.22     0.57
16   Frankfurt       1.96    3.19    -0.51     1.07     0.11     1.30     0.08     0.36
17   Geneva          1.96    4.86    -1.51     0.98     0.99     1.10     0.47     0.20
18   Helsinki        1.96    3.53    -1.55     0.26     1.05     0.08     0.68     0.02
19   Hongkong        1.96   28.00     0.64     3.40     0.18    13.18     0.01     0.41
20   Houston         1.96    5.56    -1.59    -0.80     1.11     0.74     0.46     0.12
21   Jakarta         1.96   23.97     0.32    -4.12     0.05    19.36     0.00     0.71
22   Johannesburg    1.96    8.74     0.07    -1.51     0.00     2.61     0.00     0.26
24   Lagos           1.96   11.51    -0.84    -1.86     0.31     3.95     0.06     0.30
25   Lisbon          1.96    3.02    -0.65     0.31     0.19     0.11     0.14     0.03
26   London          1.96    5.22    -1.81    -0.28     1.44     0.09     0.63     0.01
27   LosAngeles      1.96   14.72    -3.01    -0.24     3.95     0.07     0.61     0.00
28   Luxembourg      1.96   15.85    -1.12     2.89     0.54     9.56     0.08     0.53
29   Madrid          1.96    1.99    -0.07     0.05     0.00     0.00     0.00     0.00
30   Manama          1.96   30.51     4.09     0.75     7.32     0.64     0.55     0.02
31   Manila          1.96    7.96     1.95     0.35     1.65     0.14     0.48     0.02
32   Mexico          1.96   17.53     0.42    -1.71     0.08     3.35     0.01     0.17
33   Milan           1.96    4.62    -1.20     0.44     0.63     0.23     0.31     0.04
34   Montreal        1.96    4.21    -1.75    -0.39     1.34     0.17     0.73     0.04
35   Nairobi         1.96   39.30     4.67    -2.27     9.50     5.89     0.55     0.13
36   NewYork         1.96    3.79    -1.51    -0.17     0.99     0.03     0.60     0.01
37   Nicosia         1.96    5.49    -1.20     0.80     0.63     0.73     0.26     0.12
38   Oslo            1.96    4.79    -1.54    -0.14     1.04     0.02     0.50     0.00
39   Panama          1.96   17.66     2.32     0.90     2.36     0.93     0.31     0.05
40   Paris           1.96    5.14     0.81     1.28     0.29     1.86     0.13     0.32
41   Prague          1.96    7.22    -0.60    -1.40     0.16     2.23     0.05     0.27
42   RiodeJaneiro    1.96   25.04     3.07     0.28     4.11     0.09     0.38     0.00
43   SaoPaulo        1.96    9.06     0.95    -2.00     0.40     4.58     0.10     0.44
44   Seoul           1.96    2.24    -0.79     0.02     0.28     0.00     0.28     0.00
45   Singapore       1.96   14.77    -0.28    -1.14     0.03     1.48     0.01     0.09
46   Stockholm       1.96    8.11    -1.90     0.29     1.57     0.10     0.44     0.01
47   Sidney          1.96    3.59    -0.66     0.40     0.19     0.19     0.12     0.05
48   Taipei          1.96    7.90    -1.40    -1.13     0.86     1.46     0.25     0.16
49   Tel-Aviv        1.96    6.51    -0.28    -1.53     0.03     2.68     0.01     0.36
50   Tokyo           1.96    2.50    -0.84     0.38     0.31     0.17     0.28     0.06
51   Toronto         1.96    7.00    -2.38    -0.24     2.47     0.06     0.81     0.01
52   Vienna          1.96    1.93    -1.02    -0.21     0.45     0.05     0.54     0.02
53   Zurich          1.96    7.08    -1.46     1.68     0.92     3.21     0.30     0.40

<br>

There are few cities farther apart from the center of gravity: Abu Dhabi, Nairobi, and Buenos Aires. Figure \@ref(fig:fig-2-10) shows the factorial plane. On the left side we find the cities with salaries of "managerial" professions _relatively_ higher. To the right of the graph we find cities that could be labeled as more "egalitarian" (in terms of their salaries).

<div class="figure" style="text-align: center">
<img src="images/figure-2-10.png" alt="Projection of the individuals in the first factorial plane" width="80%" />
<p class="caption">(\#fig:fig-2-10)Projection of the individuals in the first factorial plane</p>
</div>

In the lower half of the graph we find cities that provide more value to the qualified manual jobs, whereas the upper half of the graph contains cities that tend to provide more value to the managerial and services types of jobs.

All these results suggest that the salaries can be explained from the level of salaries of each city (first factor from first analysis), the degree of "egalitarianism" between professions and the orientation of the types of jobs (manual jobs versus service jobs).



## Using Supplementary Elements

In section 1.1 we described the data set containing 51 cities on which 40 economic variables have been measured. Until now we have performed a couple of Principal Component Analysis using only the so-called active variables (i.e. the variables about the salaries of 12 professions). However, the data table contains additional variables that can be taken into account in order to enrich our analysis.


### Continuous Supplementary Variables

The continuous supplementary variables can be positioned in the factorial spcaes using the same formulas applied to the active variables.

Within a normalized PCA, we use the correlation of a supplementary variable $\mathbf{x^{+}_{j}}$ with the principal components $\boldsymbol{\psi_{\alpha}}$

$$
\phi^{+}_{j \alpha} = cor(\mathbf{x^{+}_{j}}, \boldsymbol{\psi_{\alpha}})
(\#eq:2-16)
$$

(the superindex + indicates that this is a supplementary variable)

With a non-normalized PCA, we just need to multiply the correlation by the standard deviation of the supplementary variable:

$$
\phi^{+}_{j \alpha} = s_j \hspace{1mm} cor(\mathbf{x^{+}_{j}}, \boldsymbol{\psi_{\alpha}})
(\#eq:2-17)
$$

The position of the supplementary variables with respect to the factorial axes is interpreted in the same way as with the active variables.

_The position of a supplementary variable in a factorial plane allows us to visualize the relationship of the variable with the set of active variables via the factorial axes._

Notice that we have not defined a distance between two supplementary variables. The relative positions between two supplementary variables does not imply any correlation between them. However, as long as the supplementary variables are well represented on the first factorial plane, and close to each other, we can expect that the similarity of their correlations with the axes (similarity of their coordinates) is a consequence of a strong correlation between them.


#### Visualized Regression {-}

The position of a continuous supplementary variable in a factorial plane resembles that of a "visual regression". From this point of view, the supplementary variable plays the role of response variable. In turn, the projection subspace (first factorial planes) play the role of explanatory variables. This analogy is depicted in figure \@ref(fig:fig-2-11)

<div class="figure" style="text-align: center">
<img src="images/figure-2-11.png" alt="Equivalence between a regression and the projection of supplementary" width="80%" />
<p class="caption">(\#fig:fig-2-11)Equivalence between a regression and the projection of supplementary</p>
</div>

In a regression, we are mostly interested in the value of the coefficients, and we care about whether the explanatory variables allows us to predict the response variable $\mathbf{y}$.

In a PCA, there is usually a considerable number of variables of type $\mathbf{y}$. Their projections onto the first factorial plane indicate, in a quick way, which are well (or not) related with the set of active variables. On the other hand, their positions with respect the axes provide what we call interpretation elements of the axes.


#### Quality of Representation for Supplementary Variables {-}

To compute the quality of representation for the supplementary variables we calculate the squared cosines of each supplementary variable with the different factorial axes. Keep in mind that the overall sum of the squared cosines on the $p$ axes will (in general) be less than one.

$$
COS^2 (j^{+}, \alpha) = cor^2 (\text{variable}^{+}, \text{factor})
$$

To get the location of a supplementary variable in the original space, we need to know its $n$ elements (its values for the $n$ individuals). This is analogous to an active variable, except that the set of active variables is found in a subset of dimension $p$ (the rank of $\mathbf{X}$, or the rank of $\mathbf{X^\mathsf{T} X}$). The coordinates on the $p$ factorial axes allow to locate any active variable. This property is not present for supplementary variables.

It doesn't make sense to calculate the contributions of the supplementary variables to the inertia of the axes, because these variables have not intervened in its construction.

In the second analysis of the cities, we have decided to treat the following variables as supplementary variables: the 12 active variables used in the first PCA, the rest of the 16 continuous variables, as well as the variables derived in the 2nd analysis, namely, `salary_inequality` and `manual_qualified`. In addition, we have also decided to consider the five axes obtained in the first analysis as supplementary variables; we do this to study the relationship of both PCA analyses.


Table: (\#tab:table-2-10)Results of the continuous supplementary variables

                         Dim.1   Dim.2   Dim.3
----------------------  ------  ------  ------
price_index_no_rent      -0.35    0.22   -0.09
price_index_with_rent    -0.31    0.26    0.05
gross_salaries           -0.61    0.37    0.02
net_salaries             -0.58    0.39    0.05
work_hours_year           0.46   -0.09    0.18
paid_vacations_year       0.10    0.34   -0.11
gross_buying_power       -0.67    0.37    0.03
net_buying_power         -0.62    0.39    0.07
bread_kg_work_time        0.42   -0.28   -0.15
burger_work_time          0.22   -0.33   -0.26
food_expenses            -0.29    0.15   -0.10
shopping_basket          -0.35    0.21   -0.09
women_apparel            -0.13    0.14   -0.05
men_apparel              -0.11    0.21   -0.16
bed4_apt_furnished        0.08    0.21    0.37
bed3_apt_unfurnished      0.06    0.08    0.45
rent_cost                -0.28    0.33    0.30
home_appliances           0.12   -0.15   -0.16
public_transportation    -0.58    0.31   -0.12
taxi                     -0.49    0.26   -0.12
car                      -0.10   -0.13    0.38
restaurant               -0.22    0.12    0.28
hotel_night              -0.18    0.22   -0.03
various_services         -0.37    0.27   -0.04
tax_pct_gross_salary     -0.68    0.17   -0.19
net_hourly_salary        -0.58    0.38    0.05
teacher                  -0.51    0.50    0.18
bus_driver               -0.56    0.43    0.14
mechanic                 -0.62    0.13   -0.01
construction_worker      -0.69    0.21   -0.01
metalworker              -0.63    0.28    0.21
cook_chef                -0.25    0.37    0.02
factory_manager          -0.04    0.52    0.16
engineer                 -0.25    0.51    0.11
bank_clerk               -0.14    0.54   -0.02
executive_secretary      -0.43    0.45    0.03
salesperson              -0.46    0.42   -0.10
textile_worker           -0.63    0.40    0.03
Axis1                    -0.48    0.43    0.07
Axis2                     0.72    0.39    0.03
Axis3                     0.01   -0.37   -0.23
Axis4                     0.01   -0.08    0.03
Axis5                    -0.05    0.01    0.71
salary_inequality         0.87    0.07    0.23
manual_qualified         -0.55   -0.68    0.27

<br>

From the above table, we see that the salaries of the 12 professions, as well as most of the expenses, are negatively correlated with the first dimension. This indicates that the cities with higher salaries tend to remunerate (relatively) less the managerial professions.

Also, the correlations with the variables `price_index_no_rent` and `price_index_with_rent` are a bit smaller than the correlations with the variables `gross_salaries` and `net_salaries`. This has to do with the most expensive cities which have a higher buying capacity, and elevated taxes and social services.

The first axis opposes cities with low salaries that pay relatively well to `factory_manager`, `engineer` and `executive_secretary`, to the cities with higher salaries that pay relatively better those professions that are less socially well considered. This axis can thus be labeled as a _salary inequality_. In fact, the derived variable `salary_inequality` is the most correlated to this axis.

This first axis is correlated to the first axis of the first PCA analysis (i.e. the so-called _size effect_). In other words, the first axis of salary inequality is correlated to the salary level: the higher the level of salary, the less the salary inequality. However, notice that the largest correlation occurs with the second axis from the first analysis. This indicates a rotation: the first axis from the analysis on the ratios corresponds to the second axis from the analysis on the raw data. This is a common phenomenon that occurs in an analysis in which we eliminate the size effect.



### Nominal Supplementary Variables

A categorical variable observed on a set of individuals defines a partition of such individuals into groups; there are as many groups as categories in the variable.

When considering the cloud of row-points, we can distinguish the various groups of individuals for each category. For each group of points we can calculate the _average point_ or center of gravity (see figure \@ref(fig:fig-2-12)).

<div class="figure" style="text-align: center">
<img src="images/figure-2-12.png" alt="Partition of individuals based on a nominal variable" width="80%" />
<p class="caption">(\#fig:fig-2-12)Partition of individuals based on a nominal variable</p>
</div>

The projection of a supplementary categorical variable is the projection of the centroids onto the space of row-points. We obtain as many projected points as categories of the nominal variable.

In our working example, we use the variable `region` as the supplementary categorical variable. In this case we obtain the following representation in the first factorial plane (see figure \@ref(fig:fig-2-13)). Each category groups the cities of a given `region` of the world.

<div class="figure" style="text-align: center">
<img src="images/figure-2-13.png" alt="Regions of the world as supplementary categories (second PCA analysis)" width="80%" />
<p class="caption">(\#fig:fig-2-13)Regions of the world as supplementary categories (second PCA analysis)</p>
</div>

This plot provides a simplified visualization of the cloud of row-points according to the chosen supplementary categorical variable---in this case `region`. The configuration of the category-points allows us to assess certain areas of the graph. This could suggest some elements useful in the interpretation of the factorial directions. For example, the opposition of Europe and North America against the rest of the world regions.


#### Supplementary Category and Supplementary Individual {-}

In summary, a supplementary category is positioned as the average point (i.e. centroid) of the individuals that form such category. Consequently, the definition of a nominal variable with three categories is equivalent to defining three supplementary individuals equal to the center of gravity of the active variables for each category. The supplementary individuals are located in the same factorial plane as the active individuals, with the same rules of interpretation.



### Profiling with V-test

The projection of a category is interpreted as the position of the average individual of the group defined by such category---the centroid. This position can be close to the center of gravity of all the individuals (i.e. the origin of the factorial coordinates).

The proximity to the overall center of gravity suggests that there is little difference between the individuals that have such category and the set of all the individuals.

In contrast, when the projected category is clearly separated from the overall centroid, this indicates that there is a relationship between the active variables and the given category.

It would be interesting to assess what category (i.e group of individuals) seems to indicate an relevant area in the factorial plane.

We can regard the overall center of gravity to be the _center of atraction_ of al the average points of all groups randomly selected. By doing this, we can highlight those centroids that differ "significantly" from the overall centroid. The individuals that form such group will have a high degree of resemblance among them, and therefore will be sufficiently unique to differentiate themselves from the center of gravity.

Suppose that we randomly select a group of $n_j$ individuals from the total of $n$ individuals. The graph of these individuals over the first factorial plane will be a a random scatter plot over this plane.

The average point of these $n_j$ individuals will differ only by the random fluctuations from the overall average represented by the origin of the coordinates (see figure \@ref(fig:fig-2-14)).

<div class="figure" style="text-align: center">
<img src="images/figure-2-14.png" alt="Random selection of a group of individuals" width="75%" />
<p class="caption">(\#fig:fig-2-14)Random selection of a group of individuals</p>
</div>

Suppose that we repeat the random selection of $n_j$ individuals a large number of times. For each repetition we calculate the average point of the selected individuals. We should expect the center of all these groups to coincide with the overall center of gravity.

Now, suppose that a set of $n_k$ individuals having the same category, non-randomly selected, are located in a certain region of the factorial plane (see figure \@ref(fig:fig-2-15)). 

<div class="figure" style="text-align: center">
<img src="images/figure-2-15.png" alt="Group of individuals defined by a certain category" width="75%" />
<p class="caption">(\#fig:fig-2-15)Group of individuals defined by a certain category</p>
</div>

We can calculate the average point of these individuals. Furthermore, we can compute the distance between this average point and the overall centroid. Is the position of this average point compatible with the hypothesis that the individuals have been randomly selected? The more the evidence againts this hypothesis, the more interesting this category will be to profile the region of the factorial plane that it occupies.


#### Interpreting results with the V-test {-}

The idea behind the so-called V-test involves performing a hypothesis test. The null hypothesis $H_0$ consists in the assumption that a set of $k$ individuals are randomly selected, without replacement, from the total of $n$ individuals.

Under the null hypothesis, we calculate the probability of observing a configuration as the one obtained, or more extreme. This is the critical probability associated to $H_0$. The smaller this probability, the less likely is the hypothesis of individuals being randomly selected.

In order to classify the elements in terms of importance, we rank them based on their critical probability. The elements that are most characteristic are those with a smaller critical probability.


The more significant is the difference between the average of the coordinates in group $k$ and the overall centroid, the more interesting the position will be of this group in the factorial plane.

Let $m$ be the average of the coordinates and $s^2$ the empirical variance calculated from the $n$ observations, which will be equal to the eigenvalue of the corresponding axis. Let $m_k$ be the average of the $n_k$ observations in group $k$. We call $M_k$ to the random variable "average of the $k$ extractions." Under the null hypothesis of random selection without replacement from a _finite populatoin_, we have that:

\begin{align*}
E_{H_0} [M_k] &= 0 \\
Var_{H_0} [M_k] &= \frac{n - n_k}{n - 1} \times \frac{\lambda_{\alpha}}{n_k} = s^{2}_{k}
\end{align*}

The average $M_k$ coincides with the average of the coordinates ($\sum_i \psi_{i\alpha} = 0$) and its variance is equal to the variance of the coordinates of the axis $\alpha$ ($\lambda_{\alpha}$) divided by the number of observations from group $k$ and scaled by the factor $(n - n_k) / (n-1)$.

If $n$ and $n_k$ are not very small, the central limit theorem is applicable (even though the extractions are not independent) and in this case the variable:

$$
U = \frac{M_k - m}{s_k}
$$

approximately follows a standard normal distribution.

The critical probability associated to this variable is the probability of a normal distribution of observing a value greater than $u$ calculated on the $n_k$ individuals for the random variable $U$.

We obtain the most characterizing probabilities of an axis, selecting the categories with the smaller critical probabilities. This is equivalent to selecting the categories that have the larger values:

$$
u = \frac{m_k - m}{s_k}
(\#eq:2-18)
$$

The statistic $u$ is what we call the __v-test__. This value expresses, in number of standard deviations, the difference between the average $m_k$ of group $k$, and the overall average $m$.

We interpret this value as follows: the probability of having a difference between both averages is the probability of exceeding this number of standard deviations in a normal distribution.

What we are doing is evaluating some sort of distance between the overall average and the average of a group, measured in terms of standard deviations from a normal distribution. By standardizing these values, we have a common unit that allows us to compare different categories, and rank them according to their importance. In this way, we assess the likelihood of the null hypothesis: that individuals from category $k$ have been randomly selected.

The larger this p-value (in absolute value), the more this indicates that the group of individuals occupies a significant position, and characterizes the region of the factorial plane where they are.

<div class="figure" style="text-align: center">
<img src="images/figure-2-16.png" alt="V-test associated to a critical probability" width="55%" />
<p class="caption">(\#fig:fig-2-16)V-test associated to a critical probability</p>
</div>

In practice, we often use the threshold of 2 standard deviations in order to determine if the variable is significant.

Values larger than 2 indicate less likely value under the null hypothesis of random selection. We can think that these individuals have some kind of relationship with the set of active variables, which makes them have an excentric position in the cloud of individuals.

However, we should take into account the total number of individuals. One could double the data table indefinitly to make the v-test as large as desired.

We must say that the v-test is used as a tool to arrange the categories according to their association with the factorial axes. We don't really use the v-test to formally test a null hypothesis.

In our working examples of the cities, we have a nominal categorical variable: the region of the world in which a city is located. This variable allows us to obtain a simplified representation of the cloud of cities. The results obtained with the first three axes are displayed in \@ref(tab:table-2-11).


Table: (\#tab:table-2-11)V-test of the variable `world region`, used in the second PCA.

                            EFF   PABS   vtest1   vtest2   vtest3
-------------------------  ----  -----  -------  -------  -------
Northern Europe               6      6     -1.9     -0.2      1.3
Central Europe                9      9     -1.4     -3.0     -0.9
Southern Europe               5      5     -1.0     -0.8      0.7
Africa                        3      3      1.1      2.5      0.7
East Asia                     5      5     -0.6     -0.5     -1.4
South Asia and Australia      5      5      1.4      1.3     -1.7
North America                 7      7     -2.4      1.4     -0.2
South America                 6      6      3.6      0.7      1.9
Middle East                   3      3      2.8     -0.7     -0.5
Eastern Europe                2      2     -0.3      0.7      0.5

The first column, named `EFF`, provides the effective of each category (total number of cities of each category). The second column, named `PABS` provides the weight (sum of the weights of all the cities in a given category). When we have uniform weights, the weight and the effective are the same. The first category is formed by six cities of Northern Europe.

The v-test controlled, for each axis, the  hypothesis of random distribution for these 6 cities among the 51 cities. In the first axis, for example, we see a significant opposition of North America with respect to South America and Middle East.

The second axis separates with significant v-tests the cities of Central Europe with the cities in Africa.



### Axes Characterization using Continuous Variables

We've seen that on each axis we have the projection of the active continuous variables, of the supplementary continuous variables, of the individuals, as well as the categories of supplementary qualitative variables.

In order to interpret the axes we should pay attention to the projected elements in their extremes. A first quick approximation to characterize the axes involves listing the projected elements on their more extreme positions (with coordinates further from the origin). To sort the categories we can use the larger v-tests on each axis.

With our working example about the international cities, the continuous variables---both active and supplementary---are arranged based on their correlation displayed in table \@ref(tab:table-2-12).


Table: (\#tab:table-2-12)Characterization of the first axis from second PCA.

                       scale        type             coord   weight       mean     stdev   number
---------------------  -----------  --------------  ------  -------  ---------  --------  -------
construction_worker2   continuous   active           -0.81       51       0.72      0.27        1
engineer2              continuous   active            0.79       51       2.12      0.75       12
construction_worker    continuous   supplementary    -0.69       51   10343.14   8239.82        1
tax_pct_gross_salary   continuous   supplementary    -0.68       51      20.04      9.54        2
gross_buying_power     continuous   supplementary    -0.67       51      56.53     31.89        3
textile_worker         continuous   supplementary    -0.63       51    9247.06   6429.78        4
work_hours_year        continuous   supplementary     0.46       51    1920.25    158.69       43
Axis1                  continuous   supplementary     0.48       51       0.00      3.18       44
Axis2                  continuous   supplementary     0.72       51       0.00      0.93       45
salary_inequality      continuous   supplementary     0.88       51       2.23      1.42       56


We can easily identify the variables that are more correlated with the first axis. The second axis opposes the `mechanic` with the rest of the professions, especially `teacher` (see table \@ref(tab:table-2-13)).


Table: (\#tab:table-2-13)Characterization of the second axis from second PCA.

                   scale        type             coord   weight       mean      stdev   number
-----------------  -----------  --------------  ------  -------  ---------  ---------  -------
teacher2           continuous   active           -0.69       51       1.19       0.37        1
mechanic2          continuous   active            0.72       51       0.96       0.24       12
bank_clerk2        continuous   supplementary    -0.54       51   18749.02   13413.83        1
factory_manager    continuous   supplementary    -0.52       51   30933.34   21250.57        2
engineer           continuous   supplementary    -0.51       51   24664.71   14019.08        3
teacher            continuous   supplementary    -0.50       51   16801.96   13243.42        4
burger_work_time   continuous   supplementary     0.33       51      66.71      97.82       42
Axis3              continuous   supplementary     0.37       51       0.00       0.57       44
Axis1              continuous   supplementary     0.43       51       0.00       3.18       45
manual_qualified   continuous   supplementary     0.68       51       1.06       0.17       46



### V-test and Data Science

The v-test values are a quick and fast tool for data science (e.g. automatic exploration of significant associations) for the raw data, as well as for the results from dimension reduction techniques (e.g. PCA) and cluster analysis. When dealing with _large_ data tables, to read complex multidimensional analysis, sorting the elements according to the v-test in decreasing order, will allow us to highlight the relevant features. This enables us to see where are the systemic patterns, which in turn will accumulate progressive knowledge about the data under analyisis.

All the available information in a data table can be ordered according to the v-tests over a fatorial plane. For example, when analyzing survey data, we could include information such as "hour of the interview", or the interaction between sex-age of the pair interviewer-interviewee, etc. These attributes, located on the factorial planes, and associated with their most significant v-test, form an interesting validation tool of the survey results.

Figure \@ref(fig:fig-2-17) shows the position of the _time of interview_ and the _age of interviewer_. The "interview in the afternoon", for instance, is the center of gravity of all the interviewed persons in the afternoon.

<div class="figure" style="text-align: center">
<img src="images/figure-2-17.png" alt="Position of additional information." width="75%" />
<p class="caption">(\#fig:fig-2-17)Position of additional information.</p>
</div>

The v-test allows us to characterize all the significant associations, although we don't take into account the redundancies nor the dependencies between elements. This fact causes multiple redundancies, and consequently, improves our knowledge about the analyzed data.

As another example, we can consider the trajectory, on a factorial plane, of the categories of _age of interviewer_, from 1 to 4. Let's suppose that these categories follow the direction of the first factorial axis, as shown in figure \@ref(fig:fig-2-18). The form of this trajectory comes from the set of associations between the active elements in the analysis.

<div class="figure" style="text-align: center">
<img src="images/figure-2-18.png" alt="Pattern of a trajectory." width="75%" />
<p class="caption">(\#fig:fig-2-18)Pattern of a trajectory.</p>
</div>

It is possible that the v-test associated to the extreme categories 1 and 4 are high. However, the central categories 2 and 3 will very likely have small v-test values that won't be significantly different from the origin. Does this mean that we should ignore these "non significant" categories, even though their alignment on the trajetory shows a coherent pattern?

We see that the notion of a "coherent pattern" is implied in the associations among variables. Some elements may have weak v-test values, but these does not imply that they are useless.


#### Note {-}

The proximity between two categories _A_ and _B_ from two different variables, can be the result from two distinct effects. On one hand, it is possible that both categories share most of the individuals in common, which results in the proximity between their average points. On the other hand, it is possible that the individuals the form each category are different, although they are located in the same region of the plane (see \@ref(fig:fig-2-19)). In both cases, the proximity between categories _A_ and _B_ can be interpreted in terms of the similarity with respect to the active variables among the individuals forming such categories.

<div class="figure" style="text-align: center">
<img src="images/figure-2-19.png" alt="Proximity between two categories." width="80%" />
<p class="caption">(\#fig:fig-2-19)Proximity between two categories.</p>
</div>

For example, two age categories may be close to each other, although they are formed by different individuals. Likewise, the individuals that have a certain voting behavior will be in the same region of the plane as those individuals that consume a certain product; this can be explained because they have the socio-cultural profile without being the same individuals.



## Simultaneous Representations

### Old Unit Axes

In a Principal Component Analysis the cloud of individuals as well as the cloud of variables are defined in different spaces, with distinct origins and distinct vector basis.

For the cloud of row-points, the origin corresponds to the center of gravity of the individuals. This space is of $p$ dimensions and we denote $\mathbf{u}_{\alpha}$ the corresponding base.

In turn, for the cloud of column-points the origin is the point zero. This space is of $n$ dimensions (although the active variables define a subspace of $p$ dimensions) and denote the factorial axes as $\mathbf{v}_{\alpha}$.

Because the row-points and the column-points are therefore in different spaces it is impossible to simultaneously visualize them in the same space, in such a way that the inner proximities are respected (without deformations).

However, it is possible to represent the directions defined by each active variable over the base of factorial axes $\mathbf{u}_{\alpha}$.

<div class="figure" style="text-align: center">
<img src="images/figure-2-20.png" alt="Old base in original p-dimensional space, and the new based formed by the factorial axes." width="70%" />
<p class="caption">(\#fig:fig-2-20)Old base in original p-dimensional space, and the new based formed by the factorial axes.</p>
</div>

The vectors that define the directions of the original variables are the vectors $(1,0,0, \dots)$, $(0,1,0,\dots)$, $(0,0,1,0,\dots)$, etc.

Let $\mathbf{e_j}$ be the $j$-th vector of this basis. Its projection on the basis determined by the vector $\mathbf{u}_{\alpha}$ is defined by the scalar product of the vectors:

$$
\mathbf{e_j}^\mathsf{T} \mathbf{u}_{\alpha} = u_{j\alpha}
(\#eq:2-19)
$$

The element $u_{j\alpha}$ is the $j$-th component of the vector $\mathbf{u}_{\alpha}$.

The projection of the old axes, defining the directions of the active variables, over a new factorial basis, is given by the components of the eigenvectors $\mathbf{u}_{\alpha}$ from the analysis of row-points.

We can considered an old axis $j$---direction of the $j$-th variable---as an artificial "individual" in the space of the individuals. This "individual" has a coordinate 1 in the $j$-th axis and 0 in the rest of the axes. In this way, the variable-point $j$ can be located in the core of the cloud of row-points of the factorial plane. Its interpretation in straightforward: this point $j$ is the extreme of the unit vector that defines the direction of growth of variable $j$ in the cloud of individuals.

Interestingly, the $p$ variables can be regarded as $p$ unit vectors located in the core of the cloud of row-points. This involves a translation of the original basis to the average point of individuals. Obviously, these $p$ points are on a hypersphere of unit radius.

In the first factorial plane of the cloud of individuals, these $p$ unit vectors will be located inside a circle of unit radius as projected on the orthonormal basis of the active variables.

<div class="figure" style="text-align: center">
<img src="images/figure-2-21.png" alt="Projection of the original axes on the factorial plane with the cloud of row-points." width="70%" />
<p class="caption">(\#fig:fig-2-21)Projection of the original axes on the factorial plane with the cloud of row-points.</p>
</div>

Notice that there is no common unit between the length 1 of the unit vector defined by the $j$-th variable and the values of the coordinates of the individuals over an axis. Because only the direction is what matters, we can strettch these unit vectors in such a way that the directions defined by them are clearly "readable" in the cloud of row-points.

In our working example, figure \@ref(fig:fig-2-22) shows the simultaneous representation of the cities and the active variables.

<div class="figure" style="text-align: center">
<img src="images/figure-2-22.png" alt="Row-points with directions of growth of the variables (old unit-vector axes)." width="75%" />
<p class="caption">(\#fig:fig-2-22)Row-points with directions of growth of the variables (old unit-vector axes).</p>
</div>

This configuration of variable-points differs from the configuration obtained in section 2.2. Before, the angle between two variables $j$ and $j'$ was a measure of correlation between them. Now, all the angles are square angles; we only observe the projection of these square angles on the factorial plane.

If the arrowhead of the vector representing a variable-point is close to the circumference of radius 1, this means that the direction of growth of this variable is well defined in the factorial plane. Likewise, the individuals that are near the center take values that are close to the average of the variable. In contrast, the individuals that are further from the center, following the direction of growth of a variable, have large values for such variable. 

Notice that, if in this simultaneous representation, all the unit vectors form an narrow array around the first factorial axis. This inidicates a _size factor_. All the variales increase---and decrease---simultaneously in the direction of the first axis.


#### About the representation of variable-points {-}

The coordinate of the variable $j$ on the axis $\alpha$ is (see formula \@ref(eq:2-10)):

$$
\sqrt{\lambda_{\alpha}} \hspace{1mm} u_{j \alpha}
$$

The coordinate of the unit vector representing the direction of growth of variable $j$ on the axis $\alpha$---in the simultaneous representation graph---is:

$$
u_{j \alpha}
$$

The similarity between these two formulas implies that their respective graphic displays are also similar. The only difference is a scaling factor of $\sqrt{\lambda_{\alpha}}$, which is visually reflected as a stretching effect.

For example, the comparison of both clouds of old and new unitary axes is shown in figure \@ref(fig:fig-2-23).

<div class="figure" style="text-align: center">
<img src="images/figure-2-23.png" alt="Representation of the variable-points (top) and the old axes (bottom)." width="50%" />
<p class="caption">(\#fig:fig-2-23)Representation of the variable-points (top) and the old axes (bottom).</p>
</div>

This graphical similarity leads to abuse of language in interpretating simultaneous representation (mixing analysis of angles and analysis of growth directions).

Notice that it is not possible to directly display at the same time the supplementary variables in a simultaneous PCA plot  of variables-individuals. The supplementary variables do not participate iin the definition of the original basis for the cloud of row-points.


#### In Summary ... {-}

In every PCA we have two available representations of the variables:

1. The representation of the cloud of variable-points: each variable is a vector (unit vector if we perform a normalized PCA), and we study the angles between these vectors.

2. The simultaneous representation of individuals and active variables: the variable-points are the ends of the orthogonal unit vectors indicating the directions of growth of the variables.

We should say that these two representations of the variables can be regarded as two extreme situations of a more general representation system introduced by Gabriel (1971). Under this system, commonly known as __biplot__, the data table is decomposed as the product of two matrices: one matrix represents the rows, and the other matrix represents the columns. The biplot involves the joint representation of both elements (rows and columns) in one graphic.


<!--chapter:end:02-mechanics.Rmd-->


# Analysis {#analysis}

Carrying out a comprehensive Principal Component Analysis is both an art and a science. The analyst must have some degree of analytical experience as well as a reasonable familiarity with the analyzed data. A rushed PCA analysis tends to lead to confusing results, and frustating endeavors. A well applied PCA involves a certain strategy to analyze the data, enforced with common sense, and taking certain precautions.

In this chapter, we present a methodology to carry out a Principal Component Analysis that goes above and beyond what is typically discussed in other texts about PCA. We try to stay away from the narrow perspective of using PCA within with the sole purpose of wroking with few variables that are compatible with a statistical model. Instead, we strongly advocate for analyses that take into account as many variables as possible. This will make the analysis richer, more _holistic_, and with more coherent interpretations.


## Themescope

We are assuming that the data you are working with comes from a context of great _data diversity_. For example, data from surveys or questionnaires, or from a database of clients, in which there is an abundance of different types of variables.

In these cases with a rich variety of variables, we can group those variables in __themes__. Each theme defining a point of view or _multivariate reality_. For instance, when we have a group of socio-economic variables, or when a set of variables have to do with preferences about a set of products. By refer to this approach as __themescope__, that is, a multidimensional description by themes.

The analysis strategy that we propose is to analyze individuals by themes. This involves selecting a particular theme in which the variables associated to it become the active variables. Having a group of active variables, we study the resemblances of the individuals according to this point of view. And then we add all the available information that has not been utilized, but that can shed some light in better understanding the relationship between individuals and variables, by using the projection of supplementary elements.

<div class="figure" style="text-align: center">
<img src="images/figure-3-1.png" alt="Projection of categorical supplementary variables" width="85%" />
<p class="caption">(\#fig:fig-3-1)Projection of categorical supplementary variables</p>
</div>


#### Various Perspectives, Diverse Analyses {-}

When selecting a particular active theme, this does not stop us from selecting another theme that can then become active in itself. By changing active themes, we have a different perspective of the analyzed data, in analogous way to taking photos of the same subject from different angles.



## Conditions of Application

### Linearity and Symmetry

We have seen the importance of the correlation coefficient (or covariance) in PCA. We can actually present PCA as a visualization technique of a correlation matrix (or coavraince matrix). The technique will excel when the correlation coefficient is a _good_ measure of the association between variables. The ideal conditions to apply PCA are when the association among avriables are linear and their distributions are symmetric (i.e. closer to the normal distribution).

Consequently, we need to be cautious when the distributions are extremely asymmetric or when the associations among variables are not linear.

A common case that can limit the applicability of PCA is when analyzing variables that are seemingly continuous, but that in reality are a hybrid of continuous and nominal scale. For example, this is the case of variables like _payed work time_ of women: this is null for a woman that is a housewife, while the distribution is continuous for women that have a payed job.

Nonlinearity associations can also limit the applicability of PCA. This is illustrated with the relation betwen age and income: overall, income tends to increase with age during active working years, but when a person retires the income tend to decrease.

Phenomena of lack of symmetry and lack of linearity will affect the results of a PCA. If these issues are not indentify, they can lead to wrong interpretations and conclusions. However, the presence of these phenomena will become apparent for the well trained eyes of an experienced analyst.

We should say that techniques such as Multiple Correspondence Analysis (MCA) can always be used after having encoded (categorized) the continuous variables. Compared to PCA, MCA has the advantage of being inherently non-linear, and thus can be used in situations when PCA is limited.



### Balancing the content of active variables

More often than not, Principal Component Analysis is performed on variables having different units of measurement. In this case, the variances tend to be vary considerable in magnitude, and are not directly comparable. The typical solution to overcome this issue is to rescale the variables in standard units (i.e. mean of zero, unit variance). In this way, all variables wil be given the same importance, and we don't have to worry about units of measurement anymore. In fact, this type of transformation has become the default solution in most PCA computer programs: to carry out a normalized PCA and work on the matrix of correlations. Keep in mind that this transformation modifies the shape of the cloud of points by providing the same spread among all directions in the space of origin.

Despite the usefulness of transforming variables into standardized scale, this transformation is not always the ideal solution to _balance_ the variables. For example, if thre is a subset of variables that are highly correlated among each other, this subset will dominate the first principal component, and therefore, will have a higher importance in the analysis.

Suppose that you have 5 variables that are measuring the same aspect of a certain phenomenon, and that the other aspects are covered each one by just one variable. You can think of the group of 5 variables as being just one variable but with a variance 5 times larger than the rest of the variables. Consequently, the first axis will be determined by the cumulative effect of the 5 highly correlated variables. In summary, we should pay attention to the effect produced by groups of variables that are highly correlated, and have a mechanism to balance the importance of each aspect in the studied phenomeno.



## Validation: stability and significance

What is the part in PCA results that is not really accounted by the structure of the data, but by the randomness in the data? Are the results stables and reproducibles? Do the configuration of points change based on the studied data? All of these questions make it necessary to assess the stability of the obtained results.

The stability of the results will depend on the randomness of the data collection process (e.g. random samples, sampling surveys), as well as on the measurement errors in the variables.


### How many axes to study and retain?

Are the directions of the first axes will defined and stable? More precisely, are the dispersions in consecutive directions really different? If not, we would have to consider that the factorial plane formed by them is stable but the associated axes are not really different (i.e. indeterminate by a rotation).

One way to answer these questions is to suppose that the data come from a sample drawn from a population with a normal distribution. In this case, the eigenvalues asymptotically follow a normal distribution (Anderson, 1963). The, we can estimate a 95% confidence interval for each eigenvalue with the formula \@ref(eq:31)

\begin{equation}
\left [ \lambda_{\alpha} \left (1 - 1.96 \sqrt{2/(n-1)} \right ); \hspace{1mm} \lambda_{\alpha} \left (1+1.96\sqrt{2/(n-1)} \right) \right ]
(\#eq:31)
\end{equation}

The width of this interval gives us an idea of the stability of the eigenvalue with respect to the sample randomness. The overlapping in the intervals of two consecutive eigenvalues suggests that these eigenvalues are equal (or very similar). The corresponding axes are thus indeterminate by one rotation. Under this situation, the analyst should focus on the interpretation of the subspace defined by the first eigenvalues that are well separated.

Although this result has to do with eigenvalues of covarance matrices, it can also be applied to the eigenvalues of correlation matrices. Simulation studies show that the confidence intervals tend to be "cautious": the coverage percentage of the true eigenvalue, is almost always greater than the anounced confidence level. In any case, the asymptotic nature of the results, and the underlying hypothesis of normality, lead us to consider the results are merely indicative (not a hard rule).

In regards of the factorial axes, it is convenient to distinguish the axes that will be studied, from the axes that will be used. The factorial axes can be seen as an ultimate result, or also as an intermediate stage for further studies.

For example, a PCA can be a preliminary stage before performing a discriminant analysis. In this case, we will try to use the axes with discriminant power, which may not coincide with the axes of largest spread.

If the goal is to classify individuals, it makes sense to retain only the axes expressing real directions of spread, in order to preserve the stable characteristics of the individuals, while excluding those directions that are mainly capturing random noise.


#### Scree Test (Cattell's rule, 1966) {-}

One of the most prevalent questions in PCA is "how many principal components (or factorial axes) to retain?" Unfortunately, there is no simple answer to this question.

If we assume that the $n$ values taken by the $p$ variables come from a random process that uniformly fills up the space, without privileging any direction, then the $p$ eigenvalues of the PCA will slowly decrease in a regular form.

If a PCA provides a histogram of the eigenvalues showing one or more staircase steps, we can think that there are sufficiently strong associations between the variables. These associations would be responsible for the appearance of directions or subspaces where most of the dispersion is concentrated.

Such pragmatic considerations, can be used to determine---in a more or less subjective way---a minimum and a maximum number of axes to retain in the analysis. The main way to do this is through visual inspection of the histogram of eigenvalues following the so-called _scree test_ or _elbow criteria_ proposed by Raymond Cattell (1966). This criteria, which is the simplest and oldest one, involves graphing a line plot of the eigenvalues, ordered from largest to smallest, and then look for the "elbow" of the graph where the eigenvalues seem to level off. 

In the example of the cities (first PCA), we obtained the following eigenvalues:


Table: (\#tab:table-3-1)Distribution of eigenvalues in 1st PCA.

 num   eigenvalues   percentage   cumulative
----  ------------  -----------  -----------
   1       10.1390        84.49        84.49
   2        0.8612         7.18        91.67
   3        0.3248         2.71        94.37
   4        0.1715         1.43        95.80
   5        0.1484         1.24        97.04
   6        0.0973         0.81        97.85
   7        0.0682         0.57        98.42
   8        0.0525         0.44        98.86
   9        0.0505         0.42        99.28
  10        0.0332         0.28        99.55
  11        0.0309         0.26        99.81
  12        0.0226         0.19       100.00

<br>

We can then plot a histogram of the eigenvales, and add a line connecting the heights of the bars to better see the way in which the sizes of the eigenvalues decrease:

<img src="_main_files/figure-html/unnamed-chunk-9-1.png" width="70%" style="display: block; margin: auto;" />


In the second PCA of the salaries divided by the mean salary of a city, we obtained the following eigenvalues:


Table: (\#tab:table-3-2)Distribution of eigenvalues in 2nd PCA.

 num   eigenvalues   percentage   cumulative
----  ------------  -----------  -----------
   1        4.4910        37.43        37.43
   2        1.7148        14.29        51.72
   3        1.2989        10.82        62.54
   4        1.0396         8.66        71.20
   5        0.8699         7.25        78.45
   6        0.7831         6.53        84.98
   7        0.5309         4.42        89.40
   8        0.3874         3.23        92.63
   9        0.3210         2.67        95.31
  10        0.2561         2.13        97.44
  11        0.2021         1.68        99.12
  12        0.1052         0.88       100.00

<br>

Graphing the scree plot we obtain the following display:

<img src="_main_files/figure-html/unnamed-chunk-10-1.png" width="70%" style="display: block; margin: auto;" />


More formally, Cattell's criteria consist of sorting the lagged differences of second order between eigenvalues, as follows:

\begin{equation}
d(\alpha) = (\lambda_{\alpha + 1} - \lambda_{\alpha}) - (\lambda_{\alpha} - \lambda_{\alpha - 1})
(\#eq:32)
\end{equation}


The reason why is called _scree test_ has to do with the metaphor of a mountain scree. According to [wikipedia](https://en.wikipedia.org/wiki/Scree), a "scree is a collection of broken rock fragments at the base of crags, mountain cliffs, volcanoes or valley shoulders that has accumulated through periodic rockfall from adjacent cliff faces."

<img src="images/scree-mountain.png" width="50%" style="display: block; margin: auto;" />



#### Note {-}

We have seen that when there is a _size effect_ in the first axis, the subsequent eigenvalues are affected and reduced. However, it is possible that subsequent eigenvalues reflect structural oppositions. This is the case of of the second PCA on raw data, which corresponds approximately, to the first axis of the analysis on the ratio data, when the size effect is eliminated.

On the other hands, it is risky to interpret the percentage of inertia as a measure of the information contained in an axis. This percentage can be made as small as possible, just by adding independent random variables to the data of active variables. The overall inertia will increase, while the "information" contained in the first axes will remain the same and, consequently, the percentage of inertia in each axis will decrease.


Table: (\#tab:table-3-3)Distribution of eigenvalues from data with random perturbations.

 num   eigenvalue   percentage   cumulative
----  -----------  -----------  -----------
   1       1.7994        15.00        15.00
   2       1.5473        12.89        27.89
   3       1.4034        11.69        39.58
   4       1.2329        10.27        49.86
   5       1.1123         9.27        59.13
   6       1.0635         8.86        67.99
   7       0.8877         7.40        75.39
   8       0.7653         6.38        81.76
   9       0.7059         5.88        87.65
  10       0.6000         5.00        92.65
  11       0.5414         4.51        97.16
  12       0.3410         2.84       100.00

<img src="_main_files/figure-html/unnamed-chunk-12-1.png" width="70%" style="display: block; margin: auto;" />


### Simulations, random effects on individuals

One way to assess the stability of results involves using the available information in the data, via computational methods to run some simulations. By following this type of approaches, we are able to free ourselves from the probabilistic assupmtions about the data, which are seldom met when dealing with multivariate data.

The strategy that we use is based on random perturbations of the data, in order to simulate a certain natural variability or measurement error in the observations.

Each observation in the data matrix is replaced by the observed value ples a random quantity that follows a normal distribution with mean and variance depending on the variable under modification.

We denote this change of value as:

\begin{equation}
x_{ij} = x_{ij} + N(x_{ij}, Ks_j)
(\#eq:33)
\end{equation}

The observed value $x_{ij}$ is modified by adding a random quantity that follows a normal distribution, centered at $x_{ij}$, and with standard deviation $K$ times the standard deviation $s_j$ of variable $j$.

The value of the constant $K$ determines the amount of perturbation that we introduce in the data. $K=0$ indicates that the observations remain unchanged. A value of $K=1$ means that each observation is affected, on average, one standard deviation.

Once we have modified the data table, we can perform a PCA, calculate its directions, the correlation of the extracted directions with the original-unmodified variables, obtaining a matrix of correlations between axis systems.

In this matrix we will inspect, for each original axis, what other modified axes are most correlated with. We will also check if an axis is correlated with all other axes in analogous way. In the former case, this indicates that an axis is stable, despite the random modifications in the data. In the latter case, this indicates that an axis is the result of randomness in the data.

By looking at the matrix of correlations described in the previous paragraph, we can detect up to what extent the rank of the axes are stables, and from what point the "natural" random fluctuations in data begin.

In the example of the international cities, we show in table (TABLE 3.4) the correlation matrices between the axes (in rows) obtained in the analysis of ratios (salaries of professions with respect to the mean salary of the city) and the axes obtained with a random perturbation of 1%, 5% and 10% of the standard deviation of each variable.



Table: (\#tab:table-3-4a)Assessment of random perturbations (rows correspond to axes of 2nd PCA).

Variables (perturbation 1%)       F1      F2      F3     F4      F5
----------------------------  ------  ------  ------  -----  ------
Factorial axis 1 (2nd PCA)      1.00    0.01    0.00   0.00    0.00
Factorial axis 2 (2nd PCA)     -0.01    0.99   -0.05   0.12   -0.06
Factorial axis 3 (2nd PCA)      0.00    0.04    0.99   0.04   -0.04
Factorial axis 4 (2nd PCA)      0.00   -0.08   -0.02   0.91    0.37
Factorial axis 5 (2nd PCA)      0.00   -0.10   -0.06   0.35   -0.89

__Perturbation of 5%__


Variables (perturbation 5%)       F1     F2      F3      F4      F5
----------------------------  ------  -----  ------  ------  ------
Factorial axis 1 (2nd PCA)      0.99   0.08   -0.03    0.01   -0.03
Factorial axis 2 (2nd PCA)     -0.08   0.87   -0.06    0.20   -0.20
Factorial axis 3 (2nd PCA)      0.01   0.13    0.85    0.24    0.34
Factorial axis 4 (2nd PCA)      0.01   0.06    0.36   -0.57   -0.52
Factorial axis 5 (2nd PCA)      0.03   0.07    0.07   -0.48    0.47

__Perturbation of 10%__


Variables (perturbation 10%)       F1      F2      F3      F4      F5
-----------------------------  ------  ------  ------  ------  ------
Factorial axis 1 (2nd PCA)      -0.64    0.72   -0.11   -0.05    0.01
Factorial axis 2 (2nd PCA)      -0.39   -0.31   -0.37    0.09   -0.32
Factorial axis 3 (2nd PCA)      -0.07    0.01    0.24    0.32    0.68
Factorial axis 4 (2nd PCA)       0.11   -0.01   -0.54   -0.14    0.24
Factorial axis 5 (2nd PCA)       0.14    0.30    0.15    0.04   -0.29

<br>

By looking at the diagonal of the tables, we observe stability in the first factor, as well as in the second and the third factors, up to a random perturbation of 5% of the original standard deviation. With a high perturbation (of 10%) only the first factor is resistant to the modifications.

Table \@ref(tab:table-3-5) displays the mean and standard deviation of the salary variables (gross salary divided by the city-mean salary), as well as the correlation between the randomly modified variable and the original variable. We can tell that with a random perturbation of 10% the standard deviations increase, while the correlations decrease.


Table: (\#tab:table-3-5)Summary Statistics of Active Variables affected by random perturbations.

Variable              Summary Statistics   Original     P1%    P5%   P10%
--------------------  -------------------  ---------  -----  -----  -----
teacher               mean                 1.19        1.18   1.18   1.17
teacher               std deviation        0.37        0.38   0.44   0.54
teacher               correlation          -           0.99   0.94   0.71
bus_driver            mean                 1.04        1.04   1.03   1.05
bus_driver            std deviation        0.25        0.26   0.27   0.35
bus_driver            correlation          -           1.00   0.96   0.60
mechanic              mean                 0.96        0.96   0.96   1.00
mechanic              std deviation        0.24        0.24   0.27   0.52
mechanic              correlation          -           0.99   0.89   0.46
construction_worker   mean                 0.72        0.73   0.73   0.72
construction_worker   std deviation        0.27        0.26   0.26   0.30
construction_worker   correlation          -           0.99   0.94   0.71
metal_worker          mean                 1.17        1.16   1.16   1.16
metal_worker          std deviation        0.22        0.23   0.26   0.30
metal_worker          correlation          -           0.99   0.86   0.79
cook_chef             mean                 1.40        1.40   1.40   1.38
cook_chef             std deviation        0.61        0.63   0.61   0.58
cook_chef             correlation          -           1.00   0.99   0.95
departmental_head     mean                 2.63        2.62   2.61   2.53
departmental_head     std deviation        1.31        1.31   1.34   1.44
departmental_head     correlation          -           1.00   0.98   0.95
engineer              mean                 2.12        2.12   2.07   2.04
engineer              std deviation        0.75        0.76   0.77   0.84
engineer              correlation          -           1.00   0.96   0.90
bank_clerk            mean                 1.51        1.52   1.48   1.48
bank_clerk            std deviation        0.61        0.61   0.63   0.71
bank_clerk            correlation          -           1.00   0.98   0.90
executive_secretary   mean                 1.13        1.13   1.13   1.14
executive_secretary   std deviation        0.28        0.27   0.27   0.35
executive_secretary   correlation          -           0.99   0.98   0.96
salesperson           mean                 0.76        0.76   0.75   0.78
salesperson           std deviation        0.16        0.16   0.16   0.18
salesperson           correlation          -           0.99   0.97   0.52
textile_worker        mean                 0.68        0.68   0.70   0.66
textile_worker        std deviation        0.17        0.18   0.20   0.27
textile_worker        correlation          -           0.99   0.87   0.72


### Bootstrap Simulations

Another way of empirical assessment can be done using random resampling methods on the data. The idea is to obtain a number of data tables, all of the same dimension as the original one, by randomly sampling with replacement the observations in the data. This approach is the so-called Bootstrap method (Efron et al, 1993). Following this approach, it is possible to estimate the sampling errors and the distribution of the various PCA results.

How to implement the bootstrap method? First, we form a large number of samples of $n$ individuals which are drawn with replacement from the $n$ original individuals in the data. This set of samples is referred to as the _bootstrap samples_. For each bootstrap sample, some of the original individuals won't be part of the sample, while some individuals may appear more than once in the sample. Each bootstrap sample gives place to a data table.

On each of the bootstrap tables we calculate its eigenvalues and eigenvectors. We then obtain a bootstrap distribution of the eigenvalues, as well as the bootstrap distribution of the correlations between the eigenvectors and the original axes.

For each eigenvalue we can obtain a confidence interval. Likewise, for each eigenvector we can obtain a confidence cone around the original eigenvector. Examining the correlation between the axes can then reveal potential rotations among axes.

The bootstrap simulations can also be used to assess the stability of the projections of the variables and the categories. We can position the different bootstrap tables as supplementary information in the analysis of the original table (Lebart et al, 1995). In this way, it is possible to visualize in the factorial planes regions of "natural" fluctuation of the different elements in the data table.



## Analysis of Table of Ranks

In PCA, it is assumed that the variables are measured on a continuous scale. When applying a normalized PCA, the results will depend on the matrix of correlations between variables. Such results can be affected by the presence of outliers or atypical observations.

One approach to make the results independent from the scale of measurement and monotone transformations, consists of working with the _ranks_ of the variables and not with the actual observed values.

To do that, we substitute each value by its rank in increasing order, depending on the considered variable. By doing this, the active table becomes a table of ranks, and consequently, the PCA is performed on a correlation matrix of ranks. This is a remarkable feature of PCA: it is a general methodology that can be applied on any correlation matrix defined by the analyst.

One interesting transformation involves working with the Spearman's correlation coefficients. This coefficient measures the monotone dependency between the rank values of two variables according to the following formula:

\begin{equation}
r_s (j, j') = 1 - \frac{6 \sum_{i}^{n} (x_{ij} - x_{ij'})^2}{n (n^2 - 1)}
(\#eq:34)
\end{equation}

where the quantities $x_{ij}$ represent the rank of the individual $i$ of the $j$ variable. The advantage of the Spearman's correlation coefficient is that it coincides with the Pearson's coeffcient (usual correlation) when applied to a matrix of ranks.

In the case of the international cities, the Spearman's correlations are given in table \@ref(tab:table-3-6) (compare these with table \@ref(tab:table-2-6)).


Table: (\#tab:table-3-6)Spearman's Rank Correlation Matrix (variables from 2nd analysis)

         tea2    bus2    mec2    con2    met2    coo2    dep2    eng2    ban2    exe2   sal2   tex2
-----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  -----  -----
tea2     1.00                                                                                      
bus2     0.22    1.00                                                                              
mec2    -0.24    0.03    1.00                                                                      
con2     0.27    0.28    0.31    1.00                                                              
met2     0.16    0.03    0.19    0.11    1.00                                                      
coo2    -0.24   -0.21   -0.20   -0.43   -0.33    1.00                                              
dep2    -0.10   -0.14   -0.42   -0.61    0.03    0.24    1.00                                      
eng2    -0.15   -0.33   -0.38   -0.68    0.02    0.28    0.49    1.00                              
ban2     0.03    0.01   -0.46   -0.37   -0.37    0.24    0.46    0.26    1.00                      
exe2    -0.22   -0.28   -0.59   -0.60   -0.36    0.19    0.41    0.49    0.37    1.00              
sal2     0.02   -0.07   -0.07   -0.13   -0.17   -0.04    0.01    0.09   -0.05    0.13   1.00       
tex2     0.16    0.27   -0.04    0.41    0.02   -0.42   -0.32   -0.25   -0.30   -0.04   0.06      1

<br>

We apply a PCA on the matrix of Spearman's correlations. The eigenvalues are depicted in table \@ref(tab:table-3-7) which can be compared to those of table \@ref(tab:table-2-7).


Table: (\#tab:table-3-7)Distribution of eigenvalues (from table of ranks).

 num   eigenvalue   percentage   cumulative
----  -----------  -----------  -----------
   1       3.8758        32.30        32.30
   2       1.6301        13.58        45.88
   3       1.3199        11.00        56.88
   4       1.2365        10.30        67.19
   5       0.9125         7.60        74.79
   6       0.8049         6.71        81.50
   7       0.6575         5.48        86.98
   8       0.4481         3.73        90.71
   9       0.3752         3.13        93.84
  10       0.3372         2.81        96.65
  11       0.2662         2.22        98.87
  12       0.1360         1.13       100.00

<br>

Likewise, table \@ref(tab:table-3-8) displays the results obtained for the ranks of the active variables.


Table: (\#tab:table-3-8)PCA results of table of ranks

                        coord1   coord2   coord3    cor1    cor2    cor3
---------------------  -------  -------  -------  ------  ------  ------
teacher2                 -0.27     0.59     0.37   -0.27    0.59    0.37
bus_driver2              -0.39     0.45     0.00   -0.39    0.45    0.00
mechanic2                -0.58    -0.65    -0.11   -0.58   -0.65   -0.11
construction_worker2     -0.85     0.16    -0.14   -0.85    0.16   -0.14
metalworker2             -0.32    -0.22     0.84   -0.32   -0.22    0.84
cook_chef2                0.55    -0.30    -0.30    0.55   -0.30   -0.30
factory_manager2          0.71     0.07     0.38    0.71    0.07    0.38
engineer2                 0.73    -0.08     0.31    0.73   -0.08    0.31
bank_clerk2               0.60     0.35    -0.09    0.60    0.35   -0.09
executive_secretary2      0.75     0.27    -0.16    0.75    0.27   -0.16
salesperson2              0.11     0.15    -0.26    0.11    0.15   -0.26
textile_worker2          -0.46     0.51    -0.11   -0.46    0.51   -0.11

<br>

The cloud of points is depicted in figure \@ref(fig:fig-3-2). The configuration of this cloud is similar to the initial cloud displayed in figure \@ref(fig:fig-2-9). The similarity of the results obtained with the table of ratios confirms the good quality of the performed analysis. Also, this similarity shows that the essential information is contained in the rank of the values, and not so much in the observed eigenvalues.

<div class="figure" style="text-align: center">
<img src="images/figure-3-2.png" alt="Circle of correlations on the first factorial plane of the second analysis" width="70%" />
<p class="caption">(\#fig:fig-3-2)Circle of correlations on the first factorial plane of the second analysis</p>
</div>

The fact that the cloud of points in figure \@ref(fig:fig-3-2) is similar to the graph \@ref(fig:fig-2-9), indicates that the visual displays do not depend on: the units of measurement, monotone transformations, or possible existance of outliers. Hence, we can say that the observed configurations are _robusts_.

In the absence of robust configurations, we can suggest positioning the elements of the original table as supplementary elements in the analysis of ranks, in order to detect whether there are sensible elements.

Sometimes it is convenient to work with variables that have distributions close to a normal distribution. In this case, each variables con be handled as having a normal distribution with expected value equal to the observation of rank $k$, instead of working directly with the ranks (Lebart et al, 1977).



## Optimal Reconstitution of Data

Principal Component Analysis allows us to approximate a data matrix, generally of column-rank $p$, by using a matrix of lower rank defined by the first eigenvalues and their corresponding eigenvectors.

The formula \@ref(eq:35), referred to as the singular value decompoisition (SVD), lets us approximate the original values $x_{ij}$ with a factorization of some set of eigenvectors and eigenvalues. In other words, we can obtain an approximate reconstitution of the data values by using only a few $q$ values and vectors from the SVD.

\begin{equation}
\hat{x}^{q}_{ij} = \sum_{\alpha = 1}^{q} \sqrt{\lambda_{\alpha}} \hspace{1mm} v_{i\alpha} u_{j\alpha}
(\#eq:35)
\end{equation}

The term $\hat{x}^{q}_{ij}$ is an approximation of the observed value $x_{ij}$ from a small set of coefficients calculated from a PCA: the eigenvalues $\lambda_{\alpha}$, the eigenvectors $v_{i\alpha}$ and $u_{j\alpha}$, of rank 1 to $q$.

This reconstitution is optimal in the sense that it provides the best least-squares approximation of the original matrix: minimizing the sum of squares of the deviations between the observed values and the approximated values (for all $q$):

$$
\min \left \{ \sum_{i} \sum_{j} (x_{ij} - \hat{x}_{ij}^{q})^2 \right \}
$$

It can be proved that:

$$
\sum_{i} \sum_{j} (x_{ij} - \hat{x}^{q}_{ij})^2 = \sum_{\alpha = q+1}^{p} \lambda_{\alpha}
(\#eq:36)
$$

The sum of the $p-q$ excluded eigenvalues measures the amount of error when approximating the original cloud of points by its projection onto a subspace of dimension $q$.

This property is of great practical application. It justifies the utilization of PCA in a data compression problem (for example, in the reconstitution of images, and also in data transmission).


#### Application to Image Reconstitution {-}

Image reconstitution, for instance images from satellites, is one of the most 
interesting applications of principal components analysis. In this case, the
size of the data tables tends to be "large" (with information about the 
gray level for each pixel). Applying PCA on such a table, allows us to detect
a set of significant eigenvalues in terms of the irregularities present in 
an image.

The reconstitution enalbles an important reduction in storage capacity, because
one goes from an image of $n \times n$ pixels into another image of size 
$q \times (2n + 1)$, where $q$ is the number of retained axes in the analysis.



## Synthetic Variables and Indices

So far we have discussed Principal Component Analysis from a purely geometric perspective: how to obtain a subspace that best approximates the original distances of the data elements.

Interestingly, PCA can also be approached from other points of views. One of them involves looking for a small set of new variables---formed by the original ones---in such a way that the loss of "information" is minimized.

<div class="figure" style="text-align: center">
<img src="images/figure-3-3.png" alt="Dimension reduction or minimization of &quot;information loss&quot;" width="50%" />
<p class="caption">(\#fig:fig-3-3)Dimension reduction or minimization of "information loss"</p>
</div>

The new variables (i.e. vectors) are searched for in a way that they are as close as possible (as much correlated as possible) to the set of original variables. 
We can think of these new variables as _synthetic variables_.

The solution is obtained with the vector $\Psi$ of $n$ elements that maximizes the function \@ref(eq:37)

$$
\max \sum_{j} cor^{2} (\boldsymbol{\Psi}, \mathbf{x_j})
(\#eq:37)
$$

In other words, we look for a new variable that is the "closest" to the set of original variables. This will provide a first common factor; the rest of the factors are obtained with the same condition but orthogonal to the directions previously obtained.

Often, the first factor is highly correlated with all the variables. This indicates the so-called _size factor_, which we have discussed in detail in section \@ref(size-factor).

The size factor can be considered as an overall summary, or synthesis, of the entire set of variables. We can compare the first factor with the average of all the original variables, and notice that they are very similar.

$$
\frac{1}{p} (\mathbf{x_1} + \mathbf{x_2} + \dots + \mathbf{x_p}) \approx \boldsymbol{\Psi_1}
(\#eq:38)
$$


Table: (\#tab:table-3-9)Coefficients of the first principal component (i.e. size factor) from first PCA analysis.

Variable               Coord1   Coeff1
--------------------  -------  -------
teacher                  0.94     0.30
bus_driver               0.96     0.30
mechanic                 0.92     0.29
construction_worker      0.90     0.28
metalworker              0.95     0.30
cook_chef                0.87     0.27
factory_manager          0.84     0.26
engineer                 0.90     0.28
bank_clerk               0.88     0.28
executive_secretary      0.97     0.31
salesperson              0.96     0.30
textile_worker           0.94     0.29

<div class="figure" style="text-align: center">
<img src="images/figure-3-4.png" alt="Size Factor of active variables" width="70%" />
<p class="caption">(\#fig:fig-3-4)Size Factor of active variables</p>
</div>

The data set about the international cities provide an example of how to find an _index of mean salary per city_. The components of the unit axis give the linear combination of the original variables (mean-centered and reduced). This is the linear combination of the first principal component, namely, the desired index of mean salaries (see table \@ref(tab:table-2-3)).

A common application of PCA is to build synthetic indices. For instance, a quality index of a given product made from several characteristics of the product. A common example in psychometrics has to do with indices that define a general aptitude factor: e.g. verbal aptitude, or math aptitude. Also in economics, we often find indices of economic capacity for a certain region or city.

Sometimes there is no clear definition of the desired index, but rather a vague notion of the aspects that such an index may comprise. In these cases, the analyst must pay careful attention to collect reliable data, with indicators of the desired index, ideally with highly correlated variables. The computed index (or a first approximation) will be determined by the first principal component of the gathered data.



## Handling Missing Values

With real data, it is common to have individuals for which one or more variable measurements are missing. For example, in a survey about quality of housing, an interviewee may not feel like answering a question about the number of bathrooms in his/her house. Or the same interviewee may not recall the value of the area of the house.
In order to have an idea of the amount of missing values, it is recommended to count the number of missing values per individual. In any case, given a data matrix with missing values, we should have a policy about to handle them.

A first approach to take care of missing values consists of removing the individuals with missing data before performing a PCA. Obviously, this solution implies losing several individuals, which could be detrimental for the overall quality of the calculated results.

Another approach involves replacing the missing values with an estimation. This approach is typically known as _imputing_ missing values.

Keep in mind that PCA relies on the analysis of the dispersion in the individuals around the center of gravity. When we don't have information about an individual, a prudent decision is to place that individual at the center of the cloud. By doing this, we don't privilege any direction of dispersion.

This gives us a first basic rule to handle missing data. We substitute an individual's missing value by the mean of the variable for which there's no available information. This works as long as the amount of missing data for that given variable is small.

Of course, more refined imputation procedures can be devised. This usually depends on the degree of knowledge about the phenomenon under study. For example, if we have an old adult male farmer for which his income is missing, we could estimate this value with the average of the incomes in this category.

Notice that we could also use the results of the PCA to _fine tune_ (in a non-parametric optimization way) the estimation of a missing value. The rationale behind this approach is based on the reconstitution formula \@ref(eq:35) to approximate the data. Under this procedure, we can estimate the value of the $ij$-th cell with the $q$ first factors.



## PCA and Cluster Analysis

The graphics obtained from Principal Components Analysis provide a quick way 
to get a "photo" of the multivariate phenomenon under study. These graphical 
displays offer an excellent visual approximation to the systematic information 
contained in data.

Having said that, such visual approximations will be, in general, partial 
approximations. This is because those low dimensional representations are 
given by scatterplots in which only two dimensions are taken into account. 
Unless the information in data is truly contained in two or three dimensions, 
most graphics will give us a limited view of the multivariate phenomenon.

Together with these graphical low dimensional representations, we can also use 
clustering methods as a complementary analytical tasks to enrich the output 
of a PCA. In clustering, we look for groups of individuals having similar 
characteristics. An individual is characterized by its membership to 
a certain cluster. In turn, the average characteristics of a group serve us to 
characterize all individuals in the corresponding cluster.

We can take the output of a clustering method, that is, take the clustering
memberships of individuals, and use that information in a PCA plot. The
location of the individuals on the first factorial plane, taking into 
consideration their clustering assignment, gives an excellent opportunity to 
"see in depth" the information contained in data. In other words, with the 
formed clusters, we can see beyond the two axes of a scatterplot, and gain 
deeper insight into the factorial displays.



## Data Weighing

In PCA, the weights of the individuals affect the calculation of the means, the covariances, and the correlations.

$$
cov(x_j, x_k) = \sum_{i=1}^{n} p_i \hspace{1mm} (x_{ij} - \bar{x}_j) (x_{ik} - \bar{x}_k)
$$

In general, all the individuals of a data table have the same weight. Consequently, the analysis focuses on the description of the individuals without privileging any of them.

However, if what we are interested in is describing the population (and not just the sample), we should determine whether the sample can be made more representative by reweighing the individuals. 
Notice that in this case, the visual displays and the interpretations are conditioned by the appropriateness of the weights: we go from a purely descriptive context with an unweighted analysis, to a more delicate inferencial context.

In any case, the weights of the individuals should not vary drastically. Put it another way, the reweighing is done to refine the estimations, not to create them in an exclusive form (it is a good habit to assess the variability of the weights by looking at their histogram).

We should also highlight the possibility of using the weights to study the _stability_ of the PCA results. This can be done with the bootstrap method with a system of weights ($p_i$ = 0 or 1 or 2, etc, $\sum p_i = n$). Alternatively, we can also use a more classic system of weights ($p_i > 0$; $\sum p_i = n$).

_Note_: we can modify an active individual into a supplementary individual by assigning it a weight of zero.



## PCA as an Intermediate Analytical Stage

Working with the first principal components provides a handful of advantages: 1) the principal components are orthogonal; 2) the random variability part is minimized; 3) the components provide an optimal dimensionality reduction; and 4) calculating the distances between individuals is simplified.

Often, a problem involves modeling a certain response variable $\mathbf{y}$, in term of a series of explanatory variables. When the response variable is a quantitative variable we talk about _regression_ models. In turn, we talk about _classification_ when the response variables is of categorical nature.

One common issue when modeling a response variable---with a regression or classfication technique---has to do with _multicollinearity_ in the explanatory variables. Geometrically, the subspace spanned by the explanatory variables is unstable. This means that small variations in the values of the variables will result in large changes on the spanned subspace.

Performing a regression analysis involves projecting the response variable onto the subspace spanned by the explanatory variables. If two explanatory variables are highly correlated, small variations in these variables will substantially modify the orientation of the space. And consequently, the projectino $\hat{\mathbf{y}}$ becomes unstable (see figure \@ref(fig:fig-3-8))

<div class="figure" style="text-align: center">
<img src="images/figure-3-8.png" alt="Orthogonal projection of y onto the plane spanned by two explanatory variables x" width="50%" />
<p class="caption">(\#fig:fig-3-8)Orthogonal projection of y onto the plane spanned by two explanatory variables x</p>
</div>

In this situation, and with more than two variables, it can be interesting to use the principal components obtained on the data table $\mathbf{X}$ formed by the explanatory variables. More specifically, we can keep those components for which their eigenvalues are sufficiently different from zero.

Because the principal components are expressed as linear combinations of the explanatory variables, we can use the components to define the projection subspace for the response variable.

By using only the first principal components, we reduce the random variability in the data. Therefore we can say that the data have been "smoothed".

One is left with the operation to undo the change given by using the components. That is, we need to express the response variable in terms of the original variables. Each principal component is written as a linear combination of the explanatory variables:

\begin{equation}
\mathbf{y} = b_0 + b_1 \Psi_1 + \dots + b_p \Psi_p = \hat{\beta}_0 + \hat{\beta}_1 \mathbf{x_1} + \dots + \hat{\beta}_p \mathbf{x_p}
(\#eq:39)
\end{equation}

The principal components in this case become _instrumental_ variables. This means that theur behave like intemediate links for a another subsequent analysis.

We can select the principal components that will be part of a given model. One way to do this is by selecting them with the Furnival-Wilson (1974) algorithm to obtain the best regression equations. In fact, the problem is much simpler given that the factors are orthogonal.

An analogous situation occurs within a classification context. As a particular case, the discriminant analysis of two groups is equivalent to a regression analysis. In this case, we can have a preliminary selection of those principal components with the most discriminant power.



## Comparing Various Tables

Consider a study in which we observe a group of individuals measured by a set of variables. Moreover, suppose that we repeat the study at different time periods, and that we are interested in looking at the evoution of such measurements over time. A study like this will provide several data tables of the same dimension (e.g. one table per time period). Obviously, one of the analytical goals has to do with comparing such tables.

One possible way to compare the tables is to take one of the data tables as the active variables, while the rest of tables are treated as supplementary variables. For example, we can take the first table as the reference table; or also the last table. Likewise, we can take the mean table and use it as a benchmark to compare all the tables against with: the mean table is the active one, and all the other tables become supplementary.

<div class="figure" style="text-align: center">
<img src="images/figure-3-9.png" alt="Comparison of two tables." width="65%" />
<p class="caption">(\#fig:fig-3-9)Comparison of two tables.</p>
</div>

In the example of the international cities, we have the same study performed at two points in time: 1991 and 1994. So far we have used only the 1994 table. However, we can use the table from 1991. The only difference is that some cities are not included in 1991: Manama, Prague, Budapest, Abu Dhabi, and Bangkok.

We have decided to consolidate both tables into a single one: by stacking both tables by columns. Doing this allows us to consider those cities from 1991 as supplemetary individuals, and then project them on the results from the analysis of the 1994 table.

The axes of this analysis are those defined in the analysis of the 1994 table. The first axis is related with the income inequality. In turn, the second axis is related with the oposition of the salaries of qualified manual labor, versus the salaries of managers and employees in the sector of services.

In this plane we project the the cities in terms of their salaries from 1991. Then, the movement of the cities suggests a greater salary inequality

<!--chapter:end:03-analysis.Rmd-->


# Appendix A: Fundamentals {#appendixa}

In this appendix we discuss some fundamental notions that will allow us to provide a geometrical development of Principal Component Analysis.


## Space of $p$ Dimensions

The objects that surround us are in a three dimensional space. Each object is defined by its coordinates in a basis formed by an origin and three orthogonal axes.

<img src="images/figure-a1.png" width="50%" style="display: block; margin: auto;" />

The axes are defined by vectors of unit length the provide a direction to each axis. The three vectors $(\mathbf{e_1}, \mathbf{e_2}, \mathbf{e_3})$ define a basis in the space of three dimensions. We can actually define various basis for the same space.

It is possible to generalize the space of three dimensions to spaces of higher dimensions. However, objects in spaces of dimensions greater than three cannot be visualized.

The points located in a space of $p$ dimensions are defined by their $p$ coordinates on the associated $p$ axes:

$$
i \longrightarrow (x_{i1}, x_{i2}, \dots, x_{ip})
$$

This kind of multidmensional spaces are introduced in data analysis methods in order to _map_ each point to a row of a data table. The coordinates are defined by the values of the row. Geometrically, these points are located in a space of as many dimensions as number of columns in the table.

Analogously, the columns of the table (i.e. the variables) can be regarded as points in a space in which the coordinates are the values of the column.


## Distances between points

The squared of the distance between two points can be calculated by the sum of the squared of the differences between coordinates:

$$
d^2(i,l) = (x_{i1} - x_{l1})^2 + (x_{i2} - x_{l2})^2 + (x_{i3} - x_{l3})^2
$$

<img src="images/figure-a2.png" width="50%" style="display: block; margin: auto;" />

The distance formula can be extended to spaces with more than three dimensions.

$$
d^2(i,l) = (x_{i1} - x_{l1})^2 + (x_{i2} - x_{l2})^2 + \dots + (x_{ip} - x_{lp})^2
$$


## Center of Gravity

For every cloud of points, there is an average point, called the _center of gravity_.

<img src="images/figure-a3.png" width="50%" style="display: block; margin: auto;" />

If the points have an associated weight ($p_i$ for individual $i$), the coordinate of the center of gravity on the $i$ axis is:

$$
G_j = \sum_{i=1}^{n} p_i \hspace{1mm} x_{ij}
$$

The center of gravity corresponds the statistical notion of _mean_ or average point.


## Inertia of a cloud of points

We measure the inertia of a cloud with respect to its of gravity by:

$$
I = \sum_{i=1}^{n} p_i \hspace{1mm} d^2(i,G)
$$

The inertia is a measure of spread; to be more precise, it is a measure of weighted spread of the points.

The statistical notion associated to the inertia is the variance (i.e. spread around the mean).


## Projection of the cloud of points on a line

The projection of a point can be obtained by the scalar product of the point and the unit vector $\mathbf{u}$ defining the line on which the projection is performed.

<img src="images/figure-a5.png" width="45%" style="display: block; margin: auto;" />


$$
\psi_i = ||\mathbf{x}|| \hspace{1mm} ||\mathbf{u}|| \hspace{1mm} cos(\alpha) = \sum_{j=1}^{p} x_{ij} u_j
$$

with

$$
||\mathbf{u}||^2 = \sum_{j=1}^{p} = u_{j}^{2} = 1
$$

The inertia of the projected cloud on a line is the variance of the projections on the line $\mathbf{u}$. The square root of the variance is called _standard deviation_.

$$
I_u = \sum_{i=1}^{n} p_i \hspace{1mm} \psi_{i}^{2}
$$

with 

$$
\sum_{i=1}^{n} \psi_i = 0
$$

There is an equivalence between the mechanics notion of Inertia and the statistical notion of Variance.


## Centered and Standardized Variable

We say that a variable is mean-centered when its mean is zero. When we centered all the variables, we shift the origin of a cloud of points to the center of gravity.

<img src="images/figure-a6.png" width="50%" style="display: block; margin: auto;" />

In addition, if we divide each value by the standard deviation of the variable, this becomes mean-centered and standardized. The variance of a centered and standardized variable is equal to one.

\begin{align*}
z_{ij} &= \frac{x_{ij} - \bar{x}_j}{s_j} \\
& \\
var(\mathbf{z_j}) &= \sum_{i=1}^{n} p_i (z_{ij} - \bar{z}_j)^2 = \frac{\sum_{i=1}^{n} p_i (x_{ij} - \bar{x}_j)^2}{s_{j}^{2}} = 1
\end{align*}

The variance of a variable is the squared of the distance from the variable-point to the origin. The centered and standardized variables have a distance of one unit from the origin (i.e. they are on a sphere of radius 1).


## Correlation Coefficient

The correlation coefficient is a measure of the association between two variables when such association is lineal.

We calculate the correlation coefficient by centering and normalizing the variables:

$$
cor(\mathbf{x_j}, \mathbf{x_k}) = \sum_{i=1}^{n} p_i \left ( \frac{x_{ij} - \bar{x}_j}{s_j} \right ) \left ( \frac{x_{ik} - \bar{x}_k}{s_k} \right )
$$

The correlation coefficient can be interpreted as the scalar product of two standardized variables. Given that the distance to the origin of these variables is equal to one, we see that the correlation between two variables coincides with the cosine of the angle formed by them.

The correlation coefficient ranges between -1 and +1. When the correlation is equal to 1, this indicates that there is a perfect lineal association between the two variables. A correlation coefficient equal to -1 indicates that there is a perfect inverse lineal association. A correlation coefficient equals to zero may indicate an absence of association between the two variables, although it may also hide a non linear association (e.g. quadratic).

<img src="images/figure-a7.png" width="50%" style="display: block; margin: auto;" />

After having calculated a correlation coefficient of a sample, it can be interesting to ask whether this coefficient corresponds to a null correlation within the population. To answer this question, we can conduct a hypothesis test assuming that:

\begin{align*}
E_{H_0} (r) &= 0 \\
& \\
var_{H_0} (r) &= \frac{1}{n-1}
\end{align*}

When we are close to a value of 0, we have symmetry in the distribution of the coefficient of correlation. Therefore, under the null hypothesis we assume that the distribution of the correlation is approximately normal. We reject the null hypothesis, at the 5% significance level, if the correlation coefficient is larger than $\pm 2 / \sqrt{n-1}$.

<!--chapter:end:05-appendixa.Rmd-->


# Appendix B: PCA Formulae {#appendixb}

In this appendix we present in a synthetic way the matrix notation of the expressions and formulae used in PCA.


## General Analysis

Notation:

$\mathbf{X}$: matrix of $n$ rows (corresponding to the individuals), and $p$ columns (corresponding to the variables). We assume mean-centered variables.

$\mathbf{N}$: diagonal matrix of dimension $n \times n$, with $n_{ii} \geq 0$

$\mathbf{M}$: matrix of dimension $p \times p$, positive definite ($\mathbf{a^\mathsf{T} M a} > \mathbf{0}$ for all $\mathbf{a}$).


#### Cloud of Points {-}

In $\mathbb{R}^p$ we assume $n$ points weighted by $\mathbf{N}$, and with metric $\mathbf{M}.$

In $\mathbb{R}^n$ we assume $p$ points weighted by $\mathbf{M}$, and with metric $\mathbf{N}.$


#### Fit in n-dimensions {-}

Let $\dot{\mathbf{u}}$ be a unit vector (e.g. $\dot{\mathbf{u}}^\mathsf{T} \mathbf{Mu} = \mathbf{1}$), defining a direction in $\mathbb{R}^p$. In this direction we want to maximize the inertia of the projection of the individuals.

Projection of individuals: $\mathbf{\Psi} = \mathbf{XM} \dot{\mathbf{u}}$

Inertia of projection: $\mathbf{\Psi^\mathsf{T} N \Psi} = (\mathbf{XM}\dot{\mathbf{u}})^\mathsf{T} \mathbf{N} (\mathbf{XM}\dot{\mathbf{u}})$

The maximum projected inertia can be obtained by maximizing: $\dot{\mathbf{u}}^\mathsf{T} (\mathbf{MX^\mathsf{T}NXM}) \dot{\mathbf{u}}$, with the condition $\dot{\mathbf{u}}^\mathsf{T} \mathbf{M} \dot{\mathbf{u}} = 1$.

The vector $\dot{\mathbf{u}}$ that maximizes the previous expression is the eigenvector associated to the eigenvalue of:

$$
\mathbf{M}^{-1} (\mathbf{M X^\mathsf{T} NXM}) \dot{\mathbf{u}} = \lambda \dot{\mathbf{u}}
$$

Simplifying we get:

$$
\mathbf{X^\mathsf{T} NXM} \dot{\mathbf{u}} = \lambda \dot{\mathbf{u}}
$$

The maximum is obtained by:

$$
(\dot{\mathbf{u}} \mathbf{M})(\mathbf{X^\mathsf{T}NXM}) \dot{\mathbf{u}} = \lambda (\dot{\mathbf{u}}^\mathsf{T} \mathbf{M} \dot{\mathbf{u}}) = \lambda
$$

The image of $\dot{\mathbf{u}}$ by the metric $\mathbf{M}$ is called _factor_: $\hat{\mathbf{u}} = \mathbf{M} \dot{\mathbf{u}} = \mathbf{M}^{1/2} \mathbf{u}$

Then:

$$
\mathbf{M}(\mathbf{X^\mathsf{T}NX}) \hat{\mathbf{u}} = \lambda \hat{\mathbf{u}}
$$

where $\hat{\mathbf{u}}$ is normalized by $\mathbf{M}^{-1}$:

$$
\dot{\mathbf{u}}^\mathsf{T} \mathbf{M} \dot{\mathbf{u}} = \hat{\mathbf{u}}^\mathsf{T} \mathbf{M}^{-1} \hat{\mathbf{u}} = 1
$$


#### Diagonalizable Matrix {-}

In practice, the matrix that is diagonalized is:

$$
\mathbf{M}^{1/2} \mathbf{X^\mathsf{T}NXM}^{1/2} \mathbf{u} = \lambda \mathbf{u} 
$$

being $\mathbf{u} = \mathbf{M}^{1/2} \dot{\mathbf{u}}$, and consequently, $\mathbf{u^\mathsf{T}u} = 1$

Let $\mathbf{Y} = \mathbf{N}^{1/2} \mathbf{XM}^{1/2}$, we can express the previous relation as:

$$
\mathbf{Y^\mathsf{T}Yu} = \lambda \mathbf{u}
$$


#### Fit in n-dimensions {-}

Let $\dot{\mathbf{v}}$ a unit vector in $\mathbb{R}^n$, with $\dot{\mathbf{v}}^\mathsf{T} \mathbf{N} \dot{\mathbf{v}} = \mathbf{1}$.

The maximum projected inertia is obtained by maximizing:

$$
\dot{\mathbf{v}}^\mathsf{T} (\mathbf{NXMX^\mathsf{T}N}) \dot{\mathbf{v}}
$$

We find the maximum by diagonalizing the matrix:

$$
(\mathbf{XMX^\mathsf{T}N}) \dot{\mathbf{v}} = \lambda \dot{\mathbf{v}}
$$

The eigenvector $\dot{\mathbf{v}}$ associated to the largest eigenvalue defines the direction of $\mathbb{R}^n$ with maximum inertia.

We call _factor_ to the vector in $\mathbb{R}^n$: $\hat{\mathbf{v}} = \mathbf{N} \dot{\mathbf{v}}$

This factor verifies the relationship: $(\mathbf{NXMX^\mathsf{T}}) \hat{\mathbf{v}} = \lambda \hat{\mathbf{v}}$, with $\hat{\mathbf{v}}^\mathsf{T} \mathbf{N}^{-1} \hat{\mathbf{v}} = 1$


#### Symmetric Matrix {-}

Introducing the metric in the coordinates: $\mathbf{v} = \mathbf{N}^{1/2} \dot{\mathbf{v}}$

$$
\mathbf{N}^{1/2} \mathbf{XMX^\mathsf{T}N}^{1/2}\mathbf{v} = \lambda \mathbf{v} \quad \text{with} \quad \mathbf{v^\mathsf{T}v} = 1
$$

Utilizing the matrix $\mathbf{Y}$ we have:

$$
\mathbf{Y^\mathsf{T}Yv} = \lambda \mathbf{v}
$$


#### Transition Relations {-}

Fit in $\mathbb{R}^p$: $(\mathbf{X^\mathsf{T}NXM}) \dot{\mathbf{u}} = \lambda \dot{\mathbf{u}}$

Fit in $\mathbb{R}^n$: $(\mathbf{XMX^\mathsf{T}N}) \dot{\mathbf{v}} = \lambda \dot{\mathbf{v}}$

$$
\mathbf{XMX^\mathsf{T}N} (\mathbf{XM} \dot{\mathbf{u}}) = \lambda (\mathbf{XM} \dot{\mathbf{u}})
$$

$$
\mathbf{X^\mathsf{T}NXM} (\mathbf{X^\mathsf{T}N} \dot{\mathbf{v}}) = \lambda (\mathbf{X^\mathsf{T}N} \dot{\mathbf{v}})
$$

Comparing the previous two relations, and imposing a normalizing restriction on the eigenvectors we have:

$$
(\mathbf{XM}\dot{\mathbf{u}})^\mathsf{T} \mathbf{N} (\mathbf{XM}\dot{\mathbf{u}}) = \lambda
$$

and

$$
(\mathbf{X^\mathsf{T}N}\dot{\mathbf{v}})^\mathsf{T} \mathbf{M} (\mathbf{X^\mathsf{T}N}\dot{\mathbf{v}}) = \lambda
$$

We deduct the so-called _transition relations_:

$$
\dot{\mathbf{v}} = \frac{1}{\sqrt{\lambda}} \mathbf{XM} \dot{\mathbf{u}}
$$

and

$$
\dot{\mathbf{u}} = \frac{1}{\sqrt{\lambda}} \mathbf{X^\mathsf{T}N} \dot{\mathbf{v}}
$$

In practice, we operate with symmetric matrices and thus the _transition relations_ become:

$$
\mathbf{v} = \frac{1}{\sqrt{\lambda}} \mathbf{N}^{1/2} \mathbf{XM}^{1/2}\mathbf{u}
$$

and

$$
\mathbf{u} = \frac{1}{\sqrt{\lambda}} \mathbf{M}^{1/2} \mathbf{X^\mathsf{T}N}^{1/2}\mathbf{v}
$$


## Formulas for PCA

From a matrix standpoint, PCA consists of studying a data matrix $\mathbf{Z}$, endowed with a metric matrix $\mathbf{I}_p$ defined in $\mathbb{R}^p$, and another metric $\mathbf{N}$ defined in $\mathbb{R}^n$ (generally $\mathbf{N} = (1/n) \mathbf{I}_n$).

The matrix $\mathbf{Z}$ comes defined in the following way:

- under a normalized PCA: $\mathbf{Z} = \mathbf{XS}^{-1}$, where $\mathbf{S}$ is the diagonal matrix of standard deviations.

- under a non-normalized PCA: $\mathbf{Z} = \mathbf{X}$

The fit in $\mathbb{R}^p$ has to do with: $\mathbf{Z^\mathsf{T}NZu} = \lambda \mathbf{u}$, with $\mathbf{u^\mathsf{T}u} = 1$.

The fit in $\mathbb{R}^n$ has to do with: $\mathbf{N}^{1/2} \mathbf{ZZ^\mathsf{T}N}^{1/2} \mathbf{v} = \lambda \mathbf{v}$, with $\mathbf{v^\mathsf{T}v} = 1$.

The transition relations can be written as:

\begin{align*}
\mathbf{u} &= \frac{1}{\sqrt{\lambda}} \mathbf{Z^\mathsf{T}N}^{1/2} \mathbf{v} \\
& \\
\mathbf{v} &= \frac{1}{\sqrt{\lambda}} \mathbf{N}^{1/2} \mathbf{Z} \mathbf{u}
\end{align*}

The symmetric matrix to be diagonalized is $\mathbf{Z^\mathsf{T}NZ}$. This matrix coincides with the matrix of correlations in the case of a normalized PCA; or with a covariance matrix in the case of a non-normalized PCA.


#### Coordinates of Individuals {-}

Regardless of whether we aera analyzing active individuals or supplementary ones, the coordinates of individuals are calculated by orthogonally projecting the rows of the data matrix $\mathbf{Z}$ onto the directions of the eigenvectors $\mathbf{u}_{\alpha}$.

$$
\boldsymbol{\psi}_{\alpha} = \mathbf{Zu}_{\alpha} = 
\begin{cases}
\mathbf{XS}^{-1}\mathbf{u}_{\alpha} & \text{(normalized PCA)} \\
\\
\mathbf{Xu}_{\alpha} & \text{(non-normalized PCA)}
\end{cases}
$$

with $i$-th element:

$$
\psi_{i \alpha} = 
\begin{cases}
\sum_{j=1}^{p} \frac{x_{ij}}{s_j} u_{j\alpha} & \text{(normalized PCA)} \\
\\
\sum_{j=1}^{p} x_{ij} u_{j\alpha} & \text{(non-normalized PCA)}
\end{cases}
$$


#### Coordinates of Active Variables {-}

The coordinates of the active variables are obtained by the orthogonal projection of the columns of $\mathbf{Z}$ onto the directions defined by $\dot{\mathbf{v}}_{\alpha}$ with the metric $\mathbf{N}$.

The projection of the active variables on an axis $\alpha$ aer given by:

$$
\mathbf{\Phi}_{\alpha} = \mathbf{Z^\mathsf{T}N}^{1/2} \mathbf{v}_{\alpha} = \frac{1}{\sqrt{\lambda_{\alpha}}} \mathbf{Z^\mathsf{T}NZu}_{\alpha} = \sqrt{\lambda_{\alpha}} \hspace{1mm} \mathbf{u}_{\alpha}
$$

with the $j$-th element:

$$
\phi_{j \alpha} = \sqrt{\lambda_{\alpha}} \hspace{1mm} u_{j \alpha}
$$


#### Correlation between Variables and PCs {-}

The correlation bewtween a variable $\mathbf{x_j}$ and a principal component $\boldsymbol{\psi}_{\alpha}$ is given by:

$$
cor(\alpha, j) = \sum_{i=1}^{n} p_i \left (\frac{x_{ij}}{s_j} \right ) \left  (\frac{\psi_{i \alpha}}{\sqrt{\lambda_{\alpha}}} \right )
$$

Using matrix notation we have:

$$
\mathbf{cor}_{\alpha} = \frac{1}{\sqrt{\lambda_{\alpha}}} (\mathbf{XS}^{-1})^\mathsf{T} \mathbf{N} (\mathbf{Zu}_{\alpha}) = (\mathbf{XS}^{-1})^\mathsf{T} \mathbf{N}^{1/2} \mathbf{v}_{\alpha}
$$

$$
\mathbf{cor}_{\alpha} = 
\begin{cases}
\mathbf{Z^\mathsf{T}N}^{1/2} \mathbf{v}_{\alpha} = \mathbf{\Phi}_{\alpha} & \text{(normalized PCA)} \\
\\
\mathbf{S}^{-1} \mathbf{Z^\mathsf{T}N}^{1/2}\mathbf{v}_{\alpha} = \mathbf{S}^{-1} \mathbf{\Phi}_{\alpha} & \text{(non-normalized PCA)}
\end{cases}
$$


$$
cor(j, \alpha) = 
\begin{cases}
\phi_{j \alpha} & \text{(normalized PCA)} \\
\\
\phi_{j \alpha} / s_j & \text{(non-normalized PCA)}
\end{cases}
$$


#### Coordinates of Supplementary Variables {-}

The supplementary variables are located by using the previous rule about the the computation of the coordinates. Let $\mathbf{Z}_{+}$ the data matrix containing the supplementary variables. Taking into account the transition relations we have that:

$$
\mathbf{\Phi}_{\alpha}^{+} = \mathbf{Z^\mathsf{T}N}^{1/2} \mathbf{v}_{\alpha} = \mathbf{Z_{+}^{\mathsf{T}} N} \left (\frac{\mathbf{Zu}_{\alpha}}{\sqrt{\lambda_{\alpha}}} \right )
$$

The projection of the supplementary variables is computed from this relation between the coordinate of a variable and the projection of the individuals. In a normalized PCA, this projection is equal to the correlation between the variables and the principal component.

$$
\phi_{j \alpha}^{+} =
\begin{cases}
\sum_{i} p_i \frac{x_{ij}}{s_j} \frac{\psi_{i \alpha}}{\sqrt{\lambda_{\alpha}}} & \text{(normalized PCA)}
\\
\sum_{i} p_i x_{ij} \frac{\psi_{i\alpha}}{\sqrt{\lambda_{\alpha}}} & \text{(non-normalized PCA)}
\end{cases}
$$



#### Old Unit-Vectors in $R^p$

Let $\mathbf{e_j}$ be a unit vector of the original basis in $\mathbb{R}^p$. The projection of this vector onto the new basis is:

$$
\mathbf{e_j}^\mathsf{T} \mathbf{u}_{\alpha} = u_{j\alpha}
$$

The elements of vectors $\mathbf{u}_{\alpha}$ directly provide the projection of the original axes of $\mathbb{R}^{p}$. Eaach axis of the original basis indicates the direction of growth of a variable. These directions can be jointly represented with the projection of the individual-points.



#### Distance of Individuals to the Origin {-}

The squared distance of an individual to the origin is the sum of the squares of the values in each row of $\mathbf{Z}$ (assuming centered data):

$$
d^2(i,G) = \sum_{j=1}^{p} z_{ij}^{2} =
\begin{cases}
\sum_{j} \left (\frac{x_{ij}}{s_j} \right )^2 & \text{(normalized PCA)}
\\
\sum_{j} x_{ij}^{2} & \text{(non-normalized PCA)}
\end{cases}
$$

This formula works for both active and supplementary individuals.


#### Distance of Variables to the Origin {-}

The distance of a variable to the origin is the sum of the squares of the values in the columns of $\mathbf{Z}$, taking into account the metric $\mathbf{N}$:

$$
d^2(j,O) = \sum_{i=1}^{n} p_i \hspace{1mm} z_{ij}^{2} =
\begin{cases}
\frac{\sum_{p_i x_{ij}^{2}}}{s_{j}^{2}} = 1 & \text{(normalized PCA)}
\\
\sum_{i} p_i x_{ij}^{2} = s{_j}^{2} & \text{(non-normalized PCA)}
\end{cases}
$$


#### Contribution of Individuals to an Axis' Inertia

The prjected inertia on an axis is: $\sum_{i=1}^{n} p_i \psi_{i \alpha}^{2} = \lambda_{\alpha}$.

The part of the inertia due to an individual is:

$$
CTR(i, \alpha) = \frac{p_i \psi_{i \alpha}^{2}}{\lambda_{\alpha}} \times 100
$$

this applies to boeth a normalized and a non-normalized PCA.



#### Squared Cosines of Individuals {-}

The squared cosine of an individual is the projection of an individual onto an axis, divided by the squared of its distance to the origin:

$$
cos^2(i, \alpha) = \frac{\psi_{i \alpha}^{2}}{d^2(i,G)}
$$



#### Contributions of Variables to the Inertia {-}

The projected inertia onto an axis in $\mathbb{R}^{n}$ is: $\lambda_{\alpha} = \sum_{j}^{p} \varphi_{j\alpha}^{2}$.

The contribution of a variable to the inertia of the axis is:

$$
CTR(j, \alpha) = \frac{\varphi_{j\alpha}^{2}}{\lambda_{\alpha}} \times 100
$$

Taking into account the formula to compute the coordinates of the variables:

$$
CTR(j, \alpha) = u_{j\alpha}^{2} \times 100
$$


#### Squared Cosines of Variables {-}

$$
cos^2(j, \alpha) = \frac{\phi_{j\alpha}^{2}}{d^2(j,O)}
$$

The distance of a variable to the origin coincides with the standard deviation of the variable under a non-normalized PCA. In turn, when performing a normalized-PCA, the distance is equal to 1.

$$
cos^2 (j, \alpha) = cor^2(j, \alpha)
$$



#### Coordinates of Categories of Nominal Variables {-}

A category point is the center of gravity of the individuals that have such category:

$$
\bar{\psi}_{k \alpha} = \frac{\sum_{i \in k} p_i \psi_{i \alpha}}{\sum_{i \in k} p_i}
$$


#### Distance of Categories to the Origin {-}

$$
d^2(k,O) = \sum_{\alpha = 1}^{p} \bar{\psi}_{k \alpha}^{2}
$$


#### V-test of Categories {-}

In a v-test we are interested in calculating the critical probability corresponding to the following hypothesis:

\begin{align*}
H_0: & \bar{\psi}_{k \alpha} = 0 \\
H_1: & \bar{\psi}_{k \alpha} > 0 \quad \text{or} \quad \bar{\psi}_{k \alpha} < 0
\end{align*}

Under the assumption of random election of individuals with category $k$, we have:

\begin{align*}
E(\bar{\psi}_{k \alpha}) &= 0 \\
var(\bar{\psi}_{k \alpha}) &= \frac{n - n_k}{n_k - 1} \frac{\lambda_{\alpha}}{n_k}
\end{align*}

By the central limit theorem, the variable $\bar{\psi}_{k \alpha}$ will (approximately) follow a normal distribution.

The v-test is the value of the standardized variable $v_{k\alpha}$ with the same elvel of significance:

$$
v_{k \alpha} = \frac{\bar{\psi}_{k \alpha}}{\sqrt{\frac{n-n_k}{n_k - 1}} \frac{\lambda_{\alpha}}{n_k}}
$$


#### V-test of Continuous Variables {-}

Let $\bar{x}_{kj}$ be the mean of the variable $j$ in the group $k$. We are interested in calculating the critical probability of the following hypothesis test:

\begin{align*}
H_0: & \mu_{k j} = \bar{x}_{j} \\
H_1: & \mu_{k j} > \bar{x}_{j}  \quad \text{or} \quad \mu_{kj} < \bar{x}_{j}
\end{align*}

Under the null hypothesis, we assume that individuals with category $k$ are randomly selected:

\begin{align*}
E(\bar{x}_{kj}) &= \bar{x}_{j} \\
var(\bar{x}_{kj}) &= \frac{n - n_k}{n_k - 1} \frac{s_{j}^{2}}{n_k} = s_{kj}^{2}
\end{align*}

By the cental limit theorem, the variable $\bar{x}_{kj}$ follows (approximately) a normmal distribution.

The v-test is the value of the standardized variable with the same level of significance.

$$
v_{k\alpha} = \frac{\bar{x}_{k\alpha} - \bar{x}_{j}}{\sqrt{\frac{n - n_k}{n_k - 1} \frac{s_{j}^{2}}{n_k}}}
$$



## Biplot and PCA

The so-called _biplot_ is a general method for simultaneously representing the rows and columns of a data table. This graphing method consists of approximating the data table by a matrix product of dimension 2. The goal is to obtain a plane of the rows and columns. The techniques behind a biplot involves an eigendecomposition, such as the one performed in PCA. Usually, the biplot is carried out with mean-centered and scaled data.

Recall that PCA provides three types of graphics to visualize the active elements:

1. The "circle of correlations" where we represent the continuous variables (the cosine of the angle between two variables is the same as the correlation between variables).

2. The configuration of the individuals in the factorial plane; the utilized distance is the classic euclidean distance.

3. The simultaneous representation---in the orthonormed basis---of the original variables in the center of gravity of the cloud of points of individuals.

We should keep in mind that the aim of a biplot is to get a projection of the individuals on the directions of the original variables that respects as much as possible the distribution of the initial data.

In a biplot, we overlap in the same graphic both the rows and the columns, according to three types of simultaneous  representations:

1. In the space of variables: the cosine of the angle between two variables approximates the correlation between these two variables; likewise, the distance between two individuals approximates the Mahalanbis distance (not the typical euclidean distance in PCA).

2. In the space of individuals: the distance between individuals approximates the euclidean distance, but the distance between variables is not directly interpretable.

3. In an intermediate space: the distances, between individuals and variables, are not directly interpretable, but we obtain a "balanced" plot.

Every matrix $\mathbf{Y}$ can be decomposed into the following product:

$$
\mathbf{Y} = \mathbf{AB^\mathsf{T}}
$$

with dimensions: $(n,p) = (n,k) \times (k,p)$, where $k$ is the rank of $\mathbf{Y}$.

In a biplot, like in PCA, we graphically represent the individuals as points and the variables as vectors (i.e. arrows). The biplot involves approximating $\mathbf{Y}$ by the product:

$$
\mathbf{Y} \approx \mathbf{AB^\mathsf{T}}
$$

with dimensions: $(n,p) = (n,2) \times (2,p)$. The rows of the matrix $\mathbf{A}$ repreent the individuals, and the rows of $\mathbf{B}$ represent the variables. In order to achieve this decomposition, we use the same decomposition in a PCA, that is, the eigendecomposition of $\mathbf{Y}$:

$$
\mathbf{Y} = \mathbf{V \Lambda U^\mathsf{T}}
$$

where $\mathbf{U}$ contains the eigenvectors of $\mathbf{Y^\mathsf{T} Y}$, and $\mathbf{\Lambda}$ is the diagonal matrix of singular values (i.e. square root of the eigenvalues of $\mathbf{Y^\mathsf{T}Y}$). We have that:

$$
\mathbf{V} = \mathbf{YU\Lambda}^{-1}
$$

Retaining only the first two eigenvalues, we obtain the rank-2 approximation of $\mathbf{Y}$ by:

$$
\mathbf{Y} \approx \hat{\mathbf{Y}} = \underset{(n,2)}{\mathbf{V}} \mathbf{\Lambda} \underset{(2,p)}{\mathbf{U}}
$$

We can define three decompositions of $\mathbf{Y}$ in therms of $\mathbf{AB^\mathsf{T}}$ based on thr form in which we assign the singular values between individuals ($\mathbf{V}$) or between variables ($\mathbf{U}$).

| Representation | $\mathbf{A}$ | $\mathbf{B^\mathsf{T}}$ |
|:----------------|:-------------|:------------------------|
| Space of variables | $\mathbf{V}$ | $\mathbf{\Lambda U^\mathsf{T}}$ |
| Balanced | $\mathbf{V\Lambda}^{1/2}$ | $\mathbf{\Lambda}^{1/2} \mathbf{U^\mathsf{T}}$ |
| Space of individuals | $\mathbf{V\Lambda}$ | $\mathbf{U^\mathsf{T}}$ |


Consider the expression $y_{ij} \approx \mathbf{a_i}^{\mathsf{T}} \mathbf{b_j}$

This scalar product shows that the projections of the points $\mathbf{a_i}$ on the directions defined by $\mathbf{b_j}$ apprixmates the distribution of the initial data of variable $\mathbf{y_j}$, regardless of the performed decomposition.



#### Simultaneous Representation in the Variables Space {-}

$$
\mathbf{Y} \approx \mathbf{AB^\mathsf{T}} = (\mathbf{V})(\mathbf{\Lambda U^\mathsf{T}})
$$

The cosine of the angle formed by the vectors $\mathbf{b_j}$ and $\mathbf{b_l}$ corresponds to the correlation between variables $\mathbf{y_j}$ and $\mathbf{y_l}$. Like in PCA, this property also holds for the active variables. With respect to the supplementary variables, this property is holds only through the axes.

The euclidean distance between the individuals $\mathbf{a_i}$ and $\mathbf{a_h}$ is proportional to the Mahalanobis distance between the individuals $\mathbf{y_i}$ and $\mathbf{y_h}$ of the partitioned table.

The Mahalanobis distance is a distance that takes into account the correlations between the variables. This distance transforms the cloud of row points, usually in an elliptical shape, into a circular shape. The Mahalnobis distance is given by:

$$
\delta^2 (i, h) = (\mathbf{y_i} - \mathbf{y_h})^\mathsf{T} \mathbf{W}^{-1} (\mathbf{y_i} - \mathbf{y_h})
$$

where $\mathbf{W}$ is the covariance-variance matrix.



#### Simultaneous Representation in the Individuals Space {-}

$$
\mathbf{Y} \approx \mathbf{AB^\mathsf{T}} = (\mathbf{V \Lambda})(\mathbf{U})
$$

The euclidean distance between two individuals $\mathbf{a_i}$ and $\mathbf{a_h}$ approximates the euclidean distance between the individuals $\mathbf{y_i}$ and $\mathbf{y_h}$ of the partitioned data table. In this case there are no special properties relative to the proximity between variables: the distances are not directly interpretable.


#### Balanced Simultaneous Representation {-}

$$
\mathbf{Y} \approx \mathbf{AB^\mathsf{T}} = (\mathbf{V \Lambda}^{1/2})(\mathbf{\Lambda}^{1/2} \mathbf{U^\mathsf{T}})
$$

This option tensd to balance the representation between the rows and the columns in the sense that, for each axis, the sum of the squared of the distances to the axis is the same for the cloud of individuals as for the cloud of variables.

We obtain a "balanced" graphic. Except by the common property of all the decompositions (i.e. the projection of individuals onto the variables approximates the data table), there are no specific properties for the interpretation of the proximities between individuals, and neither for the proximities between variables.

<!--chapter:end:06-appendixb.Rmd-->


# Appendix C: Data Analysis Reminder {#appendixc}

In this appendix we provide a brief reminder of multivariate exploratory data analysis (factorial analysis, descriptive analysis, and clustering).


## Normalized Principal Component Analysis

__Active variables__: these are continuous (or quantitative) variables.

- _Distance between variables_: based on correlation

$$
d^2(j,l) = 2(1 - cor(j,l))
$$

The more associated two variables are, the smaller the angle between them.

- _Distance between individuals_: classic (squared) euclidean distance (sum of the squares of the differences of standardized data).


#### Interpretation {-}

__Factorial Planes for Variables__: This has to do with the representation of the linear association between variables. We can interpret the directions in the plane in terms of the correlations between the variables and the axes.

__Factorial Planes for Individuals__: This has to do with the representation of the similarities between individuals. We interpret the positions of the individuals on the factorial plane in terms of the meaning assigned to the axes.


__Supplementary Variables__

- _Continuous_: correlations with the axes; they are positioned in the factorial plane of the variables.

- _Categorical_: representation of each category as the center of gravity of the individuals having such category. We use the v-test to assess their "characterization power."



## Non-normalized Principal Component Analysis

__Active variables__

- _Distance between variables_: based on the covariances of the variables

$$
d^2(j,l) = var(j) + var(h) - 2 cov(j,h)
$$

- _Distance between individuals_: classic (squared) euclidean distance (sus of the squares of the differences with centered data).


#### Interpretation {-}

__Factorial Planes for Variables__: This has to do with the representation of the variances as well as the association among variables. We can interpret the directions in the plane in terms of the covariances between the variables and the axes.

__Factorial Planes for Individuals__: This has to do with the representation of the similarities between individuals. We interpret the positions of the individuals on the factorial plane in terms of their values, and taking into account the meaning assigned to the axes.


__Supplementary Variables__

- _Continuous_: we pay attention to the covariances of the continuous variables with the factorial axes.

- _Categorical_: representation of each category in the cloud of row-points as the center of gravity of the individuals having such category. We use the v-test to assess their "characterization power."



## Simple Correpondence Analysis

__Analyzed Data__: two-way table or contingency table obtained by crossing two categorical variables.

- _Rows_: analysis of the proximities between the row-profiles.

- _Columns_: analysis of the proximities between the column-profiles.

The utilized distance in both cases is the _chi-squared_ distance.


#### Interpretation {-}

- Overlapped representation of the row-profiles and the column-profiles.

- Barycentric Property: besides the dilation coefficient $(1/\sqrt{\lambda})$, a column-point is located in the barycenter of all the row-categories, weighted by the column-profile. Likewise, a row-point is located in the barycenter of all the column-categories, weighted by the row-profile.

- In the periphery of the cloud, two row-points that are close to each other indicate similar proportions in their column-categories. The same applies to column-points.

- The same rules of the active elements apply to the supplementary rows and columns. A supplementary row is located in a pseudo-barycenter of all the active columns, and viceversa.



## Multiple Correspondence Analysis

Multiple Correspondence Analysis (MCA) is the generalization of (simple) correspondence analysis to the case when we have more than two categorical variables. This analysis can also be regarded as a generalization of a normalized PCA for a data table of categorical variables.

One of the advantages of MCA is that it allows to take into account non-linear associations among variables.

__Data Table.__ The analyzed (raw) data table in this case is formed by multiple categorical variables. This table can be transformed into a complete disjoint table, or into a Burt table crossing all variables two-by-two).

- _For individuals_: we analyze the proximities between the row-profiles of the complete disjoint table.

- _For variables_: we analyze the proximities between the column-profiles of the complete disjoint table.

In both cases (rows and columns) the utilized distance is the _chi-squared_ distance.


#### Interpretation {-}

Besides a dilation coefficient:

- an individual is the average point of its caegories.

- a category is an average point of the individuals that have chosen such category.

- the global center of gravity is also the center of gravity of al the categories.

- the part of the inertia due to a given category growths when its effective decreases.

- it is recommended to avoid categories with very few effectives (or to project them as a supplementary category).

- the inertia due to a variable growths when the number of its categories grows (it is recommended to balance the number of categories in the variables).

- in the peryphery of the cloud, two categories appear close to each other 
if the individuals with such categories have answered in a similar the set of active variables.

- a factorial plane is interpreted in terms of how close or how far the projected categories are. The interpretation should be supported by looking at the v-test, the contributions to the axes, the cosine squares, etc.

- a supplementary category is the average point of the individuals having such category.



## Clustering of Factors

We can use clustering techniques to find a reduced number of groups of individuals, homogenous, and well separated. 


#### Advantages {-}

- Using a clustering technique allows us to handle a considerable number of dimensions from a dimension reduction procedure (e.g. PCA).

- using a clustering technique also allows us to reduce the amount of "noise" in the data (by using only the most important dimensions).

- it also enables us to cluster the individuals for the active variables.


#### Clustering approaches {-}

Different clustering techniques can be used.

- Hierarchical clustering methods:
    + Ward's criterion
    + Single linkage
    + Maximum linkage
    
- Moving centroids:
    + K-means

The final partition can depend on the centers of the initial groups. To overcome this limitation, we can use the _strong_ or stable clusters.

- Hybrid clustering:
    + We can combine a hierarchical approach with a moving centroid approach. 
    + This hybrid strategy is recommended for large data sets.
    

#### Groups consolidations {-}

We can use a K-means algorithm by taking the initial centroids to be the centroids of the clusters obtained by _cutting_ or pruning the dendrogram. This improves the quality and stability of the obtained partition.


#### Description of the clusters {-}

To better understand and describe the obtained clusters, we can use both the active and supplementary variables.

Using the _v-tests_:

- _Categorical variable:_ a category characterizes a group if such category is significantly more abundant---or also relatively uncommon---in a given cluster compared to the overall population (comparison based on percentages).

- _Continuous variable:_ a continuous variable characterizes a group if its mean is significantly higher or smaller than the rest of the population (comparison based on the means).


<!--chapter:end:07-appendixc.Rmd-->
