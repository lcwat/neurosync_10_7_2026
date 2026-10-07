# "Model to meaning": How to interpret and visualize generalized multilevel models in R

This is a repository that stores code and slides associated with my presentation on 10/7/2026. Here is a brief overview of how to find your way around my project:

## `/slides`

This folder contains the slide deck I presented with.

## `/markdown`

(In progress) This folder contains a markdown file that includes code for reproducing all of the plots featured in the presentation. This will be your go to if you want to borrow my code for applying the plots to your own model.

## `/scripts`

This folder contains the base code used to simulate multilevel data, apply a model to that data, extract predictions from the model, and visualize those predictions. To find and replicate `marginaleffects` and `emmeans` code from the presentation plus more, start in the `visualize_models.R` file.

## `/fits`

This folder contains binaries for the individual model fits (`.rds` files). If you want to directly replicate the plots without running the model (as in the markdown doc and visualization script), load in the model fit from its associated binary file.

## `/fig_output`

Contains the figures used in the presentation.

## Additional resources

This presentation was heavily inspired by Andrew Heiss' many amazing blog post tutorials including one describing in details all the differences between different types of marginal effects [here](https://www.andrewheiss.com/blog/2022/05/20/marginalia/) and another blog explaining about how each package incorporates random effects into predictions with frequentist and Bayesian GLMMs [here](https://www.andrewheiss.com/blog/2022/11/29/conditional-marginal-marginaleffects/). "Model to meaning" comes from a textbook of the same title by Vincent Arel-Bundock, which is another fantastic resource for practicing probing model (you can find it [here](https://marginaleffects.com/)). 

## Contact

Feel free to reach out to me if you have any issues or questions!

Email **lcwatson\@ksu.edu** or drop by the **Connect Lab** in **Bluemont Hall room 444**.
