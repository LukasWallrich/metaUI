fixture <- function() {
  data.frame(study = rep(LETTERS[1:8], each = 2), id = rep(1:2, 8),
             label = rep("duplicate label", 16),
             yi = c(.1, .1, .5, .2, -.2, .3, .4, .4, .6, .2, -.1, .1, .2, .3, .1, .8),
             vi = seq(.01, .04, length.out = 16))
}
prepared <- function(x = fixture(), ...) prepare_data(x, "study", "yi", variance = "vi", es_label = "label", es_id = "id", ...)
fit <- function(x, models = get_model_tibble()[1:2, ]) metaUI:::metaUI_fit_models(x, models)
