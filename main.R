library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(base64enc)
library(png)
library(ggplot2)

options("tercen.workflowId" = "7132ce367ee5df28fea4032b3f011888")
options("tercen.stepId"     = "426b00e4-b970-4042-9a53-93a10ac2da90")

getValues <- function(ctx){
  values <- list()
  
  data <- ctx %>% select(.y, .ri, .ci)
  if(ctx$hasXAxis) data$.x <- select(ctx, .x)[[".x"]]
  
  if(length(ctx$colors)) data <- data %>% dplyr::bind_cols(ctx$select(ctx$colors))
  
  rnames <- ctx$rselect() 
  rnames$.ri <- seq_len(nrow(rnames)) - 1
  data <- left_join(data, rnames, by = ".ri")

  cnames <- ctx$cselect()
  cnames$.ci <- seq_len(nrow(cnames)) - 1
  data <- left_join(data, cnames, by = ".ci")
  
  return(data)
}

input.par <- list(
  plot.width = 750,
  plot.height = 750,
  jitter = T,
  log = T,
  average = "Mean",
  dot.size = 0.5,
  error.bar.type = "Standard Deviation",
  bar.width = 0.5,
  xlab = "X Values",
  ylab = "Y Values",
  title = "My title",
  subtitle = "My subtitle",
  caption = "My caption"
)

ctx <- tercenCtx()
df <- getValues(ctx)

if(input.par$log) df$.y <- log1p(df$.y)

if(input.par$average == "Mean") {
  df_agg <- df %>%
    group_by_at(vars(-.y)) %>%
    summarise(mn = mean(.y, na.rm = TRUE),
              n = n(),
              stdv = sd(.y, na.rm = TRUE))
}
if(input.par$average == "Median") {
  df_agg <- df %>%
    group_by_at(vars(-.y)) %>%
    summarise(mn = median(.y, na.rm = TRUE),
              n = n(),
              stdv = sd(.y, na.rm = TRUE))
}

fill.col <- NULL
if(length(unlist(ctx$colors)) > 0) fill.col <- unlist(ctx$colors)
x.axis <- NULL
if(!ctx$hasXAxis) {
  df_agg$.x <- ""
  df$.x <- ""
} 

theme_set(theme_light())

### Core plot with x and y axes
plt <- ggplot(df_agg, aes_string(x = ".x", y = "mn", fill = fill.col)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  labs(
    title = input.par$title,
    subtitle = input.par$subtitle,
    caption = input.par$caption,
    x = input.par$xlab,
    y = input.par$ylab,
    fill = "Legend"
  )

### Add jitter
plt <- plt + geom_jitter(
  data = df,
  aes_string(x = ".x", y = ".y", fill = fill.col),
  size = input.par$dot.size,
  position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9)
)

### Add SD bars
if(input.par$error.bar.type == "Standard Deviation") {
  plt <- plt + geom_errorbar(
    aes(ymin = mn - stdv, ymax = mn + stdv),
    width = input.par$bar.width,
    position = position_dodge(width = 0.9)
  )
}

### Annotate with sample size
  
### Facets based on rows and columns
cnames <- unlist(ctx$cnames)
if(ctx$cnames[[1]] == "") cnames <- "."
rnames <- unlist(ctx$rnames)
if(ctx$rnames[[1]] == "") rnames <- "."

plt <- plt + facet_grid(
  as.formula(paste(
    paste(rnames, collapse = "+"),
    "~",
    paste(cnames, collapse = "+")
  ))
)
  
tmp <- tempfile(fileext = ".png")
png(tmp, width = input.par$plot.width, height = input.par$plot.height, unit = "px")
plot(plt)
dev.off()

output_string <- base64enc::base64encode(
  readBin(tmp, "raw", file.info(tmp)[1, "size"]),
  "txt"
)

tibble::tibble(
  filename = "test",
  mimetype = "image/png",
  .content = output_string
) %>%
  ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
