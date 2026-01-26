# Generate Combined PK Concentration-Time Profile Plot by Cohort

This function generates a list of plotly objects PK concentration-time
profiles by group

## Usage

``` r
pkcg02(
  adnca = data(),
  xvar = "AFRLT",
  yvar = "AVAL",
  xvar_unit = "RRLTU",
  yvar_unit = "AVALU",
  color_var = NULL,
  color_var_label = NULL,
  xbreaks_var = "NFRLT",
  xbreaks_mindist = 0.5,
  xmin = NA,
  xmax = NA,
  ymin = NA,
  ymax = NA,
  xlab = paste0("!", xvar, " [$", xvar_unit, "]"),
  ylab = paste0("!", yvar, " [$", yvar_unit, "]"),
  title = NULL,
  subtitle = NULL,
  footnote = NULL,
  plotgroup_vars = c("ROUTE", "PCSPEC", "PARAM", "TRT01A"),
  plotgroup_names = list(ROUTE = "Route", PCSPEC = "Specimen", PARAM = "Analyte", TRT01A
    = "Treatment"),
  scale = c("LIN", "LOG", "SBS")[1],
  studyid = "STUDYID",
  trt_var = "TRT01A",
  plotly = TRUE
)
```

## Arguments

- adnca:

  A data frame containing the data.

- xvar:

  A character string of the variable name for the x-axis.

- yvar:

  A character string of the variable name for the y-axis.

- xvar_unit:

  A character string of the unit for the x-axis variable.

- yvar_unit:

  A character string of the unit for the y-axis variable.

- color_var:

  A character string of the variable name for the color.

- color_var_label:

  A character string of the color label.

- xbreaks_var:

  A character string of the x-axis breaks.

- xbreaks_mindist:

  A numeric value for `xbreak_mindist`.

- xmin:

  A numeric value for the minimum x-axis limit.

- xmax:

  A numeric value for the maximum x-axis limit.

- ymin:

  A numeric value for the minimum y-axis limit.

- ymax:

  A numeric value for the maximum y-axis limit.

- xlab:

  Character for x-axis label. Defaults: `xvar` label & `xvar_unit`.

- ylab:

  Character for y-axis label. Defaults: `yvar` label & `yvar_unit`.

- title:

  Character for plot title.

- subtitle:

  Character for plot subtitle.

- footnote:

  A character string of a manual footnote for the plot.

- plotgroup_vars:

  A character vector of the variables to group data.

- plotgroup_names:

  A character vector of the grouping variable names.

- scale:

  Scale for the Y axis, either "LIN" or "LOG".

- studyid:

  A character string specifying the study ID variable.

- trt_var:

  A character string specifying the treatment variable.

- plotly:

  Logical indicating whether to return plotly objects. Defaults to TRUE.

## Value

A list of ggplot or plotly objects for each unique group.

## Author

Kezia Kobana magic numbers for footnote position and margin, work in app
up to 4 lines NOTE: might require some fine tuning down the line, looks
fine now

## Examples

``` r
# Make an example small dataset
adnca <- read.csv(system.file("shiny/data/example-ADNCA.csv", package = "aNCA"))
adnca <- adnca[adnca$USUBJID %in% unique(adnca$USUBJID)[c(1, 2)],]
attr(adnca[["AFRLT"]], "label") <- "Actual time from first dose"
attr(adnca[["AVAL"]], "label") <- "Analysis value"

# Run the function
plots <- pkcg02(adnca)
plots_log <- pkcg02(adnca, scale = "LOG")
plots_custom <- pkcg02(adnca, xmin = 0, xmax = 48, title = "PK Profile", footnote = "Study X")
plotly::plotly_build(plots[[1]]) # View the first plot

{"x":{"data":[{"x":[-0.084000000000000005,0.53800000000000003,1.5109999999999999,1.5169999999999999,3.137,4.4800000000000004,6.0110000000000001,7.5899999999999999,12.044,24.033999999999999,24.654,25.573,25.576000000000001,27.042000000000002,28.606000000000002,30.126000000000001,31.646000000000001,36.142000000000003,47.893999999999998,48.533999999999999,49.417000000000002,49.482999999999997,51.042999999999999,52.439999999999998,53.991999999999997,55.415999999999997,60.015999999999998,72.007000000000005,72.674000000000007,73.631,73.676000000000002,75.040999999999997,76.634,78.135000000000005,79.620999999999995,84.156999999999996,96.022999999999996,96.495999999999995,97.593999999999994,97.596000000000004,99.064999999999998,100.59999999999999,102.075,103.554,108.092,119.946,120.592,121.542,121.58199999999999,123.048,124.636,126.021,127.505,132.173,143.80799999999999,144.328,145.28800000000001,145.37700000000001,146.74000000000001,148.35599999999999,149.88300000000001,151.52699999999999,155.82900000000001,167.833,168.399,169.38200000000001,169.38800000000001,170.95699999999999,172.44999999999999,173.934,175.39099999999999,180.017],"y":[0.111,0.73299999999999998,2.1110000000000002,2.097,3.597,1.827,0.70199999999999996,0.308,0,0,0.65100000000000002,1.9530000000000001,1.887,3.8759999999999999,1.778,0.78500000000000003,0.40500000000000003,0,0,0.70399999999999996,1.8200000000000001,2.0920000000000001,3.8660000000000001,1.913,0.85499999999999998,0.437,0,0,0.83599999999999997,1.9650000000000001,2.153,4.0049999999999999,1.7669999999999999,0.876,0.35399999999999998,0.107,0,0.379,1.9770000000000001,1.9379999999999999,3.9390000000000001,1.996,0.85199999999999998,0.52500000000000002,0,0.155,0.76900000000000002,1.9630000000000001,2.1019999999999999,3.9940000000000002,1.857,0.80000000000000004,0.40999999999999998,0,0,0.63100000000000001,1.8700000000000001,1.9419999999999999,3.637,1.839,0.96099999999999997,0.29999999999999999,0,0.051999999999999998,0.55100000000000005,1.962,2.0219999999999998,3.9740000000000002,1.9059999999999999,0.98399999999999999,0.433,0],"text":["AFRLT:  -0.084<br />AVAL: 0.111","AFRLT:   0.538<br />AVAL: 0.733","AFRLT:   1.511<br />AVAL: 2.111","AFRLT:   1.517<br />AVAL: 2.097","AFRLT:   3.137<br />AVAL: 3.597","AFRLT:   4.480<br />AVAL: 1.827","AFRLT:   6.011<br />AVAL: 0.702","AFRLT:   7.590<br />AVAL: 0.308","AFRLT:  12.044<br />AVAL: 0.000","AFRLT:  24.034<br />AVAL: 0.000","AFRLT:  24.654<br />AVAL: 0.651","AFRLT:  25.573<br />AVAL: 1.953","AFRLT:  25.576<br />AVAL: 1.887","AFRLT:  27.042<br />AVAL: 3.876","AFRLT:  28.606<br />AVAL: 1.778","AFRLT:  30.126<br />AVAL: 0.785","AFRLT:  31.646<br />AVAL: 0.405","AFRLT:  36.142<br />AVAL: 0.000","AFRLT:  47.894<br />AVAL: 0.000","AFRLT:  48.534<br />AVAL: 0.704","AFRLT:  49.417<br />AVAL: 1.820","AFRLT:  49.483<br />AVAL: 2.092","AFRLT:  51.043<br />AVAL: 3.866","AFRLT:  52.440<br />AVAL: 1.913","AFRLT:  53.992<br />AVAL: 0.855","AFRLT:  55.416<br />AVAL: 0.437","AFRLT:  60.016<br />AVAL: 0.000","AFRLT:  72.007<br />AVAL: 0.000","AFRLT:  72.674<br />AVAL: 0.836","AFRLT:  73.631<br />AVAL: 1.965","AFRLT:  73.676<br />AVAL: 2.153","AFRLT:  75.041<br />AVAL: 4.005","AFRLT:  76.634<br />AVAL: 1.767","AFRLT:  78.135<br />AVAL: 0.876","AFRLT:  79.621<br />AVAL: 0.354","AFRLT:  84.157<br />AVAL: 0.107","AFRLT:  96.023<br />AVAL: 0.000","AFRLT:  96.496<br />AVAL: 0.379","AFRLT:  97.594<br />AVAL: 1.977","AFRLT:  97.596<br />AVAL: 1.938","AFRLT:  99.065<br />AVAL: 3.939","AFRLT: 100.600<br />AVAL: 1.996","AFRLT: 102.075<br />AVAL: 0.852","AFRLT: 103.554<br />AVAL: 0.525","AFRLT: 108.092<br />AVAL: 0.000","AFRLT: 119.946<br />AVAL: 0.155","AFRLT: 120.592<br />AVAL: 0.769","AFRLT: 121.542<br />AVAL: 1.963","AFRLT: 121.582<br />AVAL: 2.102","AFRLT: 123.048<br />AVAL: 3.994","AFRLT: 124.636<br />AVAL: 1.857","AFRLT: 126.021<br />AVAL: 0.800","AFRLT: 127.505<br />AVAL: 0.410","AFRLT: 132.173<br />AVAL: 0.000","AFRLT: 143.808<br />AVAL: 0.000","AFRLT: 144.328<br />AVAL: 0.631","AFRLT: 145.288<br />AVAL: 1.870","AFRLT: 145.377<br />AVAL: 1.942","AFRLT: 146.740<br />AVAL: 3.637","AFRLT: 148.356<br />AVAL: 1.839","AFRLT: 149.883<br />AVAL: 0.961","AFRLT: 151.527<br />AVAL: 0.300","AFRLT: 155.829<br />AVAL: 0.000","AFRLT: 167.833<br />AVAL: 0.052","AFRLT: 168.399<br />AVAL: 0.551","AFRLT: 169.382<br />AVAL: 1.962","AFRLT: 169.388<br />AVAL: 2.022","AFRLT: 170.957<br />AVAL: 3.974","AFRLT: 172.450<br />AVAL: 1.906","AFRLT: 173.934<br />AVAL: 0.984","AFRLT: 175.391<br />AVAL: 0.433","AFRLT: 180.017<br />AVAL: 0.000"],"type":"scatter","mode":"lines+markers","line":{"width":1.5118110236220474,"color":"rgba(52,60,255,1)","dash":"solid"},"hoveron":"points","name":"S1-01","legendgroup":"S1-01","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","marker":{"autocolorscale":false,"color":"rgba(52,60,255,1)","opacity":1,"size":7.559055118110237,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(52,60,255,1)"}},"frame":null}],"layout":{"margin":{"t":91.28984157179606,"r":0,"b":46.460942070908636,"l":22.249896222498961},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835205},"title":{"text":"Plot of PK Concentration-Time Profile linear scale, by Cohort: S1<br><sup>Treatment Group: DrugA 5 mg, Infusion (N=72)<br>Route: INTRAVENOUS DRIP, Specimen: SERUM, Analyte: DrugA, Treatment: DrugA 5 mg, Infusion<\/sup>","font":{"color":"rgba(0,0,0,1)","family":"sans","size":18.596928185969279},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":true,"range":[-9.0890500000000003,189.02205000000001],"tickmode":"auto","ticktext":["-0.083","6","12","23.917","30","36","47.917","54","60","71.917","78","84","95.917","102","108","119.917","126","132","143.917","150","156","167.917","174","180"],"tickvals":[-0.083000000000000185,6,12,23.917000000000002,30,36,47.917000000000002,53.999999999999993,60,71.917000000000002,78,84,95.917000000000002,102,108.00000000000001,119.91700000000002,125.99999999999999,132,143.91700000000003,150,156,167.91700000000003,174,180],"categoryorder":"array","categoryarray":["-0.083","6","12","23.917","30","36","47.917","54","60","71.917","78","84","95.917","102","108","119.917","126","132","143.917","150","156","167.917","174","180"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":10.62681610626816},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Actual time from first dose [Hours]","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835205}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":true,"range":[-0.20025000000000001,4.2052499999999995],"tickmode":"auto","ticktext":["0","1","2","3","4"],"tickvals":[0,1,2,3,3.9999999999999996],"categoryorder":"array","categoryarray":["0","1","2","3","4"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":10.62681610626816},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Analysis value [ug/mL]","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835205}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(190,190,190,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":10.62681610626816},"orientation":"h","x":0.5,"y":-0.14999999999999999,"xanchor":"center","yanchor":"top","title":{"text":"USUBJID","font":{"color":"rgba(0,0,0,1)","family":"","size":13.283520132835205}}},"hovermode":"closest","height":577.5,"barmode":"relative","annotations":[{"x":0,"y":-0.10000000000000001,"text":"","showarrow":false,"yref":"paper","xref":"paper","align":"left","parse":true}]},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"2391742876f":{"x":{},"y":{},"colour":{},"type":"scatter"},"23917ca698e":{"x":{},"y":{},"colour":{}}},"cur_data":"2391742876f","visdat":{"2391742876f":["function (y) ","x"],"23917ca698e":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
