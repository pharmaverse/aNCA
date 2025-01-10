# Adding new Tables, Listing and Graphs
In order to add new TLGs, two things are needed:
### TLG definition
Specified in [tlg.yaml](../../inst/shiny/tlg.yaml) file. Entry in that file is responsible for providing some metadata regarding the TLG (like name, descriptions, links), a function reference for creating the resulting TLG and options, taken as arguments by the generating function, that allow for quick and easy definition of widgets to be rendered in the application UI. This then allows the user to customize plots in accordance to their needs.

### `R` function
The code that creates the actual results.

## Listings
Currently not implemented.

## Tables
Currently not implemented

## Graphs
### yaml
The `Graph` entry should have the following format. Identifying keys (wrapped in `<>`) should be provided by the creator and be unique within their scope (indentation level).
 ```yaml
# unique identifier for given entry
<entry id>:
  # true / false whether TLG should be included as default
  is_default:
  # type of the TLG, in this case must be Graph
  type:
  # name of the dataset
  dataset:
  # standarized id of the TLG, e.g. pkcg01     
  pkid:
  # short label to be displayed as tab name
  label:
  # longer descriptions, to be displayed in the order table
  description:
  # link to the documentation of the TLG
  link:
  # name of the function exported by the package, responsible for generating TLG, must return a list of plots to be displayed
  fun:
  # options that can be passed as arguments to the function    
  options:
    # option id, the same as the argument that is passed to the rendering function, must be unique in the scope of the TLG entry
    <option id>:
      # type of the option/widget, one of: text, numeric, select
      type:
      # label to be displayed in the editing widget
      label:
      # default value to be provided in the field;
      # if provided, will overwrite function argument defaults;
      # if left empty, function defaults will be applied;
      # if type is 'select', '.all' keyword can be applied to select all choices;
      default: 
      # applicable to 'select' type, whether to allow for multiple values to be selected  
      multiple:
      # applicable to 'select' type, choices to pick from the dropdown, either specified outright or using a special keyword:
      # - '.colnames' keyword will pull the choices from the data column names
      # - '$COLUMN_NAME' keyword will pull choices from values of a specific column
      choices:
    # will create a label to help visually group related widgets; should be a character string; N should be replaced with an integer to uniquely identify the keyword    
    .group_label_N:
    
  template:
  # You can also specify template definitions. This is a character string with id of another TLG definition. All options will be copied over from template id. Any provided parameters will be a) overwritten if existing in the template or b) added as new.
```

### function
Function should be defined within `aNCA` package and accept at least a `data` argument for providing results to be plotted. Should return a list of plots (might be just one plot).