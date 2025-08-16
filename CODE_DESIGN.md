# Sarah's understanding of how ODMAPS works and my modifications

The original app is built off the file www/odmap_dict.csv. We have added several
modules that only use the table for their ID and otherwise get most of their
inputs from csvs in the inst/app/www/datasets_solutions folder or have them hard coded.

Tabs and headings are determined by the section and subsection columns. Then 
each element_id is cycled through to create a shiny input using a function which
is determined by element_type. 

The column element_placeholder is used as the label of the shiny input, while 
element is the label used by the knit functions before the content from the input. 

I have added a details column which I am using to add examples. I you want to
add details to a text output you can do that by changing element_type from
"text" to "text_details".

When a new type of input is needed you must:

  1. Add a new row in www/odmap_dict.csv with a new element_type and save the csv
  1. Optional, create a module to contain ui and server code for the new element in element_type.R 
  1. Create a `render_<element_type>` function in element_type.R 
  1. Add a corresponding `knit_<element_type>` function in element_type.R 
  1. **TODO remove the need for these switch calls** Add the new element_type and knit function call to the `switch` call in 
     protocol_output.Rmd and protocol_preview.Rmd
  1. Add a corresponding `export_<element_type>` function in element_type.R unless
     `export_standard` is applicable 
  1. **TODO remove the need for these switch calls** Add the new element_type and 
     export function call to the `switch` call in `output$protocol_download` 
     
NOTE Aug 16, 2025: the new functions added as modules do not have knit, export or import 
functions created yet so those functionalities will not work. If the new modules
were modified to all return similar types of tables then it might be possible to
make generic knit, export and import functions that work for all or most of them.

     
To run the local version of the app either install the app with 
`remotes::install_github("LandSciTech/ODMAP-NOBMWG")` or if you have cloned the 
repo call `devtools::load_all(".")`, then call `run_metadata()`. 
  
