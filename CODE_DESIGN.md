# Sarah's understanding of how ODMAPS works and my modifications

The app is built off the file www/odmap_dict.csv. 

Tabs and headings are determined by the section and subsection columns. Then 
each element_id is cycled through to create a shiny input using a function which
is determined by element_type. 

The column element_placeholder is used as the label of the shiny input, while 
element is the label used by the knit functions before the content from the input. 

I have added a details column which I am using to add more detailed instructions
or explanations to the shiny inputs. In some cases this is placeholder text 
inside the text box, in others it is text that shows before the inputs.

When a new type of input is needed you must:

  1. Add a new row in www/odmap_dict.csv with a new element_type and save the csv
  1. Create a `render_<element_type>` function in server.R 
  1. Add the new element_type and function call to the `switch` call in `render_section()` 
  1. Add a corresponding `knit_<element_type>` function in server.R
  1. Add the new element_type and knit function call to the `switch` call in 
     protocol_output.Rmd and protocol_preview.Rmd
  1. Add a corresponding `export_<element_type>` function in server.R unless
     `export_standard` is applicable 
  1. Add the new element_type and export function call to the `switch` call in `output$protocol_download` 
     
     
**Note** So far I have not added knit or export functions for any of the new inputs I have created. 
  