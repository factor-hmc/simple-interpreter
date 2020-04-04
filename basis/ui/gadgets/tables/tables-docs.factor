USING: help.markup help.syntax ui.commands kernel ;
IN: ui.gadgets.tables

ARTICLE: "ui.gadgets.tables.renderers" "Table row renderer protocol"
"Table gadgets use a row renderer to display rows and do a few other things."
$nl
"Renderers are usually instances of singleton classes, since they don't need any state of their own. Renderers are required to implement a single generic word:"
{ $subsections row-columns }
"Renderers can also implement the following optional generic words for additional row information:"
{ $subsections
    row-color
    row-value
    row-value?
}
"The following optional generic words allow the renderer to provide some information about the display of all rows:"
{ $subsections
    prototype-row
    column-alignment
    filled-column
    column-titles
} ;

ARTICLE: "ui.gadgets.tables.selection" "Table row selection"
"A few slots in the table gadget concern row selection:"
{ $slots
  { "selection" { " - if set to a model, the values of the currently selected row or rows, as determined by a " { $link row-value } " call to the renderer, is stored in this model. See " { $link "models" } "." } }
  { "selection-index" { " - if set to a model, the indices of the currently selected rows." } }
  { "selection-required?" { " - if set to a true value, the table ensures that some row is always selected, if the model is non-empty. If set to " { $link f } ", a state where nothing is selected is permitted to occur. The default is " { $link f } "." } }
}
"Some words for row selection:"
{ $subsections
    selected-row
    (selected-row)
} ;

ARTICLE: "ui.gadgets.tables.actions" "Table row actions"
"When the user double-clicks on a row, or presses " { $command table "row" row-action } " while a row is selected, optional action and hook quotations are invoked. The action receives the row value and the hook receives the table gadget itself. These quotations are stored in the " { $slot "action" } " and " { $snippet "hook" } " slots of a table, respectively."
$nl
"If the " { $slot "single-click?" } " slot is set to a true value, then single-clicking on a row will invoke the row action. The default value is " { $link f } "."
$nl
"The row action can also be invoked programmatically:"
{ $subsections row-action } ;

ARTICLE: "ui.gadgets.tables.config" "Table gadget configuration"
"Various slots in the table gadget can be set to change the appearance and behavior of the table gadget."
{ $slots
  { "gap" }
  { "focus-border-color" }
  { "mouse-color" }
  { "column-line-color" }
  { "takes-focus?" }
} ;

ARTICLE: "ui.gadgets.tables.example" "Table gadget example"
"The " { $vocab-link "color-table" } " vocabulary implements a simple application which demonstrates table gadgets. It lists all the colors in the " { $snippet "rgb.txt" } " database shipped with " { $vocab-link "colors.constants" } " in a table. Rows are highlighted with their actual color, and columns show the red, green, and blue components. Column titles are supplied." ;

ARTICLE: "ui.gadgets.tables" "Table gadgets"
"The " { $vocab-link "ui.gadgets.tables" } " vocabulary implements table gadgets. Table gadgets display a grid of values, with each row's columns generated by a renderer object."
$nl
"Tables display a model as a series of rows. The model must be a sequence, and a " { $emphasis "renderer" } " creates a sequence of columns for each row. Tables are built from and inherit all features of " { $link "ui.gadgets.line-support" } "."
{ $command-map table "row" }
"The class of tables:"
{ $subsections
    table
    table?
}
"Creating new tables:"
{ $subsections
    <table>
    "ui.gadgets.tables.renderers"
    "ui.gadgets.tables.selection"
    "ui.gadgets.tables.actions"
    "ui.gadgets.tables.example"
} ;

ABOUT: "ui.gadgets.tables"
