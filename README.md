# Rect

Framework designed to generate a financial report with a main focus in Excel spreadsheet.

You build rectangles (rect from here onward) from various data sources and manimpulate/compose those rects into one final rect.
The final rect will be rendered into an Excel spreadsheet, text file, HTML or whatever you like.
 
A rect consists of cells having its type and format.

Rect operations:

* head/tail holizontally/vertically on a rect
* merging rects holizontally/vertiacally

Rect generators:

* GenericRectGen can handle typical use cases and is the only available Rect generator now.
