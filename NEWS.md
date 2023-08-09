# streetnamer (development version)

## streetnamer 0.0.0.9029

* Fix missin lau_year parameter in sn_export preventing geojson export

...

## streetnamer 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.
* cities now ordered by population (biggest cities first) in the selector
* municipalities have the nuts_3 (province) they belong to within brackets, unless their name corresponds to nuts_3 name
* early integration of Wikidata infobox
* enable export for authenticated users
* Fix "mapping diversity" title
* enable default country selection at startup
* introduce custom_head_html for analytics
* introduce early app tour with Cicerone
* introduce functioning ignore streets filter
* introduce parameter for passing database connection in deployed environments
