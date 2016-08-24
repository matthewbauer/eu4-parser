# eu4-parser

This project parses assets for the "Clausewitz" game engine used by the popular games "Europa Universalis IV" and "Crusader Kings II". To make sense of this data, it does a couple of things:

- Opens the ```default.map``` bitmap file and the ```definitions.csv``` containing the location of each province. Each province is then converted from bitmap data to a list of "edge" points that make up its shape. All of the points are then put into a geojson format file. See ```borders.hs```, ```generate-geojson.hs```, ```map.hs```, and ```geojson.hs``` for more info.
- Parses other assets used by the Clausewitz engine into a more generic JSON text format. See ```clausewitztext.hs```, ```generate-area.hs```, ```generate-climate.hs```, ```generate-countries.hs```, ```generate-positions.hs```, ```generate-provinces.hs```, ```generate-region.hs```, ```generate-superregion.hs```, ```generate-winds.hs```, and ```generator.hs``` for more info.
- Provides a frontend to make sense of the above JSON files. Each of the geojson features are projected onto the world map. Each feature has some data associated with it like "capital" or "base_manpower". See ```index.html``` for more info.

## Live demo

A live demo is available at https://matthewbauer.us/eu4-parser/.

## Building

```sh
exec generate-all.sh
python -m SimpleHTTPServer
```

Open localhost:8000 for interactive map.
