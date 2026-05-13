# Transnational Party Strategies Dashboard

This is a Quarto/R dashboard project for mapping Dutch, Belgian and Swiss political parties and their degree of transnationalisation.

## Data

Place the research data in `data/`. The dashboard looks first for Excel files (`.xlsx`, `.xls`) and then CSV files. If no research file is found, it falls back to `data/parties_template.csv`.

Recommended columns:

- `country`: Netherlands, Belgium, Switzerland. Austria and Germany are filtered out automatically.
- `region`: Use `Flanders` or `Wallonia` for Belgian parties.
- `party`: Full party name. For the main workbook, this is column A.
- `party_abbr`: Short label used when a logo is missing.
- `physical_level`: Physical transnational party infrastructure, `low`, `medium`, or `high`. For the main workbook, this is column H.
- `formalisation_level`: Degree of formalisation, `low`, `medium`, or `high`. For the main workbook, this is column S.
- `incorporation_level`: Degree of incorporation, `low`, `medium`, or `high`. For the main workbook, this is column AA.
- `digital_level`: Digital transnational party infrastructure/social media, `low`, `medium`, or `high`. For the main workbook, this is column AH.
- `logo_file`: File name in `logos/`, for example `vvd.png`.
- `lat`, `lon`: Optional. If omitted, the dashboard places parties inside their country/region automatically.

## Logos

Place party logos in `logos/`. PNG, JPG, JPEG, SVG and WebP files are supported by the browser. Logo circles are kept at a similar visual size.

To refresh the local logo assets:

```sh
Rscript --vanilla scripts/fetch_party_logos.R
```

The fetcher prioritises Wikidata/Wikimedia logo files and falls back to icons from each party's official web domain when Wikimedia is unavailable or rate-limited. It writes `logos/logo_manifest.csv` with source URLs and status.

## Render

```sh
quarto render
```

Open `_site/index.html` after rendering.

## Map Boundaries

Accurate local GeoJSON assets are generated from Eurostat/GISCO 2024 boundaries:

```sh
Rscript --vanilla scripts/prepare_geodata.R
```

The dashboard uses:

- `www/geodata/europe_outline.geojson`: Europe external outline.
- `www/geodata/target_countries.geojson`: Netherlands, Belgium and Switzerland borders.
- `www/geodata/target_regions.geojson`: Clickable Netherlands, Switzerland, Flanders and Wallonia polygons.
- `www/geodata/belgium_language_border.geojson`: dotted Flanders-Wallonia boundary.
