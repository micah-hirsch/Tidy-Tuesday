NHL Birth Months
================
Micah Hirsch
2024-01-09

# Import Data

``` r
library(tidytuesdayR) # install.packages("tidytuesdayR")
library(tidyverse) # install.packages("tidyverse")
library(gt) # install.packages("gt")

data <- tidytuesdayR::tt_load('2024-01-09')
```

    ## 
    ##  Downloading file 1 of 4: `canada_births_1991_2022.csv`
    ##  Downloading file 2 of 4: `nhl_player_births.csv`
    ##  Downloading file 3 of 4: `nhl_rosters.csv`
    ##  Downloading file 4 of 4: `nhl_teams.csv`

``` r
births_canada <- data$canada_births_1991_2022
nhl_births <- data$nhl_player_births
nhl_teams <- data$nhl_teams
nhl_rosters <- data$nhl_rosters

rm(data)
```

# Recreating Birth Month Figure

This week, I am focused on recreating a version of the figure from
JLaw’s blog post (linked below).

Link:
<https://jlaw.netlify.app/2023/12/04/are-birth-dates-still-destiny-for-canadian-nhl-players/>

## Gather Canadian NHL Player Birth Information

First, I am going to gather the birth year and month information from
the Canadian NHL players. Since the Canadian births dataset is from
years 1991 to 2022, I am going to filter out players who were born
before 1991.

After this filtering out players born before 1991, I checked the year
range in the dataset.

``` r
canadian_players <- nhl_births |>
  # Filter to include only Canadian NHL Players
  dplyr::filter(birth_country == "CAN") |>
  # Filter to include those players born in or after 1991
  dplyr::filter(birth_year >= 1991) |>
  # selecting the variables that I need
  select(player_id, birth_year, birth_month)


canadian_players |>
  dplyr::summarize(min_year = min(birth_year),
                   max_year = max(birth_year)) |>
  gt::gt() |>
  gt::cols_label(min_year = "Min",
                 max_year = "Max") |>
  gt::tab_spanner(label = "Age Range",
              columns = c("min_year", "max_year"))
```

<div id="cgeplyyujf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cgeplyyujf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#cgeplyyujf thead, #cgeplyyujf tbody, #cgeplyyujf tfoot, #cgeplyyujf tr, #cgeplyyujf td, #cgeplyyujf th {
  border-style: none;
}
&#10;#cgeplyyujf p {
  margin: 0;
  padding: 0;
}
&#10;#cgeplyyujf .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#cgeplyyujf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#cgeplyyujf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#cgeplyyujf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#cgeplyyujf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#cgeplyyujf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#cgeplyyujf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#cgeplyyujf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#cgeplyyujf .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#cgeplyyujf .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#cgeplyyujf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#cgeplyyujf .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#cgeplyyujf .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#cgeplyyujf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#cgeplyyujf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cgeplyyujf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#cgeplyyujf .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#cgeplyyujf .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#cgeplyyujf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cgeplyyujf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#cgeplyyujf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cgeplyyujf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#cgeplyyujf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cgeplyyujf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#cgeplyyujf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cgeplyyujf .gt_left {
  text-align: left;
}
&#10;#cgeplyyujf .gt_center {
  text-align: center;
}
&#10;#cgeplyyujf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#cgeplyyujf .gt_font_normal {
  font-weight: normal;
}
&#10;#cgeplyyujf .gt_font_bold {
  font-weight: bold;
}
&#10;#cgeplyyujf .gt_font_italic {
  font-style: italic;
}
&#10;#cgeplyyujf .gt_super {
  font-size: 65%;
}
&#10;#cgeplyyujf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#cgeplyyujf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#cgeplyyujf .gt_indent_1 {
  text-indent: 5px;
}
&#10;#cgeplyyujf .gt_indent_2 {
  text-indent: 10px;
}
&#10;#cgeplyyujf .gt_indent_3 {
  text-indent: 15px;
}
&#10;#cgeplyyujf .gt_indent_4 {
  text-indent: 20px;
}
&#10;#cgeplyyujf .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="Age Range">
        <span class="gt_column_spanner">Age Range</span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Min">Min</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Max">Max</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="min_year" class="gt_row gt_right">1991</td>
<td headers="max_year" class="gt_row gt_right">2005</td></tr>
  </tbody>
  &#10;  
</table>
</div>

## Gathering Canadian Birth Information

Now I’ll look at the overall Canadian birth data. Since we know the year
range for the Canadian NHL players is from 1991 to 2005, I am filtering
out the data from years after 2005 (to my knowledge, this step was not
done in the blog post).

From there, I am calculating the overall percentage of Canadian births
that happen for each calendar month across those years. Also, like in
the JLaw’s blog post, I will calculate the expected percent births per
month assuming everyday of the year had an equal chance.

``` r
births <- births_canada |>
  # filtering out years above 2005
  dplyr::filter(year <= 2005) |>
  # Calculating sum of births per calendar month between 1991 and 2005
  dplyr::group_by(month) |>
  dplyr::summarize(n_births = sum(births)) |>
  dplyr::ungroup() |>
  # Calculating actual percent births and expected percent births
  dplyr::mutate(p_births = n_births/sum(n_births),
                month_label = month.abb[month],
                month_label = as.factor(month_label),
                month_label = fct_relevel(month_label, "Jan", "Feb",
                                          "Mar", "Apr", "May", "Jun",
                                          "Jul", "Aug", "Sep", "Oct",
                                          "Nov", "Dec"),
                p_expected = case_when(
                  month_label %in% c("Apr", "Jun", "Sep", "Nov") ~ 30/365,
                  month_label == "Feb" ~ 28/365,
                  TRUE ~ 31/365)) 

births |>
  dplyr::select(month_label, p_births, p_expected) |>
  tidyr::gather(label, value, -month_label) |>
  tidyr::spread(month_label, value) |>
  dplyr::mutate(across(Jan:Dec, ~scales::percent(., accuracy = .01))) |>
  dplyr::mutate(label = recode(label, p_births = "Actual % Births", 
                               p_expected = "Expected % Births")) |>
  gt::gt() %>%
  gt::cols_label(label = "")
```

<div id="fadfrxljdu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#fadfrxljdu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#fadfrxljdu thead, #fadfrxljdu tbody, #fadfrxljdu tfoot, #fadfrxljdu tr, #fadfrxljdu td, #fadfrxljdu th {
  border-style: none;
}
&#10;#fadfrxljdu p {
  margin: 0;
  padding: 0;
}
&#10;#fadfrxljdu .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#fadfrxljdu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#fadfrxljdu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#fadfrxljdu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#fadfrxljdu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#fadfrxljdu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#fadfrxljdu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#fadfrxljdu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#fadfrxljdu .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#fadfrxljdu .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#fadfrxljdu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#fadfrxljdu .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#fadfrxljdu .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#fadfrxljdu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#fadfrxljdu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fadfrxljdu .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#fadfrxljdu .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#fadfrxljdu .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#fadfrxljdu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fadfrxljdu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#fadfrxljdu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fadfrxljdu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#fadfrxljdu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fadfrxljdu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#fadfrxljdu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#fadfrxljdu .gt_left {
  text-align: left;
}
&#10;#fadfrxljdu .gt_center {
  text-align: center;
}
&#10;#fadfrxljdu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#fadfrxljdu .gt_font_normal {
  font-weight: normal;
}
&#10;#fadfrxljdu .gt_font_bold {
  font-weight: bold;
}
&#10;#fadfrxljdu .gt_font_italic {
  font-style: italic;
}
&#10;#fadfrxljdu .gt_super {
  font-size: 65%;
}
&#10;#fadfrxljdu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#fadfrxljdu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#fadfrxljdu .gt_indent_1 {
  text-indent: 5px;
}
&#10;#fadfrxljdu .gt_indent_2 {
  text-indent: 10px;
}
&#10;#fadfrxljdu .gt_indent_3 {
  text-indent: 15px;
}
&#10;#fadfrxljdu .gt_indent_4 {
  text-indent: 20px;
}
&#10;#fadfrxljdu .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Jan">Jan</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Feb">Feb</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mar">Mar</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Apr">Apr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="May">May</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Jun">Jun</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Jul">Jul</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Aug">Aug</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Sep">Sep</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Oct">Oct</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Nov">Nov</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Dec">Dec</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Actual % Births</td>
<td headers="Jan" class="gt_row gt_right">8.00%</td>
<td headers="Feb" class="gt_row gt_right">7.64%</td>
<td headers="Mar" class="gt_row gt_right">8.68%</td>
<td headers="Apr" class="gt_row gt_right">8.60%</td>
<td headers="May" class="gt_row gt_right">8.93%</td>
<td headers="Jun" class="gt_row gt_right">8.58%</td>
<td headers="Jul" class="gt_row gt_right">8.82%</td>
<td headers="Aug" class="gt_row gt_right">8.54%</td>
<td headers="Sep" class="gt_row gt_right">8.57%</td>
<td headers="Oct" class="gt_row gt_right">8.19%</td>
<td headers="Nov" class="gt_row gt_right">7.66%</td>
<td headers="Dec" class="gt_row gt_right">7.79%</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Expected % Births</td>
<td headers="Jan" class="gt_row gt_right">8.49%</td>
<td headers="Feb" class="gt_row gt_right">7.67%</td>
<td headers="Mar" class="gt_row gt_right">8.49%</td>
<td headers="Apr" class="gt_row gt_right">8.22%</td>
<td headers="May" class="gt_row gt_right">8.49%</td>
<td headers="Jun" class="gt_row gt_right">8.22%</td>
<td headers="Jul" class="gt_row gt_right">8.49%</td>
<td headers="Aug" class="gt_row gt_right">8.49%</td>
<td headers="Sep" class="gt_row gt_right">8.22%</td>
<td headers="Oct" class="gt_row gt_right">8.49%</td>
<td headers="Nov" class="gt_row gt_right">8.22%</td>
<td headers="Dec" class="gt_row gt_right">8.49%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

Based on the chart above, the actual percent of Canadian births in
January between 1991 and 2005 is lower than the expected percent of
Canadian births. The percentages above initially suggest Canadians are
more likely to be born in the spring and summer. Although I did an extra
filtering step, this is still consistent with the data reported in
JLaw’s blog post.

## Gathering Percent Births per Month for the NHL Players

I am now going to extract the percent births for each month for the NHL
players and merge it with the overall Canadian birth data

``` r
canadian_players <- canadian_players |>
  dplyr::group_by(birth_month) |>
  dplyr::summarize(n_births = n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(p_nhl = n_births/sum(n_births)) |>
  dplyr::rename(month = birth_month) |>
  dplyr::select(-n_births)

merged <- births |>
  dplyr::select(-n_births) |>
  dplyr::left_join(canadian_players, by = "month") |>
  dplyr::select(-month)

# creating a color palette with NJ Devils colors
my_pal <- c("Canadian NHL Players" = "#CE1126", "Canadian Population" = "#000000")

merged |>
  ggplot() +
  aes(x = month_label) +
  geom_line(aes(y = p_expected, group = 1), 
            linetype = "dashed") +
  geom_linerange(aes(ymin = p_births, ymax = p_nhl)) +
  geom_point(aes(y = p_births, color = "Canadian Population"), size = 3) +
  geom_point(aes(y = p_nhl, color = "Canadian NHL Players"), size = 3) +
  geom_text(aes(label = scales::percent(p_births, accuracy = 0.01),
                y = if_else(p_births > p_nhl, p_births + .007,
                            p_births - .007)), size = 3) +
  geom_text(aes(label = scales::percent(p_nhl, accuracy = 0.007),
                y = if_else(p_nhl > p_births, p_nhl + .01,
                            p_nhl - .007)), size = 3) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(merged$month_label))) +
  labs(x = "Birth Month",
       y = "Percent",
       title = "Canadian NHL Player Birth Months") +
  scale_color_manual(values = my_pal) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())
```

![](NHL_Birth_Months_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggsave("NHL_birth_months.png", plot = last_plot())
```

    ## Saving 7 x 5 in image
