Municipal-Grand-List

Municipal Grand List reports indicators of the Equalized Net Grand List and related Mill Rates. List of datasets is available from <https://portal.ct.gov/OPM/IGP-MUNFINSR/Municipal-Financial-Services/Municipal-Fiscal-Indicators>

Most recent dataset available is for SFY 2017-2018, in the [2014-2018 MS Access database on Socrata](https://data.ct.gov/Local-Government/Municipal-Fiscal-Indicators-2014-2018-MS-Access-da/k2mn-ewcm).

## Updating instructions
1. Download the new .mdb file, and place it to `raw/{NEW-YEAR}/`.
1. Run `get_csvs.sh` file in same directory as .mdb file (make sure to change name of file in script). This will extract Excel and CSV files from the MS Access database.
1. Locate GL Components CSV file in the raw data, which should be called like `GL Components 2017 GL Year.csv`. Open it in Excel, change all numeric values format from *Scientific* to *General* to make sure numbers aren't rounded in the final output, and save as XLS to `/raw/components/{YEAR}_list.xls`.
1. In RStudio, modify `scripts/componentGL-processing.R` with an appropriate output file name (update year).
1. In RStudio, modify output file in `scripts/municipal_grand_list-processing.R` (end of file), as well as `destfile` URL value to match the output file from the previous step (around line 100), and add a new fiscal year to the `final_years` list (around line 111).
1. Run the processing script.

## License MIT

## Getting Setup

We recommend approaching data processing as just another software development project. That means a few specific thing 
for us.

1. Version Control
2. Dedicated Environments
3. Automated Testing
4. Continuous Deployment


### Version control

We use git as our VCS. In most cases we can commit our full processing directory, but in cases where we are responsible 
for data suppression, we specifically exclude raw files from version control.


### Virtual Environments

Processing typically happens in either Python or R. However, testing is done with Python. We recommend setting up a 
virtual environment for managing any specific dependencies for testing a given dataset as follow:

`python3 -m venv /path/to/new/virtual/environment`

You can then install the requirements like so:

`pip install -r requirements.txt`


### Metadata

We implement many of the practices and tools from the [Frictionless Data](http://frictionlessdata.io/) paradigm. 
Metadata should be specified in the generated `datapackage.json` file. A number of fields are pre-populated, but 
complete specification is necessary in order to use our dataset testing framework and our publishing tools.
 
We add a number of additional properties to facilitate specific testing and publishing workflows. The `ckan_extras` 
dictionary contains a number of additional required metadata fields. These fields are required by
our CKAN extension.

Entries should follow the following structure:

```json
 "key": {
      "ckan_name": "name when published to ckan",
      "value": "value",
      "type": "string",
      "constraints": {
        "enum": ["v1", "v2", "v3"]
      }
    }
```

We use the convention specified by the JSON Table Schema to add constraints or limitations to expected values. Our 
testing framework will evaluate the `value` property against the `constraints`.

Resources follow the standard form as described in the [JSON Table Schema]() with one exception; a boolean field 
titled `dimensions` should be added to each field in the `schema`. This field controls which fields should be 
populated and enumerated for CKAN, which in turn controls the filter options that are presented to the user.

The final extra property that should be present is an array of spot check tests which should be specified in the 
`spot_tests` array using the following form:

```json
"spot_checks": [
  {
    "filters": {
      "field1": "field_name",
      "field2": "field_name",
      "field3": "field_name",
    },
    "expected_value": {
      "value": value_as_a_numeric,
      "type": "integer"
    }
  }
]
```

The filters should be sufficiently comprehensive so as to return only one result, which will then be compared against
 the `expected_value` property within the testing framework.
 
 
 **TODO: Explain factor relationship specification as provided within the PyTest plugin tests**
  
### Automated Testing

Testing relies on PyTest and a custom CTData PyTest plugin which is installed as a requirement dependency.

An example testing script is included in the `/tests` directory. Running `pytest -v` will execute this and 
other tests.

Our custom PyTest plugin will bootstrap a number of fixtures with values that can be tested without additional logic. 

If spot checks are provide in metadata, they will be automatically run as part of the basic testing suite. Spot checks 
should provide a series of keys corresponding to the factor level selections required to extract a single row from the 
final dataset. The value should also be provide, along with any required format conversions [NEED TO EXPAND].

In addition to spot checking, if the data processing does things like calculate percentages of a whole and there is an 
expectation that all subgroups are accounted for, it would be a good practice to extract subgroups and test that these 
percentages sum to 1 (or 100 depending on formatting).

### Deployment

We use travis-ci to automate publication of datasets to our CKAN installation. Deployment will only occur on the master 
branch and requires 100% testing success.
