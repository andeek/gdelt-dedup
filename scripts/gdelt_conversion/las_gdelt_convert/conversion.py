# -*- coding: utf-8 -*-
from pathlib import Path

import pandas as pd

from . import definitions


def load_gdelt_events1_csv(f, reconcile_cols=True):
    """
    reads a path or file handle pointing to a GDELT events 1.0 csv file and returns a pandas data frame.

    Checks number of columns and uses either the pre-2013 or post-2013 column headers as appropriate.

    Passed file handle must be seekable.

    reconcile_cols: if True, the returned dataframe will include all column names present in either the old
    or the new (post-2013) GDELT events 1.0 format. Otherwise will include only those column names inferred from the
    number of columns in the original data.
    """
    tmp_df = pd.read_csv(f, nrows=1, sep='\t', header=None)
    try:
        f.seek(0)
    except AttributeError:
        # probably passed a path, not a file handle
        pass
    ncols = len(tmp_df.columns)
    if ncols == len(definitions.gdelt_1_colnames):
        df = pd.read_csv(f, sep='\t', header=None, names=definitions.gdelt_1_colnames, dtype="str")
    elif ncols == len(definitions.gdelt_1_colnames_old):
        df = pd.read_csv(f, sep='\t', header=None, names=definitions.gdelt_1_colnames_old, dtype="str")
    else:
        raise Exception("Unknown number of columns " + str(ncols) + " in input")
    if reconcile_cols:
        df = reconcile_columns(df)
    # df["GLOBALEVENTID"] = df["GLOBALEVENTID"].astype(int)
    # df.sort_values(by="GLOBALEVENTID", inplace=True)
    return df


def reconcile_columns(df):
    """
    Modifies dataframe in place to include all columns present in either the old or the new GDELT 1.0 format. Columns
    not present in the original data will be filled with None values.
    """
    new_cols = set(definitions.gdelt_1_colnames).symmetric_difference(set(definitions.gdelt_1_colnames_old))
    for c in new_cols:
        if c not in df.columns:
            df[c] = None
    return df


class CountryConverter(object):
    def __init__(self, countries_of_interest=definitions.WEST_AFRICAN_COUNTRIES):
        data_path = ( Path(__file__).parent / "data" / "countryInfo.txt")
        self.iso_df = pd.read_csv(data_path, sep='\t', header=0, comment='#')
        self.converter_df, self.countries_without_iso_codes = None, None
        self._create_converter(countries_of_interest)

    def _create_converter(self, countries_of_interest):
        coi_srs = pd.Series(countries_of_interest)
        tmp_df = coi_srs.to_frame().merge(self.iso_df, how='left', left_on=0, right_on="Country")
        self.countries_without_iso_codes = tmp_df[tmp_df["Country"].isnull()][0].tolist()
        # self.converter_df = tmp_df[["Country", "ISO2", "ISO3", "ISONUM"]].dropna()
        self.converter_df = tmp_df[["Country", "ISO", "ISO3", "ISO-Numeric", "fips"]].dropna()

    def filter_gdelt_events_by_country(self, gdelt_df, country_col="ActionGeo_CountryCode"):
        """
        Returns a new data frame containing only those rows from gdelt_df where gdelt_df[country_col] specifies one
        of the countries of interest passed to the CountryConverter at creation

        country_col should be one of ["Actor1CountryCode", "Actor2CountryCode", "ActionGeo_CountryCode"]
        """
        if country_col == "ActionGeo_CountryCode":
            filt_col = "fips"  # not "ISO2"; these are FIPS 10-4 codes. See Germany as 'GM'; ISO2 would be 'DE'
        elif country_col in ("Actor1CountryCode", "Actor2CountryCode"):
            filt_col = "ISO3"
        else:
            raise Exception("country_col " + country_col + 'should be one of "Actor1CountryCode", ' +
                            '"Actor2CountryCode", "ActionGeo_CountryCode"')
        return gdelt_df[gdelt_df[country_col].isin(set(self.converter_df[filt_col]))]


def filter_gdelt_events_by_cameo_base(gdelt_df, cameo_root_codes, cameo_root_col="EventRootCode"):
    """
    Filters a passed data frame of GDELT events to include only events whose cameo root code (two-digit cameo code)
    is in the passed sequence of cameo root codes.

    Returns a new data frame containing only filtered rows.
    """
    cameo_root_codes = frozenset(map(str, cameo_root_codes))
    return gdelt_df[gdelt_df[cameo_root_col].isin(cameo_root_codes)]


def reduce_events(gdelt_df):
    """
    Collapse events on 'SQLDATE', 'Actor1Code', 'Actor1Name', 'Actor2Code', 'Actor2Name', 'EventCode', assuming that
    the first such event in the input is representative of all such rows
    """
    orig_cols = gdelt_df.columns.tolist()
    gdelt_df =  gdelt_df.groupby(['SQLDATE', 'Actor1Code', 'Actor1Name', 'Actor2Code', 'Actor2Name', 'EventCode'])\
        .first().reset_index()
    gdelt_df = gdelt_df[orig_cols]
    return gdelt_df


def convert_gdelt_events_to_acled_format(gdelt_df, country_converter):
    """
    Converts a passed data frame of GDELT 1.0 events into a format similar to the ACLED Africa events data
    """
    # note that GDELT Actor1CountryCode and Actor2CountryCode are ISO3 alpha codes; ActionGeo_CountryCode is FIPS
    df = gdelt_df.reset_index(drop=True)
    converted_gdelt_df = pd.DataFrame()
    tmp_df = pd.merge(df["ActionGeo_CountryCode"].to_frame(), country_converter.converter_df[["fips", "ISO-Numeric"]],
                      left_on="ActionGeo_CountryCode", right_on="fips", how="left").reset_index()
    converted_gdelt_df["ISO"] = tmp_df["ISO-Numeric"].astype(int).astype(str)
    tmp_df = pd.merge(df[["ActionGeo_CountryCode", "GLOBALEVENTID"]], country_converter.converter_df[["fips", "ISO3"]],
                      left_on="ActionGeo_CountryCode", right_on="fips", how="left")[["ISO3", "GLOBALEVENTID"]].reset_index()
    converted_gdelt_df["EVT_ID_CNTY"] = tmp_df["ISO3"] + tmp_df["GLOBALEVENTID"].astype(str)
    converted_gdelt_df["EVT_ID_NO_CNTY"] = tmp_df["GLOBALEVENTID"].astype(str)
    converted_gdelt_df["EVENT_DATE"] = df["SQLDATE"].map(
        lambda sqldate: '-'.join((sqldate[:4], sqldate[4:6], sqldate[6:])))
    converted_gdelt_df["YEAR"] = df["SQLDATE"].map(lambda sqldate: sqldate[:4])
    converted_gdelt_df[
        "TIME_PRECISION"] = 1  # time for GDELT maybe should be interpreted as time the article was posted?
    converted_gdelt_df["EVENT_TYPE"] = df["EventCode"]
    converted_gdelt_df["ACTOR1"] = df["Actor1Code"] + "_" + df["Actor1Name"]
    converted_gdelt_df["ASSOC_ACTOR_1"] = None
    converted_gdelt_df["INTER1"] = 0
    converted_gdelt_df["ACTOR2"] = df["Actor2Code"] + "_" + df["Actor2Name"]
    converted_gdelt_df["ASSOC_ACTOR_2"] = None
    converted_gdelt_df["INTER2"] = 0
    converted_gdelt_df["INTERACTION"] = 0
    converted_gdelt_df["REGION"] = "Africa"  # could update based on lookup of countries
    tmp_df = pd.merge(df["ActionGeo_CountryCode"].to_frame(), country_converter.converter_df[["fips", "Country"]],
                      left_on="ActionGeo_CountryCode", right_on="fips", how="left").reset_index()
    converted_gdelt_df["COUNTRY"] = tmp_df["Country"]

    def get_name_component(nm, n_component):
        '''
        Assumes nm will be a GDELT ActionGeo_FullName
        n_component:
          -1: returns biggest component (e.g. United States)
          -2: returns next biggest component (e.g. Tennessee) if present, None otherwise
          -3: returns next biggest component (e.g. Nashville) if present, None otherwise
          0: returns most precise available component
        '''
        try:
            tokens = nm.split(',')
        except AttributeError:
            # will get AttributeError here if nm is None or NaN
            return None
        try:
            return tokens[n_component]
        except IndexError:
            return None

    converted_gdelt_df["ADMIN1"] = df["ActionGeo_FullName"].map(
        lambda name: get_name_component(name, -1))
    converted_gdelt_df["ADMIN2"] = df["ActionGeo_FullName"].map(
        lambda name: get_name_component(name, -2))
    converted_gdelt_df["ADMIN3"] = df["ActionGeo_FullName"].map(
        lambda name: get_name_component(name, -3))
    converted_gdelt_df["LOCATION"] = df["ActionGeo_FullName"].map(
        lambda name: get_name_component(name, 0))
    converted_gdelt_df["LATITUDE"] = df["ActionGeo_Lat"]
    converted_gdelt_df["LONGITUDE"] = df["ActionGeo_Long"]
    converted_gdelt_df["GEO_PRECISION"] = 3  # default to coarse, "regional capital" precision
    try:
        converted_gdelt_df["SOURCE"] = df["SOURCEURL"]
    except KeyError:
        # SOURCEURL only present in recent GDELT data
        converted_gdelt_df["SOURCE"] = None
    # assume international; we're currently looking at English language news sites:
    converted_gdelt_df["SOURCE_SCALE"] = "International"
    converted_gdelt_df["NOTES"] = ""
    converted_gdelt_df["FATALITIES"] = None

    # converted_gdelt_df["EVT_ID_NO_CNTY"] = converted_gdelt_df["EVT_ID_NO_CNTY"].astype(int)
    # converted_gdelt_df.sort_values(by="EVT_ID_NO_CNTY", inplace=True)
    return converted_gdelt_df
