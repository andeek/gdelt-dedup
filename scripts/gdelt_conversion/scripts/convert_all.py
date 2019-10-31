#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os
from pathlib import Path
import subprocess
import tempfile
import urllib3
from urllib3.util import parse_url

OUTDIR = "../../../data/gdelt/raw/"

las_gdelt_convert = None

# import from local dir if we have it, otherwise import from installed packages
__local_package_path = (Path(__file__).parent / ".." )
if __local_package_path.is_dir():
    import sys
    sys.path = [str(__local_package_path.absolute())] + sys.path
    import las_gdelt_convert as lgc
else:
    import las_gdelt_convert as lgc


filter_func = None
# def filter_func(basename):
#     """
#     Function to define what data to bother downloading. Return true to process, false to skip.
#
#     Currently set to download all data from 2017-10-19 or later.
#     """
#     year = int(basename[:4])
#     if len(basename) > 4:
#         mo = int(basename[4:6])
#     else:
#         mo = None
#     if len(basename) > 6:
#         day = int(basename[6:8])
#     else:
#         day = None
#     return year > 2017 or (year == 2017 and mo > 10) or (year == 2017 and mo == 10 and day >= 19)
#

if __name__ == '__main__':

    country_converter = lgc.CountryConverter(countries_of_interest=lgc.WEST_AFRICAN_COUNTRIES)

    os.makedirs(Path(OUTDIR), exist_ok=True)
    # os.makedirs((Path(OUTDIR) / "reduced_events"), exist_ok=True)

    response = subprocess.run( str(Path(__file__).parent / "get_gdelt_links.py"),
                               capture_output=True, shell=False, text=True)

    lines = response.stdout.split("\n")

    for line in reversed(lines):
        if line:
            orig_fname = os.path.basename(parse_url(line.strip()).path)
            basename, ext = orig_fname.split(os.extsep, 1)
            if (filter_func and filter_func(basename)) or (filter_func is None):
                with tempfile.TemporaryDirectory() as td:
                    with open((Path(td) / orig_fname), 'w+b') as temp:
                        print(line.strip(), orig_fname, (Path(td) / orig_fname))
                        manager = urllib3.PoolManager()
                        req = manager.urlopen('GET', line.strip())
                        temp.write(req.data)

                    df = lgc.load_gdelt_events1_csv((Path(td) / orig_fname))
                    df = country_converter.filter_gdelt_events_by_country(df)
                    df = lgc.filter_gdelt_events_by_cameo_base(df, lgc.CONFLICT_CAMEO_BASE_CODES)
                    converted_df = lgc.convert_gdelt_events_to_acled_format(df, country_converter)

                    # reduced_df = lgc.reduce_events(df)
                    # TODO: this writes zip file that when unzipped, regenerates full absolute path
                    # at which archive was created. Should redo using zipfile module to write only a single
                    # "filename.csv" file.
                    filt_fname = (Path(OUTDIR) / (basename + "_filt_gdelt." + ext))
                    df.to_csv(filt_fname, index=False, compression="zip", header=True)

                    conv_fname = (Path(OUTDIR) / (basename + "_filt_acled." + ext))
                    converted_df.to_csv(conv_fname, index=False, compression="zip", header=True)
