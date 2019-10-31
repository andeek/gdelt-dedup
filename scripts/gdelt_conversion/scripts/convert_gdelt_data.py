#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from pathlib import Path

las_gdelt_convert = None

# import from local dir if we have it, otherwise import from installed packages
__local_package_path = (Path(__file__).parent / ".." )
if __local_package_path.is_dir():
    import sys
    sys.path = [str(__local_package_path.absolute())] + sys.path
    import las_gdelt_convert as lgc
else:
    import las_gdelt_convert as lgc


def run(args):
    df = lgc.load_gdelt_events1_csv(args.gdelt_path)
    country_converter = lgc.CountryConverter(countries_of_interest=lgc.WEST_AFRICAN_COUNTRIES)
    df = country_converter.filter_gdelt_events_by_country(df)
    if args.cameofilt:
        df = lgc.filter_gdelt_events_by_cameo_base(df, lgc.CONFLICT_CAMEO_BASE_CODES)
    if args.reduce:
        df = lgc.reduce_events(df)
    converted_df = lgc.convert_gdelt_events_to_acled_format(df, country_converter)
    return converted_df, df


def pipesafe_write(output_path, write_func):
    exit_code = 0
    if output_path == '-':
        opf = sys.stdout
    else:
        opf = open(output_path, 'w')
    try:
        write_func(opf)
    except BrokenPipeError:
        import os
        # from <https://www.quora.com/How-can-you-avoid-a-broken-pipe-error-on-Python>:
        # Python flushes standard streams on exit; redirect remaining output
        # to devnull to avoid another BrokenPipeError at shutdown
        devnull = os.open(os.devnull, os.O_WRONLY)
        os.dup2(devnull, sys.stdout.fileno())
        exit_code = 1  # Python exits with error code 1 on EPIPE
    finally:
        opf.close()
    return exit_code


def parse_options():
    import argparse

    parser = argparse.ArgumentParser(description='Convert GDELT events data into an ACLED-like format, ' +
                                                 'retaining only events in specified countries and of specified types.')
    parser.add_argument('gdelt_path', type=str, help="Path to GDELT events file")
    parser.add_argument('output_path', type=str, help="Path to output csv file; default is to write to stdout",
                        default='-', nargs='?')
    parser.add_argument('--filtered_path', type=str, help="Path to write filtered events without conversion to ACLED " +
                        "format; default is not to write", default=None)
    parser.add_argument('--reduce', action="store_true", help="Collapse events on date, actors, and event type",
                        default=False)
    parser.add_argument('--no_cameo_filt', dest='cameofilt', action="store_false",
                        help="Do not run filtering based in cameo event codes, instead keeping all cameo events.")

    return parser.parse_args()


if __name__ == '__main__':
    args = parse_options()
    converted_df, filtered_df = run(args)
    exit_code = pipesafe_write(args.output_path, lambda fp: converted_df.to_csv(fp, index=False))
    if args.filtered_path:
        exit_code = exit_code | pipesafe_write(args.filtered_path, lambda fp: filtered_df.to_csv(fp, index=False))
    sys.exit(exit_code)
