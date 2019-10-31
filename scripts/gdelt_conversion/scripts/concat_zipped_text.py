#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Example usage:
./scripts/concat_zipped_text.py '~/Documents/las/gdelt_conversion/output_180917/*_gdelt*.zip' \
  ~/Documents/las/gdelt_conversion/westafrica_conflict_evts_1979-2012_gdelt_180917.csv.zip
"""
import glob
import os
import zipfile


def parse_options():
    import argparse

    parser = argparse.ArgumentParser(description='Concatenates zipped text files into single zipped output file')
    parser.add_argument('input_glob', type=str, help="Glob pattern for input files")
    parser.add_argument('output_path', type=str, help="Path to output zip file")
    parser.add_argument('--header', action="store_true", default=False, help="Interpret first row of each input file" +
                                                                             " as a header; keep only header from " +
                                                                             "first file in output")

    return parser.parse_args()


if __name__ == '__main__':
    args = parse_options()

    outfilename = os.path.basename(args.output_path)
    if outfilename.lower().endswith(".zip"):
        outfilename = outfilename[:-4]

    with zipfile.ZipFile(args.output_path, 'w', compression=zipfile.ZIP_DEFLATED, compresslevel=6) as zf:
        with zf.open(outfilename, "w") as ozf:
            print(ozf)
            for idx, input_path in enumerate(sorted(glob.glob(os.path.expanduser(args.input_glob)))):
                print(input_path)
                with zipfile.ZipFile(input_path, 'r') as input_zip:
                    with input_zip.open(input_zip.namelist()[0]) as input_file:
                        if args.header and idx > 0:
                            # skip header row in each file after the first
                            _ = input_file.readline()
                        ozf.write(input_file.read())
