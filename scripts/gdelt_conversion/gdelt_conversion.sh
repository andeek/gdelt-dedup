# run from gdelt_conversion folder
python3 ./scripts/convert_all.py

# create single csv
python3 ./scripts/concat_zipped_text.py '../../data/gdelt/raw/*_gdelt*.zip' '../../data/gdelt/westafrica_conflict_evts_1979-2019_gdelt.csv.zip'

# unzip csv
unzip ../../data/gdelt/westafrica_conflict_evts_1979-2019_gdelt.csv.zip
mv westafrica_conflict_evts_1979-2019_gdelt.csv ../../data/gdelt
