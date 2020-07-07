import csv
from contextlib import ExitStack
from pathlib import Path

source_path = Path('E:/Research Projects/Worker Accidents and Pollution/Data/Deryugina-QCEW_naics2-OSHA.csv')
file_base_name = source_path.stem
dest_folder = source_path.parent / 'State files'

fips_state_dict = {
    1: 'AL', 2: 'AK', 4: 'AZ', 5: 'AR',
    6: 'CA', 8: 'CO', 9: 'CT', 10: 'DE',
    12: 'FL', 13: 'GA', 15: 'HI',
    16: 'ID', 17: 'IL', 18: 'IN', 19: 'IA', 20: 'KS',
    21: 'KY', 22: 'LA', 23: 'ME', 24: 'MD', 25: 'MA',
    26: 'MI', 27: 'MN', 28: 'MS', 29: 'MO', 30: 'MT',
    31: 'NE', 32: 'NV', 33: 'NH', 34: 'NJ', 35: 'NM',
    36: 'NY', 37: 'NC', 38: 'ND', 39: 'OH', 40: 'OK',
    41: 'OR', 42: 'PA', 44: 'RI', 45: 'SC',
    46: 'SD', 47: 'TN', 48: 'TX', 49: 'UT', 50: 'VT',
    51: 'VA', 53: 'WA', 54: 'WV', 55: 'WI',
    56: 'WY', 72: 'PR'
}

with open(source_path, newline='') as source_file:
    reader = csv.DictReader(source_file)
    fieldnames = reader.fieldnames

    with ExitStack() as stack:
        fips_file_dict = {fips: stack.enter_context(open(dest_folder
                                                         .joinpath(file_base_name + '_' + st_abbrev)
                                                         .with_suffix('.csv'), 'w', newline=''))
                          for fips, st_abbrev in fips_state_dict.items()}

        fips_writer_dict = {fips: csv.DictWriter(file_name, fieldnames=fieldnames)
                            for fips, file_name in fips_file_dict.items()}

        for writer in fips_writer_dict.values():
            writer.writeheader()

        counter = 0  # Set up a line counter so I can track the progress of the script
        for row in reader:
            counter += 1
            if counter % 100000 == 0:
                print(counter)

            if int(row['state_fips']) in fips_writer_dict:
                fips_writer_dict[int(row['state_fips'])].writerow(row)
