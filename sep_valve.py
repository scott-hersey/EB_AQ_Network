'''
Separating indoor and outdoor data from 3-way valve.
Pretty ad-hoc code that's case-specific to the Revere High Principal's office
data taken in March 2021.

To get the raw file, I simply loaded the text files for the principal's office
in R as shown in hepa_main.Rmd, fixed the timezones, then exported the file as
a .csv.

Author: Megan Ku
'''

import csv

PERIOD_ROWS = 300
HOLD_NUM = 30
START_SWAP = 80
START_INSIDE = False
SHIFT = 15

if __name__ == "__main__":

    extra_row = 0
    row_num = 0
    inside = START_INSIDE
    swap_ctr = START_SWAP - HOLD_NUM
    hold = False
    hold_ctr = 2*HOLD_NUM
    adjusted = False

    with open('principal_valve_raw.csv', 'r') as f:
        reader = csv.reader(f)
        input_list = list(reader)

    outdoor_list = []
    indoor_list = []

    for row in input_list:
        if (row_num == 0):
            row_num += 1
            outdoor_list.append(row)
            indoor_list.append(row)
        else:
            row_num += 1


            # After a certain point, it seems that the delay on the RasPi adds
            # up. I wrote this pretty bad code to add more to the swap counter
            # to make up for it.
            if row_num > 70000 and not adjusted:
                if not hold:
                    adjusted = True
                    swap_ctr += SHIFT
            if row_num == 200000:
                adjusted = False
            # Data switches sources
            if (swap_ctr == 0):
                if not inside:
                    inside = True
                else:
                    inside = False
                if (extra_row == 1):
                    swap_ctr = PERIOD_ROWS - (2*HOLD_NUM)
                    extra_row = 0

                else:
                    swap_ctr = PERIOD_ROWS - (2*HOLD_NUM) + 1
                    extra_row += 1
                hold = True
                hold_ctr = 2*HOLD_NUM

            # omit transition between indoor and outdoor data to clean up noise
            if hold:
                hold_ctr -= 1
                if (hold_ctr < 1):
                    hold = False

            # Write to appropriate .csv
            # When I exported the CSV originally, the number column stayed.
            # This manually avoids that but should be fixed for usability later.
            else:
                swap_ctr -= 1
                if inside:
                    indoor_list.append(row[1:])
                else:
                    outdoor_list.append(row[1:])

    with open("revere_high_principal_cpc.csv", "w", newline='') as g:
        writer2 = csv.writer(g)
        writer2.writerows(indoor_list)

    with open("revere_high_outdoor_cpc.csv", "w", newline='') as h:
        writer3 = csv.writer(h)
        writer3.writerows(outdoor_list)
