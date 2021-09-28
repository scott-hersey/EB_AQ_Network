# HEPA Pilot Analysis in R
## ADE Air Partners
### Author: Megan Ku

*Acknowledgements: Much of this was made possible by the foundational work done by Alli Busa in 2019.*

## Goals
The purpose of this code is to analyze collected PM and CPC data from several pilots located in the North Suffolk area.

## Project Structure
The root directory has a folder for data, images and folders each of the pilots.
- City Hall (`CH`)
- Revere High School (`RH`)
- Little Folks (`LF`)
- Shining Star (`SS`)

Within each folder are three scripts:
- `00_XX_merge_IO.Rmd`: merges indoor and outdoor CPC and PM data
- `01_XX_filter.Rmd`: manually removes non-normal indoor and outdoor sources
- `02_XX_boxplots.Rmd`: creates boxplots and calculates quartile values

The root directory has three helper `.R` files.
- `merge_data.R`: contains helper functions for `00_XX_merge_IO.Rmd` files
- `filter_data.R`: contains helper functions for `01_XX_filter.Rmd` files
- `make_boxplots.R`: contains helper functions for `02_XX_boxplots.Rmd` files

For file management, I would recommend storing original data in `./data/raw`, and if the folders don't exist already, creating folders within `data` called `merged` and `filtered`. I would also create an `images` folder in the root to store generated boxplots.
