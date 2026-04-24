# Batch script and configuration files for producing D-PLACE tree (version 2)

## Description

The batch script `dplace_v2.sh` takes the global language phylogeny 
`edge6636-March-2023-no-metadata.trees` (downloadable from 
[here](https://github.com/rbouckaert/global-language-tree-pipeline/releases/))
and runs the following steps:

1.  Grafts in ancient, extinct, colonial, and otherwise missing languages
2.  Removes taxa that do not have a D-PLACE society associated with glottocode
3.  Replaces taxa with society xd_ids
4.  Sets tip dates to focal times reported in D-PLACE
5.  Creates the output tree files (standard and human interpretable versions)

These steps produce a posterior treeset with 1258 D-PLACE societies.

## Instructions

To reproduce the D-PLACE tree in `data/tree/dplace.nxs`, you will need to:

1.  Download the file `edge6636-March-2023-no-metadata.trees` from 
[here](https://github.com/rbouckaert/global-language-tree-pipeline/releases/)
2.  Install [BEAST2](https://www.beast2.org/) and the Babel package
3.  Set file paths in the batch script to your local machine
4.  Run the batch script `dplace_v2.sh`

## Files

- `dplace_v2.sh` -- batch script
- `glotto_graft_map_v2.cfg` -- configuration file for grafting (see below)
- `glotto2society_v2.dat` -- tab-separated data file linking glottocodes to 
(multiple) society xd_ids
- `glottocodes_v2.dat` -- tab-separated data file listing glottocodes to retain
in the tree
- `soc2label_v2.dat` -- tab-separated data file listing human-readable names for
societies
- `society_id-focal_year_v2.tsv` -- tab-separated data file listing focal times
for societies

## Further details about grating configuration file `glotto_graft_map_v2.cfg`

Tab-separated configuration file containing three columns:

- Column 1: name of taxon
- Column 2: height (age) of taxon -- a range for the insertion age can be 
specified by adding the lower and upper bound with commas in between (i.e. 
"<tip age>,<lower parent age>,<upper parent age>")
- Column 3: a comma separated list of taxa determining MRCA to graft above in 
source tree (if no constraints have been specified). If lists of taxa are 
separated by a bar "|" instead of using the MRCA of all taxa, a taxon is 
randomly selected above the MRCA of the individual sets separated by bars, and
below the MRCA of all of these sets. (optional)
