import sys

from xpathwalker.args import parse_args


##########################################################################
# Main                                                                   #
##########################################################################

def main(argv):
    parse_args(argv)


if __name__ == "__main__":
    main(sys.argv[1:])

# Bench:
# wc -l
# 21302 /mnt/VM/home/lompik/Programmation/Web/d3_Map_GOM/Data/fldunit.json
# with multiproc:
# # string | .features[99].properties.UNIT_AGT_N
# # string | .features[99].properties.VERSION_DA
# # string | .features[99].type
# # string | .features[9].geometry.type
# # string | .features[9].properties.OUTLINE_TY
# # string | .features[9].properties.SDE_TEXT_L
# # string | .features[9].properties.UNIT_AGT_1
# # string | .features[9].properties.UNIT_AGT_N
# # string | .features[9].properties.VERSION_DA
# # string | .features[9].type
# # string | .type
# # ./xmlxpath.py  -a -M  374.99s user 29.73s system 569% cpu 1:11.05 total

# without multiproc:
# #  array | .features[9].geometry.coordinates[1][0][7]
# # number | .features[9].geometry.coordinates[1][0][7][0]
# # number | .features[9].geometry.coordinates[1][0][7][1]
# #  array | .features[9].geometry.coordinates[1][0][8]
# # number | .features[9].geometry.coordinates[1][0][8][0]
# # number | .features[9].geometry.coordinates[1][0][8][1]
# #  array | .features[9].geometry.coordinates[1][0][9]
# # number | .features[9].geometry.coordinates[1][0][9][0]
# # number | .features[9].geometry.coordinates[1][0][9][1]
# # string | .features[9].geometry.type
# # object | .features[9].properties
# # string | .features[9].properties.OUTLINE_TY
# # string | .features[9].properties.SDE_TEXT_L
# # number | .features[9].properties.SEQUENCE_N
# # number | .features[9].properties.SHAPE_Area
# # number | .features[9].properties.SHAPE_Leng
# # number | .features[9].properties.SN_UNIT_AL
# # number | .features[9].properties.SN_UNIT_OU
# # string | .features[9].properties.UNIT_AGT_1
# # string | .features[9].properties.UNIT_AGT_N
# # string | .features[9].properties.VERSION_DA
# # string | .features[9].type
# # string | .type
# # ./xmlxpath.py  -a  241.04s user 13.05s system 93% cpu 4:31.48 total
