# day20 of advent of code 2020
# https://adventofcode.com/2020/day/20
#
# testbench: python3 -im day20 and poke around.
from typing import List, Tuple, Dict, NamedTuple, Set, Optional
import re


def check(name, actual, expected):
    "Checks equality and provides a clear error message on failure."
    assert expected == actual, f'not ok - {name}: expected {expected}, got {actual}'


example = '''
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
'''

puzzle_input = '''
Tile 2129:
.#.#..##..
..#..#....
..........
...###..#.
..#.......
#...#...##
#...#.....
#...#.#...
##.......#
.#..###..#

Tile 2861:
##..##.###
#..#......
.#...#....
#.#....#..
....#.#...
.#......##
#.#..#.#.#
........#.
#.........
..#..###.#

Tile 1789:
#...##..##
...#.##...
#.....#...
....#.....
.#...#....
.....#..##
....#..#.#
.....##..#
.........#
##.#...##.

Tile 2371:
..##.##..#
..#.##....
..........
#..##..#..
....##.#..
....#...#.
...#.##...
...##....#
#..#.#....
##.#.##..#

Tile 3613:
#######..#
#........#
.#......##
#...#.....
...##....#
#.#..#..#.
##.......#
#.#.#..#.#
.##..#..#.
##..######

Tile 1697:
.##..#####
........#.
..##......
....#.#..#
#...#.#..#
#..##..#.#
#......#.#
..#......#
#..#.....#
.#.#.#.#..

Tile 1607:
#.#...#..#
#.#.##...#
.#..#.....
##........
#.........
.....#.###
...#...#.#
#......#.#
........#.
##.#.####.

Tile 1657:
#.#.#....#
..####...#
##........
.#.....#..
#..#......
##.##...#.
.##..#..##
#.#.##...#
#....##...
...#...#..

Tile 1249:
.#.#......
.#.......#
#..#....##
#..###.#.#
##.....#..
.##......#
..........
.##...##..
..#....#.#
.#..####.#

Tile 1223:
#....#..#.
..#....#.#
..#.#..#.#
#.#.....#.
#........#
#....#..#.
#.....#...
#.........
####......
#...###.##

Tile 1373:
.....##.##
#..#....##
#.........
##...#....
.#.......#
...##.....
..#......#
#.......#.
..........
##.##.#.#.

Tile 1109:
.#.###....
#........#
#.#..#...#
.........#
#........#
.#..#....#
#.##......
....#.##.#
...###..#.
...#.##.##

Tile 3607:
##.#...###
#.......#.
.#.##.#...
....##..#.
..#...#..#
.....#..##
.....###..
##..#.#...
.##.......
.#...###..

Tile 2309:
.##.#...##
..#..#...#
##...#.#..
#........#
#........#
.#..#.....
.....#.#..
#...#....#
........##
##...##.##

Tile 3041:
.#..#.####
#.#.....##
..#....#.#
#.......##
#......#.#
.#.......#
##...#....
##..#...##
##...#...#
..#.##....

Tile 1427:
.##..###..
.....###.#
#.........
.........#
#.#..##..#
###..##.#.
##....##..
#........#
....#....#
####..##.#

Tile 3461:
.#.####...
..........
.......#.#
........#.
#.........
..#......#
#....#...#
.#.......#
......#...
..###.....

Tile 1327:
#..#####..
#....#....
#..#......
....##..##
.......#.#
##....#...
###.......
........#.
..##.....#
..#..#.#..

Tile 1289:
.#.##..##.
#..#......
.......#.#
...#.#....
#.##...#.#
##..#...##
##.......#
...#....#.
####..#...
#...#.###.

Tile 2857:
##.#####.#
.........#
.#.##..#..
###..#....
##....#...
...#.#.#.#
#.......#.
##.##....#
#...##.##.
###.##..##

Tile 3823:
#.........
..#.......
...#......
#.........
#...##.#.#
..##.#....
..##.....#
.#..#.#...
#...#....#
.###...#..

Tile 3557:
.##.#.##.#
#..#.#..##
....###.##
##...#..#.
#...#.#..#
#...###..#
#...#..#..
.##.#..#..
###..#....
##..##..#.

Tile 2713:
...##.###.
#.........
.........#
..##...#..
.....#...#
.......##.
...#......
#.#....#..
#.#.......
.#....###.

Tile 1213:
.##..#.##.
#.#.......
........#.
.....##...
....#....#
#....#.###
...##....#
#.#...#..#
#...##...#
.###..##..

Tile 2341:
.#######..
..#....##.
####...#..
.###.....#
#....#..#.
...#...##.
....##...#
#........#
#..#.....#
.#.#..##..

Tile 3467:
....#..##.
....##.#.#
....##...#
##..###...
...#......
#......#..
#.....#..#
....###..#
#....#....
..#.#.#.#.

Tile 1583:
.#.#.#.#.#
.#.#......
#...#.....
...###.#.#
##........
#..#...#.#
#...#....#
....#.....
#....##...
##..######

Tile 1087:
###.#.#..#
#.#.......
#.#......#
##........
.#.#...#.#
#........#
#.#......#
#......##.
.....#....
.##.#.#.##

Tile 3691:
###.#.....
#.#..#...#
.....##..#
#..#.##...
#........#
#.##.....#
.##.#....#
#........#
##......#.
#.##.###..

Tile 3881:
#.#..##...
#..#.....#
..#.#....#
###......#
#..##....#
....#..#.#
##....#.#.
.#.#....##
###.......
#...##.##.

Tile 1033:
...#.##..#
#.##...#..
......#.#.
..####....
....#..#..
#........#
..#..#...#
...#......
##...#.#.#
##.##.####

Tile 3361:
..##.#....
.........#
..##......
....##...#
##....#..#
...#....#.
.##......#
###......#
..#..#...#
.....##..#

Tile 1459:
#.#.##..#.
........#.
...#...#..
.#........
..#.#....#
#...###..#
#.#..##..#
#..#.#...#
...##.....
##.#...##.

Tile 3833:
.#..####.#
..#.#....#
###.####.#
..#..#..##
#.........
#.......#.
..........
......#.#.
#....#....
##......#.

Tile 3779:
..###...##
......#...
.....###..
####.....#
.......#.#
....#.#...
....##.#..
#...#....#
#...#....#
.#..#.....

Tile 2333:
...###.###
#..#......
.........#
#.#......#
#..#..#.#.
###....###
#.#.......
#.....#..#
.......#.#
.#..#...##

Tile 2099:
#..##.#..#
#.........
#........#
..#......#
#.........
#..##.##.#
#..##...#.
...###....
....#.#..#
.....#.##.

Tile 1801:
####.###.#
#..#.....#
#......#.#
..........
.....#..#.
..#...#...
.........#
#...#.....
.#.....#.#
#....##.#.

Tile 2237:
##..##.#..
..#.#..#..
...#..#..#
.....#...#
..#....#.#
...##.##..
#......#..
......####
......###.
.#.##.###.

Tile 2357:
...#.###.#
#..#.#...#
#..#..#...
##......#.
..........
#..##....#
..#..##...
#....#...#
#....#....
.#..#..##.

Tile 1597:
..#.###..#
...#......
#...#.....
...#.#.#..
#....#....
#....#....
.....#.#..
#..#..#.#.
#....#...#
.#...##.##

Tile 3229:
#.......##
#.........
#...###..#
..#.#.....
..........
.#.#.....#
#.#..#.###
.......#..
#...#....#
..#.##.###

Tile 2609:
...#.#.##.
..##......
#.#......#
#....#.#.#
#...#.#...
..##...##.
#..#...#..
.#.#.#..##
#.##.....#
...###....

Tile 2719:
#..#.#.###
##.#....#.
......##.#
.##.#.....
.##...##.#
###...#...
.........#
..........
....##....
######.##.

Tile 3407:
..#.######
.........#
#......#.#
#........#
#.........
#.#......#
#.....#..#
.........#
.#.#......
..###.###.

Tile 1063:
.####.###.
#..#.....#
#..#...#.#
.........#
#......#..
#.........
..#..#...#
.#..#.#..#
...#.##...
#.##.....#

Tile 2111:
.#.#######
..##...#..
.#.#......
...##.....
##.......#
.##...#..#
#........#
#.#.#.#...
#.........
#.#..####.

Tile 1499:
..#..###.#
.#.....#.#
.........#
##....#.#.
..#.##..##
##.#..#..#
....##....
........#.
#.........
.###.##.##

Tile 1283:
#...##..#.
......#.##
.....##...
..#....#..
......#..#
#........#
....#....#
##........
.#......##
.#..##.#..

Tile 3499:
...#.#.#.#
..#......#
#...##...#
#...#....#
...###.#.#
.#.#..#..#
#.....##..
..#.#..#.#
.#.##..#..
##....#...

Tile 2251:
##.##.....
###..#...#
#.....#...
....#..#..
#..#..#...
........#.
......#...
#.#.#....#
#....#....
#.##.#...#

Tile 2819:
..#...#..#
.#.#.##...
.#..##....
##...#.#..
##.......#
........#.
#..#..#...
.#...#.#.#
......#...
...###...#

Tile 2711:
.......#.#
#..#.##..#
....#.#...
..........
..........
#....#....
#..##....#
#....#....
#.........
.#########

Tile 2423:
#....####.
....#.#..#
....#.####
..#.....##
##.......#
##..#.#..#
...#.....#
#.....#...
.##...#..#
#.##.#....

Tile 3907:
..#..#.#..
##.......#
....##....
#....##..#
.......#..
#....#.#..
##.......#
###....##.
#....#..#.
#..##.#...

Tile 3929:
##.##.###.
...#..#.##
....#.....
#.#..#...#
.#......##
.....#....
....#..##.
.#........
..##.....#
.#..#.....

Tile 1879:
..#.###..#
##...#.#..
#........#
##.......#
#........#
....#...#.
#.......##
#..##....#
........##
##.###.###

Tile 1951:
#...###.#.
#..###...#
#......#..
#.#.....#.
#.##.....#
...###....
.#.#....#.
.........#
.#.#......
.##.#.#..#

Tile 2503:
#.#..##...
#.....##.#
#.........
..###...#.
#.#.#....#
###...#...
.##..#...#
...#......
###..#...#
.....#.#..

Tile 3539:
.####.#.##
....##.#.#
#.......#.
.#.#.#..##
.#.#.....#
#...##..#.
...#.#....
...#...##.
##..#.##.#
.#####..#.

Tile 2039:
.##.#...##
....#....#
...#.....#
.......#.#
..##..#...
.........#
##........
#......#..
.#...#....
.##.#..##.

Tile 2243:
..####....
#....#...#
#........#
#...##...#
#.#...#...
#.........
##..#....#
...#......
#........#
.##....#.#

Tile 1103:
##..###.#.
...#..#..#
#...#.....
...#.#...#
.##..#..##
....#.#..#
#..##....#
#.......#.
#...#....#
.#..#..###

Tile 2693:
.#..##...#
#....#...#
..#.......
.........#
#.####.#..
#.....#...
....#..##.
......#..#
....#..#..
....##...#

Tile 2971:
#..#..##..
#.........
#...#.....
...#......
...#..#..#
.#.......#
#...#..#..
#.##.....#
......#...
.#.####...

Tile 1483:
.##.###..#
#......##.
..#.#.....
#...#..#.#
#..#....##
..##.##.##
.#.#..####
#.....##..
..#.......
..###..##.

Tile 3583:
###.#....#
.....#....
#...###...
#.....#...
......#.##
..........
.........#
##........
....#.#...
.##.##.#..

Tile 2179:
##.....###
....#.....
#.........
##.....#.#
##.......#
.####.....
#...#.#.#.
...#......
.....#....
....#...##

Tile 1567:
#.######..
###.......
..........
.....#.#..
#..#......
.......#.#
........#.
......#.#.
..........
#..#.####.

Tile 3851:
..###..#.#
.##..#..##
#..###.#..
..##..##.#
###.......
.......#..
#.###.#...
#..#....#.
##.##..#.#
##..####.#

Tile 3343:
##.##...##
....#.....
.......#.#
.....#..#.
#...##.#.#
.......##.
#..#.....#
...#......
.........#
.####.##.#

Tile 3319:
#.#.......
..#....#..
....#...#.
.##......#
.....#....
..#.#.....
#...#...#.
#.#...#..#
###.......
#..##.....

Tile 3947:
.#..#..##.
.##.......
##....#...
.#.#......
..#.#.#..#
.....#...#
......#...
..#.#.####
#...#....#
.#.##..#..

Tile 1559:
.....#...#
##....#...
##......##
#.###..###
##..#.....
##.......#
...###.#.#
#.......#.
..##....#.
##..####.#

Tile 2063:
#.#..#.#..
...#.....#
#........#
.#.#....##
#.#.#....#
......#..#
#..##....#
.##...#...
##...#....
.#.##..##.

Tile 3163:
..##.#.##.
....#.#...
#.........
#..###...#
#..#..#..#
#..#....##
#......#.#
.....#....
#..#....#.
##..#.##.#

Tile 3659:
.#....#..#
##.......#
#.....#...
#..#.....#
#.#......#
###.#..###
##...#####
.#....##.#
#.#....#..
####.##.#.

Tile 1229:
....###...
...#.#...#
....#....#
.......###
......#.##
#....###..
#......#.#
......#..#
#..#...#..
.......#..

Tile 1433:
.###.....#
....#.#...
##....#..#
.....#.#.#
..........
.##...#..#
#.#..#....
#....##...
.....###.#
....####.#

Tile 1487:
.##.###.##
####..#.##
###..#....
....#.....
..........
##.#...#.#
..#...#..#
#.#.......
.##..#...#
...##.#...

Tile 2939:
.#....###.
##........
##........
##......#.
.#........
.#.##..#..
.#.#.##..#
..........
...#......
#..###.##.

Tile 2753:
###.#...##
##....#...
##.#.#...#
#.....#..#
.........#
##.......#
#........#
#.#.#..#..
#....#..##
.###..##.#

Tile 1429:
..##....#.
......#..#
#.#..##...
#.......##
#.........
#.....#...
....#.#.##
#...#.....
#......#..
...#.#.#..

Tile 3371:
.#........
.........#
#........#
#.###.#..#
....###..#
.##.##.#..
###.##....
##....#.#.
...##....#
#.#.##.##.

Tile 1973:
..##.#....
...#...#..
.###..#.##
.#.#.....#
#...#.....
..#..#.#.#
##...#.#..
....#..#.#
#.##...#.#
#.####.#..

Tile 3967:
..#..#.##.
#..#..#...
##..#....#
.##.....#.
...#.....#
..#.#....#
........##
#..##..#..
..........
#..##.###.

Tile 2207:
.##...###.
#...#.##..
....#..#.#
..........
##..#.....
..........
#..#.....#
#.....#..#
.........#
#.#..#.#..

Tile 1669:
....#..#.#
...#...#.#
#..#...#..
#.....#..#
#.##.#....
...#....##
#.........
#.........
.........#
###....#.#

Tile 1361:
#....#..##
.......#.#
#...#.#...
.........#
##..#.#.##
.........#
..#..##...
#....#....
....##....
..#..#.###

Tile 2851:
##..#...##
##.#....#.
##.....###
.#.##.....
...##....#
#...##...#
##..#...##
.......#.#
#.#...#.#.
.##.##....

Tile 1217:
...#.#.#..
#.#..#.#.#
#......#..
.#..##..#.
#..#......
##..#....#
#..##...#.
#..#...#..
.#.....#.#
#..##.##.#

Tile 3631:
......####
#...#.#.##
......##..
...#..#.#.
#......#.#
.#.#...#.#
#...##...#
#......#..
#......#..
...##..#..

Tile 3571:
#....#.#..
#.....#...
......#.#.
..........
#...#.....
.#....#...
..##.###..
#.##....##
#.........
.......##.

Tile 3067:
##........
.........#
#...#.....
...#....##
#........#
##.......#
#.....#..#
#.........
...#..#...
##.....#.#

Tile 2069:
.#.##.....
#......###
..#.##...#
.##..#....
#.....###.
#.#...##.#
........#.
#.........
..........
..#.#.##..

Tile 2551:
##.#.##...
....##..#.
......#...
#.........
..#..#...#
......#...
...#.##.##
...#...#..
#......#..
#..#.###..

Tile 1747:
##.#......
#....#.#.#
....#....#
....#..#..
#..#...#.#
#....#....
#.........
#.#...##..
.......#..
..######.#

Tile 2017:
.#####....
##.##.##.#
.........#
#.##.....#
#.....#.#.
......#.##
#.##......
#.....#..#
....#.#...
.#.#.###..

Tile 2789:
..#.....##
##.....#.#
#.....##..
#.#...#...
.......#.#
.#.#...#.#
.#.......#
##.....#.#
#.#..##...
#######...

Tile 2477:
####.##.##
.##.......
#.#....#..
#.#....#..
#..##..#.#
##...##...
#....#....
......#...
..........
#####..##.

Tile 3299:
#.##..####
.......#.#
#.###..#.#
#.#..##.#.
##.#..##..
.......#..
#.........
....#.#...
#..##.#...
.##...#..#

Tile 3169:
#...##.###
#.....##.#
#.#.#..#.#
#.......#.
#...#.....
.#...#....
#.#.......
#...#.....
..#...#..#
...##.#..#

Tile 2887:
#.#..#...#
.#...##.#.
#......#.#
#.....##.#
#.....##.#
#........#
..........
....#...##
.....#.#..
#...#.##.#

Tile 3917:
#....#.#.#
.##..##.#.
##....##..
#.##.....#
....#.....
#...#.#...
........##
##...#....
..........
##..#.#...

Tile 3761:
...#.##.##
.....##..#
#.....##.#
...#..#..#
####...##.
.........#
#.#..#....
#######.##
#......#..
..##......

Tile 1511:
##.#.##.##
....#....#
..##....##
##.##..#..
....###.#.
#..###...#
.#........
#..#.#....
....#.....
.###.#.##.

Tile 3023:
.#.###.#..
.....#...#
.......#..
.#..#....#
....###..#
###.#..###
..........
#.........
#.....#..#
..###.#.##

Tile 3221:
.......##.
##..#.#..#
...#......
#.........
#...#.#...
#..#.#...#
.###....#.
.#..#..#.#
.###.#..#.
###.##....

Tile 1009:
.#.#....##
.#...#....
..#.##...#
..##...##.
#..#......
...####..#
..........
.#.###.#..
...##....#
...#######

Tile 1931:
..##.#####
.....#....
#..##.....
#.#.....##
#####..#..
.....#....
.#........
#.......#.
#.....#...
##..##....

Tile 1783:
#..##.####
....#..#.#
...#...#..
......#...
...##.....
...##....#
#...##..#.
##..##....
...#...#..
#..#.#.#..

Tile 1129:
..#.#..###
##........
.#..#.#.##
....##...#
.......#.#
#.#.###...
.....#..##
..#..#....
#.#.....##
..#..#.###

Tile 1051:
.######.##
.#..#...##
.#....####
.......#.#
.....#..#.
......#...
#...#.##..
.##.......
#.........
..###.#..#

Tile 1693:
.##.##.###
..........
.....#....
#....#.#..
#.....#...
#...#.##.#
#.#..#...#
#.#.#.#..#
.##.......
..##.##..#

Tile 3719:
...##..#..
#...#.....
...#.#..##
.#..##...#
.....#..#.
#.........
...#.#.##.
...#..#..#
......##..
.#.....##.

Tile 1493:
##..#...##
....#..#.#
#..#....#.
#.##.#.###
###.#.#...
.###.#...#
#..#.#..#.
.....##...
#.#...#..#
...#..###.

Tile 1013:
.##.###.##
..........
....#..#.#
........##
....#..#..
#.##.##.#.
#.........
#.##..##.#
#......#..
.#####..#.

Tile 2087:
...######.
#...###.##
#.#.......
..........
#.....#.#.
..#..#...#
.....#...#
#....#....
.#...#.##.
....#.##.#

Tile 1201:
##...#..#.
.....#..##
.........#
..#...#...
.....#.###
#..#..#...
.....#.#.#
.#........
#...##..#.
..#.#.##.#

Tile 1877:
...###..#.
#..###...#
...#...#.#
#......###
.#........
#.......#.
#.........
.#..#....#
#.#..##..#
##.#.###..

Tile 3697:
..#..#....
.........#
#..#.#....
#.##..#..#
.##..#.#.#
...#..#..#
.......#..
..##.....#
#........#
#.##..#...

Tile 1187:
#...####.#
#........#
...#..#.##
.........#
#........#
#..#..#..#
.........#
##.....##.
#.#....#.#
.##.#...#.

Tile 1303:
##..##.##.
..........
#.....#...
...##....#
....#.....
.###.#....
....##....
.#..#...##
#....#...#
.#..#..###

Tile 3109:
..##....#.
..#.#....#
....#..#..
...##.#...
#...#..#..
..#..#...#
.......#.#
#..#.###..
..#.....#.
####...##.

Tile 1553:
.#.#####..
#...##....
..#....#..
#....#.#..
#..#....#.
#.#.#.##.#
...##...##
#.....#..#
.........#
.##.#.##.#

Tile 2797:
.#.##...##
#.......##
##...#...#
####.#....
##.##...##
.....#....
.....#....
##..#...#.
...#....#.
##.##..###

Tile 3301:
###...##..
#.......#.
....#...#.
###.##.#.#
..##...###
#.....#...
##.......#
.#........
...#.....#
#.###..#.#

Tile 2707:
#.#.##...#
..###.....
..........
..#.#.#..#
..#....##.
.......#..
......#..#
.#..#.....
.#.....#.#
.#........

Tile 1409:
#..#..#.#.
#..#..#..#
.........#
......#..#
####..#.##
##..#..#.#
..#.#....#
.#......##
..#.#.#..#
#.###..#.#

Tile 1297:
..##..##.#
...#.##...
...#####..
....##...#
#.......#.
.#..#.....
#.#.#....#
..#.......
###.......
.#..###...

Tile 2729:
..##.##.#.
#...#.#.##
#..#.#.##.
###...##..
#.#....#.#
........##
#........#
#...#..###
.#....#...
.#...#.#..

Tile 3359:
##...#.#..
...#......
...#..#..#
##.......#
.......###
#...##.#.#
##.#..##..
.#....#..#
.....#....
.#.##.#..#

Tile 1321:
..####..#.
#..##...#.
.........#
.........#
##..#.##..
###..##.#.
...#.....#
.#....#..#
#..#.##..#
###...#...

Tile 1307:
......#.##
...#.#..##
###.#.#...
..#...##.#
#..#...#..
........#.
#...#..##.
#.....####
...#.....#
###..###.#

Tile 1949:
.#.####..#
#..##...#.
#......#..
..........
#...##....
#.####..#.
#.....#...
......##.#
#...#...##
....#.#.##

Tile 3637:
..#######.
#....#.#..
#.....#..#
..#.......
........#.
.#......#.
#.....#..#
..##..#...
.#...#....
#.#.....##

Tile 1601:
##..##....
.##......#
........##
#.........
#.#.......
##.#......
..#.#....#
..........
##...#.#..
.#..#..#..

Tile 3389:
.###.#..##
#.......##
.#......#.
..###.....
....#....#
........##
#...#....#
.......#..
...#......
#.####....

Tile 3253:
##.###.#..
#.#.....##
#.........
..........
...#.....#
#........#
..#....#.#
#..#......
....#....#
.#...#.###

Tile 1933:
.#..#.#.##
........#.
..#..#....
.....#...#
.......#..
#.#..##..#
.........#
..#..#..#.
#.#......#
###..###..

Tile 3463:
......#...
###......#
...#.#....
..#..##.#.
##....#...
#..#...#..
.#....#.#.
###...####
#..#.#.#..
###..#....

Tile 1279:
.##..#..##
##.....#.#
....#.....
..#......#
#.#.#.....
.....#....
..##......
#.....##.#
...#...#..
###..###.#

Tile 3793:
...#..##.#
....#.##..
..##....##
........##
.....#....
........##
......#...
.........#
#.........
###..#.###

Tile 2671:
##.#.#.#.#
#.##..#..#
#.##..###.
.........#
#..#.#....
....##..#.
....##.#..
.#.#......
....#..#.#
.##.####.#
'''

Tile = Tuple[int, List[str]]
ID = 0
ROWS = 1

# A tiny tile that is easy to write tests against.
SIMPLE_TILE = (-1, [['A', 'B'], ['D', 'C']])
# No rotations needed.
SIMPLE_EAST_MATCH = (-2, [['B', 'X'], ['C', 'Y']])
# Requires a flip and 3 rotates to become DC/XY.
TRICKY_SOUTH_MATCH = (-3, [['D', 'X'], ['C', 'Y']])


def parse_input(text: str) -> List[Tile]:
    sections = text.split('\n\n')
    tiles = []
    for s in sections:
        lines = s.strip().splitlines()
        id = int(lines[0][:-1].split()[-1], 10)
        rows = lines[1:]
        tiles.append((id, [list(r) for r in rows]))
    return tiles


def show_tile(tile: Tile, header=False) -> str:
    h = f'Tile {tile[ID]}:'
    lines = [''.join(r) for r in tile[ROWS]]
    body = '\n'.join(lines)
    if header:
        return h + '\n' + body
    else:
        return body


def print_tile(tile: Tile, header=False):
    print(show_tile(tile, header))


def col(i0, tile):
    "Returns the ith column ordered from top to bottom."
    rows = tile[ROWS]
    return [row[i0] for row in rows]


assert col(0, SIMPLE_TILE) == ['A', 'D'], "col is busted"


def tile_edge(tile: Tile, dir: str) -> List[str]:
    "N, E, S, or W edge"
    d = dir.lower()
    if d == 'n':
        return tile[ROWS][0]
    elif d == 's':
        return tile[ROWS][-1]
    elif d == 'e':
        return col(-1, tile)
    else:  # w
        return col(0, tile)


def rotate(tile: Tile) -> Tile:
    rows = tile[ROWS]
    rows_next = [col(i, tile) for i in range(len(rows[0]))]
    for row in rows_next:
        row.reverse()
    tile_next = (tile[ID], rows_next)
    return tile_next


assert rotate(SIMPLE_TILE) == (-1, [['D', 'A'], ['C', 'B']]
                               ), f'rotate error: expected DA/CB, got {rotate(SIMPLE_TILE)[ROWS]}'


def flip(tile: Tile) -> Tile:
    "Flips side to side."
    rows = tile[ROWS]
    rows_next = [list(reversed(row)) for row in rows]
    tile_next = (tile[ID], rows_next)
    return tile_next


assert flip(SIMPLE_TILE) == (-1, [['B', 'A'], ['C', 'D']]
                             ), f'flip error: expected BA/DC, got {flip(SIMPLE_TILE)[ROWS]}'


def variants(tile: Tile) -> List[Tile]:
    "All rotations, with and without flipping."
    accumulator = []
    for flipped in [False, True]:
        for rotations in range(4):
            next = (tile[ID], tile[ROWS])
            if flipped:
                next = flip(next)
            for _ in range(rotations):
                next = rotate(next)
            accumulator.append(next)
    assert len(
        accumulator) == 8, f'expected 8 variants total, but got only {len(accumulator)}'
    assert tile in accumulator, 'expected the original tile to be included amongst the variants'
    return accumulator


# Trigger embedded assertions.
variants(SIMPLE_TILE)


class Pos(NamedTuple):
    "A position. X or Y can be negative as needed."
    x: int
    y: int


Grid = Dict[Pos, Tile]


def neighbors(p: Pos) -> List[Pos]:
    "Returns W, E, N, S positions."
    x, y = p
    deltas = [-1, 1]
    result = [Pos(x + dx, y)
              for dx in deltas]
    result.extend([Pos(x, y + dy) for dy in deltas])
    return result


check('neighbors', neighbors((0, 0)), [(-1, 0), (1, 0), (0, -1), (0, 1)])


OPPOSITE_DIR = dict(n='s', e='w', s='n', w='e')


def opposite_edge(dir: str) -> str:
    return OPPOSITE_DIR[dir.lower()]


def fits(edge: str, of: Tile, to: Tile):
    "Whether tile's n/e/s/w edge fits against the other tile, as-is, without performing any rotation or flip."
    ok = tile_edge(of, edge) == tile_edge(to, opposite_edge(edge))
    return ok


check("fits", fits('e', SIMPLE_TILE, SIMPLE_EAST_MATCH), True)


def fitting_at(tile: Tile, grid: Grid, pos: Pos) -> Optional[Tile]:
    "If tile fits, then returns a copy rotated as appropriate."
    # get any extant neighbors
    surrounding_tiles = zip([grid.get(p) for p in neighbors(pos)], 'wens')
    surrounding_tiles = [(tile, dir)
                         for (tile, dir) in surrounding_tiles if tile]
    # print('surrounding_tiles', [(d, show_tile(n))
    #                             for (n, d) in surrounding_tiles])

    # check all variants against them
    for variant in variants(tile):
        # print(f'checking {show_tile(variant)}')
        # check edges against neighbor edges
        ok = True
        for neighbor, dir in surrounding_tiles:
            ok = fits(dir, variant, neighbor)
            if not ok:
                # print(f'NO: {dir} side of {variant} does not fit {neighbor}')
                break
        if ok:
            return variant
    return None


check("fitting_at", fitting_at(TRICKY_SOUTH_MATCH, {Pos(0, 0): SIMPLE_TILE}, Pos(
    x=0, y=1)), rotate(rotate(rotate(flip(TRICKY_SOUTH_MATCH)))))


def arrange(tiles: List[Tile]) -> Grid:
    "Returns a mapping from location to (already flipped and rotated as appropriate) tile."
    # Start with an empty grid
    grid: Grid = dict()
    # And empty set of edge@pos pairs
    # Or maybe just use Pos?
    frontier: Set[Pos] = set()
    # And full pool
    pool = tiles[:]

    # Seed with some tile as it is
    grid[Pos(0, 0)] = pool.pop()
    for n in neighbors(Pos(0, 0)):
        frontier.add(n)

    # Now chain out the frontier till all assigned, rotating as needed
    while frontier:
        loc = frontier.pop()
        fit = None

        for tile in pool:
            fit = fitting_at(tile, grid, loc)
            if fit:
                grid[loc] = fit
                pool.remove(tile)
                for n in neighbors(loc):
                    frontier.add(n)
                break

    return grid


def show_grid(g: Grid, header=True) -> str:
    positions = sorted(g.keys(), key=lambda p: (p[1], p[0]))
    rows = list(set([y for x, y in positions]))
    rows.sort()
    lines = []
    # FIXME: need to actually grab all the tiles in a row, then glue them row by row together. cannot just concat them.
    ncols = len(g[positions[0]][ROWS][0])
    for y in rows:
        positions = sorted([p for p in g if p[1] == y])
        tiles = [g[p] for p in positions]
        headers = []
        for tile in tiles:
            header = f'Tile {tile[ID]}'
            delta = ncols - len(header)
            if delta > 0:
                header += delta*' '
            headers.append(header)
        lines.append(headers)
        for scanline in range(len(tiles[0][ROWS])):
            row = []
            for tile in tiles:
                row.append(''.join(tile[ROWS][scanline]))
            lines.append(row)
        lines.append([])

    result = '\n'.join((' '.join(tiles) for tiles in lines))
    return result


def print_grid(g: Grid, header=True):
    print(show_grid(g, header))


E = parse_input(example)
R = arrange(E)
# print_grid(R)

# Part 2:
# Take the arrangement and strip the border off every arranged tile.


def tile_sans_borders(tile: Tile) -> Tile:
    (id, rows) = tile
    dropped_rows = rows[1:-1]
    dropped_cols = [r[1:-1] for r in dropped_rows]
    return (id, dropped_cols)


check("tile_sans_borders", tile_sans_borders(
    (0, ['XXXX', 'XYYX', 'XYYX', 'XXXX'])), (0, ['YY', 'YY']))


def grid_without_borders(grid: Grid) -> Grid:
    next: Grid = dict()
    for pos, tile in grid.items():
        next[pos] = tile_sans_borders(tile)
    return next

# Then merge them to make a 2d image.


def grid_to_image(grid: Grid) -> str:
    positions = sorted(grid.keys(), key=lambda p: (p[1], p[0]))
    rows = list(set([y for x, y in positions]))
    rows.sort()
    lines = []
    for y in rows:
        positions = sorted([p for p in grid if p[1] == y])
        tiles = [grid[p] for p in positions]
        for scanline in range(len(tiles[0][ROWS])):
            line = ''
            for tile in tiles:
                line += ''.join(tile[ROWS][scanline])
            lines.append(line)
    return '\n'.join(lines)


test_image = grid_to_image(grid_without_borders(R))

check("grid_to_image", test_image, '''.#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###''')

# Then search the 2d image using a filter. Keep trying variants till you get one with a match. Report the total number of matches.
# What makes this tricky is it's not just a single, simple regex, since it is working in a rectangular region of the image.
# In this filter, # must match exactly, but the spaces can be . or #.
SEA_MONSTER = '''                  #
#    ##    ##    ###
 #  #  #  #  #  #'''
MONSTER_CHOP = len([x for x in SEA_MONSTER if x == '#'])

MONSTER_REGEX = [
    re.compile('^..................#.'),
    re.compile('#....##....##....###'),
    re.compile('^.#..#..#..#..#..#...')
]


def tile_monsters(tile: Tile) -> int:
    "Returns the count of monsters."
    monster_count = 0
    rows = [''.join(row) for row in tile[ROWS]]
    for y in range(1, len(rows) - 1):
        m = MONSTER_REGEX[1].search(rows[y])
        while m:
            cols_to_drop = m.start()
            row_above = rows[y - 1][cols_to_drop:]
            row_below = rows[y + 1][cols_to_drop:]
            if MONSTER_REGEX[0].match(row_above) and MONSTER_REGEX[2].match(row_below):
                monster_count += 1
            m = MONSTER_REGEX[1].search(rows[y], m.end())
    # Easiest is probably to use regex to match the middle row of the monster, then if we get a hit, take rows around, whack off prefix to match index, and check them.
    return monster_count


HAS_SEA_MONSTER = (0, [list(x) for x in '''.#.#...#.###...#.##.##..
#.#.##.###.#.##.##.#####
..##.###.####..#.####.##'''.splitlines()])

check('tile_monsters', tile_monsters(HAS_SEA_MONSTER), 1)

DOUBLE_MONSTERS = (2, [list(x) for x in [2*'.#.#...#.###...#.##.##..',
                                         2*'#.#.##.###.#.##.##.#####', 2*'..##.###.####..#.####.##']])

check('double monsters', tile_monsters(DOUBLE_MONSTERS), 2)


def monster_count(image: str) -> int:
    fake_input = f'Tile 9001:\n{image}'
    tile = parse_input(fake_input)[0]
    # print_tile(tile, header=True)
    for variant in variants(tile):
        # print('\n\n\n\tchecking\n', show_tile(variant))
        n = tile_monsters(variant)
        if n > 0:
            print(f'found {n} monsters in variant:\n', show_tile(variant))
            return n
    return -1


check('monster_count', monster_count(test_image), 2)

# Then count the '#' that are not part of a match for the filter.
# The sea monsters are long enough we don't have to worry about them overlapping, so we can probably just count the # and subtract out MATCH_COUNT * the count of # in the monster to get the number.
# Test image has 303 #, and 2 monsters. Monsters have 15 #. 303 - 2*15 = 273, which is the expected amount, so this works!


def part2_answer(image: str) -> int:
    chop = len([x for x in image if x == '#'])
    n = monster_count(image)
    answer = chop - n*MONSTER_CHOP
    return answer


check('part2_answer', part2_answer(test_image), 273)

# x = 1
# ans = [x*c for c in corners(R)]
P = parse_input(puzzle_input)
A = arrange(P)
# print_grid(A)
part1_answer = 2753*1087*1327*1009  # 4006801655873

print('== SOLVING: Part 2! ==')
puzzle_image = grid_to_image(grid_without_borders(A))
final_answer = part2_answer(puzzle_image)
assert final_answer < 1883, 'final answer was too high'

print(f'Part 2: got {final_answer}')
print('== * ==')
