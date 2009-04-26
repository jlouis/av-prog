import sys, re

ROMAN = (
    (1000,  'M'),
    ( 900, 'CM'),
    ( 500,  'D'),
    ( 400, 'CD'),
    ( 100,  'C'),
    (  90, 'XC'),
    (  50,  'L'),
    (  40, 'XL'),
    (  10,  'X'),
    (   9, 'IX'),
    (   5,  'V'),
    (   4, 'IV'),
    (   1,  'I'))
def to_roman(n):
    if n == 0: return 'N'
    out = ''
    while n:
        val, letter = filter(lambda t: t[0] <= n, ROMAN)[0]
        n, out = n-val, out+letter
    return out

print re.sub(r'(\d+)', lambda m: to_roman(int(m.group(0))), open(sys.argv[0]).read())
