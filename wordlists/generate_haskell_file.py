"""
This script turns .list files into a file list.hs that contains native
haskell lists.
"""

import glob

class DoubleQuoteStr:
    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return '"{}"'.format(self.s)

def to_camel_case(snake_str):
    components = snake_str.split('_')
    # We capitalize the first letter of each component except the first one
    # with the 'title' method and join them together.
    return components[0] + "".join(x.title() for x in components[1:])

def parse_files(directory):
    outputs = [] # list of (name, lists)
    paths = glob.glob(directory + "/*.list")
    for path in paths:
        with open(path, mode='r') as f:
            name = path[path.rfind('/') + 1:]
            l = []
            for line in f:
                l.append(DoubleQuoteStr(line.strip()))

            l_name = to_camel_case(name[0:name.find('.')])

            outputs.append((l_name, l))

    for (name, l) in outputs:
        print('{} = {}'.format(name, l))
        print()

if __name__ == '__main__':
    parse_files('.')
