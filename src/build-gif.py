from graphviz import Source
import imageio
from pathlib import Path
import os
import sys

if __name__ == '__main__':
    os.chdir(sys.argv[1])
    dotfiles = list(Path('.').glob('*.dot'))
    for dotfilepath in dotfiles:
        with open(dotfilepath, 'r') as dotfile:
            graph = Source(dotfile.read(), engine='fdp', format='png')
            graph.render(filename=dotfilepath)
    images = []
    for i in range(0, len(dotfiles)):
        images.append(imageio.imread('%d.dot.png' % i))
    imageio.mimsave('replay.gif', images, duration=0.5)
