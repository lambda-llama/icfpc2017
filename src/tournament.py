import ast
import os
import urllib2
import re
import subprocess
from threading import Thread
from collections import namedtuple, defaultdict

Room = namedtuple('Room', ['players_in', 'players_total', 'map_name', 'port'])
port_pattern = re.compile("""<td>\w*</td><td>(9\d+)</td>""")
status_pattern = re.compile("""(\d+)\/(\d+)""")
map_pattern = re.compile("""/(\w+).json""")
STRATEGY = None

wins = defaultdict(int)
games = defaultdict(int)

def get_rooms():
    page = urllib2.urlopen("http://punter.inf.ed.ac.uk/status.html").read()

    rooms = []

    lines = page.split("\n")
    for line in lines:
        status = re.search(status_pattern, line)
        port   = re.search(port_pattern, line)
        map_name = re.search(map_pattern, line)

        if status and port and map_name:
            room = Room(players_in=int(status.group(1)), players_total=int(status.group(2)), map_name=map_name.group(1), port=int(port.group(1)))
            rooms.append(room)
    print 'n rooms: ', len(rooms)

    return rooms

def get_score(port, strategy):
    command = ["dotnet", "Icfpc2017.App/bin/Release/netcoreapp2.0/Icfpc2017.App.dll", str(port), strategy]
    try:
        s = subprocess.check_output(command, stderr=subprocess.STDOUT)
        #print s
        meta = ast.literal_eval(s.splitlines()[-1])
        return meta["me"], meta["scores"]
    except Exception as e:
        print "FAILED", command
        return None

def print_stats(strategy):
    for map_name in games:
        print "strategy: {}, map: {}".format(strategy, map_name)
        print "\ttotal games: {}".format(games[map_name])
        print "\twins: {}".format(wins[map_name])

def thread_func(strategy, room):
    result = get_score(room.port, strategy)
    print result
    if result is not None:
        me, scores = result
        print me, scores
        games[room.map_name] += 1
        wins[room.map_name] += 1 if scores[me] == max(scores) else 0
        print_stats(strategy)

if __name__ == '__main__':
    import sys
    import random
    import time
    n_threads = 20

    strategy = sys.argv[1]
    workers = []

    while True:
        rooms = get_rooms()
        random.shuffle(rooms)
        spawned_threads = 0

        for room in rooms:
            if room.players_in + 1 == room.players_total and spawned_threads < n_threads:
                worker = Thread(target=thread_func, args=(strategy, room))
                worker.daemon = True
                worker.start()
                workers.append(worker)
                spawned_threads += 1

        time.sleep(10)
        remaining_workers = []
        for w in workers:
            w.join(1)
            if w.isAlive():
                remaining_workers.append(w)
        print 'remaining {} workers'.format(len(remaining_workers))
        workers = remaining_workers
