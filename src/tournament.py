import ast
import os
import urllib2
import re
import subprocess
from threading import Thread
from collections import namedtuple, defaultdict

Room = namedtuple('Room', ['players_in', 'players_total', 'map_name', 'port', 'enemies'])
port_pattern = re.compile("""<td>\w*</td><td>(9\d+)</td>""")
status_pattern = re.compile("""(\d+)\/(\d+)""")
map_pattern = re.compile("""/(\w+).json""")
STRATEGY = None

wins = {}
games = defaultdict(int)

def get_enemies(s):
    # UGLY AS MY LIFE
    p1 = s.find(")</td><td>")
    s = s[p1 + 10:]
    p2 = s.find("""</td><td></td><td>""")
    s = s[:p2]
    return map(lambda x: x.strip(), s.split(","))

def get_rooms():
    page = urllib2.urlopen("http://punter.inf.ed.ac.uk/status.html").read()

    rooms = []
    lines = page.split("\n")
    for line in lines:
        if 'lambda-llama' in line:
            continue
        status = re.search(status_pattern, line)
        port   = re.search(port_pattern, line)
        map_name = re.search(map_pattern, line)
        enemies = get_enemies(line)
        if status and port and map_name:
            room = Room(players_in=int(status.group(1)), players_total=int(status.group(2)), map_name=map_name.group(1), port=int(port.group(1)), enemies=enemies)
            rooms.append(room)

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
        print 'strategy: {}, map_name: {}, wins/games: {}/{}'.format(strategy, map_name, sum(wins[map_name].itervalues()), sum(games[map_name].itervalues()))
        for enemy in games[map_name]:
            print "\tagainst {} wins/games: {}/{}".format(enemy, wins[map_name][enemy], games[map_name][enemy])

def thread_func(strategy, room):
    result = get_score(room.port, strategy)
    if result is not None:
        me, scores = result
        for enemy in set(room.enemies):
            if room.map_name not in games:
                games[room.map_name] = defaultdict(int)
            if room.map_name not in wins:
                wins[room.map_name] = defaultdict(int)

            games[room.map_name][enemy] += 1
            wins[room.map_name][enemy] += 1 if scores[me] == max(scores) else 0

if __name__ == '__main__':
    import sys
    import random
    import time
    n_threads = 50

    strategy = sys.argv[1]
    workers = []

    while True:
        rooms = get_rooms()
        random.shuffle(rooms)
        spawned_threads = 0

        for room in rooms:
            if room.players_in > 0 and spawned_threads < n_threads:
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
        print_stats(strategy)

