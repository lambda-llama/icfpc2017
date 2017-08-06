import ast
import os
import urllib2
import re
import subprocess
from threading import Thread
from collections import namedtuple, defaultdict

Room = namedtuple('Room', ['players_in', 'players_total', 'map_name', 'port'])

re._MAXCACHE=1000


port_pattern = re.compile("""<td>\w*</td><td>(9\d+)</td>""")
status_pattern = re.compile("""(\d+)\/(\d+)""")
score_pattern = re.compile("""{"sortedScores": "\[([0-9,; ]*)\]" "me": "(\d?)"}""")
map_pattern = re.compile("""/(\w+).json""")

acc_wins = defaultdict(int)
acc_scores = defaultdict(int)
acc_games = defaultdict(int)

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
            print room
            rooms.append(room)

    return rooms

def get_score(port, strategy):
    command = ["dotnet", "Icfpc2017.App/bin/Debug/netcoreapp2.0/Icfpc2017.App.dll", str(port), strategy]
    try:
        s = subprocess.check_output(command, stderr=subprocess.STDOUT)
        print s
        meta = ast.literal_eval(s.splitlines()[-1])
        return meta["me"], meta["scores"]
    except Exception as e:
        print "FAILED", command
        return None

def print_stats():
    for strategy in acc_games:
        print "strategy: {}".format(strategy)
        print "\ttotal games: {}".format(acc_games[strategy])
        print "\twins: {}".format(acc_wins[strategy])
        print "\tmean scores: {}".format(acc_scores[strategy]/acc_games[strategy])

def thread_func(strategy, room):
    result = get_score(room.port, strategy)
    print result
    if result is not None:
        me, scores = result
        print me, scores
        acc_games[strategy] += 1
        acc_scores[strategy] += scores[me]
        acc_wins[strategy] += 1 if scores[me] == max(scores) else 0
        print_stats()

if __name__ == '__main__':
    #s = """{"sortedScores": "[9; 27]" "me": "1"}"""
    #scores = re.search(score_pattern, s)
    #print scores.group(1), scores.group(2)
    #exit(0)


    import sys
    import random
    import time
    n_threads = 10

    strategy = 'minimax2'

    rooms = get_rooms()
    workers = []

    for room in rooms:
        if room.players_total - 1 == room.players_in and n_threads > 0:
            worker = Thread(target=thread_func, args=(strategy, room))
            worker.daemon = True
            worker.start()
            workers.append(worker)
            n_threads -= 1

    for w in workers:
        w.join()

    #print acc_wins, acc_scores, acc_games
