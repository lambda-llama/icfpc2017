import os
import urllib2
import re
import subprocess
from threading import Thread

re._MAXCACHE=1000


port_pattern = re.compile("""<td>\w*</td><td>(9\d+)</td>""")
score_pattern = re.compile("{\"sortedScores\": \"([0-9,]*)\" \"me\": \"(\d?)\"}")

def get_ports():
    page = urllib2.urlopen("http://punter.inf.ed.ac.uk/status.html").read()
    ports = []
    for k in re.findall(port_pattern, page):
        ports.append(k)

    return map(int, ports)

def get_score(port, strategy):
    command = ["dotnet", "Icfpc2017.App/bin/Debug/netcoreapp2.0/Icfpc2017.App.dll", str(port), strategy]
    try:
        s = subprocess.check_output(command, stderr=subprocess.STDOUT)
        scores = re.search(score_pattern, s)
        print scores.group(0), scores.group(1), scores.group(2)
        return int(scores.group(2)), map(int, scores.group(1).split(","))
    except:
        return None

def print_stats(acc_wins, acc_scores, acc_games):
    for strategy in acc_games:
        print "strategy: {}".format(strategy)
        print "\ttotal games: {}".format(acc_games[strategy])
        print "\twins: {}".format(acc_wins[strategy]/acc_games[strategy])
        print "\tmean scores: {}".format(acc_scores[strategy]/acc_games[strategy])

if __name__ == '__main__':
    import sys
    import random
    from collections import defaultdict
    import time

    #ports = range(9016, 9021)
    ports = range(9096, 9100)
    strategies = ['gready']#'bruteForce1|bruteForce3|gready|minimax'.split("|")
    #strategies = 'bruteForce1|bruteForce3|gready|minimax'.split("|")

    acc_wins = defaultdict(int)
    acc_scores = defaultdict(int)
    acc_games = defaultdict(int)

    n_threads = 50
    def thread_func():
        time.sleep(random.randint(1, 100))
        port = random.choice(ports)
        strategy = random.choice(strategies)
        result = get_score(port, strategy)
        print result
        if result is not None:
            me, scores = result
            print me, scores
            acc_games[strategy] += 1
            acc_scores[strategy] += scores[me]
            acc_wins[strategy] += 1 if scores[me] == max(scores) else 0
            print_stats(acc_wins, acc_scores, acc_games)

    while True:
        workers = []
        for i in xrange(n_threads):
            worker = Thread(target=thread_func, args=())
            worker.daemon = True
            worker.start()
            workers.append(worker)

        for w in workers:
            w.join()

    print acc_wins, acc_scores, acc_games

