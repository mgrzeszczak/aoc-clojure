import re
from queue import PriorityQueue
from functools import reduce


def parse_line(line):
    valve, flow_rate, valves = re.match(r'Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)',
                                        line).groups()
    return valve, int(flow_rate), valves.split(", ")


def read_data(path):
    with open(path) as f:
        for l in f:
            yield parse_line(l)


def neighbors(start, connections):
    return [(c, 1) for c in connections[start]]


def dijkstra(start, neighbor_fn):
    pq = PriorityQueue()
    pq.put((0, start))
    state = {start: 0}
    while not pq.empty():
        cost, current = pq.get()
        nodes_to_check = [(n, cost + d) for (n, d) in neighbor_fn(current)]
        nodes_to_update = filter(lambda x: state[x[0]] > x[1] if x[0] in state else True, nodes_to_check)
        for n, d in nodes_to_update:
            pq.put((d, n))
            state[n] = d
    return state


def p1(current, remaining, time, paths, flow_rates, cache):
    state = (current, frozenset(set(remaining)), time)
    if len(remaining) > 12:
        print(remaining)
    if state in cache:
        return cache[state]
    valve_score = flow_rates[current] * time
    if not remaining:
        cache[state] = valve_score
        return valve_score
    res = None
    for r in list(remaining):
        new_time = time - paths[current][r] - 1
        if new_time <= 0:
            continue
        remaining.remove(r)
        score = valve_score + p1(r, remaining, new_time, paths, flow_rates, cache)
        remaining.add(r)
        if not res or res < score:
            res = score
    res = res if res else valve_score
    cache[state] = res
    return res


def pairs(collection):
    for a in collection:
        copy = set(collection)
        copy.remove(a)
        for b in copy:
            yield a, b
            yield b, a


ELEPHANT = 0
PLAYER = 1


def p2(player, elephant, remaining, player_time, elephant_time, paths, flow_rates, cache, who_moved, current_score, best_score):
    state = (player, elephant, frozenset(set(remaining)), player_time, elephant_time, who_moved)
    if len(remaining) > 13:
        print(remaining)
    if state in cache:
        res = cache[state]
        if current_score + res > best_score[0]:
            best_score[0] = current_score + res
        return res

    reachable = filter(lambda r: max(player_time - paths[player][r] - 1, elephant_time - paths[elephant][r] - 1) > 0,
                       remaining)

    valve_score = flow_rates[player if who_moved == PLAYER else elephant] * (
        player_time if who_moved == PLAYER else elephant_time)

    threshold = sum(
        [flow_rates[r] * max(elephant_time - paths[elephant][r] - 1, player_time - paths[elephant][r] - 1) for r in
         reachable])

    if current_score + valve_score + threshold < best_score[0]:
        return valve_score

    if not remaining:
        cache[state] = valve_score
        if current_score + valve_score > best_score[0]:
            best_score[0] = current_score + valve_score
        return valve_score

    res = None

    for r in list(remaining):
        for c in [PLAYER, ELEPHANT]:
            time = player_time if c == PLAYER else elephant_time
            pos = player if c == PLAYER else elephant
            new_time = time - paths[pos][r] - 1
            if new_time <= 0:
                continue
            remaining.remove(r)
            if c == PLAYER:
                score = valve_score + p2(r, elephant, remaining, new_time, elephant_time, paths, flow_rates, cache,
                                         PLAYER, current_score + valve_score, best_score)
            else:
                score = valve_score + p2(player, r, remaining, player_time, new_time, paths, flow_rates, cache,
                                         ELEPHANT, current_score + valve_score, best_score)
            remaining.add(r)
            if not res or res < score:
                res = score
    res = res if res else valve_score
    cache[state] = res
    if current_score + res > best_score[0]:
        best_score[0] = current_score + res
    return res


if __name__ == '__main__':
    data = list(read_data('d16'))
    flow_rates = {v: fr for (v, fr, _) in data}
    connections = {v: vs for (v, _, vs) in data}
    neighbor_fn = lambda x: neighbors(x, connections)
    paths = {k: dijkstra(k, neighbor_fn) for k in flow_rates.keys()}
    working_valves = set([v for v in flow_rates.keys() if flow_rates[v] > 0])

    print(p1('AA', working_valves, 30, paths, flow_rates, {}))
    print(p2('AA', 'AA', working_valves, 26, 26, paths, flow_rates, {}, PLAYER, 0, [0]))
