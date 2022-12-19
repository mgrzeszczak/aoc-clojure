import re
from queue import PriorityQueue
from functools import reduce
import math


def parse_line(line):
    id, ore_robot_ore_cost, clay_robot_ore_cost, obsidian_robot_ore_cost, \
    obsidian_robot_clay_cost, geode_robot_ore_cost, geode_robot_obsidian_cost = [int(i) for i in
                                                                                 re.findall(r'(\d+)', line)]
    return {
        'id': id,
        'ore_robot_ore_cost': ore_robot_ore_cost,
        'clay_robot_ore_cost': clay_robot_ore_cost,
        'obsidian_robot_ore_cost': obsidian_robot_ore_cost,
        'obsidian_robot_clay_cost': obsidian_robot_clay_cost,
        'geode_robot_ore_cost': geode_robot_ore_cost,
        'geode_robot_obsidian_cost': geode_robot_obsidian_cost
    }


def read_data(path):
    with open(path) as f:
        for l in f:
            yield parse_line(l)


def initial_state():
    return {
        'time': 0,
        'ore': 0,
        'clay': 0,
        'obsidian': 0,
        'geode': 0,
        'ore_robots': 1,
        'clay_robots': 0,
        'obsidian_robots': 0,
        'geode_robots': 0
    }


def eval_blueprint(blueprint, state, cache, max_time, best_holder, best_per_time):
    max_needed_ore_robots = max(blueprint['ore_robot_ore_cost'], blueprint['clay_robot_ore_cost'],
                                blueprint['obsidian_robot_ore_cost'])
    max_needed_clay_robots = blueprint['obsidian_robot_clay_cost']
    max_needed_obsidian_robots = blueprint['geode_robot_obsidian_cost']

    t = state['time']
    if t in best_per_time and (state['geode'] + 5) < best_per_time[t]:
        return -1
    new_best_per_time = max(best_per_time[t] if t in best_per_time else 0, state['geode'])
    best_per_time[t] = new_best_per_time

    best = best_holder[0]
    if state['time'] < 20:
        blueprint_id = blueprint['id']
        print(f'blueprint {blueprint_id}, time {t}, current best {best}')
    # cache_state = frozenset(state.items())
    # if cache_state in cache:
    #     return cache[cache_state]
    if state['time'] == max_time:
        return state['geode']

    can_afford_geode_every_turn = state['ore_robots'] >= blueprint['geode_robot_ore_cost'] and \
                                  state['obsidian_robots'] >= blueprint['geode_robot_obsidian_cost']

    ore_robot, clay_robot, obsidian_robot, geode_robot = -1, -1, -1, -1

    if not can_afford_geode_every_turn:

        if state['ore_robots'] < max_needed_ore_robots:
            # build ore robot
            ore = state['ore']
            missing_ore = max(0, blueprint['ore_robot_ore_cost'] - ore)
            ore_robots = state['ore_robots']
            minutes_to_skip = math.ceil(missing_ore / ore_robots)

            if state['time'] + minutes_to_skip + 1 < max_time:
                new_state = state.copy()
                new_state['ore'] = ore - blueprint['ore_robot_ore_cost'] + (minutes_to_skip + 1) * ore_robots
                new_state['clay'] += (minutes_to_skip + 1) * state['clay_robots']
                new_state['obsidian'] += (minutes_to_skip + 1) * state['obsidian_robots']
                new_state['geode'] += (minutes_to_skip + 1) * state['geode_robots']
                new_state['ore_robots'] += 1
                new_state['time'] += minutes_to_skip + 1
                ore_robot = eval_blueprint(blueprint, new_state, cache, max_time, best_holder, best_per_time)

        if state['clay_robots'] < max_needed_clay_robots:
            # build clay robot
            ore = state['ore']
            missing_ore = max(0, blueprint['clay_robot_ore_cost'] - ore)
            ore_robots = state['ore_robots']
            minutes_to_skip = math.ceil(missing_ore / ore_robots)

            if state['time'] + minutes_to_skip + 1 < max_time:
                new_state = state.copy()
                new_state['ore'] = ore - blueprint['clay_robot_ore_cost'] + (minutes_to_skip + 1) * ore_robots
                new_state['clay'] += (minutes_to_skip + 1) * state['clay_robots']
                new_state['obsidian'] += (minutes_to_skip + 1) * state['obsidian_robots']
                new_state['geode'] += (minutes_to_skip + 1) * state['geode_robots']
                new_state['clay_robots'] += 1
                new_state['time'] += minutes_to_skip + 1
                clay_robot = eval_blueprint(blueprint, new_state, cache, max_time, best_holder, best_per_time)

        # build obsidian robot if possible
        if state['clay_robots'] > 0 and state['obsidian_robots'] < max_needed_obsidian_robots:
            ore = state['ore']
            missing_ore = max(0, blueprint['obsidian_robot_ore_cost'] - ore)
            ore_robots = state['ore_robots']

            clay = state['clay']
            missing_clay = max(0, blueprint['obsidian_robot_clay_cost'] - clay)
            clay_robots = state['clay_robots']

            minutes_to_skip = max(math.ceil(missing_ore / ore_robots), math.ceil(missing_clay / clay_robots))

            if state['time'] + minutes_to_skip + 1 < max_time:
                new_state = state.copy()
                new_state['ore'] = ore - blueprint['obsidian_robot_ore_cost'] + (minutes_to_skip + 1) * ore_robots
                new_state['clay'] = clay - blueprint['obsidian_robot_clay_cost'] + (minutes_to_skip + 1) * clay_robots
                new_state['obsidian'] += (minutes_to_skip + 1) * state['obsidian_robots']
                new_state['geode'] += (minutes_to_skip + 1) * state['geode_robots']
                new_state['obsidian_robots'] += 1
                new_state['time'] += minutes_to_skip + 1
                obsidian_robot = eval_blueprint(blueprint, new_state, cache, max_time, best_holder, best_per_time)

    # build geode robot if possible
    if state['obsidian_robots'] > 0:
        ore = state['ore']
        missing_ore = max(0, blueprint['geode_robot_ore_cost'] - ore)
        ore_robots = state['ore_robots']

        obsidian = state['obsidian']
        missing_obsidian = max(0, blueprint['geode_robot_obsidian_cost'] - obsidian)
        obsidian_robots = state['obsidian_robots']

        minutes_to_skip = max(math.ceil(missing_ore / ore_robots), math.ceil(missing_obsidian / obsidian_robots))

        if state['time'] + minutes_to_skip + 1 < max_time:
            new_state = state.copy()
            new_state['ore'] = ore - blueprint['geode_robot_ore_cost'] + (minutes_to_skip + 1) * ore_robots
            new_state['clay'] += (minutes_to_skip + 1) * state['clay_robots']
            new_state['obsidian'] = obsidian - blueprint['geode_robot_obsidian_cost'] + (
                    minutes_to_skip + 1) * obsidian_robots
            new_state['geode'] += (minutes_to_skip + 1) * state['geode_robots']
            new_state['geode_robots'] += 1
            new_state['time'] += minutes_to_skip + 1
            geode_robot = eval_blueprint(blueprint, new_state, cache, max_time, best_holder, best_per_time)

    no_new_robots = state['geode'] + (max_time - state['time']) * state['geode_robots']

    result = max(no_new_robots, ore_robot, clay_robot, obsidian_robot, geode_robot)
    if result > best_holder[0]:
        best_holder[0] = result
    # cache[cache_state] = result
    return result


p1_max_time = 24
p2_max_time = 32

if __name__ == '__main__':
    data = list(read_data('d19'))

    # p1 = sum([d['id'] * eval_blueprint(d, initial_state(), {}, p1_max_time, [-1], {}) for d in data])
    # print(p1)
    p2_results = [eval_blueprint(d, initial_state(), {}, p2_max_time, [-1], {}) for d in data[:3]]
    print(p2_results)
    p2 = reduce(lambda a, b: a * b, p2_results)
    print(p2)
