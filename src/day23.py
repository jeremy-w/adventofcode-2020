from pprint import pprint
from collections import deque

print('===== day 23 =====')

def to_cups(i: str):
	return [int(x) for x in i]

def step(cups):
	current = cups[0]
	picked_up = cups[1:4]
	del cups[0:4]
	cups.append(current)
	dest = find_dest(current - 1, cups)
	i = cups.index(dest)
	next = cups[0:i+1] + picked_up + cups[i+1:]
	return next

MAX = 9
def find_dest(n, ns):
	#assert ns != []
	# assert len([x for x in ns if x > 9 or x < 1]) == 0
	n = wrap(n)
	while n not in ns:
		n = wrap(n - 1)
	return n

def wrap(n):
	if n < 1:
		n = MAX
	return n

def run(cups, steps):
	#seen = dict()
	for i in range(steps):
		if (i % 1000) == 0:
			print(i)
		#h = str(cups)
		#seen[h] = i
		
		cups = step(cups)
		
		if False:
			h = str(cups)
			if h in seen:
				prev = seen[h]
				print('repeat! previously saw on step', prev, 'and seen again on step', i, ', delta', i - prev)
			seen[h] = i
	return cups

def solution(cups):
	i = cups.index(1)
	result = cups[i+1:] + cups[0:i]
	return ''.join((str(x) for x in result))

example = to_cups("389125467")
real_input = to_cups("716892543")

p1_answer = solution(run(real_input, 100))
print('part 1 answer:', p1_answer)
assert p1_answer == '49725386'

ONE_MIL = 1_000_000
MAX = ONE_MIL

def grow_input(cups):
	c = max(cups)
	print(c)
	rest = list(range(c + 1, ONE_MIL + 1))
	result = cups + rest
	assert max(result) == 1_000_000, 'max was %s' % max(result)
	assert len(result) == 1_000_000, 'len was %s - cups len %s - rest len %d' % (len(result), len(cups), len(rest))
	return result

def solution2(cups):
	i = cups.index(1)
	a = cups[(i + 1) % len(cups)]
	b = cups[(i + 2) % len(cups)]
	result = a*b
	print('a', a, '- b', b, ' - part 2 solution', result)
	return result

example = to_cups("389125467")
real_input = to_cups("716892543")
e2 = grow_input(example)
a2 = grow_input(real_input)

ten_mil = 10_000_000

def p2(cups):
	print('solving part 2')
	after = run(cups, ten_mil)
	result = solution2(after)
	print('done')
	return result

expected = 149245887792
assert p2(e2) == expected

p2(a2)
