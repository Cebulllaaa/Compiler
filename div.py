
def mul(c,d):
  e = 0
  while c != 0:
    f = c
    f >>= 1
    f <<= 1
    if c != f:
        e += d
    c >>= 1
    d <<= 1
  return e

def div(c,d):
  e = 0
  if d == 0:
    return e
  f = d
  f >>= 1
  f <<= 1
  while d == f:
    c >>= 1
    d >>= 1
    f = d
    f >>= 1
    f <<= 1
  while c >= d:
    d <<= 1
  f = d
  f >>= 1
  f <<= 1
  while d == f:
    e <<= 1
    d >>= 1
    if c >= d:
      c -= d
      e += 1
    f = d
    f >>= 1
    f <<= 1
  return e

def mod(c,d):
  if d == 0:
    c = 0
    return c
  e = 0
  while c >= d:
    d <<= 1
    e += 1
  while e > 0:
    if c >= d:
      c -= d
    d >>= 1
    e -= 1
  if c >= d:
    c -= d
  return c

x = int(input('x: '))
y = int(input('y: '))
print('mul:',mul(x,y))
print('div:',div(x,y))
print('mod:',mod(x,y))


