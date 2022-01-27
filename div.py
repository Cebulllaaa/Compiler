def even(d) :
  return ((d >> 1) << 1) - d >= 0

def mul(c,d):
  e = 0
  if c <= 0:
    c = -c
    d = -d
  while c != 0:
    if not even(c):
        e += d
    c >>= 1
    d <<= 1
  return e

def div(c,d):
  e = 0
  if d == 0:
    return e
  if d <= 0:
    c = -c
    d = -d
  f = 0
  if c <= 0:
    c = -c
    f += 1
  g = 0
  while c - d >= 0:
    d <<= 1
    g += 1
  while g != 0:
    g -= 1
    e <<= 1
    d >>= 1
    a = c - d
    if a >= 0:
      c = a
      e += 1
  if f != 0:
    e = -e
    if c != 0:
      e = e-1
  return e

def mod(c,d):
  if d == 0:
    c = 0
    return c
  f = 0
  if d <= 0:
    f += 1
    c = -c
    d = -d
  if c <= 0:
    f += 2
    c = -c
  e = 0
  while c - d >= 0:
    d <<= 1
    e += 1
  while e != 0:
    d >>= 1
    a = c - d
    if a >= 0:
      c = a
    e -= 1
  if (f >> 1) != 0:
    if c != 0:
      c = d - c
  print(c,d,e,f)
  if ((f >> 1) << 1) - f != 0:
    c = -c
  return c

x = int(input('x: '))
y = int(input('y: '))
print('mul:',mul(x,y))
print('div:',div(x,y))
print('mod:',mod(x,y))