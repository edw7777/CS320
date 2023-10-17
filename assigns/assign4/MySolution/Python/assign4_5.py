def string_get_at(cs, i):
  return cs[i]

def string_length(cs):
  return len(cs)

def string_tabulate(n0, work):
  res = ""
  for i in range(n0):
    res = res + str(work(i))
  return res

def string_fset_at(cs, i0, c0):
  n0 = string_length(cs)
  return string_tabulate(n0, lambda i: string_get_at(cs,i) if i != i0 else c0)

alphabet = "abcdefghijklmnopqrstuvwxyz"

def list_make_fwork(fwork):
    res = []

    def work(x0):
        res.append(x0)

    fwork(lambda x: work(x))
    return reversed(res)

def int1_foreach(n0, work_func):
    i0 = 0
    while(i0 < n0):
        work_func(i0)
        i0 = (i0 + 1)
    return None

def string_foreach(cs, work):
  n0 = string_length(cs)
  int1_foreach(n0, lambda i0: work(string_get_at(cs, i0)))

def list_of_buddies(word):
  n0 = string_length(word)
  def helper(i0, c1, work):
    if c1 != string_get_at(word, i0):
      work(string_fset_at(word, i0, c1))
  return list_make_fwork(lambda work: int1_foreach(n0, lambda i0: string_foreach(alphabet, lambda c1: helper(i0, c1, work) ) ) )

#list_make_fwork(int1_foreach(n0, lambda i0: string_foreach(alphabet, lambda c1: if c1 != string_get_at(word, i0): string_fest_at(word)(i0)(c1)) ) ) )