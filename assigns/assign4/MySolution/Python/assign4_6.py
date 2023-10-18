def nats1():
    i = 0
    while True:
        yield i
        i += 1

def nats2():
    i = 0
    while True:
        yield i
        i += 1

def lte(x, y):
  return x <= y

class StrCon:
    def __init__(self, tag, value):
        self.tag = tag
        self.value = value

def str_nil():
    return StrCon("StrNil", None)

def str_cons(a, f):
    return StrCon("StrCons", (a, f))

def stream_merge(fxs0, fys0, lte):
    def merge_streams():
        fxs0_val = fxs0()
        fys0_val = fys0()
        
        if len(fxs0) == 0
            return fys0
        elif len(fys0) == 0:
            return fxs0
        
        (x1, fxs1) = fxs0_val.value
        (y1, fys1) = fys0_val.value
        
        if lte(x1)(y1):
            return str_cons(x1, lambda: stream_merge(fxs1, fys0, lte))
        else:
            return str_cons(y1, lambda: stream_merge(fxs0, fys1, lte))
    
    return merge_streams

def stream_map(fxs, fopr):
    def map_stream():
        fxs_val = fxs()
        
        if fxs_val.tag == "StrNil":
            return str_nil()
        
        x1, fxs_rest = fxs_val.value
        return str_cons(fopr(x1), lambda: stream_map(fxs_rest, fopr))
    
    return map_stream

def theNatPairs_cubesum(fxs, fys):
    def theNatPairs_cubesum_stream():
        fxs_val = fxs()
        
        if fxs_val.tag == "StrNil":
            return str_nil()
        
        x1, fxs_rest = fxs_val.value
        res1 = stream_map(fys, lambda y: (x1, y))
        res2 = theNatPairs_cubesum(fxs_rest, fys)
        
        return str_cons(x1, lambda: stream_merge(res1, res2))
    
    return theNatPairs_cubesum_stream
"""
let rec
stream_merge
(fxs0)(fys0)(lte) = fun() ->
match fxs0(), fys0() with
  | StrNil, _ -> fys0()
  | _, StrNil -> fxs0()
  | StrCons(x1, fxs1), StrCons(y1, fys1) ->
    if
    lte(x1)(y1)
    then StrCons(x1, stream_merge(fxs1)(fys0)(lte))
    else StrCons(y1, stream_merge(fxs0)(fys1)(lte))
;;


let rec x2 (nats1) (nats2) = 
  fun() ->
  match fxs() with 
  | StrNil -> StrNil
  | StrCons(x1, fxs) ->
    let res1 = map(fys)(fun y -> x1, y)
    let res2 = x2(fxs)(fys)
  StrCons(res1 first element) (streammerge (res1)(res2)) */
  """