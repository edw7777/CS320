########################################################################
#
# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python
#
########################################################################
# datatype 'a mylist =
# nil | cons of ('a * 'a list)

class mylist:
    ctag = -1
    def get_ctag(self):
        return self.ctag
    def __iter__(self):
        return mylist_iter(self)
    def __reversed__(self):
        return mylist_reverse(self)

class mylist_iter:
    def __iter__(self):
        return self
    def __init__(self, itms):
        self.itms = itms
    def __next__(self):
        if (self.itms.ctag==0):
            raise StopIteration
        else:
            itm1 = self.itms.cons1
            self.itms = self.itms.cons2
            return itm1

class mylist_nil(mylist):
    def __init__(self):
        self.ctag = 0

class mylist_cons(mylist):
    def __init__(self, cons1, cons2):
        self.ctag = 1
        self.cons1 = cons1
        self.cons2 = cons2
    def get_cons1(self):
        return self.cons1
    def get_cons2(self):
        return self.cons2

class mylist_snoc(mylist):
    def __init__(self, cons1, cons2):
        self.ctag = 1
        self.cons1 = cons2
        self.cons2 = cons1
    def get_cons1(self):
        return self.cons1
    def get_cons2(self):
        return self.cons2

class mylist_reverse(mylist):
    def __init__(self,cons2):
        self.ctag = 1
        self.cons2 = cons2
    def get_cons2(self):
        return self.cons2

class mylist_append2(mylist):
    def __init__(self, cons1, cons2):
        self.ctag = 1
        self.cons1 = cons1
        self.cons2 = cons2
    def get_cons1(self):
        return self.cons1
    def get_cons2(self):
        return self.cons2

def mylist_sing(x0):
    res = mylist_nil()
    res = mylist_cons(x0, res)
    return res

def mylist_print(xs):
    nx = 0
    sep = "; "
    print("mylist[",end='')
    while(xs.ctag > 0):
        if (nx > 0):
            print(sep,end='')        
        print(xs.cons1,end='')
        nx = nx + 1; xs = xs.cons2
    print("]", end='')

def mylist_foreach(xs, work):
    match xs:
        case mylist_nil():
            None
        case mylist_cons():
            x1 = xs.cons1
            xs = xs.cons2
            work(x1)
            mylist_foreach(xs, work)
        case mylist_snoc():
            x1 = xs.cons1
            xs = xs.cons2
            mylist_foreach(xs, work)
            work(x1)
        case mylist_reverse():
            xs = xs.cons2
            mylist_rforeach(xs, work)
        case mylist_append2():
            xs1 = xs.cons1
            xs2 = xs.cons2
            mylist_foreach(xs1, work)
            mylist_foreach(xs2, work)

def mylist_rforeach(xs, work):
    match xs:
        case mylist_nil():
            None
        case mylist_cons():
            x1 = xs.cons1
            xs = xs.cons2
            mylist_rforeach(xs, work)
            work(x1)
        case mylist_snoc():
            x1 = xs.cons1
            xs = xs.cons2
            work(x1)
            mylist_rforeach(xs, work)
        case mylist_reverse():
            xs = xs.cons2
            mylist_foreach(xs, work)
        case mylist_append2():
            xs1 = xs.cons1
            xs2 = xs.cons2
            mylist_rforeach(xs2, work)
            mylist_rforeach(xs1, work)