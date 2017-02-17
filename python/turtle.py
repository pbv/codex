#
# Turtle replacement module for execise assement 
#
# Pedro Vasconcelos, 2013
#

import math

class TurtleCmd:
    "Abstract class for turtle commands"
    pass

class Forward(TurtleCmd):
    def __init__(this, n):
        this.arg = n

    def __eq__(this,that):
        return ((isinstance(that,Forward) and that.arg == this.arg) or
                (isinstance(that,Backward) and that.arg == -this.arg))

    def __repr__(this):
        return "forward(%g)" % this.arg

class Backward(TurtleCmd):
    def __init__(this, n):
        this.arg = n

    def __eq__(this,that):
        return ((isinstance(that,Backward) and that.arg == this.arg) or
                (isinstance(that,Forward) and that.arg == -this.arg))

    def __repr__(this):
        return "backward(%g)" % this.arg


class Left(TurtleCmd):
    def __init__(this, n):
        this.arg = n

    def __eq__(this,that):
        return ((isinstance(that,Left) and that.arg == this.arg) or
                (isinstance(that,Right) and that.arg == -this.arg))

    def __repr__(this):
        return "left(%g)" % this.arg


class Right(TurtleCmd):
    def __init__(this, n):
        this.arg = n

    def __eq__(this,that):
        return ((isinstance(that,Right) and that.arg == this.arg) or
                (isinstance(that,Left) and that.arg == -this.arg))

    def __repr__(this):
        return "right(%g)" % this.arg


## these two are singletons
class PenDown(TurtleCmd):
    def __repr__(this):
        return "pendown()"
        
class PenUp(TurtleCmd):
    def __repr__(this):
        return "penup()"


class Turtle:
    "Class for turtle programs"
    
    # create singletons 
    __penDown__ = PenDown()
    __penUp__ = PenUp()

    def __init__(this):
        this.reset()

    #def __repr__(this):
    #    return "Turtle(" + repr(this.cmdlist) + ")"

    def position(this):
        return (this.__xcoord, this.__ycoord)

    def xcor(this):
        return this.__xcoord

    def ycor(this):
        return this.__ycoord

    def isdown(this):
        return this.__pendown

    def heading(this):
        return this.__heading

    def goto(this, x, y):
        this.__xcoord = x
        this.__ycoord = y

    def reset(this):
        this.__cmdlist = []
        this.__xcoord  = 0.0
        this.__ycoord  = 0.0
        this.__heading = 0.0
        this.__pendown = True

    def clear(this):
        this.__cmdlist = []

    def commands(this):
        if this.__cmdlist == []:
            print("*** No turtle commands ***")
        else:
            for cmd in this.__cmdlist:
                print(cmd)


    def penup(this):
        this.__cmdlist.append(__penUp__)
        this.__pendown = False

    def pendown(this):
        this.__cmdlist.append(__penDown__)
        this.__pendown = True

    # rotation command assume degrees
    def left(this, arg):
        this.__cmdlist.append(Left(arg))
        this.__heading = (this.__heading + arg) % 360

    def right(this, arg):
        this.__cmdlist.append(Right(arg))
        this.__heading = (this.__heading - arg) % 360

    def forward(this, arg):
        this.__cmdlist.append(Forward(arg))
        r = this.__heading/360*2*math.pi
        this.__xcoord += math.cos(r)*arg
        this.__ycoord += math.sin(r)*arg

    # shorthand wrappers
    def lt(this,arg):
        this.left(arg)

    def rt(this,arg):
        this.right(arg)

    def fd(this,arg):
        this.forward(arg)


# create a default turtle
__turtle__ = Turtle()

# wrappers for default turtle
def clear():
    __turtle__.clear()

def reset():
    __turtle__.reset()

def commands():
    return __turtle__.commands()

def forward(n):
    __turtle__.forward(n)

def backward(n):
    __turtle__.backward(n)

def left(n):
    __turtle__.left(n)

def right(n):
    __turtle__.right(n)

def fd(n):
    __turtle__.fd(n)
def rt(n):
    __turtle__.rt(n)
def lt(n):
    __turtle__.lt(n)

def heading():
    return __turtle__.heading()

def position():
    return __turtle__.position()

def goto(x,y):
    __turtle__.goto(x,y)

