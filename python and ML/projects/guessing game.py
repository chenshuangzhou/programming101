class Hero:
    def __init__(self,name):
        self.name = name
        self.health = 100
    def eat (self,food):
        if (food == 'apple'):
            self.health -= 10
        elif (food == 'ham'):
            self.health += 20

Kevin = Hero('Kevin') 
print (Kevin.name)
print (Kevin.health)
Kevin.eat('apple')
print (Kevin.health)
