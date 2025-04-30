class ValidatedProperty:
    def __init__(self, validator):
        self.validator = validator
        self.name = None
        
    def __set_name__(self, owner, name):
        self.name = name
        
    def __get__(self, instance, owner):
        if instance is None:
            return self
        return instance.__dict__[self.name]
        
    def __set__(self, instance, value):
        if not self.validator(value):
            raise ValueError(f"Invalid value for {self.name}")
        instance.__dict__[self.name] = value

# Usage
def positive(x):
    return isinstance(x, int) and x > 0

class Person:
    age = ValidatedProperty(positive)
    
    def __init__(self, age):
        self.age = age
