# Duck typing in Ruby
class Duck
  def quack
    puts "Quack!"
  end
  
  def swim
    puts "Swimming like a duck"
  end
end

class Person
  def quack
    puts "I'm imitating a duck!"
  end
  
  def swim
    puts "Swimming like a human"
  end
end

def make_it_quack(object)
  object.quack  # Will work with any object that responds to 'quack'
end

duck = Duck.new
person = Person.new

make_it_quack(duck)    # Output: "Quack!"
make_it_quack(person)  # Output: "I'm imitating a duck!"
