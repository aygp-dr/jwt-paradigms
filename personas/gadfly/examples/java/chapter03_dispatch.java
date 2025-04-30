// Static vs. dynamic dispatch in Java
class Animal {
    public void makeSound() {
        System.out.println("Some generic animal sound");
    }
    
    public void eat() {
        System.out.println("Animal eating");
    }
}

class Dog extends Animal {
    @Override
    public void makeSound() {
        System.out.println("Woof!");
    }
    
    public void fetch() {
        System.out.println("Dog fetching");
    }
}

public class Main {
    public static void main(String[] args) {
        Animal animal = new Dog();  // Dog object, Animal reference
        
        // Dynamic dispatch - calls Dog's implementation
        animal.makeSound();  // Output: "Woof!"
        
        // Static dispatch - Animal reference can't see Dog-specific methods
        // animal.fetch();  // Compilation error
    }
}
