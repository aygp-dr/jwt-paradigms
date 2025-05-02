interface Named {
    name: string;
}

function greet(person: Named) {
    console.log(`Hello, ${person.name}!`);
}

// Works with any object that has a name property
greet({ name: "Alice" });                  // Object literal
greet(new class { name = "Bob" }());       // Class instance
greet({ name: "Charlie", age: 30 });       // Object with extra properties
