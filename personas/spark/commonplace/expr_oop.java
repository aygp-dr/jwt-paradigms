// Object-oriented approach
// Easy to add new expressions, hard to add operations
interface Expr {
    double evaluate();
    String prettyPrint();
}

class Constant implements Expr {
    private double value;
    
    public Constant(double value) {
        this.value = value;
    }
    
    public double evaluate() {
        return value;
    }
    
    public String prettyPrint() {
        return Double.toString(value);
    }
}

class Addition implements Expr {
    private Expr left;
    private Expr right;
    
    public Addition(Expr left, Expr right) {
        this.left = left;
        this.right = right;
    }
    
    public double evaluate() {
        return left.evaluate() + right.evaluate();
    }
    
    public String prettyPrint() {
        return "(" + left.prettyPrint() + " + " + right.prettyPrint() + ")";
    }
}

// Example usage
class Main {
    public static void main(String[] args) {
        Expr expr = new Addition(new Constant(5), new Addition(new Constant(3), new Constant(2)));
        System.out.println("Evaluated: " + expr.evaluate());
        System.out.println("Expression: " + expr.prettyPrint());
    }
}

// Adding a new operation like "compile" requires modifying all classes!
// Adding a new expression class is easy
