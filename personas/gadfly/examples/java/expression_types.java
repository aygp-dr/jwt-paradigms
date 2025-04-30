interface Expr {
    int eval();
    String prettyPrint();
}

class Lit implements Expr {
    private int value;
    
    public Lit(int value) { this.value = value; }
    
    public int eval() { return value; }
    
    public String prettyPrint() { return Integer.toString(value); }
}

class Add implements Expr {
    private Expr left, right;
    
    public Add(Expr left, Expr right) {
        this.left = left;
        this.right = right;
    }
    
    public int eval() { return left.eval() + right.eval(); }
    
    public String prettyPrint() {
        return "(" + left.prettyPrint() + " + " + right.prettyPrint() + ")";
    }
}
