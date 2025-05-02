// The element hierarchy
interface Expr {
    <R> R accept(Visitor<R> visitor);
}

class Lit implements Expr {
    private int value;
    
    public Lit(int value) { this.value = value; }
    
    public <R> R accept(Visitor<R> visitor) {
        return visitor.visitLit(this);
    }
    
    public int getValue() { return value; }
}

class Add implements Expr {
    private Expr left, right;
    
    public Add(Expr left, Expr right) {
        this.left = left;
        this.right = right;
    }
    
    public <R> R accept(Visitor<R> visitor) {
        return visitor.visitAdd(this);
    }
    
    public Expr getLeft() { return left; }
    public Expr getRight() { return right; }
}

// The visitor hierarchy
interface Visitor<R> {
    R visitLit(Lit lit);
    R visitAdd(Add add);
}

class EvalVisitor implements Visitor<Integer> {
    public Integer visitLit(Lit lit) {
        return lit.getValue();
    }
    
    public Integer visitAdd(Add add) {
        return add.getLeft().accept(this) + add.getRight().accept(this);
    }
}

class PrettyPrintVisitor implements Visitor<String> {
    public String visitLit(Lit lit) {
        return Integer.toString(lit.getValue());
    }
    
    public String visitAdd(Add add) {
        return "(" + add.getLeft().accept(this) + " + " + add.getRight().accept(this) + ")";
    }
}
