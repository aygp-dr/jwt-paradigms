// A simple counter with a race condition
public class Counter {
    private int count = 0;
    
    public void increment() {
        count++;  // Not atomic! Read, increment, write
    }
    
    public int getCount() {
        return count;
    }
}
