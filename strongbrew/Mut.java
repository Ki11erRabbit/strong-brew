package strongbrew;

public class Mut <T> {
    private T value;
    public Mut(T value) {
        this.value = value;
    }

    public T get() {
        java.lang.System.out.println("get called");
        return value;
    }
    
    public void set(T value) {
        java.lang.System.out.println("set called");
        this.value = value;
    }
}
