package strongbrew;

public class Mut <T> {
    private T value;
    public Mut(T value) {
        this.value = value;
    }

    public T get() {
        return value;
    }
    
    public void set(T value) {
        this.value = value;
    }
    
    public void set(Mut<T> value) {
        this.value = value.get();
    }
}
