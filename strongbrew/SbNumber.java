package strongbrew.numbers;


public interface SbNumber<O> {
    public O add(O num);
    public O subtract(O num);
    public O multiply(O num);
    public O divide(O num);
    public O mod(O num);
    public O pow(int num);
    public O abs();
    public O negate();
    public O not();
    public float floatValue();
    public double doubleValue();
    public int intValue();
    public long longValue();
}
