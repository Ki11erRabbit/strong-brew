package strongbrew.numbers;

import java.math.BigInteger;

public abstract class SbNumber {
    protected BigInteger value;
    public abstract SbNumber add(SbNumber num);
    public abstract SbNumber subtract(SbNumber num);
    public abstract SbNumber multiply(SbNumber num);
    public abstract SbNumber divide(SbNumber num);
    public abstract SbNumber mod(SbNumber num);
    public abstract SbNumber pow(int num);
    public abstract SbNumber abs();
    public abstract SbNumber negate();
    public abstract SbNumber not();
    public float floatValue() {
        return value.floatValue();
    }
    public double doubleValue(){
        return value.doubleValue();
    }
    public int intValue(){
        return value.intValue();
    }
    public long longValue() {
        return value.longValue();
    }
    @Override
    public String toString() {
        return value.toString();
    }
}
