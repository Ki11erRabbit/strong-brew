package strongbrew.numbers;

import java.math.BigInteger;

public class Int implements SbNumber<Int>, SbNumber<Nat> {
    public BigInteger value;
    
    public Int(int value) {
        this.value = BigInteger.valueOf(value);
    }
    public Int(BigInteger value) {
        this.value = value;
    }
    public Int(String value) {
        this.value = new BigInteger(value);
    }

    public Int add(Int num) {
        return new Int(value.add(num.value));
    }
    public Int subtract(Int num) {
        return new Int(value.subtract(num.value));
    }
    public Int multiply(Int num) {
        return new Int(value.multiply(num.value));
    }
    public Int divide(Int num) {
        return new Int(value.divide(num.value));
    }
    public Int mod(Int num) {
        return new Int(value.remainder(num.value));
    }
    public Int pow(int num) {
        return new Int(value.pow(num));
    }
    public Int abs() {
        return new Int(value.abs());
    }
    public Int negate() {
        return new Int(value.negate());
    }
    public Int not() {
        return new Int(value.not());
    }
    public float floatValue() {
        return value.floatValue();
    }
    public double doubleValue() {
        return value.doubleValue();
    }
    public int intValue() {
        return value.intValue();
    }
    public long longValue() {
        return value.longValue();
    }

    public Int add(Nat num) {
        return new Int(value.add(num.value));
    }
    public Int subtract(Nat num) {
        return new Int(value.subtract(num.value));
    }
    public Int multiply(Nat num) {
        return new Int(value.multiply(num.value));
    }
    public Int divide(Nat num) {
        return new Int(value.divide(num.value));
    }
    public Int mod(Nat num) {
        return new Int(value.remainder(num.value));
    }
    public Int pow(Nat num) {
        return new Int(value.pow(num.intValue()));
    }
}
