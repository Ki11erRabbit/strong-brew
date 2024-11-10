package strongbrew.numbers;

import java.math.BigInteger;

public class Int extends SbNumber {
    
    public Int(int value) {
        this.value = BigInteger.valueOf(value);
    }
    public Int(long value) {
        this.value = BigInteger.valueOf(value);
    }
    public Int(byte value) {
        this.value = BigInteger.valueOf(value);
    }
    public Int(short value) {
        this.value = BigInteger.valueOf(value);
    }
    public Int(BigInteger value) {
        this.value = value;
    }
    public Int(String value) {
        this.value = new BigInteger(value);
    }

    @Override
    public SbNumber add(SbNumber num) {
        return new Int(value.add(num.value));
    }
    @Override
    public SbNumber subtract(SbNumber num) {
        return new Int(value.subtract(num.value));
    }
    @Override
    public SbNumber multiply(SbNumber num) {
        return new Int(value.multiply(num.value));
    }
    @Override
    public SbNumber divide(SbNumber num) {
        return new Int(value.divide(num.value));
    }
    @Override
    public SbNumber mod(SbNumber num) {
        return new Int(value.remainder(num.value));
    }
    @Override
    public SbNumber pow(int num) {
        return new Int(value.pow(num));
    }
    @Override
    public SbNumber abs() {
        return new Int(value.abs());
    }
    @Override
    public SbNumber negate() {
        return new Int(value.negate());
    }
    @Override
    public SbNumber not() {
        return new Int(value.not());
    }
}
