package strongbrew.numbers;

import java.math.BigInteger;

public class Nat extends SbNumber {
    public Nat(int value) {
        this.value = BigInteger.valueOf(value);
    }
    public Nat(BigInteger value) {
        this.value = value;
    }
    public Nat(String value) {
        this.value = new BigInteger(value);
    }
    public Nat(long value) {
        this.value = BigInteger.valueOf(value);
    }
    public Nat(byte value) {
        this.value = BigInteger.valueOf(value);
    }
    public Nat(short value) {
        this.value = BigInteger.valueOf(value);
    }
    @Override
    public SbNumber add(SbNumber num) {
        Class c = num.getClass();
        if (c.equals(this.getClass())) {
            return new Nat(value.add(num.value));
        } else {
            if (num.value.compareTo(BigInteger.ZERO) < 0) {
                return new Int(value.add(num.value));
            }
        }
        return new Nat(value.add(num.value));
    }
    @Override
    public SbNumber subtract(SbNumber num) {
        Class c = num.getClass();
        if (c.equals(this.getClass())) {
            if (num.value.compareTo(value) > 0) {
                return new Int(value.subtract(num.value));
            }
            return new Nat(value.subtract(num.value));
        } else {
            if (num.value.compareTo(value) > 0) {
                return new Nat(value.subtract(num.value));
            }
            return new Int(value.subtract(num.value));
        }
    }
    @Override
    public SbNumber multiply(SbNumber num) {
        Class c = num.getClass();
        if (c.equals(this.getClass())) {
            return new Nat(value.multiply(num.value));
        } else {
            if (num.value.compareTo(BigInteger.ZERO) < 0) {
                return new Int(value.multiply(num.value));
            }
        }
        return new Nat(value.multiply(num.value));
    }
    @Override
    public SbNumber divide(SbNumber num) {
        Class c = num.getClass();
        if (c.equals(this.getClass())) {
            return new Nat(value.divide(num.value));
        } else {
            if (num.value.compareTo(BigInteger.ZERO) < 0) {
                return new Int(value.divide(num.value));
            }
        }
        return new Nat(value.multiply(num.value));
    }
    @Override
    public SbNumber mod(SbNumber num) {
        Class c = num.getClass();
        if (c.equals(this.getClass())) {
            return new Nat(value.remainder(num.value));
        } else {
            if (num.value.compareTo(BigInteger.ZERO) < 0) {
                return new Int(value.remainder(num.value));
            }
        }
        return new Nat(value.remainder(num.value));
    }
    @Override
    public SbNumber pow(int num) {
        return new Nat(value.pow(num));
    }
    @Override
    public SbNumber abs() {
        return new Nat(value);
    }
    @Override
    public SbNumber negate() {
        return new Int(value.negate());
    }
    @Override
    public SbNumber not() {
        return new Int(value.not().multiply(BigInteger.valueOf(-1)));
    }
}
