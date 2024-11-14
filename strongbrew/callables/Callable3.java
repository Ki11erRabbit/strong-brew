package strongbrew.callables;
@FunctionalInterface
public interface Callable3 <A, B, C, D> {
    public D call(A a, B b, C c, D d);
}