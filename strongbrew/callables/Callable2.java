package strongbrew.callables;
@FunctionalInterface
public interface Callable2 <A, B, C> {
    public C call(A a, B b);
}