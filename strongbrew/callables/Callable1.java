package strongbrew.callables;
@FunctionalInterface
public interface Callable1 <A, B> {
    public B call(A a);
}