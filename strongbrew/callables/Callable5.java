package strongbrew.callables;
@FunctionalInterface
public interface Callable5 <A, B, C, D, E, F> {
    public F call(A a, B b, C c, D d, E e, F f);
}