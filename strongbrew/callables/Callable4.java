package strongbrew.callables;
@FunctionalInterface
public interface Callable4 <A, B, C, D, E> {
    public E call(A a, B b, C c, D d, E e);
}