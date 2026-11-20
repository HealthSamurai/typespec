package typespec.fixture;

import java.util.HashMap;
import java.util.List;

public final class GenericReturns {
    static {
        if (Boolean.parseBoolean("true")) {
            throw new AssertionError("Static analysis must not initialize this class");
        }
    }

    public interface Provider<T> {
        default T fromInterface() {
            return null;
        }
    }

    public interface Intermediate<U> extends Provider<U> {}

    public abstract static class Base<B extends Base<B>> {
        public B settings(Object ignored) {
            return null;
        }

        public Base<B> baseView() {
            return this;
        }

        public <B> B methodVariable() {
            return null;
        }

        protected Object build() {
            return null;
        }
    }

    public abstract static class Middle<M extends Middle<M>>
            extends Base<M> implements Intermediate<M> {}

    public static final class Product {}

    public static final class Builder extends Middle<Builder> {
        @Override
        public Product build() {
            return new Product();
        }
    }

    @SuppressWarnings("rawtypes")
    public static final class Raw extends Base {}

    public static class GenericList<E> implements Provider<List<E>> {}

    public static final class NamedList extends GenericList<String> {}

    @SuppressWarnings("rawtypes")
    public static final class RawList extends GenericList {}

    public static final class ProductMap extends HashMap<String, Product> {}

    public static Builder builder() {
        return new Builder();
    }

    public static ProductMap products() {
        return new ProductMap();
    }
}
