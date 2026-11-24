package typespec.fixture;

public final class StaticFields {
    public static final int number = 7;
    public static final String text = "field";
    public static final String[] labels = failIfInitialized();

    public static String number() {
        return "method";
    }

    public static int text(int n) {
        return n;
    }

    private static String[] failIfInitialized() {
        throw new AssertionError("Typespec should only read field metadata");
    }

    public static final class Members {
        public static int total;
        public int size = 1;
        public int alt = 2;
        public String[] labels = {"value"};
        public String text;
        public Long boxed;
        public char initial;
        public boolean enabled;
        public final String fixed = "fixed";

        public static int total(int amount) {
            return amount;
        }

        public String size() {
            return "method";
        }

        public String alt(int n) {
            return "method";
        }
    }
}
