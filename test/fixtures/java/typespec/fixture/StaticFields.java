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
}
