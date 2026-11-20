# FHIR registry: Java builder triage (metadata only)

## Verdict

`src/fhir/registry/gcs.clj:49:76`, TSP2339 (`ServiceOptions$Builder has no method build`) is a **checker false positive caused by erasing an inherited generic self type**. It is not evidence that the application's builder lacks `build`. The generic `B` returned by `setCredentials` resolves to `StorageOptions$Builder`, which exposes public `build`. No checker or application code was changed. No claim is made that credentials, network access, or the entire service works at runtime.

## Scope and provenance

- Upstream checkout: `.hyper/tmp/ceya/upstream`, pinned by baseline to `ea79b6a18d9ae759593c3824f502bc1200209e22`.
- Checker HEAD during investigation: `9a997297564fb4e2a820185699e8d95f037fe501`.
- Reused `examples/fhir-registry-check/resolution-classpath.txt` directly; did not invoke tools.deps or `run.sh`/`prepare.sh`.
- Classpath SHA256: `4460122fd17da25c85994c7a8fd38a4d8cfd5dc7de8aedeb7c66320b6e7d6ac0`.
- Resolved jars: google-cloud-storage **2.32.1**, google-cloud-core **2.31.0**. Clojure **1.12.4**, Java **27.0.1.1**.
- Only local file reads, `javap`, non-initializing `Class/forName(..., false, loader)`, Java reflection metadata, and checker parsing/inference were used. No cloud methods or constructors invoked; no credentials read; no network operations performed. Upstream Clojure namespaces were not required/evaluated. Post-check `find-ns` for `fhir.registry`, `fhir.registry.gcs`, `gcs` returned none.

## Metadata evidence

`javap` on the exact resolved classpath reports:

```java
public abstract class StorageOptions extends ServiceOptions<Storage, StorageOptions> {
  public static StorageOptions.Builder newBuilder();
}
public abstract class StorageOptions.Builder
    extends ServiceOptions.Builder<Storage, StorageOptions, StorageOptions.Builder> {
  public abstract StorageOptions build();
  public ServiceOptions build(); // synthetic bridge
}
public abstract class ServiceOptions.Builder<
    ServiceT extends Service<OptionsT>,
    OptionsT extends ServiceOptions<ServiceT, OptionsT>,
    B extends ServiceOptions.Builder<ServiceT, OptionsT, B>> {
  protected abstract ServiceOptions<ServiceT, OptionsT> build();
  protected B self();
  public B setCredentials(com.google.auth.Credentials);
}
```

Reflection on `StorageOptions$Builder`:

| Method | Erased return | Generic return | bridge / synthetic |
|---|---|---|---|
| inherited `setCredentials(Credentials)` | `ServiceOptions$Builder` | `B` | false / false |
| `build()` | `StorageOptions` | `StorageOptions` | false / false |
| `build()` | `ServiceOptions` | `ServiceOptions` | true / true |

`getGenericSuperclass()` is `ServiceOptions$Builder<Storage, StorageOptions, StorageOptions$Builder>`. Thus the declaring class's `B` has an explicit, recoverable substitution.

`javap -c -p` also confirms, without execution: `StorageOptions.newBuilder()` delegates to `http()`, which delegates to `HttpStorageOptions.newBuilder()`. `HttpStorageOptions$Builder` extends `StorageOptions$Builder` and has public `build` returning `HttpStorageOptions`, plus bridges returning `StorageOptions` and `ServiceOptions`. `ServiceOptions$Builder.setCredentials` ends with `aload_0; invokevirtual self; areturn`; base `self()` is `aload_0; areturn`. This is a fluent self-return, not a conversion to a different builder.

## Minimal checker reproduction

The following is **source text submitted to the checker**, never evaluated:

```clojure
(ns triage (:import [com.google.cloud.storage StorageOptions]))
(defn repro [creds]
  (-> (StorageOptions/newBuilder) (.setCredentials creds) .build))
```

Observed results:

| Expression inside `repro` | Checker result |
|---|---|
| `(.build (StorageOptions/newBuilder))` | no diagnostics |
| `(-> (StorageOptions/newBuilder) (.setCredentials creds) .build)` | one TSP2339: `com.google.cloud.ServiceOptions$Builder has no method build` |
| `(.build (.setCredentials (StorageOptions/newBuilder) creds))` | same single TSP2339 |

All three have trust `{:cast 0 :any 0 :ignore 0}`. The expanded form rules out threading macro expansion as the primary cause. The unconstrained parameter deliberately avoids credential construction; this test isolates return inference, not credential argument validation.

Reproduce offline from `examples/fhir-registry-check` (uses existing resolved jars only):

```sh
J=/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home/bin
CP=$(cat resolution-classpath.txt)
"$J/java" -cp "$CP" clojure.main - <<'CLJ'
(require '[typespec.parse :as p] '[typespec.check :as c])
(doseq [expr ["(.build (StorageOptions/newBuilder))"
              "(-> (StorageOptions/newBuilder) (.setCredentials creds) .build)"
              "(.build (.setCredentials (StorageOptions/newBuilder) creds))"]]
  (let [source (str "(ns triage (:import [com.google.cloud.storage StorageOptions])) "
                    "(defn repro [creds] " expr ")")
        r (c/check-files [(p/parse-string "triage.clj" source)] {})]
    (prn expr (:diagnostics r) (:trust r))))
(prn :upstream-loaded (vec (filter find-ns '[fhir.registry fhir.registry.gcs gcs])))
(shutdown-agents)
CLJ
"$J/javap" -classpath "$CP" 'com.google.cloud.storage.StorageOptions$Builder'
"$J/javap" -classpath "$CP" -c -p 'com.google.cloud.ServiceOptions$Builder'
```

## Root cause and proposed fix (not implemented)

`src/typespec/java.clj`, `methods-named`, records `Method.getReturnType()` only. For `setCredentials`, it returns `:ret "com.google.cloud.ServiceOptions$Builder"` and discards generic return `B`. `src/typespec/check.clj:1229`, `java-ret`, converts that erased name straight into a class type. On the next call the receiver is therefore the base builder. Its `build` is **protected**, so `Class.getMethods()` correctly does not expose it; the checker then emits TSP2339. The false step was the earlier loss of type information, not reflection's visibility rules.

Proposed bounded correction:

1. Preserve generic return metadata and resolve class-level type variables through the receiver's parameterized superclass/interface chain to the method's declaring class.
2. For this exact case map `ServiceOptions$Builder.B` to `StorageOptions$Builder` and use it for the inherited method return. Do not hardcode GCS, infer self merely from setter names, or replace every superclass return with the receiver.
3. Make bridge/override selection deterministic: prefer non-bridge/non-synthetic and more-specific covariant declarations among the same parameter signature, with a stable tie-breaker. Current sorting only specially ranks `Object` returns, then deduplicates by parameters; reflection order otherwise leaks into selection. Here the observed selected `build` is already the non-bridge `StorageOptions` variant, so bridges are a **separate robustness concern, not the immediate trigger**.
4. Keep public-member filtering. Exposing protected `build` or suppressing TSP2339 would mask the cause.

Suggested tests before implementation approval: local Java generic fluent base + concrete subclass fixture, inherited `B` return, multi-hop generic substitution, covariant bridge ordering, ordinary base-returning methods that must remain base-typed, and genuine missing-method negative controls. Keep the GCS text-only regression as an optional pinned integration test; core tests should not need cloud dependencies. This triage does not implement general Java generic inference or rebaseline either initial error.
