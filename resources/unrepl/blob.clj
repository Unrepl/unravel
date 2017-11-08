(ns compliment.utils
  "Functions and utilities for source implementations."
  (:import java.io.File java.nio.file.Files
           [java.util.jar JarFile JarEntry]))

(def ^:dynamic *extra-metadata*
  "Signals to downstream sources which additional information about completion
  candidates they should attach . Should be a set of keywords."
  nil)

(defn fuzzy-matches?
  "Tests if symbol matches the prefix when symbol is split into parts on
  separator."
  [prefix, ^String symbol, separator]
  (when (or (.startsWith symbol prefix) (= (first prefix) (first symbol)))
    (loop [pre (rest prefix), sym (rest symbol), skipping false]
      (cond (empty? pre) true
            (empty? sym) false
            skipping (if (= (first sym) separator)
                       (recur (if (= (first pre) separator)
                                (rest pre) pre)
                              (rest sym) false)
                       (recur pre (rest sym) true))
            (= (first pre) (first sym)) (recur (rest pre) (rest sym) false)
            :else (recur pre (rest sym) (not= (first sym) separator))))))

(defn fuzzy-matches-no-skip?
  "Tests if symbol matches the prefix where separator? checks whether character
  is a separator. Unlike `fuzzy-matches?` requires separator characters to be
  present in prefix."
  [prefix, ^String symbol, separator?]
  (when (or (.startsWith symbol prefix) (= (first prefix) (first symbol)))
    (loop [pre prefix, sym symbol, skipping false]
      (cond (empty? pre) true
            (empty? sym) false
            skipping (if (separator? (first sym))
                       (recur pre sym false)
                       (recur pre (rest sym) true))
            (= (first pre) (first sym)) (recur (rest pre) (rest sym) false)
            :else (recur pre (rest sym) true)))))

(defn resolve-class
  "Tries to resolve a classname from the given symbol, or returns nil
  if classname can't be resolved."
  [ns sym]
  (when-let [val (try (ns-resolve ns sym)
                      (catch ClassNotFoundException ex nil))]
    (when (class? val) val)))

(defn resolve-namespace
  "Tries to resolve a namespace from the given symbol, either from a
  fully qualified name or an alias in the given namespace."
  [sym ns]
  (or (find-ns sym) ((ns-aliases ns) sym)))

(defmacro ^{:doc "Defines a memoized function."
            :forms '([name doc-string? [params*] body])}
  defmemoized [name & fdecl]
  (let [[doc & fdecl] (if (string? (first fdecl))
                        [(first fdecl) (rest fdecl)]
                        ["" fdecl])]
    `(def ~name ~doc (memoize (fn ~@fdecl)))))

(def primitive-cache (atom {}))

(defmacro cache-last-result
  "If cache for `name` is absent, or `key` doesn't match the key in the cache,
  calculate `v` and return it. Else return value from cache."
  {:style/indent 2}
  [name key value]
  (let [ksym ()]
    `(let [name# ~name
           key# ~key
           [cached-key# cached-value#] (@primitive-cache name#)]
       (if (and (contains? @primitive-cache name#) (= cached-key# key#))
         cached-value#
         (let [value# ~value]
           (swap! primitive-cache assoc name# [key# value#])
           value#)))))

(defn flush-caches
  "Removes all cached values, forcing functions that depend on
  `cache-last-result` to recalculate."
  []
  (reset! primitive-cache {}))

;; Classpath inspection

(def android-vm?
  "Signifies if the application is running on Android."
  (.contains ^String (System/getProperty "java.vendor") "Android"))

(defn- classpath
  "Returns a sequence of File objects of the elements on the classpath."
  []
  (if android-vm?
    ()
    (mapcat #(.split (or (System/getProperty %) "") File/pathSeparator)
            ["sun.boot.class.path" "java.ext.dirs" "java.class.path"
             ;; This is where Boot keeps references to dependencies.
             "fake.class.path"])))

(defn- symlink?
  "Checks if the given file is a symlink."
  [^File f]
  (Files/isSymbolicLink (.toPath f)))

(defn- file-seq-nonr
  "A tree seq on java.io.Files, doesn't resolve symlinked directories to avoid
  infinite sequence resulting from recursive symlinked directories."
  [dir]
  (tree-seq
   (fn [^File f] (and (.isDirectory f) (not (symlink? f))))
   (fn [^File d] (seq (.listFiles d)))
   dir))

(defn- list-files
  "Given a path (either a jar file, directory with classes or directory with
  paths) returns all files under that path."
  [^String path, scan-jars?]
  (cond (.endsWith path "/*")
        (for [^File jar (.listFiles (File. path))
              :when (.endsWith ^String (.getName jar) ".jar")
              file (list-files (.getPath jar) scan-jars?)]
          file)

        (.endsWith path ".jar")
        (if scan-jars?
          (try (for [^JarEntry entry (enumeration-seq (.entries (JarFile. path)))
                     :when (not (.isDirectory entry))]
                 (.getName entry))
               (catch Exception e))
          ())

        (= path "") ()

        :else
        (for [^File file (file-seq-nonr (File. path))
              :when (not (.isDirectory file))]
          (.replace ^String (.getPath file) path ""))))

(defn- all-files-on-classpath
  "Given a list of files on the classpath, returns the list of all files,
  including those located inside jar files."
  [classpath]
  (cache-last-result ::all-files-on-classpath classpath
    (mapcat #(list-files % true) classpath)))

(defn classes-on-classpath
  "Returns a map of all classes that can be located on the classpath. Key
  represent the root package of the class, and value is a list of all classes
  for that package."
  []
  (let [classpath (classpath)]
    (cache-last-result ::classes-on-classpath classpath
      (->> (for [^String file (all-files-on-classpath classpath)
                 :when (and (.endsWith file ".class") (not (.contains file "__"))
                            (not (.contains file "$")))]
             (.. (if (.startsWith file File/separator)
                   (.substring file 1) file)
                 (replace ".class" "") (replace File/separator ".")))
           (group-by #(subs % 0 (max (.indexOf ^String % ".") 0)))))))

(defn namespaces-on-classpath
  "Returns the list of all Clojure namespaces obtained by classpath scanning."
  []
  (let [classpath (classpath)]
    (cache-last-result ::namespaces-on-classpath classpath
      (set (for [^String file (all-files-on-classpath classpath)
                 :when (and (.endsWith file ".clj")
                            (not (.startsWith file "META-INF")))
                 :let [[_ ^String nsname] (re-matches #"[^\w]?(.+)\.clj" file)]
                 :when nsname]
             (.. nsname (replace File/separator ".") (replace "_" "-")))))))

(defn project-resources
  "Returns a list of all non-code files in the current project."
  []
  (let [classpath (classpath)]
    (cache-last-result ::project-resources classpath
      (for [path classpath
            ^String file (list-files path false)
            :when (not (or (empty? file) (.endsWith file ".clj")
                           (.endsWith file ".jar") (.endsWith file ".class")))]
        (if (.startsWith file File/separator)
          (.substring file 1) file)))))
(ns compliment.sources
  "Tools for defining sources for the completion.")

(def ^{:doc "Stores defined sources."
       :private true}
  sources (atom nil))

(defn all-sources
  "Returns the list of all completion sources, or the selected once specified by
  `source-kws`."
  ([] @sources)
  ([source-kws]
   (select-keys @sources source-kws)))

(defn defsource
  "Defines a source with the given name and argument map. Map must
  contain two keys - `:candidates` and `:doc`.

  Value of `:candidates`should be a function of prefix, namespace and
  context.

  Value of `:doc` latter should be a function of symbol name and
  namespace."
  [name & {:as kw-args}]
  {:pre [(every? kw-args [:candidates :doc])]}
  (swap! sources assoc name (assoc kw-args :enabled true)))
(ns compliment.sources.ns-mappings
  "Completion for vars and classes in the current namespace."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches? resolve-namespace
                                      *extra-metadata*]])
  (:import java.io.StringWriter))

(defn var-symbol?
  "Test if prefix resembles a var name."
  [x]
  (re-matches #"([^\/\:][^\.\/]*([^\/\:]*\/[^\.\/]*)?)?" x))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [prefix var]
  (fuzzy-matches? prefix var \-))

(defn get-scope-and-prefix
  "Tries to get take apart scope namespace and prefix in prefixes like
  `scope/var`."
  [^String s, ns]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (resolve-namespace (symbol scope-name) ns))
        prefix (if scope
                 (or sym "") s)]
    [scope-name scope prefix]))

(defn try-get-ns-from-context
  "Tries to extract a namespace name if context is a `ns` definition."
  [context]
  (let [[var-list ns-def use-def top-form] context]
    (when (and (sequential? (:form var-list))
               (= (first (:form top-form)) 'ns)
               (or (and (= (first (:form use-def)) :use)
                        (= (second (:form ns-def)) :only))
                   (and (= (first (:form use-def)) :require)
                        (= (second (:form ns-def)) :refer))))
      (find-ns (first (:form ns-def))))))

(defn generate-docstring
  "Generates a docstring from a given var metadata. Copied from
  `clojure.repl` with some minor modifications."
  [m]
  (binding [*out* (StringWriter.)]
    (println (str (when-let [ns (:ns m)] (str (ns-name ns) "/")) (:name m)))
    (cond
      (:forms m) (doseq [f (:forms m)]
                   (print "  ")
                   (prn f))
      (:arglists m) (prn (:arglists m)))
    (if (:special-form m)
      (do
        (println "Special Form")
        (println " " (:doc m))
        (if (contains? m :url)
          (when (:url m)
            (println (str "\n  Please see http://clojure.org/" (:url m))))
          (println (str "\n  Please see http://clojure.org/special_forms#"
                        (:name m)))))
      (do
        (when (:macro m)
          (println "Macro"))
        (println " " (:doc m))))
    (str *out*)))

(defn candidates
  "Returns a list of namespace-bound candidates, with namespace being
  either the scope (if prefix is scoped), `ns` arg or the namespace
  extracted from context if inside `ns` declaration."
  [^String prefix, ns context]
  (when (var-symbol? prefix)
    (let [[scope-name scope ^String prefix] (get-scope-and-prefix prefix ns)
          ns-form-namespace (try-get-ns-from-context context)
          vars (cond
                 scope (ns-publics scope)
                 ns-form-namespace (ns-publics ns-form-namespace)
                 :else (ns-map ns))]
      (for [[var-sym var] vars
            :let [var-name (name var-sym)
                  {:keys [arglists doc] :as var-meta} (meta var)]
            :when (dash-matches? prefix var-name)]
        (if (= (type var) Class)
          {:candidate var-name, :type :class,
           :package (when-let [pkg (.getPackage ^Class var)]
                      ;; Some classes don't have a package
                      (.getName ^Package pkg))}

          (cond-> {:candidate (if scope
                                (str scope-name "/" var-name)
                                var-name)
                   :type (cond (:macro var-meta) :macro
                               arglists :function
                               :else :var)
                   :ns (str (or (:ns var-meta) ns))}
            (and arglists(:arglists *extra-metadata*))
            (assoc :arglists (apply list (map pr-str arglists)))

            (and doc (:doc *extra-metadata*))
            (assoc :doc (generate-docstring var-meta))))))))

(defn doc
  "Documentation function for this sources' completions."
  [symbol-str ns]
  (if (var-symbol? symbol-str)
    (when-let [var (ns-resolve ns (symbol symbol-str))]
      (when (meta var)
        (generate-docstring (meta var))))))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc #'doc)
(ns compliment.sources.resources
  "Completion for bundled resource files."
  (:require [clojure.java.io :as io]
            [compliment.sources :refer [defsource]]
            [compliment.utils :as utils])
  (:import java.io.File
           java.net.URLConnection))

(defn inside-resource-call?
  "If context is not nil, check if prefix inside the string in a
  clojure.java.io/resource call."
  [ctx]
  (when (and ctx)
    (let [[str call] ctx
          fn (first (:form call))]
      (and (string? (:form str))
           (list? (:form call))
           (symbol? fn)
           (= (name fn) "resource")))))

(defn candidates
  "Returns list of completions for project resources if within certain context."
  [prefix _ context]
  (when (inside-resource-call? context)
    (for [^String res (utils/project-resources)
          :when (.startsWith res prefix)]
      {:candidate res
       :type :resource})))

(defn doc
  "Documentation function for project resources."
  [resource-name _]
  (try (let [^String filename (.getFile (io/resource resource-name))]
         (format "File type: %s, size: %d bytes"
                 (or (URLConnection/guessContentTypeFromName filename)
                     "application/unknown")
                 (.length (io/file filename))))
       (catch Exception ex nil)))

(defsource ::resources
  :candidates #'candidates
  :doc #'doc)
(ns compliment.sources.special-forms
  "Completion for Clojure's special forms."
  (:require [clojure.repl :as repl]
            [compliment.sources :refer [defsource]]
            [compliment.sources.ns-mappings :as vars]))

(def ^:private special-forms
  (set (map name '[def if do quote var recur throw try catch
                   monitor-enter monitor-exit new set!])))

(defn first-item-in-list?
  "If context is not nil, check if prefix is the first item in a list form."
  [ctx]
  (if ctx
    (when-let [expr (first ctx)]
      (and (list? (:form expr)) (= (:idx expr) 0)))
    true))

(defn candidates
  "Returns list of completions for special forms."
  [prefix _ context]
  (when (and (vars/var-symbol? prefix) (first-item-in-list? context))
    (for [form special-forms
          :when (vars/dash-matches? prefix form)]
      {:candidate form
       :type :special-form})))

(defn doc
  "Documentation function for special forms."
  [symbol-str _]
  (when (and (vars/var-symbol? symbol-str) (special-forms symbol-str))
    (vars/generate-docstring (#'repl/special-doc (symbol symbol-str)))))

(defsource ::special-forms
  :candidates #'candidates
  :doc #'doc)

(defn literal-candidates
  "We define `true`, `false`, and `nil` in a separate source because they are
  not context-dependent (don't have to be first items in the list)."
  [prefix _ __]
  (->> ["true" "false" "nil"]
       (filter #(.startsWith ^String % prefix))
       (map (fn [c] {:candidate c, :type :special-form}))))

(defsource ::literals
  :candidates #'literal-candidates
  :doc (constantly nil))
(ns compliment.sources.class-members
  "Completion for both static and non-static class members."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches-no-skip? resolve-class]]
            [clojure.string :refer [join]])
  (:import [java.lang.reflect Method Field Member Modifier]))

(defn static?
  "Tests if class member is static."
  [^Member member]
  (Modifier/isStatic (.getModifiers member)))

;; ## Regular (non-static) members

(def ^{:doc "Stores cache of all non-static members for every
  namespace."}
  members-cache (atom {}))

(defn populate-members-cache
  "Populates members cache for a given namespace. `classes-cnt` is a
  number that indicates the current number of imported classes in this
  namespace."
  [ns classes-cnt]
  (loop [cache (transient {})

         [^Member c & r]
         (for [^Class class (vals (ns-map ns))
               :when (class? class)
               ^Member member (concat (.getMethods class) (.getFields class))
               :when (not (static? member))]
           (let [dc (.getDeclaringClass member)]
             (if (= dc class)
               member
               (if (instance? Method member)
                 (.getMethod dc (.getName member)
                             (.getParameterTypes ^Method member))
                 (.getField dc (.getName member))))))]
    (if c
      (let [full-name (.getName c)]
        (if (cache full-name)
          (recur (assoc! cache full-name (conj (cache (.getName c)) c)) r)
          (recur (assoc! cache full-name [c]) r)))
      (swap! members-cache assoc ns {:classes-cnt classes-cnt
                                     :methods (persistent! cache)}))))

(defn update-cache
  "Updates members cache for a given namespace if necessary."
  [ns]
  (let [imported-cls-cnt (count (filter class? (vals (ns-map *ns*))))]
    (when (or (nil? (@members-cache ns))
              (not= (get-in @members-cache [ns :classes-cnt])
                    imported-cls-cnt))
      (populate-members-cache ns imported-cls-cnt))))

(defn get-all-members
  "Returns all non-static members for a given namespace."
  [ns]
  (update-cache ns)
  (get-in @members-cache [ns :methods]))

(defn class-member-symbol?
  "Tests if a symbol name looks like a non-static class member."
  [^String x]
  (.startsWith x "."))

(defn camel-case-matches?
  "Tests if prefix matches the member name following camel case rules.
  Thus, prefix `getDeF` matches member `getDeclaredFields`."
  [prefix member-name]
  (fuzzy-matches-no-skip? prefix member-name #(Character/isUpperCase ^char %)))

(defn try-get-object-class
  "Tries to get the type of the object from the context, which the
  member will be applied to. Object should be a Var."
  [ns context]
  (when (= (:idx (first context)) 0)
    (let [sym (second (:form (first context)))]
      (when (and (symbol? sym)
                 (= (type (ns-resolve ns sym)) clojure.lang.Var))
        (type (deref (ns-resolve ns sym)))))))

(defn members-candidates
  "Returns a list of Java non-static fields and methods candidates."
  [prefix ns context]
  (when (class-member-symbol? prefix)
    (let [prefix (subs prefix 1)
          inparts? (re-find #"[A-Z]" prefix)
          klass (try-get-object-class ns context)]
      (for [[member-name members] (get-all-members ns)
            :when (if inparts?
                    (camel-case-matches? prefix member-name)
                    (.startsWith ^String member-name prefix))
            :when
            (or (not klass)
                (some #(= klass (.getDeclaringClass ^Member %)) members))]
        {:candidate (str "." member-name)
         :type (if (instance? Method (first members))
                 :method :field)}))))

;; ### Member documentation

(defn type-to-pretty-string
  "Takes a type (either a class or a primitive) and returns it's
  human-readable name."
  [^Class t]
  (if (or (.isLocalClass t)
          (.isMemberClass t))
    (.getName t)
    (.getSimpleName t)))

(defn doc-method-parameters
  "Takes a list of method parameters and stringifies it."
  [parameters]
  (->> parameters
       (map type-to-pretty-string)
       (interpose " ")
       join
       (format "(%s)")))

(defn create-members-doc
  "Takes a list of members (presumably with the same name) and turns
  them into a docstring."
  [members]
  (->> members
       (group-by (fn [^Member m] (.getDeclaringClass m)))
       (map (fn [[^Class class, members]]
              (let [^Member f-mem (first members)]
                (str (.getName class) "." (.getName f-mem)
                     (if (instance? Field f-mem)
                       (str " = " (try (.get ^Field f-mem nil)
                                       (catch Exception e "?"))
                            " (" (type-to-pretty-string (.getType ^Field f-mem)) ")\n"
                            (Modifier/toString (.getModifiers f-mem)))
                       (join
                        (map (fn [^Method member]
                               (when (instance? Method member)
                                 (str "\n  " (doc-method-parameters (.getParameterTypes member))
                                      " -> " (type-to-pretty-string (.getReturnType ^Method member))
                                      " (" (Modifier/toString (.getModifiers member)) ")")))
                             (distinct members))))
                     "\n"))))
       (interpose "\n")
       join))

(defn members-doc
  "Documentation function for non-static members."
  [member-str ns]
  (when (class-member-symbol? member-str)
    (update-cache ns)
    (when-let [member (get-in @members-cache [ns :methods (subs member-str 1)])]
      (create-members-doc member))))

(defn classname-doc [^Class class]
  (let [members (group-by static? (concat (.getMethods class)
                                          (.getFields class)))
        [static non-static] (for [flag [true false]]
                              (->> (for [^Member m (members flag)]
                                     (.getName m))
                                   distinct
                                   (interpose ", ")
                                   join))]
    (str (.getName class) "\n\n"
         " Non-static members:\n  " non-static "\n\n"
         " Static members:\n  " static "\n")))

(defsource ::members
  :candidates #'members-candidates
  :doc #'members-doc
  :tag-fn (fn [m {:keys [ns]}]
            (assoc m :type (if (->> (get-in @members-cache [ns :methods
                                                            (subs (:candidate m) 1)])
                                    first
                                    (instance? Method))
                             :method :field))))

;; ## Static members

(defn static-member-symbol?
  "Tests if prefix looks like a static member symbol."
  [x]
  (re-matches #"[^\/\:\.][^\:]*\/.*" x))

(def ^{:doc "Stores cache of all static members for every class."}
  static-members-cache (atom {}))

(defn populate-static-members-cache
  "Populates static members cache for a given class."
  [^Class class]
  (loop [cache {}, [^Member c & r] (concat (.getMethods class)
                                           (.getFields class))]
    (if c
      (if (static? c)
        (let [full-name (.getName c)]
          (if (cache (.getName c))
            (recur (update-in cache [full-name] conj c) r)
            (recur (assoc cache full-name [c]) r)))
        (recur cache r))
      (swap! static-members-cache assoc class cache))))

(defn update-static-cache
  "Updates static members cache for a given class if necessary."
  [class]
  (when-not (@static-members-cache class)
    (populate-static-members-cache class)))

(defn static-members
  "Returns all static members for a given class."
  [^Class class]
  (update-static-cache class)
  (@static-members-cache class))

(defn static-members-candidates
  "Returns a list of static member candidates."
  [^String prefix, ns context]
  (when (static-member-symbol? prefix)
    (let [[cl-name member-prefix] (.split prefix "/")
          cl (resolve-class ns (symbol cl-name))
          member-prefix (or member-prefix "")]
      (when cl
        (let [inparts? (re-find #"[A-Z]" member-prefix)]
          (for [[^String member-name members] (static-members cl)
                :when  (if inparts?
                         (camel-case-matches? member-prefix member-name)
                         (.startsWith member-name member-prefix))]
            {:candidate (str cl-name "/" member-name)
             :type (if (instance? Method (first members))
                     :static-method :static-field)}))))))

(defn resolve-static-member
  "Given a string representation of a static member returns Member object."
  [^String member-str ns]
  (let [[cl-name member-name] (.split member-str "/")
        cl (resolve-class ns (symbol cl-name))]
    (when cl
      (update-static-cache cl)
      (get-in @static-members-cache [cl member-name]))))

(defn static-member-doc
  "Given a member name and class returns its docstring."
  [member-str ns]
  (when (static-member-symbol? member-str)
    (let [member (resolve-static-member member-str ns)]
      (when member
        (create-members-doc member)))))

(defsource ::static-members
  :candidates #'static-members-candidates
  :doc #'static-member-doc)
(ns compliment.sources.keywords
  "Completion for keywords interned globally across the application"
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [defmemoized resolve-namespace]])
  (:import java.lang.reflect.Field))

(defmemoized ^:private keywords-table
  []
  (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
    (.setAccessible field true)
    (.get field nil)))

(defn- tagged-candidate [c]
  {:candidate c, :type :keyword})

(defn qualified-candidates
  "Returns a list of namespace-qualified double-colon keywords (like ::foo)
  resolved for the given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [[kw _] (keywords-table)
          :when (= (namespace kw) ns-name)
          :when (.startsWith (name kw) prefix)]
      (tagged-candidate (str "::" (name kw))))))

(defn namespace-alias-candidates
  "Returns a list of namespace aliases prefixed by double colon required in the
  given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [[alias _] (ns-aliases ns)
          :let [aname (name alias)]
          :when (.startsWith aname prefix)]
      (tagged-candidate (str "::" aname)))))

(defn aliased-candidates
  "Returns a list of alias-qualified double-colon keywords (like ::str/foo),
  where alias has to be registered in the given namespace."
  [prefix ns]
  (when-let [[_ alias prefix] (re-matches #"::([^/]+)/(.*)" prefix)]
    (let [alias-ns-name (str (resolve-namespace (symbol alias) ns))]
      (for [[kw _] (keywords-table)
            :when (= (namespace kw) alias-ns-name)
            :when (.startsWith (name kw) prefix)]
        (tagged-candidate (str "::" alias "/" (name kw)))))))

(defn candidates
  [^String prefix, ns _]
  (let [single-colon? (.startsWith prefix ":")
        double-colon? (.startsWith prefix "::")
        has-slash? (> (.indexOf prefix "/") -1)]
    (cond (and double-colon? has-slash?) (aliased-candidates prefix ns)
          double-colon? (concat (qualified-candidates prefix ns)
                                (namespace-alias-candidates prefix ns))
          single-colon? (for [[kw _] (keywords-table)
                              :when (.startsWith (str kw) (subs prefix 1))]
                          (tagged-candidate (str ":" kw))))))

(defsource ::keywords
  :candidates #'candidates
  :doc (constantly nil))
(ns compliment.sources.local-bindings
  "Completion source for local bindings introduced by defn, let and the like."
  (:require [compliment.sources :refer [defsource]]
            [compliment.sources.ns-mappings :refer [var-symbol? dash-matches?]]))

(def let-like-forms '#{let if-let when-let if-some when-some loop with-open
                       dotimes with-local-vars})

(def defn-like-forms '#{defn defn- fn defmacro})

(def doseq-like-forms '#{doseq for})

(def letfn-like-forms '#{letfn})

(defn parse-binding
  "Given a binding node returns the list of local bindings introduced by that
  node. Handles vector and map destructuring."
  [binding-node]
  (cond (vector? binding-node)
        (mapcat parse-binding binding-node)

        (map? binding-node)
        (let [normal-binds (->> (keys binding-node)
                                (remove keyword?)
                                (mapcat parse-binding))
              keys-binds (if-let [ks (:keys binding-node)]
                           (mapv str ks) ())
              as-binds (if-let [as (:as binding-node)]
                        [(str as)] ())]
          (concat normal-binds keys-binds as-binds))

        (not (#{'& '_} binding-node))
        [(str binding-node)]))

(defn parse-fn-body
  "Extract function name and arglists from the function body, return list of all
  completable variables."
  [fn-body]
  (let [fn-name (when (symbol? (first fn-body))
                  (name (first fn-body)))
        fn-body (if fn-name (rest fn-body) fn-body)]
    (cond->
        (mapcat parse-binding
                (loop [[c & r] fn-body, bnodes []]
                  (cond (nil? c) bnodes
                        (list? c) (recur r (conj bnodes (first c))) ;; multi-arity case
                        (vector? c) c                               ;; single-arity case
                        :else (recur r bnodes))))
      fn-name (conj fn-name))))

(defn extract-local-bindings
  "When given a form that has a binding vector traverses that binding vector and
  returns the list of all local bindings."
  [form]
  (when (list? form)
    (cond (let-like-forms (first form))
          (mapcat parse-binding (take-nth 2 (second form)))

          (defn-like-forms (first form)) (parse-fn-body (rest form))

          (letfn-like-forms (first form))
          (mapcat parse-fn-body (second form))

          (doseq-like-forms (first form))
          (->> (partition 2 (second form))
               (mapcat (fn [[left right]]
                         (if (= left :let)
                           (take-nth 2 right) [left])))
               (mapcat parse-binding))

          (= 'as-> (first form)) [(name (nth form 2))])))

(defn bindings-from-context
  "Returns all local bindings that are established inside the given context."
  [ctx]
  (try (distinct (mapcat (comp extract-local-bindings :form) ctx))
       (catch Exception ex ())))

(defn candidates
  "Returns a list of local bindings inside the context that match prefix."
  [prefix _ context]
  (when (var-symbol? prefix)
    (for [binding (bindings-from-context context)
          :when (dash-matches? prefix binding)]
      {:candidate binding, :type :local})))

(defsource ::local-bindings
  :candidates #'candidates
  :doc (constantly nil))
(ns compliment.sources.namespaces-and-classes
  "Completion for namespace and class names."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches?] :as utils]
            [compliment.sources.class-members :refer [classname-doc]])
  (:import java.io.File))

(defn nscl-symbol?
  "Tests if prefix looks like a namespace or classname."
  [x]
  (re-matches #"[^\/\:\.][^\/\:]+" x))

(defn nscl-matches?
  "Tests if prefix partially matches a var name with periods as
  separators."
  [prefix namespace]
  (fuzzy-matches? prefix namespace \.))

;;; Obtaining the list of classes

(defn imported-classes
  "Returns names of all classes imported into a given namespace."
  [ns]
  (for [[_ ^Class val] (ns-map ns) :when (class? val)]
    (.getName val)))

(defn all-classes-short-names
  "Returns a map where short classnames are matched with vectors with
  package-qualified classnames."
  []
  (let [all-classes (utils/classes-on-classpath)]
    (utils/cache-last-result ::all-classes-short-names all-classes
      (group-by #(-> (re-matches #"([^\.]+\.)*([^\.]+)" %)
                     (nth 2))
                (reduce into [] (vals all-classes))))))

(defn- analyze-import-context
  "Checks if the completion is called from ns import declaration. If so, and the
  prefix is inside import vector, return that package name, otherwise return
  `:root`. If not inside :import, return nil."
  [ctx]
  (let [ns-decl (:form (last ctx))
        import-list (:form (last (butlast ctx)))
        prefix-form (:form (first ctx))]
    (when (and (sequential? ns-decl)
               (= (first ns-decl) 'ns)
               (sequential? import-list)
               (= (first import-list) :import))
      (if (= prefix-form import-list)
        :root
        (str (first prefix-form))))))

(defn- get-all-full-names
  "Returns a list of package-qualified classnames given a short classname."
  [prefix]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (.startsWith short-name prefix)
                 (concat l (map (fn [c] {:candidate c, :type :class})
                                full-names))
                 l))
             ()
             (all-classes-short-names)))

(defn- get-classes-by-package-name
  "Returns simple classnames that match the `prefix` and belong to `pkg-name`."
  [prefix pkg-name]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (and (.startsWith short-name prefix)
                        (some #(.startsWith ^String % pkg-name) full-names))
                 (conj l {:candidate short-name, :type :class})
                 l))
             ()
             (all-classes-short-names)))

(defn candidates
  "Returns a list of namespace and classname completions."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)
          import-ctx (analyze-import-context context)]
      ((comp distinct concat)
       (for [ns-str (concat (map (comp name ns-name) (all-ns))
                            (map name (keys (ns-aliases ns))))
             :when (nscl-matches? prefix ns-str)]
         {:candidate ns-str, :type :namespace})
       (for [class-str (imported-classes ns)
             :when (nscl-matches? prefix class-str)]
         {:candidate class-str, :type :class})
       (cond (= import-ctx :root) (get-all-full-names prefix)
             import-ctx (get-classes-by-package-name prefix import-ctx))
       ;; Fuzziness is too slow for all classes, so just startsWith.
       ;; Also have to do clever tricks to keep the performance high.
       (if has-dot
         (concat (for [[root-pkg classes] (utils/classes-on-classpath)
                       :when (.startsWith prefix root-pkg)
                       ^String cl-str classes
                       :when (.startsWith cl-str prefix)]
                   {:candidate cl-str, :type :class})
                 (for [ns-str (utils/namespaces-on-classpath)
                       :when (nscl-matches? prefix ns-str)]
                   {:candidate ns-str, :type :namespace}))
         (concat (for [[^String root-pkg _] (utils/classes-on-classpath)
                       :when (.startsWith root-pkg prefix)]
                   {:candidate (str root-pkg "."), :type :class})
                 (for [^String ns-str (utils/namespaces-on-classpath)
                       :when (.startsWith ns-str prefix)]
                   {:candidate ns-str, :type :namespace})))))))

(defn doc [ns-or-class-str curr-ns]
  (when (nscl-symbol? ns-or-class-str)
    (if-let [ns (find-ns (symbol ns-or-class-str))]
      (str ns "\n" (:doc (meta ns)) "\n")
      (when-let [class (try (ns-resolve curr-ns (symbol ns-or-class-str))
                            (catch Exception ex nil))]
        (when (= (type class) Class)
          (classname-doc class))))))

(defsource ::namespaces-and-classes
  :candidates #'candidates
  :doc #'doc)
(ns compliment.context
  "Utilities for parsing and storing the current completion context."
  (:require [clojure.walk :refer [walk]]))

(defn- restore-map-literals [context]
  (clojure.walk/postwalk (fn [el]
                           (if (and (sequential? el)
                                    (= (first el) 'compliment-hashmap))
                             (apply hash-map
                                    (if (even? (count el))
                                      (concat (rest el) [nil])
                                      (rest el)))
                             el)) context))

(defn- safe-read-context-string [^String context]
  (try (-> context
           (.replace "{" "(compliment-hashmap ")
           (.replace "}" ")")
           read-string
           restore-map-literals)
       (catch Exception ex nil)))

(def ^{:doc "Stores the last completion context."
       :private true}
  previous-context (atom nil))

(def ^{:doc "Special symbol which substitutes prefix in the context,
  so the former can be found unambiguously."}
  prefix-placeholder '__prefix__)

(defn parse-context
  "Takes a context which is a Lisp form and returns a transformed context.

  The result is a list of maps, each map represents a level of the
  context from inside to outside. Map has `:idx` and `:form` values,
  and `:map-role` if the level is a map. `:idx` defines the position
  of prefix (or the form containing prefix) on the current
  level (number for lists and vectors, key or value for maps).

  Example: `(dotimes [i 10] ({:foo {:baz __prefix__}, :bar 42} :quux))`

  Transformed it looks like:

  `({:idx :baz, :map-role :value, :form {:baz __prefix__}}
    {:idx :foo, :map-role :key, :form {:foo {:baz __prefix__}, :bar 42}}
    {:idx 0, :form ({:foo {:baz __prefix__}, :bar 42} :quux)}
    {:idx 2, :form (dotimes [i 10] ({:foo {:baz __prefix__}, :bar 42} :quux))})`."
  [context]
  (let [parse (fn parse [ctx]
                (cond
                 (sequential? ctx)
                 (when-let [res (first (keep-indexed (fn [idx el]
                                                       (when-let [p (parse el)]
                                                         [idx p]))
                                                     ctx))]
                   (cons {:idx (first res) :form ctx} (second res)))

                 (map? ctx)
                 (when-let [res (first (keep (fn [[k v]]
                                               (if-let [p (parse v)]
                                                 [k :value p]
                                                 (when-let [p (parse k)]
                                                   [v :key p])))
                                             ctx))]
                   (cons {:idx (first res) :map-role (second res) :form ctx}
                         (nth res 2)))

                 (string? ctx)
                 (let [idx (.indexOf ^String ctx (name prefix-placeholder))]
                   (when (>= idx 0)
                     [{:idx idx :form ctx}]))

                 (= ctx prefix-placeholder) ()))
        parsed (parse context)]
    (when parsed
      (reverse parsed))))

(defn cache-context
  "Parses the context, or returns one from cache if it was unchanged."
  [context-string]
  (let [context (safe-read-context-string context-string)]
    (when-not (= context :same)
      (reset! previous-context (parse-context context))))
  @previous-context)
;; ## Compliment - a completion library you deserve.
;; This library provides a fast and extensible way to complete symbols in your
;; editor. It is intended to be maximally editor-agnostic where
;; possible, to avoid duplicating implementation in different clients.

(ns compliment.core
  "Core namespace. Most interactions with Compliment should happen
  through functions defined here."
  (:require (compliment.sources ns-mappings
                                namespaces-and-classes
                                class-members
                                keywords
                                special-forms
                                local-bindings
                                resources)
            [compliment.sources :refer [all-sources]]
            [compliment.context :refer [cache-context]]
            [compliment.utils :refer [*extra-metadata*]]
            [clojure.string :refer [join]])
  (:import java.util.Comparator))

(def all-files
  "List of all Compliment files in an order they should be loaded. This is
  required by REPLy."
  (map (partial format "compliment/%s.clj")
       ["utils" "context" "sources" "sources/class_members"
        "sources/ns_mappings" "sources/namespaces_and_classes"
        "sources/keywords" "sources/special_forms" "sources/local_bindings"
        "sources/resources"
        "core"]))

(def ^:private by-length-comparator
  (reify Comparator
    (compare [_ s1 s2]
      (let [res (compare (count s1) (count s2))]
        (if (zero? res)
          (compare s1 s2)
          res)))))

(defn sort-by-length
  "Sorts list of strings by their length first, and then alphabetically if
  length is equal. Works for tagged and non-tagged results."
  [candidates]
  (sort-by :candidate by-length-comparator candidates))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [ns]
  (cond (instance? clojure.lang.Namespace ns) ns
        (symbol? ns) (or (find-ns ns) (find-ns 'user) *ns*)
        :else *ns*))

(defn completions
  "Returns a list of completions for the given prefix. Options map can contain
  the following options:
  - :ns - namespace where completion is initiated;
  - :context - code form around the prefix;
  - :sort-order (either :by-length or :by-name);
  - :plain-candidates - if true, returns plain strings instead of maps;
  - :extra-metadata - set of extra fields to add to the maps;
  - :sources - list of source keywords to use."
  ([prefix]
   (completions prefix {}))
  ([prefix options-map]
   (if (string? options-map)
     (completions prefix {:context options-map})
     (let [{:keys [ns context sort-order sources extra-metadata]
            :or {sort-order :by-length}} options-map
           ns (ensure-ns ns)
           options-map (assoc options-map :ns ns)
           ctx (cache-context context)
           sort-fn (if (= sort-order :by-name)
                     (partial sort-by :candidate)
                     (partial sort-by-length true))]
       (binding [*extra-metadata* extra-metadata]
         (let [candidate-fns (keep (fn [[_ src]]
                                     (when (:enabled src)
                                       (:candidates src)))
                                   (if sources
                                     (all-sources sources)
                                     (all-sources)))]
           (as-> (mapcat (fn [f] (f prefix ns ctx)) candidate-fns)
               candidates

             (if (= sort-order :by-name)
               (sort-by :candidate candidates)
               (sort-by :candidate by-length-comparator candidates))

             (if (:plain-candidates options-map)
               (map :candidate candidates)
               candidates)

             (doall candidates))))))))

(defn documentation
  "Returns a documentation string that describes the given symbol."
  ([symbol-str]
   (documentation symbol-str *ns*))
  ([symbol-str ns]
   (if (empty? symbol-str)
     ""
     (->> (for [[_ {:keys [doc enabled]}] (all-sources)
                :when enabled
                :let [docstr (doc symbol-str (ensure-ns ns))]
                :when docstr]
            docstr)
          (interpose "\n\n")
          join))))
(clojure.core/let [prefix__641__auto__ (clojure.core/name (clojure.core/gensym)) code__642__auto__ (.replaceAll "(ns unrepl.print\n  (:require [clojure.string :as str]\n    [clojure.edn :as edn]))\n\n(def ^:dynamic *elide*\n  \"Function of 1 argument which returns the elision.\"\n  (constantly nil))\n\n(def ^:dynamic *attach* nil)\n\n(def ^:dynamic *string-length* 80)\n\n(defprotocol DefaultEdnize\n  (default-ednize [x]))\n\n(def ^:dynamic *ednize* #'default-ednize)\n\n(def ^:dynamic *realize-on-print*\n  \"Set to false to avoid realizing lazy sequences.\"\n  true)\n\n(deftype ElidedKVs [s]\n  clojure.lang.Seqable\n  (seq [_] (seq s)))\n\n(def atomic? (some-fn nil? true? false? char? string? symbol? keyword? #(and (number? %) (not (ratio? %)))))\n\n(defn- as-str\n  \"Like pr-str but escapes all ASCII control chars.\"\n  [x]\n  ;hacky\n  (cond\n    (string? x) (str/replace (pr-str x) #\"\\p{Cntrl}\"\n                  #(format \"\\\\u%04x\" (int (.charAt ^String % 0))))\n    (char? x) (str/replace (pr-str x) #\"\\p{Cntrl}\"\n                #(format \"u%04x\" (int (.charAt ^String % 0))))\n    :else (pr-str x)))\n\n(defmacro ^:private latent-fn [& fn-body]\n  `(let [d# (delay (binding [*ns* (find-ns '~(ns-name *ns*))] (eval '(fn ~@fn-body))))]\n     (fn\n       ([] (@d#))\n       ([x#] (@d# x#))\n       ([x# & xs#] (apply @d# x# xs#)))))\n\n(defn- as-inst [x]\n  (edn/read-string {:readers {'inst #(tagged-literal 'inst %)}} (pr-str x)))\n\n(def ^:dynamic *object-representations*\n  \"map of classes to functions returning their representation component (3rd item in #unrepl/object [class id rep])\"\n  {clojure.lang.IDeref\n   (fn [x]\n     (let [pending? (and (instance? clojure.lang.IPending x) ; borrowed from https://github.com/brandonbloom/fipp/blob/8df75707e355c1a8eae5511b7d73c1b782f57293/src/fipp/ednize.clj#L37-L51\n                      (not (.isRealized ^clojure.lang.IPending x)))\n           [ex val] (when-not pending?\n                      (try [false @x]\n                        (catch Throwable e\n                          [true e])))\n           failed? (or ex (and (instance? clojure.lang.Agent x)\n                            (agent-error x)))\n           status (cond\n                    failed? :failed\n                    pending? :pending\n                    :else :ready)]\n       {:unrepl.ref/status status :unrepl.ref/val val}))\n   \n   java.io.File (fn [^java.io.File f]\n                  (into {:path (.getPath f)}\n                    (when (and *attach* (.isFile f))\n                      {:attachment (tagged-literal 'unrepl/mime\n                                     (into {:content-type \"application/octet-stream\"\n                                           :content-length (.length f)}\n                                       (*attach* #(java.io.FileInputStream. f))))})))\n   \n   java.awt.Image (latent-fn [^java.awt.Image img]\n                    (let [w (.getWidth img nil)\n                          h (.getHeight img nil)]\n                      (into {:width w, :height h}\n                       (when *attach*\n                         {:attachment\n                          (tagged-literal 'unrepl/mime\n                            (into {:content-type \"image/png\"}\n                              (*attach* #(let [bos (java.io.ByteArrayOutputStream.)]\n                                               (when (javax.imageio.ImageIO/write\n                                                       (doto (java.awt.image.BufferedImage. w h java.awt.image.BufferedImage/TYPE_INT_ARGB)\n                                                         (-> .getGraphics (.drawImage img 0 0 nil)))\n                                                       \"png\" bos)\n                                                 (java.io.ByteArrayInputStream. (.toByteArray bos)))))))}))))\n   \n   Object (fn [x]\n            (if (-> x class .isArray)\n              (seq x)\n              (str x)))})\n\n(defn- object-representation [x]  \n  (reduce-kv (fn [_ class f]\n               (when (instance? class x) (reduced (f x)))) nil *object-representations*)) ; todo : cache\n\n(defn- class-form [^Class x]\n  (if (.isArray x) [(-> x .getComponentType class-form)] (symbol (.getName x))))\n\n(extend-protocol DefaultEdnize\n  clojure.lang.TaggedLiteral (default-ednize [x] x)\n  clojure.lang.Ratio (default-ednize [^clojure.lang.Ratio x] (tagged-literal 'unrepl/ratio [(.numerator x) (.denominator x)]))\n  clojure.lang.Var (default-ednize [x]\n                     (tagged-literal 'clojure/var\n                       (when-some [ns (:ns (meta x))] ; nil when local var\n                         (symbol (name (ns-name ns)) (name (:name (meta x)))))))\n  Throwable (default-ednize [t] (tagged-literal 'error (Throwable->map t)))\n  Class (default-ednize [x] (tagged-literal 'unrepl.java/class (class-form x)))\n  java.util.Date (default-ednize [x] (as-inst x))\n  java.util.Calendar (default-ednize [x] (as-inst x))\n  java.sql.Timestamp (default-ednize [x] (as-inst x))\n  clojure.lang.Namespace (default-ednize [x] (tagged-literal 'unrepl/ns (ns-name x)))\n  java.util.regex.Pattern (default-ednize [x] (tagged-literal 'unrepl/pattern (str x)))\n  Object\n  (default-ednize [x]\n    (tagged-literal 'unrepl/object\n      [(class x) (format \"0x%x\" (System/identityHashCode x)) (object-representation x)\n       {:bean {(tagged-literal 'unrepl/... (*elide* (ElidedKVs. (bean x)))) (tagged-literal 'unrepl/... nil)}}])))\n\n(defmacro ^:private blame-seq [& body]\n  `(try (seq ~@body)\n     (catch Throwable t#\n       (list (tagged-literal 'unrepl/lazy-error t#)))))\n\n(defn- may-print? [s]\n  (or *realize-on-print* (not (instance? clojure.lang.IPending s)) (realized? s)))\n\n(defn- elide-vs [vs print-length]\n  (cond\n    (pos? print-length)\n    (lazy-seq\n      (if (may-print? vs)\n        (if-some [[v :as vs] (blame-seq vs)]\n          (cons v (elide-vs (rest vs) (dec print-length)))\n          ())\n        (list (tagged-literal 'unrepl/... (*elide* vs)))))\n    (and (may-print? vs) (nil? (blame-seq vs))) ()\n    :else (list (tagged-literal 'unrepl/... (*elide* vs)))))\n\n(defn- elide-kvs [kvs print-length]\n  (if-some [more-kvs (when print-length (seq (drop print-length kvs)))]\n    (concat (take print-length kvs) [[(tagged-literal 'unrepl/... (*elide* (ElidedKVs. more-kvs))) (tagged-literal 'unrepl/... nil)]])\n    kvs))\n\n(defn ednize \"Shallow conversion to edn safe subset.\" \n  ([x] (ednize x *print-length* *print-meta*))\n  ([x print-length] (ednize x print-length *print-meta*))\n  ([x print-length print-meta]\n  (cond\n    (atomic? x) x\n    (and print-meta (meta x)) (tagged-literal 'unrepl/meta [(meta x) (ednize x print-length false)])\n    (map? x) (into {} (elide-kvs x print-length))\n    (instance? ElidedKVs x) (ElidedKVs. (elide-kvs x print-length))\n    (instance? clojure.lang.MapEntry x) x\n    (vector? x) (into (empty x) (elide-vs x print-length))\n    (seq? x) (elide-vs x print-length)\n    (set? x) (into #{} (elide-vs x print-length))\n    :else (let [x' (*ednize* x)]\n            (if (= x x')\n              x\n              (recur x' print-length print-meta)))))) ; todo : cache\n\n(declare print-on)\n\n(defn- print-vs \n  ([write vs rem-depth]\n    (print-vs write vs rem-depth print-on \" \"))\n  ([write vs rem-depth print-v sep]\n    (when-some [[v & vs] (seq vs)]\n      (print-v write v rem-depth)\n      (doseq [v vs]\n        (write sep)\n        (print-v write v rem-depth)))))\n\n(defn- print-kv [write [k v] rem-depth]\n  (print-on write k rem-depth)\n  (write \" \")\n  (print-on write v rem-depth))\n\n(defn- print-kvs [write kvs rem-depth]\n    (print-vs write kvs rem-depth print-kv \", \"))\n\n(defn- print-on [write x rem-depth]\n  (let [rem-depth (dec rem-depth)\n        x (ednize x (if (neg? rem-depth) 0 *print-length*))]\n    (cond\n      (tagged-literal? x)\n      (do (write (str \"#\" (:tag x) \" \"))\n        (case (:tag x)\n          unrepl/... (binding ; don't elide the elision \n                       [*print-length* Long/MAX_VALUE\n                        *print-level* Long/MAX_VALUE\n                        *string-length* Long/MAX_VALUE]\n                       (print-on write (:form x) Long/MAX_VALUE))\n          (recur write (:form x) rem-depth)))\n      (or (map? x) (instance? ElidedKVs x)) (do (write \"{\") (print-kvs write x rem-depth) (write \"}\"))\n      (vector? x) (do (write \"[\") (print-vs write x rem-depth) (write \"]\"))\n      (seq? x) (do (write \"(\") (print-vs write x rem-depth) (write \")\"))\n      (set? x) (do (write \"#{\") (print-vs write x rem-depth) (write \"}\"))\n      (and (string? x) (> (count x) *string-length*))\n      (let [i (if (and (Character/isHighSurrogate (.charAt ^String x (dec *string-length*)))\n                    (Character/isLowSurrogate (.charAt ^String x *string-length*)))\n                (inc *string-length*) *string-length*)\n            prefix (subs x 0 i)\n            rest (subs x i)]\n        (if (= rest \"\")\n          (write (as-str x))\n          (do\n            (write \"#unrepl/string [\")\n            (write (as-str prefix))\n            (write \" \")\n            (print-on write (tagged-literal 'unrepl/... (*elide* rest)) rem-depth)\n            (write \"]\"))))\n      (atomic? x) (write (as-str x))\n      :else (throw (ex-info \"Can't print value.\" {:value x})))))\n\n(defn edn-str [x]\n  (let [out (java.io.StringWriter.)\n        write (fn [^String s] (.write out s))]\n    (binding [*print-readably* true\n              *print-length* (or *print-length* 10)]\n      (print-on write x (or *print-level* 8))\n      (str out))))\n\n(defn full-edn-str [x]\n  (binding [*print-length* Long/MAX_VALUE\n            *print-level* Long/MAX_VALUE]\n    (edn-str x)))\n(ns unrepl.repl\n  (:require [clojure.main :as m]\n    [unrepl.print :as p]\n    [clojure.edn :as edn]\n    [clojure.java.io :as io]))\n\n(defn classloader\n  \"Creates a classloader that obey standard delegating policy.\n   Takes two arguments: a parent classloader and a function which\n   takes a keyword (:resource or :class) and a string (a resource or a class name) and returns an array of bytes\n   or nil.\"\n  [parent f]\n  (let [define-class (doto (.getDeclaredMethod ClassLoader \"defineClass\" (into-array [String (Class/forName \"[B\") Integer/TYPE Integer/TYPE]))\n                       (.setAccessible true))]\n    (proxy [ClassLoader] [parent]\n      (findResource [name]\n        (when-some  [bytes (f :resource name)]\n          (let [file (doto (java.io.File/createTempFile \"unrepl-sideload-\" (str \"-\" (re-find #\"[^/]*$\" name)))\n                       .deleteOnExit)]\n            (io/copy bytes file)\n            (-> file .toURI .toURL))))\n      (findClass [name]\n        (if-some  [bytes (f :class name)]\n          (.invoke define-class this (to-array name bytes 0 (count bytes)))\n          (throw (ClassNotFoundException. name)))))))\n\n(defn tagging-writer\n  ([write]\n    (proxy [java.io.Writer] []\n      (close []) ; do not cascade\n      (flush []) ; atomic always flush\n      (write\n        ([x]\n          (write (cond \n                   (string? x) x\n                   (integer? x) (str (char x))\n                   :else (String. ^chars x))))\n        ([string-or-chars off len]\n          (when (pos? len)\n            (write (subs (if (string? string-or-chars) string-or-chars (String. ^chars string-or-chars))\n                     off (+ off len))))))))\n  ([tag write]\n    (tagging-writer (fn [s] (write [tag s]))))\n  ([tag group-id write]\n    (tagging-writer (fn [s] (write [tag s group-id])))))\n\n(defn blame-ex [phase ex]\n  (if (::phase (ex-data ex))\n    ex\n    (ex-info (str \"Exception during \" (name phase) \" phase.\")\n      {::ex ex ::phase phase} ex)))\n\n(defmacro blame [phase & body]\n  `(try ~@body\n     (catch Throwable t#\n       (throw (blame-ex ~phase t#)))))\n\n(defn atomic-write [^java.io.Writer w]\n  (fn [x]\n    (let [s (blame :print (p/edn-str x))] ; was pr-str, must occur outside of the locking form to avoid deadlocks\n      (locking w\n        (.write w s)\n        (.write w \"\\n\")\n        (.flush w)))))\n\n(defn fuse-write [awrite]\n  (fn [x]\n    (when-some [w @awrite]\n      (try\n        (w x)\n        (catch Throwable t\n          (reset! awrite nil))))))\n\n(def ^:dynamic write)\n\n(defn unrepl-reader [^java.io.Reader r before-read]\n  (let [offset (atom 0)\n        offset! #(swap! offset + %)]\n    (proxy [clojure.lang.LineNumberingPushbackReader clojure.lang.ILookup] [r]\n      (valAt\n        ([k] (get this k nil))\n        ([k not-found] (case k :offset @offset not-found)))\n      (read\n        ([]\n          (before-read)\n          (let [c (proxy-super read)]\n            (when-not (neg? c) (offset! 1))\n            c))\n        ([cbuf]\n          (before-read)\n          (let [n (proxy-super read cbuf)]\n            (when (pos? n) (offset! n))\n            n))\n        ([cbuf off len]\n          (before-read)\n          (let [n (proxy-super read cbuf off len)]\n            (when (pos? n) (offset! n))\n            n)))\n      (unread\n        ([c-or-cbuf]\n          (if (integer? c-or-cbuf)\n            (when-not (neg? c-or-cbuf) (offset! -1))\n            (offset! (- (alength c-or-cbuf))))\n          (proxy-super unread c-or-cbuf))\n        ([cbuf off len]\n          (offset! (- len))\n          (proxy-super unread cbuf off len)))\n      (skip [n]\n        (let [n (proxy-super skip n)]\n          (offset! n)\n          n))\n      (readLine []\n        (when-some [s (proxy-super readLine)]\n          (offset! (count s))\n          s)))))\n\n(defn- close-socket! [x]\n  ; hacky way because the socket is not exposed by clojure.core.server\n  (loop [x x]\n    (if (= \"java.net.SocketInputStream\" (.getName (class x)))\n      (do (.close x) true)\n      (when-some [^java.lang.reflect.Field field \n                  (->> x class (iterate #(.getSuperclass %)) (take-while identity)\n                    (mapcat #(.getDeclaredFields %))\n                    (some #(when (#{\"in\" \"sd\"} (.getName ^java.lang.reflect.Field %)) %)))]\n        (recur (.get (doto field (.setAccessible true)) x))))))\n\n(defn weak-store [make-action not-found]\n  (let [ids-to-weakrefs (atom {})\n        weakrefs-to-ids (atom {})\n        refq (java.lang.ref.ReferenceQueue.)\n        NULL (Object.)]\n    (.start (Thread. (fn []\n                       (let [wref (.remove refq)]\n                         (let [id (@weakrefs-to-ids wref)]\n                           (swap! weakrefs-to-ids dissoc wref)\n                           (swap! ids-to-weakrefs dissoc id)))\n                           (recur))))\n    {:put (fn [x]\n            (let [x (if (nil? x) NULL x)\n                  id (keyword (gensym))\n                  wref (java.lang.ref.WeakReference. x refq)]\n              (swap! weakrefs-to-ids assoc wref id)\n              (swap! ids-to-weakrefs assoc id wref)\n              {:get (make-action id)}))\n     :get (fn [id]\n            (if-some [x (some-> @ids-to-weakrefs ^java.lang.ref.WeakReference (get id) .get)]\n              (if (= NULL x) nil x)\n              not-found))}))\n\n(defn- base64-encode [^java.io.InputStream in]\n  (let [table \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/\"\n        sb (StringBuilder.)]\n    (loop [shift 4 buf 0]\n      (let [got (.read in)]\n        (if (neg? got)\n          (do\n            (when-not (= shift 4)\n              (let [n (bit-and (bit-shift-right buf 6) 63)]\n                (.append sb (.charAt table n))))\n            (cond\n              (= shift 2) (.append sb \"==\")\n              (= shift 0) (.append sb \\=))\n            (str sb))\n          (let [buf (bit-or buf (bit-shift-left got shift))\n                n (bit-and (bit-shift-right buf 6) 63)]\n            (.append sb (.charAt table n))\n            (let [shift (- shift 2)]\n              (if (neg? shift)\n                (do\n                  (.append sb (.charAt table (bit-and buf 63)))\n                  (recur 4 0))\n                (recur shift (bit-shift-left buf 6))))))))))\n\n(defn- base64-decode [^String s]\n  (let [table \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/\"\n        in (java.io.StringReader. s)\n        bos (java.io.ByteArrayOutputStream.)]\n    (loop [bits 0 buf 0]\n      (let [got (.read in)]\n        (when-not (or (neg? got) (= 61 #_\\= got))\n          (let [buf (bit-or (.indexOf table got) (bit-shift-left buf 6))\n                bits (+ bits 6)]\n            (if (<= 8 bits)\n              (let [bits (- bits 8)]\n                (.write bos (bit-shift-right buf bits))\n                (recur bits (bit-and 63 buf)))\n              (recur bits buf))))))\n    (.toByteArray bos)))\n\n(defonce ^:private sessions (atom {}))\n\n(def ^:private unreachable (tagged-literal 'unrepl/... nil))\n(defonce ^:private elision-store (weak-store #(list `fetch %) unreachable))\n(defn fetch [id] \n  (let [x ((:get elision-store) id)]\n    (cond\n      (= unreachable x) x\n      (instance? unrepl.print.ElidedKVs x) x\n      (string? x) x\n      :else (seq x))))\n\n(defonce ^:private attachment-store (weak-store #(list `download %) (constantly nil)))\n(defn download [id] ((:get attachment-store) id))\n\n(defn session [id]\n  (some-> @sessions (get id) deref))\n\n(defn interrupt! [session-id eval]\n  (let [{:keys [^Thread thread eval-id promise]}\n        (some-> session-id session :current-eval)]\n    (when (and (= eval eval-id)\n            (deliver promise\n              {:ex (doto (ex-info \"Evaluation interrupted\" {::phase :eval})\n                     (.setStackTrace (.getStackTrace thread)))\n               :bindings {}}))\n      (.stop thread)\n      true)))\n\n(defn background! [session-id eval]\n  (let [{:keys [eval-id promise future]}\n        (some-> session-id session :current-eval)]\n    (boolean\n      (and\n        (= eval eval-id)\n        (deliver promise\n          {:eval future\n           :bindings {}})))))\n\n(defn exit! [session-id] ; too violent\n  (some-> session-id session :in close-socket!))\n\n(defn reattach-outs! [session-id]\n  (some-> session-id session :write-atom \n    (reset!\n      (if (bound? #'write)\n        write\n        (let [out *out*]\n          (fn [x]\n            (binding [*out* out\n                      *print-readably* true]\n              (prn x))))))))\n\n(defn attach-sideloader! [session-id]\n  (some-> session-id session :side-loader \n    (reset!\n      (let [out *out*\n            in *in*]\n        (fn self [k name]\n          (binding [*out* out]\n            (locking self\n              (prn [k name])\n              (some-> (edn/read {:eof nil} in) base64-decode)))))))\n  (let [o (Object.)] (locking o (.wait o))))\n\n(defn set-file-line-col [session-id file line col]\n  (when-some [^java.lang.reflect.Field field \n              (->> clojure.lang.LineNumberingPushbackReader\n                .getDeclaredFields\n                (some #(when (= \"_columnNumber\" (.getName ^java.lang.reflect.Field %)) %)))]\n    (doto field (.setAccessible true)) ; sigh\n    (when-some [in (some-> session-id session :in)]\n      (set! *file* file)\n      (set! *source-path* file)\n      (.setLineNumber in line)\n      (.set field in (int col)))))\n\n(defn- writers-flushing-repo [max-latency-ms]\n  (let [writers (java.util.WeakHashMap.)\n        flush-them-all #(locking writers\n                          (doseq [^java.io.Writer w (.keySet writers)]\n                            (.flush w)))]\n    (.scheduleAtFixedRate\n      (java.util.concurrent.Executors/newScheduledThreadPool 1)\n      flush-them-all\n      max-latency-ms max-latency-ms java.util.concurrent.TimeUnit/MILLISECONDS)\n    (fn [w]\n      (locking writers (.put writers w nil)))))\n\n(defn start []\n  (with-local-vars [in-eval false\n                    unrepl false\n                    eval-id 0\n                    prompt-vars #{#'*ns* #'*warn-on-reflection*}\n                    current-eval-future nil]\n    (let [session-id (keyword (gensym \"session\"))\n          raw-out *out*\n          aw (atom (atomic-write raw-out))\n          write-here (fuse-write aw)\n          schedule-writer-flush! (writers-flushing-repo 50) ; 20 fps (flushes per second)\n          scheduled-writer (fn [& args]\n                             (-> (apply tagging-writer args)\n                               java.io.BufferedWriter.\n                               (doto schedule-writer-flush!)))\n          edn-out (scheduled-writer :out write-here)\n          ensure-raw-repl (fn []\n                            (when (and @in-eval @unrepl) ; reading from eval!\n                              (var-set unrepl false)\n                              (write [:bye {:reason :upgrade :actions {}}])\n                              (flush)\n                              ; (reset! aw (blocking-write))\n                              (set! *out* raw-out)))\n          in (unrepl-reader *in* ensure-raw-repl)\n          session-state (atom {:current-eval {}\n                               :in in\n                               :write-atom aw\n                               :log-eval (fn [msg]\n                                           (when (bound? eval-id)\n                                             (write [:log msg @eval-id])))\n                               :log-all (fn [msg]\n                                          (write [:log msg nil]))\n                               :side-loader (atom nil)\n                               :prompt-vars #{#'*ns* #'*warn-on-reflection*}})\n          current-eval-thread+promise (atom nil)\n          ensure-unrepl (fn []\n                          (when-not @unrepl\n                            (var-set unrepl true)\n                            (flush)\n                            (set! *out* edn-out)\n                            (binding [*print-length* Long/MAX_VALUE\n                                      *print-level* Long/MAX_VALUE]\n                              (write [:unrepl/hello {:session session-id\n                                                     :actions {:exit `(exit! ~session-id)\n                                                               :start-aux `(start-aux ~session-id)\n                                                               :log-eval\n                                                               `(some-> ~session-id session :log-eval)\n                                                               :log-all\n                                                               `(some-> ~session-id session :log-all)\n                                                               :set-source\n                                                               `(unrepl/do\n                                                                  (set-file-line-col ~session-id\n                                                                   ~(tagged-literal 'unrepl/param :unrepl/sourcename)\n                                                                   ~(tagged-literal 'unrepl/param :unrepl/line)\n                                                                   ~(tagged-literal 'unrepl/param :unrepl/column)))\n                                                               :unrepl.jvm/start-side-loader\n                                                               `(attach-sideloader! ~session-id)}}]))))\n          \n          interruptible-eval\n          (fn [form]\n            (try\n              (let [original-bindings (get-thread-bindings)\n                    p (promise)\n                    f\n                    (future\n                      (swap! session-state update :current-eval\n                        assoc :thread (Thread/currentThread))\n                      (with-bindings original-bindings\n                        (try\n                          (write [:started-eval\n                                  {:actions \n                                   {:interrupt (list `interrupt! session-id @eval-id)\n                                    :background (list `background! session-id @eval-id)}}\n                                  @eval-id])\n                          (let [v (with-bindings {in-eval true}\n                                    (blame :eval (eval form)))]\n                            (deliver p {:eval v :bindings (get-thread-bindings)})\n                            v)\n                          (catch Throwable t\n                            (deliver p {:ex t :bindings (get-thread-bindings)})\n                            (throw t)))))]\n                (swap! session-state update :current-eval\n                  into {:eval-id @eval-id :promise p :future f})\n                (let [{:keys [ex eval bindings]} @p]\n                  (doseq [[var val] bindings\n                          :when (not (identical? val (original-bindings var)))]\n                    (var-set var val))\n                  (if ex\n                    (throw ex)\n                    eval)))\n              (finally\n                (swap! session-state assoc :current-eval {}))))\n          cl (.getContextClassLoader (Thread/currentThread))\n          slcl (classloader cl\n                 (fn [k x]\n                   (when-some [f (some-> session-state deref :side-loader deref)]\n                     (f k x))))]\n      (swap! session-state assoc :class-loader slcl)\n      (swap! sessions assoc session-id session-state)\n      (binding [*out* raw-out\n                *err* (tagging-writer :err write)\n                *in* in\n                *file* \"unrepl-session\"\n                *source-path* \"unrepl-session\"\n                p/*elide* (:put elision-store)\n                p/*attach* (:put attachment-store)\n                write write-here]\n        (.setContextClassLoader (Thread/currentThread) slcl)\n        (with-bindings {clojure.lang.Compiler/LOADER slcl}\n          (try\n            (m/repl\n              :prompt (fn []\n                        (ensure-unrepl)\n                        (write [:prompt (into {:file *file*\n                                               :line (.getLineNumber *in*)\n                                               :column (.getColumnNumber *in*)\n                                               :offset (:offset *in*)}\n                                          (map (fn [v]\n                                                 (let [m (meta v)]\n                                                   [(symbol (name (ns-name (:ns m))) (name (:name m))) @v])))\n                                          (:prompt-vars @session-state))]))\n             :read (fn [request-prompt request-exit]\n                     (blame :read (let [line+col [(.getLineNumber *in*) (.getColumnNumber *in*)]\n                                        offset (:offset *in*)\n                                        r (m/repl-read request-prompt request-exit)\n                                        line+col' [(.getLineNumber *in*) (.getColumnNumber *in*)]\n                                        offset' (:offset *in*)\n                                        len (- offset' offset)\n                                        id (when-not (#{request-prompt request-exit} r)\n                                             (var-set eval-id (inc @eval-id)))]\n                                    (write [:read {:from line+col :to line+col'\n                                                   :offset offset\n                                                   :len (- offset' offset)}\n                                            id])\n                                    (if (and (seq?  r) (= (first r) 'unrepl/do))\n                                      (let [id @eval-id]\n                                        (binding [*err* (tagging-writer :err id write)\n                                                  *out* (scheduled-writer :out id write)]\n                                          (eval (cons 'do (next r))))\n                                        request-prompt)\n                                      r))))\n             :eval (fn [form]\n                     (let [id @eval-id]\n                       (binding [*err* (tagging-writer :err id write)\n                                 *out* (scheduled-writer :out id write)]\n                         (interruptible-eval form))))\n             :print (fn [x]\n                      (ensure-unrepl)\n                      (write [:eval x @eval-id]))\n             :caught (fn [e]\n                       (ensure-unrepl)\n                       (let [{:keys [::ex ::phase]\n                              :or {ex e phase :repl}} (ex-data e)]\n                         (write [:exception {:ex e :phase phase} @eval-id]))))\n           (finally\n             (.setContextClassLoader (Thread/currentThread) cl))))\n        (write [:bye {:reason :disconnection\n                      :outs :muted\n                      :actions {:reattach-outs `(reattach-outs! ~session-id)}}])))))\n\n(defn start-aux [session-id]\n  (let [cl (.getContextClassLoader (Thread/currentThread))]\n    (try\n        (some->> session-id session :class-loader (.setContextClassLoader (Thread/currentThread)))\n        (start)\n        (finally\n          (.setContextClassLoader (Thread/currentThread) cl)))))\n\n(unrepl.repl/start)" "unrepl\\.(?:repl|print)" (clojure.core/str "$0" prefix__641__auto__)) rdr__643__auto__ (clojure.core/-> code__642__auto__ java.io.StringReader. clojure.lang.LineNumberingPushbackReader.)] (try (clojure.core/binding [clojure.core/*ns* clojure.core/*ns*] (clojure.core/loop [ret__644__auto__ nil] (clojure.core/let [form__645__auto__ (clojure.core/read rdr__643__auto__ false (quote eof__646__auto__))] (if (clojure.core/= (quote eof__646__auto__) form__645__auto__) ret__644__auto__ (recur (clojure.core/eval form__645__auto__)))))) (catch java.lang.Throwable t__647__auto__ (clojure.core/println "[:unrepl.upgrade/failed]") (throw t__647__auto__))))
