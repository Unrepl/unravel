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
