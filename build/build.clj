(ns build
  (:require
   [clojure.tools.build.api :as b]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [meander.epsilon :as m]
   [deps-deploy.deps-deploy :as d]
   [badigeon.javac :as javac]))

(def scm-url "git@github.com:ribelo/fatum.git")

(defn javac [_]
  (println "Compiling Java")
  (println (shell/sh "java" "--version"))
  (javac/javac "src/main" {:compile-path     "target/classes"
                           ;; Additional options used by the javac command
                           :compiler-options ["-cp" "clojure-1.10.3.jar:src:target/classes" "-target" "1.8"
                                              "-source" "1.8" "-Xlint:-options" "--release 8"]})
  (println "Compilation Completed"))

(defn sha
  [{:keys [dir path] :or {dir "."}}]
  (-> {:command-args (cond-> ["git" "rev-parse" "HEAD"]
                       path (conj "--" path))
       :dir (.getPath (b/resolve-path dir))
       :out :capture}
      b/process
      :out
      str/trim))

(defn git-branch-name
  "Attempts to get the current branch name via the shell."
  []
  (m/match (shell/sh "git" "rev-parse" "--abbrev-ref" "HEAD")
    {:exit 0, :out ?out}
    (str/trim ?out)

    ?result
    (throw (ex-info "Unable to compute branch name" ?result))))

(def git-commit-count-start
  "Starting SHA to count commits from."
  "c62c351acd48535c4785628db473709757b41480")

(defn git-branch-commit-count
  "Attempts to get the current number of commits on the current branch
  via the shell."
  []
  (m/match (shell/sh "git" "rev-list" (str git-commit-count-start "...") "--count")
    {:exit 0, :out ?out}
    (str/trim ?out)

    ?result
    (throw (ex-info "Unable to compute commit count" ?result))))

(def lib (symbol "com.github.ribelo" "fatum"))
(def basis (b/create-basis {:project "deps.edn"}))
(def version (format "0.0.%s" (git-branch-commit-count)))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (git-branch-name) version))

(defn jar [_]
  (b/delete {:path "target"})
  (b/write-pom {:class-dir class-dir
                :lib       lib
                :version   version
                :basis     basis
                :src-dirs  ["src/main"]
                :scm {:tag (sha nil)
                      :connection (str "scm:git:" scm-url)
                      :developerConnection (str "scm:git:" scm-url)
                      :url scm-url}})
  (javac _)
  (b/copy-dir {:src-dirs   ["src/main"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))

(defn deploy [args]
  (-> args
      (assoc  :artifact jar-file
              :pom-file (str class-dir "/META-INF/maven/" (namespace lib) "/" (name lib) "/pom.xml"))
      (d/deploy)))

(defn clojars [args]
  (-> {:installer :remote :sign-releases? true}
      (merge args)
      (deploy)))

(defn install [args]
  (-> {:installer :local}
      (merge args)
      (deploy)))
