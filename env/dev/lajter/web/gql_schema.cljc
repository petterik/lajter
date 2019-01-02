(ns lajter.web.gql-schema
  #?(:cljs
     (:require-macros [lajter.web.gql-schema :refer [inline-gql-schema inline-github-token]]))
  #?(:clj
     (:require
       [clojure.java.io :as jio]
       [clojure.data.json :as json])))

#?(:clj
   (defmacro inline-gql-schema []
     (slurp (jio/resource "test/graphql/github-schema.json"))))

#?(:clj
   (defmacro inline-github-token []
     (System/getenv "GITHUB_GRAPHQL_TOKEN")))

#?(:cljs
   (def gql-schema (inline-gql-schema)))

#?(:cljs
   (def github-token (inline-github-token)))
