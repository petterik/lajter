(ns lajter.model.sulo-domain
  (:require
    [lajter.model :as model]
    [lajter.model.gen :as gen]
    [lajter.model.db :as db]
    [datascript.core :as d]))

(def model
  '[User
    [^String email
     ^User.Cart ^:component cart
     ^User.Profile ^:component profile]

    User.Profile
    [^String name
     ^Photo photo]

    User.Cart
    [^Store.Item ^:many items]

    Photo
    [^String path]

    Store
    [^UUID uuid
     ^Store.Owner ^:many ^:component owners
     ^Store.Item ^:many items
     ^Store.Section ^:many ^:component sections]

    Store.Owner
    [^User user
     ^Keyword role]

    Store.Item
    [^UUID uuid
     ^String name
     ^BigDecimal price
     ^Category category
     ^Store.Section ^:many ^:component section
     ^Photo photo
     ^Store.Item.Sku ^:many ^:component skus]

    Store.Section
    [^String label
     ^String path]

    Store.Item.Sku
    [^String variation
     ^Store.Item.Sku.Inventory ^:component inventory]

    Store.Item.Sku.Inventory
    [^Store.Item.Sku.Type type
     ^:enum value [IN_STOCK
                   OUT_OF_STOCK
                   LIMITED]]

    ^:enum Store.Item.Sku.Type
    [FINITE INFINITE]

    Category
    [^ID path
     ^String name
     ^String label
     ^Category ^:many children]])

(comment
  #_(def model (gql/schema->model gql-schema))
  ;; ^ Will hopefully do most of the work in actual apps,
  ;;   translating gql schema to our model format.

  (def sulo-meta-db (model/index-model (model/init-meta-db) model))
  (def data (gen/gen sulo-meta-db '[User Store]))
  (def schema (db/datascript-schema sulo-meta-db))
  (def app-state (-> (d/create-conn schema)
                     (d/db)
                     (db/merge sulo-meta-db data)))
  (def queries (gen/queries sulo-meta-db model))
  ;; ^ May have to depend on query to get all
  ;; the registered queries. OR, does it create its
  ;; own queries? (It'd have to have a way to do that
  ;; though, most likely in the query ns).
  (def view-data (query/pull app-state
                             sulo-meta-db
                             queries))

  ;; Sends:
  (def remote-query (query/send sulo-meta-db queries))
  (def response (gen/gen sulo-meta-db remote-query))
  (def app-state (db/merge app-state sulo-meta-db response))
  (def view-data-2 (query/pull app-state
                               sulo-meta-db
                               queries))

  ;; ^ Some of these things require "env" and it should
  ;; have it.

  ;; The "normal model" can be added at any time to the
  ;; sulo-meta-db.

  (render (clojure.data/diff view-data view-data-2))
  ;; ^ React
  )
