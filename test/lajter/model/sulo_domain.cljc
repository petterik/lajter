(ns lajter.model.sulo-domain)

(def normal-model
  '[User [^ID id]
    Photo [^{:db/unique :db.unique/identity} Path]])

(def model
  '[User
    [^String email
     ^User.Cart ^:db/isComponent cart
     ^User.Profile ^:db/isComponent profile]

    User.Profile
    [^String name
     ^Photo photo]

    User.Cart
    [^Store.Item ^:db.cardinality/many items]

    Photo
    [^String path]

    Store
    [^UUID uuid
     ^Store.Owner ^:db.cardinality/many ^:db/isComponent owners
     ^Store.Item ^:db.cardinality/many items
     ^Store.Section ^:db.cardinality/many ^:db/isComponent sections]

    Store.Owner
    [^User user
     ^Keyword role]

    Store.Item
    [^UUID uuid
     ^String name
     ^BigDecimal price
     ^Category category
     ^Store.Section ^:db.cardinality/many ^:db/isComponent section
     ^Photo photo
     ^Store.Item.Sku ^:db.cardinality/many ^:db/isComponent skus]

    Store.Section
    [^String label
     ^String path]

    Store.Item.Sku
    [^String variation
     ^Store.Item.Sku.Inventory ^:db/isComponent inventory]

    Store.Item.Sku.Inventory
    [^Keyword
    ^{:model.type.keyword/namespace
      "store.item.sku.inventory.type"
      :model.type.keyword/values #{:finite :infinite}}
    type
     ^Keyword
     ^{:model.type.keyword/namespace "store.item.sku.inventory"
       :model.type.keyword/values #{:in-stock :out-of-stock :limited}}
     value]

    ;; TODO: Add recursive gen support. Currently recurses infinitely.
    Category
    [^ID path
     ^String name
     ^String label
     ^Category2 ^:db.cardinality/many children]

    Category2
    [^ID path
     ^String name
     ^String label
     ^Category3 ^:db.cardinality/many children]

    Category3
    [^ID path
     ^String name
     ^String label]])

