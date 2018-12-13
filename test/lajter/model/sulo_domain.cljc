(ns lajter.model.sulo-domain)

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
     ^Category2 ^:many children]

    Category2
    [^ID path
     ^String name
     ^String label
     ^Category3 ^:many children]

    Category3
    [^ID path
     ^String name
     ^String label]])

