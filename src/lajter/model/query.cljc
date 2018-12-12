(ns lajter.model.query)


'[RootQuery
  [^ID id
   ^Model requires [^Model normal
                    ^Model params
                    ^Model env-selector]
   ^Model provides
   call ^Entity [^Lajter.Env env]]

  ViewQuery
  [^RootQuery root-query
   ^Model pull-pattern]]

(comment
  ;; Figure out a way to define root queries
  ;; Then execute a root query against an env (db).

  ;; Resolving a root query against an app-state
  ;; returns entities. Pull is a separate step.
  ;; Let's implement pull in this namespace as well.

  ;; Pull is something that might be needed in
  ;; other situations as well.

  ;; There's nothing else to do here until we've
  ;; got some actual data to define this in?
  )

(defn ->root-query [{:keys [id requires provides call]}])

