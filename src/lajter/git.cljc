(ns lajter.git)

(defn blob [[file data]]
  {:git/type      :git.type/blob
   :git.blob/file file
   :git.blob/data data})

(defn -commit [sha parent blobs]
  {:git/type          :git.type/commit
   :git.commit/sha    sha
   :git.commit/blobs  blobs
   :git.commit/parent parent})

(defn reference [name pointer]
  {:git/type        :git.type/reference
   :git.ref/name    name
   :git.ref/pointer pointer})

(defprotocol IGitClient
  (checkout [this sha])
  (add [this file data])
  (commit [this])
  (branch [this branch-name])
  (delete-branch [this branch-name])
  (squash-history-after [this after-sha])
  (cherry-pick [this sha])
  (-log [this latest earliest]))

(defn resolve-ref [git r]
  (if (map? r)
    r
    (get-in git [:refs r])))

(defn resolve-commit [git c]
  (if (map? c)
    c
    (get-in git [:commits c])))

(defn resolve-sha [git x]
  (when (some? x)
    (cond
      (not (map? x))
      (recur git (or (resolve-commit git x)
                     (resolve-ref git x)))

      (= (:git/type x) :git.type/commit)
      (:git.commit/sha x)

      (= (:git/type x) :git.type/reference)
      (when-some [p (:git.ref/pointer x)]
        ;; pointer can point to nil.
        (recur git p)))))

(defn head-ref [git]
  (resolve-ref git (:head git)))

(defn head-sha [git]
  (resolve-sha git (head-ref git)))

(defn next-id [id-gen]
  (swap! id-gen inc))

(defrecord HeadlessGit [git]
  IGitClient
  (checkout [_ sha]
    ;; goes back to a possible non-headless git.
    (checkout git sha))
  (-log [_ latest earliest]
    ;; Non modifying action.
    (-log git latest earliest))

  ;; Updates the headless git for branch operations.
  (branch [_ branch-name]
    (branch git branch-name))
  (delete-branch [this branch-name]
    (update this :git delete-branch branch-name))

  ;; Cannot modify the current ref
  (squash-history-after [this sha]
    (throw (ex-info "Cannot call \"squash-history-after\" when in headless git."
                    {:sha sha})))
  (add [this file data]
    (throw (ex-info "Cannot call \"add\" when in headless git."
                    {:file file :data data})))
  (commit [this]
    (throw (ex-info "Cannot call \"commit\" when in headless git." {})))
  (cherry-pick [this sha]
    (throw (ex-info "Cannot call \"cherry-pick\" when in headless git." {:sha sha})))
  )

(defn commit-seq [git sha]
  (when-some [c (resolve-commit git sha)]
    (lazy-seq
      (cons c (commit-seq git (:git.commit/parent c))))))

(defrecord GitClient [id-gen]
  IGitClient
  (checkout [this ref-or-sha]
    (if-let [r (resolve-ref this ref-or-sha)]
      (assoc this :head (:git.ref/name r))
      (if-let [c (resolve-commit this ref-or-sha)]
        (->HeadlessGit
          (assoc this :head (reference "headless" (:git.commit/sha c))))
        (throw (ex-info (str "Unable to checkout " ref-or-sha)
                        {:ref-or-sha ref-or-sha
                         :refs       (keys (:refs this))})))))
  (add [this file data]
    (update this :index assoc file data))
  (commit [this]
    (if-let [index (not-empty (:index this))]
      (let [blobs (into {}
                        (comp (map blob)
                              (map (juxt :git.blob/file identity)))
                        index)
            commit (-commit (:next-id this) (head-sha this) blobs)]
        (-> this
            (update :next-id inc)
            (assoc :index nil)
            (update :commits assoc (:git.commit/sha commit) commit)
            (assoc-in [:refs (:git.ref/name (head-ref this)) :git.ref/pointer]
                      (:git.commit/sha commit))))
      this))

  (cherry-pick [this sha]
    (if-some [{:git.commit/keys [blobs]} (resolve-commit this sha)]
      (->> (vals blobs)
           (reduce (fn [git {:git.blob/keys [file data]}]
                     (add git file data))
                   this)
           (commit))
      (throw (ex-info (str "Unable to cherry-pick " sha
                           ". Commit could not be resolved.")
                      {:sha sha
                       :head (head-ref this)}))))

  (-log [this latest-sha earliest-sha]
    (let [latest-sha (or latest-sha (head-sha this))
          sha-before-earliest
          (->> (commit-seq this earliest-sha)
                           (drop 1)
                           (first)
                           (:git.commit/sha))]
      (->> (commit-seq this latest-sha)
           (take-while (comp #(not= % sha-before-earliest)
                             :git.commit/sha)))))

  (squash-history-after [this after-sha]
    (let [[to-keep & to-squash :as commits] (commit-seq this after-sha)
          shas (into #{} (map :git.commit/sha) to-squash)
          blobs (reduce merge {} (map :git.commit/blobs (reverse commits)))]
      (-> this
          (update :commits #(reduce dissoc % shas))
          (update-in [:commits (:git.commit/sha to-keep)]
                     assoc
                     :git.commit/blobs blobs
                     :git.commit/parent nil)
          (update :refs (fn [refs]
                          (into {}
                                (remove (comp shas :git.ref/pointer val))
                                refs)))
          (as-> this
                (cond-> this
                        (contains? (:refs this)
                                   (:git.ref/name (head-ref this)))
                        (assoc :head "master"))))))

  ;; branches and refs
  (branch [this branch-name]
    (cond-> (assoc this :head branch-name)
            (not (resolve-ref this branch-name))
            (update :refs assoc
                    branch-name
                    (reference branch-name (head-sha this)))))
  (delete-branch [this branch-name]
    (if (= "master" branch-name)
      this
      (cond-> (update this :refs dissoc branch-name)
              ;; If we're on the branch, change to master.
              (= branch-name (:git.ref/name (head-ref this)))
              (assoc :head "master")))))

(defn log [git & [latest earliest]]
  (-log git (or latest (head-sha git)) earliest))

(defn git-client []
  (-> (map->GitClient {:next-id 0
                       :refs {"master" (reference "master" nil)}
                       :head "master"})))

(comment
  (def g (git-client))
  (delete-branch (branch g "foo") "foo")
  (= g (delete-branch (branch g "foo") "foo"))

  (-> g
      (add :foo "bar")
      (commit)
      (add :foo "xyz")
      (add :bar "max")
      (commit)
      (add :foo "rawr")
      (commit))

  (def g2 *1)

  (= (log g2) (log g2 2))

  (commit (checkout g2 1))

  (= 1
     (-> g2
       (checkout 1)
       (branch "nr1")
       (head-sha)))

  (-> g2
      (squash-history-after 1)
      (log))

  (-> g2
      (checkout 1)

      (branch "nr1")
      (add :nr1 :data)
      (commit)
      (branch "master")
      (cherry-pick 3)
      (cherry-pick 3)
      (log))

  )