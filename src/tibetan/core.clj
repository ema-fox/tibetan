(ns tibetan.core
  (:require [clojure.string :as str]
            [duratom.core :refer [duratom]]
            [ring.adapter.undertow :refer [run-undertow]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.util.response :refer [response set-cookie redirect]]
            [reitit.ring :refer [ring-handler router routes create-resource-handler create-default-handler]]
            [hiccup.page :as p]))

(def notes
  (->>
   [{:uchen "ཀ", :wojk "ka"} {:uchen "ཁ", :wojk "kha"} {:uchen "ག", :wojk "ga"} {:uchen "ང", :wojk "nga"}
    {:uchen "ཅ", :wojk "ca"} {:uchen "ཆ", :wojk "cha"} {:uchen "ཇ", :wojk "ja"} {:uchen "ཉ", :wojk "nya"}
    {:uchen "ཏ", :wojk "ta"} {:uchen "ཐ", :wojk "tha"} {:uchen "ད", :wojk "da"} {:uchen "ན", :wojk "na"}
    {:uchen "པ", :wojk "pa"} {:uchen "ཕ", :wojk "pha"} {:uchen "བ", :wojk "ba"} {:uchen "མ", :wojk "ma"}
    {:uchen "ཙ", :wojk "tsa"} {:uchen "ཚ", :wojk "tsha"} {:uchen "ཛ", :wojk "dza"} {:uchen "ཝ", :wojk "wa"}
    {:uchen "ཞ", :wojk "zha"} {:uchen "ཟ", :wojk "za"} {:uchen "འ", :wojk "'a"} {:uchen "ཡ", :wojk "ya"}
    {:uchen "ར", :wojk "ra"} {:uchen "ལ", :wojk "la"} {:uchen "ཤ", :wojk "sha"} {:uchen "ས", :wojk "sa"}
    {:uchen "ཧ", :wojk "ha"} {:uchen "ཨ", :wojk "a"}
    {:uchen "ཨི", :wojk "i"} {:uchen "ཨུ", :wojk "u"} {:uchen "ཨེ", :wojk "e"} {:uchen "ཨོ", :wojk "o"}
    {:uchen "མཆོད་རྟེན་" :wojk "mChod rTen" :english "symbolic monument for offering and reliquaries"}
    {:uchen "དྲིལ་བུ་" :wojk "dril bu" :english "bell"}
    {:uchen "བདེ་ཆེན་" :wojk "bDe chen" :english "great bliss"}
    {:uchen "མཁའ་འགྲོ་མ་" :wojk "mKha' 'gro ma" :english "female sky dancer"}
    {:uchen "མཁའ་འགྲོ་པ་" :wojk "mKha' 'gro pa" :english "male sky dancer"}
    {:uchen "བླ་མ་" :wojk "bLa ma" :english "teacher, Vajra Master"}
    {:uchen "མ་མོ་" :wojk "ma mo" :english "female protector"}
    {:uchen "མེ་ལོང་" :wojk "me long" :english "mirror"}
    {:uchen "དཔའ་བོ་" :wojk "dPa' bo" :english "hero"}
    {:uchen "དཔའ་མོ་" :wojk "dPa' mo" :english "heroine"}]
   (mapv #(assoc % :wylie (str/lower-case (:wojk %))))))

(def note-index
  (into {} (map (juxt :wylie identity)) notes))

(defonce !answers
  (duratom :local-file
           :file-path "answers.edn"
           :init []))

(defonce !settings
  (duratom :local-file
           :file-path "settings.edn"
           :init {}))

(defn note->cards [n]
  (concat [[(:wylie n) :uchen :wojk]]
          (when (:english n)
            [[(:wylie n) :wojk :english]])))

(def cards (mapcat note->cards notes))

(defn correct? [{:keys [card-id free-answer wylie-answer] :as x}]
  (= (first card-id) (or wylie-answer (str/lower-case free-answer))))

(defn ratio [x]
  (let [tr (get x true 0)
        fl (get x false 0)]
    (/ tr (+ tr fl))))

(defn safe-ratio [x]
  (let [tr (inc (get x true 0))
        fl (inc (get x false 0))]
    (/ tr (+ tr fl))))

(defn answers-by [user-id]
  (filter (comp #{user-id} :by) @!answers))

(defn take-it-easy? [user-id]
  (let [{xs true ys false} (group-by (comp boolean :wylie-answer)
                                     (answers-by user-id))]
    (not (< 5 (+ (rand-int 3)
                 (count (filter correct? xs))
                 (- (* 2 (count (remove correct? ys)))))))))

(defn foo-answers-by [user-id]
  (cond->> (answers-by user-id)
    (get-in @!settings [user-id :retry?])
    (filter correct?)))

(defn get-open-questions [user-id]
  (let [known (into #{}
                    (map :card-id)
                    (foo-answers-by user-id))]
    (-> (into []
              (comp (remove known)
                    (map #(do {:card-id %})))
              cards)
        not-empty)))

(defn add-choices [q]
  (->> notes
       (filter (nth (:card-id q) 2))
       (sort-by #(abs (- (count (first (:card-id q)))
                         (count (:wylie %)))))
       (take 10)
       shuffle
       (cons (note-index (first (:card-id q))))
       distinct
       (take 6)
       shuffle
       (assoc q :choices )))

(defn prepare-question [q user-id]
  (cond-> q
    (or (= :english (nth (:card-id q) 2))
        (take-it-easy? user-id))
    add-choices))

(defn get-question [user-id]
  (some-> (get-open-questions user-id)
          rand-nth
          (prepare-question user-id)))

(defn calc-stats [answers]
  (-> answers
      (->> (group-by :card-id))
      (update-vals (fn [as] (frequencies (map correct? as))))))

(defn stats [user-id]
  (let [user-stats (calc-stats (answers-by user-id))]
    [:div
     [:h2 "Statistics"]
     [:table
      [:tbody
       [:tr
        [:th]
        [:th "Everyone (" (count (distinct (map :by @!answers))) " so far)"]
        [:th "You"]]
       (for [[[wylie front back :as card-id] x]
             (-> (calc-stats @!answers)
                 (select-keys (keys user-stats))
                 (->> (sort-by (fn [[_ x]] (ratio #_ safe-ratio x)) >)))]
         [:tr
          [:td (front (note-index wylie))]
          [:td
           [:progress {:value (float (ratio x))}]]
          [:td
           (if-let [y (user-stats card-id)]
             [:progress {:value (float (ratio y))}])]])]]]))

(defn question [user-id]
  (if (< (count (answers-by user-id))
         (get-in @!settings [user-id :limit] 20))
    (if-let [{[wylie front back] :card-id :keys [choices]} (get-question user-id)]
      [:form
       {:action "/answer" :method "post"
        :style {:background "lightgray"
                :padding "1em"
                :margin-top "1em"}}
       [:div {:style {:text-align "center"
                      :font-size "3em"}}
        (front (note-index wylie))]
       [:input {:type "hidden" :name "wylie" :value wylie}]
       [:input {:type "hidden" :name "front" :value front}]
       [:input {:type "hidden" :name "back" :value back}]
       (if choices
         [:div
          "Choose the correct "
          (case back
            :wojk "transcription"
            :english "meaning")
          [:div {:style {:display "grid"
                         :grid-template-columns "auto auto"
                         :gap "1ch"}}
           (for [c choices]
             [:button {:name "choice" :value (:wylie c)
                       :style {:padding "1em"

                               :font-size "1.5em"}}
              (back c)])]]
         [:div
          [:div {:style {:display "flex"
                         :gap "1ch"
                         :align-items "center"}}
           "Enter transcription*: "
           [:input {:name "answer" :autofocus true
                    :style {:font-size "1.5em"}}]
           [:button {:style {:font-size "1.5em"}}
            "submit"]]
          [:div {:style {:font-size "0.8em"
                         :padding-top "0.5em"}}
           "*either Wylie or Wojkowitz"]])]
      [:div
       [:h1 "You answered all the questions!"]
       (when-not (get-in @!settings [user-id :retry?])
         [:form {:action "/enable-retry" :method "post"}
          [:button {:style {:font-size "1.5em"
                            :padding "1ch"}}
           "Retry previous mistakes"]])
       [:br]
       (stats user-id)])
    [:div
     [:h1 "Thank you for your answers!"]
     [:form {:action "/answer-more" :method "post"}
      [:button {:style {:font-size "1.5em"
                        :padding "1ch"}}
       "answer more"]]
     [:br]
     (stats user-id)]))

(defn show-answer-entry [{:as answer-entry [wylie front back] :card-id :keys [wylie-answer free-answer]}]
  (let [note (note-index wylie)]
    [:div {:style {:display "flex"
                   :gap "1ch"
                   :align-items "center"}}
     [:div {:style {:text-align "center"
                    :font-size "2em"}}
      (front note)]
     [:div
      [:div
       "correct: " [:span {:style "background-color: lightgreen"} (back note)]]
      (if (correct? answer-entry)
        [:div "\u00a0"]
        [:div
         "not correct: "
         [:span {:style "background-color: pink"}
          (or (back (note-index wylie-answer))
              free-answer)]])]]))

(defn page [user-id & body]
  (response
   (p/html5 {:encoding "UTF-8" :xml? true}
            [:head
             [:title "Tibetan quiz"]]
            [:body
             (let [canswered (count (answers-by user-id))
                   limit (min (+ canswered (count (get-open-questions user-id)))
                              (get-in @!settings [user-id :limit] 20))]
               [:div {:style {:display "flex"
                              :gap "1ch"
                              :padding "0 1ch"}}
                [:progress {:style {:width "100%"}
                            :max limit
                            :value canswered}]
                (str canswered "/" limit)])
             #_
             (str user-id)
             [:div {:style {:max-width "80ch"
                            :font-size "1.5em"
                            :margin "auto"}}
              body]])))

(defn wrap-user-id [handler]
  (fn [req]
    (if-let [user-id (some-> (:value (get (:cookies req) "user-id"))
                             parse-uuid)]
      (handler (assoc req :user-id user-id))
      (let [user-id (random-uuid)]
        (-> (handler (assoc req :user-id user-id))
            (set-cookie "user-id" (str user-id)
                        {:max-age (* 60 60 24 365)}))))))

#_
(def dup-ids (repeatedly 100 random-uuid))

(def handler
  (-> (ring-handler
       (router
        ["/"
         [""
          {:get (fn [{:keys [user-id]}]
                  (page user-id
                        (when-let [a (last (answers-by user-id))]
                          [:div
                           [:span {:style {:font-size "0.8em"}} "Your last response:"]
                           (show-answer-entry a)])
                        (question user-id)))}]
         ["answer"
          {:post (fn [{:as req :keys [user-id]}]
                   (let [{:keys [answer wylie choice front back]} (:params req)
                         answer-entry {:by user-id
                                       :card-id [wylie (keyword front) (keyword back)]
                                       :wylie-answer choice
                                       :free-answer answer}]
                     (swap! !answers conj answer-entry)
                     #_(swap! !answers into (for [id dup-ids]
                                            (assoc answer-entry :by id)))
                     (redirect "/" :see-other)))}]
         ["answer-more"
          {:post (fn [{:keys [user-id]}]
                   (swap! !settings update-in [user-id :limit] (fnil + 20) 20)
                   (redirect "/" :see-other))}]
         ["enable-retry"
          {:post (fn [{:keys [user-id]}]
                   (swap! !settings assoc-in [user-id :retry?] true)
                   (redirect "/" :see-other))}]
         ["stats"
          {:get (fn [{:keys [user-id]}]
                  (page user-id (stats user-id)))}]])
       (routes
        (create-resource-handler {:path "/"})
        (create-default-handler)))
      wrap-user-id
      wrap-keyword-params
      wrap-params
      wrap-cookies))

(defn -main [& args]
  (run-undertow (wrap-reload #'handler) {:host "0.0.0.0" :port (or (some-> (first args) Long.) 8080)}))
