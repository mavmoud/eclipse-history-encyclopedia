(ns a3comp348.core)

(defn read-eclipse-events []                                ; This function reads eclipse events from a given file
  (with-open [rdr (clojure.java.io/reader "eclipse_events.txt")]
    (reduce (fn [events line]
              (if (re-matches #"\ADate:.*" line)
                (conj events [line])
                (update events (dec (count events)) conj line)))
            []
            (line-seq rdr))))

(defn view-eclipse-events []                                ; This function print all events in the given file along with the total number of events
  (let [events (read-eclipse-events)]
    (println)
    (println (str "Total Eclipse Events: " (count events)))
    (println)
    (doseq [event (butlast events)] ; Process all but the last event
      (doseq [detail event]
        (println detail))
      (println "--------------------------------------------------------------------------------")) ; Dotted line after each event
    (let [last-event (last events)] ; Process the last event separately
      (doseq [detail last-event]
        (println detail))
      (println)))) ; Empty line after the last event

(defn add-new-eclipse-event []                              ; This function adds a new event to the given events file
  (println)
  (println "Enter date:")
  (let [date (str "Date: " (read-line))
        _ (println "Enter location:")
        location (str "Location: " (read-line))
        _ (println "Enter type:")
        type (str "Type: " (read-line))
        _ (println "Enter significance:")
        significance (str "Significance: " (read-line))]
    (spit "eclipse_events.txt"
          (str "\n" date "\n" location "\n" type "\n" significance "\n")
          :append true)
    (println)
    (println "Event added successfully.\n")))

(defn prompt-for-update [prompt old-value]                  ;This function is used to work with the modify-eclipse-event function by
  (println (str prompt " [" old-value "]:"))
  (let [input (read-line)]
    (if (empty? input) old-value input)))

(defn modify-eclipse-event []
  (let [events (read-eclipse-events)]
    (doseq [[idx event] (map-indexed vector events)]
      (println)
      (println (str "Index: " (inc idx)))
      (doseq [detail event]
        (println detail))
      (if (not= idx (dec (count events)))
        (println "--------------------------------------------------------------------------------")))
    (println)
    (println "Enter the index of the event you want to modify:")
    (let [index (loop []
                  (let [input (read-line)
                        idx (try
                              (Integer/parseInt input)
                              (catch NumberFormatException e nil))]
                    (if (and idx (<= idx (count events)) (> idx 0))
                      idx
                      (do
                        (println "Invalid index, please enter a valid index:")
                        (recur)))))]
      (let [selected-event (nth events (dec index))]
        (let [updated-date (prompt-for-update "Enter updated date" (nth selected-event 0))
              updated-location (prompt-for-update "Enter updated location" (nth selected-event 1))
              updated-type (prompt-for-update "Enter updated type" (nth selected-event 2))
              updated-significance (prompt-for-update "Enter updated significance" (nth selected-event 3))
              updated-event (list (str "Date: " updated-date) (str "Location: " updated-location) (str "Type: " updated-type) (str "Significance: " updated-significance))]
          (spit "eclipse_events.txt"
                (apply str (interpose "\n" (map #(str (first %) "\n" (second %) "\n" (nth % 2) "\n" (nth % 3) "\n") (assoc events (dec index) updated-event))))
                :append false)
          (println)
          (println "Event modified successfully.\n"))))))

(defn search-for-eclipse-events []
  (println "Enter search type (date/location/date range):")
  (let [search-type (clojure.string/lower-case (read-line))]
    (cond
      (or (= search-type "date") (= search-type "location"))
      (do
        (println "Enter search query:")
        (let [search-query (clojure.string/lower-case (read-line))
              events (read-eclipse-events)
              matching-events (filter (fn [event]
                                        (let [event-date (clojure.string/lower-case (nth event 0))
                                              event-location (clojure.string/lower-case (nth event 1))]
                                          (or (and (= search-type "date") (clojure.string/includes? event-date search-query))
                                              (and (= search-type "location") (clojure.string/includes? event-location search-query)))))
                                      events)]
          (println "\nSearch Results:\n")
          (doseq [[idx event] (map-indexed vector matching-events)]
            (doseq [detail event]
              (println detail))
            (when (not= idx (dec (count matching-events)))
              (println "--------------------------------------------------------------------------------")))
          (println))) ; This adds an empty line after the last search result
      (= search-type "date range")
      (do
        (println "Enter start year:")
        (let [start-year (Integer/parseInt (read-line))
              _ (println "Enter end year:")
              end-year (Integer/parseInt (read-line))
              events (read-eclipse-events)
              matching-events (filter (fn [event]
                                        (let [event-year (Integer/parseInt (re-find #"\d{4}" (nth event 0)))]
                                          (and (>= event-year start-year) (<= event-year end-year))))
                                      events)]
          (println "\nSearch Results:\n")
          (doseq [[idx event] (map-indexed vector matching-events)]
            (doseq [detail event]
              (println detail))
            (when (not= idx (dec (count matching-events)))
              (println "--------------------------------------------------------------------------------")))
          (println))) ; This adds an empty line after the last search result
      :else (println "Invalid search type. Please enter 'date', 'location', or 'date range'."))))

(defn main-menu []
  (println "=== Eclipse History Encyclopedia ===")
  (println "")
  (println "1. View Eclipse Events")
  (println "2. Add New Eclipse Event")
  (println "3. Modify Eclipse Event")
  (println "4. Search for Eclipse Events")
  (println "5. Exit")
  (println "")
  (print "Enter your choice (1-5): ")
  (flush))


(defn get-choice []
  (read-line))

(defn process-choice [choice]
  (cond
    (= choice "1") (view-eclipse-events)
    (= choice "2") (add-new-eclipse-event)
    (= choice "3") (modify-eclipse-event)
    (= choice "4") (search-for-eclipse-events)
    (= choice "5") (do (println "Exiting the Eclipse History Encyclopedia. Goodbye!")
                       (System/exit 0))
    :else (println "Invalid choice, please enter a number between 1-5.")))

(defn -main []
  (loop []
    (main-menu)
    (let [choice (get-choice)]
      (process-choice choice)
      (when (not= choice "5")
        (recur)))))

(-main)