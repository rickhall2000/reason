(ns reason.night
  (:use [clojure.core.logic]))

(def years [1920 1926 1932 1938])
(def islands ['fuijzen 'perlkin 'quyit 'voroddo])
(def scales [8.3 8.7 8.8 9.0])




(comment

. The earthquake with its epicenter at Perlkin was either the quake that took place in 1920 or the earthquake that took place in 1926.

2. The 8.8 event occurred 6 years before the event with its epicenter at Perlkin.

3. The event with its epicenter at Fujizen had a rating of 8.3.

4. The event with its epicenter at Quyit occurred 6 years after the 8.3 event.

5. The 8.7 earthquake occurred sometime before the event with its epicenter at Fujizen.
  )
