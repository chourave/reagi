(ns reagi.runner
  (:require [cljs.test :as test]
            [doo.runner :refer-macros [doo-all-tests doo-tests]]
            [reagi.core-test]))

(doo-tests 'reagi.core-test)
