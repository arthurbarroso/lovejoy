(ns validate.core
  (:require [clojure.spec.alpha :as s]))

(def cspec
  (s/def :propertyDef
    (s/keys :req [:type (or :default :value)])))

(s/def :unq/person
  (s/keys :req-un [:acct/first-name :acct/last-name :acct/email]
          :opt-un [:acct/phone]))

(s/def :note/v
  (s/keys :req [:nt/type (or :nt/default :nt/value)]))

(s/conform :note/v {:nt/type "a" :nt/default "z"})

(s/valid? ::propertyDef {::type "t" ::default "a"})
(s/valid? ::propertyDef {:type "a" :default "b"})

(s/def :unq/person
  (s/keys :req-un [:acct/first-name :acct/last-name :acct/email]
          :opt-un [:acct/phone]))

(s/conform :unq/person
           {:first-name "Bugs"
            :last-name "Bunny"
            :email "bugs@example.com"})

(s/def :nn/v
  (s/keys :req-un [:acct/type (or :acct/default :acct/value)]))

(s/conform :nn/v
           {:type "z" :value "p"})

(s/conform :nn/v
           {:type "z"})

(s/valid? :nn/v
          {:type "z"})
