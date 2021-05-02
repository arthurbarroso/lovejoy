(ns using-maps.core)

(def favorite-fruit {:name "Kiwi" :color "green" :kcal_per_100g 61 :distinguish_mark "hairy"})
(get favorite-fruit :name)
(get favorite-fruit :taste)
(get favorite-fruit :taste "good")
(:name favorite-fruit)
(favorite-fruit :name)
(:taste favorite-fruit)
(:taste favorite-fruit "good")

(assoc favorite-fruit :shape "egg-like")
; favorite-fruit remains unchanged
(def complete-fav-fruit (assoc favorite-fruit :shape "egg-like" :color "brown"))
(def no-color-fav-fruit (dissoc complete-fav-fruit :color))
