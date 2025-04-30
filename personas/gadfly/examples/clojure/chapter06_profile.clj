(defn update-profile [user-id profile-updates]
  (let [current-profile (get-profile user-id)
        updated-profile (merge current-profile profile-updates)
        valid? (validate-profile updated-profile)]
    (if valid?
      (do
        (save-profile user-id updated-profile)
        {:status :success, :profile updated-profile})
      {:status :error, :message "Invalid profile data"})))
