// A seemingly innocent piece of imperative code
public void updateUserStatus(User user) {
    if (user.isLoggedIn()) {
        if (user.getLastActiveTime() < System.currentTimeMillis() - TIMEOUT) {
            user.setStatus("INACTIVE");
            notifyUser(user);
        }
        if (user.getStatus().equals("INACTIVE")) {
            user.setLoginAttempts(0);
        }
    }
}
