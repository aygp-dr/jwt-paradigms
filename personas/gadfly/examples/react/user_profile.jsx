// React example
function UserProfile({ user }) {
  return (
    <div className="profile">
      <img src={user.avatarUrl} alt={user.name} />
      <h2>{user.name}</h2>
      {user.isAdmin && <AdminBadge />}
      <p>{user.bio}</p>
    </div>
  );
}
